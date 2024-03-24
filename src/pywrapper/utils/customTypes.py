import sys
import os
import psutil

from dataclasses import dataclass, fields, asdict, is_dataclass
from datetime import datetime, timedelta

import numpy as np


from .customIO import _logger
from .importHandler import yaml
from .importHandler import f2py_wrapper
from .importHandler import re_import

strptime = datetime.strptime
strftime = datetime.strftime

# TODO: implement not_proofed from __setarrt__ to initiate __proof() when values are changed for all classes
# TODO: proof the values of the inputs too (e.g. [0 < theta_r < theta_s < 1])

# fmt: off
@dataclass
class vanGenuchten:
    alpha:          float       # [m^-1]
    n:              float       # [-]
    m:              float       # [-]
    theta_r:        float       # [m^3 m^-3]
    theta_s:        float       # [m^3 m^-3]
    theta_wilt:     float       # [m^3 m^-3]
    theta_crit:     float       # [m^3 m^-3]
    def __init__(self, alpha: float = None, n: float = None, m: float = None, 
                 theta_r: float = None, theta_s: float = None, 
                 theta_wilt: float = None, theta_crit: float = None):
        self.alpha = alpha
        self.n = n
        self.m = m
        self.theta_r = theta_r
        self.theta_s = theta_s
        self.theta_wilt = theta_wilt
        self.theta_crit = theta_crit


@dataclass
class soilLayer:
    name:       str
    isactive:   np.ndarray
    vanG:       vanGenuchten
    ks:         np.ndarray      # [m s^-1]
    porosity:   np.ndarray      # [m^3 m^-3]
    def __init__(self, name: str = None, isactive: np.ndarray = None, 
                 vanG: vanGenuchten = vanGenuchten(), ks: np.ndarray = None, 
                 porosity: np.ndarray = None):
        self.name = name
        self.isactive = isactive
        self.vanG = vanG
        self.ks = ks
        self.porosity = porosity

@dataclass
class spatialDomain:
    ne:             int             # Number of elements
    nl:             int             # Number of layers
    top:            np.ndarray      # [m]
    bot:            np.ndarray      # [m]
    layers:         list            # List of layer objects
    __proofed:      bool
    __initialised:  bool
    __fprecesion:   np.dtype
    def __init__(self, ne: int = None, nl: int = None, 
                 top: np.ndarray = None, bot: np.ndarray = None, 
                 layers: list = []):
        self.ne = ne
        self.nl = nl
        self.top = top
        self.bot = bot
        self.layers = layers
        self.__proofed = False
        self.__initialised = False
        self.__fprecesion = None

    def __proof(self) -> None:
        try:
            self.ne = int(self.ne)
        except ValueError:
            print(f"Number of model elements [Domain.space.ne] needs to be an integer")
            sys.exit(1)
        self.nl = int(self.nl)
        try:
            self.top = np.array(self.top, dtype=self.__fprecesion, order="F")
        except ValueError:
            print(f"Number of model layers [Domain.space.nl] needs to be an integer")
            sys.exit(1)
        ne, nl = self.ne, self.nl

        if not self.top.size == ne:
            try:
                self.top = self.top.reshape(ne)
            except ValueError:
                print(f"Domain.space.top must be an array of size {ne}.")
                sys.exit(1)
        self.bot = np.array(self.bot, dtype=self.__fprecesion, order="F")
        if not self.bot.shape == (nl, ne):
            try:
                self.bot = self.bot.reshape(nl, ne)
            except ValueError:
                print(f"Domain.space.bot must be an array of shape ({nl},{ne}).")
                sys.exit(1)

        if not len(self.layers) == nl:
            print(f"Definition of layers does not match the number of defined layers: Domain.space.nl {nl}.")
            sys.exit(1)

        for lidx in range(nl):
            self.layers[lidx].isactive = np.array(self.layers[lidx].isactive, dtype=self.__fprecesion, order="F")
            if not self.layers[lidx].isactive.size == ne:
                print(f"Shape of isactive for layer[{lidx}] does not match Domain.space.ne.")
                sys.exit(1)
            self.layers[lidx].ks = np.array(self.layers[lidx].ks, dtype=self.__fprecesion, order="F")
            if not self.layers[lidx].ks.size == ne:
                print(f"Shape of ks for layer[{lidx}] does not match Domain.space.ne.")
                sys.exit(1)
            self.layers[lidx].porosity = np.array(self.layers[lidx].porosity, dtype=self.__fprecesion, order="F")
            if not self.layers[lidx].porosity.size == ne:
                print(f"Shape of porosity for layer[{lidx}] does not match Domain.space.ne.")
                sys.exit(1)
        
        self.__proofed = True
        
    def init(self) -> None:
        if not self.__proofed:
            self.__proof()

        self.__initialised = True

@dataclass
class temporalDomain:
    # TODO: #FIXME: implement variable time-stepping:
    #       1. allow dynamic definition of dt and nts => results can only be stored in a dynamic array in py (np.append)
    #       2. split dt into global_dt and local_dt. exchange at dynamic local_dt but store values at global_dt
    # TODO: use the not_proofed status from __setarrt__ to initiate __proof() when values are changed
    dt:              timedelta       # Time step size [datetime.timedelta]
    start:           datetime        # Start time [datetime.datetime]
    stop:            datetime        # End time [datetime.datetime]
    current:         datetime        # Current time [datetime.datetime]
    nts:             int             # Number of time steps
    _dt:             int             # Time step size [s]
    _start:          int             # Start time [s]
    _stop:           int             # End time [s]
    _current:        int             # Current time [s]
    _valid_formats:  list
    __status:        dict

    def __init__(self, dt: timedelta = None, start: datetime = None, stop: datetime = None):
        self.dt = dt
        self.start = start
        self.stop = stop
        self._valid_formats = ['%Y%m%d_%H%M%S', '%Y-%m-%dT%H:%M:%S']
        self.current = None
        self.nts = None
        self._dt = None
        self._start = None
        self._stop = None
        self._current = None
        self.__status = {}
        self.__status['proofed'] = False
        self.__status['initialised'] = False

        self._current = 0
        self.current = self.start

    def __proof(self) -> None:
        if type(self.start) in [datetime, str]:
            if type(self.start) == str:
                for tformat in self._valid_formats:
                    try:
                        self.start = datetime.strptime(self.start, tformat)
                    except ValueError:
                        print(
                            f"Unsupported Domain.time.start format. Supported formats: {self._valid_formats}."
                        )
                    sys.exit(1)
        else:
            print(f"Domain.time.start must be a datetime object or a string in {self._valid_formats}.")
            sys.exit(1)
        if type(self.stop) in [datetime, str]:
            if type(self.stop) == str:
                for tformat in self._valid_formats:
                    try:
                        self.stop = datetime.strptime(self.stop, tformat)
                    except ValueError:
                        print(
                            f"Unsupported Domain.time.stop format. Supported formats: {self._valid_formats}."
                        )
                        sys.exit(1)
        else:
            print(f"Domain.time.stop must be a datetime object or a string in {self._valid_formats}.")
            sys.exit(1)

        if type(self.dt) in [timedelta, int, float]:
            if type(self.dt) in [int, float]:
                self.dt = timedelta(seconds=self.dt)
        else:
            print(f"Domain.time.dt must either be a timedelta object or an int in seconds.")
            sys.exit(1)

        self._dt = int(self.dt.total_seconds())
        self._start = 0
        self._stop = int((self.stop - self.start).total_seconds())

        self.nts = self._stop / self._dt
        if not self.nts > 0:
            print(f"Domain.time.dt must be smaller than Domain.time.stop - Domain.time.start.")
            sys.exit(1)
        elif self.nts < 0:
            print(f"Domain.time.dt must be smaller than Domain.time.stop - Domain.time.start.")
            sys.exit(1)
        elif self.nts != int(self.nts):
            print(f"Domain.time.dt must be smaller than the total simulation time, and it must divide the total simulation time evenly.")
            sys.exit(1)
        else:
            self.nts = int(self.nts)
        
        self.__status['proofed'] = True
    
    def __setattr__(self, name, value):
        if hasattr(self, name) and getattr(self, name) != value:
            self.__status['proofed'] = False
        super().__setattr__(name, value)

    def init(self) -> None:
        if not self.__status['proofed']:
            self.__proof()

        self.__status['initialised'] = True
        

@dataclass
class gwswexDomain:
    space:          spatialDomain
    time:           temporalDomain
    _proofed:       bool
    __initialised:  bool
    def __init__(self, space: spatialDomain = spatialDomain(), time: temporalDomain = temporalDomain()):
        self.space = space
        self.time = time
        self.__initialised = False
    
    def init(self) -> None:
        self.space.init()
        self.time.init()        
        self.__initialised = True

@dataclass
class initialConditions:
    gw:         np.ndarray      # [m^3 m^-2]
    sw:         np.ndarray      # [m^3 m^-2]
    uz:         np.ndarray      # [m^3 m^-2]
    def __init__(self, gw: np.ndarray = None, sw: np.ndarray = None, uz: np.ndarray = None):
        self.gw = gw
        self.sw = sw
        self.uz = uz

@dataclass
class meteorologicalForcings:
    precip:     np.ndarray      # [m s^-1]
    pet:        np.ndarray      # [m s^-1]
    aet:        np.ndarray      # [m s^-1]
    evap:       np.ndarray      # [m s^-1]
    temp:       np.ndarray      # [K]
    rh:         np.ndarray      # [-, %]
    wind:       np.ndarray      # [m s^-1]
    rad_in:     np.ndarray      # [W m^-2]
    rad_out:    np.ndarray      # [W m^-2]
    def __init__(self, precip: np.ndarray = None, pet: np.ndarray = None, 
                 aet: np.ndarray = None, evap: np.ndarray = None, 
                 temp: np.ndarray = None, rh: np.ndarray = None, wind: np.ndarray = None, 
                 rad_in: np.ndarray = None, rad_out: np.ndarray = None):
        self.precip = precip
        self.pet = pet
        self.aet = aet
        self.evap = evap
        self.temp = temp
        self.rh = rh
        self.wind = wind
        self.rad_in = rad_in
        self.rad_out = rad_out

@dataclass
class boundaryConditions:
    name:       str
    type:       str
    value:      np.ndarray
    time:       np.ndarray      # [s] since start

@dataclass
class externalForcings:
    name:       str
    type:       str
    value:      np.ndarray
    time:       np.ndarray      # [s] since start

@dataclass
class solverSettings:
    verbose:        bool
    omp_cores:      str
    integrator:     str
    nts_per_flux:   np.ndarray
    def __init__(self, verbose: bool = None, omp_cores: str = None, 
                 integrator: str = None, nts_per_flux: np.ndarray = None):
        self.verbose = verbose
        self.omp_cores = omp_cores
        self.integrator = integrator
        self.nts_per_flux = nts_per_flux

@dataclass
class gwswexPaths:
    output:         str
    input:          str
    logfile:        str
    config:         str
    f2pywrapper:    str
    def __init__(self, output: str = None, input: str = None, logfile: str = None, 
                 config: str = None, f2pywrapper: str = None):
        self.output = output
        self.input = input
        self.logfile = logfile
        self.config = config
        self.f2pywrapper = f2pywrapper

@dataclass
class gwswexIO:
    verbose:        bool
    debug:          bool
    logger:         _logger
    def __init__(self, verbose: bool = None, debug: bool = None, logger: _logger = None):
        self.verbose = verbose
        self.debug = debug
        self.logger = logger
# fmt: on


class Model:
# TODO: Implement a method to display model properties
    """Top-Level Class for the GWSWEX model."""

    def __infer_config_keys(self):
        def prune(d: dict):
            for key, value in list(d.items()):
                if isinstance(value, dict):
                    prune(value)
                if key.startswith('_'):
                    del d[key]

        self.__cnf_keys: dict = {}
        self.__cnf_keys['Domain'] = {}
        self.__cnf_keys['Domain']['space'] = {'.': [field.name for field in fields(spatialDomain)]}
        self.__cnf_keys['Domain']['time'] = {'.': [field.name for field in fields(temporalDomain)]}
        self.__cnf_keys['Domain']['space']['layers'] = {'.': [field.name for field in fields(soilLayer)]}
        self.__cnf_keys['Domain']['space']['layers']['.'].remove('vanG')
        self.__cnf_keys['Domain']['space']['layers']['vanG'] = [field.name for field in fields(vanGenuchten)]
        self.__cnf_keys['Ini'] = [field.name for field in fields(initialConditions)]
        self.__cnf_keys['Meteo'] = [field.name for field in fields(meteorologicalForcings)]
        self.__cnf_keys['Boundaries'] = [field.name for field in fields(boundaryConditions)]
        self.__cnf_keys['ExtForcings'] = [field.name for field in fields(externalForcings)]
        self.__cnf_keys['Solver'] = [field.name for field in fields(solverSettings)]
        self.__cnf_keys['Paths'] = [field.name for field in fields(gwswexPaths)]
        self.__cnf_keys['IO'] = [field.name for field in fields(gwswexIO)]

        # self.__cnf_keys = prune(self.__cnf_keys)

    def __define_required_configurations(self):
        self.__cnf_req_keys: dict = {'.': ['Domain', 'Ini', 'Meteo']}
        self.__cnf_req_keys['Domain'] = {'.': ['space', 'time']}
        self.__cnf_req_keys['Domain']['space'] = {'.': ['ne', 'nl', 'top', 'bot']}
        self.__cnf_req_keys['Domain']['space']['layers'] = {'.': ["isactive", 'vanG', 'ks', 'porosity']}
        self.__cnf_req_keys['Domain']['space']['layers']['vanG'] = [
            'alpha',
            'n',
            'theta_r',
            'theta_s',
        ]
        self.__cnf_req_keys['Domain']['time'] = ['dt', 'start', 'stop']
        self.__cnf_req_keys['Ini'] = ['gw', 'sw']
        self.__cnf_req_keys['Meteo'] = ["precip", "pet"]
        self.__cnf_req_keys['Boundaries'] = ['type', 'value', 'time']
        self.__cnf_req_keys['ExtForcings'] = ['type', 'value', 'time']
        self.__cnf_req_keys['Solver'] = []
        self.__cnf_req_keys['Paths'] = []
        self.__cnf_req_keys['IO'] = []
        
    def __init__(
        self,
        fpath: str,
        logfile: str = None,
        verbose: bool = False,
        debug: bool = False,
        float_precesion: type = np.float64
    ):
        """Initializes the GWSWEX model data structure.

        Args:
            fpath (str): Path to the configuration file.
        
        Optional Args:
            logfile (str, optional): Path to the logfile. Defaults to None.
            verbose (bool, optional): If True, additional information will be printed during initialization. Defaults to False.
            debug (bool, optional): If True, debug information will be printed during initialization. Defaults to False.
            float_precesion (type, optional): Precesion of floating point numbers. Defaults to np.float64.
        """
        self.__req_checked: bool = False
        self.__proofed: bool = False
        self.__initialised: bool = False
        self.__parsed_from_config: bool = False
        self.__parsed_to_config: bool = False

        self.fprecesion: np.dtype = float_precesion
        self.strlen: int = int(f2py_wrapper.gwswex.strlen)
        

        self.Domain: gwswexDomain = gwswexDomain()
        self.Ini: initialConditions = initialConditions()
        self.Meteo: meteorologicalForcings = meteorologicalForcings()
        self.Boundaries: dict = {}
        self.ExtForcings: dict = {}
        self.Solver: solverSettings = solverSettings()
        self.Paths: gwswexPaths = gwswexPaths()
        self.IO: gwswexIO = gwswexIO()
        self.log: _logger

        self.Domain.space.__fprecesion = self.fprecesion

        self.config: dict = {}
        self.__infer_config_keys()
        self.__define_required_configurations()

        self.config_file: str = os.path.abspath(fpath)
        self.wd: str = os.path.dirname(self.config_file)
        self.Paths.config = os.path.abspath(fpath)

        if not logfile:
            logfile = os.path.join(self.wd, 'gwswex_pywrapper.log')
        if debug:
            level: int = 10
        if verbose:
            level: int = 20
        else:
            level: int = 30
        self.log = _logger(os.path.abspath(logfile), level=level)
        self.log.info(f"Model configuration file: {self.config_file}")
        self.Paths.logfile = logfile
        self.IO.logger = self.log
        self.IO.verbose = verbose
        self.IO.debug = debug

        if os.path.exists(fpath):
            file_status = self.__read_config()
            if file_status:
                self.__parse_from_config()
                self.__parsed_from_config = True
                self.__check_req_config()
                self.__req_checked = True
                self.__proof_config()
                self.__proofed = True
                self.__initialised = True
            else:
                self.log.warning(f"Configuration file {fpath} is empty! Initialising a blank model instance.")
            

    def __check_req_config_dict(self) -> bool:
        """Checks the configuration dictionary for required keys.
        Returns:
            bool: True if the configuration dictionary is valid, False otherwise.
        """
        config = self.config
        config_req_keys = self.__cnf_req_keys
        config_file = self.config_file
        log = self.log

        for key in config_req_keys['.']:
            try:
                config[key]
            except KeyError:
                log.error(f"Model configuration '{key}' was not found in {config_file}.")
                return False

        for key in config_req_keys['Domain']['.']:
            try:
                config['Domain'][key]
            except KeyError:
                log.error(f"Model [Domain] configuration '{key}' was not found in {config_file}.")
                return False

        for key in config_req_keys['Domain']['space']['.']:
            try:
                config['Domain']['space'][key]
            except KeyError:
                log.error(f"Model [Domain.space] configuration '{key}' was not found in {config_file}.")
                return False

        for key in config_req_keys['Domain']['time']:
            try:
                config['Domain']['time'][key]
            except KeyError:
                log.error(f"Model [Domain.time] configuration '{key}' was not found in {config_file}.")
                return False

        for layer in [f"layer{lidx}" for lidx in range(1, config['Domain']['space']['nl'] + 1)]:
            for key in config_req_keys['Domain']['space']['layers']['.']:
                try:
                    config['Domain']['space']['layers'][layer][key]
                    for key in config_req_keys['Domain']['space']['layers']['vanG']:
                        try:
                            config['Domain']['space']['layers'][layer]['vanG'][key]
                        except KeyError:
                            log.error(f"Configuration [vanG.{key}] for '{layer}' was not found in {config_file}.")
                            return False
                except KeyError:
                    log.error(f"Configuration [{key}] for '{layer}' was not found in {config_file}.")
                    return False

        for key in config_req_keys['Ini']:
            try:
                config['Ini'][key]
            except KeyError:
                log.error(f"Model [initial] configuration '{key}' was not found in {config_file}.")
                return False

        for key in config_req_keys['Meteo']:
            try:
                config['Meteo'][key]
            except KeyError:
                log.error(f"Model [Meteo] configuration '{key}' was not found in {config_file}.")
                return False

        return True

    def __parse_from_config(self):
        """Parses the configuration file into the model data structure.

        Required Attributes:
            config (dict): Configuration dictionary
        """
        log = self.log
        config = self.config
        config_keys = self.__cnf_keys
        config_req_keys = self.__cnf_req_keys

        self.Domain = gwswexDomain()

        for key in config_keys['Domain']['space']['.']:
            try:
                config['Domain']['space'][key]
                setattr(self.Domain.space, key, config['Domain']['space'][key])
            except KeyError:
                if not key in config_req_keys['Domain']['space']['.']:
                    log.warning(f"Model [Domain.space] configuration '{key}' was not found in {self.Paths.config}.")
                else:
                    log.error(f"Model [Domain.space] configuration '{key}' was not found in {self.Paths.config}.")
                    raise
            except ValueError:
                if (
                    type(config['Domain']['space'][key]) != type(getattr(self.Domain.space, key))
                    and type(config['Domain']['space'][key]) == str
                ):
                    log.debug(
                        f"Expected type {type(getattr(self.Domain.space, key))} for [model.Domain.space.{key}]. Trying to read from file, assuming filename was provided."
                    )
                    try:
                        data = np.loadtxt(config['Domain']['space'][key])
                        setattr(self.Domain.space, key, data)
                    except:
                        log.error(
                            f"Unable to read model [Domain].space configuration '{key}' either as {type(getattr(self.Domain.space, key))} or as a file with np.loadtxt."
                        )
                        raise

        for key in config_keys['Domain']['time']['.']:
            try:
                config['Domain']['time'][key]
                setattr(self.Domain.time, key, config['Domain']['time'][key])
            except KeyError:
                if not key in config_req_keys['Domain']['time']:
                    log.warning(f"Model [Domain.time] configuration '{key}' was not found in {self.Paths.config}.")
                else:
                    log.error(f"Model [Domain.time] configuration '{key}' was not found in {self.Paths.config}.")
                    raise
            except ValueError:
                if key == 'dt' and type(config['Domain']['time'][key]) in [
                    int,
                    float,
                    str,
                ]:
                    try:
                        setattr(
                            self.Domain.time,
                            key,
                            timedelta(seconds=int(config['Domain']['time'][key])),
                        )
                    except:
                        log.error(
                            f"Expected type {type(getattr(self.Domain.time, key))} for [model.Domain.time.{key}] but got {type(config['Domain']['time'][key])}."
                        )
                        raise
                if key == 'start' and type(config['Domain']['time'][key]) == str:
                    for tformat in self.Domain.time._valid_formats:
                        try:
                            setattr(
                                self.Domain.time,
                                key,
                                strptime(config['Domain']['time'][key]),
                                tformat,
                            )
                        except:
                            log.error(
                                f"Unsupported datetime format {tformat} for [model.Domain.time.{key}]. Supported formats: {self.Domain.time._valid_formats}."
                            )
                            raise
                if key == 'stop' and type(config['Domain']['time'][key]) == str:
                    for tformat in self.Domain.time._valid_formats:
                        try:
                            setattr(
                                self.Domain.time,
                                key,
                                strptime(config['Domain']['time'][key]),
                                tformat,
                            )
                        except:
                            log.error(
                                f"Unsupported datetime format {tformat} for [model.Domain.time.{key}]. Supported formats: {self.Domain.time._valid_formats}."
                            )
                            raise
                else:
                    log.error(
                        f"Expected type {type(getattr(self.Domain.time, key))} for [model.Domain.time.{key}] but got {type(config['Domain']['time'][key])}."
                    )
                    raise

        layer_keys = [f"layer{lidx}" for lidx in range(1, config['Domain']['space']['nl'] + 1)]
        layers = []
        for layer in layer_keys:
            layers.append(soilLayer())
        self.Domain.space.layers = layers
        for lidx, layer in enumerate(layers):
            layer_config = config['Domain']['space']['layers'][layer_keys[lidx]]
            for key in config_keys['Domain']['space']['layers']['.']:
                try:
                    layer_config[key]
                    setattr(layer, key, layer_config[key])
                except KeyError:
                    if not key in config_req_keys['Domain']['space']['layers']['.']:
                        log.warning(f"Model [Domain] configuration '{key}' was not found in {self.Paths.config}.")
                    else:
                        log.error(f"Model [Domain] configuration '{key}' was not found in {self.Paths.config}.")
                        raise
                except ValueError:
                    if (
                        type(config['Domain']['space']['layers'][key]) != type(getattr(self.Domain.space.layer, key))
                        and type(config['Domain']['space']['layers'][key]) == str
                    ):
                        log.debug(
                            f"Expected type {type(getattr(self.Domain.space.layer, key))} for [model.Domain.space.layer.{key}]. Trying to read from file, assuming filename was provided."
                        )
            for key in config_keys['Domain']['space']['layers']['vanG']:
                try:
                    layer_config['vanG'][key]
                    setattr(
                        layer.vanG,
                        key,
                        layer_config['vanG'][key],
                    )
                except KeyError:
                    if not key in config_req_keys['Domain']['space']['layers']['vanG']:
                        log.warning(
                            f"Model [Domain.space.layer] configuration '{key}' was not found in {self.Paths.config}."
                        )
                    else:
                        log.error(
                            f"Model [Domain.space.layer] configuration '{key}' was not found in {self.Paths.config}."
                        )
                        raise
                except ValueError:
                    log.error(
                        f"Expected type {type(getattr(layer.vanG, key))} for [model.Domain.space.layer.vanG.{key}] but got {type(layer_config['vanG'][key])}."
                    )
                    raise

        self.Ini = initialConditions()
        for key in config_keys['Ini']:
            try:
                config['Ini'][key]
                setattr(self.Ini, key, config['Ini'][key])
            except KeyError:
                if not key in config_req_keys['Ini']:
                    log.warning(f"Model [initial] configuration '{key}' was not found in {self.Paths.config}.")
                else:
                    log.error(f"Model [initial] configuration '{key}' was not found in {self.Paths.config}.")
                    raise
            except ValueError:
                if (
                    type(config['Ini'][key]) != type(getattr(self.Ini, key))
                    and type(config['Ini'][key]) == str
                ):
                    log.debug(
                        f"Expected type {type(getattr(self.Ini, key))} for [model.initial.{key}]. Trying to read from file, assuming filename was provided."
                    )
                    try:
                        data = np.loadtxt(config['Ini'][key])
                        setattr(self.Ini, key, data)
                    except:
                        log.error(
                            f"Unable to read model [initial] configuration '{key}' either as {type(getattr(self.Ini, key))} or as a file with np.loadtxt."
                        )
                        raise

        self.Meteo = meteorologicalForcings()
        for key in config_keys['Meteo']:
            try:
                config['Meteo'][key]
                setattr(self.Meteo, key, config['Meteo'][key])
            except KeyError:
                if not key in config_req_keys['Meteo']:
                    log.warning(f"Model [Meteo] configuration '{key}' was not found in {self.Paths.config}.")
                else:
                    log.error(f"Model [Meteo] configuration '{key}' was not found in {self.Paths.config}.")
                    raise
            except ValueError:
                if type(config['Meteo'][key]) != type(getattr(self.Meteo, key)) and type(config['Meteo'][key]) == str:
                    log.debug(
                        f"Expected type {type(getattr(self.Meteo, key))} for [model.Meteo.{key}]. Trying to read from file, assuming filename was provided."
                    )
                    try:
                        data = np.loadtxt(config['Meteo'][key])
                        setattr(self.Meteo, key, data)
                    except:
                        log.error(
                            f"Unable to read model [Meteo] configuration '{key}' either as {type(getattr(self.Meteo, key))} or as a file with np.loadtxt."
                        )
                        raise

        self.Boundaries = {}
        try:
            config['Boundaries']
            for key, value in config['Boundaries'].items():
                self.Boundaries[key] = boundaryConditions(name=key)
                for key in config_keys['Boundaries'][key]:
                    try:
                        value[key]
                        setattr(self.Boundaries[key], key, value[key])
                    except KeyError:
                        if not key in config_req_keys['Boundaries'][key]:
                            log.warning(
                                f"Model [Boundaries] configuration '{key}' was not found in {self.Paths.config}."
                            )
                        else:
                            log.error(f"Model [Boundaries] configuration '{key}' was not found in {self.Paths.config}.")
                            raise
                    except ValueError:
                        if type(value[key]) != type(getattr(self.Boundaries[key], key)) and type(value[key]) == str:
                            log.debug(
                                f"Expected type {type(getattr(self.Boundaries[key], key))} for [model.Boundaries.{key}]. Trying to read from file, assuming filename was provided."
                            )
                            try:
                                data = np.loadtxt(value[key])
                                setattr(self.Boundaries[key], key, data)
                            except:
                                log.error(
                                    f"Unable to read model [Boundaries] configuration '{key}' either as {type(getattr(self.Boundaries[key], key))} or as a file with np.loadtxt."
                                )
                                raise
        except:
            log.warning("No definitions for [model.boundaryConditions]")

        self.ExtForcings = {}
        try:
            config['ExtForcings']
            for key, value in config['ExtForcings'].items():
                self.ExtForcings[key] = externalForcings(name=key)
                for key in config_keys['ExtForcings'][key]:
                    try:
                        value[key]
                        setattr(self.ExtForcings[key], key, value[key])
                    except KeyError:
                        if not key in config_req_keys['ExtForcings'][key]:
                            log.warning(f"Model [ExtForcings] configuration '{key}' was not found in {self.Paths.config}.")
                        else:
                            log.error(f"Model [ExtForcings] configuration '{key}' was not found in {self.Paths.config}.")
                            raise
                    except ValueError:
                        if type(value[key]) != type(getattr(self.ExtForcings[key], key)) and type(value[key]) == str:
                            log.debug(
                                f"Expected type {type(getattr(self.ExtForcings[key], key))} for [model.ExtForcings.{key}]. Trying to read from file, assuming filename was provided."
                            )
                            try:
                                data = np.loadtxt(value[key])
                                setattr(self.ExtForcings[key], key, data)
                            except:
                                log.error(
                                    f"Unable to read model [ExtForcings] configuration '{key}' either as {type(getattr(self.ExtForcings[key], key))} or as a file with np.loadtxt."
                                )
                                raise
        except:
            log.warning("No definitions for [model.externalForcings]")

        self.Solver = solverSettings()
        try:
            config_keys['Solver']
            for key in config_keys['Solver']:
                try:
                    config['Solver']
                    setattr(self.Solver, key, config['Solver'][key])
                except KeyError:
                    if not key in config_req_keys['Solver']:
                        log.warning(f"Model [Solver] configuration '{key}' was not found in {self.Paths.config}.")
                    else:
                        log.error(f"Model [Solver] configuration '{key}' was not found in {self.Paths.config}.")
                        raise
                except ValueError:
                    if (
                        type(config['Solver'][key]) != type(getattr(self.Solver, key))
                        and type(config['Solver'][key]) == str
                    ):
                        log.debug(
                            f"Expected type {type(getattr(self.Solver, key))} for [model.Solver.{key}]. Trying to read from file, assuming filename was provided."
                        )
                        try:
                            data = np.loadtxt(config['Solver'][key])
                            setattr(self.Solver, key, data)
                        except:
                            log.error(
                                f"Unable to read model [Solver] configuration '{key}' either as {type(getattr(self.Solver, key))} or as a file with np.loadtxt."
                            )
                            raise
        except:
            log.warning("No definitions for [model.solverSettings]")

        self.Paths = gwswexPaths()
        try:
            config_keys['Paths']
            for key in config_keys['Paths']:
                try:
                    config['Paths'][key]
                    setattr(self.Paths, key, config['Paths'][key])
                except KeyError:
                    if not key in config_req_keys['Paths']:
                        log.warning(f"Model [Paths] configuration '{key}' was not found in {self.Paths.config}.")
                    else:
                        log.error(f"Model [Paths] configuration '{key}' was not found in {self.Paths.config}.")
                        raise
                except ValueError:
                    if type(config['Paths'][key]) != type(getattr(self.Paths, key)) and type(config['Paths'][key]) == str:
                        log.debug(
                            f"Expected type {type(getattr(self.Paths, key))} for [model.Paths.{key}]. Trying to read from file, assuming filename was provided."
                        )
                        try:
                            data = np.loadtxt(config['Paths'][key])
                            setattr(self.Paths, key, data)
                        except:
                            log.error(
                                f"Unable to read model [Paths] configuration '{key}' either as {type(getattr(self.Paths, key))} or as a file with np.loadtxt."
                            )
                            raise
        except:
            log.warning("No definitions for [model.gwswexPaths]")

        self.IO = gwswexIO()
        try:
            config_keys['IO']
            for key in config_keys['IO']:
                try:
                    config['IO'][key]
                    setattr(self.IO, key, config['IO'][key])
                except KeyError:
                    if not key in config_req_keys['IO']:
                        log.warning(f"Model [IO] configuration '{key}' was not found in {self.Paths.config}.")
                    else:
                        log.error(f"Model [IO] configuration '{key}' was not found in {self.Paths.config}.")
                        raise
                except ValueError:
                    if type(config['IO'][key]) != type(getattr(self.IO, key)) and type(config['IO'][key]) == str:
                        log.debug(
                            f"Expected type {type(getattr(self.IO, key))} for [model.IO.{key}]. Trying to read from file, assuming filename was provided."
                        )
                        try:
                            data = np.loadtxt(config['IO'][key])
                            setattr(self.IO, key, data)
                        except:
                            log.error(
                                f"Unable to read model [IO] configuration '{key}' either as {type(getattr(self.IO, key))} or as a file with np.loadtxt."
                            )
                            raise
        except:
            log.warning("No definitions for [model.gwswexIO]")

    def __read_config(self) -> bool:
        """Reads a YAML format configuration file.

        Required Attributes:
            config_file (str): Path to the configuration file.
        """

        log = self.log

        try:
            with open(self.config_file, 'r') as f:
                self.config = yaml.load(f, Loader=yaml.FullLoader)
                log.info(f"Read configuration from {self.config_file}:\n{self.config}")
        except FileNotFoundError:
            log.error(f"Configuration file not found: {self.config_file}")
            print(f"Configuration file not found: {self.config_file}")
            sys.exit(1)
        except Exception as e:
            log.error(f"Error reading configuration file: {self.config_file}. Check {self.log} for details.")
            log.error(e)
            sys.exit(1)

        if self.config is not None:
            if not self.__check_req_config_dict():
                log.error(f"Incomplete configuration file: {self.config_file}")
                print(f"Incomplete configuration file: {self.config_file}. Check {log.fname} for details.")
                sys.exit(1)
        else:
            log.warning(f"Configuration file {self.config_file} is empty! Initialising a blank model instance.")
            return False

        try:
            self.__parse_from_config()
        except KeyError as e:
            log.error(f"Incomplete definitions in configuration file: {self.config_file}.")
            print(f"Incomplete definitions in configuration file: {self.config_file}. Check {log.fname} for details.")
            sys.exit(1)
        except ValueError as e:
            log.error(f"Invalid definitions in configuration file: {self.config_file}.")
            print(f"Invalid definitions in configuration file: {self.config_file}. Check {log.fname} for details.")
            sys.exit(1)
        except Exception as e:
            log.error(f"Error parsing configuration file: {self.config_file}")
            log.error(e)
            raise

        return True

    def __check_req_config(self) -> bool:
        """Checks the model data structure for required attributes.

        Returns True if all required attributes are defined, False otherwise.
        """
        self.missing = []
        log = self.log

        cnf_req_keys = self.__cnf_req_keys

        for key in cnf_req_keys['.']:
            try:
                if getattr(self, key) is None:
                    self.missing.append(key)
                    log.debug(f"Required attribute '{key}' is not defined.")
                    return False
            except AttributeError:
                self.missing.append(key)
                log.debug(f"Required attribute '{key}' is not defined.")
                return False

        for key in cnf_req_keys['Domain']['.']:
            try:
                if getattr(self.Domain, key) is None:
                    self.missing.append(f"Domain.{key}")
                    log.debug(f"Required attribute Domain.'{key}' is not defined.")
                    return False
            except AttributeError:
                self.missing.append(f"Domain.{key}")
                log.debug(f"Required attribute Domain.'{key}' is not defined.")
                return False
        for key in cnf_req_keys['Domain']['space']['.']:
            try:
                if getattr(self.Domain.space, key) is None:
                    self.missing.append(f"Domain.space.{key}")
                    log.debug(f"Required attribute Domain.space.'{key}' is not defined.")
                    return False
            except AttributeError:
                self.missing.append(f"Domain.space.{key}")
                log.debug(f"Required attribute Domain.space.'{key}' is not defined.")
                return False
        if not len(self.Domain.space.layers) == self.Domain.space.nl:
            self.missing.append(f"Domain.space.layers")
            log.debug(f"Required attribute Domain.space.layers is not defined.")
            return False
        for lidx in range(self.Domain.space.nl):
            for key in cnf_req_keys['Domain']['space']['layers']['.']:
                try:
                    if getattr(self.Domain.space.layers[lidx], key) is None:
                        self.missing.append(f"Domain.space.layer[{lidx}].{key}")
                        log.debug(f"Required attribute Domain.space.layer[{lidx}].'{key}' is not defined.")
                        return False
                except AttributeError:
                    self.missing.append(f"Domain.space.layer[{lidx}].{key}")
                    log.debug(f"Required attribute Domain.space.layer[{lidx}].'{key}' is not defined.")
                    return False

            for vkey in cnf_req_keys['Domain']['space']['layers']['vanG']:
                try:
                    if getattr(self.Domain.space.layers[lidx].vanG, vkey) is None:
                        self.missing.append(f"Domain.space.layer[{lidx}].vanG.{vkey}")
                        log.debug(f"Required attribute '{key}.{vkey}' is not defined.")
                        return False
                except AttributeError:
                    self.missing.append(f"Domain.space.layer[{lidx}].vanG.{vkey}")
                    log.debug(f"Required attribute '{key}.{vkey}' is not defined.")
                    return False

        for key in cnf_req_keys['Domain']['time']:
            try:
                if getattr(self.Domain.time, key) is None:
                    self.missing.append(f"Domain.time.{key}")
                    log.debug(f"Required attribute Domain.time.'{key}' is not defined.")
                    return False
            except AttributeError:
                self.missing.append(f"Domain.time.{key}")
                log.debug(f"Required attribute Domain.time.'{key}' is not defined.")
                return False

        for key in cnf_req_keys['Ini']:
            try:
                if getattr(self.Ini, key) is None:
                    self.missing.append(f"initial.{key}")
                    log.debug(f"Required attribute initial.'{key}' is not defined.")
                    return False
            except AttributeError:
                self.missing.append(f"initial.{key}")
                log.debug(f"Required attribute initial.'{key}' is not defined.")
                return False

        for key in cnf_req_keys['Meteo']:
            try:
                if getattr(self.Meteo, key) is None:
                    self.missing.append(f"Meteo.{key}")
                    log.debug(f"Required attribute Meteo.'{key}' is not defined.")
                    return False
            except AttributeError:
                self.missing.append(f"Meteo.{key}")
                log.debug(f"Required attribute Meteo.'{key}' is not defined.")
                return False

        if self.Boundaries:
            for bnd_key, bnd in self.Boundaries.items():
                for key in cnf_req_keys['Boundaries']:
                    try:
                        if getattr(bnd, key) is None:
                            self.missing.append(f"Boundaries.[{bnd_key}].{key}")
                            log.debug(f"Required attribute Boundaries.[{bnd_key}].'{key}' is not defined.")
                            return False
                    except AttributeError:
                        self.missing.append(f"Boundaries.[{bnd_key}].{key}")
                        log.debug(f"Required attribute Boundaries.[{bnd_key}].'{key}' is not defined.")
                        return False

        if self.ExtForcings:
            for frc_key, frc in self.ExtForcings.items():
                for key in cnf_req_keys['ExtForcings']:
                    try:
                        if getattr(frc, key) is None:
                            self.missing.append(f"ExtForcings.[{frc_key}].{key}")
                            log.debug(f"Required attribute ExtForcings.[{frc_key}].'{key}' is not defined.")
                            return False
                    except AttributeError:
                        self.missing.append(f"ExtForcings.[{frc_key}].{key}")
                        log.debug(f"Required attribute ExtForcings.[{frc_key}].'{key}' is not defined.")
                        return False

        for key in cnf_req_keys['Solver']:
            try:
                if getattr(self.Solver, key) is None:
                    self.missing.append(f"Solver.{key}")
                    log.debug(f"Required attribute Solver.'{key}' is not defined.")
                    return False
            except AttributeError:
                self.missing.append(f"Solver.{key}")
                log.debug(f"Required attribute Solver.'{key}' is not defined.")
                return False

        for key in cnf_req_keys['Paths']:
            try:
                if getattr(self.Paths, key) is None:
                    self.missing.append(f"Paths.{key}")
                    log.debug(f"Required attribute Paths.'{key}' is not defined.")
                    return False
            except AttributeError:
                self.missing.append(f"Paths.{key}")
                log.debug(f"Required attribute Paths.'{key}' is not defined.")
                return False

        for key in cnf_req_keys['IO']:
            try:
                if getattr(self.IO, key) is None:
                    self.missing.append(f"IO.{key}")
                    log.debug(f"Required attribute IO.'{key}' is not defined.")
                    return False
            except AttributeError:
                self.missing.append(f"IO.{key}")
                log.debug(f"Required attribute IO.'{key}' is not defined.")
                return False

        self.__req_checked = True
        return True

    def __proof_config(self) -> None:
        """Checks if the model configuration is complete.

        Returns True if all required attributes are defined, False otherwise.
        """
        log = self.log

        if not self.__req_checked:
            self.__check_req_config()
            if not self.__req_checked:
                log.error(f"Missing required configuration(s) to initialise model: {self.missing}")
                print(f"Missing required configuration(s) to initialise model: {self.missing}")
                sys.exit(1)

        self.Ini.gw = np.array(self.Ini.gw, dtype=self.fprecesion, order="F")
        if not self.Ini.gw.size == self.Domain.space.ne:
            try:
                self.Ini.gw = self.Ini.gw.reshape(self.Domain.space.ne)
            except ValueError:
                log.error(f"initial.gw must be an array of size {self.Domain.space.ne}.")
                print(f"initial.gw must be an array of size {self.Domain.space.ne}.")
                sys.exit(1)
        self.Ini.sw = np.array(self.Ini.sw, dtype=self.fprecesion, order="F")
        if not self.Ini.sw.size == self.Domain.space.ne:
            try:
                self.Ini.sw = self.Ini.sw.reshape(self.Domain.space.ne)
            except ValueError:
                log.error(f"initial.sw must be an array of size {self.Domain.space.ne}.")
                print(f"initial.sw must be an array of size {self.Domain.space.ne}.")
                sys.exit(1)

        self.Meteo.precip = np.array(self.Meteo.precip, dtype=self.fprecesion, order="F")
        if not self.Meteo.precip.shape == (self.Domain.time.nts, self.Domain.space.ne):
            try:
                self.Meteo.precip = self.Meteo.precip.reshape(self.Domain.time.nts, self.Domain.space.ne)
            except ValueError:
                log.error(f"Meteo.precip must be an array of shape ({self.Domain.time.nts},{self.Domain.space.ne}).")
                print(f"Meteo.precip must be an array of shape ({self.Domain.time.nts},{self.Domain.space.ne}).")
                sys.exit(1)
        self.Meteo.pet = np.array(self.Meteo.pet, dtype=self.fprecesion, order="F")
        if not self.Meteo.pet.shape == (self.Domain.time.nts, self.Domain.space.ne):
            try:
                self.Meteo.pet = self.Meteo.pet.reshape(self.Domain.time.nts, self.Domain.space.ne)
            except ValueError:
                log.error(f"Meteo.pet must be an array of shape ({self.Domain.time.nts},{self.Domain.space.ne}).")
                print(f"Meteo.pet must be an array of shape ({self.Domain.time.nts},{self.Domain.space.ne}).")
                sys.exit(1)

        if self.Paths.f2pywrapper:
            if self.Paths.f2pywrapper != f2py_wrapper.__file__:
                re_import("f2pywrapper", self.Paths.f2pywrapper)
                from importHandler import gwswex as f2pywrapper
        
        self.__proofed = True

    def __set_defaults(self) -> None:
        """Sets default values if not defined."""

        self.Solver.omp_cores = str(psutil.cpu_count(logical=False))
        self.Solver.verbose = False

        self.Paths.root = self.wd
        self.Paths.input = os.path.join(self.Paths.root, "input")
        self.Paths.output = os.path.join(self.Paths.root, "output")
        self.Paths.f2pywrapper = f2py_wrapper.__file__

    @staticmethod
    def __nested_dataclass_to_dict(instance) -> dict:
        result: dict = {}
        for field in fields(instance):
            if not (field.name.startswith('_') or field.name.startswith('__')):
                value = getattr(instance, field.name)
                if is_dataclass(value):
                    result[field.name] = Model.__nested_dataclass_to_dict(value)
                else:
                    result[field.name] = value
                    # if isinstance(value, np.ndarray):
                    #     result[field.name] = value.tolist()
        return result
    
    @staticmethod
    def __prune_dict(d: dict) -> dict:
        if isinstance(d, dict):
            return {k: Model.__prune_dict(v) for k, v in d.items() if v is not None}
        elif isinstance(d, list):
            return [Model.__prune_dict(v) for v in d if v is not None]
        elif isinstance(d, np.ndarray):
            return d.tolist()
        else:
            return d

    def __parse_to_config(self):
        """Parses the model data structure into the configuration file.

        This function parses the required model data structure into
        the configuration file.
        """
        if not self.__req_checked:
            self.__check_req_config()

        self.config = {
            'Domain': Model.__nested_dataclass_to_dict(self.Domain),
            'Ini': Model.__nested_dataclass_to_dict(self.Ini),
            'Meteo': Model.__nested_dataclass_to_dict(self.Meteo),
            'Solver': Model.__nested_dataclass_to_dict(self.Solver),
            'Paths': Model.__nested_dataclass_to_dict(self.Paths),
            'IO': Model.__nested_dataclass_to_dict(self.IO),
        }

        if len(self.Boundaries) != 0:
            self.config["Boundaries"] = Model.__nested_dataclass_to_dict(self.Boundaries)
        
        if len (self.ExtForcings) != 0:
            self.config["ExtForcings"] = Model.__nested_dataclass_to_dict(self.ExtForcings)
        
        parsed_layers = {}
        if isinstance(self.config['Domain']['space']['layers'], list) and len(self.config['Domain']['space']['layers']) == self.Domain.space.nl:
            for lidx, layer in enumerate(self.config['Domain']['space']['layers']):
                layer_tag = f"layer{lidx+1}"
                parsed_layers[layer_tag] = {}
                try:
                    if layer.name:
                        parsed_layers[layer_tag]['name'] = layer.name
                except:
                    pass
                parsed_layers[layer_tag]['isactive'] = layer.isactive
                parsed_layers[layer_tag]['vanG'] = {}
                parsed_layers[layer_tag]['vanG']['alpha'] = layer.vanG.alpha
                parsed_layers[layer_tag]['vanG']['n'] = layer.vanG.n
                try:
                    if layer.vanG.m:
                        parsed_layers[layer_tag]['vanG']['m'] = layer.vanG.m
                except:
                    pass
                parsed_layers[layer_tag]['vanG']['theta_r'] = layer.vanG.theta_r
                parsed_layers[layer_tag]['vanG']['theta_s'] = layer.vanG.theta_s
                try:
                    if layer.vanG.theta_wilt:
                        parsed_layers[layer_tag]['vanG']['theta_wilt'] = layer.vanG.theta_wilt
                    if layer.vanG.theta_crit:
                        parsed_layers[layer_tag]['vanG']['theta_crit'] = layer.vanG.theta_crit
                except:
                    pass
                parsed_layers[layer_tag]['ks'] = layer.ks
                parsed_layers[layer_tag]['porosity'] = layer.porosity
            self.config['Domain']['space']['layers'] = parsed_layers

        self.config['Domain']['time']['dt'] = int(self.Domain.time.dt.total_seconds())
        self.config['Domain']['time']['start'] = self.Domain.time.start.strftime(self.Domain.time._valid_formats[0])
        self.config['Domain']['time']['stop'] = self.Domain.time.start.strftime(self.Domain.time._valid_formats[0])

        # self.config = {key: value for key, value in self.config.items() if value is not None}
        self.config = Model.__prune_dict(self.config)
        del self.config['IO']['logger']

        self.__parsed_to_config = True

    def __write_config(self):
        """Writes a YAML format configuration file.

        Required Attributes:
            Paths.config (str): Path to the configuration file.
        """
        log = self.log

        if not self.__req_checked:
            self.__check_req_config()
            if not self.__req_checked:
                log.error(f"Missing required configuration(s) to initialise model: {self.missing}")
                print(f"Missing required configuration(s) to initialise model: {self.missing}")
                sys.exit(1)
        if not self.__proofed:
            self.proof()
        if not self.__parsed_to_config:
            self.__parse_to_config()

        with open(self.Paths.config, 'w') as f:
            yaml.dump(self.config, f, sort_keys=False, default_flow_style=False)

        log.info(f"Wrote configuration to {self.Paths.config}:\n{self.config}")

    def init(self):
        """Initialises the model data structure with the configuration file.

        This function initialises the required model data structure
        with the configuration file.
        """
        log = self.log

        if not self.__req_checked:
            self.__check_req_config()
            if not self.__req_checked:
                log.error(f"Missing required configuration(s) to initialise model: {self.missing}")
                print(f"Missing required configuration(s) to initialise model: {self.missing}")
                sys.exit(1)

        if not self.__proofed:
            self.__proof_config()
        
        if not self.__initialised:
            self.__set_defaults()
            self.__write_config()

        self.__initialised = True

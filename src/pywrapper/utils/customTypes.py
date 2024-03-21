import sys
import os
import psutil

from dataclasses import dataclass, fields, asdict, is_dataclass
from datetime import datetime, timedelta

import numpy as np


from .customIO import _logger
from .importHandler import yaml
from .importHandler import gwswex as f2py_wrapper
from .importHandler import re_import
from .importHandler import exit_fn

strptime = datetime.strptime

# fmt: off
@dataclass
class _vanG_mdc:
    alpha:          float       # [m^-1]
    n:              float       # [-]
    m:              float       # [-]
    theta_r:        float       # [m^3 m^-3]
    theta_s:        float       # [m^3 m^-3]
    theta_wilt:     float       # [m^3 m^-3]
    theta_crit:     float       # [m^3 m^-3]
    def __init__(self, alpha: float = None, n: float = None, m: float = None, theta_r: float = None, theta_s: float = None, theta_wilt: float = None, theta_crit: float = None):
        self.alpha = alpha
        self.n = n
        self.m = m
        self.theta_r = theta_r
        self.theta_s = theta_s
        self.theta_wilt = theta_wilt
        self.theta_crit = theta_crit


@dataclass
class _layer_mdc:
    name:       str
    isactive:   np.ndarray
    vanG:       _vanG_mdc
    ks:         np.ndarray      # [m s^-1]
    porosity:   np.ndarray      # [m^3 m^-3]
    def __init__(self, name: str = None, isactive: np.ndarray = None, vanG: _vanG_mdc = _vanG_mdc(), ks: np.ndarray = None, porosity: np.ndarray = None):
        self.name = name
        self.isactive = isactive
        self.vanG = vanG
        self.ks = ks
        self.porosity = porosity

@dataclass
class _space_mdc:
    ne:         int             # Number of elements
    nl:         int             # Number of layers
    top:        np.ndarray      # [m]
    bot:        np.ndarray      # [m]
    layers:     list            # List of layer objects
    def __init__(self, ne: int = None, nl: int = None, top: np.ndarray = None, bot: np.ndarray = None, layers: list = []):
        self.ne = ne
        self.nl = nl
        self.top = top
        self.bot = bot
        self.layers = layers

@dataclass
class _time_mdc:
    #TODO, #FIXME: implement variable time-stepping:
    #       1. allow dynamic definition of dt and nts => results can only be stored in a dynamic array in py (np.append)
    #       2. split dt into global_dt and local_dt. exchange at dynamic local_dt but store values at global_dt
    dt:         timedelta       # Time step size [datetime.timedelta]
    start:      datetime        # Start time [datetime.datetime]
    stop:       datetime        # End time [datetime.datetime]
    current:    datetime        # Current time [datetime.datetime]
    nts:        int             # Number of time steps
    _dt:        int             # Time step size [s]
    _start:     int             # Start time [s]
    _stop:      int             # End time [s]
    _current:   int             # Current time [s]
    supported_formats: list

    def __init__(self, dt: timedelta = None, start: datetime = None, stop: datetime = None):
        self.dt = dt
        self.start = start
        self.stop = stop
        self.supported_formats = ['%Y%m%d_%H%M%S', '%Y-%m-%dT%H:%M:%S']
        self.current = None
        self.nts = None
        self._dt = None
        self._start = None
        self._stop = None
        self._current = None
        

@dataclass
class _domain_mdc:
    space:    _space_mdc
    time:   _time_mdc
    def __init__(self, space: _space_mdc = _space_mdc(), time: _time_mdc = _time_mdc()):
        self.space = space
        self.time = time

@dataclass
class _initial_mdc:
    gw:         np.ndarray      # [m^3 m^-2]
    sw:         np.ndarray      # [m^3 m^-2]
    uz:         np.ndarray      # [m^3 m^-2]
    def __init__(self, gw: np.ndarray = None, sw: np.ndarray = None, uz: np.ndarray = None):
        self.gw = gw
        self.sw = sw
        self.uz = uz

@dataclass
class _meteo_mdc:
    precip:     np.ndarray      # [m s^-1]
    pet:        np.ndarray      # [m s^-1]
    aet:        np.ndarray      # [m s^-1]
    evap:       np.ndarray      # [m s^-1]
    temp:       np.ndarray      # [K]
    rh:         np.ndarray      # [-, %]
    wind:       np.ndarray      # [m s^-1]
    rad_in:     np.ndarray      # [W m^-2]
    rad_out:    np.ndarray      # [W m^-2]
    def __init__(self, precip: np.ndarray = None, pet: np.ndarray = None, aet: np.ndarray = None, evap: np.ndarray = None, temp: np.ndarray = None, rh: np.ndarray = None, wind: np.ndarray = None, rad_in: np.ndarray = None, rad_out: np.ndarray = None):
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
class _boundary_mdc:
    name:       str
    type:       str
    value:      np.ndarray
    time:       np.ndarray      # [s] since start

@dataclass
class _forcing_mdc:
    name:       str
    type:       str
    value:      np.ndarray
    time:       np.ndarray      # [s] since start

@dataclass
class _solver_mdc:
    verbose:        bool
    omp_cores:      str
    integrator:     str
    nts_per_flux:   np.ndarray
    def __init__(self, verbose: bool = None, omp_cores: str = None, integrator: str = None, nts_per_flux: np.ndarray = None):
        self.verbose = verbose
        self.omp_cores = omp_cores
        self.integrator = integrator
        self.nts_per_flux = nts_per_flux

@dataclass
class _paths_mdc:
    output:         str
    input:          str
    logfile:        str
    config:         str
    f2pywrapper:    str
    def __init__(self, output: str = None, input: str = None, logfile: str = None, config: str = None, f2pywrapper: str = None):
        self.output = output
        self.input = input
        self.logfile = logfile
        self.config = config
        self.f2pywrapper = f2pywrapper

@dataclass
class _io_mdc:
    verbose:        bool
    debug:          bool
    logger:         _logger
    def __init__(self, verbose: bool = None, debug: bool = None, logger: _logger = None):
        self.verbose = verbose
        self.debug = debug
        self.logger = logger
# fmt: on


class model:
    """Top-Level Class for the GWSWEX model."""

    def __infer_config_keys(self):
        self.__cnf_keys: dict = {}
        self.__cnf_keys["domain"] = {}
        self.__cnf_keys["domain"]["space"] = {
            ".": [field.name for field in fields(_space_mdc)]
        }
        self.__cnf_keys["domain"]["time"] = {
            ".": [field.name for field in fields(_time_mdc)]
        }
        self.__cnf_keys["domain"]["space"]["layer"] = {
            ".": [field.name for field in fields(_layer_mdc)]
        }
        self.__cnf_keys["domain"]["space"]["layer"]["vanG"] = [
            field.name for field in fields(_vanG_mdc)
        ]
        self.__cnf_keys["initial"] = [field.name for field in fields(_initial_mdc)]
        self.__cnf_keys["meteo"] = [field.name for field in fields(_meteo_mdc)]
        self.__cnf_keys["boundaries"] = [field.name for field in fields(_boundary_mdc)]
        self.__cnf_keys["forcing"] = [field.name for field in fields(_forcing_mdc)]
        self.__cnf_keys["solver"] = [field.name for field in fields(_solver_mdc)]
        self.__cnf_keys["paths"] = [field.name for field in fields(_paths_mdc)]
        self.__cnf_keys["io"] = [field.name for field in fields(_io_mdc)]

    def __init__(
        self,
        fpath: str,
        logfile: str = None,
        verbose: bool = False,
        debug: bool = False,
    ):
        """Initializes the GWSWEX model data structure.

        Args:
            fpath (str): Path to the configuration file.

        Optional Args:
            logfile (str): Path to the log file.
            verbose (bool): Verbose logging.
            debug (bool): Debug logging.

        Required Attributes:
            config (dict): Configuration dictionary
            domain (_domain_mdc): Model domain configuration
            meteo (_meteo_mdc): Meteorological conditions
            initial (_initial_mdc): Initial conditions
            forcings (dict): External Forcings
            solver (_solver_mdc): Solver configuration
            paths (_paths_mdc): File paths
            io (_io_mdc): Input/Output configuration

        Optional Attributes:
            log: _logger
            boundaries (dict): Boundary conditions

        """
        self.__infer_config_keys()

        self.__cnf_req_keys: dict = {}
        self.__cnf_req_keys = {".": ["domain", "initial", "meteo"]}
        self.__cnf_req_keys["domain"] = {".": ["space", "time"]}
        self.__cnf_req_keys["domain"]["space"] = {".": ["ne", "nl", "top", "bot"]}
        self.__cnf_req_keys["domain"]["space"]["layer"] = {
            ".": ["isactive", "vanG", "ks", "porosity"]
        }
        self.__cnf_req_keys["domain"]["space"]["layer"]["vanG"] = [
            "alpha",
            "n",
            "theta_r",
            "theta_s",
        ]
        self.__cnf_req_keys["domain"]["time"] = ["dt", "start", "stop"]
        self.__cnf_req_keys["initial"] = ["gw", "sw"]
        self.__cnf_req_keys["meteo"] = ["precip", "pet"]
        self.__cnf_req_keys["boundaries"] = ["type", "value", "time"]
        self.__cnf_req_keys["forcing"] = ["type", "value", "time"]
        self.__cnf_req_keys["solver"] = []
        self.__cnf_req_keys["paths"] = []
        self.__cnf_req_keys["io"] = []

        self.config: dict = {}

        self.domain: _domain_mdc = _domain_mdc()
        self.initial: _initial_mdc = _initial_mdc()
        self.meteo: _meteo_mdc = _meteo_mdc()
        self.boundaries: dict = {}
        self.forcings: dict = {}
        self.solver: _solver_mdc = _solver_mdc()
        self.paths: _paths_mdc = _paths_mdc()
        self.io: _io_mdc = _io_mdc()
        self.log: _logger

        self.config_file = os.path.abspath(fpath)
        self.wd = os.path.dirname(self.config_file)
        self.paths.config = os.path.abspath(fpath)

        if not logfile:
            logfile = "./gwswex_pywrapper.log"
        if debug:
            level = 10
        if verbose:
            level = 20
        else:
            level = 30
        self.log = _logger(os.path.abspath(logfile), level=level)
        self.log.critical(f"Model configuration file: {self.config_file}")

        self.paths.logfile = logfile
        self.io.logger = self.log
        self.io.verbose = verbose
        self.io.debug = debug

        if os.path.exists(fpath):
            self.__read_config()

    def __check_req_config_dict(self) -> bool:
        """Checks the configuration dictionary for required keys.

        Required Attributes:
            config (dict): Configuration dictionary
        """
        log = self.log
        config = self.config
        config_path = self.paths.config

        for key in self.__cnf_req_keys["."]:
            try:
                config[key]
            except KeyError:
                log.error(
                    f"Model configuration '{key}' was not found in {config_path}."
                )
                return False

        for key in self.__cnf_req_keys["domain"]['.']:
            try:
                config["domain"][key]
            except KeyError:
                log.error(
                    f"Model [domain] configuration '{key}' was not found in {config_path}."
                )
                return False

        for key in self.__cnf_req_keys["domain"]["space"]["."]:
            try:
                config["domain"]["space"][key]
            except KeyError:
                log.error(
                    f"Model [domain.space] configuration '{key}' was not found in {config_path}."
                )
                return False

        for key in self.__cnf_req_keys["domain"]["time"]:
            try:
                config["domain"]["time"][key]
            except KeyError:
                log.error(
                    f"Model [domain.time] configuration '{key}' was not found in {config_path}."
                )
                return False

        for layer in [
            f"layer{lidx}" for lidx in range(1, config["domain"]["nlayers"] + 1)
        ]:
            try:
                config["domain"][key]
                for key in self.__cnf_req_keys["domain"]["space"]["layer"]["."]:
                    try:
                        config[layer][key]
                        for key in self.__cnf_req_keys["domain"]["space"]["layer"][
                            "vanG"
                        ]:
                            try:
                                config[layer]["vanG"][key]
                            except KeyError:
                                log.error(
                                    f"Configuration [vanG.{key}] for '{layer}' was not found in {config_path}."
                                )
                                return False
                    except KeyError:
                        log.error(
                            f"Configuration [{key}] for '{layer}' was not found in {config_path}."
                        )
                        return False
            except KeyError:
                log.error(
                    f"Configuration for '{layer}' was not found in {config_path}."
                )
                return False

        for key in self.__cnf_req_keys["initial"]:
            try:
                config["initial"][key]
            except KeyError:
                log.error(
                    f"Model [initial] configuration '{key}' was not found in {config_path}."
                )
                return False

        for key in self.__cnf_req_keys["meteo"]:
            try:
                config["meteo"][key]
            except KeyError:
                log.error(
                    f"Model [meteo] configuration '{key}' was not found in {config_path}."
                )
                return False

        return True

    def __parse_from_config(self):
        """Parses the configuration file into the model data structure.

        Required Attributes:
            config (dict): Configuration dictionary
        """
        log = self.log
        config = self.config

        layer_keys = [
            f"layer{lidx}" for lidx in range(1, config["domain"]["nlayers"] + 1)
        ]
        layers = []
        for layer in layer_keys:
            layers.append(_layer_mdc())

        self.domain = _domain_mdc()
        for key in self.__cnf_keys["domain"]["."]:
            try:
                setattr(self.domain, key, config["domain"][key])
            except KeyError:
                if not key in self.__cnf_req_keys["domain"]:
                    log.warning(
                        f"Model [domain] configuration '{key}' was not found in {self.paths.config}."
                    )
                else:
                    log.error(
                        f"Model [domain] configuration '{key}' was not found in {self.paths.config}."
                    )
                    raise
            except ValueError:
                if (
                    type(config["domain"][key]) != type(getattr(self.domain, key))
                    and type(config["domain"][key]) == str
                ):
                    log.debug(
                        f"Expected type {type(getattr(self.domain, key))} for [model.domain.{key}]. Trying to read from file, assuming filename was provided."
                    )
                    try:
                        data = np.loadtxt(config["domain"][key])
                        setattr(self.domain, key, data)
                    except:
                        log.error(
                            f"Unable to read model [domain] configuration '{key}' either as {type(getattr(self.domain, key))} or as a file with np.loadtxt."
                        )
                        raise

        for key in self.__cnf_keys["domain"]["space"]["."]:
            try:
                setattr(self.domain.space, key, config["domain"]["space"][key])
            except KeyError:
                if not key in self.__cnf_req_keys["domain"]["space"]["."]:
                    log.warning(
                        f"Model [domain.space] configuration '{key}' was not found in {self.paths.config}."
                    )
                else:
                    log.error(
                        f"Model [domain.space] configuration '{key}' was not found in {self.paths.config}."
                    )
                    raise
            except ValueError:
                if (
                    type(config["domain"]["space"][key])
                    != type(getattr(self.domain.space, key))
                    and type(config["domain"]["space"][key]) == str
                ):
                    log.debug(
                        f"Expected type {type(getattr(self.domain.space, key))} for [model.domain.space.{key}]. Trying to read from file, assuming filename was provided."
                    )
                    try:
                        data = np.loadtxt(config["domain"]["space"][key])
                        setattr(self.domain.space, key, data)
                    except:
                        log.error(
                            f"Unable to read model [domain].space configuration '{key}' either as {type(getattr(self.domain.space, key))} or as a file with np.loadtxt."
                        )
                        raise

        for key in self.__cnf_keys["domain"]["time"]:
            try:
                setattr(self.domain.time, key, config["domain"]["time"][key])
            except KeyError:
                if not key in self.__cnf_req_keys["domain"]["time"]:
                    log.warning(
                        f"Model [domain.time] configuration '{key}' was not found in {self.paths.config}."
                    )
                else:
                    log.error(
                        f"Model [domain.time] configuration '{key}' was not found in {self.paths.config}."
                    )
                raise
            except ValueError:
                if key == "dt" and type(config["domain"]["time"][key]) in [
                    int,
                    float,
                    str,
                ]:
                    try:
                        setattr(
                            self.domain.time,
                            key,
                            timedelta(seconds=int(config["domain"]["time"][key])),
                        )
                    except:
                        log.error(
                            f"Expected type {type(getattr(self.domain.time, key))} for [model.domain.time.{key}] but got {type(config['domain']['time'][key])}."
                        )
                        raise
                if key == "start" and type(config["domain"]["time"][key]) == str:
                    for tformat in self.domain.time.supported_formats:
                        try:
                            setattr(
                                self.domain.time,
                                key,
                                strptime(config["domain"]["time"][key]),
                                tformat,
                            )
                        except:
                            log.error(
                                f"Unsupported datetime format {tformat} for [model.domain.time.{key}]. Supported formats: {self.domain.time.supported_formats}."
                            )
                            raise
                if key == "stop" and type(config["domain"]["time"][key]) == str:
                    for tformat in self.domain.time.supported_formats:
                        try:
                            setattr(
                                self.domain.time,
                                key,
                                strptime(config["domain"]["time"][key]),
                                tformat,
                            )
                        except:
                            log.error(
                                f"Unsupported datetime format {tformat} for [model.domain.time.{key}]. Supported formats: {self.domain.time.supported_formats}."
                            )
                            raise
                else:
                    log.error(
                        f"Expected type {type(getattr(self.domain.time, key))} for [model.domain.time.{key}] but got {type(config['domain']['time'][key])}."
                    )
                    raise

        for layer in layers:
            for key in self.__cnf_keys["domain"]["space"]["layer"]["."]:
                try:
                    setattr(layer, key, config["domain"]["space"]["layer"][key])
                    for key in self.__cnf_keys["domain"]["space"]["layer"]["vanG"]:
                        try:
                            setattr(
                                layer.vanG,
                                key,
                                config["domain"]["space"]["layer"]["vanG"][key],
                            )
                        except KeyError:
                            if (
                                not key
                                in self.__cnf_req_keys["domain"]["space"]["layer"][
                                    "vanG"
                                ]
                            ):
                                log.warning(
                                    f"Model [domain.space.layer] configuration '{key}' was not found in {self.paths.config}."
                                )
                            else:
                                log.error(
                                    f"Model [domain.space.layer] configuration '{key}' was not found in {self.paths.config}."
                                )
                                raise
                        except ValueError:
                            log.error(
                                f"Expected type {type(getattr(layer.vanG, key))} for [model.domain.space.layer.vanG.{key}] but got {type(config['domain']['space']['layer']['vanG'][key])}."
                            )
                            raise

                except KeyError:
                    if not key in self.__cnf_req_keys["domain"]["space"]["layer"]["."]:
                        log.warning(
                            f"Model [domain] configuration '{key}' was not found in {self.paths.config}."
                        )
                    else:
                        log.error(
                            f"Model [domain] configuration '{key}' was not found in {self.paths.config}."
                        )
                        raise
                except ValueError:
                    if (
                        type(config["domain"]["space"]["layer"][key])
                        != type(getattr(self.domain.space.layer, key))
                        and type(config["domain"]["space"]["layer"][key]) == str
                    ):
                        log.debug(
                            f"Expected type {type(getattr(self.domain.space.layer, key))} for [model.domain.space.layer.{key}]. Trying to read from file, assuming filename was provided."
                        )
                        try:
                            data = np.loadtxt(config["domain"]["space"]["layer"][key])
                            setattr(layer, key, data)
                        except:
                            log.error(
                                f"Unable to read model [domain] configuration '{key}' either as {type(getattr(self.domain.space.layer, key))} or as a file with np.loadtxt."
                            )
                            raise

        self.initial = _initial_mdc()
        for key in self.__cnf_keys["initial"]:
            try:
                setattr(self.initial, key, config["initial"][key])
            except KeyError:
                if not key in self.__cnf_req_keys["initial"]:
                    log.warning(
                        f"Model [initial] configuration '{key}' was not found in {self.paths.config}."
                    )
                else:
                    log.error(
                        f"Model [initial] configuration '{key}' was not found in {self.paths.config}."
                    )
                    raise
            except ValueError:
                if (
                    type(config["initial"][key]) != type(getattr(self.initial, key))
                    and type(config["initial"][key]) == str
                ):
                    log.debug(
                        f"Expected type {type(getattr(self.initial, key))} for [model.initial.{key}]. Trying to read from file, assuming filename was provided."
                    )
                    try:
                        data = np.loadtxt(config["initial"][key])
                        setattr(self.initial, key, data)
                    except:
                        log.error(
                            f"Unable to read model [initial] configuration '{key}' either as {type(getattr(self.initial, key))} or as a file with np.loadtxt."
                        )
                        raise

        self.meteo = _meteo_mdc()
        for key in self.__cnf_keys["meteo"]:
            try:
                setattr(self.meteo, key, config["meteo"][key])
            except KeyError:
                if not key in self.__cnf_req_keys["meteo"]:
                    log.warning(
                        f"Model [meteo] configuration '{key}' was not found in {self.paths.config}."
                    )
                else:
                    log.error(
                        f"Model [meteo] configuration '{key}' was not found in {self.paths.config}."
                    )
                    raise
            except ValueError:
                if (
                    type(config["meteo"][key]) != type(getattr(self.meteo, key))
                    and type(config["meteo"][key]) == str
                ):
                    log.debug(
                        f"Expected type {type(getattr(self.meteo, key))} for [model.meteo.{key}]. Trying to read from file, assuming filename was provided."
                    )
                    try:
                        data = np.loadtxt(config["meteo"][key])
                        setattr(self.meteo, key, data)
                    except:
                        log.error(
                            f"Unable to read model [meteo] configuration '{key}' either as {type(getattr(self.meteo, key))} or as a file with np.loadtxt."
                        )
                        raise

        self.boundaries = {}
        if config["boundaries"]:
            for key, val in config["boundaries"].items():
                self.boundaries[key] = _boundary_mdc(name=key)
                for key in self.__cnf_keys["boundaries"][key]:
                    try:
                        setattr(self.boundaries[key], key, val[key])
                    except KeyError:
                        if not key in self.__cnf_req_keys["boundaries"][key]:
                            log.warning(
                                f"Model [boundaries] configuration '{key}' was not found in {self.paths.config}."
                            )
                        else:
                            log.error(
                                f"Model [boundaries] configuration '{key}' was not found in {self.paths.config}."
                            )
                            raise
                    except ValueError:
                        if (
                            type(val[key]) != type(getattr(self.boundaries[key], key))
                            and type(val[key]) == str
                        ):
                            log.debug(
                                f"Expected type {type(getattr(self.boundaries[key], key))} for [model.boundaries.{key}]. Trying to read from file, assuming filename was provided."
                            )
                            try:
                                data = np.loadtxt(val[key])
                                setattr(self.boundaries[key], key, data)
                            except:
                                log.error(
                                    f"Unable to read model [boundaries] configuration '{key}' either as {type(getattr(self.boundaries[key], key))} or as a file with np.loadtxt."
                                )
                                raise

        self.forcings = {}
        for key, value in config["forcings"].items():
            self.forcings[key] = _forcing_mdc(name=key)
            for key in self.__cnf_keys["forcings"][key]:
                try:
                    setattr(self.forcings[key], key, value[key])
                except KeyError:
                    if not key in self.__cnf_req_keys["forcings"][key]:
                        log.warning(
                            f"Model [forcings] configuration '{key}' was not found in {self.paths.config}."
                        )
                    else:
                        log.error(
                            f"Model [forcings] configuration '{key}' was not found in {self.paths.config}."
                        )
                        raise
                except ValueError:
                    if (
                        type(value[key]) != type(getattr(self.forcings[key], key))
                        and type(value[key]) == str
                    ):
                        log.debug(
                            f"Expected type {type(getattr(self.forcings[key], key))} for [model.forcings.{key}]. Trying to read from file, assuming filename was provided."
                        )
                        try:
                            data = np.loadtxt(value[key])
                            setattr(self.forcings[key], key, data)
                        except:
                            log.error(
                                f"Unable to read model [forcings] configuration '{key}' either as {type(getattr(self.forcings[key], key))} or as a file with np.loadtxt."
                            )
                            raise

        self.solver = _solver_mdc()
        for key in self.__cnf_keys["solver"]:
            try:
                setattr(self.solver, key, config["solver"][key])
            except KeyError:
                if not key in self.__cnf_req_keys["solver"]:
                    log.warning(
                        f"Model [solver] configuration '{key}' was not found in {self.paths.config}."
                    )
                else:
                    log.error(
                        f"Model [solver] configuration '{key}' was not found in {self.paths.config}."
                    )
                    raise
            except ValueError:
                if (
                    type(config["solver"][key]) != type(getattr(self.solver, key))
                    and type(config["solver"][key]) == str
                ):
                    log.debug(
                        f"Expected type {type(getattr(self.solver, key))} for [model.solver.{key}]. Trying to read from file, assuming filename was provided."
                    )
                    try:
                        data = np.loadtxt(config["solver"][key])
                        setattr(self.solver, key, data)
                    except:
                        log.error(
                            f"Unable to read model [solver] configuration '{key}' either as {type(getattr(self.solver, key))} or as a file with np.loadtxt."
                        )
                        raise

        self.paths = _paths_mdc()
        for key in self.__cnf_keys["paths"]:
            try:
                setattr(self.paths, key, config["paths"][key])
            except KeyError:
                if not key in self.__cnf_req_keys["paths"]:
                    log.warning(
                        f"Model [paths] configuration '{key}' was not found in {self.paths.config}."
                    )
                else:
                    log.error(
                        f"Model [paths] configuration '{key}' was not found in {self.paths.config}."
                    )
                    raise
            except ValueError:
                if (
                    type(config["paths"][key]) != type(getattr(self.paths, key))
                    and type(config["paths"][key]) == str
                ):
                    log.debug(
                        f"Expected type {type(getattr(self.paths, key))} for [model.paths.{key}]. Trying to read from file, assuming filename was provided."
                    )
                    try:
                        data = np.loadtxt(config["paths"][key])
                        setattr(self.paths, key, data)
                    except:
                        log.error(
                            f"Unable to read model [paths] configuration '{key}' either as {type(getattr(self.paths, key))} or as a file with np.loadtxt."
                        )
                        raise

        self.io = _io_mdc()
        for key in self.__cnf_keys["io"]:
            try:
                setattr(self.io, key, config["io"][key])
            except KeyError:
                if not key in self.__cnf_req_keys["io"]:
                    log.warning(
                        f"Model [io] configuration '{key}' was not found in {self.paths.config}."
                    )
                else:
                    log.error(
                        f"Model [io] configuration '{key}' was not found in {self.paths.config}."
                    )
                    raise
            except ValueError:
                if (
                    type(config["io"][key]) != type(getattr(self.io, key))
                    and type(config["io"][key]) == str
                ):
                    log.debug(
                        f"Expected type {type(getattr(self.io, key))} for [model.io.{key}]. Trying to read from file, assuming filename was provided."
                    )
                    try:
                        data = np.loadtxt(config["io"][key])
                        setattr(self.io, key, data)
                    except:
                        log.error(
                            f"Unable to read model [io] configuration '{key}' either as {type(getattr(self.io, key))} or as a file with np.loadtxt."
                        )
                        raise

    def __read_config(self):
        """Reads a YAML format configuration file.

        Required Attributes:
            paths.config (str): Path to the configuration file.
        """
        log = self.log

        config_path = self.paths.config
        try:
            with open(config_path, "r") as f:
                self.config = yaml.load(f, Loader=yaml.FullLoader)
                log.info(f"Read configuration from {config_path}:\n{self.config}")
        except FileNotFoundError:
            log.error(f"Configuration file not found: {config_path}")
            print(f"Configuration file not found: {config_path}")
            exit_fn(1)
        except Exception as e:
            log.error(f"Error reading configuration file: {config_path}")
            log.error(e)
            raise

        if not self.__check_req_config_dict():
            log.error(f"Incomplete configuration file: {config_path}")
            print(
                f"Incomplete configuration file: {config_path}. Check {log.fname} for details."
            )
            exit_fn(1)

        try:
            self.__parse_from_config()
        except KeyError as e:
            log.error(f"Incomplete definitions in configuration file: {config_path}.")
            print(
                f"Incomplete definitions in configuration file: {config_path}. Check {log.fname} for details."
            )
            exit_fn(1)
        except ValueError as e:
            log.error(f"Invalid definitions in configuration file: {config_path}.")
            print(
                f"Invalid definitions in configuration file: {config_path}. Check {log.fname} for details."
            )
            exit_fn(1)
        except Exception as e:
            log.error(f"Error parsing configuration file: {config_path}")
            log.error(e)
            raise

    def __check_req_config(self) -> bool:
        """Checks the model data structure for required attributes.

        Returns True if all required attributes are defined, False otherwise.
        """
        self.missing = []
        log = self.log
        
        cnf_req_keys = self.__cnf_req_keys

        for key in cnf_req_keys["."]:
            try:
                if getattr(self, key) is None:
                    self.missing.append(key)
                    log.debug(f"Required attribute '{key}' is not defined.")
                    return False
            except AttributeError:
                self.missing.append(key)
                log.debug(f"Required attribute '{key}' is not defined.")
                return False

        for key in cnf_req_keys["domain"]['.']:
            try:
                if getattr(self.domain, key) is None:
                    self.missing.append(f"domain.{key}")
                    log.debug(f"Required attribute domain.'{key}' is not defined.")
                    return False
            except AttributeError:
                self.missing.append(f"domain.{key}")
                log.debug(f"Required attribute domain.'{key}' is not defined.")
                return False
        for key in cnf_req_keys["domain"]["space"]["."]:
            try:
                if getattr(self.domain.space, key) is None:
                    self.missing.append(f"domain.space.{key}")
                    log.debug(f"Required attribute domain.space.'{key}' is not defined.")
                    return False
            except AttributeError:
                self.missing.append(f"domain.space.{key}")
                log.debug(f"Required attribute domain.space.'{key}' is not defined.")
                return False
        if not len(self.domain.space.layers) == self.domain.space.nl:
            self.missing.append(f"domain.space.layers")
            log.debug(f"Required attribute domain.space.layers is not defined.")
            return False
        for lidx in range(self.domain.space.nl):
            for key in cnf_req_keys["domain"]["space"]["layer"]["."]:
                try:
                    if getattr(self.domain.space.layers[lidx], key) is None:
                        self.missing.append(f"domain.space.layer[{lidx}].{key}")
                        log.debug(f"Required attribute domain.space.layer[{lidx}].'{key}' is not defined.")
                        return False
                except AttributeError:
                    self.missing.append(f"domain.space.layer[{lidx}].{key}")
                    log.debug(f"Required attribute domain.space.layer[{lidx}].'{key}' is not defined.")
                    return False
                
            for vkey in cnf_req_keys["domain"]["space"]["layer"]["vanG"]:
                try:
                    if getattr(self.domain.space.layers[lidx].vanG, vkey) is None:
                        self.missing.append(f"domain.space.layer[{lidx}].vanG.{vkey}")
                        log.debug(f"Required attribute '{key}.{vkey}' is not defined.")
                        return False
                except AttributeError:
                    self.missing.append(f"domain.space.layer[{lidx}].vanG.{vkey}")
                    log.debug(f"Required attribute '{key}.{vkey}' is not defined.")
                    return False

        for key in cnf_req_keys["domain"]["time"]:
            try:
                if getattr(self.domain.time, key) is None:
                    self.missing.append(f"domain.time.{key}")
                    log.debug(f"Required attribute domain.time.'{key}' is not defined.")
                    return False
            except AttributeError:
                self.missing.append(f"domain.time.{key}")
                log.debug(f"Required attribute domain.time.'{key}' is not defined.")
                return False

        for key in cnf_req_keys["initial"]:
            try:
                if getattr(self.initial, key) is None:
                    self.missing.append(f"initial.{key}")
                    log.debug(f"Required attribute initial.'{key}' is not defined.")
                    return False
            except AttributeError:
                self.missing.append(f"initial.{key}")
                log.debug(f"Required attribute initial.'{key}' is not defined.")
                return False

        for key in cnf_req_keys["meteo"]:
            try:
                if getattr(self.meteo, key) is None:
                    self.missing.append(f"meteo.{key}")
                    log.debug(f"Required attribute meteo.'{key}' is not defined.")
                    return False
            except AttributeError:
                self.missing.append(f"meteo.{key}")
                log.debug(f"Required attribute meteo.'{key}' is not defined.")
                return False

        if self.boundaries:
            for bnd_key, bnd in self.boundaries.items():
                for key in cnf_req_keys["boundaries"]:
                    try:
                        if getattr(bnd, key) is None:
                            self.missing.append(f"boundaries.[{bnd_key}].{key}")
                            log.debug(
                                f"Required attribute boundaries.[{bnd_key}].'{key}' is not defined."
                            )
                            return False
                    except AttributeError:
                        self.missing.append(f"boundaries.[{bnd_key}].{key}")
                        log.debug(
                            f"Required attribute boundaries.[{bnd_key}].'{key}' is not defined."
                        )
                        return False

        if self.forcings:
            for frc_key, frc in self.forcings.items():
                for key in cnf_req_keys["forcings"]:
                    try:
                        if getattr(frc, key) is None:
                            self.missing.append(f"forcings.[{frc_key}].{key}")
                            log.debug(
                                f"Required attribute forcings.[{frc_key}].'{key}' is not defined."
                            )
                            return False
                    except AttributeError:
                        self.missing.append(f"forcings.[{frc_key}].{key}")
                        log.debug(
                            f"Required attribute forcings.[{frc_key}].'{key}' is not defined."
                        )
                        return False

        for key in cnf_req_keys["solver"]:
            try:
                if getattr(self.solver, key) is None:
                    self.missing.append(f"solver.{key}")
                    log.debug(f"Required attribute solver.'{key}' is not defined.")
                    return False
            except AttributeError:
                self.missing.append(f"solver.{key}")
                log.debug(f"Required attribute solver.'{key}' is not defined.")
                return False

        for key in cnf_req_keys["paths"]:
            try:
                if getattr(self.paths, key) is None:
                    self.missing.append(f"paths.{key}")
                    log.debug(f"Required attribute paths.'{key}' is not defined.")
                    return False
            except AttributeError:
                self.missing.append(f"paths.{key}")
                log.debug(f"Required attribute paths.'{key}' is not defined.")
                return False

        for key in cnf_req_keys["io"]:
            try:
                if getattr(self.io, key) is None:
                    self.missing.append(f"io.{key}")
                    log.debug(f"Required attribute io.'{key}' is not defined.")
                    return False
            except AttributeError:
                self.missing.append(f"io.{key}")
                log.debug(f"Required attribute io.'{key}' is not defined.")
                return False
        return True
    
    def __proof_config(self) -> None:
        """Checks if the model configuration is complete.

        Returns True if all required attributes are defined, False otherwise.
        """
        #TODO: check for np.ndarray dtype and order and convert if necessary
        log = self.log

        self.strlen = int(f2py_wrapper.strlen)

        self.domain.space.ne = int(self.domain.space.ne)
        self.domain.space.nl = int(self.domain.space.nl)
        ne, nl = self.domain.space.ne, self.domain.space.nl

#FIXME: exits with err even if datetime
        if (
            not type(self.domain.time.start) == datetime
            and type(self.domain.time.start) == str
        ):
            for tformat in self.domain.time.supported_formats:
                try:
                    self.domain.time.start = datetime.strptime(
                        self.domain.time.start, tformat
                    )
                except ValueError:
                    log.error(
                        f"Unsupported domain.time.start format. Supported formats: {self.domain.time.supported_formats}."
                    )
                    print(
                        f"Unsupported domain.time.start format. Supported formats: {self.domain.time.supported_formats}."
                    )
                    exit_fn(1)
        else:
            log.error(
                f"domain.time.start must be a datetime object or a string in {self.domain.time.supported_formats}."
            )
            print(
                f"domain.time.start must be a datetime object or a string in {self.domain.time.supported_formats}."
            )
            exit_fn(1)
        if (
            not type(self.domain.time.stop) == datetime
            and type(self.domain.time.stop) == str
        ):
            for tformat in self.domain.time.supported_formats:
                try:
                    self.domain.time.stop = datetime.strptime(
                        self.domain.time.stop, tformat
                    )
                except ValueError:
                    log.error(
                        f"Unsupported domain.time.stop format. Supported formats: {self.domain.time.supported_formats}."
                    )
                    print(
                        f"Unsupported domain.time.stop format. Supported formats: {self.domain.time.supported_formats}."
                    )
                    exit_fn(1)
        else:
            log.error(
                f"domain.time.stop must be a datetime object or a string in {self.domain.time.supported_formats}."
            )
            print(
                f"domain.time.stop must be a datetime object or a string in {self.domain.time.supported_formats}."
            )
            exit_fn(1)

        if not type(self.domain.time.dt) == timedelta and type(self.domain.time.dt) in [
            int,
            float,
        ]:
            for tformat in self.domain.time.supported_formats:
                try:
                    if type(self.domain.time.dt) == float:
                        log.critical(
                            f"discarding fractional seconds in domain.time.dt."
                        )
                    self.domain.time.dt = timedelta(seconds=int(self.domain.time.dt))
                except ValueError:
                    log.error(
                        f"domain.time.dt must either be a timedelta object or an int in seconds."
                    )
                    print(
                        f"domain.time.dt must either be a timedelta object or an int in seconds."
                    )
                    exit_fn(1)
        else:
            log.error(
                f"domain.time.dt must either be a timedelta object or an int in seconds."
            )
            print(
                f"domain.time.dt must either be a timedelta object or an int in seconds."
            )
            exit_fn(1)

        self.domain.time._dt = int(self.domain.time.dt.total_seconds())
        self.domain.time._start = 0
        self.domain.time._stop = int(
            self.domain.time.stop - self.domain.time.start
        ).total_seconds()
        nts = self.domain.time._stop / self.domain.time._dt

        if not self.domain.space.top.size == ne:
            try:
                self.domain.space.top = self.domain.space.top.reshape(ne)
            except ValueError:
                log.error(f"domain.space.top must be an array of size {ne}.")
                print(f"domain.space.top must be an array of size {ne}.")
                exit_fn(1)
        if not self.domain.space.bot.shape == (nl, ne):
            try:
                self.domain.space.bot = self.domain.space.bot.reshape(nl, ne)
            except ValueError:
                log.error(f"domain.space.bot must be an array of shape ({nl},{ne}).")
                print(f"domain.space.bot must be an array of shape ({nl},{ne}).")
                exit_fn(1)

        if not len(self.domain.space.layers) == nl:
            log.error(
                f"Definition of layers does not match the number of defined layers: domain.space.nlayers"
            )
            print(
                f"Definition of layers does not match the number of defined layers: domain.space.nlayers {nl}."
            )
            exit_fn(1)

        for lidx in range(nl):
            if not self.domain.space.layers[lidx].ks.size == ne:
                log.error(
                    f"Shape of ks for layer[{lidx}] does not match domain.space.ne."
                )
                print(f"Shape of ks for layer[{lidx}] does not match domain.space.ne.")
                exit_fn(1)
            if not self.domain.space.layers[lidx].porosity.size == ne:
                log.error(
                    f"Shape of porosity for layer[{lidx}] does not match domain.space.ne."
                )
                print(
                    f"Shape of porosity for layer[{lidx}] does not match domain.space.ne."
                )
                exit_fn(1)

        if not self.initial.gw.size == ne:
            try:
                self.initial.gw = self.initial.gw.reshape(ne)
            except ValueError:
                log.error(f"initial.gw must be an array of size {ne}.")
                print(f"initial.gw must be an array of size {ne}.")
                exit_fn(1)
        if not self.boundaries.sw.size == ne:
            try:
                self.boundaries.sw = self.boundaries.sw.reshape(ne)
            except ValueError:
                log.error(f"boundaries.sw must be an array of size {ne}.")
                print(f"boundaries.sw must be an array of size {ne}.")
                exit_fn(1)

        if not self.meteo.precip.shape == (nts, ne):
            try:
                self.meteo.precip = self.meteo.precip.reshape(nts, ne)
            except ValueError:
                log.error(f"meteo.precip must be an array of shape ({nts},{ne}).")
                print(f"meteo.precip must be an array of shape ({nts},{ne}).")
                exit_fn(1)
        if not self.meteo.pet.shape == (nts, ne):
            try:
                self.meteo.pet = self.meteo.pet.reshape(nts, ne)
            except ValueError:
                log.error(f"meteo.pet must be an array of shape ({nts},{ne}).")
                print(f"meteo.pet must be an array of shape ({nts},{ne}).")
                exit_fn(1)

        if self.paths.f2pywrapper:
            re_import("f2pywrapper", self.paths.f2pywrapper)
            from importHandler import gwswex as f2pywrapper

    def __set_defaults(self) -> None:
        """Sets default values if not defined."""

        self.solver.omp_cores = str(psutil.cpu_count(logical=False))
        self.solver.verbose = False

        self.paths.root = self.wd
        self.paths.input = os.path.join(self.paths.root, "input")
        self.paths.output = os.path.join(self.paths.root, "output")
        self.paths.f2pywrapper = f2py_wrapper.__file__

    def __nested_dataclass_to_dict(self, instance):
        result = {}
        for field in fields(instance):
            value = getattr(instance, field.name)
            if is_dataclass(value):
                result[field.name] = self.__nested_dataclass_to_dict(value)
            else:
                result[field.name] = value
        return result

    def __parse_to_config(self):
        """Parses the model data structure into the configuration file.

        This function parses the required model data structure into
        the configuration file.
        """
        if not self.__registered:
            self.register()

        self.config = {
            "domain": self.__nested_dataclass_to_dict(self.domain),
            "initial": self.__nested_dataclass_to_dict(self.initial),
            "meteo": self.__nested_dataclass_to_dict(self.meteo),
            "boundaries": self.__nested_dataclass_to_dict(self.boundaries),
            "forcing": self.__nested_dataclass_to_dict(self.forcing),
            "solver": self.__nested_dataclass_to_dict(self.solver),
            "paths": self.__nested_dataclass_to_dict(self.paths),
            "io": self.__nested_dataclass_to_dict(self.io),
        }

    def __write_config(self):
        """Writes a YAML format configuration file.

        Required Attributes:
            paths.config (str): Path to the configuration file.
        """
        log = self.log

        if not self.check_config():
            log.error(f"Incomplete configuration.")
            raise ValueError(
                f"Incomplete configuration. Check {log.fname} for details."
            )

        with open(self.paths.config, "w") as f:
            yaml.dump(self.config, f, sort_keys=False)

        log.info(f"Wrote configuration to {self.paths.config}:\n{self.config}")

    def register(self):
        """Registers the model data structure with the configuration file.

        This function registers the required model data structure
        with the configuration file.
        """
        log = self.log

        if not self.__check_req_config():
            log.error(
                f"Missing required configuration(s) to register model: {self.missing}"
            )
            print(
                f"Missing required configuration(s) to register model: {self.missing}"
            )
            exit_fn(1)

        self.__proof_config()
        self.__set_defaults()

        self.__write_config()

        self.__registered = True

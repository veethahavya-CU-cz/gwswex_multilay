import os, sys
from ctypes import *

import numpy as np
from scipy.io import FortranFile

import yaml
from dataclasses import dataclass

from datetime import datetime, timedelta

import matplotlib.pyplot as plt


try:
    sys.path.append(os.path.abspath("/opt/gwswex/lib/"))
    from gwswex_f2pywrapper import gwswex
except ImportError:
    try:
        sys.path.append(os.path.abspath("../../lib/"))
        from gwswex_f2pywrapper import gwswex
    except ImportError:
        raise ModuleNotFoundError(
            f"Could not find the gwswex_f2pywrapper module. Expected locations: /opt/gwswex/lib/ or {os.abspath('../../lib/')}"
        )


strlen = int(gwswex.strlen)


class GWSWEX:
    """Top-Level Class for the GWSWEX model."""

    def __prase_config(self):
        self.config_dict = self.config

        @dataclass
        class config:
            @dataclass
            class model:
                @dataclass
                class domain:
                    @dataclass
                    class layers:
                        @dataclass
                        class layer:
                            name: str
                            isactive: bool
                            @dataclass
                            class vanG:
                                theta_r: float
                                theta_s: float
                                alpha: float
                                n: float
                                m: float
                            ks: str
                            porosity: str

    def __init__(
        self, config_file: str, null_config: bool = False, verbose: bool = False
    ):
        """Initializes the GWSWEX model.

        Args:
            config_file (str): The path to the configuration file.
            null_config (bool, optional): If True, the configuration file will be ignored. Defaults to False.
            verbose (bool, optional): If True, additional information will be printed during initialization. Defaults to False.
        """
        self.config_file = config_file
        self.null_config = null_config
        self.verbose = verbose

        if not self.null_config:
            with open(self.config_file, "r") as file:
                self.config = yaml.load(file, Loader=yaml.FullLoader)

            print(self.config)

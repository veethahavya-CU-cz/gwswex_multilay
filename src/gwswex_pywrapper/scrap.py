import os, sys
from ctypes import *

import numpy as np
from scipy.io import FortranFile

import yaml
from dataclasses import dataclass

from datetime import datetime, timedelta

import matplotlib.pyplot as plt




from gwswex_f2pywrapper import gwswex
# try:
#     sys.path.append(os.path.abspath('/opt/gwswex/lib/'))
#     from gwswex_f2pywrapper import gwswex
# except ImportError:
#     try:
#         sys.path.append(os.path.abspath('./lib/'))
#         from gwswex_f2pywrapper import gwswex
#     except ImportError:
#         raise ModuleNotFoundError(f"Could not find the gwswex_f2pywrapper module. Expected locations: /opt/gwswex/lib/ or {os.abspath('../../lib/')}")
    

gw_ini = np.zeros(50000, dtype=np.float64, order='F')
sw_ini = np.zeros(50000, dtype=np.float64, order='F')

gwswex.init(os.path.abspath('./test/test.yml'), gw_ini, sw_ini)
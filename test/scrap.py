import os, sys
from ctypes import *

import numpy as np
from scipy.io import FortranFile

import yaml
from dataclasses import dataclass

from datetime import datetime, timedelta

import matplotlib.pyplot as plt



# sys.path.append(os.path.abspath('/opt/gwswex/lib/'))
sys.path.append(os.path.abspath('../build/lib/'))

from gwswex_f2pywrapper import gwswex
    

gw_ini = np.zeros(1, dtype=np.float64, order='F')
sw_ini = np.zeros(1, dtype=np.float64, order='F')

gwswex.init(os.path.abspath('./gwswex.yml'), gw_ini, sw_ini)

gwswex.run()
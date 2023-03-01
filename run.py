import numpy as np
from ctypes import *
import os, psutil, sys
from scipy.io import FortranFile
from dataclasses import dataclass
from datetime import datetime, timedelta

# %%
os.environ['OMP_NUM_THREADS'] = str(psutil.cpu_count(logical = False))

sys.path.append(os.path.abspath('libs/'))
from gwswex_wrapper import gwswex as GWSWEX


Fyaml = create_string_buffer(b'/home/gwswex_dev/gwswex_multilay/gwswex.yml', 256)

#%%
elems = int(1)
nlay = 3

Gnts = int(24*30*6) #one every hour for 6 months
Gdt = 3600
tstart = datetime(2020, 1, 1, 0, 0, 0)
tstop = tstart + timedelta(seconds=Gnts*Gdt)

@dataclass
class pvanGI:
	theta_r: np.float64 = 0.02
	theta_s: np.float64 = 0.42
	alpha: np.float64 = 0.35
	n: np.float64 = 1.25

top = np.full(elems, 150, dtype=np.float64, order='F')
bot = np.full((nlay, elems), 0, dtype=np.float64, order='F')
bot[0] = top - 5
bot[1] = top - 15
bot[2] = top - 30

porosity = np.full(elems, pvanGI.theta_s, dtype=np.float64, order='F')
ks = np.full(elems, 50e-5, dtype=np.float64, order='F')
chd = np.full(elems, 0, dtype=int, order='F')
p = np.full((elems,Gnts+1), 2.5*(1e-3/3600)) #mm/h
p[:,0:500] = 3.5*(1e-3/3600)
p[:,500:750] = 0*(1e-3/3600)
p[:,1000:1250] = 0*(1e-3/3600)
p[:,1000:1250] = 0*(1e-3/3600)
p[:,1750:2000] = 0*(1e-3/3600)
p[:,2100:Gnts] = 0*(1e-3/3600)
et = np.full((elems,Gnts+1), 0.33*(1e-3/3600))

isactive = np.full((nlay, elems), 1, dtype=int, order='F')
gw_ini = np.array(bot[2] + 5, dtype=np.float64, order='F')
sw_ini = np.array(np.random.default_rng().uniform(0, 1e-2, elems), dtype=np.float64, order='F')

#%%
def fwrite(fname, val):

	Ffile = FortranFile(os.path.join(ip_path,fname), 'w')
	Ffile.write_record(val.T)
	Ffile.close()

def fread(fname):
	shape = (elems, Gnts)
	Ffile = FortranFile(os.path.join(op_path,fname), 'r')
	val = Ffile.read_reals().reshape(shape, order='F')
	Ffile.close()
	return val

ip_path = '/home/gwswex_dev/gwswex_multilay/runtime/input'
op_path = '/home/gwswex_dev/gwswex_multilay/runtime/output'

fwrite('top.ip', top)
fwrite('bot.ip', bot)

fwrite('l1_active.ip', isactive[0])
fwrite('l1_ks.ip', ks)
fwrite('l1_por.ip', porosity)
fwrite('l2_active.ip', isactive[1])
fwrite('l2_ks.ip', ks)
fwrite('l2_por.ip', porosity)
fwrite('l3_active.ip', isactive[2])
fwrite('l3_ks.ip', ks)
fwrite('l3_por.ip', porosity)

fwrite('GW_chd.ip', chd)
fwrite('GW_ini.ip', gw_ini)
fwrite('SW_ini.ip', sw_ini)

fwrite('p.ip', p)
fwrite('et.ip', et)

#%%
GWSWEX.wrap_init('/home/gwswex_dev/gwswex_multilay/gwswex.yml')
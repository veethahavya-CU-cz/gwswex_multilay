#%%
import numpy as np
from ctypes import *
import os, psutil, sys
from scipy.io import FortranFile
from dataclasses import dataclass
from datetime import datetime, timedelta
import matplotlib.pyplot as plt

# %%
os.environ['OMP_NUM_THREADS'] = str(psutil.cpu_count(logical = False))

sys.path.append(os.path.abspath('libs/'))
from gwswex_wrapper import gwswex as GWSWEX


Fyaml = create_string_buffer(b'/home/gwswex_dev/gwswex_multilay/gwswex.yml', 256)

# %%
def plot(elem, plotWlev=True, plotPrec=True, plotDis=True, plotBal=True, savefig=True, dDPI=90, pDPI=1600, alpha_scatter=0.7, scatter_size=3, format='jpg'):
	#formats = jpg, svg, png, jpg
	fig_path = os.path.join(op_path, 'figs/')
	if not os.path.exists(fig_path):
		os.makedirs(fig_path)
	pal = ['#E74C3C', '#2ECC71', '#5EFCA1', '#E7AC3C', '#2980B9', '#1A3BFF', '#FF6600'] #[gw, sms, epv, sv, sw, p, et]

	def disPlot(elem):
		plt.figure(dpi=dDPI)
		plt.xlabel("Time Steps")
		plt.ylabel("Discharges in Storage")
		plt.scatter(range(0,Gnts), gw_dis[elem,:], label="GW_dis", color=pal[0],\
		alpha=alpha_scatter, s=scatter_size)
		plt.scatter(range(0,Gnts), sm_dis[elem,:], label="SM_dis", color=pal[1],\
		alpha=alpha_scatter, s=scatter_size)
		plt.scatter(range(0,Gnts), sw_dis[elem,:], label="SW_dis", color=pal[4],\
		alpha=alpha_scatter, s=scatter_size)
		plt.legend(loc='best', fontsize='small')
		plt.tight_layout()
		plt.xticks(range(0,Gnts,24*30))

	def wlevPlot(elem,gws,sws,sms):
		plt.figure(dpi=dDPI)
		plt.xlabel("Time Steps (h)")
		plt.ylabel("Water Levels (m.a.s.l.)")
		plt.ylim([bot[-1,elem]-0.5, sws[elem,:].max()+0.5+top[elem]])
		plt.stackplot(range(0,Gnts), gws[elem,:-1], sms[elem,:-1],\
		epv[elem,:-1]-sms[elem,:-1], (np.full(Gnts,top[elem])-gws[elem,:-1])*(1-porosity[elem]),\
		sws[elem,:-1], labels=["Groundwater","Soil Moisture", "Effective Pore Volume", "Soil Volume", "Surface Water"], colors=pal)
		if plotPrec:
			p_dom, et_dom = [], []
			ht = (sws[elem,:].max()+0.5+top[elem]) + (bot[-1,elem]-0.5)
			for ts in range(Gnts):
				if p[elem,ts] > et[elem,ts]:
					p_dom.append(ht)
					et_dom.append(0)
				else:
					et_dom.append(ht)
					p_dom.append(0)
			plt.stackplot(range(0,Gnts), p_dom, labels=["Precipitation Dominant", ], colors=['#A8EAED'], alpha=0.21)
			plt.stackplot(range(0,Gnts), et_dom, labels=["Evapotranspiration Dominant", ], colors=['#E8A78B'], alpha=0.21)
		plt.plot(range(0,Gnts+1), np.full(Gnts+1,top[elem]), color='#502D16', linewidth=0.5, label="Ground Level")
		plt.plot(range(0,Gnts+1), np.full(Gnts+1,bot[-1,elem]), color='black', linewidth=0.5, label="Bottom")
		plt.legend(loc='lower right', fontsize=3)
		plt.tight_layout()
		plt.xticks(range(0,Gnts,24*30))

	def balPlot():
		plt.figure(dpi=dDPI)
		ind = np.random.choice(Qdiff.shape[0], 1, replace=False)[0]
		plt.xlabel("Time Steps")
		plt.ylabel("Mass Balance Error (Total = {:.2g})".format(Qdiff[ind].sum()))
		plt.plot(Qdiff[ind], "r")
		plt.tight_layout()
		plt.xticks(range(0,Gnts,24*30))

	if plotDis:
		disPlot(0)
		if savefig:
			plt.savefig(os.path.join(fig_path,"discharges."+format), format=format, dpi=pDPI)

	if plotWlev:
		wlevPlot(0,gws,sws,sms)
		if savefig:
			plt.savefig(os.path.join(fig_path,"water_levels."+format), format=format, dpi=pDPI)

	if plotBal:
		balPlot()
		if savefig:
			plt.savefig(os.path.join(fig_path,"mBal."+format), format=format, dpi=pDPI)

# %%
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
# p[:,1750:2000] = 0*(1e-3/3600)
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
GWSWEX.init('/home/gwswex_dev/gwswex_multilay/gwswex.yml')

GWSWEX.run(gw_ini, sw_ini)

gws = np.empty((elems, Gnts+1), dtype=np.float64, order='F')
sws = np.empty((elems, Gnts+1), dtype=np.float64, order='F')
sms = np.empty((elems, Gnts+1), dtype=np.float64, order='F')
epv = np.empty((elems, Gnts+1), dtype=np.float64, order='F')
GWSWEX.pass_vars(gws, sws, sms, epv)

plot(0, plotWlev=True, plotPrec=True, plotDis=False, plotBal=False, savefig=True) #True False
# %%

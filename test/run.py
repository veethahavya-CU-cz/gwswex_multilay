#!/bin/python
#%%
import numpy as np
from ctypes import *
import os, psutil, sys
from scipy.io import FortranFile
from dataclasses import dataclass
from datetime import datetime, timedelta
import matplotlib.pyplot as plt
import time

# %%
os.environ['OMP_NUM_THREADS'] = str(psutil.cpu_count(logical = False))

# sys.path.append(os.path.abspath('lib/'))
from gwswex_f2pywrapper import gwswex as GWSWEX


# Fyaml = create_string_buffer(b'/home/gwswex_dev/GWSWEX/multilay/gwswex.yml', 256)

# %%
def plot(elem, nts_ll, nts_ul, tick_res=24, nlay=1, plotWlev=True, plotPrec=True, plotDis=True, plotBal=True, savefig=True, dDPI=90, pDPI=1600, alpha_scatter=0.7, scatter_size=3, format='jpg'):
	#formats = jpg, svg, png, jpg
	fig_path = os.path.join(op_path, 'figs/')
	if not os.path.exists(fig_path):
		os.makedirs(fig_path)
	pal = ['#E74C3C', '#2ECC71', '#5EFCA1', '#E7AC3C', '#2980B9', '#1A3BFF', '#FF6600'] #[gw, sms, epv, sv, sw, p, et]
	if nlay != 1:
		pal_nlay = ['#E74C3C', ]
		for i in range(nlay):
			pal_nlay.append(pal[1])
			pal_nlay.append(pal[2])
			pal_nlay.append(pal[3])
		pal_nlay.append(pal[4])
		

	def disPlot(elem):
		plt.figure(dpi=dDPI)
		plt.xlabel("Time Steps")
		plt.ylabel("Discharges in Storage")
		plt.scatter(range(nts_ll,nts_ul), gw_dis[elem,nts_ll:nts_ul], label="GW_dis", color=pal[0],\
		alpha=alpha_scatter, s=scatter_size)
		plt.scatter(range(nts_ll,nts_ul), uz_dis[elem,nts_ll:nts_ul], label="SM_dis", color=pal[1],\
		alpha=alpha_scatter, s=scatter_size)
		plt.scatter(range(nts_ll,nts_ul), sw_dis[elem,nts_ll:nts_ul], label="SW_dis", color=pal[4],\
		alpha=alpha_scatter, s=scatter_size)
		plt.legend(loc='best', fontsize='small')
		plt.tight_layout()
		plt.xticks(range(nts_ll,nts_ul,tick_res))

	def wlevPlot(elem,gws,sws,sms):
		plt.figure(dpi=dDPI)
		plt.xlabel("Time Steps (h)")
		plt.ylabel("Water Levels (m.a.s.l.)")
		plt.ylim([bot[-1,elem]-0.5, np.nanmax(sws[elem,:])+0.5+top[elem]])
		plt.stackplot(range(nts_ll,nts_ul), gws[elem,nts_ll:nts_ul], sms[elem,nts_ll:nts_ul],\
		epv[elem,nts_ll:nts_ul]-sms[elem,nts_ll:nts_ul], (np.full(nts_ul-nts_ll,top[elem])-gws[elem,nts_ll:nts_ul])*(1-porosity[elem]),\
		sws[elem,nts_ll:nts_ul], labels=["Groundwater","Soil Moisture", "Effective Pore Volume", "Soil Volume", "Surface Water"], colors=pal)
		if plotPrec:
			p_dom, et_dom = [], []
			ht = (np.nanmax(sws[elem,:])+0.5+top[elem]) + (bot[-1,elem]-0.5)
			for ts in range(nts_ul-nts_ll):
				if p[elem,ts] > et[elem,ts]:
					p_dom.append(ht)
					et_dom.append(0)
				else:
					et_dom.append(ht)
					p_dom.append(0)
			plt.stackplot(range(nts_ll,nts_ul), p_dom, labels=["Precipitation Dominant", ], colors=['#A8EAED'], alpha=0.21)
			plt.stackplot(range(nts_ll,nts_ul), et_dom, labels=["Evapotranspiration Dominant", ], colors=['#E8A78B'], alpha=0.21)
		plt.plot(range(nts_ll,nts_ul+1), np.full((nts_ul-nts_ll)+1,top[elem]), color='#502D16', linewidth=0.5, label="Ground Level")
		plt.plot(range(nts_ll,nts_ul+1), np.full((nts_ul-nts_ll)+1,bot[-1,elem]), color='black', linewidth=0.5, label="Bottom")
		plt.legend(loc='lower right', fontsize=3)
		plt.tight_layout()
		plt.xticks(range(nts_ll-1,nts_ul-1,int((nts_ul-nts_ll)/6)))


	def wlevPlot_nlay(elem,gws,sws,sms,epv):
		plt.figure(dpi=dDPI)
		plt.xlabel("Time Steps (h)")
		plt.ylabel("Water Levels (m.a.s.l.)")
		plt.ylim([bot[-1,elem]-0.5, np.nanmax(sws[elem,1:])+0.5+top[elem]])
		stack = [gws[elem,nts_ll:nts_ul], ]
		for l in range(nlay-1,-1,-1):
			sv, epv_l, sm = [], [], []
			for t in range(nts_ll, nts_ul, 1):
				sm.append(sms[l][elem,t])
				epv_l.append(epv[l][elem,t]-sms[l][elem,t])
				if l == 0:
					sv.append(max((top[elem] - max(bot[l,elem], gws[elem][t]))*(1-porosity[elem]),0))
				else:
					sv.append(max((bot[l-1,elem] - max(bot[l,elem], gws[elem][t]))*(1-porosity[elem]),0))
			stack.append(sm)
			stack.append(epv_l)
			stack.append(sv)
		stack.append(sws[elem,nts_ll:nts_ul])

		plt.stackplot(range(nts_ll,nts_ul), stack, \
			labels=["Groundwater","Soil Moisture", "Effective Pore Volume", "Soil Volume", "Surface Water"], colors=pal_nlay)
		
		if plotPrec:
			p_dom, et_dom = [], []
			ht = (sws[elem,:].max()+0.5+top[elem]) + (bot[-1,elem]-0.5)
			for ts in range(nts_ul-nts_ll):
				if p[elem,ts] > et[elem,ts]:
					p_dom.append(ht)
					et_dom.append(0)
				else:
					et_dom.append(ht)
					p_dom.append(0)
			plt.stackplot(range(nts_ll,nts_ul), p_dom, labels=["Precipitation Dominant", ], colors=['#A8EAED'], alpha=0.21)
			plt.stackplot(range(nts_ll,nts_ul), et_dom, labels=["Evapotranspiration Dominant", ], colors=['#E8A78B'], alpha=0.21)
		plt.plot(range(nts_ll,nts_ul+1), np.full((nts_ul-nts_ll)+1,top[elem]), color='#502D16', linewidth=0.5, label="Ground Level")
		plt.plot(range(nts_ll,nts_ul+1), np.full((nts_ul-nts_ll)+1,bot[-1,elem]), color='black', linewidth=0.5, label="Bottom")
		plt.legend(loc='lower right', fontsize=3)
		plt.tight_layout()
		plt.xticks(range(nts_ll-1,nts_ul-1,int((nts_ul-nts_ll)/6)))

	def balPlot():
		plt.figure(dpi=dDPI)
		ind = np.random.choice(qdiff.shape[0], 1, replace=False)[0]
		plt.xlabel("Time Steps")
		plt.ylabel("Mass Balance Error (Total = {:.2g})".format(qdiff[ind].sum()))
		plt.plot(qdiff[ind], "r")
		plt.tight_layout()
		plt.xticks(range(nts_ll,nts_ul,24*30))

	if plotDis:
		disPlot(0)
		if savefig:
			plt.savefig(os.path.join(fig_path,"discharges."+format), format=format, dpi=pDPI)

	if plotWlev:
		if nlay == 1:
			wlevPlot(elem,gws,sws,sms)
		else:
			wlevPlot_nlay(elem,gws,sws,sms,epv)
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
tstop = datetime(2020, 6, 29, 0, 0, 0)

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
ks = np.full(elems, 75e-5, dtype=np.float64, order='F')
chd = np.full(elems, 0, dtype=bool, order='F')
p = np.full((elems,Gnts+1), 2.5*(1e-3/3600))
# p[:,0:500] = 3.5*(1e-3/3600)
p[:,500:750] = 0*(1e-3/3600)
p[:,1000:1250] = 0*(1e-3/3600)
p[:,1250:1750] = 3.5*(1e-3/3600)
p[:,1750:2000] = 0*(1e-3/3600)
p[:,2100:Gnts] = 0*(1e-3/3600)
et = np.full((elems,Gnts+1), 0.33*(1e-3/3600))

isactive = np.full((nlay, elems), 1, dtype=bool, order='F')
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

ip_path = os.path.abspath('runtime/input')
op_path = os.path.abspath('runtime/output')
if not os.path.exists(ip_path):
	os.makedirs(ip_path, exist_ok=True)
if not os.path.exists(op_path):
	os.makedirs(op_path, exist_ok=True)

fwrite('top.ip', top)
fwrite('bot.ip', bot)

fwrite('l1_active.ip', isactive[0])
fwrite('l1_ks.ip', ks)
fwrite('l1_porosity.ip', porosity)
fwrite('l2_active.ip', isactive[1])
fwrite('l2_ks.ip', ks)
fwrite('l2_porosity.ip', porosity)
fwrite('l3_active.ip', isactive[2])
fwrite('l3_ks.ip', ks)
fwrite('l3_porosity.ip', porosity)

fwrite('GW_chd.ip', chd)
fwrite('GW_ini.ip', gw_ini)
fwrite('SW_ini.ip', sw_ini)

fwrite('p.ip', p)
fwrite('et.ip', et)

#%%
GWSWEX.init(os.path.abspath('test/gwswex.yml'), gw_ini, sw_ini)

GWSWEX.run()

### FOR MULTILAYERED SM PLOTS ###
gws = np.empty((elems, Gnts+1), dtype=np.float64, order='F')
sws = np.empty((elems, Gnts+1), dtype=np.float64, order='F')
sms = np.empty((nlay, elems, Gnts+1), dtype=np.float64, order='F')
epv = np.empty((nlay, elems, Gnts+1), dtype=np.float64, order='F')

GWSWEX.pass_vars_nlay(gws, sws, sms, epv)

plot(0, 1, Gnts+1, nlay=nlay, plotWlev=True, plotPrec=True, plotDis=False, plotBal=False, savefig=True) #True False

### FOR SINGLE LAYERED SM PLOTS ###
# gws = np.empty((elems, Gnts+1), dtype=np.float64, order='F')
# sws = np.empty((elems, Gnts+1), dtype=np.float64, order='F')
# sms = np.empty((elems, Gnts+1), dtype=np.float64, order='F')
# epv = np.empty((elems, Gnts+1), dtype=np.float64, order='F')
# GWSWEX.pass_vars(gws, sws, sms, epv)
# plot(0, 1, Gnts+1, nlay=1, plotWlev=True, plotPrec=True, plotDis=False, plotBal=False, savefig=True) #True False


# %%

gw_dis, sw_dis, uz_dis, qdiff = np.empty(gws.shape, dtype=np.float64, order='F'), np.empty(gws.shape, dtype=np.float64, order='F'), np.empty(gws.shape, dtype=np.float64, order='F'), np.empty(gws.shape, dtype=np.float64, order='F')
GWSWEX.pass_dis(gw_dis, uz_dis, sw_dis, qdiff)

# plt.figure()
# plt.plot(qdiff.sum(axis=0))
# plt.savefig(os.path.join(op_path,'figs',"mBal."+'png'), format='png', dpi=1600)
# plt.show()

uzs = np.empty((elems, Gnts+1), dtype=np.float64, order='F')
epv = np.empty((elems, Gnts+1), dtype=np.float64, order='F')
GWSWEX.pass_vars(gws, sws, uzs, epv)
influx = (p.sum())*Gdt - (et.sum())*Gdt
delta_storages = (uzs[:,-1]-uzs[0,0]).sum() + ((gws[:,-1]-gws[:,0])*pvanGI.theta_s).sum() + (sws[:,-1]-sws[:,0]).sum()
print("mbal err: {:.2e}".format(influx-delta_storages))
print("mbal err %: {:.2e}".format((influx-delta_storages)/influx))

plot(0, 1, Gnts+1, nlay=nlay, plotWlev=False, plotPrec=False, plotDis=True, plotBal=True, savefig=True) #True False

# for i in range(1,gws.shape[1]):
#     gw_dis[0][i-1] = (gws[0][i] - gws[0][i-1])*porosity[0]
#     sw_dis[0][i-1] = sws[0][i] - sws[0][i-1]
#     sm_dis[0][i-1] = sms[0][i] - sms[0][i-1]
# qout = gw_dis.sum() + sw_dis.sum() + sm_dis.sum()
# qin = (p[0].sum() - et[0].sum())*Gdt

# ll=0
# plt.figure(figsize=(10,5))
# plt.plot((gw_dis[0][ll:-1]+sw_dis[0][ll:-1]+sm_dis[0][ll:-1]))
# plt.plot((p[0][ll:-1]-et[0][ll:-1])*Gdt)
# plt.show()
# %%
GWSWEX.finalize()
#%%
import os, sys, psutil
import numpy as np
from scipy.integrate import quad
import matplotlib.pyplot as plt
from ctypes import cdll
from dataclasses import dataclass
import yaml #PyYAML
from datetime import datetime, timedelta

os.environ['OMP_NUM_THREADS'] = str(psutil.cpu_count(logical=False))

cdll.LoadLibrary('/usr/local/lib/libyaml-wrapper.so')
sys.path.append(os.path.abspath('libs/'))
from gwswex_wrapper import gwswex as GWSWEX

def vanGI(d):
	def theta(h_c):
		theta_r = pvanGI.theta_r
		theta_s = pvanGI.theta_s
		alpha = pvanGI.alpha
		n = pvanGI.n
		m = (1-(1/n))
		return theta_r + ((theta_s - theta_r)/((1+(alpha*(abs(h_c)))**n))**m)
	return np.float64(quad(theta,-d,0)[0])

def plot(elem, plotWlev=True, plotPrec=True, plotDis=True, plotBal=True, savefig=True, dDPI=90, pDPI=1600, alpha_scatter=0.7, scatter_size=3, format='jpg'):
	#formats = jpg, svg, png, jpg
	fig_path = os.path.join(op_path, 'figs/')
	if not os.path.exists(fig_path):
		os.makedirs(fig_path)
	pal = ['#E74C3C', '#2ECC71', '#5EFCA1', '#E7AC3C', '#2980B9', '#1A3BFF', '#FF6600'] #[gw, sm, epv, sv, sw, p, et]

	def disPlot(elem):
		plt.figure(dpi=dDPI)
		plt.xlabel("Time Steps")
		plt.ylabel("Discharges in Storage")
		plt.scatter(range(0,nts), gw_dis[elem,:], label="GW_dis", color=pal[0],\
		alpha=alpha_scatter, s=scatter_size)
		plt.scatter(range(0,nts), sm_dis[elem,:], label="SM_dis", color=pal[1],\
		alpha=alpha_scatter, s=scatter_size)
		plt.scatter(range(0,nts), sw_dis[elem,:], label="SW_dis", color=pal[4],\
		alpha=alpha_scatter, s=scatter_size)
		plt.legend(loc='best', fontsize='small')
		plt.tight_layout()
		plt.xticks(range(0,nts,24*30))

	def wlevPlot(elem,gws,sws,sm):
		plt.figure(dpi=dDPI)
		plt.xlabel("Time Steps (h)")
		plt.ylabel("Water Levels (m.a.s.l.)")
		plt.ylim([bot[elem]-0.5, sws[elem,:].max()+0.5+gok[elem]])
		plt.stackplot(range(0,nts), gws[elem,1:], sm[elem,1:],\
		epv[elem,1:]-sm[elem,1:], (np.full(nts,gok[elem])-gws[elem,1:])*(1-n[elem]),\
		sws[elem,1:], labels=["Groundwater","Soil Moisture", "Effective Pore Volume", "Soil Volume", "Surface Water"], colors=pal)
		if plotPrec:
			p_dom, et_dom = [], []
			ht = (sws[elem,:].max()+0.5+gok[elem]) + (bot[elem]-0.5)
			for ts in range(nts):
				if p[elem,ts] > et[elem,ts]:
					p_dom.append(ht)
					et_dom.append(0)
				else:
					et_dom.append(ht)
					p_dom.append(0)
			plt.stackplot(range(0,nts), p_dom, labels=["Precipitation Dominant", ], colors=['#A8EAED'], alpha=0.21)
			plt.stackplot(range(0,nts), et_dom, labels=["Evapotranspiration Dominant", ], colors=['#E8A78B'], alpha=0.21)
		plt.plot(range(0,nts+1), np.full(nts+1,gok[elem]), color='#502D16', linewidth=0.5, label="Ground Level")
		plt.plot(range(0,nts+1), np.full(nts+1,bot[elem]), color='black', linewidth=0.5, label="Bottom")
		plt.legend(loc='lower right', fontsize=3)
		plt.tight_layout()
		plt.xticks(range(0,nts,24*30))

	def balPlot():
		plt.figure(dpi=dDPI)
		ind = np.random.choice(Qdiff.shape[0], 1, replace=False)[0]
		plt.xlabel("Time Steps")
		plt.ylabel("Mass Balance Error (Total = {:.2g})".format(Qdiff[ind].sum()))
		plt.plot(Qdiff[ind], "r")
		plt.tight_layout()
		plt.xticks(range(0,nts,24*30))

	if plotDis:
		disPlot(0)
		if savefig:
			plt.savefig(os.path.join(fig_path,"discharges."+format), format=format, dpi=pDPI)

	if plotWlev:
		wlevPlot(0,gws,sws,sm)
		if savefig:
			plt.savefig(os.path.join(fig_path,"water_levels."+format), format=format, dpi=pDPI)

	if plotBal:
		balPlot()
		if savefig:
			plt.savefig(os.path.join(fig_path,"mBal."+format), format=format, dpi=pDPI)

#%%
elems = int(1)
nts = int(24*30*6) #one every hour for 6 months
dt = 3600
tstart = datetime(2020, 1, 1, 0, 0, 0)
tstop = tstart + timedelta(seconds=nts*dt)

@dataclass
class pvanGI:
	theta_r: float = 0.02
	theta_s: float = 0.42
	alpha: float = 0.35
	n: float = 1.25

gok = np.random.default_rng().uniform(-3, 3, elems)+150
bot = gok - 30
n = np.full(elems, pvanGI.theta_s)
k = np.full(elems, 50e-5)
macropore_inf_degree = np.full(elems, 0, dtype=np.float64)
chd = np.full(elems, False, dtype=bool)
p = np.full((elems,nts+1), 2.5*(1e-3/3600)) #mm/h
p[:,0:500] = 3.5*(1e-3/3600)
p[:,500:750] = 0*(1e-3/3600)
p[:,1000:1250] = 0*(1e-3/3600)
p[:,1000:1250] = 0*(1e-3/3600)
p[:,1750:2000] = 0*(1e-3/3600)
p[:,2100:nts] = 0*(1e-3/3600)
et = np.full((elems,nts+1), 0.33*(1e-3/3600))

sm = np.zeros((elems,nts+1), dtype=np.float64, order='F')
gws = np.zeros((elems,nts+1), dtype=np.float64, order='F')
gws[:,0] = bot + 5
sws = np.zeros((elems,nts+1), dtype=np.float64, order='F')
sws[:,0] = np.random.default_rng().uniform(0, 1e-2, elems)
epv = np.zeros((elems,nts+1), dtype=np.float64, order='F')
epv[:,0] = (gok-gws[:,0])*n
for x in range(elems):
	sm[x,0] = (vanGI(gok[x]-gws[x,0]))
gw_sm_interconnectivity = np.full(elems, pvanGI.theta_r, dtype=np.float64, order='F') #incorrect - you are mistaking gw_sm_IC with IC ratio! this is abs value

fyaml_path = '/home/gwswex_dev/gwswex_multilay/gwswex.yml'
gwsewex_config = {}

gwsewex_config['model'] = {}
gwsewex_config['model']['domain'] = {}
gwsewex_config['model']['domain']['nelements'] = elems
gwsewex_config['model']['domain']['dt'] = dt
gwsewex_config['model']['domain']['tstart'] = tstart.strftime('%Y%m%d %H%M%S')
gwsewex_config['model']['domain']['tend'] = tstop.strftime('%Y%m%d %H%M%S')
gwsewex_config['model']['domain']['nlay'] = 1
gwsewex_config['model']['domain']['top'] = 'top.ip'
gwsewex_config['model']['domain']['bot'] = 'bot.ip'
gwsewex_config['model']['domain']['layer1'] = {}
gwsewex_config['model']['domain']['layer1']['name'] = 'l1'
gwsewex_config['model']['domain']['layer1']['isactive'] = 'l1_active.ip'
gwsewex_config['model']['domain']['layer1']['vanG'] = {}
gwsewex_config['model']['domain']['layer1']['vanG']['alpha'] = pvanGI.alpha
gwsewex_config['model']['domain']['layer1']['vanG']['n'] = pvanGI.n
gwsewex_config['model']['domain']['layer1']['vanG']['theta_r'] = pvanGI.theta_r
gwsewex_config['model']['domain']['layer1']['vanG']['theta_s'] = pvanGI.theta_s
gwsewex_config['model']['domain']['layer1']['ks'] = 'l1_ks.ip'
gwsewex_config['model']['domain']['layer1']['porosity'] = 'l1_porosity.ip'
# gwsewex_config['model']['domain']['layer1']['macropore_inf_degree'] = 'l1_macropore_inf_degree.ip'

gwsewex_config['model']['boundary'] = {}
gwsewex_config['model']['boundary']['GW_chd'] = 'GW_chd.ip'

gwsewex_config['model']['initial conditions'] = {}
gwsewex_config['model']['initial conditions']['GW'] = 'GW.ip'
gwsewex_config['model']['initial conditions']['SW'] = 'SW.ip'



gwsewex_config['paths'] = {}
gwsewex_config['paths']['dirs'] = {'root': str(os.getcwd()), 'input': str(os.getcwd()), 'output': str(os.getcwd())}
gwsewex_config['paths']['files'] = {'log': 'GWSWEX_.log'}

gwsewex_config['utils'] = {}
gwsewex_config['utils']['logger'] = {'level': 1}

with open(fyaml_path, 'w') as fyaml_path:
	yaml.dump(gwsewex_config, fyaml_path, default_flow_style=False, allow_unicode=True, sort_keys=False)

# GWSWEX.initialize(fyaml_path, gok, bot, n, k, macropore_inf_degree, vanG_pars, chd, p, et)

#%%
GWSWEX.finalize(gws, sws, sm, epv, gw_sm_interconnectivity)
GWSWEX.fetch_1d('gw_sm_interconnectivity', gw_sm_interconnectivity)
gw_dis = np.zeros((elems,nts), dtype=np.float64, order='F')
GWSWEX.fetch_2d('gw_dis', gw_dis)
sw_dis = np.zeros((elems,nts), dtype=np.float64, order='F')
GWSWEX.fetch_2d('sw_dis', sw_dis)
sm_dis = np.zeros((elems,nts), dtype=np.float64, order='F')
GWSWEX.fetch_2d('sm_dis', sm_dis)
Qin = np.zeros((elems,nts), dtype=np.float64, order='F')
GWSWEX.fetch_2d('Qin', Qin)
Qout = np.zeros((elems,nts), dtype=np.float64, order='F')
GWSWEX.fetch_2d('Qout', Qout)
Qdiff = np.zeros((elems,nts), dtype=np.float64, order='F')
GWSWEX.fetch_2d('Qdiff', Qdiff)

influx = (p[0].sum()-p[0,0])*dt - (et[0].sum()-et[0,0])*dt
delta_storages = sm[0,-1]-sm[0,0] + (gws[0,-1]-gws[0,0])*pvanGI.theta_s + sws[0,-1]-sws[0,0]
print("mbal err: {:.2e}".format(influx-delta_storages))

op_path = 'output/'
if not os.path.exists(op_path):
	os.makedirs(op_path)
plot(0, plotWlev=True, plotPrec=True, plotDis=True, plotBal=True, savefig=False) #True False

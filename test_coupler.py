#%%
import numpy as np
from ctypes import *
import os, psutil, sys
from scipy.io import FortranFile
from dataclasses import dataclass
from datetime import datetime, timedelta
import matplotlib.pyplot as plt
import time
import yaml, pickle


start = time.time()
os.environ['OMP_NUM_THREADS'] = str(psutil.cpu_count(logical = False))

sys.path.append(os.path.abspath('lib/'))
from gwswex_wrapper import gwswex as GWSWEX

# TODO: make GWSWEX py library so that these defs need not be repeated
# TODO: set all dis to nan if 0
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


nelems = int(1)
elems = nelems
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


def fwrite(fname, val):

	Ffile = FortranFile(os.path.join(ip_path,fname), 'w')
	Ffile.write_record(val.T)
	Ffile.close()

def fread(fname):
	shape = (nelems, Gnts)
	Ffile = FortranFile(os.path.join(op_path,fname), 'r')
	val = Ffile.read_reals().reshape(shape, order='F')
	Ffile.close()
	return val

root_path = os.path.abspath('runtime')
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
mdef_tformat = '%Y%m%d %H%M%S'
mdef = {
    'model': {
        'domain': {
            'nelements': nelems,
            'dt': int(Gdt),
            'tstart': tstart.strftime(mdef_tformat),
            'tstop': tstop.strftime(mdef_tformat),
            'nlay': nlay,
            'top': 'top.ip',
            'bot': 'bot.ip',
            'layer1': {
                'name': 'l1',
                'isactive': 'l1_active.ip',
                'vanG': [pvanGI.alpha, pvanGI.n, pvanGI.theta_r, pvanGI.theta_s],
                'ks': 'l1_ks.ip',
                'porosity': 'l1_porosity.ip'
            },
            'layer2': {
                'name': 'l2',
                'isactive': 'l2_active.ip',
                'vanG': [pvanGI.alpha, pvanGI.n, pvanGI.theta_r, pvanGI.theta_s],
                'ks': 'l2_ks.ip',
                'porosity': 'l2_porosity.ip'
            },
            'layer3': {
                'name': 'l3',
                'isactive': 'l3_active.ip',
                'vanG': [pvanGI.alpha, pvanGI.n, pvanGI.theta_r, pvanGI.theta_s],
                'ks': 'l3_ks.ip',
                'porosity': 'l3_porosity.ip'
            },
        },
        'boundary conditions': {
            'GW CHD': 'GW_chd.ip'
        },
        'initial conditions': {
            'GW': 'GW_ini.ip',
            'SW': 'SW_ini.ip'
        },
        'external forcings': {
            'p': 'p.ip',
            'et': 'et.ip'
        },
        'solver settings': {
            'pet_intensities': [2.78e-5, 5.56e-4, 8.33e-3],
            'pet_nts': [3, 5, 7]
        }
    },
    'paths': {
        'dirs': {
            'root': root_path,
            'input': ip_path,
            'output': op_path
        },
        'files': {
            'DMN.TOP': 1,
            'DMN.BOT': 1,
            'DMN.LAY.ACT': 1,
            'DMN.LAY.KS': 1,
            'DMN.LAY.POR': 1,
            'BND.CHD': 1,
            'IC.GW': 1,
            'IC.SW': 1,
            'EXTF.p': 1,
            'EXTF.et': 1
        }
    },
    'utils': {
        'logger': {
            'level': 1,
            'fname': 'GWSWEX.log'
        }
    }
}



def represent_list(dumper, data):
    if len(data) == 1:
        return dumper.represent_scalar(u'tag:yaml.org,2002:seq', str(data[0]))
    else:
        return dumper.represent_sequence(u'tag:yaml.org,2002:seq', data, flow_style=True)

yaml.add_representer(list, represent_list)

with open('./test.yml', 'w') as outfile:
    yaml.dump(mdef, outfile, sort_keys=False)

mdef['auxiliary'] = {}
mdef['auxiliary']['mdef_tformat'] = mdef_tformat
mdef['auxiliary']['Gnts'] = Gnts
mdef['auxiliary']['tstart_dt'] = tstart
mdef['auxiliary']['tstop_dt'] = tstop

# with open('../../common/data/test.pkl', 'wb') as file:
#     pickle.dump(mdef, file)



#%%
GWSWEX.init(os.path.abspath('test.yml'), gw_ini, sw_ini)

Lnts = GWSWEX.get_lnts()
Gts = GWSWEX.get_gts()
curr_time = datetime.utcfromtimestamp(GWSWEX.get_curr_time_unix())

gws_l = np.empty((nelems, Lnts), dtype=np.float64, order='F')
sws_l = np.empty((nelems, Lnts), dtype=np.float64, order='F')
uzs_l = np.empty((nelems, Lnts), dtype=np.float64, order='F')
epv_l = np.empty((nelems, Lnts), dtype=np.float64, order='F')
GWSWEX.grab_result('gws_l', gws_l)
GWSWEX.grab_result('sws_l', sws_l)
GWSWEX.grab_result('uzs_l', uzs_l)
GWSWEX.grab_result('epv_l', epv_l)

gw_dis = np.empty((nelems, Lnts), dtype=np.float64, order='F')
sw_dis = np.empty((nelems, Lnts), dtype=np.float64, order='F')
uz_dis = np.empty((nelems, Lnts), dtype=np.float64, order='F')
GWSWEX.grab_result('gw_dis_l', gw_dis)
GWSWEX.grab_result('sw_dis_l', sw_dis)
GWSWEX.grab_result('uz_dis_l', uz_dis)

gw_res = []
lateral_gw_flux_sum, lateral_sw_flux_sum = 0, 0
while(curr_time < tstop):
    GWSWEX.update(auto_advance=True)

    Lnts = GWSWEX.get_lnts()
    Gts = GWSWEX.get_gts()
    curr_time = datetime.utcfromtimestamp(GWSWEX.get_curr_time_unix())

    gws_l = np.empty((nelems, Lnts), dtype=np.float64, order='F')
    sws_l = np.empty((nelems, Lnts), dtype=np.float64, order='F')
    uzs_l = np.empty((nelems, Lnts), dtype=np.float64, order='F')
    epv_l = np.empty((nelems, Lnts), dtype=np.float64, order='F')
    GWSWEX.grab_result('gws_l', gws_l)
    GWSWEX.grab_result('sws_l', sws_l)
    GWSWEX.grab_result('uzs_l', uzs_l)
    GWSWEX.grab_result('epv_l', epv_l)

    gw_dis = np.empty((nelems, Lnts), dtype=np.float64, order='F')
    sw_dis = np.empty((nelems, Lnts), dtype=np.float64, order='F')
    uz_dis = np.empty((nelems, Lnts), dtype=np.float64, order='F')
    GWSWEX.grab_result('gw_dis_l', gw_dis)
    GWSWEX.grab_result('sw_dis_l', sw_dis)
    GWSWEX.grab_result('uz_dis_l', uz_dis)

    # print("Solved until: ", curr_time, "Lnts = ", Lnts)

    # print("gws_l = ", gws_l[0], "\nsws_l = ", sws_l[0], "\nuzs_l = ", uzs_l[0])

    # print("Simulating external discharges...")
    # gws_ext = np.sort(gws_l + np.random.randn(Lnts)*1e-2)
    # sws_ext = np.sort(sws_l + abs(np.random.randn(Lnts))*1e-5)
    # print("gws_ext = ", gws_ext[0], "\nsws_ext = ", sws_ext[0])

    # lateral_gw_flux = gws_ext - gws_l
    # lateral_sw_flux = sws_ext - sws_l
    # for e in range(nelems):
    #     for t in range(1,Lnts):
    #         lateral_gw_flux[e,t] = lateral_gw_flux[e,t-1] - lateral_gw_flux[e,t-1]
    # #         lateral_sw_flux[e,t] = lateral_sw_flux[e,t-1] - lateral_sw_flux[e,t-1]
    # lateral_gw_flux_sum += np.sum(lateral_gw_flux)
    # lateral_sw_flux_sum += np.sum(lateral_sw_flux)
    # print("Updating external discharges...")
    # GWSWEX.resolve(lateral_gw_flux, lateral_sw_flux)

    # gws_l = np.empty((nelems, Lnts), dtype=np.float64, order='F')
    # sws_l = np.empty((nelems, Lnts), dtype=np.float64, order='F')
    # uzs_l = np.empty((nelems, Lnts), dtype=np.float64, order='F')
    # GWSWEX.grab_result('gws_l', gws_l)
    # GWSWEX.grab_result('sws_l', sws_l)
    # GWSWEX.grab_result('uzs_l', uzs_l)

    # print("gws_l = ", gws_l[0], "\nsws_l = ", sws_l[0], "\nuzs_l = ", uzs_l[0])
    # print("GW residuals = ", gws_l - gws_ext, "SW residuals = ", sws_l - sws_ext)
    # gw_res.append(np.sum(gws_l - gws_ext))


### FOR MULTILAYERED SM PLOTS ###
gws = np.empty((nelems, Gnts+1), dtype=np.float64, order='F')
sws = np.empty((nelems, Gnts+1), dtype=np.float64, order='F')
sms = np.empty((nlay, nelems, Gnts+1), dtype=np.float64, order='F')
epv = np.empty((nlay, nelems, Gnts+1), dtype=np.float64, order='F')
GWSWEX.pass_vars_nlay(gws, sws, sms, epv)
plot(0, 1, Gnts+1, nlay=nlay, plotWlev=True, plotPrec=True, plotDis=False, plotBal=False, savefig=True) #True False

### FOR SINGLE LAYERED SM PLOTS ###
# gws = np.empty((elems, Gnts+1), dtype=np.float64, order='F')
# sws = np.empty((elems, Gnts+1), dtype=np.float64, order='F')
# sms = np.empty((elems, Gnts+1), dtype=np.float64, order='F')
# epv = np.empty((elems, Gnts+1), dtype=np.float64, order='F')
# GWSWEX.pass_vars(gws, sws, sms, epv)
# plot(0, 1, Gnts+1, nlay=1, plotWlev=True, plotPrec=True, plotDis=True, plotBal=True, savefig=True) #True False

gw_dis, sw_dis, uz_dis, qdiff = np.empty(gws.shape, dtype=np.float64, order='F'), np.empty(gws.shape, dtype=np.float64, order='F'), np.empty(gws.shape, dtype=np.float64, order='F'), np.empty(gws.shape, dtype=np.float64, order='F')
GWSWEX.pass_dis(gw_dis, uz_dis, sw_dis, qdiff)

uzs = np.empty((nelems, Gnts+1), dtype=np.float64, order='F')
epv = np.empty((nelems, Gnts+1), dtype=np.float64, order='F')
GWSWEX.pass_vars(gws, sws, uzs, epv)

influx = (p.sum())*Gdt - (et.sum())*Gdt + lateral_gw_flux_sum + lateral_sw_flux_sum
delta_storages = (uzs[:,-1]-uzs[0,0]).sum() + ((gws[:,-1]-gws[:,0])*pvanGI.theta_s).sum() + (sws[:,-1]-sws[:,0]).sum()

print("mbal err: {:.2e}".format(influx-delta_storages))
print("mbal err %: {:.2e}".format((influx-delta_storages)/influx))

plot(0, 1, Gnts+1, nlay=nlay, plotWlev=False, plotPrec=False, plotDis=True, plotBal=True, savefig=True) #True False

GWSWEX.finalize()

# plt.figure()
# plt.plot(gw_res)
# plt.show()
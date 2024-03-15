mdef_tformat = '%Y%m%d %H%M%S'
mdef = {
    'model': {
        'domain': {
            'nelementslements': nelements,
            'dt': int(Gdt.total_seconds()),
            'tstart': tstart.strftime(mdef_tformat),
            'tstop': tstop.strftime(mdef_tformat),
            'nlay': nlay,
            'top': 'top.ip',
            'bot': 'bot.ip',
            'layer1': {
                'name': 'l1',
                'isactive': 'l1_active.ip',
                'vanG': vG[:],
                'ks': 'l1_ks.ip',
                'porosity': 'l1_porosity.ip'
            },
            'layer2': {
                'name': 'l2',
                'isactive': 'l2_active.ip',
                'vanG': vG[:],
                'ks': 'l2_ks.ip',
                'porosity': 'l2_porosity.ip'
            },
            'layer3': {
                'name': 'l3',
                'isactive': 'l3_active.ip',
                'vanG': vG[:],
                'ks': 'l3_ks.ip',
                'porosity': 'l3_porosity.ip'
            },
            'layer4': {
                'name': 'l4',
                'isactive': 'l4_active.ip',
                'vanG': vG[:],
                'ks': 'l4_ks.ip',
                'porosity': 'l4_porosity.ip'
            },
            'layer5': {
                'name': 'l5',
                'isactive': 'l5_active.ip',
                'vanG': vG[:],
                'ks': 'l5_ks.ip',
                'porosity': 'l5_porosity.ip'
            }
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
            'pet_intensities': [0.05/3600, 0.11/3600, 0.21/3600],
            'pet_nts': [1, 1, 1]
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

with open('../tilted-v.yml', 'w') as outfile:
    yaml.dump(mdef, outfile, sort_keys=False)

mdef['auxiliary'] = {}
mdef['auxiliary']['mdef_tformat'] = mdef_tformat
mdef['auxiliary']['Gnts'] = Gnts
mdef['auxiliary']['tstart_dt'] = tstart
mdef['auxiliary']['tstop_dt'] = tstop

with open('../../common/data/mdef.pkl', 'wb') as file:
    pickle.dump(mdef, file)
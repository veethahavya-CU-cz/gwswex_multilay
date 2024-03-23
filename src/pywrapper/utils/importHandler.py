import importlib
import sys
import os

import numpy as np


lib_paths = [
    "/opt/gwswex/lib/",
    "../../lib/",
    "../../build/lib/",
]

try:
    sys.path.append(os.path.abspath("/opt/gwswex/lib/"))
    sys.path.append(os.path.abspath("../../lib/"))
    sys.path.append(os.path.abspath("../../build/lib/"))

    import gwswex_f2pywrapper as f2py_wrapper
    
except ImportError:
    raise ModuleNotFoundError(
        f"Could not find the gwswex_f2pywrapper module. \
            Expected locations: /opt/gwswex/lib/ or {os.path.abspath('../../lib/')}"
    )


import yaml

def __custom_yaml_dumper(dumper, data):
    if len(data) == 1:
        return dumper.represent_scalar("tag:yaml.org,2002:seq", str(data[0]))
    else:
        return dumper.represent_sequence("tag:yaml.org,2002:seq", data, flow_style=True)

def numpy_tostring_representer(dumper, data):
    return dumper.represent_scalar('tag:yaml.org,2002:str', np.array2string(data))

yaml.SafeDumper.add_representer(np.ndarray, numpy_tostring_representer)
yaml.add_representer(list, __custom_yaml_dumper)


def re_import(module_name: str, module_path: str) -> None:
    sys.path.append(os.path.dirname(module_path))
    importlib.reload(sys.modules[module_name])
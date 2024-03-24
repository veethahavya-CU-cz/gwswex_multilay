from .utils.customIO import _logger

from .utils.customTypes import Model
from .utils.customTypes import boundaryConditions as boundary
from .utils.customTypes import externalForcings as forcing
from .utils.customTypes import soilLayer as layer

from .utils.importHandler import f2py_wrapper


class gwswexModel:
    """Top-Level Class for the GWSWEX model."""

    def __init__(self, fpath_config: str, logfile: str = None, verbose: bool = False, debug: bool = False):
        """Initializes the GWSWEX model.

        Args:
            fpath_config (str, optional): Path to the configuration file. Defaults to None.
            verbose (bool, optional): If True, additional information will be printed during initialization. Defaults to False.
        """
        self.model = Model(fpath_config, logfile=logfile, verbose=True, debug=False)

    
    def init(self):
        self.model.init()

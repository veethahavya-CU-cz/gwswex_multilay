from .utils.customIO import _logger

from .utils.customTypes import model
from .utils.customTypes import _boundary_mdc as boundary
from .utils.customTypes import _forcing_mdc as forcing
from .utils.customTypes import _layer_mdc as layer

from .utils.importHandler import gwswex as fpywrapper


class gwswex_model:
    """Top-Level Class for the GWSWEX model."""

    def __init__(self, fpath_config: str, logfile: str = None, verbose: bool = False, debug: bool = False):
        """Initializes the GWSWEX model.

        Args:
            fpath_config (str, optional): Path to the configuration file. Defaults to None.
            verbose (bool, optional): If True, additional information will be printed during initialization. Defaults to False.
        """
        self.model = model(fpath_config, logfile=logfile, verbose=True, debug=False)

    
    def init(self):
        self.model.register()

import sys

import logging


class CustomLoggingFormatter(logging.Formatter):
    def format(self, record):
        log_time = self.formatTime(record, "%H:%M:%S")
        return f"[{record.levelname}] [{log_time}]: {record.getMessage()}"


class _logger:
    def __init__(self, fname: str = None, level: int = logging.INFO):
        self.fname = fname
        self.level = level

        self._logger = logging.getLogger('gwswex_pywrapper')
        self._logger.setLevel(level)
        formatter = CustomLoggingFormatter()

        self.stdout_handler = logging.StreamHandler(sys.stdout)
        self.stdout_handler.setLevel(logging.ERROR)
        self.stdout_handler.setFormatter(formatter)
        self._logger.addHandler(self.stdout_handler)

        self.file_handler = logging.FileHandler(fname, 'w')
        self.file_handler.setLevel(level)
        self.file_handler.setFormatter(formatter)
        self._logger.addHandler(self.file_handler)

    def set_level(self, level: int):
        self.level = level
        self._logger.setLevel(self.level)

    def get_level(self):
        return self.level

    def debug(self, message: str):
        self._logger.debug(message)

    def info(self, message: str):
        self._logger.info(message)

    def warning(self, message: str):
        self._logger.warning(message)

    def error(self, message: str):
        self._logger.error(message)

    def critical(self, message: str):
        self._logger.critical(message)

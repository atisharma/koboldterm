import readline
import hy
from importlib.metadata import version, PackageNotFoundError

import koboldterm.repl


try:
    __version__ = version("package-name")
except PackageNotFoundError:
    # package is not installed
    pass

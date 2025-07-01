import os
from pathlib import Path
from typing import Optional



CWD = os.getcwd()
DESTPATH: Optional[Path] = None
LOGPATH = Path(CWD, "log.yaml")
DEVMODE       = True
DRYRUN        = True
SILENTMODE    = False
COPYOVERWRITE = False
COPYSYNC      = True
EXPORTLOG     = False
REMOVE_EMPTY_HEADER = False

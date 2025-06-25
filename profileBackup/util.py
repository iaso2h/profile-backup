import config

import re
import logging
import datetime
from logging.handlers import RotatingFileHandler
from rich.console import Console
console = Console()
richTextPat = re.compile(r"\[/?[a-zA-Z ]+]")

# Logging set up
handler = RotatingFileHandler(
    config.LOGPATH,
    maxBytes=5 * 1024 * 1024,  # 5 MB
    backupCount=3,
    encoding="utf-8",
)
# formatter = logging.Formatter("%(asctime)s - %(message)s")
handler.setFormatter(
    logging.Formatter("%(message)s")
)

logger = logging.getLogger("profileBackup")
logger.setLevel(logging.INFO)
logger.addHandler(handler)

def getTimeStamp() -> str:
    now = datetime.datetime.now()
    # return str(now.strftime("%H:%M:%S"))
    return "[{}]".format(
        str(
            now.strftime(f"{now.month}/%d %H:%M:%S")
        )
    )
    # return str(now.strftime(f"%Y/{now.month}/%d %H:%M:%S"))


def humanReadableSize(sizeBytes: int) -> str:
    """Convert a file size in bytes to a human-readable format (KB, MB, GB, etc.)."""
    if sizeBytes == 0:
        return "0 B"

    units = ["B", "KB", "MB", "GB", "TB", "PB", "EB", "ZB", "YB"]
    unitIdx = 0
    size = float(sizeBytes)

    while size >= 1024 and unitIdx < len(units) - 1:
        size /= 1024
        unitIdx += 1

    return f"{size:.2f} {units[unitIdx]}"  # 2 decimal places (e.g., "3.14 MB")


def print(*args, **kwargs):
    if not config.SILENTMODE:
        if len(args) == 1 and isinstance(args[0], str):
            if "Skip unchanged" in args[0]:
                return

        console.print(*args, **kwargs)
    if config.EXPORTLOG:
        convertedArgs = []
        for arg in args:
            if isinstance(arg, str):
                convertedArgs.append(richTextPat.sub("", arg))
            else:
                convertedArgs.append(str(arg))

        logMessage = "\n".join(convertedArgs)
        logger.info(logMessage)


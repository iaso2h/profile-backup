import config

import re
import winreg
import logging
import datetime
from typing import Optional, Tuple
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


def print(*args, skipChk:bool=True, **kwargs) -> None:
    if not config.SILENTMODE:
        if len(args) == 1 and isinstance(args[0], str):
            if (skipChk and "skip" in args[0].lower()) or "permission error" in args[0]:
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


regexSpecialCharPat = re.compile(r"^[a-zA-Z]\W*$")
def regGlobKeys(keyPathPat) -> Tuple[list[str], str]:
    if not isinstance(keyPathPat, str):
        raise ValueError("string value is expected from keyPathPat.")
    if "/" in keyPathPat:
        raise ValueError('"/" character is not allowed in the keyPathPat.')
    if keyPathPat[0] == "\\":
        raise ValueError("the keypath must not start with a backslash.")

    keyGlobPaths = []
    # Get key components
    keyComponent = keyPathPat.split("\\")
    keyComponentInvalidIndex = [
        idx for idx, component in enumerate(keyComponent) if regexSpecialCharPat.fullmatch(component)  # Exclude regex patterns like \s*, \d+
    ]

    # Get HKEY
    hkeyStr = keyComponent[0]
    try:
        hkey = getattr(winreg, hkeyStr)
    except AttributeError:
        raise ValueError(f'invalid registry hkey "{hkeyStr}" for keyPathPat.')

    # Get path glob pattern
    if len(keyComponentInvalidIndex) > 1:
        raise ValueError(f"number of regex specical characters cannot exceed 1 in keyPathPat.")
    elif len(keyComponentInvalidIndex) == 0:
        # Doen't contain any regex sepcial characters, return the full path pattern
        return ["\\".join(keyComponent[1:])], hkeyStr
    else:
        keyGlobPatternMid = "\\".join(keyComponent[1:keyComponentInvalidIndex[0] - 1])
        # Test if the middle part of the path pattern is valid
        try:
            keyGlobStart = winreg.OpenKey(hkey, keyGlobPatternMid)
        except FileNotFoundError:
            return keyGlobPaths, hkeyStr

        keyGlobPatternHead = "\\".join(keyComponent[0:keyComponentInvalidIndex[0] - 1])
        keyGlobPatternTail = "\\" + "\\".join(keyComponent[keyComponentInvalidIndex[0] + 1 :]) if keyComponentInvalidIndex[0] != len(keyComponent) - 1 else ""
        keyGlobPatternStr = "{}\\\\{}".format(
            re.escape(keyGlobPatternHead),
            "\\".join(keyComponent[keyComponentInvalidIndex[0] - 1: keyComponentInvalidIndex[0] + 1]),
        )
        keyGlobPattern = re.compile(keyGlobPatternStr)


    # Glob as many paths that match the glob pattern
    i = 0
    while True:
        try:
            subkeyName = winreg.EnumKey(keyGlobStart, i)
            globPathHead = f"{keyGlobPatternHead}\\{subkeyName}"
            if keyGlobPattern.search(globPathHead):
                keyGlobPaths.append("{}\\{}{}".format(
                    keyGlobPatternMid,
                    subkeyName,
                    keyGlobPatternTail
                    )
                )

            i += 1
        except OSError:
            break
    keyGlobStart.Close()


    return keyGlobPaths, hkeyStr


def regQueryData(keyPathPat: str, valueName: str) -> Tuple[str, bool]:
    valData = ""
    succeedChk = False
    keyGlobPaths, hkeyStr = regGlobKeys(keyPathPat)
    if not keyGlobPaths:
        return valData, succeedChk
    # Always get the last keyPath
    keyMidPath = keyGlobPaths[-1]
    hkey = getattr(winreg, hkeyStr)

    with winreg.OpenKey(hkey, keyMidPath) as key:
        try:
            valData, regtype = winreg.QueryValueEx(key, valueName)
        except WindowsError:
            pass

    if valData != "":
        succeedChk = True

    return valData, succeedChk

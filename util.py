import re
import winreg
import logging
import datetime
from typing import Optional, Tuple
from logging.handlers import RotatingFileHandler
from rich.console import Console

import config
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
    """
    Returns current timestamp in format '[MM/DD HH:MM:SS]'.

    Returns:
        str: Formatted timestamp string enclosed in square brackets.
    """
    now = datetime.datetime.now()
    # return str(now.strftime("%H:%M:%S"))
    return "[{}]".format(str(now.strftime(f"{now.month}/%d %H:%M:%S")))
    # return str(now.strftime(f"%Y/{now.month}/%d %H:%M:%S"))


def humanReadableSize(sizeBytes: int) -> str:
    """
    Convert bytes to human-readable string with appropriate unit (KB, MB, etc.).

    Args:
        sizeBytes: The size in bytes to be converted.

    Returns:
        str: Formatted string with size and unit (e.g. "3.14 MB").
    """
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


def print(*args, skipChk: bool = True, **kwargs) -> None:
    """
    Custom print function that handles console output and logging.

    Args:
        *args: Variable length argument list to be printed.
        skipChk (bool): If True, skips printing messages containing 'skip'. Defaults to True.
        **kwargs: Arbitrary keyword arguments passed to console.print().

    Notes:
        - Respects SILENTMODE configuration (no output when True)
        - Automatically skips messages containing 'skip' (when skipChk=True) or 'permission error'
        - Logs all messages to file when EXPORTLOG is True (with rich text formatting removed)
    """
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


def containsRegexSyntax(s):
    """
    Checks if a string contains regular expression syntax.

    Args:
        s (str): The string to check for regex syntax.

    Returns:
        bool: True if the string contains regex metacharacters or fails compilation,
              False otherwise.

    Note:
        This function may return false positives for strings containing regex
        metacharacters that aren't actually used as regex (e.g. literal dots).
    """
    regexMetacharacters = r".^$*+?{}[]\|()-"  # List of common regex metacharacters

    try:
        re.compile(s)
        # If compilation succeeds, check for presence of common metacharacters.
        # A string like "abc" will compile successfully but isn't regex syntax.
        # We want to identify if it *uses* regex syntax.
        for char in regexMetacharacters:
            if char in s:
                # Special case for hyphen inside character classes (e.g., [a-z])
                # is complex to check perfectly without a full regex parser.
                # For simplicity, we'll mark any presence of metacharacters.
                return True
        return False # No invalid syntax and no obvious metacharacters
    except re.error:
        return True # Compilation failed, definitely contains regex syntax issues


# Iterating over each component to get the key paths match the glob pattern
# Do nothing for the root key, so initiate the loop from the second component(0-based index)
def getGlobPattern(
    keyComponents: list[str],
    keyComponentRegexIndex: list[int],
    keyParent: winreg.HKEYType,
    keyRelRootStr: str,
    componentIdxInit: int = 1,
) -> list[str]:
    """
    Recursively searches Windows registry keys matching given patterns.

    This function implements a glob-like pattern matching system for Windows registry keys,
    supporting both exact matches and regular expression patterns. It recursively traverses
    the registry tree to find all matching keys.

    Args:
        keyComponents (list[str]): List of registry key components to match. Each component
            represents one level in the registry hierarchy.
        keyComponentRegexIndex (list[int]): List of indices indicating which components
            should be treated as regex patterns. Components not in this list are matched exactly.
        keyParent (winreg.HKEYType): Parent registry key handle to start the search from.
            This should be an open registry key handle.
        keyRelRootStr (str): Relative path string of the parent key, used for building
            complete paths during recursion.
        componentIdxInit (int, optional): Starting index in keyComponents to begin matching.
            Defaults to 1 to skip the root key.

    Returns:
        list[str]: List of matching subkey names or relative paths. Returns an empty list
            if no matches are found. For deep matches, returns full relative paths from
            the starting point.

    Raises:
        ValueError: If no matching key path pattern is found or if the pattern is invalid.
        OSError: If there are permission issues or other Windows registry access errors.

    Example:
        >>> components = ["HKEY_CURRENT_USER", "Software", ".*", "Settings"]
        >>> regex_indices = [2]  # Third component is a regex pattern
        >>> root_key = winreg.HKEY_CURRENT_USER
        >>> matches = getGlobPattern(components, regex_indices, root_key, "HKEY_CURRENT_USER")
        >>> # Returns paths like ["Software\\Microsoft\\Settings", "Software\\Adobe\\Settings"]

    Note:
        - The function uses Windows-style backslashes in returned paths
        - Regular expression components are matched using re.search()
        - The function automatically handles registry key enumeration
        - Supports both shallow and deep pattern matching
        - Thread-safe as long as the registry handles are not shared
    """
    subkeyNames: list[str] = []
    for componentIdx, component in enumerate(keyComponents):
        if componentIdx < componentIdxInit:
            continue

        keyIdx = 0

        if componentIdx in keyComponentRegexIndex:
            componentPat = re.compile(component)

        while True:
            try:
                subkeyName = winreg.EnumKey(keyParent, keyIdx)

                if (
                    componentIdx not in keyComponentRegexIndex
                    and component == subkeyName
                ) or (
                    componentIdx in keyComponentRegexIndex
                    and componentPat.search(subkeyName) # pyright: ignore [reportPossiblyUnboundVariable]
                ):
                    # Collect matching subkey name. Deal with them until no
                    # more subkeys are found in current registry key level
                    subkeyNames.append(subkeyName)

                    keyIdx += 1 # Enter next iteration to glob other subkeys that might match the pattern
                else:
                    keyIdx += 1 # Enter next iteration
            except OSError:
                # No match found
                if len(subkeyNames) == 0:
                    return subkeyNames

                if componentIdx == len(keyComponents) - 1:
                    # Deepest level reached, return the key names
                    return subkeyNames
                else:
                    # Glob deeper level, and compose the relative key paths by
                    # combining subkey names at current level with the ones
                    # from deeper level
                    flatKeyRelPaths = []
                    for subkeyName in subkeyNames:
                        with winreg.OpenKey(keyParent, subkeyName) as subKeyParent:
                            subkeyNamesDeeper = getGlobPattern(
                                keyComponents,
                                keyComponentRegexIndex,
                                subKeyParent,
                                f"{keyRelRootStr}\\{subkeyName}",
                                componentIdx + 1
                            )
                        flatKeyRelPaths.extend([subkeyName + "\\" + keyRelPathDeeper for keyRelPathDeeper in subkeyNamesDeeper])

                    return flatKeyRelPaths

    raise ValueError("Failed to find the key path pattern.")
    # Fallback value after iterating all components
    # return ""


def regGlobKeyRelPaths(keyPathPat) -> Tuple[list[str], Optional[str]]:
    """
    Searches for registry keys matching a given pattern with optional regex components.

    This function provides a high-level interface for searching the Windows registry
    using glob-like patterns with regex support. It parses the input pattern,
    validates it, and delegates the actual search to getGlobPattern().

    Args:
        keyPathPat (str): Registry key path pattern (e.g. "HKEY_CURRENT_USER/Software/*/Settings").
                          Must not start with backslash and must contain at least one subkey.
                          Components are separated by forward slashes (/).
                          Components can contain regex patterns for flexible matching.

    Returns:
        Tuple[list[str], Optional[str]]:
            - List of matching relative key paths (without root key), using Windows-style
              backslashes as separators
            - Root key string (e.g. "HKEY_CURRENT_USER") or None if no matches found

    Raises:
        ValueError: For invalid path patterns, non-existent root keys, or if the pattern
                   doesn't contain at least one subkey component.
        AttributeError: If the specified root key doesn't exist in winreg.
        TypeError: If keyPathPat is not a string.

    Example:
        >>> paths, root = regGlobKeyRelPaths("HKEY_CURRENT_USER/Software/.*/Settings")
        >>> # Might return (["Software\\Microsoft\\Settings", "Software\\Adobe\\Settings"], "HKEY_CURRENT_USER")

    Note:
        - Input pattern uses forward slashes (/) as separators
        - Output paths use Windows-style backslashes (\\) as separators
        - The function automatically detects regex syntax in pattern components
        - For non-regex patterns, performs a direct key lookup for efficiency
        - Returns empty list and None if no matches are found
        - Thread-safe as it creates new registry handles for each call
    """
    if not isinstance(keyPathPat, str):
        raise ValueError("string value is expected from key path pattern.")
    if keyPathPat[0] == "\\" or keyPathPat[0] == "/":
        raise ValueError("the keypath must not start with a backslash.")

    # Get key components
    keyComponents = keyPathPat.split("/")
    # The key path pattern cannot contain root key only
    if len(keyComponents) == 1:
        raise ValueError("invalid key path pattern, it must contain at least one key component separated by \"/\" delimiter.")

    # Get HKEY
    rootKeyStr = keyComponents[0]
    try:
        rootKey = getattr(winreg, rootKeyStr)
    except AttributeError:
        raise ValueError(f'invalid registry hkey "{rootKeyStr}" for key path pattern.')

    keyComponentRegexIndexes = [
        idx for idx, component in enumerate(keyComponents) if containsRegexSyntax(component)
    ]

    # Check if regular expression syntax appear in root key
    if len(keyComponentRegexIndexes) == 0:
        # Doen't contain any regex sepcial characters, return the full path pattern
        keyRel = "\\".join(keyComponents[1:])
        try:
            with winreg.OpenKey(rootKey, keyRel) as _:
                pass
        except FileNotFoundError:
            return [], None
        return [keyRel], rootKeyStr
    else:
        return getGlobPattern(
            keyComponents=keyComponents,
            keyComponentRegexIndex=keyComponentRegexIndexes,
            keyParent=rootKey,
            keyRelRootStr=rootKeyStr
        ), rootKeyStr


def regQueryData(keyPathPat: str, valueName: str) -> Tuple[str, bool]:
    """
    Query registry value data from a given key path pattern and value name.

    Args:
        keyPathPat: Registry key path pattern to search (supports glob patterns)
        valueName: Name of the registry value to query

    Returns:
        Tuple[str, bool]: The queried value data and a success flag (True if found)
    """
    valData = ""
    succeedChk = False
    keyRelPaths, hkeyStr = regGlobKeyRelPaths(keyPathPat)
    if not keyRelPaths or not hkeyStr:
        return valData, succeedChk
    # Always get the last keyPath
    keyRelPathLast = keyRelPaths[-1]
    hkey = getattr(winreg, hkeyStr)

    with winreg.OpenKey(hkey, keyRelPathLast) as key:
        try:
            valData, regtype = winreg.QueryValueEx(key, valueName)
        except WindowsError:
            pass

    if valData != "":
        succeedChk = True

    return valData, succeedChk

import shutil
import os
from types import GeneratorType
from typing import Callable, List, Union
from pathlib import Path
from rich.console import Console


# Writable from other files
DESTPATH = None # type: Path
DRYRUN   = True


ALWAYSOVERWRITE = False
console     = Console()
userName    = os.getlogin()
appDataPath = Path("C:/Users/{}/AppData".format(userName))
homePath    = Path("C:/Users/{}".format(userName))


def idx2sequence(index: int):
    num = index + 1
    remainder = num % 10
    suffix = None
    if remainder == 1:
        suffix = "st"
    elif remainder == 2:
        suffix = "nd"
    elif remainder == 3:
        suffix = "rd"
    else:
        suffix = "th"

    return str(num) + suffix

class Ignore():
    def __init__(self, ignoreList):
        self.ignoreList = ignoreList

    @property
    def ignoreList(self):
        return self._ignoreList

    @ignoreList.setter
    def ignoreList(self, val):
        self._ignoreList = val


class Backup():
    # Track the total backup time whenever a file is backup up
    totalBackupCount = 0
    # The total software list has been properly configured
    softwareNameList = []
    # The ticked software list. Written after user confirm the software list to backup
    softwareNameTickedList = []

    # TODO: type check https://stackoverflow.com/questions/2489669/how-do-python-functions-handle-the-types-of-parameters-that-you-pass-in
    def __init__(self, name: str, globPatterns: list):
        self.name = name
        type(self).softwareNameList.append(self.name)

        # The current software sequence during a configured software backup list
        self.softwareSequence = len(type(self).softwareNameList)

        # The value of glob pattern will get validated when assigned value
        self.globPatterns = globPatterns

        # Track the total count of files being backed up related to current software
        self.softwareBackupCount = 0

        # Track the ticked state of a software. It will be written when current
        # software name is ticked during a cli
        self.ticked = False
        # whether to copy files in current parent folder or recursive copy all files nested in folders.
        self.recursiveCopy = True

    @property
    def name(self):
        return self._name
    @name.setter
    def name(self, val):
        if not isinstance(val, str):
            raise ValueError("string expected")
        self._name = val


    @property
    def globPatterns(self):
        return self._globPatterns
    @globPatterns.setter
    def globPatterns(self, arg):
        def skip():
            console.print(f"[gray]  Skipped unfound file at: {str(srcPath)}[/[gray]]")
            arg[i]["parentSrcPath"] = False

        def validParentPathGlob(globPatternIndex: int, parentSrcPath: list[Path]):
            for p in parentSrcPath:
                if p.is_dir():
                    return True

            arg[globPatternIndex]["parentSrcPath"] = False
            return False

        for i, globPattern in enumerate(arg):
            for key, val in globPattern.items():
                # Validate path pattern
                if key == "parentSrcPath":
                    if isinstance(val, GeneratorType):
                        arg[i]["parentSrcPath"] = list(val)
                        # A parent directroy glob paths cannot contain any file path
                        if not arg[i]["parentSrcPath"]:
                            arg[i]["parentSrcPath"] = False
                            continue

                        if not validParentPathGlob(i, arg[i]["parentSrcPath"]):
                            raise ValueError(f"The {idx2sequence(i)} glob pattern of software {self.name} didn't find valid directory path.")
                    elif isinstance(val, str):
                        if "*" in val:
                            # Add "/" suffix in the end if "*" is already the last character to make sure the the glob result return directory path
                            if val[-1:] == "*":
                                val = val + "/"

                            # Turn string literal into a path glob generator
                            if val[1:2] == ":":
                                rootPath = Path(val[0:3])
                                if rootPath.exists:
                                    parentSrcPath = rootPath.glob(val[3:])
                                    if parentSrcPath:
                                        arg[i]["parentSrcPath"] = list(parentSrcPath) # type list[Path]
                                        if not arg[i]["parentSrcPath"]:
                                            arg[i]["parentSrcPath"] = False
                                            continue

                                        # A parent directroy glob paths cannot contain any file path
                                        if not validParentPathGlob(i, arg[i]["parentSrcPath"]):
                                            raise ValueError(f"The {idx2sequence(i)} glob pattern of software {self.name} didn't find valid directory path.")
                                    else:
                                        skip()
                                        continue
                                else:
                                    skip()
                                    continue
                            else:
                                skip()
                                continue
                        else:
                            srcPath = Path(val)
                            if not srcPath.exists():
                                skip()
                                continue
                            elif srcPath.is_file():
                                raise ValueError(f"{self.name}: parent path pattern({str(val)}) cannot be a file path.")
                            else:
                                arg[i]["parentSrcPath"] = [srcPath]
                    else:
                        raise ValueError(
    f"Wrong given parent path pattern. Path glob generator or string is expected from the {idx2sequence(i)} glob pattern of software {self.name}."
                                )
                # Validate version string
                elif key == "versionFind":
                    if not isinstance(val, Callable) and not isinstance(val, str):
                        raise ValueError(
    f"Wrong given version. String or function is expected from the {idx2sequence(i)} glob pattern of software {self.name}."
                                )
                # Validate filter type.
                elif key == "filterType":
                    if not isinstance(val, str):
                        raise ValueError("string expected")
                    if val != "include" and val != "exclude":
                        raise ValueError(f'Wrong given string value from the {idx2sequence(i)} glob pattern of software {self.name}. The value can only be either "include" or "exclude"')
                # Validate filter pattern. It could be a include pattern of a exclude pattern
                elif key == "filterPattern":
                    if not isinstance(val, Callable) and not isinstance(val, list):
                        raise ValueError(
    f"Wrong given filter pattern. List or function is expected from the {idx2sequence(i)} glob pattern of software {self.name}."
                                )
                    if isinstance(val, list):
                        for k in val:
                            if not isinstance(k, str):
                                raise ValueError(f"A filter string list must consist of string only.")

                elif key == "recursiveCopy":
                    if not isinstance(val, bool):
                        raise ValueError(
    f"Wrong given filter pattern from the {idx2sequence(i)} glob pattern of software {self.name}.")
                elif key == "silentReport":
                    if not isinstance(val, bool):
                        raise ValueError(
    f"Wrong given filter pattern from the {idx2sequence(i)} glob pattern of software {self.name}.")
                else:
                    raise ValueError(f"Unrecognized key: {key}")

        self._globPatterns = arg


    @staticmethod
    def copyFile(fileSrcPath: Path, fileDstPath: Path, silentReport: bool) -> int:
        count = 0
        if fileDstPath.exists():
            if ALWAYSOVERWRITE or (fileSrcPath.stat().st_mtime - fileDstPath.stat().st_mtime) > 0:
                if not DRYRUN:
                    shutil.copy2(fileSrcPath, fileDstPath)
                    if not silentReport:
                        console.print(f"[white]    Backing up file: [yellow]{fileSrcPath.name}[/yellow][/white]")
                else:
                    if not silentReport:
                        console.print(f"[white]    Found file: [yellow]{fileSrcPath.name}[/yellow][/white]")

                count = count + 1
            else:
                if not silentReport:
                    console.print(f"[gray]    Skip non-modified file: {fileSrcPath.name}[/gray]")
        else:

            if not DRYRUN:
                os.makedirs(fileDstPath.parent, exist_ok=True)
                shutil.copy2(fileSrcPath, fileDstPath)
                console.print(f"[white]    Backing up file: [yellow]{fileSrcPath.name}[/yellow][/white]")
            else:
                console.print(f"[white]    Found file: [yellow]{fileSrcPath.name}[/yellow][/white]")


            count = count + 1

        return count


    def iterCopy(
            self,
            parentSrcPath: Path,
            parentDstPath: Path,
            filterType: str,
            filterPattern: Union[List[str], Callable[[Path], bool]],
            filterAllPathStrs: list,
            recursiveCopy: bool,
            silentReport: bool,
            topParentSrcPath: Union[None, Path] = None,
            count: int = 0
            ) -> int:
        """Iter through a parent source directory to validate and copy each file

        Args:
            parentSrcPath: what parent source path to iterlate
            parentDstPath: what destination path for the parent directory to backup up
            filter: include or exclude from the filter pattern
            filterPattern: determine what kind of file fit in the filter pattern
            filterAllPathStrs: all the Paths that fit in the filter pattern
            recursiveCopy: whether to recursive copy in all nested sub-direcotries
            silentReport: whether to silent the report message
            topParentSrcPath: parameter for recursive function call

        Returns: how many file has been backed up

        """
        # Initialization for the first funtion call
        if not topParentSrcPath:
            topParentSrcPath = parentSrcPath

        for srcPath in parentSrcPath.iterdir():
            if srcPath.is_dir() and recursiveCopy:
                count = count + self.iterCopy(srcPath, parentDstPath, filterType, filterPattern, filterAllPathStrs, recursiveCopy, silentReport, topParentSrcPath, count)
            else:
                if isinstance(filterPattern, list):
                    if filterType == "exclude" and str(srcPath) in filterAllPathStrs:
                        continue
                    elif filterType == "include" and str(srcPath) not in filterAllPathStrs:
                        continue
                    else:
                        srcRelParentPath = srcPath.relative_to(topParentSrcPath)
                        dstPath = Path(parentDstPath, srcRelParentPath)
                        count = count + self.copyFile(srcPath, dstPath, silentReport)
                else:
                    if filterType == "exclude" and filterPattern(srcPath):
                        continue
                    elif filterType == "include" and not filterPattern(srcPath):
                        continue
                    else:
                        srcRelParentPath = srcPath.relative_to(topParentSrcPath)
                        dstPath = Path(parentDstPath, srcRelParentPath)
                        count = count + self.copyFile(srcPath, dstPath, silentReport)

        return count


    def backup(self):
        console.print(f"[white]Checking up [green bold]{self.name}[/green bold][/white]...")

        for globPattern in self.globPatterns:
            parentSrcPaths = globPattern["parentSrcPath"] # type: list
            if not parentSrcPaths:
                continue

            versionFind   = globPattern["versionFind"]   # type: Callable | str
            filterType    = globPattern["filterType"]    # type: str
            filterPattern = globPattern["filterPattern"] # type: Callable | list
            recursiveCopy = globPattern["recursiveCopy"] # type: bool
            silentReport  = globPattern["silentReport"]  # type: bool

            parentSrcPath = None #type: Path
            parentDstPath = None #type: Path

            for parentSrcPath in parentSrcPaths:
                if parentSrcPath.is_file():
                    raise ValueError(f"{self.name}: parent path pattern({str(parentSrcPath)}) cannot be a file path.")

                if isinstance(versionFind, str):
                    versionStr = versionFind
                else:
                    try:
                        versionStr = versionFind(parentSrcPath)
                    except Exception as e:
                        console.print(e)
                        console.print('[red]  Version string use "unnamed" instead\n[/red]')
                        versionStr = "unnamed"

                console.print(f"[white]  Checking up [green bold]{self.name} {versionStr}[/green bold] files inside folder: [yellow]{parentSrcPath}[/yellow][/white]")

                parentSrcRelAnchorPath = parentSrcPath.relative_to(parentSrcPath.anchor)
                # NOTE: versionStr can be an empty string
                parentDstPath = Path(
                        DESTPATH,
                        self.name,
                        versionStr,
                        parentSrcPath.anchor[:1],
                        parentSrcRelAnchorPath
                    )

                # Get all filter pattern paths
                filterAllPaths = []
                if isinstance(filterPattern, list):
                    for pattern in filterPattern:
                        if not pattern.startswith("\\") and pattern.startswith("/"):
                            pattern = "/" + pattern

                        filterPaths = list(parentSrcPath.glob(pattern))
                        if len(filterPaths) == 0:
                            continue
                        filterAllPaths.extend(filterPaths)

                filterAllPathStrs = list(map(lambda p: str(p), filterAllPaths))

                # Filter out path that match the excluded paths
                currentParentSrcCount = self.iterCopy(parentSrcPath, parentDstPath, filterType, filterPattern, filterAllPathStrs, recursiveCopy, silentReport)
                self.softwareBackupCount = self.softwareBackupCount + currentParentSrcCount

                # Report count for the current parent source directory
                if not DRYRUN:
                    console.print(f"[white]Backed up [purple bold]{currentParentSrcCount}[/purple bold] {self.name} files in: {str(parentSrcPath)}\n[/white]")
                else:
                    console.print(f"[white]Found [purple bold]{currentParentSrcCount}[/purple bold] {self.name} files in: {str(parentSrcPath)}\n[/white]")

        # Report total count for the current Backup(software setting) config
        if not DRYRUN:
            console.print(f"[white]Backed up [purple bold]{self.softwareBackupCount}[/purple bold] {self.name} files\n[/white]")
        else:
            console.print(f"[white]Found [purple bold]{self.softwareBackupCount}[/purple bold] {self.name} files\n[/white]")

        type(self).totalBackupCount = type(self).totalBackupCount + self.softwareBackupCount
        # Report the total count as the last object
        if self.softwareSequence == len(type(self).softwareNameList):
            if not DRYRUN:
                console.print(f"[white]Backed up [purple bold]{type(self).totalBackupCount}[/purple bold] files from [green]{type(self).softwareNameTickedList}[/green].\n[/white]")
            else:
                console.print(f"[white]Found [purple bold]{type(self).totalBackupCount}[/purple bold] files from [green]{type(self).softwareNameTickedList}[/green].\n[/white]")



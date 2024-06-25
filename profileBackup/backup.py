import shutil
import os
from types import GeneratorType
from typing import Callable, Generator, List, Union
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

        def validParentPathGlob(parentSrcPath: list[Path]):
            for i in parentSrcPath:
                if i.is_file:
                    return False

            arg[i]["parentSrcPath"] = False
            return True

        for i, globPattern in enumerate(arg):
            for key, val in globPattern.items():
                # Validate path pattern
                if key == "parentSrcPath":
                    if isinstance(val, GeneratorType):
                        arg[i]["parentSrcPath"] = list(val)
                        # A parent directroy glob paths cannot contain any file path
                        if not validParentPathGlob(arg[i]["parentSrcPath"]):
                            raise ValueError(f"The {idx2sequence(i)} glob pattern of software {self.name} is targetting a file path as parent directory path.")
                    elif isinstance(val, str):
                        if "*" in val:
                            # Turn string literal into a path glob generator
                            if val[1:2] == ":":
                                rootPath = Path(val[0:2])
                                if rootPath.exists:
                                    parentSrcPath = rootPath.glob(val[3:])
                                    if parentSrcPath:
                                        arg[i]["parentSrcPath"] = list(parentSrcPath) # type list[Path]
                                        # A parent directroy glob paths cannot contain any file path
                                        if not validParentPathGlob(arg[i]["parentSrcPath"]):
                                            raise ValueError(f"The {idx2sequence(i)} glob pattern of software {self.name} is targetting a file path as parent directory path.")
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
                elif key == "includeType":
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
                else:
                    raise ValueError(f"Unrecognized key: {key}")

        self._globPatterns = arg


    @staticmethod
    def copyFile(fileSrcPath: Path, fileDstPath: Path) -> int:
        count = 0
        if fileDstPath.exists():
            if ALWAYSOVERWRITE or (fileSrcPath.stat().st_mtime - fileDstPath.stat().st_mtime) > 0:
                if not DRYRUN:
                    shutil.copy2(fileSrcPath, fileDstPath)
                    console.print(f"[white]    Backing up file: [yellow]{fileSrcPath.name}[/yellow][/white]")
                else:
                    console.print(f"[white]    Found file: [yellow]{fileSrcPath.name}[/yellow][/white]")

                count = count + 1
            else:
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


    def iterRecursive(self, parentSrcPath: Path, parentDstPath: Path, typeStr: str, filterPattern: Union[List[str], Callable[[Path], bool]] , filterAllPathStrs: list, recursiveCopy: bool, topParentSrcPath: Path = None) -> int:
        count = 0

        if not topParentSrcPath:
            topParentSrcPath = parentSrcPath

        for srcPath in parentSrcPath.iterdir():
            if srcPath.is_dir() and recursiveCopy:
                count = count + self.iterRecursive(srcPath, parentDstPath, typeStr, filterPattern, filterAllPathStrs, recursiveCopy, topParentSrcPath)
            else:
                if isinstance(filterPattern, list):
                    if typeStr == "exclude" and str(srcPath) in filterAllPathStrs:
                        continue
                    elif typeStr == "include" and str(srcPath) not in filterAllPathStrs:
                        continue
                    else:
                        srcRelParentPath = srcPath.relative_to(topParentSrcPath)
                        dstPath = Path(parentDstPath, srcRelParentPath)
                        count = count + self.copyFile(srcPath, dstPath)
                else:
                    if typeStr == "exclude" and filterPattern(srcPath):
                        continue
                    elif typeStr == "include" and not filterPattern(srcPath):
                        continue
                    else:
                        srcRelParentPath = srcPath.relative_to(topParentSrcPath)
                        dstPath = Path(parentDstPath, srcRelParentPath)
                        count = count + self.copyFile(srcPath, dstPath)

        return count


    def backup(self):
        console.print(f"[white]Checking up [green bold]{self.name}[/green bold][/white]...")

        for globPattern in self.globPatterns:
            parentSrcPaths = globPattern["parentSrcPath"] # type: list | str | bool
            if not parentSrcPaths:
                continue

            versionFind   = globPattern["versionFind"]   # type: Callable | str
            typeStr       = globPattern["includeType"]   # type: str
            filterPattern = globPattern["filterPattern"] # type: Callable | list
            recursiveCopy = globPattern["recursiveCopy"] # type: Callable | list

            parentSrcPath = None #type: Path
            parentDstPath = None #type: Path

            for parentSrcPath in parentSrcPaths:
                if parentSrcPath.is_file():
                    raise ValueError(f"{self.name}: parent path pattern({str(parentSrcPath)}) cannot be a file path.")

                if isinstance(versionFind, str):
                    versionStr = versionFind
                else:
                    versionStr = versionFind(parentSrcPath)
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
                parentSrcCount = self.iterRecursive(parentSrcPath, parentDstPath, typeStr, filterPattern, filterAllPathStrs, recursiveCopy)
                self.softwareBackupCount = self.softwareBackupCount + parentSrcCount
        if not DRYRUN:
            console.print(f"[white]Backed up [purple bold]{self.softwareBackupCount}[/purple bold] {self.name} files\n[/white]")
        else:
            console.print(f"[white]Found [purple bold]{self.softwareBackupCount}[/purple bold] {self.name} files\n[/white]")


        # Record
        type(self).totalBackupCount = type(self).totalBackupCount + self.softwareBackupCount
        # Report the total count as the last object

        if self.softwareSequence == len(type(self).softwareNameList):
            if not DRYRUN:
                console.print(f"[white]Backed up [purple bold]{type(self).totalBackupCount}[/purple bold] files from [green]{type(self).softwareNameTickedList}[/green].\n[/white]")
            else:
                console.print(f"[white]Found [purple bold]{type(self).totalBackupCount}[/purple bold] files from [green]{type(self).softwareNameTickedList}[/green].\n[/white]")



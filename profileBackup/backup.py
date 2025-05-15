import shutil

import os
from types import GeneratorType
from typing import Callable, TypedDict, Optional
from pathlib import Path
from rich.console import Console


# Writable from other files
DESTPATH: Optional[Path] = None
DRYRUN     = True
SILENTMODE = False


COPYOVERWRITE = False
COPYSYNC      = True
console     = Console()
userName    = os.getlogin()
appDataPath = Path("C:/Users/{}/AppData".format(userName))
homePath    = Path("C:/Users/{}".format(userName))


def print(*args, **kwargs):
    if not SILENTMODE:
        console.print(*args, **kwargs)


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

class profileConfig(TypedDict):
    name: str
    enabled: bool
    globPatterns: list

class Profile():
    totalBackupCount = 0
    profileList = []
    profileEnabledList = []
    profileTickedList = []

    syncFilesToDelete = {}
    fitPatBackupRelStr = {}

    # TODO: type check https://stackoverflow.com/questions/2489669/how-do-python-functions-handle-the-types-of-parameters-that-you-pass-in
    def __init__(
            self,
            name: str,
            category: str,
            enabled: bool,
            recursiveCopy: bool,
            silentReport: bool,
            parentSrcPaths,
            versionFind,
            filterType: str,
            filterPattern,
            ):
        self.name = name
        self.category = category
        self.enabled = enabled
        self.recursiveCopy = recursiveCopy
        self.silentReport = silentReport
        self.parentSrcPaths = parentSrcPaths
        self.versionFind = versionFind
        self.filterType = filterType
        self.filterPattern = filterPattern

        type(self).profileList.append(self)

        # Deafult value
        self.versionStr = ""
        self.softwareBackupCount = 0
        self.ticked = False
        self.enabledIndex = -1


    # Validation of name {{{
    @property
    def name(self):
        return self._name
    @name.setter
    def name(self, val):
        if not isinstance(val, str):
            raise ValueError(f"string value is expected from the name parameter from {self.category} from {self.name} configuration.")
        self._name = val
    # }}}

    # Validation of category {{{
    @property
    def category(self):
        return self._category
    @category.setter
    def category(self, val):
        if not isinstance(val, str):
            raise ValueError(f"string value is expected from the category parameter from {self.category} from {self.name} configuration.")
        self._category = val
    # }}}

    # Validation of enabled {{{
    @property
    def enabled(self):
        return self._enabled
    @enabled.setter
    def enabled(self, val):
        if not isinstance(val, bool):
            raise ValueError(f"bool value is expected from the enabled parameter from {self.category} from {self.name} configuration.")
        self._enabled = val
    # }}}

    # Validation of recursiveCopy {{{
    @property
    def recursiveCopy(self):
        return self._recursiveCopy
    @recursiveCopy.setter
    def recursiveCopy(self, val):
        if not isinstance(val, bool):
            raise ValueError(f"bool value is expected from the recursiveCopy parameter from {self.category} from {self.name} configuration.")
        self._recursiveCopy = val
    # }}}

    # Validation of silentReport {{{
    @property
    def silentReport(self):
        return self._silentReport
    @silentReport.setter
    def silentReport(self, val):
        if not isinstance(val, bool):
            raise ValueError(f"bool is expected from the silentReport parameter from {self.category} from {self.name} configuration.")
        self._silentReport = val
    # }}}

    # Validation of parentSrcPath {{{
    @property
    def parentSrcPaths(self) -> list[Path]:
        return self._parentSrcPath # type: ignore
    @parentSrcPaths.setter
    def parentSrcPaths(self, val):
        # Validate path pattern
        def skip():
            print(f"[gray]Skipped unfound profile for {self.category} from {self.name}[/[gray]]")
            self.enabled = False
            self.updateEnabledList()

        def checkDirPath(paths: list[ Path ]):
            for p in paths:
                if p.is_dir():
                    return True
            return False


        if isinstance(val, GeneratorType) or isinstance(val, map):
            self._parentSrcPath = list(val)
            if not checkDirPath(self._parentSrcPath):
                skip()
        elif isinstance(val, Path):
            srcPath = val
            if not srcPath.exists():
                skip()
            elif srcPath.is_file():
                skip()
            else:
                self._parentSrcPath = [val]
        elif isinstance(val, str):
            if "*" in val:
                # Adding "/" suffix to the end if "*" is already the last character to make sure the the glob result always return a directory path
                if val[-1:] == "*":
                    val = val + "/"

                # Turn string literal into a path glob generator
                if val[1:2] == ":":
                    rootPath = Path(val[0:3])
                    if rootPath.exists():
                        parentSrcPath = rootPath.glob(val[3:])
                        self._parentSrcPath = list(parentSrcPath)
                        if not checkDirPath(self._parentSrcPath):
                            skip()
                    else:
                        skip()
                else:
                    skip()
            else:
                srcPath = Path(val)
                if not srcPath.exists():
                    skip()
                elif srcPath.is_file():
                    skip()
                else:
                    self._parentSrcPath = [val]
        else:
            raise ValueError(f"Path object, Path glob generator or string is expected from the parentSrcPath parameter from {self.category} from {self.name} configuration.")
    # }}}

    # Validation of versionFind {{{
    @property
    def versionFind(self):
        return self._versionFind
    @versionFind.setter
    def versionFind(self, val):
        if not isinstance(val, Callable) and not isinstance(val, str):
            raise ValueError(f"string or function is expected from the versionFind parameter from {self.category} from {self.name} configuration.")

        self._versionStr = val
        if self._versionFind == "":
            self._versionFind = "unnamedVersion"
    # }}}

    # Validation of filterType {{{
    @property
    def filterType(self):
        return self._filterType
    @filterType.setter
    def filterType(self, val):
        if not isinstance(val, str):
            raise ValueError(f"string is expected from the filterType parameter from {self.category} from {self.name} configuration.")
        if val != "include" and val != "exclude":
            raise ValueError(f"filterType parameter must be either 'include' or 'exclude' from {self.category} from {self.name} configuration.")

        self._filterType = val
    # }}}

    # Validation of filterPattern {{{
    @property
    def filterPattern(self):
        return self._filterPattern
    @filterPattern.setter
    def filterPattern(self, val):
        if not isinstance(val, list) and not isinstance(val, Callable):
            raise ValueError(f"list or function is expected from the filterPattern parameter from {self.category} from {self.name} configuration.")

        if isinstance(val, list):
            for k in val:
                if not isinstance(k, str):
                    raise ValueError(f"a filterPattern list must contain string only from the parameter from {self.category} from {self.name} configuration.")

        self._filterPattern = val
    # }}}

    @classmethod
    def updateTickedList(cls):
        if not cls.profileEnabledList:
            print("No enabled profiles to process.")
            raise ValueError
        for profile in cls.profileEnabledList:
            if profile.ticked:
                cls.profileTickedList.append(profile)


    def updateEnabledList(self):
        for p in type(self).profileList:
            if p.enabled and p not in type(self).profileEnabledList:
                type(self).profileEnabledList.append(self)
                self.enabledIndex = len(type(self).profileEnabledList) - 1
            elif not p.enabled and p in type(self).profileEnabledList:
                type(self).profileEnabledList.remove(self)


    def copyFile( # {{{
            self,
            srcPath: Path,
            topParentSrcPath: Path,
            topParentDstPath: Path,
            ) -> int:
        """Backup file and return backup file count

        Args:
            srcPath: the source to be backuped
            topParentSrcPath: top parent source path
            topParentDstPath: top parent destination path

        Returns: number count of backuped files

        """
        # Recoding
        if not self.name in type(self).fitPatBackupRelStr:
            type(self).fitPatBackupRelStr[self.name] = {}

        if not str(topParentSrcPath) in type(self).fitPatBackupRelStr[self.name]:
            type(self).fitPatBackupRelStr[self.name][str(topParentSrcPath)] = []

        # Compose desionation path
        srcRelTopParentPath    = srcPath.relative_to(topParentSrcPath)
        srcRelTopParentPathStr = str(srcRelTopParentPath)
        type(self).fitPatBackupRelStr[self.name][str(topParentSrcPath)].append(srcRelTopParentPathStr)

        dstPath = Path(topParentDstPath, srcRelTopParentPath)


        # Deicde whether to dry run
        count = 0
        foundFilePrefix = "Backing up" if not DRYRUN else "Found"
        if dstPath.exists():
            if COPYOVERWRITE or (srcPath.stat().st_mtime - dstPath.stat().st_mtime) > 0:
                if not DRYRUN:
                    shutil.copy2(srcPath, dstPath)

                print(f"[white]    {foundFilePrefix} file: [yellow]{srcRelTopParentPathStr}[/yellow][/white]")
                count = count + 1
            else:
                print(f"[gray]    Skip unchanged file: {srcRelTopParentPathStr}[/gray]")
        else:
            if not DRYRUN:
                os.makedirs(dstPath.parent, exist_ok=True)
                shutil.copy2(srcPath, dstPath)

            print(f"[white]    {foundFilePrefix} file: [yellow]{srcRelTopParentPathStr}[/yellow][/white]")
            count = count + 1

        return count # }}}


    def iterCopy( # {{{
            self,
            parentSrcPath: Path,
            parentDstPath: Path,
            filterAllPathStrs: list,
            topParentSrcPath: Optional[Path] = None,
            ) -> int:
        """Iter through a parent source directory to validate and copy each file

        Args:
            parentSrcPath: what parent source path to iterlate
            parentDstPath: what destination path for the parent directory to backup up
            filterAllPathStrs: all the Paths that fit in the filter pattern
            topParentSrcPath: preserved top parent source directory for recursive function call

        Returns: how many file has been backed up

        """
        count = 0

        # Initialization for the first funtion call
        if not topParentSrcPath:
            topParentSrcPath = parentSrcPath

        for srcPath in parentSrcPath.iterdir():
            if srcPath.is_dir():
                if self.recursiveCopy:
                    count += self.iterCopy(
                        parentSrcPath=srcPath,
                        parentDstPath=parentDstPath,
                        filterAllPathStrs=filterAllPathStrs,
                        topParentSrcPath=topParentSrcPath
                    )
                else:
                    pass
            else:
                if isinstance(self.filterPattern, list):
                    if self.filterType == "exclude" and str(srcPath) in filterAllPathStrs:
                        continue
                    elif self.filterType == "include" and str(srcPath) not in filterAllPathStrs:
                        continue
                    else:
                        count += self.copyFile(
                            srcPath,
                            topParentSrcPath,
                            parentDstPath,
                        )
                else:
                    if self.filterType == "exclude" and self.filterPattern(srcPath):
                        continue
                    elif self.filterType == "include" and not self.filterPattern(srcPath):
                        continue
                    else:
                        count += self.copyFile(
                            srcPath=srcPath,
                            topParentSrcPath=topParentSrcPath,
                            topParentDstPath=parentDstPath,
                        )

        return count # }}}


    def iterSync( # {{{
            self,
            srcRelTopParentPathList: list[str],
            parentDstPath: Path,
            topParentSrcPath: Path,
            topParentDstPath: Optional[Path] = None
            ):
        """Iterate throught destination directory to check whether a file exist in current source directory. If not, delete that file

        Args:
            srcRelTopParentPathList: List contains path string relactive to the source file
            parentDstPath: What destination directory to iterate through
            topParentSrcPath: Top parent source directory where all source file is relative to
            topParentDstPath: Preserved top parent destionation directory for recursive function call
        """
        # Abort when parent destination directory doesn't exist
        if not parentDstPath.exists():
            return

        # Initialization for the first funtion call
        if not topParentDstPath:
            topParentDstPath = parentDstPath

        if not self.name in type(self).syncFilesToDelete:
            type(self).syncFilesToDelete[self.name] = {}
        if not self.versionStr in type(self).syncFilesToDelete[self.name]:
            type(self).syncFilesToDelete[self.name][self.versionStr] = []

        for dstPath in parentDstPath.iterdir():
            if dstPath.is_dir():
                if not any(dstPath.iterdir()):
                    type(self).syncFilesToDelete[self.name][self.versionStr].append(str(dstPath) + os.path.sep)
                else:
                    self.iterSync(srcRelTopParentPathList, dstPath, topParentSrcPath, topParentDstPath)
            else:
                dstRelTopParentPathStr = str(dstPath.relative_to(topParentDstPath))

                if not dstRelTopParentPathStr in srcRelTopParentPathList:
                    type(self).syncFilesToDelete[self.name][self.versionStr].append(str(dstPath)) # }}}


    def backup(self): # {{{
        global SILENTMODE
        # Alter global silent report for current backup session
        SILENTMODE = self.silentReport

        print(f"[white]Checking up [green bold]{self.name} {self.category}[/green bold][/white]...")
        for parentSrcPath in self.parentSrcPaths:
            # Get version string
            if isinstance(self.versionFind, Callable):
                try:
                    self.versionStr = self.versionFind(parentSrcPath)
                except Exception as e:
                    print(e)
                    print('[red]  Version string use "unnamedVersion" instead\n[/red]')
                    self.versionStr = "unnamedVersion"

            print(f"[white]  Checking up [green bold]{self.name} {self.versionStr}[/green bold] files inside folder: [yellow]{parentSrcPath}[/yellow][/white]")

            # Get parent destination path
            parentSrcRelAnchorPath = parentSrcPath.relative_to(parentSrcPath.anchor)
            parentDstPath = Path(
                    DESTPATH, # type: ignore
                    self.name,
                    self.category,
                    self.versionStr,
                    parentSrcPath.anchor[:1],
                    parentSrcRelAnchorPath
                )


            # Glob all filter pattern paths
            filterAllPaths = []
            if isinstance(self.filterPattern, list):
                for pattern in self.filterPattern:
                    if not pattern.startswith("\\") and pattern.startswith("/"):
                        pattern = "/" + pattern

                    filterPaths = list(parentSrcPath.glob(pattern))
                    if len(filterPaths) == 0:
                        continue
                    filterAllPaths.extend(filterPaths)

            filterAllPathStrs = list(map(lambda p: str(p), filterAllPaths))


            # Copy files from source to destionation
            currentParentSrcCount = self.iterCopy(
                parentSrcPath=parentSrcPath,
                parentDstPath=parentDstPath,
                filterAllPathStrs=filterAllPathStrs,
                )
            self.softwareBackupCount = self.softwareBackupCount + currentParentSrcCount


            # Preserve files doesn't exist in destination directory for the current parent source directory
            if COPYSYNC:
                srcRelTopParentPathList = type(self).fitPatBackupRelStr[self.name][str(parentSrcPath)]
                self.iterSync(srcRelTopParentPathList, parentDstPath, parentSrcPath)

            # Report count for the current parent source directory
            if not DRYRUN:
                print(f"  [white]Backed up [purple bold]{currentParentSrcCount}[/purple bold] files")
            else:
                print(f"  [white]Found [purple bold]{currentParentSrcCount}[/purple bold] files")

        if not DRYRUN:
            print(f"[white]Backed up [purple bold]{self.softwareBackupCount}[/purple bold] [green bold]{self.name} {self.versionStr}[/green bold] files\n[/white]")
        else:
            print(f"[white]Found [purple bold]{self.softwareBackupCount}[/purple bold] [green bold]{self.name} {self.versionStr}[/green bold] files\n[/white]")

        type(self).totalBackupCount = type(self).totalBackupCount + self.softwareBackupCount
        # }}}

    @classmethod
    def reportBackupCount(cls):
        # Report the total count if the current enabled profile is the last one in the list
        profileTickedNames = list(map(lambda i: str(i.name), cls.profileTickedList))
        if not DRYRUN:
            print(f"[white]Backed up [purple bold]{cls.totalBackupCount}[/purple bold] files from [green]{profileTickedNames}[/green].\n[/white]")
        else:
            print(f"[white]Found [purple bold]{cls.totalBackupCount}[/purple bold] files from [green]{profileTickedNames}[/green].\n[/white]")

import shutil
import util
import config

import os
from types import GeneratorType
from typing import Callable, Optional, Tuple, Iterator
from pathlib import Path


userName    = os.getlogin()
appDataPath = Path("C:/Users/{}/AppData".format(userName))
homePath    = Path("C:/Users/{}".format(userName))
print = util.print



class Profile(): # {{{
    totalBackupCount = 0
    totalBackupSize = 0
    foundFileMessage = "Backing up" if not config.DRYRUN else "Found"
    profileDict: dict = {  } # type: ignore
    def __init__(self, profileName, categories, enabled):
        # Default value
        self.ticked = True
        self.backupCount = 0
        self.backupSize  = 0

        self.profileName = profileName
        self.categories  = categories
        self.enabled     = enabled
        if self.profileName not in type(self).profileDict:
            type(self).profileDict[self.profileName] = self

    def __str__(self):
        return f"{self.profileName}"

    def __repr__(self):
        return f"{type(self).__name__}(profileName={self.profileName})"

    # Validation of profile name {{{
    @property
    def profileName(self):
        return self._profileName
    @profileName.setter
    def profileName(self, val):
        if not isinstance(val, str):
            raise ValueError("string value is expected for Profile name.")
        self._profileName = val
    # }}}


    # Validation of enabled {{{
    @property
    def enabled(self):
        return self._enabled
    @enabled.setter
    def enabled(self, val):
        if not isinstance(val, bool):
            raise ValueError("bool value is expected from the enabled parameter.")
        self._enabled = val
    # }}}


    # Validation of categories {{{
    @property
    def categories(self) -> list:
        return self._categories
    @categories.setter
    def categories(self, val):
        if not isinstance(val, list):
            raise ValueError(f"list is expceted for categories under Profile {self.profileName}.")
        for category in val:
            if not isinstance(category, Category):
                raise ValueError(f"{type(category)} value is inside the list of categories which must be consist of Catogory class only.")

        self._categories = val
    # }}}

    @classmethod
    def updateFoundFileMessage(cls):
        cls.foundFileMessage = "Backing up" if not config.DRYRUN else "Found"
# }}}


class Category(Profile): # {{{
    syncFilesToDelete:    dict[str, dict[Path, list[Path]]] = {}
    relPathsTopParentSrc: dict[str, dict[Path, list[str]]] = {}

    # TODO: type check https://stackoverflow.com/questions/2489669/how-do-python-functions-handle-the-types-of-parameters-that-you-pass-in
    def __init__(
        self,
        profileName: str,
        categoryName: str,
        versionFind: str | Callable,
        enabled: bool,
        recursiveCopy: bool,
        silentReport: bool,
        parentSrcPaths: str | Path | list[Path] | Iterator[Path] | map,
        filterType: str,
        filterPattern,
    ):
        # Default value
        self.versionStr = ""
        self.backupCount = 0
        self.backupSize = 0

        self.profileName  = profileName
        self.categoryName = categoryName
        self.enabled      = enabled
        if not self.enabled:
            return

        self.recursiveCopy  = recursiveCopy
        self.silentReport   = silentReport
        self.parentSrcPaths = parentSrcPaths
        self.versionFind    = versionFind
        self.filterType     = filterType
        self.filterPattern  = filterPattern

    def __str__(self):
        return f"{self.profileName} {self.categoryName}"

    def __repr__(self):
        return f"{type(self).__name__}(profileName={self.profileName}, categoryName={self.categoryName})"

    # Validation of categoryName {{{
    @property
    def categoryName(self):
        return self._categoryName
    @categoryName.setter
    def categoryName(self, val):
        if not isinstance(val, str):
            raise ValueError(f"string value is expected from the categoryName parameter from {self.categoryName} from {self.profileName} configuration.")
        self._categoryName = val
    # }}}

    # Validation of recursiveCopy {{{
    @property
    def recursiveCopy(self):
        return self._recursiveCopy
    @recursiveCopy.setter
    def recursiveCopy(self, val):
        if not isinstance(val, bool):
            raise ValueError(f"bool value is expected from the recursiveCopy parameter from {self.categoryName} from {self.profileName} configuration.")
        self._recursiveCopy = val
    # }}}

    # Validation of silentReport {{{
    @property
    def silentReport(self):
        return self._silentReport
    @silentReport.setter
    def silentReport(self, val):
        if not isinstance(val, bool):
            raise ValueError(f"bool is expected from the silentReport parameter from {self.categoryName} from {self.profileName} configuration.")
        self._silentReport = val
    # }}}

    # Validation of parentSrcPaths {{{
    @property
    def parentSrcPaths(self) -> list[Path]:
        return self._parentSrcPaths # type: ignore
    @parentSrcPaths.setter
    def parentSrcPaths(self, val):
        # Validate path pattern
        def skip(valParentSrcPaths):
            print(f"[gray]Skipped unfound parent source paths for {valParentSrcPaths} from {self.categoryName} from {self.profileName} configuration.[/gray]")
            self.enabled = False

        def checkDirPath(paths: list[ Path ]):
            for p in paths:
                if p.is_dir():
                    return True
            return False


        if isinstance(val, GeneratorType) or isinstance(val, map):
            self._parentSrcPaths = list(val)
            if not checkDirPath(self._parentSrcPaths):
                skip(self._parentSrcPaths)
        elif isinstance(val, Path):
            srcPath = val
            if not srcPath.exists():
                skip(srcPath)
            elif srcPath.is_file():
                skip(srcPath)
            else:
                self._parentSrcPaths = [val]
        elif isinstance(val, str):
            if "*" in val:
                # Adding "/" suffix to the end if "*" is already the last character to make sure the glob result always return a directory path
                if val[-1:] == "*":
                    val = val + "/"

                # Turn string literal into a path glob generator
                if val[1:2] == ":":
                    rootPath = Path(val[0:3])
                    if rootPath.exists():
                        parentSrcPaths = rootPath.glob(val[3:])
                        self._parentSrcPaths = list(parentSrcPaths)
                        if not checkDirPath(self._parentSrcPaths):
                            skip(self._parentSrcPaths)
                    else:
                        skip(val)
                else:
                    skip(val)
            else:
                srcPath = Path(val)
                if not srcPath.exists():
                    skip(srcPath)
                elif srcPath.is_file():
                    skip(srcPath)
                else:
                    self._parentSrcPaths = [srcPath]
        else:
            raise ValueError(f"Path object, Path glob generator or string is expected from the parentSrcPath parameter from {self.categoryName} from {self.profileName} configuration.")
    # }}}

    # Validation of versionFind {{{
    @property
    def versionFind(self):
        return self._versionFind
    @versionFind.setter
    def versionFind(self, val):
        if not isinstance(val, Callable) and not isinstance(val, str):
            raise ValueError(f"string or function is expected from the versionFind parameter from {self.categoryName} from {self.profileName} configuration.")

        self._versionFind = val
        if self._versionFind == "":
            self._versionFind = "Generic"
    # }}}

    # Validation of filterType {{{
    @property
    def filterType(self):
        return self._filterType
    @filterType.setter
    def filterType(self, val):
        if not isinstance(val, str):
            raise ValueError(f"string is expected from the filterType parameter from {self.categoryName} from {self.profileName} configuration.")
        if val != "include" and val != "exclude":
            raise ValueError(f"filterType parameter must be either 'include' or 'exclude' from {self.categoryName} from {self.profileName} configuration.")

        self._filterType = val
    # }}}

    # Validation of filterPattern {{{
    @property
    def filterPattern(self):
        return self._filterPattern
    @filterPattern.setter
    def filterPattern(self, val):
        if not isinstance(val, list) and not isinstance(val, Callable):
            raise ValueError(f"list or function is expected from the filterPattern parameter from {self.categoryName} from {self.profileName} configuration.")

        if isinstance(val, list):
            for k in val:
                if not isinstance(k, str):
                    raise ValueError(f"a filterPattern list must contain string only from the parameter from {self.categoryName} from {self.profileName} configuration.")

        self._filterPattern = val
    # }}}


    def copyFile( # {{{
            self,
            srcPath: Path,
            topParentSrcPath: Path,
            topParentDstPath: Path,
            ) -> Tuple[int, int]:
        """Backup file and return backup file count

        Args:
            srcPath: the source to be backuped
            topParentSrcPath: top parent source path
            topParentDstPath: top parent destination path

        Returns: number count and size count of backuped files

        """
        # Recording
        if not self.profileName in type(self).relPathsTopParentSrc:
            type(self).relPathsTopParentSrc[self.profileName] = {}

        if not topParentSrcPath in type(self).relPathsTopParentSrc[self.profileName]:
            type(self).relPathsTopParentSrc[self.profileName][topParentSrcPath] = []
        relPathTopParentSrcList = type(self).relPathsTopParentSrc[self.profileName][topParentSrcPath]


        # Compose destination path
        relPathTopParentSrc    = srcPath.relative_to(topParentSrcPath)
        relPathTopParentSrcStr = str(relPathTopParentSrc)
        relPathTopParentSrcList.append(relPathTopParentSrcStr)

        dstPath = Path(topParentDstPath, relPathTopParentSrc)


        # Decide whether to dry run
        count = 0
        size = 0
        if dstPath.exists():
            if config.COPYOVERWRITE or (srcPath.stat().st_mtime - dstPath.stat().st_mtime) > 0:
                if not config.DRYRUN:
                    try:
                        shutil.copy2(srcPath, dstPath)
                        count += 1
                        size += srcPath.stat().st_size
                        print(f"[white]    {type(self).foundFileMessage} file: [yellow]{relPathTopParentSrcStr}[/yellow][/white][blue]({util.humanReadableSize(size)})[/blue]")
                    except PermissionError:
                        print(f"[red]    Skip file due to permission error: [yellow]{relPathTopParentSrcStr}[/yellow][/red]")
            else:
                print(f"[gray]    Skip unchanged file: {relPathTopParentSrcStr}[/gray]")
        else:
            if not config.DRYRUN:
                os.makedirs(dstPath.parent, exist_ok=True)
                shutil.copy2(srcPath, dstPath)

            print(f"[white]    {type(self).foundFileMessage} file: [yellow]{relPathTopParentSrcStr}[/yellow][/white]")
            count += count
            size += srcPath.stat().st_size

        return count, size # }}}


    def iterCopy( # {{{
        self,
        parentSrcPath: Path,
        parentDstPath: Path,
        filterAllPathStrs: list,
        topParentSrcPath: Optional[Path] = None,
    ) -> Tuple[int, int]:
        """Iter through a parent source directory to validate and copy each file

        Args:
            parentSrcPath: what parent source path to iterlate
            parentDstPath: what destination path for the parent directory to backup up
            filterAllPathStrs: all the Paths that fit in the filter pattern
            topParentSrcPath: preserved top parent source directory for recursive function call

        Returns: how many file has been backed up and the accumulated file size

        """
        countAccumulated = 0
        sizeAccumulated = 0

        # Initialization for the first function call
        if not topParentSrcPath:
            topParentSrcPath = parentSrcPath

        for srcPath in parentSrcPath.iterdir():
            if srcPath.is_dir():
                if self.recursiveCopy:
                    count, size = self.iterCopy(
                        parentSrcPath=srcPath,
                        parentDstPath=parentDstPath,
                        filterAllPathStrs=filterAllPathStrs,
                        topParentSrcPath=topParentSrcPath
                    )
                    countAccumulated += count
                    sizeAccumulated += size

                else:
                    pass
            else:
                if isinstance(self.filterPattern, list):
                    if self.filterType == "exclude" and str(srcPath) in filterAllPathStrs:
                        continue
                    elif self.filterType == "include" and str(srcPath) not in filterAllPathStrs:
                        continue
                    else:
                        count, size = self.copyFile(
                            srcPath,
                            topParentSrcPath,
                            parentDstPath,
                        )
                        countAccumulated += count
                        sizeAccumulated += size
                else:
                    if self.filterType == "exclude" and self.filterPattern(srcPath):
                        continue
                    elif self.filterType == "include" and not self.filterPattern(srcPath):
                        continue
                    else:
                        count, size = self.copyFile(
                            srcPath=srcPath,
                            topParentSrcPath=topParentSrcPath,
                            topParentDstPath=parentDstPath,
                        )
                        countAccumulated += count
                        sizeAccumulated += size

        return countAccumulated, sizeAccumulated # }}}


    def iterSync( # {{{
        self,
        relPathsTopParentSrc: list[str],
        parentDstPath: Path,
        topParentSrcPath: Path,
        topParentDstPath: Optional[Path] = None
    ):
        """Iterate throught destination directory to check whether a file exist in local source directory. If not, delete that file

        Args:
            relPathsTopParentSrc: List contains path string relactive to the parent source path
            parentDstPath: What destination directory to iterate through
            topParentSrcPath: Top parent source directory where all source file is relative to
            topParentDstPath: Preserved top parent destionation directory for recursive function call
        """
        # Abort when parent destination directory doesn't exist
        if not parentDstPath.exists():
            return

        # Initialization for the first function call
        if not topParentDstPath:
            topParentDstPath = parentDstPath

        if not self.profileName in type(self).syncFilesToDelete:
            type(self).syncFilesToDelete[self.profileName] = {}
        if not topParentSrcPath in type(self).syncFilesToDelete[self.profileName]:
            type(self).syncFilesToDelete[self.profileName][topParentSrcPath] = []
        syncFilesToDeleteRelCurrentParentDst = type(self).syncFilesToDelete[self.profileName][topParentSrcPath]

        for dstPath in parentDstPath.iterdir():
            if dstPath.is_dir():
                if not any(dstPath.iterdir()): # Remove empty directories
                    syncFilesToDeleteRelCurrentParentDst.append(dstPath)
                else:
                    self.iterSync(
                        relPathsTopParentSrc=relPathsTopParentSrc,
                        parentDstPath=dstPath,
                        topParentSrcPath=topParentSrcPath,
                        topParentDstPath=topParentDstPath
                    )
            else:
                relPathTopParentDst = str(dstPath.relative_to(topParentDstPath))

                if not relPathTopParentDst in relPathsTopParentSrc:
                    syncFilesToDeleteRelCurrentParentDst.append(dstPath) # }}}


    def backup(self): # {{{
        # Alter global silent report for current backup session
        config.SILENTMODE = self.silentReport

        print(f"  {util.getTimeStamp()}[white]Checking up [green bold]{self.profileName} {self.categoryName}[/green bold][/white]...")
        for parentSrcPath in self.parentSrcPaths:
            # Get version string
            if isinstance(self.versionFind, Callable):
                try:
                    self.versionStr = self.versionFind(parentSrcPath)
                except Exception as e:
                    print(e)
                    print('[red]  Version string use "Generic" instead\n[/red]')
                    self.versionStr = "Generic"

            print(f"    {util.getTimeStamp()}[white]Checking up files for [green bold]{self.profileName} {self.categoryName} {self.versionStr}[/green bold] inside folder: [yellow]{parentSrcPath}[/yellow][/white]")

            # Get parent destination path
            parentSrcRelAnchorPath = parentSrcPath.relative_to(parentSrcPath.anchor)
            parentDstPath = Path(
                    config.DESTPATH, # type: ignore
                    self.profileName,
                    self.categoryName,
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


            # Copy files from source to destination
            currentParentSrcCount, currentParentSrcSize = self.iterCopy(
                parentSrcPath=parentSrcPath,
                parentDstPath=parentDstPath,
                filterAllPathStrs=filterAllPathStrs,
                )
            self.backupCount += currentParentSrcCount
            self.backupSize  += currentParentSrcSize


            # Get path strings that relative to the current source parent path
            relPathsTopParentSrc = type(self).relPathsTopParentSrc[self.profileName][parentSrcPath]
            # Mark down files that doesn't exist in destination directory for the current parent source directory
            if config.COPYSYNC:
                self.iterSync(
                    relPathsTopParentSrc=relPathsTopParentSrc,
                    parentDstPath=parentDstPath,
                    topParentSrcPath=parentSrcPath
            )

            # Report count for the current parent source directory
            print(f"    {util.getTimeStamp()}[white]{type(self).foundFileMessage} [purple bold]{currentParentSrcCount}[/purple bold] files of [blue bold]{util.humanReadableSize(currentParentSrcSize)}[/blue bold] for [green bold]{self.profileName} {self.categoryName} {self.versionStr}[/green bold] files inside folder: [yellow]{parentSrcPath}[/yellow][/white]")

        # Report count for the current category
        print(f"  {util.getTimeStamp()}[white]{type(self).foundFileMessage} [purple bold]{self.backupCount}[/purple bold] files of [blue bold]{util.humanReadableSize(self.backupSize)}[/blue bold] for [green bold]{self.profileName} {self.categoryName}[/green bold].[/white]")

        Profile.profileDict[self.profileName].backupCount += self.backupCount
        Profile.profileDict[self.profileName].backupSize  += self.backupSize
        Profile.totalBackupCount += self.backupCount
        Profile.totalBackupSize  += self.backupSize
        # }}}
# }}}

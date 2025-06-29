import shutil
import util
import config

import os
import winreg
import re
from types import GeneratorType
from typing import Callable, Optional, Tuple, Iterator, cast
from pathlib import Path

print = util.print



class Profile(): # {{{
    """
    A backup profile that manages file and registry backup configurations.

    This class represents a backup profile that can contain multiple file and registry categories
    to be backed up. It tracks backup statistics and manages the backup process for all its categories.

    Class Attributes:
        totalBackupCount (int): Total number of files and registry entries backed up across all profiles.
        totalFileBackupSize (int): Total size in bytes of all files backed up across all profiles.
        foundFileMessage (str): Dynamic message that changes between "Found" (dry run) and "Backed up" (actual backup).
        profileDict (dict[str, Profile]): Global dictionary mapping profile names to their Profile instances.

    Instance Attributes:
        profileName (str): Unique name identifying this profile.
        categories (list): List of FileCategory and RegCategory instances for this profile.
        enabled (bool): Whether this profile is active for backup operations.
        ticked (bool): Whether this profile is selected in the UI.
        backupCount (int): Number of files/registries backed up in this profile.
        backupSize (int): Total size in bytes of files backed up in this profile.
    """
    totalBackupCount = 0
    totalFileBackupSize = 0
    foundFileMessage = "Backed up" if not config.DRYRUN else "Found"
    profileDict: dict = {  } # type: ignore

    def __init__(self, profileName, categories, enabled):
        """
        Initialize a new Profile instance.

        Creates a new backup profile with specified name, categories, and enabled status.
        The profile is automatically registered in the global profileDict.

        Args:
            profileName (str): Unique name identifying this profile.
            categories (list): List of dictionaries defining file or registry categories.
                Each dictionary must contain a "type" key with value "file" or "registry"
                and other parameters specific to the category type.
            enabled (bool): Whether this profile should be active for backup operations.
        """
        # Default value
        self.ticked = True
        self.backupCount = 0
        self.backupSize  = 0

        self.profileName = profileName
        self.categories  = categories
        self.enabled     = enabled
        if not self.enabled:
            return
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

        self._categories = []
        for categoryArgs in val:
            if categoryArgs["type"] == "file":
                del categoryArgs["type"]
                self._categories.append(FileCategory(profileName=self.profileName, **categoryArgs))
            elif categoryArgs["type"] == "registry":
                del categoryArgs["type"]
                self._categories.append(RegCategory(profileName=self.profileName, **categoryArgs))
            else:
                # raise error for invalid category type
                raise ValueError(f"Invalid category type {categoryArgs["type"]} for category {categoryArgs["categoryName"]} under Profile {self.profileName}.")
    # }}}

    @classmethod
    def updateFoundFileMessage(cls):
        """
        Updates the foundFileMessage class attribute based on the current dry run mode.

        Sets the message to "Backing up" during actual backup operations or "Found" during dry runs.
        This affects all status messages displayed during the backup process.
        """
        cls.foundFileMessage = "Backing up" if not config.DRYRUN else "Found"
# }}}


class FileCategory(Profile): # {{{
    """
    A category for backing up files from specified source paths.

    This class handles the backup of files from one or more source directories to corresponding
    destination directories, applying filtering rules and tracking backup statistics.

    Class Attributes:
        syncFilesToDelete (dict): Tracks files in destination that don't exist in source for sync operations.
        relPathsTopParentSrc (dict): Maps profile names to source paths and their relative paths.

    Instance Attributes:
        profileName (str): Name of the parent profile this category belongs to.
        categoryName (str): Name identifying this specific category.
        enabled (bool): Whether this category is active for backup.
        recursiveCopy (bool): Whether to recursively copy subdirectories.
        silentReport (bool): Whether to suppress detailed reporting.
        parentSrcPaths (list[Path]): List of source directory paths to back up.
        versionFind (str|Callable): Version string or function to determine version.
        filterType (str): Either "include" or "exclude" to define filter behavior.
        filterPattern (list|Callable): Patterns or function to filter files for backup.
        backupCount (int): Number of files backed up in this category.
        backupSize (int): Total size in bytes of files backed up in this category.
    """
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
        filterPattern: str | Callable,
    ):
        self.backupCount = 0
        self.backupSize  = 0

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
            raise ValueError(f"string value is expected from the categoryName parameter for category {self.categoryName} under profile {self.profileName}.")
        self._categoryName = val
    # }}}

    # Validation of recursiveCopy {{{
    @property
    def recursiveCopy(self):
        return self._recursiveCopy
    @recursiveCopy.setter
    def recursiveCopy(self, val):
        if not isinstance(val, bool):
            raise ValueError(f"bool value is expected from the recursiveCopy parameter for category {self.categoryName} under configuration {self.profileName}.")
        self._recursiveCopy = val
    # }}}

    # Validation of silentReport {{{
    @property
    def silentReport(self):
        return self._silentReport
    @silentReport.setter
    def silentReport(self, val):
        if not isinstance(val, bool):
            raise ValueError(f"bool is expected from the silentReport parameter for category {self.categoryName} under profile {self.profileName}.")
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
            print(f"[gray]Skipped unfound parent source paths for {valParentSrcPaths} for category {self.categoryName} under profile {self.profileName}.[/gray]", skipChk=False)
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
            raise ValueError(f"Path object, Path glob generator or string is expected from the parentSrcPath parameter for category {self.categoryName} under profile {self.profileName}.")
    # }}}

    # Validation of versionFind {{{
    @property
    def versionFind(self):
        return self._versionFind
    @versionFind.setter
    def versionFind(self, val):
        if not isinstance(val, Callable) and not isinstance(val, str):
            raise ValueError(f"string or function is expected from the versionFind parameter for category {self.categoryName} under profile {self.profileName}.")

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
            raise ValueError(f"string is expected from the filterType parameter for category {self.categoryName} under profile {self.profileName}.")
        if val != "include" and val != "exclude":
            raise ValueError(f"filterType parameter must be either 'include' or 'exclude' for category {self.categoryName} under profile {self.profileName}.")

        self._filterType = val
    # }}}

    # Validation of filterPattern {{{
    @property
    def filterPattern(self):
        return self._filterPattern
    @filterPattern.setter
    def filterPattern(self, val):
        if not isinstance(val, list) and not isinstance(val, Callable):
            raise ValueError(f"list or function is expected as the filterPattern parameter for category {self.categoryName} under profile {self.profileName}.")

        if isinstance(val, list):
            for k in val:
                if not isinstance(k, str):
                    raise ValueError(f"a filterPattern list must only contain string as the parameter for category {self.categoryName} under profile {self.profileName}.")

        self._filterPattern = val
    # }}}


    def copyFile( # {{{
            self,
            srcPath: Path,
            parentSrcPath: Path,
            parentDstPath: Path,
            bufferOutput: list[str],
            ) -> Tuple[int, int, list[str]]:
        """
        Copies a single file from source to destination and updates backup statistics.

        This method handles the actual file copying operation, maintaining file metadata
        (like modification times), creating destination directories as needed, and
        tracking backup statistics. It respects dry run mode and only copies files
        that are new or modified based on configuration settings.

        Args:
            srcPath (Path): Source file path to be backed up.
            parentSrcPath (Path): Parent source directory path for relative path calculation.
            parentDstPath (Path): Parent destination directory path where file will be copied.
            bufferOutput (list[str]): List to collect output messages during operation.

        Returns:
            Tuple[int, int, list[str]]: A tuple containing:
                - count: Number of files backed up (0 or 1)
                - size: Size in bytes of backed up file (0 if not backed up)
                - bufferOutput: Updated list of output messages
        """
        relPathTopParentSrcList = type(self).relPathsTopParentSrc[self.profileName][parentSrcPath]


        # Compose destination path
        relPathTopParentSrc    = srcPath.relative_to(parentSrcPath)
        relPathTopParentSrcStr = str(relPathTopParentSrc)
        relPathTopParentSrcList.append(relPathTopParentSrcStr)

        dstPath = Path(parentDstPath, relPathTopParentSrc)


        # Decide whether to dry run
        count = 0
        size = 0
        if dstPath.exists():
            if config.COPYOVERWRITE or srcPath.stat().st_mtime != dstPath.stat().st_mtime:
                if not config.DRYRUN:
                    try:
                        shutil.copy2(srcPath, dstPath)
                        count += 1
                        size += srcPath.stat().st_size
                        bufferOutput.append(f"[white]    {Profile.foundFileMessage} file: [yellow]{relPathTopParentSrcStr}[/yellow][/white][blue]({util.humanReadableSize(size)})[/blue]")
                    except PermissionError:
                        bufferOutput.append(f"[red]    Skip file due to permission error: [yellow]{relPathTopParentSrcStr}[/yellow][/red]")
                else:
                    count += 1
                    size += srcPath.stat().st_size
                    bufferOutput.append(f"[white]    {Profile.foundFileMessage} file: [yellow]{relPathTopParentSrcStr}[/yellow][/white][blue]({util.humanReadableSize(size)})[/blue]")
            else:
                bufferOutput.append(f"[gray]    Skip unchanged file: {relPathTopParentSrcStr}[/gray]")
        else:
            if not config.DRYRUN:
                os.makedirs(dstPath.parent, exist_ok=True)
                shutil.copy2(srcPath, dstPath)

            bufferOutput.append(f"[white]    {Profile.foundFileMessage} file: [yellow]{relPathTopParentSrcStr}[/yellow][/white]")
            count += 1
            size += srcPath.stat().st_size

        return count, size, bufferOutput # }}}


    def iterCopy( # {{{
        self,
        parentSrcPath: Path,
        parentDstPath: Path,
        filterAllPaths: list[Path],
        bufferOutput: list[str],
        topParentSrcPath: Optional[Path] = None,
    ) -> Tuple[int, int, list[str]]:
        """
        Recursively iterates through a source directory to copy files based on filter rules.

        This method walks through a source directory, applying filter patterns to determine
        which files and subdirectories to back up. It handles both recursive and non-recursive
        copying modes, and supports both include and exclude filtering using either pattern
        lists or callable filters.

        Args:
            parentSrcPath (Path): Source directory path to iterate through.
            parentDstPath (Path): Destination directory path for backup files.
            filterAllPaths (list[Path]): List of paths that match the filter patterns.
            bufferOutput (list[str]): List to collect output messages during operation.
            topParentSrcPath (Optional[Path]): Top-level source directory for recursive calls,
                defaults to parentSrcPath on first call.

        Returns:
            Tuple[int, int, list[str]]: A tuple containing:
                - countAccumulated: Total number of files backed up
                - sizeAccumulated: Total size in bytes of backed up files
                - bufferOutput: Updated list of output messages

        Note:
            The method respects the recursiveCopy setting and applies filterType/filterPattern
            rules to determine which files and directories to process.
        """
        countAccumulated = 0
        sizeAccumulated = 0

        # Initialization for the first function call
        if not topParentSrcPath:
            topParentSrcPath = parentSrcPath
        # Recording
        if self.profileName not in type(self).relPathsTopParentSrc:
            type(self).relPathsTopParentSrc[self.profileName] = {}
        if parentSrcPath not in type(self).relPathsTopParentSrc[self.profileName]:
            type(self).relPathsTopParentSrc[self.profileName][parentSrcPath] = []

        try:
            for srcPath in parentSrcPath.iterdir():
                if srcPath.is_dir():
                    if self.recursiveCopy:
                        # Check whether the directory is in the filter pattern list
                        srcPathInsideFilterChk = False
                        if isinstance(self.filterPattern, list):
                            if self.filterType == "exclude":
                                for p in filterAllPaths:
                                    if p.is_dir() and srcPath == p:
                                        srcPathInsideFilterChk = True
                                        break
                            elif self.filterType == "include":
                                for p in filterAllPaths:
                                    if p.is_dir() and srcPath != p:
                                        srcPathInsideFilterChk = True
                                        break
                        else:
                            if self.filterType == "exclude" and self.filterPattern(srcPath):
                                srcPathInsideFilterChk = True
                                break
                            elif self.filterType == "include" and not self.filterPattern(srcPath):
                                srcPathInsideFilterChk = True
                                break

                        if srcPathInsideFilterChk:
                            continue

                        count, size, bufferOutput = self.iterCopy(
                            parentSrcPath=srcPath,
                            parentDstPath=parentDstPath,
                            filterAllPaths=filterAllPaths,
                            bufferOutput=bufferOutput,
                            topParentSrcPath=topParentSrcPath
                        )
                        countAccumulated += count
                        sizeAccumulated += size

                    else:
                        continue
                else:
                    if isinstance(self.filterPattern, list):
                        if self.filterType == "exclude" and srcPath in filterAllPaths:
                            continue
                        elif self.filterType == "include" and srcPath not in filterAllPaths:
                            continue
                        else:
                            count, size, bufferOutput = self.copyFile(
                                srcPath=srcPath,
                                parentSrcPath=topParentSrcPath,
                                parentDstPath=parentDstPath,
                                bufferOutput=bufferOutput,
                            )
                            countAccumulated += count
                            sizeAccumulated += size
                    else:
                        if self.filterType == "exclude" and self.filterPattern(srcPath):
                            continue
                        elif self.filterType == "include" and not self.filterPattern(srcPath):
                            continue
                        else:
                            count, size, bufferOutput = self.copyFile(
                                srcPath=srcPath,
                                parentSrcPath=topParentSrcPath,
                                parentDstPath=parentDstPath,
                                bufferOutput=bufferOutput
                            )
                            countAccumulated += count
                            sizeAccumulated += size
        except PermissionError:
            print(f"[red]    Skip directory due to permission error: [yellow]{parentSrcPath}[/yellow][/red]")

        return countAccumulated, sizeAccumulated, bufferOutput # }}}


    def iterSync( # {{{
        self,
        relPathsTopParentSrc: list[str],
        parentDstPath: Path,
        topParentSrcPath: Path,
        topParentDstPath: Optional[Path] = None
    ):
        """
        Identifies files in destination that don't exist in source for synchronization.

        This method recursively walks through the destination directory, comparing files
        against the source directory listing. It marks files and empty directories that
        exist in the destination but not in the source for potential deletion during
        sync operations.

        Args:
            relPathsTopParentSrc (list[str]): List of relative paths that exist in the
                source directory, used for comparison.
            parentDstPath (Path): Current destination directory being checked.
            topParentSrcPath (Path): Top-level source directory path, used for tracking
                files to delete in the syncFilesToDelete dictionary.
            topParentDstPath (Optional[Path]): Top-level destination directory for recursive
                calls, defaults to parentDstPath on first call.

        Note:
            - Files found in destination but not in relPathsTopParentSrc are added to
              syncFilesToDelete for later removal.
            - Empty directories in destination are also marked for removal.
            - This method only identifies files to delete; actual deletion happens elsewhere.
        """
        # Abort when parent destination directory doesn't exist
        if not parentDstPath.exists():
            return

        # Initialization for the first function call
        if not topParentDstPath:
            topParentDstPath = parentDstPath

        if self.profileName not in type(self).syncFilesToDelete:
            type(self).syncFilesToDelete[self.profileName] = {}
        if topParentSrcPath not in type(self).syncFilesToDelete[self.profileName]:
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

                if relPathTopParentDst not in relPathsTopParentSrc:
                    syncFilesToDeleteRelCurrentParentDst.append(dstPath) # }}}


    def backup(self, bufferOutput:list[str]) -> list[str]: # {{{
        """
        Performs the backup operation for this registry category.

        Exports Windows registry keys and values to .reg files based on the configured
        paths and filter patterns. Creates the necessary directory structure and
        handles both recursive and non-recursive registry key backups.

        Args:
            bufferOutput (list[str]): List to collect output messages during the backup operation.

        Returns:
            list[str]: Updated list of output messages with backup results and statistics.

        Note:
            - This method respects the global DRYRUN setting.
            - It updates both category-level and profile-level backup statistics.
            - Registry entries are exported to .reg files with proper formatting.
            - For each registry path, it creates a corresponding destination file with
              appropriate directory structure.
            - The method handles registry access errors gracefully.
        """
        """
        Performs the backup operation for this file category.

        This is the main method that orchestrates the entire backup process for a file category.
        It processes each source path, determines version information, creates destination paths,
        applies filters, copies files, and handles synchronization if enabled. It also tracks
        and reports backup statistics.

        Args:
            bufferOutput (list[str]): List to collect output messages during the backup operation.

        Returns:
            list[str]: Updated list of output messages with backup results and statistics.

        Note:
            - This method respects the global DRYRUN and COPYSYNC settings.
            - It updates both category-level and profile-level backup statistics.
            - For each source path, it creates a corresponding destination path with
              appropriate directory structure.
            - In sync mode, it identifies files in destination that don't exist in source.
        """
        # Alter global silent report for current backup session
        config.SILENTMODE = self.silentReport
        self.backupCount = 0
        self.backupSize = 0

        bufferOutput.append(f"  {util.getTimeStamp()}[white]Checking up [green bold]{self.profileName} {self.categoryName}[/green bold][/white]...")
        for parentSrcPath in self.parentSrcPaths:

            # Get version string
            if isinstance(self.versionFind, Callable):
                try:
                    self.versionFind = self.versionFind(parentSrcPath)
                    if self.versionFind == "":
                        self.versionFind = "Generic"
                except Exception as e:
                    self.versionFind = "Generic"
                    print(e)
                    print('[red]  Version string use "Generic" instead\n[/red]')

            bufferOutput.append(f"    {util.getTimeStamp()}[white]Checking up files for [green bold]{self.profileName} {self.categoryName}[/green bold] inside folder: [yellow]{parentSrcPath}[/yellow][/white]")

            # Get parent destination path
            parentSrcRelAnchorPath = parentSrcPath.relative_to(parentSrcPath.anchor)
            parentDstPath = Path(
                config.DESTPATH, # type: ignore
                self.profileName,
                self.categoryName,
                self.versionFind,
                parentSrcPath.anchor[:1],
                parentSrcRelAnchorPath
            )


            # Glob all filter pattern paths
            filterAllPaths = []
            if isinstance(self.filterPattern, list):
                for pattern in self.filterPattern:
                    filterPaths = list(parentSrcPath.glob(pattern))
                    if len(filterPaths) == 0:
                        continue
                    filterAllPaths.extend(filterPaths)


            # Copy files from source to destination
            currentParentSrcCount, currentParentSrcSize, bufferOutput = self.iterCopy(
                parentSrcPath=parentSrcPath,
                parentDstPath=parentDstPath,
                filterAllPaths=filterAllPaths,
                bufferOutput = bufferOutput
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
            if currentParentSrcCount > 0:
                bufferOutput.append(f"    {util.getTimeStamp()}[white]{Profile.foundFileMessage} [purple bold]{currentParentSrcCount}[/purple bold] files of [blue bold]{util.humanReadableSize(currentParentSrcSize)}[/blue bold] for [green bold]{self.profileName} {self.categoryName} {self.versionFind}[/green bold] files inside folder: [yellow]{parentSrcPath}[/yellow][/white]")
            else:
                bufferOutput.append(f"    {util.getTimeStamp()}[white]Skipped [purple bold]{currentParentSrcCount}[/purple bold] files of [blue bold]{util.humanReadableSize(currentParentSrcSize)}[/blue bold] for [green bold]{self.profileName} {self.categoryName} {self.versionFind}[/green bold] files inside folder: [yellow]{parentSrcPath}[/yellow][/white]")
                for lineIdx, line in enumerate(bufferOutput[::-1]):
                    if "Checking up" in line:
                        bufferOutput[-1 - lineIdx] = line.replace("Checking up", "Skipped")
                        break

        # Report count for the current category
        if self.backupCount > 0:
            bufferOutput.append(f"  {util.getTimeStamp()}[white]{Profile.foundFileMessage} [purple bold]{self.backupCount}[/purple bold] files of [blue bold]{util.humanReadableSize(self.backupSize)}[/blue bold] for [green bold]{self.profileName} {self.categoryName}[/green bold].[/white]")
        else:
            bufferOutput.append(f"  {util.getTimeStamp()}[white]Skipped [purple bold]{self.backupCount}[/purple bold] files of [blue bold]{util.humanReadableSize(self.backupSize)}[/blue bold] for [green bold]{self.profileName} {self.categoryName}[/green bold].[/white]")
            for lineIdx, line in enumerate(bufferOutput[::-1]):
                if "Checking up" in line:
                    bufferOutput[-1 - lineIdx] = line.replace("Checking up", "Skipped")
                    break

        Profile.profileDict[self.profileName].backupCount += self.backupCount
        Profile.profileDict[self.profileName].backupSize  += self.backupSize
        Profile.totalBackupCount += self.backupCount
        Profile.totalFileBackupSize  += self.backupSize

        return bufferOutput
        # }}}

# }}}


class RegCategory(FileCategory): # {{{
    """
    A category for backing up Windows registry keys and values.

    This class handles the backup of Windows registry entries to .reg files,
    applying filtering rules to include or exclude specific registry keys and values.
    It inherits from FileCategory but overrides key methods to handle registry operations.

    Instance Attributes:
        profileName (str): Name of the parent profile this category belongs to.
        categoryName (str): Name identifying this specific category.
        enabled (bool): Whether this category is active for backup.
        recursiveCopy (bool): Whether to recursively back up registry subkeys.
        silentReport (bool): Whether to suppress detailed reporting.
        parentPaths (str): Registry path to back up (e.g., "HKEY_CURRENT_USER\\Software\\...").
        filterType (str): Either "include" or "exclude" to define filter behavior.
        filterPattern (list[re.Pattern]): List of compiled regex patterns to filter registry keys.
        hkey (winreg.HKEYType): Windows registry key handle.
        componentMidPathStr (str): Middle part of the registry path.
        componentPathStrs (list[str]): List of registry paths that match the pattern.
    """
    def __init__(
        self,
        profileName: str,
        categoryName: str,
        enabled: bool,
        recursiveCopy: bool,
        silentReport: bool,
        parentPaths: str,
        filterType: str,
        filterPattern: str | Callable,
    ):
        self.hkey = cast(winreg.HKEYType, None)
        self.hkeyStr: str = ""
        self.componentPathsMid: str = ""
        self.componentPathStrs: list[str] = []
        # UGLY:
        # self.hkey: Optional[winreg.HKEYType] = None
        self.profileName  = profileName
        self.categoryName = categoryName
        self.enabled      = enabled
        if not self.enabled:
            return

        self.recursiveCopy = recursiveCopy
        self.silentReport  = silentReport
        self.filterType    = filterType
        self.filterPattern = filterPattern
        self.parentPaths   = parentPaths

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
            raise ValueError(f"string value is expected from the categoryName parameter for category {self.categoryName} under profile {self.profileName}.")
        self._categoryName = val
    # }}}

    # Validation of recursiveCopy {{{
    @property
    def recursiveCopy(self):
        return self._recursiveCopy
    @recursiveCopy.setter
    def recursiveCopy(self, val):
        if not isinstance(val, bool):
            raise ValueError(f"bool value is expected from the recursiveCopy parameter for category {self.categoryName} under configuration {self.profileName}.")
        self._recursiveCopy = val
    # }}}

    # Validation of silentReport {{{
    @property
    def silentReport(self):
        return self._silentReport
    @silentReport.setter
    def silentReport(self, val):
        if not isinstance(val, bool):
            raise ValueError(f"bool is expected from the silentReport parameter for category {self.categoryName} under profile {self.profileName}.")
        self._silentReport = val
    # }}}

    # Validation of parentPaths {{{
    @property
    def parentPaths(self):
        return self._parentPaths
    @parentPaths.setter
    def parentPaths(self, val):
        # Validate path pattern
        if not isinstance(val, str):
            raise ValueError(f"string value is expected from the registry parent path for category {self.categoryName} under profile {self.profileName}.")
        if "/" in val:
            raise ValueError(fr'"/" character is not allowed in the registry parent path for category {self.categoryName} under profile {self.profileName}.')
        if val[0] == "\\":
            raise ValueError(f"the registry parent path must not start with a backslash for category {self.categoryName} under profile {self.profileName}.")

        # Get path components
        componentPaths = val.split("\\")
        componentPathsValid = [
            part for part in componentPaths if not re.fullmatch(r"^[a-zA-Z]\W*$", part)  # Exclude regex patterns like \s*, \d+
        ]
        self.hkeyStr = componentPathsValid[0]
        self.componentPathsMid = "\\".join(componentPathsValid[1:-1])
        fullPathPatFistStr = "\\".join(componentPathsValid[0:-1]) + "\\"
        fullPathPatStr = re.escape(fullPathPatFistStr) + val.replace(fullPathPatFistStr, "")
        fullPathPat = re.compile(fullPathPatStr)
        if self.hkeyStr not in ("HKEY_CLASSES_ROOT", "HKEY_CURRENT_USER", "HKEY_LOCAL_MACHINE", "HKEY_USERS", "HKEY_CURRENT_CONFIG"):
            raise ValueError(f"invalid registry hkey for category {self.categoryName} under profile {self.profileName}.")
        else:
            self.hkey = getattr(winreg, self.hkeyStr)
        try:
            with winreg.OpenKey(self.hkey, self.componentPathsMid) as _:
                pass
        except FileNotFoundError:
            self._enable = False
            print(f"[gray]Skipped unfound registry parent path for {val} for category {self.categoryName} under profile {self.profileName}.[/gray]", skipChk=False)

        # Glob as many paths that match the last component of the parent path as possible
        self.componentPathStrs = []
        with winreg.OpenKey(self.hkey, self.componentPathsMid) as key:
            i = 0
            while True:
                try:
                    subkey = winreg.EnumKey(key, i)
                    fullPathStr = fr"{self.hkeyStr}\{self.componentPathsMid}\{subkey}"
                    # Treat subkeys as values here to make parent subkeys always pass skip check if `self.filterType` is "include"
                    if not self.shouldSkipKey(fullPathStr, False) and fullPathPat.search(fullPathStr):
                        self.componentPathStrs.append(subkey)

                    i += 1
                except OSError:
                    break
        if not self.componentPathStrs:
            self._enable = False
            print(f"[gray]Skipped unfound registry parent path for {val} for category {self.categoryName} under profile {self.profileName}.[/gray]", skipChk=False)


        self._parentPaths = val
    # }}}


    # Validation of filterType {{{
    @property
    def filterType(self):
        return self._filterType
    @filterType.setter
    def filterType(self, val):
        if not isinstance(val, str):
            raise ValueError(f"string is expected from the filterType parameter for category {self.categoryName} under profile {self.profileName}.")
        if val != "include" and val != "exclude":
            raise ValueError(f"filterType parameter must be either 'include' or 'exclude' for category {self.categoryName} under profile {self.profileName}.")

        self._filterType = val
    # }}}

    # Validation of filterPattern {{{
    @property
    def filterPattern(self) -> list[re.Pattern]:
        return self._filterPattern
    @filterPattern.setter
    def filterPattern(self, val):
        if not isinstance(val, list):
            raise ValueError(f"list value is expected as the filterPattern parameter for category {self.categoryName} under profile {self.profileName}.")
        for pattern in val:
            if not isinstance(pattern, str):
                raise ValueError(f"string value is expected in the filterPattern list for category {self.categoryName} under profile {self.profileName}.")
        self._filterPattern = list(map(lambda p: re.compile(p), val))
    # }}}

    def shouldSkipKey(self, fullPath: str, isSubKey:bool) -> bool: # {{{
        """
        Determines if a registry key should be skipped based on filter patterns.

        Applies the configured filter patterns to the registry key path to decide
        whether it should be included or excluded from the backup.

        Args:
            fullPath (str): Full registry key path to check against filter patterns.

        Returns:
            bool: True if the key should be skipped, False if it should be processed.

        Note:
            - For "exclude" filterType: Returns True if any pattern matches (skip matched keys)
            - For "include" filterType: Returns True if no pattern matches (skip unmatched keys)
        """
        if self.filterType == "exclude":
            for p in self.filterPattern:
                if re.search(p, fullPath):
                    return True
            return False
        else:
            if isSubKey:
                for p in self.filterPattern:
                    if re.search(p, fullPath):
                        return False
                return True
            else:
                 # For values, always include them
                return False

    # }}}

    @staticmethod
    def formatRegValue(value, valueType) -> str: # {{{
        """
        Formats a registry value according to its type for .reg file export.

        Converts registry values of different types (string, DWORD, binary, etc.)
        into the proper string format required for Windows .reg files.

        Args:
            value: The registry value to format (can be various types).
            valueType: The Windows registry value type constant (from winreg module).

        Returns:
            str: Formatted string representation of the registry value suitable for .reg files.

        Note:
            Handles common registry types including:
            - REG_SZ (strings)
            - REG_DWORD (32-bit integers)
            - REG_BINARY (binary data)
            - REG_MULTI_SZ (multiple strings)
            - REG_EXPAND_SZ (expandable strings)

            For unsupported types, falls back to string representation.
        """
        match valueType:
            case winreg.REG_SZ:
                return '"{}"'.format(
                    value.replace('\\', '\\\\').replace('"', '\\"')
                )
            case winreg.REG_DWORD:
                return f"dword:{value:08x}"
            case winreg.REG_BINARY:
                # Format as comma-separated hex bytes with leading zeros
                hexBytes = [f"{byte:02x}" for byte in value]
                return f"hex:{','.join(hexBytes)}"
            case winreg.REG_MULTI_SZ:
                # Format as hex(7) type with comma-separated bytes including null terminators
                bytesList = []
                for s in value:
                    bytesList.extend([ord(c) for c in s])
                    bytesList.append(0)  # null terminator
                bytesList.append(0)  # final null terminator
                hexStr = ','.join(f"{b:02x}" for b in bytesList)
                return f"hex(7):{hexStr}"
            case winreg.REG_EXPAND_SZ:
                hexStr = ','.join(f"{ord(c):02x}" for c in value + "\0")
                return f"hex(2):{hexStr}"
            case _:
                return f'"{value}"'  # Fallback for other types # }}}


    def recursiveExport(
        self,
        currentComponentPath:str,
        key:winreg.HKEYType,
        regContent: list[str]
    ): # {{{
        """
        Recursively exports registry keys and values to a .reg file.

        This method traverses the registry tree starting from a given key, exporting
        all values and subkeys that match the filter criteria to the specified file.

        Args:
            currentComponentPath (str): Current registry path being processed.
            key (winreg.HKEYType): Windows registry key handle for the current path.
            exportFile (_io.TextIOWrapper): Open file handle for writing .reg content.

        Note:
            - Applies shouldSkipKey filtering to determine which keys and values to export
            - Formats registry values according to their types using formatRegValue
            - Handles registry access errors gracefully
            - Creates properly formatted .reg file entries with appropriate headers
        """

        # Write key header
        regContent.append(f"[{self.hkeyStr}\\{currentComponentPath}]")

        # Export values
        try:
            i = 0
            while True:
                try:
                    name, value, valueType = winreg.EnumValue(key, i)
                    if self.shouldSkipKey(f"{currentComponentPath}\\{name}", False):
                        continue

                    formattedValue = self.formatRegValue(value, valueType)

                    # Write to file
                    if name:
                        regContent.append(f'"{name}"={formattedValue}')
                    else:  # Default value
                        regContent.append(f'@={formattedValue}')
                    i += 1
                except OSError:
                    break
        except WindowsError:
            pass

        regContent.append("")  # Extra newline between keys

        # Recursively export subkeys
        try:
            j = 0
            while True:
                try:
                    subkeyName = winreg.EnumKey(key, j)
                    fullSubpath = f"{currentComponentPath}\\{subkeyName}"

                    if not self.shouldSkipKey(fullSubpath, True):
                        with winreg.OpenKey(key, subkeyName) as subkey:
                            regContent = self.recursiveExport(fullSubpath, subkey, regContent)
                    j += 1
                except OSError:
                    break
        except WindowsError:
            pass

        return regContent
    # }}}


    def backup(self, bufferOutput:list[str]) -> list[str]:
        for componentStr in self.componentPathStrs:
            outputFilePath = Path(
                config.DESTPATH, # type: ignore
                self.profileName,
                self.categoryName,
                componentStr + ".reg"
            )
            regContent = []
            try:
                if not config.DRYRUN:
                    os.makedirs(outputFilePath.parent, exist_ok=True)
                    with open(outputFilePath, 'w', encoding='utf-16') as exportFile:
                        # Write REG file header
                        regContent.append("Windows Registry Editor Version 5.00\n")

                        # Start export from root key
                        parentComponentPath = self.componentPathsMid + "\\" + componentStr
                        with winreg.OpenKey(self.hkey, parentComponentPath) as key:
                            regContent = self.recursiveExport(parentComponentPath, key, regContent)

                        exportFile.write('\n'.join(regContent))

                    bufferOutput.append(r"[white]    {} regitry at: [yellow]{}\{}\{}[/yellow]".format(
                        Profile.foundFileMessage,
                        self.hkeyStr,
                        self.componentPathsMid,
                        componentStr)
                    )


                bufferOutput.append(fr"  {util.getTimeStamp()}[white]{Profile.foundFileMessage} {len(self.componentPathStrs)} matched registry paths at [green bold]{self.parentPaths}[/green bold].[/white]")
                Profile.profileDict[self.profileName].backupCount += len(self.componentPathStrs)
                Profile.totalBackupCount += len(self.componentPathStrs)
            except FileNotFoundError:
                bufferOutput.append(fr"[red]    registry path: {self.hkey}\{self.componentPathsMid} doesn't exist.[/red]")
            except PermissionError:
                bufferOutput.append("[red]    permission denied. make sure to run the script as Administrator.[/red]")
            except Exception as e:
                bufferOutput.append(f"[red]    Error exporting registry: {str(e)}[/red]")

        return bufferOutput
# }}}

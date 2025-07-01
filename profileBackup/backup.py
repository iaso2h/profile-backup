import os
import winreg
import shutil
import re
from types import GeneratorType
from typing import Callable, Optional, Tuple, Iterator, cast
from pathlib import Path

import util
import config

print = util.print
WINDOWS_ANCHOR_START_PAT = re.compile(r"^[a-zA-Z]:\\")
WINDOWS_REG_HEADER = "Windows Registry Editor Version 5.00"
WINDOWS_REG_ENCODING = "utf-16"


class Profile(): # {{{
    """
    A backup profile that manages file and registry backup configurations.

    This class represents a backup profile that can contain multiple file and registry categories
    to be backed up. It tracks backup statistics and manages the backup process for all its categories.
    Each profile serves as a logical grouping of related backup items that can be enabled or disabled
    as a unit.

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
        The profile is automatically registered in the global profileDict if enabled.
        Initializes tracking statistics for backup operations and sets up category instances
        based on the provided configuration.

        Args:
            profileName (str): Unique name identifying this profile.
            categories (list): List of dictionaries defining file or registry categories.
                Each dictionary must contain:
                - "type" key with value "file" or "registry"
                - For file categories:
                    * categoryName (str): Name of the category
                    * enabled (bool): Whether the category is active
                    * recursiveCopy (bool): Whether to copy subdirectories
                    * silentReport (bool): Whether to suppress detailed reporting
                    * parentSrcPaths (str|Path|list): Source paths to back up
                    * filterType (str): "include" or "exclude"
                    * filterPattern (list|Callable): Patterns or function for filtering
                - For registry categories:
                    * categoryName (str): Name of the category
                    * enabled (bool): Whether the category is active
                    * recursiveCopy (bool): Whether to copy subkeys
                    * silentReport (bool): Whether to suppress detailed reporting
                    * stripePathValue (bool): Whether to create stripped versions
                    * parentPaths (str): Registry paths to back up
                    * filterType (str): "include" or "exclude"
                    * filterPattern (list): Regex patterns for filtering
            enabled (bool): Whether this profile should be active for backup operations.

        Note:
            - Initializes ticked status as True by default
            - Sets initial backup count and size to 0
            - Validates all input parameters through property setters
            - Automatically registers enabled profiles in profileDict
            - Creates appropriate category instances based on type
            - Handles both file and registry category configurations
            - Performs type checking and validation for all parameters
            - Supports both file system and registry backup configurations
            - Maintains a global registry of all enabled profiles
            - Establishes parent-child relationships between profiles and categories
            - Provides foundation for the backup operation workflow
            - Ensures consistent initialization of all profile attributes
            - Supports dynamic category creation based on configuration type
            - Maintains separation between file and registry backup logic
            - Implements proper error handling for invalid configurations
            - Supports flexible backup source specification (paths, globs, etc.)
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
        This affects all status messages displayed during the backup process and ensures consistent
        messaging throughout the application based on the current operation mode.

        Note:
            - Checks config.DRYRUN to determine the appropriate message
            - Used to maintain consistent messaging across all backup operations
            - Called before starting backup operations to ensure correct messaging
            - Affects all instances of Profile and its subclasses
            - Supports both actual backup and simulation (dry run) modes
            - Changes are applied globally to all profile instances
        """
        cls.foundFileMessage = "Backing up" if not config.DRYRUN else "Found"
# }}}


class FileCategory(Profile): # {{{
    """
    A category for backing up files from specified source paths.

    This class handles the backup of files from one or more source directories to corresponding
    destination directories, applying filtering rules and tracking backup statistics. It supports
    both include and exclude filtering patterns, recursive directory traversal, and synchronization
    between source and destination.

    Class Attributes:
        syncFilesToDelete (dict): Tracks files in destination that don't exist in source for sync operations.
            Structure: {profileName: {parentDstPath: [list of Path objects to delete]}}
        relPathsTopParentSrc (dict): Maps profile names to source paths and their relative paths.
            Structure: {profileName: {parentSrcPath: [list of relative path strings]}}

    Instance Attributes:
        profileName (str): Name of the parent profile this category belongs to.
        categoryName (str): Name identifying this specific category.
        enabled (bool): Whether this category is active for backup.
        recursiveCopy (bool): Whether to recursively copy subdirectories.
        silentReport (bool): Whether to suppress detailed reporting.
        parentSrcPaths (list[Path]): List of source directory paths to back up.
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
        enabled: bool,
        recursiveCopy: bool,
        silentReport: bool,
        parentSrcPaths: str | Path | list[Path] | Iterator[Path] | map,
        filterType: str,
        filterPattern: str | Callable,
    ):
        """
        Initialize a new FileCategory instance for file backup.

        Creates a file backup category with specified configuration parameters.
        Sets up source paths and filter patterns for file backup operations.

        Args:
            profileName (str): Name of the parent profile this category belongs to.
            categoryName (str): Name identifying this specific category.
            enabled (bool): Whether this category is active for backup.
            recursiveCopy (bool): Whether to recursively copy subdirectories.
            silentReport (bool): Whether to suppress detailed reporting.
            parentSrcPaths (str|Path|list[Path]|Iterator[Path]|map): Source paths to back up.
                Can be a single path, list of paths, or path generator.
            filterType (str): Either "include" or "exclude" to define filter behavior.
            filterPattern (str|Callable): Patterns or function for filtering files.
        """
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
                - bufferOutput: Updated list of output messages with copy operation details

        Note:
            - Handles permission errors gracefully with appropriate error messages
            - Creates destination directories automatically when needed
            - Preserves file metadata using shutil.copy2
            - Respects COPYOVERWRITE setting to determine whether to overwrite existing files
            - Tracks relative paths for later synchronization operations
            - Provides colored console output for different file statuses
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
                - bufferOutput: Updated list of output messages with copy operation details

        Note:
            - Handles both directory and file paths differently
            - Applies filtering logic based on filterType (include/exclude)
            - Supports both pattern-based and callable-based filtering
            - Maintains relative path relationships between source and destination
            - Handles permission errors gracefully with appropriate error messages
            - Accumulates statistics for reporting purposes
            - Recursively processes subdirectories when recursiveCopy is enabled
        """
        countAccumulated = 0
        sizeAccumulated = 0

        # Initialization for the first function call
        if not topParentSrcPath:
            topParentSrcPath = parentSrcPath

        # Statistic recording
        if self.profileName not in type(self).relPathsTopParentSrc:
            type(self).relPathsTopParentSrc[self.profileName] = {}
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
        sync operations. This ensures that the destination directory becomes an exact
        mirror of the source directory when synchronization is completed.

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
            - The method is called when COPYSYNC configuration is enabled.
            - Maintains a hierarchical structure of files to delete organized by profile and destination path.
            - Skips processing if the destination directory doesn't exist.
            - Performs a depth-first traversal of the destination directory structure
            - Efficiently handles large directory structures through recursive processing
        """
        # Abort when parent destination directory doesn't exist
        if not parentDstPath.exists():
            return

        # Initialization for the first function call
        if not topParentDstPath:
            topParentDstPath = parentDstPath

        syncFilesToDeleteRelCurrentParentDst = type(self).syncFilesToDelete[self.profileName][topParentDstPath]

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
        Performs the backup operation for this file category.

        This method processes all configured source paths, copying files that match
        the filter criteria to their corresponding destination paths. It handles
        directory creation, file filtering, and tracks backup statistics. The method
        also manages synchronization between source and destination when COPYSYNC is enabled.

        Args:
            bufferOutput (list[str]): List to collect output messages during operation.

        Returns:
            list[str]: Updated list of output messages with backup operation details.

        Note:
            - Sets the global silent report mode based on category configuration
            - Processes each parent source path separately
            - Applies filter patterns to determine which files to back up
            - Constructs appropriate destination paths preserving source structure
            - Updates statistics at category, profile, and global levels
            - Provides detailed progress reporting with color-coded messages
            - Handles synchronization marking when COPYSYNC is enabled
            - Adapts output messages based on whether files were found or skipped
            - Maintains consistent messaging based on dry run mode
        """
        # Alter global silent report for current backup session
        config.SILENTMODE = self.silentReport
        self.backupCount = 0
        self.backupSize = 0

        bufferOutput.append(f"  {util.getTimeStamp()}[white]Checking up [green bold]{self.profileName} {self.categoryName}[/green bold][/white]...")
        for parentSrcPath in self.parentSrcPaths:

            bufferOutput.append(f"    {util.getTimeStamp()}[white]Checking up files for [green bold]{self.profileName} {self.categoryName}[/green bold] inside folder: [yellow]{parentSrcPath}[/yellow][/white]")

            # Get parent destination path
            parentSrcRelAnchorPath = parentSrcPath.relative_to(parentSrcPath.anchor)
            parentDstPath = Path(
                config.DESTPATH, # type: ignore
                self.profileName,
                self.categoryName,
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
                # Statistic recording
                if self.profileName not in type(self).syncFilesToDelete:
                    type(self).syncFilesToDelete[self.profileName] = {}
                type(self).syncFilesToDelete[self.profileName][parentDstPath] = []

                self.iterSync(
                    relPathsTopParentSrc=relPathsTopParentSrc,
                    parentDstPath=parentDstPath,
                    topParentSrcPath=parentSrcPath
            )

            # Report count for the current parent source directory
            if currentParentSrcCount > 0:
                bufferOutput.append(f"    {util.getTimeStamp()}[white]{Profile.foundFileMessage} [purple bold]{currentParentSrcCount}[/purple bold] files of [blue bold]{util.humanReadableSize(currentParentSrcSize)}[/blue bold] for [green bold]{self.profileName} {self.categoryName}[/green bold] files inside folder: [yellow]{parentSrcPath}[/yellow][/white]")
            else:
                bufferOutput.append(f"    {util.getTimeStamp()}[white]Skipped [purple bold]{currentParentSrcCount}[/purple bold] files of [blue bold]{util.humanReadableSize(currentParentSrcSize)}[/blue bold] for [green bold]{self.profileName} {self.categoryName} {parentSrcPath}[/green bold] files inside folder: [yellow]{parentSrcPath}[/yellow][/white]")
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
    The class supports creating multiple versions of registry exports, including stripped
    versions that remove file paths for portability.

    Instance Attributes:
        profileName (str): Name of the parent profile this category belongs to.
        categoryName (str): Name identifying this specific category.
        enabled (bool): Whether this category is active for backup.
        recursiveCopy (bool): Whether to recursively back up registry subkeys.
        silentReport (bool): Whether to suppress detailed reporting.
        stripePathValue (bool): Whether to create stripped versions of registry values.
        parentPaths (str): Registry path to back up (e.g., "HKEY_CURRENT_USER/Software/...").
        filterType (str): Either "include" or "exclude" to define filter behavior.
        filterPattern (list[re.Pattern]): List of compiled regex patterns to filter registry keys.
        rootKey (winreg.HKEYType): Windows registry key handle for the root key.
        rootKeyStr (str): String representation of the root key (e.g., "HKEY_CURRENT_USER").
        keyRelPaths (list[str]): List of registry paths relative to the root key.
        regContent (list): Buffer for standard registry content.
        regContentRefined (list): Buffer for refined registry content.
        regContentStripped (list): Buffer for stripped registry content (without file paths).

    Note: learn about structure of Windows registry -> https://learn.microsoft.com/en-us/windows/win32/sysinfo/structure-of-the-registry
    """
    def __init__(
        self,
        profileName: str,
        categoryName: str,
        enabled: bool,
        recursiveCopy: bool,
        silentReport: bool,
        stripePathValue: bool,
        parentPaths: str,
        filterType: str,
        filterPattern: str | Callable,
        keyPathNamingConvention: Optional[Callable] = None,
    ):
        """
        Initialize a new RegCategory instance for registry backup.

        Creates a registry backup category with specified configuration parameters.
        Sets up registry key paths and handles based on the provided parent paths.

        Args:
            profileName (str): Name of the parent profile this category belongs to.
            categoryName (str): Name identifying this specific category.
            enabled (bool): Whether this category is active for backup.
            recursiveCopy (bool): Whether to recursively back up registry subkeys.
            silentReport (bool): Whether to suppress detailed reporting.
            stripePathValue (bool): Whether to create stripped versions of registry values.
            parentPaths (str): Registry path to back up (e.g., "HKEY_CURRENT_USER/Software/...").
            filterType (str): Either "include" or "exclude" to define filter behavior.
            filterPattern (str|Callable): Patterns for filtering registry keys.

        Note:
            - Handles registry path globbing to find matching keys
            - Maintains proper registry path formatting and structure
            - Supports pattern-based filtering of registry keys and values
            - Provides both standard and stripped versions of registry exports
        """
        self.rootKey = cast(winreg.HKEYType, None)
        self.rootKeyStr: Optional[str] = None
        self.keyRelPaths: list[str] = []
        # UGLY:
        # self.hkey: Optional[winreg.HKEYType] = None
        self.profileName  = profileName
        self.categoryName = categoryName
        self.enabled      = enabled
        if not self.enabled:
            return

        self.regContent = []
        self.regContentWriteChk = False
        self.regContentRefined = []
        self.regContentRefinedWriteChk = False
        self.regContentStripped = []
        self.regContentStrippedWriteChk = False
        self.recursiveCopy   = recursiveCopy
        self.silentReport    = silentReport
        self.stripePathValue = stripePathValue
        self.filterType      = filterType
        self.filterPattern   = filterPattern
        self.parentPaths     = parentPaths
        self.keyPathNamingConvention = keyPathNamingConvention


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

    # Validation of stripePathValue {{{
    @property
    def stripePathValue(self):
        return self._stripePathValue
    @stripePathValue.setter
    def stripePathValue(self, val):
        if not isinstance(val, bool):
            raise ValueError(f"bool is expected from the stripePathValue parameter for category {self.categoryName} under profile {self.profileName}.")
        self._stripePathValue = val
    # }}}

    # Validation of parentPaths {{{
    @property
    def parentPaths(self):
        return self._parentPaths
    @parentPaths.setter
    def parentPaths(self, val):
        # Validate path pattern
        try:
            self.keyRelPaths, self.rootKeyStr = util.regGlobKeyRelPaths(val)
        except Exception as e:
            # Re-raise the exception with additional time information
            raise type(e)(f"errors occured when parsing recipe for category {self.categoryName} under profile {self.profileName}.")

        if not self.keyRelPaths:
            self._enable = False
            print(f"[gray]Skipped unfound registry parent path for {val} for category {self.categoryName} under profile {self.profileName}.[/gray]", skipChk=False)
            return

        self.rootKey = getattr(winreg, self.rootKeyStr)
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

    # Validation of parentPathsNamingConvention {{{
    @property
    def keyPathNamingConvention(self) -> Optional[Callable]:
        return self._keyPathNamingConvention
    @keyPathNamingConvention.setter
    def keyPathNamingConvention(self, val):
        if not val is None and not isinstance(val, Callable):
            raise ValueError(f"function value is expected as the parentPathsNamingConvention parameter for category {self.categoryName} under profile {self.profileName}.")

        self._keyPathNamingConvention = val
    # }}}


    def shouldSkipKey(self, fullPath: str) -> bool: # {{{
        """
        Determines if a registry key should be skipped based on filter patterns.

        Applies the configured filter patterns to the registry key path to decide
        whether it should be included or excluded from the backup.

        Args:
            fullPath (str): Full registry key path to check against filter patterns.

        Returns:
            bool: True if the key should be skipped, False if it should be processed.
        """
        if self.filterType == "exclude":
            for p in self.filterPattern:
                if re.search(p, fullPath):
                    return True
            return False
        else:
            for p in self.filterPattern:
                if re.search(p, fullPath):
                    return False
            return True

    # }}}

    @staticmethod
    def formatRegValue(value, valueType) -> Tuple[str, str]: # {{{
        """
        Formats a registry value according to its type for .reg file export.

        Converts registry values of different types (string, DWORD, binary, etc.)
        into the proper string format required for Windows .reg files. For string
        values, also provides a stripped version without file paths when possible,
        which is useful for creating portable registry exports.

        Args:
            value: The registry value to format (can be various types).
            valueType: The Windows registry value type constant (from winreg module).

        Returns:
            Tuple[str, str]: A tuple containing:
                - str: Formatted string representation of the registry value
                - str: Stripped version (without file paths) for string values,
                  or the same formatted value for other types

        Note:
            Handles common registry types including:
            - REG_SZ (strings): Returns both original and stripped versions
            - REG_DWORD (32-bit integers): Returns same value for both tuple elements
            - REG_BINARY (binary data): Returns hex-formatted representation
            - REG_MULTI_SZ (multiple strings): Returns hex(7)-formatted representation
            - REG_EXPAND_SZ (expandable strings): Returns hex(2)-formatted representation

            For string values (REG_SZ), the stripped version removes Windows file paths
            if present, returning an empty string. For other types, both tuple elements
            contain the same value. The method properly escapes special characters and
            formats values according to Windows Registry Editor requirements.
        """
        match valueType:
            case winreg.REG_SZ:
                val = '"{}"'.format(
                    value.replace('\\', '\\\\').replace('"', '\\"')
                )
                if not WINDOWS_ANCHOR_START_PAT.search(value):
                    return val, val
                else:
                    return val, ""
            case winreg.REG_DWORD:
                val = f"dword:{value:08x}"
                return val, val
            case winreg.REG_BINARY:
                # Format as comma-separated hex bytes with leading zeros
                hexBytes = [f"{byte:02x}" for byte in value]
                val = f"hex:{','.join(hexBytes)}"
                return val, val
            case winreg.REG_MULTI_SZ:
                # Format as hex(7) type with comma-separated bytes including null terminators
                bytesList = []
                for s in value:
                    bytesList.extend([ord(c) for c in s])
                    bytesList.append(0)  # null terminator
                bytesList.append(0)  # final null terminator
                hexStr = ','.join(f"{b:02x}" for b in bytesList)
                val = f"hex(7):{hexStr}"
                return val, val
            case winreg.REG_EXPAND_SZ:
                hexStr = ','.join(f"{ord(c):02x}" for c in value + "\0")
                val = f"hex(2):{hexStr}"
                return val, val
            case _:
                val = f'"{value}"'
                return val, val

    # }}}


    def recursiveExport(self, currentSubkeyPath:str, key:winreg.HKEYType) -> None: # {{{
        """
        Recursively exports registry keys and values to .reg file format.

        This method traverses the registry tree starting from a given key, collecting
        all values and subkeys that match the filter criteria into three separate content
        lists - one for regular content, one for refined content, and one for content
        with stripped file paths. This allows for creating different versions of registry
        exports with varying levels of portability.

        Args:
            currentSubkeyPath (str): Current registry path being processed.
            key (winreg.HKEYType): Windows registry key handle for the current path.

        Note:
            - Applies shouldSkipKey filtering to determine which keys and values to export
            - Formats registry values according to their types using formatRegValue
            - Recursively processes subkeys when recursiveCopy is enabled
            - Creates three versions of registry content:
                * Standard: Complete registry content
                * Refined: Registry content without empty keys
                * Stripped: Registry content with file paths removed
            - Maintains consistent formatting for Windows Registry Editor compatibility
        """

        # Compose key header for later use
        currentSubkeyHeader = f"\n[{self.rootKeyStr}\\{currentSubkeyPath}]"

        # Export values
        try:
            i = 0
            # - Check if the current key has any valid value.
            # - These flags will be changed right before the very first valid
            # value is written(technically buffered).
            # - Moreover, they are used to control the adding of current subkey
            # header, thus avoiding empty headers in the final exported .reg
            # files
            dataWriteUnderCurrentKeyChk = False
            dataRefinedWriteUnderCurrentKeyChk = False
            dataStrippedWriteUnderCurrentKeyChk = False
            while True:
                try:
                    valName, valData, valueType = winreg.EnumValue(key, i)
                    if self.shouldSkipKey(f"{currentSubkeyPath}\\{valName}"):
                        i += 1 # Enter next iteration to get next sibling value
                        continue

                    formattedValue, formattedValueStriped = self.formatRegValue(valData, valueType)

                    if valName:
                        regValDataLine = f'"{valName}"={formattedValue}'
                    else:
                        regValDataLine = f'@={formattedValue}'

                    if not dataWriteUnderCurrentKeyChk:
                        self.regContent.append(currentSubkeyHeader)
                        dataWriteUnderCurrentKeyChk = True
                        if not self.regContentWriteChk:
                            self.regContentWriteChk = True
                    self.regContent.append(regValDataLine)

                    if formattedValueStriped == "":
                        if not dataStrippedWriteUnderCurrentKeyChk:
                            self.regContentStripped.append(currentSubkeyHeader)
                            dataStrippedWriteUnderCurrentKeyChk = True
                            if not self.regContentStrippedWriteChk:
                                self.regContentStrippedWriteChk = True
                        self.regContentStripped.append(regValDataLine)
                    else:
                        if not dataRefinedWriteUnderCurrentKeyChk:
                            self.regContentRefined.append(currentSubkeyHeader)
                            dataRefinedWriteUnderCurrentKeyChk = True
                            if not self.regContentRefinedWriteChk:
                                self.regContentRefinedWriteChk = True
                        self.regContentRefined.append(regValDataLine)

                    i += 1 # Enter next iteration to get next sibling value
                except OSError:
                    break
        except WindowsError:
            pass

        # Recursively export subkeys
        if self.recursiveCopy:
            try:
                j = 0
                while True:
                    try:
                        subkeyName = winreg.EnumKey(key, j)
                        fullSubpath = f"{currentSubkeyPath}\\{subkeyName}"

                        if not self.shouldSkipKey(fullSubpath):
                            with winreg.OpenKey(key, subkeyName) as subkey:
                                self.recursiveExport(fullSubpath, subkey)
                        else:
                            pass # Skip this subkey

                        j += 1 # Enter next iteration to get next sibling key
                    except OSError:
                        break
            except WindowsError:
                pass
    # }}}


    def backup(self, bufferOutput:list[str]) -> list[str]:
        """
        Performs the backup operation for this registry category.

        This method exports registry keys and values to .reg files based on the configured
        paths and filter patterns. It creates multiple versions of the registry files:
        standard, refined (without empty keys), and stripped (without file paths) when
        stripePathValue is enabled. All files are created with proper Windows Registry
        Editor formatting and encoding.

        Args:
            bufferOutput (list[str]): List to collect output messages during operation.

        Returns:
            list[str]: Updated list of output messages with registry backup information.

        Note:
            - Processes each registry key path in keyRelPaths separately
            - Creates multiple versions of registry files:
                * Standard (.reg): Complete registry content
                * Refined (_refined.reg): Registry content without empty keys
                * Stripped (_stripped.reg): Registry content with file paths removed
            - Uses UTF-16 encoding for .reg files as required by Windows Registry Editor
            - Applies filter patterns to determine which keys and values to include
            - Tracks backup statistics at both profile and global levels
        """
        for keyRelPath in self.keyRelPaths:
            if self.keyPathNamingConvention:
                try:
                    regName = self.keyPathNamingConvention(keyRelPath)
                except Exception as e:
                    raise type(e)(f"errors occured when applying naming convention for keyname category {self.categoryName} under profile {self.profileName}.")
            else:
                regName = keyRelPath.replace('\\', '_')

            outputFilePath = Path(
                config.DESTPATH, # type: ignore
                self.profileName,
                self.categoryName,
                regName + ".reg"
            )
            outputFileRefinedPath = Path(
                config.DESTPATH, # type: ignore
                self.profileName,
                self.categoryName,
                regName + "_Refined" + ".reg"
            )
            outputFileStrippedPath = Path(
                config.DESTPATH, # type: ignore
                self.profileName,
                self.categoryName,
                regName + "_Stripped" + ".reg"
            )
            if not config.DRYRUN:
                os.makedirs(outputFilePath.parent, exist_ok=True)
                with open(outputFilePath, 'w', encoding=WINDOWS_REG_ENCODING) as exportRegFile:
                    # Start export from the first sub key
                    try:
                        with winreg.OpenKey(self.rootKey, keyRelPath) as key:
                            self.recursiveExport(keyRelPath, key)
                    except FileNotFoundError:
                        bufferOutput.append(fr"[red]    registry path: {self.rootKeyStr}\{keyRelPath} doesn't exist.[/red]")
                    except PermissionError:
                        bufferOutput.append("[red]    permission denied. make sure to run the script as Administrator.[/red]")
                    except Exception as e:
                        raise e

                    # Write buffer content into .reg file
                    if self.regContentWriteChk:
                        self.regContent.insert(0, WINDOWS_REG_HEADER)
                        joindContent = '\n'.join(self.regContent)
                        exportRegFile.write(joindContent)

                        # Reset buffer content
                        self.regContent = []
                        self.regContentWriteChk = False
                    else:
                        print(fr"[yellow]    no valid keys being saved at: {self.rootKeyStr}\{keyRelPath}.[/yellow]")

                # Write buffer content into refined and stripped .reg files
                if self.regContentRefinedWriteChk:
                    with open(outputFileRefinedPath, 'w', encoding=WINDOWS_REG_ENCODING) as exportRegRefinedFile:
                        self.regContentRefined.insert(0, WINDOWS_REG_HEADER)
                        joindContentRefined = '\n'.join(self.regContentRefined)
                        exportRegRefinedFile.write(joindContentRefined)

                        # Reset buffer content
                        self.regContentRefined = []
                        self.regContentRefinedWriteChk = False
                if self.regContentStrippedWriteChk:
                    with open(outputFileStrippedPath, 'w', encoding=WINDOWS_REG_ENCODING) as exportRegStrippedFile:
                        self.regContentStripped.insert(0, WINDOWS_REG_HEADER)
                        joindContentStripped = '\n'.join(self.regContentStripped)
                        exportRegStrippedFile.write(joindContentStripped)

                        # Reset buffer content
                        self.regContentStripped = []
                        self.regContentStrippedWriteChk = False

                # Statistics update for current component
                bufferOutput.append(r"[white]    {} regitry at: [yellow]{}\{}[/yellow]".format(
                    Profile.foundFileMessage,
                    self.rootKeyStr,
                    keyRelPath)
                )

        # Statistics update for all found components match the `self.parentPaths` pattern
        bufferOutput.append(
            r"  {}[white]{} [purple bold]{}[/purple bold] for [green bold]{} {}[/green bold] registry sets matched: [yellow]{}[/yellow].[/white]".format(
                util.getTimeStamp(),
                Profile.foundFileMessage,
                len(self.keyRelPaths),
                self.profileName,
                self.categoryName,
                self.parentPaths
            )
        )
        Profile.profileDict[self.profileName].backupCount += len(self.keyRelPaths)
        Profile.totalBackupCount += len(self.keyRelPaths)

        return bufferOutput
# }}}

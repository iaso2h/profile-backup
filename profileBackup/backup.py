import cli
import util

ALWAYSOVERWRITE = False


import shutil
import logging
import os
from types import GeneratorType
from typing import Callable, Generator, List, Union
from pathlib import Path, PurePath
from icecream import ic
logger = logging.getLogger(__name__)

# VIMRUN: g$[^#] \zslogger.debug$exe "norm gcc"$e

if not cli.drive:
    destPath = PurePath(os.getcwd(), "bk")
else:
    destPath = PurePath("{}:/Profiles".format(cli.drive))
os.makedirs(destPath, exist_ok=True)

userName    = os.getlogin()
appDataPath = Path("C:/Users/{}/AppData".format(userName))
homePath    = Path("C:/Users/{}".format(userName))


class Backup():
    totalBackupCount = 0
    softwareList     = []

    # TODO: type check https://stackoverflow.com/questions/2489669/how-do-python-functions-handle-the-types-of-parameters-that-you-pass-in
    def __init__(self, name: str, globPatterns: list):
        self.name         = name
        type(self).softwareList.append(self.name)
        self.softwareSequence = len(type(self).softwareList)

        self.globPatterns = globPatterns

        self.softwareBackupCount = 0


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
        for i, globPattern in enumerate(arg):
            if len(globPattern) != 4:
                raise ValueError(f"The {util.idx2sequence(i)} glob pattern doesn't contain 4 elements")
            for j, val in enumerate(globPattern):
                # Validate path pattern
                if j == 0:
                    if not isinstance(val, GeneratorType) and not isinstance(val, str):
                        raise ValueError(
    f"Wrong given path pattern. Generator or string is expected in the {util.idx2sequence(j)} element from the {util.idx2sequence(i)} glob pattern."
                                )
                # Validate version string
                elif j == 1:
                    if not isinstance(val, Callable) and not isinstance(val, str):
                        raise ValueError(
    f"Wrong given version. String or function is expected in the {util.idx2sequence(j)} element from the {util.idx2sequence(i)} glob pattern."
                                )
                # Validate filter type.
                elif j == 2:
                    if not isinstance(val, str):
                        raise ValueError("string expected")
                    if val != "include" and val != "exclude":
                        raise ValueError('Wrong given string value, it can only be either "include" or "exclude"')
                # Validate filter pattern. It could be a include pattern of a exclude pattern
                else:
                    if not isinstance(val, Callable) and not isinstance(val, list):
                        raise ValueError(
    f"Wrong given filter pattern. List or function is expected in the {util.idx2sequence(j)} element from the {util.idx2sequence(i)} glob pattern."
                                )
                    if isinstance(val, list):
                        for k in val:
                            if not isinstance(k, str):
                                raise ValueError(f"A filter string list must consist of string only.")

        self._globPatterns = arg


    @staticmethod
    def copyFile(fileSrcPath: Path, fileDstPath: Path) -> int:
        backupCount = 0
        if fileDstPath.exists():
            if ALWAYSOVERWRITE or (fileSrcPath.stat().st_mtime - fileDstPath.stat().st_mtime) > 0:
                logger.info("    Backing up file: " + fileSrcPath.name)
                # logger.debug("    Destination: " + str(fileDstPath))
                shutil.copy2(fileSrcPath, fileDstPath)
                backupCount = backupCount + 1
            else:
                logger.info("    Skip non-modified file: " + fileSrcPath.name)
        else:
            logger.info("    Backing up file: " + fileSrcPath.name)
            # logger.debug("    Destination: " + str(fileDstPath))
            os.makedirs(fileDstPath.parent, exist_ok=True)
            shutil.copy2(fileSrcPath, fileDstPath)
            backupCount = backupCount + 1

        return backupCount


    def iterRecursive(self, parentSrcPath: Path, parentDstPath: Path, typeStr: str, filter: Union[List[str], Callable[[Path], bool]] , filterAllPathStrs: list) -> int:
        count = 0

        for srcPath in parentSrcPath.iterdir():
            if srcPath.is_dir():
                count = count + self.iterRecursive(srcPath, parentDstPath, typeStr, filter, filterAllPathStrs)
            else:
                if isinstance(filter, list):
                    if typeStr == "exclude" and str(srcPath) in filterAllPathStrs:
                        continue
                    elif typeStr == "include" and str(srcPath) not in filterAllPathStrs:
                        continue
                    else:
                        srcRelParentPath = srcPath.relative_to(parentSrcPath)
                        dstPath = Path(parentDstPath, srcRelParentPath)
                        count = count + self.copyFile(srcPath, dstPath)
                else:
                    if typeStr == "exclude" and filter(srcPath):
                        continue
                    elif typeStr == "include" and not filter(srcPath):
                        continue
                    else:
                        srcRelParentPath = srcPath.relative_to(parentSrcPath)
                        dstPath = Path(parentDstPath, srcRelParentPath)
                        count = count + self.copyFile(srcPath, dstPath)

        return count


    def backup(self):
        logger.info("Checking up " + self.name + "...")

        for globPattern in self.globPatterns:
            pathPattern = globPattern[0] # type: Generator | str
            versionFind = globPattern[1] # type: Callable | str
            typeStr     = globPattern[2] # type: str
            filter      = globPattern[3] # type: Callable | list

            if isinstance(pathPattern, str):
                srcPath = Path(pathPattern)
                if not srcPath.exists():
                    logger.info("  Skipped unfound file at: " + str(srcPath))
                    continue
                else:
                    parentSrcPaths = [srcPath]     # type: list[Path]
            else:
                parentSrcPaths = list(pathPattern) # type: list[Path]

            parentSrcPath  = None
            parentDstPath  = None
            fileSrcPath    = None
            fileDstPath    = None


            for parentSrcPath in parentSrcPaths:
                # logger.debug("  parentSrcPath: " + str(parentSrcPath))
                if isinstance(versionFind, str):
                    versionStr = versionFind
                else:
                    versionStr = versionFind(parentSrcPath)
                logger.info("  Checking up {} {} files inside: {}".format(self.name, versionStr, parentSrcPath))

                parentSrcRelAnchorPath = parentSrcPath.relative_to(parentSrcPath.anchor)
                # NOTE: versionStr can be an empty string
                parentDstPath = Path(
                        destPath,
                        self.name,
                        versionStr,
                        parentSrcPath.anchor[:1],
                        parentSrcRelAnchorPath
                    )

                # Get all exclude pattern paths
                filterAllPaths = []
                if isinstance(filter, list):
                    for filterPattern in filter:
                        if not filterPattern.startswith("\\") and filterPattern.startswith("/"):
                            filterPattern = "/" + filterPattern

                        filterPaths = list(parentSrcPath.glob(filterPattern))
                        if len(filterPaths) == 0:
                            continue
                        filterAllPaths.extend(filterPaths)

                filterAllPathStrs = list(map(lambda p: str(p), filterAllPaths))

                # Filter out path that match the excluded paths
                parentSrcCount = self.iterRecursive(parentSrcPath, parentDstPath, typeStr, filter, filterAllPathStrs)
                self.softwareBackupCount = self.softwareBackupCount + parentSrcCount

        logger.info("Backed up {} {} files\n".format(self.softwareBackupCount, self.name))

        # Record
        type(self).totalBackupCount = type(self).totalBackupCount + self.softwareBackupCount
        if self.softwareSequence == len(type(self).softwareList):
            logger.info(f"Backed up {type(self).totalBackupCount} files from {type(self).softwareList}.\n")


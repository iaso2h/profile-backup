import shutil
import logging
import os
import cli
from types import GeneratorType
from typing import Callable, Generator
from pathlib import Path, PurePath
from icecream import ic
logger = logging.getLogger(__name__)

# VIMRUN: g$[^#] \zslogger.debug$exe "norm gcc"$e

if not cli.drive:
    destPath    = PurePath(os.getcwd())
else:
    destPath = PurePath("{}:/Profiles".format(cli.drive))
os.makedirs(destPath, exist_ok=True)

userName    = os.getlogin()
appDataPath = Path("C:/Users/{}/AppData".format(userName))
homePath    = Path("C:/Users/{}".format(userName))



class Exclude:
    # TODO: type check https://stackoverflow.com/questions/2489669/how-do-python-functions-handle-the-types-of-parameters-that-you-pass-in
    def __init__(self, name: str, globPatterns: list):
        self.name         = name
        self.globPatterns = globPatterns
        self.backupCount  = 0

    def backup(self):
        logger.info("Backing up " + self.name + "...")

        for glob in self.globPatterns:
            # glob = [
                # userAppDataPath.glob("Local/Autodesk/3dsMax/*/*/*/UI/Workspaces"), <function object to extract version str from the globbed path>, <function object to filter file, filter out when evaluated to True>
            #]
            parentGlobPath    = glob[0]
            versionFindFunc   = glob[1]           # type: Callable
            excludeFilterFunc = glob[2]           # type: Callable

            # TODO: will raise error if parentGlobPath point to an invalid path
            # if not isinstance(parentGlobPath, GeneratorType):

            parentSrcPaths = list(parentGlobPath) # type: list[Path]
            parentSrcPath  = ""
            parentDstPath  = ""
            fileSrcPath    = ""
            fileDstPath    = ""

            # Skip if no glob pattern found
            if len(parentSrcPaths) == 0:
                continue

            for parentSrcPath in parentSrcPaths:
                # logger.debug("  parentSrcPath: " + str(parentSrcPath))
                versionStr = versionFindFunc(parentSrcPath)
                logger.info("  Backing up {} {} inside: {}".format(self.name, versionStr, parentSrcPath))

                parentSrcRelAnchorPath = parentSrcPath.relative_to(parentSrcPath.anchor)
                # NOTE: versionStr can be an empty string
                parentDstPath = Path(
                        destPath,
                        self.name,
                        versionStr,
                        parentSrcPath.anchor[:1],
                        parentSrcRelAnchorPath
                        )


                for fileSrcPath in parentSrcPath.iterdir():
                    if not excludeFilterFunc(fileSrcPath):
                        fileSrcRelParentPath = fileSrcPath.relative_to(parentSrcPath)
                        fileDstPath = Path(parentDstPath, fileSrcRelParentPath)

                        # TODO: repeat block with Exclude class. Make it concise
                        if fileDstPath.exists():
                            if (fileSrcPath.stat().st_mtime - fileDstPath.stat().st_mtime) > 0:
                                logger.info("    Backing up file: " + fileSrcPath.name)
                                # logger.debug("    Destination: " + str(fileDstPath))
                                shutil.copy2(fileSrcPath, fileDstPath)
                                self.backupCount = self.backupCount + 1
                            else:
                                logger.info("    Skip non-modified file: " + fileSrcPath.name)
                        else:
                            logger.info("    Backing up file: " + fileSrcPath.name)
                            os.makedirs(fileDstPath.parent, exist_ok=True)
                            # TODO: Support folder
                            shutil.copy2(fileSrcPath, fileDstPath)
                            self.backupCount = self.backupCount + 1

        logger.info("  Backed up {} {} files at\n".format(self.backupCount, self.name))


class Include():
    def __init__(self, name: str, globPatterns: list):
        self.name         = name
        self.globPatterns = globPatterns
        self.backupCount  = 0

    def copyFile(self, fileSrcPath, fileDstPath):
        if fileDstPath.exists():
            if (fileSrcPath.stat().st_mtime - fileDstPath.stat().st_mtime) > 0:
                logger.info("    Backing up file: " + fileSrcPath.name)
                # logger.debug("    Destination: " + str(fileDstPath))
                shutil.copy2(fileSrcPath, fileDstPath)
                self.backupCount = self.backupCount + 1
            else:
                logger.info("    Skip non-modified file: " + fileSrcPath.name)
        else:
            logger.info("    Backing up file: " + fileSrcPath.name)
            # logger.debug("    Destination: " + str(fileDstPath))
            os.makedirs(fileDstPath.parent, exist_ok=True)
            shutil.copy2(fileSrcPath, fileDstPath)
            self.backupCount = self.backupCount + 1

    # TODO: Skip non-modified files
    # TODO: dry run
    def copyTree(self, fileSrcPath, fileDstPath):
        logger.info("    Backing up directory: " + fileSrcPath.name)
        # logger.debug("    Destination: " + str(fileDstPath))
        os.makedirs(fileDstPath.parent, exist_ok=True)
        shutil.copytree(fileSrcPath, fileDstPath, dirs_exist_ok=True)
        count = sum(len(files) for _, _, files in os.walk(fileSrcPath))
        self.backupCount = self.backupCount + count


    def backup(self):
        logger.info("Backing up " + self.name + "...")

        for glob in self.globPatterns:
            # glob = [
                # userAppDataPath.glob("Local/Autodesk/3dsMax/*/*/*/UI/Workspaces"), <function object to extract version str from the globbed path>, <function object to filter file, preseve when evaluated to True>
            #]
            parentGlobPath        = glob[0]
            versionFindFunc       = glob[1] # type: Callable
            includeFileSrcRelStrs = glob[2] # type: list[str]

            # Deal with passing a single Path object as the parent Glob Path
            if not isinstance(parentGlobPath, GeneratorType):
                if not parentGlobPath.exists():
                    return

                fileSrcPath = parentGlobPath.resolve()
                fileSrcRelAnchorPath = fileSrcPath.relative_to(fileSrcPath.anchor)
                versionStr = versionFindFunc(fileSrcPath)
                logger.info("  Backing up {} {} inside: {}".format(self.name, versionStr, fileSrcPath))
                # NOTE: versionStr can be an empty string
                fileDstPath = Path(
                        destPath,
                        self.name,
                        versionStr,
                        fileSrcPath.anchor[:1],
                        fileSrcRelAnchorPath
                        )
                if fileSrcPath.is_dir():
                    self.copyTree(fileSrcPath, fileDstPath)
                else:
                    self.copyFile(fileSrcPath, fileDstPath)

                return logger.info("  Backed up {} {} {} files at: {}\n".format(self.backupCount, self.name, versionStr, fileSrcPath))


            parentSrcPaths = list(parentGlobPath) # type: list[Path]
            parentSrcPath  = None
            parentDstPath  = None
            fileSrcPaths   = None
            fileSrcPath    = None
            fileDstPath    = None


            # Skip if no glob pattern found
            if len(parentSrcPaths) == 0:
                continue

            for parentSrcPath in parentSrcPaths:
                # logger.debug("  parentSrcPath: " + str(parentSrcPath))
                versionStr = versionFindFunc(parentSrcPath)
                logger.info("  Backing up {} {} inside: {}".format(self.name, versionStr, parentSrcPath))

                parentSrcRelAnchorPath = parentSrcPath.relative_to(parentSrcPath.anchor)
                # NOTE: versionStr can be an empty string
                parentDstPath = Path(
                        destPath,
                        self.name,
                        versionStr,
                        parentSrcPath.anchor[:1],
                        parentSrcRelAnchorPath
                        )

                for fileSrcRelStr in includeFileSrcRelStrs:
                    if "*" in fileSrcRelStr:
                        # Relative string contrains wildcard character

                        fileSrcPaths = list(parentSrcPath.glob(fileSrcRelStr))

                        # Skip if no glob pattern found
                        if len(fileSrcPaths) == 0:
                            continue

                        for fileSrcPath in fileSrcPaths:
                            fileSrcRelParentPath = fileSrcPath.relative_to(parentSrcPath)
                            fileDstPath = Path(parentDstPath, fileSrcRelParentPath)
                            self.copyFile(fileSrcPath, fileDstPath)
                    else:
                        # Relative string doesn't contrains wildcard character,
                        # but its existance need to be validated
                        fileSrcPath = Path(parentSrcPath, fileSrcRelStr)
                        if fileSrcPath.exists():
                            fileSrcRelParentPath = fileSrcPath.relative_to(parentSrcPath)
                            fileDstPath = Path(parentDstPath, fileSrcRelParentPath)

                            if fileSrcPath.is_file():
                                self.copyFile(fileSrcPath, fileDstPath)
                            elif fileSrcPath.is_dir():
                                self.copyTree(fileSrcPath, fileDstPath)

        logger.info("  Backed up {} {} files\n".format(self.backupCount, self.name))

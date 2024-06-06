import shutil
import logging
import os
from pathlib import Path, PurePath
from icecream import ic
logger = logging.getLogger(__name__)


destPath    = PurePath(os.getcwd())
userName    = os.getlogin()
appDataPath = Path("C:/Users/{}/AppData".format(userName))
homePath    = Path("C:/Users/{}".format(userName))



class Backup:
    # TODO: type check https://stackoverflow.com/questions/2489669/how-do-python-functions-handle-the-types-of-parameters-that-you-pass-in
    def __init__(self, name: str, globPatterns: list):
        self.name         = name
        self.globPatterns = globPatterns
        self.backupCount  = 0

    def backup(self):
        logger.info("Backing up " + self.name + "...")

        for glob in self.globPatterns:
            # glob = [
                # userAppDataPath.glob("Local/Autodesk/3dsMax/*/*/*/UI/Workspaces"), <function object to extract version str from the globbed path>, <function object to filter file, preseve when evaluated to True>
            #]
            parentGlobPath, versionFindFunc, fileFilterFunc = glob
            parentSrcPaths = list(parentGlobPath)

            # Skip if no glob pattern found
            if len(parentSrcPaths) == 0:
                continue

            for parentSrcPath in parentSrcPaths:
                versionStr = versionFindFunc(parentSrcPath)
                logger.info("  Backing up {} {} inside: {}".format(self.name, versionStr, parentSrcPath))

                parentSrcRelAnchorPath = parentSrcPath.relative_to(parentSrcPath.anchor)
                # NOTE: versionStr can be an empty string
                parentDstPath = Path(destPath, self.name, versionStr, parentSrcRelAnchorPath)


                parentDestPathCreateChk = False
                for fileSrcPath in parentSrcPath.iterdir():
                    if fileFilterFunc(fileSrcPath):
                        # Create parent folder. Run only once
                        if not parentDestPathCreateChk:
                            if not parentDstPath.exists():
                                parentDstPath.mkdir(parents=True)

                            parentDestPathCreateChk = True

                        fileSrcRelParentPath = fileSrcPath.relative_to(parentSrcPath)
                        fileDstPath = Path(parentDstPath, fileSrcRelParentPath)
                        if fileSrcPath.stat().st_mtime - fileDstPath.stat().st_mtime:
                            logger.info("    Backing up file: " + fileSrcPath.name)
                            shutil.copy2(fileSrcPath, fileDstPath)
                            self.backupCount = self.backupCount + 1
                        else:
                            logger.info("    Skip non-modified file: " + fileSrcPath.name)

                logger.info("  Backed up {} {} {} files at: {}\n".format(self.backupCount, self.name, versionStr, parentSrcPath))

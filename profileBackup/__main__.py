import logging
import psutil
from backup import Backup, appDataPath, homePath
from icecream import ic

# TODO: adapt user name in new desitionation
# TODO: make commandline prompt to choose




def findRemovableDrive():
    # Credit: https://stackoverflow.com/questions/12266211/python-windows-list-only-usb-removable-drives
    externals = [i.mountpoint for i in psutil.disk_partitions() if "removable" in i.opts]



def main():
    threedsMax = Backup(
        "3ds Max",
        [
            [
                appDataPath.glob("Local/Autodesk/3dsMax/*/*/*/UI/Workspaces"),
                lambda parentSrcPath: parentSrcPath.parts[7][0: 4],
                lambda fileSrcPath: fileSrcPath.is_file() and not str.startswith(
                        fileSrcPath.name,
                        "Workspace"
                )
            ],
            [
                homePath.glob("Autodesk/3ds Max*/User Settings"),
                lambda parentSrcPath: parentSrcPath.parts[4][8: 12],
                lambda _: True
            ],
        ]
    )
    threedsMax.backup()


if __name__ == "__main__":
    ic.configureOutput(includeContext=True)
    logger = logging.getLogger(__name__)
    logging.basicConfig(format='[%(asctime)s] %(message)s', datefmt='%m/%d/%Y %I:%M:%S %p', level=logging.INFO)
    logger.info("Started")
    main()
    logger.info("Finished")

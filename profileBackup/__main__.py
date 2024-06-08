# File: profileBakup
# Author: iaso2h
# Description: Backup software profiles on Windows
# Version: 0.0.2
# Last Modified: 2024-06-07

import logging

from pathlib import Path
from backup import Backup, appDataPath, homePath
from icecream import ic

# TODO: adapt user name in new desitionation



def main() -> None:
    threedsMax = Backup(
        "3ds Max",
        [
            [
                appDataPath.glob("Local/Autodesk/3dsMax/*/*/*/UI/Workspaces"),
                lambda parentSrcPath: parentSrcPath.parts[7][:4],
                "exclude",
                lambda srcPath: srcPath.is_dir() or str.startswith(
                        srcPath.name,
                        "Workspace"
                )
            ],
            [
                homePath.glob("Autodesk/3ds Max*/User Settings"),
                lambda parentSrcPath: parentSrcPath.parts[4][-4:],
                "include",
                lambda _: True
            ],
        ]
    )
    autoCAD = Backup(
        "AutoCAD",
        [
            [
                appDataPath.glob("Roaming/Autodesk/AutoCAD*/*/*"),
                lambda parentSrcPath: parentSrcPath.parts[6][-4:],
                "include",
                [
                    "Plotters/Plot Styles/*.ctb",
                    "Plotters/Plot Styles/*.stb",
                    "Plotters/0___HQ.pc3",
                    "Support/acad.CUIX",
                    "Support/Profiles/TArch20*/Profile.aws",
                    "Support/Profiles/FixedProfile.aws"
                ]
            ],
            [
                "C:/ProgramData/IvySoft/YSTool/Freedom",
                "YSTool",
                "include",
                lambda _: True
            ],
            [
                "D:/Tangent/TArchT*",
                lambda parentSrcPath: parentSrcPath.parts[2][5:],
                "include",
                [
                    "SYS/*.lay",
                    "SYS/tangent.cuix",
                    "sys20x64/*.dwt",
                    "sys24x64/*.dwt"
                ]
            ],
            [
                "C:/贱人工具箱*",
                "贱人工具箱",
                "include",
                lambda _: True
            ],
            [
                "G:\AutoCAD",
                "Asset",
                "include",
                lambda _: True
            ],
        ]
    )

    threedsMax.backup()
    autoCAD.backup()


if __name__ == "__main__":
    ic.configureOutput(includeContext=True)
    logger = logging.getLogger(__name__)
    logging.basicConfig(format='[%(asctime)s] %(message)s', datefmt='%m/%d/%Y %I:%M:%S %p', level=logging.DEBUG)
    # logging.basicConfig(format='[%(asctime)s] %(message)s', datefmt='%m/%d/%Y %I:%M:%S %p', level=logging.INFO)


    logger.info("Started")
    main()
    logger.info("Finished")

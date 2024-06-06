import logging
import cli

from pathlib import Path
from backup import Exclude, Include, appDataPath, homePath
from icecream import ic

# TODO: adapt user name in new desitionation



def main():
    threedsMax = Exclude(
        "3ds Max",
        [
            [
                appDataPath.glob("Local/Autodesk/3dsMax/*/*/*/UI/Workspaces"),
                lambda parentSrcPath: parentSrcPath.parts[7][:4],
                lambda fileSrcPath: fileSrcPath.is_dir() or str.startswith(
                        fileSrcPath.name,
                        "Workspace"
                )
            ],
            [
                homePath.glob("Autodesk/3ds Max*/User Settings"),
                lambda parentSrcPath: parentSrcPath.parts[4][-4:],
                lambda _: True
            ],
        ]
    )
    autoCAD = Include(
        "AutoCAD",
        [
            [
                appDataPath.glob("Roaming/Autodesk/AutoCAD*/*/*"),
                lambda parentSrcPath: parentSrcPath.parts[6][-4:],
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
                Path("C:/ProgramData/IvySoft/YSTool/Freedom"),
                lambda _: "YSTool",
                [
                    "*",
                ]
            ],
            [
                Path("C:/ProgramData/IvySoft/YSTool/Freedom"),
                lambda _: "YSTool",
                [
                    "*",
                ]
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

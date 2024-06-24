from backup import Backup, DRYRUN, DESTPATH, appDataPath, homePath
import os


# Written from other file
softwareConfigs = [


    Backup(
        "3ds Max",
        [
            {
                "parentSrcPaths": appDataPath.glob("Local/Autodesk/3dsMax/*/*/*/UI/Workspaces"),
                "versionFind": lambda parentSrcPath: parentSrcPath.parts[7][:4],
                "includeType": "exclude",
                "filterPattern": lambda srcPath: srcPath.is_dir()
                    or str.startswith(srcPath.name, "Workspace"),
            },
            {
                "parentSrcPaths": homePath.glob("Autodesk/3ds Max*/User Settings"),
                "versionFind": lambda parentSrcPath: parentSrcPath.parts[4][-4:],
                "includeType": "include",
                "filterPattern": lambda _: True,
            },
        ],
    ),
    Backup(
        "Blender",
        [
            {
                "parentSrcPaths": appDataPath.glob(
                    "Roaming/Blender Foundation/Blender/*/scripts/presets/keyconfig"
                ),
                "versionFind": lambda parentSrcPath: parentSrcPath.parts[7],
                "includeType": "include",
                "filterPattern": [ "*.py", ],
            },
        ],
    ),
    Backup(
        "AutoCAD",
        [
            {
                "parentSrcPaths": appDataPath.glob("Roaming/Autodesk/AutoCAD*/*/*"),
                "versionFind": lambda parentSrcPath: parentSrcPath.parts[6][-4:],
                "includeType": "include",
                "filterPattern": [
                    "Plotters/Plot Styles/*.ctb",
                    "Plotters/Plot Styles/*.stb",
                    "Plotters/0___HQ.pc3",
                    "Support/acad.CUIX",
                    "Support/acadm.CUIX",
                    "Support/Profiles/TArch20*/Profile.aws",
                    "Support/Profiles/FixedProfile.aws",
                ],
            },
            {
                "parentSrcPaths":  "C:/ProgramData/IvySoft/YSTool/Freedom",
                "versionFind": "YSTool",
                "includeType": "include",
                "filterPattern": lambda _: True,
            },
            {
                "parentSrcPaths": "D:/Tangent/TArchT*",  # TODO create glob for string contains wild chracter
                "versionFind": lambda parentSrcPath: parentSrcPath.parts[2][5:],
                "includeType": "include",
                "filterPattern": ["SYS/*.lay", "SYS/tangent.cuix", "sys20x64/*.dwt", "sys24x64/*.dwt"],
            },
            {
                "parentSrcPaths": "D:/Asset/AutoCAD",
                "versionFind": "Asset",
                "includeType": "include",
                "filterPattern": lambda _: True
            },
        ],
    ),
    Backup(
        "Photoshop",
        [
            {
                "parentSrcPaths": appDataPath.glob(
                    "Roaming/Adobe/Adobe Photoshop */Adobe Photoshop * Settings"
                ),
                "versionFind": lambda parentSrcPath: parentSrcPath.parts[6][-4:],
                "includeType": "include",
                "filterPattern": [
                    "WorkSpaces*/*",
                    "Menu Customization.psp",
                    "Keyboard Shortcuts.psp",
                ],
            },
        ],
    ),
    Backup(
        "Everything",
        [
            {
                "parentSrcPaths": appDataPath.glob("Roaming/Everything"),
                "versionFind": "",
                "includeType": "include",
                "filterPattern": lambda _: True
            },
        ],
    ),
    Backup(
        "TubesT",
        [
            {
                "parentSrcPaths": appDataPath.glob("Roaming/Friendess/Tubest/*"),
                "versionFind": lambda parentSrcPath: parentSrcPath.parts[7],
                "includeType": "include",
                "filterPattern": lambda srcPath: srcPath.suffix.lower() == ".config",
            },
        ],
    ),
]


def start():

    if not DRYRUN:
        os.makedirs(str(DESTPATH), exist_ok=True)

    for i in softwareConfigs:
          if i.ticked:
              i.backup()

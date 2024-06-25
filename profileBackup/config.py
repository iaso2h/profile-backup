from backup import Backup, appDataPath, homePath


# Written from other file
softwareConfigs = [


    Backup(
        "3ds Max",
        [
            {
                "parentSrcPath": appDataPath.glob("Local/Autodesk/3dsMax/*/*/*/UI/Workspaces"),
                "versionFind": lambda parentSrcPath: parentSrcPath.parts[7][:4],
                "includeType": "exclude",
                "filterPattern": lambda srcPath: srcPath.is_dir()
                    or str.startswith(srcPath.name, "Workspace"),
                "recursiveCopy": True
            },
            {
                "parentSrcPath": homePath.glob("Autodesk/3ds Max*/User Settings"),
                "versionFind": lambda parentSrcPath: parentSrcPath.parts[4][-4:],
                "includeType": "include",
                "filterPattern": lambda _: True,
                "recursiveCopy": True
            },
        ],
    ),
    Backup(
        "Blender",
        [
            {
                "parentSrcPath": appDataPath.glob(
                    "Roaming/Blender Foundation/Blender/*/scripts/presets/keyconfig"
                ),
                "versionFind": lambda parentSrcPath: parentSrcPath.parts[7],
                "includeType": "include",
                "filterPattern": [ "*.py", ],
                "recursiveCopy": True
            },
        ],
    ),
    Backup(
        "AutoCAD",
        [
            {
                "parentSrcPath": appDataPath.glob("Roaming/Autodesk/AutoCAD*/*/*"),
                "versionFind": lambda parentSrcPath: parentSrcPath.parts[6][-4:],
                "includeType": "include",
                "filterPattern": lambda srcPath: srcPath.name.lower() not in [
                    "acad.ctb",
                    "acad.stb",
                    "autodesk-color.stb",
                    "autodesk-mono.stb",
                    "dwf virtual pens.ctb",
                    "fill patterns.ctb",
                    "grayscale.ctb",
                    "monochrome.ctb",
                    "monochrome.stb",
                    "screening 100%.ctb",
                    "screening 25%.ctb",
                    "screening 50%.ctb",
                    "screening 75%.ctb",
                    ] and (srcPath.name.lower() in [
                        "0___hq.pc3",
                        "acad.cuix",
                        "acadm.cuix",
                        "profile.aws",
                        "fixedprofile.aws",
                        ] or srcPath.suffix == ".ctb" or srcPath.suffix == ".stb"),
                "recursiveCopy": True
            },
            {
                "parentSrcPath":  "C:/ProgramData/IvySoft/YSTool/Freedom",
                "versionFind": "YSTool",
                "includeType": "include",
                "filterPattern": lambda _: True,
                "recursiveCopy": True
            },
            {
                "parentSrcPath": "D:/Tangent/TArchT*",
                "versionFind": lambda parentSrcPath: parentSrcPath.parts[2][5:],
                "includeType": "include",
                "filterPattern": ["SYS/*.lay", "SYS/tangent.cuix", "sys20x64/*.dwt", "sys24x64/*.dwt"],
                "recursiveCopy": True
            },
            {
                "parentSrcPath": "D:/Asset/AutoCAD",
                "versionFind": "Asset",
                "includeType": "include",
                "filterPattern": lambda _: True,
                "recursiveCopy": True
            },
        ],
    ),
    Backup(
        "Photoshop",
        [
            {
                "parentSrcPath": appDataPath.glob(
                    "Roaming/Adobe/Adobe Photoshop */Adobe Photoshop * Settings"
                ),
                "versionFind": lambda parentSrcPath: parentSrcPath.parts[6][-4:],
                "includeType": "include",
                "filterPattern": [
                    "WorkSpaces*/*",
                    "Menu Customization.psp",
                    "Keyboard Shortcuts.psp",
                ],
                "recursiveCopy": True
            },
        ],
    ),
    Backup(
        "Everything",
        [
            {
                "parentSrcPath": appDataPath.glob("Roaming/Everything"),
                "versionFind": "",
                "includeType": "include",
                "filterPattern": lambda _: True,
                "recursiveCopy": True
            },
        ],
    ),
    Backup(
        "TubesT",
        [
            {
                "parentSrcPath": appDataPath.glob("Roaming/Friendess/Tubest/*"),
                "versionFind": lambda parentSrcPath: parentSrcPath.parts[7],
                "includeType": "include",
                "filterPattern": lambda srcPath: srcPath.suffix.lower() == ".config",
                "recursiveCopy": True
            },
        ],
    ),
]

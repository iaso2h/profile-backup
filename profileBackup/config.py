from backup import Backup, appDataPath, homePath
from pathlib import Path
import winreg as wrg


# Written from other file
softwareConfigs = [
    Backup(
        {
            "name": "demoBackup",
            "enabled": False,
            "globPatterns": [
                {
                    # 1. Path object
                    # 2. Path glob generator
                    # 3. string contains parent source directory(support */** wildcard character)
                    "parentSrcPath": Path(Path("__file__").resolve().parent, "profileBackup", "tests", "test_backup_src_files"),
                    # 1. function with `parentSrcPath` as parameter
                    # 2. string
                    "versionFind": lambda parentSrcPath: parentSrcPath.parts[7][:4],
                    # 1. string(either `"exclude` or `"include`)
                    "filterType": "exclude",
                    # 1. function with `parentSrcPath` as parameter, return boolean
                    # 2. list contains string(support */** wildcard character)
                    "filterPattern": lambda srcPath: srcPath.is_dir()
                        or str.startswith(srcPath.name, "Workspace"),
                    # 1. boolean
                    "recursiveCopy": True,
                    # 1. boolean
                    "silentReport": False
                },
                {
                    # Other version branch...
                }
            ],
        }
    ),
    Backup(
        {
            "name": "3ds Max",
            "enabled": True,
            "globPatterns": [
                {
                    "parentSrcPath": appDataPath.glob("Local/Autodesk/3dsMax/*/*/*/UI/Workspaces"),
                    "versionFind": lambda parentSrcPath: parentSrcPath.parts[7][:4],
                    "filterType": "exclude",
                    "filterPattern": lambda srcPath: srcPath.is_dir()
                        or str.startswith(srcPath.name, "Workspace"),
                    "recursiveCopy": True,
                    "silentReport": False
                },
                {
                    "parentSrcPath": homePath.glob("Autodesk/3ds Max*/User Settings"),
                    "versionFind": lambda parentSrcPath: parentSrcPath.parts[4][-4:],
                    "filterType": "include",
                    "filterPattern": lambda _: True,
                    "recursiveCopy": True,
                    "silentReport": False
                },
            ],
        }
    ),
    Backup(
        {
            "name": "SOLIDWORKS",
            "enabled": True,
            "globPatterns": [
                {
                    "parentSrcPath": "D:/Asset/SOLIDWORKS",
                    "versionFind": "Asset",
                    "filterType": "include",
                    "filterPattern": lambda _: True,
                    "recursiveCopy": True,
                    "silentReport": True
                },
                {
                    "parentSrcPath": "D:/**/SOLIDWORKS Corp/SOLIDWORKS/lang/*",
                    "versionFind": lambda _: wrg.QueryValueEx(
                            wrg.OpenKey(
                                wrg.HKEY_LOCAL_MACHINE,
                                "SOFTWARE\\SolidWorks\\IM"
                                ),
                            "IMSchedulerVersion"
                        )[0][:4],
                    "filterType": "include",
                    "filterPattern": [
                        "calloutformat.txt",
                        "calloutformat_2.txt",
                    ],
                    "recursiveCopy": True,
                    "silentReport": False
                },
                {
                    "parentSrcPath": "D:/**/SOLIDWORKS Corp/SOLIDWORKS/data",
                    "versionFind": lambda _: wrg.QueryValueEx(
                            wrg.OpenKey(
                                wrg.HKEY_LOCAL_MACHINE,
                                "SOFTWARE\\SolidWorks\\IM"
                                ),
                            "IMSchedulerVersion"
                        )[0][:4],
                    "filterType": "include",
                    "filterPattern": [
                        "ttfontratiomap.txt",
                        "drawfontmap.txt",
                    ],
                    "recursiveCopy": True,
                    "silentReport": False
                },
            ]
        }
    ),
    Backup(
        {
            "name": "Blender",
            "enabled": True,
            "globPatterns": [
                {
                    "parentSrcPath": appDataPath.glob(
                        "Roaming/Blender Foundation/Blender/*/scripts/presets/keyconfig"
                    ),
                    "versionFind": lambda parentSrcPath: parentSrcPath.parts[7],
                    "filterType": "include",
                    "filterPattern": [ "*.py", ],
                    "recursiveCopy": True,
                    "silentReport": False
                },
            ],
        }
    ),
    Backup(
        {
            "name": "AutoCAD",
            "enabled": True,
            "globPatterns": [
                {
                    "parentSrcPath": appDataPath.glob("Roaming/Autodesk/AutoCAD*/*/*"),
                    "versionFind": lambda parentSrcPath: parentSrcPath.parts[6][-4:],
                    "filterType": "include",
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
                    "recursiveCopy": True,
                    "silentReport": False
                },
                {
                    "parentSrcPath":  "C:/ProgramData/IvySoft/YSTool/Freedom",
                    "versionFind": "YSTool",
                    "filterType": "include",
                    "filterPattern": lambda _: True,
                    "recursiveCopy": True,
                    "silentReport": False
                },
                {
                    "parentSrcPath": "D:/Program Files/Tangent/TArchT20*",
                    "versionFind": lambda parentSrcPath: parentSrcPath.parts[2][5:],
                    "filterType": "include",
                    "filterPattern": ["SYS/*.lay", "SYS/tangent.cuix", "sys20x64/*.dwt", "sys24x64/*.dwt", "sys24x64/Tch.tmn"],
                    "recursiveCopy": True,
                    "silentReport": False
                },
                {
                    "parentSrcPath": "F:/Asset/AutoCAD",
                    "versionFind": "Asset",
                    "filterType": "include",
                    "filterPattern": lambda _: True,
                    "recursiveCopy": True,
                    "silentReport": False
                },
            ],
        }
    ),
    Backup(
        {
            "name": "Photoshop",
            "enabled": True,
            "globPatterns": [
                {
                    "parentSrcPath": appDataPath.glob(
                        "Roaming/Adobe/Adobe Photoshop */Adobe Photoshop * Settings"
                    ),
                    "versionFind": lambda parentSrcPath: parentSrcPath.parts[6][-4:],
                    "filterType": "include",
                    "filterPattern": [
                        "WorkSpaces*/*",
                        "Menu Customization.psp",
                        "Keyboard Shortcuts.psp",
                    ],
                    "recursiveCopy": True,
                    "silentReport": False
                },
            ],
        }
    ),
    Backup(
        {
            "name": "Everything",
            "enabled": True,
            "globPatterns": [
                {
                    "parentSrcPath": appDataPath.glob("Roaming/Everything"),
                    "versionFind": "",
                    "filterType": "include",
                    "filterPattern": lambda _: True,
                    "recursiveCopy": True,
                    "silentReport": False
                },
            ],
        }
    ),
    Backup(
        {
            "name": "TubesT",
            "enabled": True,
            "globPatterns": [
                {
                    "parentSrcPath": appDataPath.glob("Roaming/Friendess/Tubest/*"),
                    "versionFind": lambda parentSrcPath: parentSrcPath.parts[7],
                    "filterType": "include",
                    "filterPattern": lambda srcPath: srcPath.suffix.lower() == ".config",
                    "recursiveCopy": True,
                    "silentReport": False
                },
            ]
        }
    ),
]

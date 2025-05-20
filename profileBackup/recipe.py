from backup import Profile, Category, appDataPath, homePath
from pathlib import Path
import winreg as wrg

Profile(
    profileName="demoBackup",
    enabled=False,
    categories=[
        Category(
            profileName="demoBackup",
            categoryName="Main",
            # 1. function with `parentSrcPath` as parameter
            # 2. string
            versionFind=lambda parentSrcPath: parentSrcPath.parts[7][:4],
            enabled=False,
            recursiveCopy=True,
            silentReport=False,
            # 1. Path object
            # 2. Path glob generator
            # 3. string contains parent source directory(support */** wildcard character)
            parentSrcPaths=Path(Path("__file__").resolve().parent, "profileBackup", "tests", "test_backup_src_files"),
            # 1. "exclude"
            # 2. "include"
            filterType="exclude",
            # 1. function with `parentSrcPath` as parameter, return boolean
            # 2. list contains string(support */** wildcard character)
            filterPattern=lambda srcPath: srcPath.is_dir()
                or str.startswith(srcPath.name, "Workspace"),
        ),
    ]
)
Profile(
    profileName="3ds Max",
    enabled=True,
    categories=[
        Category(
            profileName="3ds Max",
            categoryName="Workspace",
            versionFind=lambda parentSrcPath: parentSrcPath.parts[7][:4],
            enabled=True,
            recursiveCopy=True,
            silentReport=False,
            parentSrcPaths=appDataPath.glob("Local/Autodesk/3dsMax/*/*/*/UI/Workspaces"),
            filterType="exclude",
            filterPattern=lambda srcPath: srcPath.is_dir()
                or str.startswith(srcPath.name, "Workspace"),
        ),
        Category(
            profileName="3ds Max",
            categoryName="User Settings",
            versionFind=lambda parentSrcPath: parentSrcPath.parts[4][-4:],
            enabled=True,
            recursiveCopy=True,
            silentReport=False,
            parentSrcPaths=homePath.glob("Autodesk/3ds Max*/User Settings"),
            filterType="include",
            filterPattern=lambda _: True,
        ),
    ]
)
Profile(
    profileName="SOLIDWORKS",
    enabled=True,
    categories=[
        Category(
            profileName="SOLIDWORKS",
            categoryName="Asset",
            versionFind="Generic",
            enabled=True,
            recursiveCopy=True,
            silentReport=True,
            parentSrcPaths="D:/Asset/SOLIDWORKS",
            filterType="include",
            filterPattern=lambda _: True,
        ),
        Category(
            profileName="SOLIDWORKS",
            categoryName="Language Configs",
            versionFind=lambda _: wrg.QueryValueEx(
                wrg.OpenKey(
                    wrg.HKEY_LOCAL_MACHINE,
                    "SOFTWARE\\SolidWorks\\IM"
                    ),
                "IMSchedulerVersion"
                )[0][:4],
            enabled=True,
            recursiveCopy=True,
            silentReport=True,
            parentSrcPaths="D:/**/SOLIDWORKS Corp/SOLIDWORKS/lang/*",
            filterType="include",
            filterPattern=[
                "calloutformat.txt",
                "calloutformat_2.txt",
            ],
        ),
        Category(
            profileName="SOLIDWORKS",
            categoryName="Data",
            versionFind=lambda _: wrg.QueryValueEx(
                    wrg.OpenKey(
                        wrg.HKEY_LOCAL_MACHINE,
                        "SOFTWARE\\SolidWorks\\IM"
                        ),
                    "IMSchedulerVersion"
                    )[0][:4],
            enabled=True,
            recursiveCopy=True,
            silentReport=True,
            parentSrcPaths="D:/**/SOLIDWORKS Corp/SOLIDWORKS/data",
            filterType="include",
            filterPattern=[
                "ttfontratiomap.txt",
                "drawfontmap.txt",
            ],
        ),
    ]
)
Profile(
    profileName="Blender",
    enabled=True,
    categories=[
        Category(
            profileName="Blender",
            categoryName="Main",
            versionFind=lambda parentSrcPath: parentSrcPath.parts[7],
            enabled=True,
            recursiveCopy=True,
            silentReport=False,
            parentSrcPaths=appDataPath.glob(
                "Roaming/Blender Foundation/Blender/*/scripts/presets/keyconfig"
                ),
            filterType="include",
            filterPattern=[ "*.py", ],
        ),
    ]
)
Profile(
    profileName="AutoCAD",
    enabled=True,
    categories=[
        Category(
            profileName="AutoCAD",
            categoryName="Plot",
            versionFind=lambda parentSrcPath: parentSrcPath.parts[6][-4:],
            enabled=True,
            recursiveCopy=True,
            silentReport=False,
            parentSrcPaths=appDataPath.glob("Roaming/Autodesk/AutoCAD*/*/*"),
            filterType="include",
            filterPattern=lambda srcPath: srcPath.name.lower() not in [
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
                    ] or srcPath.suffix == ".ctb" or srcPath.suffix == ".stb")
        ),
        Category(
            profileName="AutoCAD",
            categoryName="YSTool",
            versionFind="Generic",
            enabled=True,
            recursiveCopy=True,
            silentReport=False,
            parentSrcPaths= "C:/ProgramData/IvySoft/YSTool/Freedom",
            filterType="include",
            filterPattern=lambda _: True,
        ),
        Category(
            profileName="AutoCAD",
            categoryName="Tangent",
            versionFind=lambda parentSrcPath: parentSrcPath.parts[2][5:],
            enabled=True,
            recursiveCopy=True,
            silentReport=False,
            parentSrcPaths="D:/Program Files/Tangent/TArchT20*",
            filterType="include",
            filterPattern=["SYS/*.lay", "SYS/tangent.cuix", "sys20x64/*.dwt", "sys24x64/*.dwt", "sys24x64/Tch.tmn"],
        ),
        Category(
            profileName="AutoCAD",
            categoryName="Asset",
            versionFind="Generic",
            enabled=True,
            recursiveCopy=True,
            silentReport=False,
            parentSrcPaths="F:/Asset/AutoCAD",
            filterType="include",
            filterPattern=lambda _: True,
        ),
    ]
)
Profile(
    profileName="Photoshop",
    enabled=True,
    categories=[
        Category(
            profileName="Photoshop",
            categoryName="Main",
            versionFind=lambda parentSrcPath: parentSrcPath.parts[6][-4:],
            enabled=True,
            recursiveCopy=True,
            silentReport=False,
            parentSrcPaths=appDataPath.glob(
                "Roaming/Adobe/Adobe Photoshop */Adobe Photoshop * Settings"
                ),
            filterType="include",
            filterPattern=[
                "WorkSpaces*/*",
                "Menu Customization.psp",
                "Keyboard Shortcuts.psp",
                ],
        ),
    ]
)
Profile(
    profileName="Everything",
    enabled=True,
    categories=[
        Category(
            profileName="Everything",
            categoryName="Main",
            versionFind="Generic",
            enabled=True,
            recursiveCopy=True,
            silentReport=False,
            parentSrcPaths=appDataPath.glob("Roaming/Everything"),
            filterType="include",
            filterPattern=lambda _: True,
        ),
    ]
)
Profile(
    profileName="TubesT",
    enabled=True,
    categories=[
        Category(
            profileName="TubesT",
            categoryName="Main",
            versionFind=lambda parentSrcPath: parentSrcPath.parts[7],
            enabled=True,
            recursiveCopy=True,
            silentReport=False,
            parentSrcPaths=appDataPath.glob("Roaming/Friendess/Tubest/*"),
            filterType="include",
            filterPattern=lambda srcPath: srcPath.suffix.lower() == ".config",
        ),
    ]
)

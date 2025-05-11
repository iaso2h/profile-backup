from backup import Profile, appDataPath, homePath
from pathlib import Path
import winreg as wrg


# Written from other file
profileConfigs = [
    Profile(
        name="demoBackup",
        categoryName="Main",
        enabled=False,
        recursiveCopy=True,
        silentReport=False,
        # 1. Path object
        # 2. Path glob generator
        # 3. string contains parent source directory(support */** wildcard character)
        parentSrcPath=Path(Path("__file__").resolve().parent, "profileBackup", "tests", "test_backup_src_files"),
        # 1. function with `parentSrcPath` as parameter
        # 2. string
        versionFind=lambda parentSrcPath: parentSrcPath.parts[7][:4],
        # 1. "exclude"
        # 2. "include"
        filterType="exclude",
        # 1. function with `parentSrcPath` as parameter, return boolean
        # 2. list contains string(support */** wildcard character)
        filterPattern=lambda srcPath: srcPath.is_dir()
            or str.startswith(srcPath.name, "Workspace"),
    ),
    Profile(
        name="3ds Max",
        categoryName="Workspace",
        enabled=True,
        recursiveCopy=True,
        silentReport=False,
        parentSrcPath=appDataPath.glob("Local/Autodesk/3dsMax/*/*/*/UI/Workspaces"),
        versionFind=lambda parentSrcPath: parentSrcPath.parts[7][:4],
        filterType="exclude",
        filterPattern=lambda srcPath: srcPath.is_dir()
            or str.startswith(srcPath.name, "Workspace"),
    ),
    Profile(
        name="3ds Max",
        categoryName="User Settings",
        enabled=True,
        recursiveCopy=True,
        silentReport=False,
        parentSrcPath=homePath.glob("Autodesk/3ds Max*/User Settings"),
        versionFind=lambda parentSrcPath: parentSrcPath.parts[4][-4:],
        filterType="include",
        filterPattern=lambda _: True,
    ),
    Profile(
        name="SOLIDWORKS",
        categoryName="Asset",
        enabled=True,
        recursiveCopy=True,
        silentReport=True,
        parentSrcPath="D:/Asset/SOLIDWORKS",
        versionFind="Asset",
        filterType="include",
        filterPattern=lambda _: True,
    )
    Profile(
        name="SOLIDWORKS",
        categoryName="Language Configs",
        enabled=True,
        recursiveCopy=True,
        silentReport=True,
        parentSrcPath="D:/**/SOLIDWORKS Corp/SOLIDWORKS/lang/*",
        versionFind=lambda _: wrg.QueryValueEx(
            wrg.OpenKey(
                wrg.HKEY_LOCAL_MACHINE,
                "SOFTWARE\\SolidWorks\\IM"
                ),
            "IMSchedulerVersion"
            )[0][:4],
        filterType="include",
        filterPattern=[
            "calloutformat.txt",
            "calloutformat_2.txt",
            ],
    ),
    Profile(
        name="SOLIDWORKS",
        categoryName="Data",
        enabled=True,
        recursiveCopy=True,
        silentReport=True,
        parentSrcPath="D:/**/SOLIDWORKS Corp/SOLIDWORKS/data",
        versionFind=lambda _: wrg.QueryValueEx(
                wrg.OpenKey(
                    wrg.HKEY_LOCAL_MACHINE,
                    "SOFTWARE\\SolidWorks\\IM"
                    ),
                "IMSchedulerVersion"
                )[0][:4],
        filterType="include",
        filterPattern=[
            "ttfontratiomap.txt",
            "drawfontmap.txt",
        ],
    ),
    Profile(
        name="Blender",
        categoryName="Main",
        enabled=True,
        recursiveCopy=True,
        silentReport=False,
        parentSrcPath=appDataPath.glob(
            "Roaming/Blender Foundation/Blender/*/scripts/presets/keyconfig"
            ),
        versionFind=lambda parentSrcPath: parentSrcPath.parts[7],
        filterType="include",
        filterPattern=[ "*.py", ],
    ),
    Profile(
        name="AutoCAD",
        categoryName="Plot",
        enabled=True,
        recursiveCopy=True,
        silentReport=False,
        parentSrcPath=appDataPath.glob("Roaming/Autodesk/AutoCAD*/*/*"),
        versionFind=lambda parentSrcPath: parentSrcPath.parts[6][-4:],
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
    )
    Profile(
        name="AutoCAD",
        categoryName="YSTool",
        enabled=True,
        recursiveCopy=True,
        silentReport=False,
        parentSrcPath= "C:/ProgramData/IvySoft/YSTool/Freedom",
        versionFind="YSTool",
        filterType="include",
        filterPattern=lambda _: True,
    )
    Profile(
        name="AutoCAD",
        categoryName="Tangent",
        enabled=True,
        recursiveCopy=True,
        silentReport=False,
        parentSrcPath="D:/Program Files/Tangent/TArchT20*",
        versionFind=lambda parentSrcPath: parentSrcPath.parts[2][5:],
        filterType="include",
        filterPattern=["SYS/*.lay", "SYS/tangent.cuix", "sys20x64/*.dwt", "sys24x64/*.dwt", "sys24x64/Tch.tmn"],
    )
    Profile(
        name="AutoCAD",
        categoryName="Asset",
        enabled=True,
        recursiveCopy=True,
        silentReport=False,
        parentSrcPath="F:/Asset/AutoCAD",
        versionFind="Asset",
        filterType="include",
        filterPattern=lambda _: True,
    )
    Profile(
        name="Photoshop",
        categoryName="Main",
        enabled=True,
        recursiveCopy=True,
        silentReport=False,
        parentSrcPath=appDataPath.glob(
            "Roaming/Adobe/Adobe Photoshop */Adobe Photoshop * Settings"
            ),
        versionFind=lambda parentSrcPath: parentSrcPath.parts[6][-4:],
        filterType="include",
        filterPattern=[
            "WorkSpaces*/*",
            "Menu Customization.psp",
            "Keyboard Shortcuts.psp",
            ],
    ),
    Profile(
        name="Everything",
        categoryName="Main",
        enabled=True,
        recursiveCopy=True,
        silentReport=False,
        parentSrcPath=appDataPath.glob("Roaming/Everything"),
        versionFind="",
        filterType="include",
        filterPattern=lambda _: True,
    ),
    Profile(
        name="TubesT",
        categoryName="Main",
        enabled=True,
        recursiveCopy=True,
        silentReport=False,
        parentSrcPath=appDataPath.glob("Roaming/Friendess/Tubest/*"),
        versionFind=lambda parentSrcPath: parentSrcPath.parts[7],
        filterType="include",
        filterPattern=lambda srcPath: srcPath.suffix.lower() == ".config",
    ),
]

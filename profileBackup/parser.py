from backup import Backup, DRYRUN, DESTPATH, appDataPath, homePath
import os


# Written from other file
softwareChoice = None # type: list
softwareList = [



    Backup(
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
    ),
    Backup(
        "Blender",
        [
            [
                appDataPath.glob("Roaming/Blender Foundation/Blender/*/scripts/presets/keyconfig"),
                lambda parentSrcPath: parentSrcPath.parts[7],
                "include",
                [
                    "*.py",
                ]
            ],
        ]
    ),
    Backup(
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
                "D:/Tangent/TArchT*", # TODO
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
                "D:/Asset/AutoCAD/AutoCAD LISP",
                "Asset",
                "include",
                lambda _: True
            ],
        ]
    ),
    Backup(
        "Photoshop",
        [
            [
                appDataPath.glob("Roaming/Adobe/Adobe Photoshop */Adobe Photoshop * Settings"),
                lambda parentSrcPath: parentSrcPath.parts[6][-4:],
                "include",
                [
                    "WorkSpaces*/*",
                    "Menu Customization.psp",
                    "Keyboard Shortcuts.psp",
                ]
            ],
        ]
    ),
    Backup(
        "Everything",
        [
            [
                appDataPath.glob("Roaming/Everything"),
                "",
                "include",
                lambda _: True
            ],
        ]
    ),
    Backup(
        "TubesT",
        [
            [
                appDataPath.glob("Roaming/Friendess/Tubest/*"),
                lambda parentSrcPath: parentSrcPath.parts[7],
                "include",
                lambda srcPath: srcPath.suffix.lower() == ".config",
            ],
        ]
    ),


]

def start():

    if not DRYRUN:
        os.makedirs(str(DESTPATH), exist_ok=True)
        
    for i in softwareChoice:
        softwareList[i].backup()

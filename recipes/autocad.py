import os
from backup import Profile
from pathlib import Path
appDataPath = Path(os.getenv('APPDATA')).parent

Profile(
    profileName="AutoCAD",
    enabled=True,
    categories=[
        {
            "type": "file",
            "categoryName": "Plot",
            "versionFind": lambda parentSrcPath: parentSrcPath.parts[6][-4:],
            "enabled": True,
            "recursiveCopy": True,
            "silentReport": False,
            "parentSrcPaths": appDataPath.glob("Roaming/Autodesk/AutoCAD*/*/*"),
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
                    ] or srcPath.suffix == ".ctb" or srcPath.suffix == ".stb")
        },
        {
            "type": "file",
            "categoryName": "YSTool",
            "versionFind": "Generic",
            "enabled": True,
            "recursiveCopy": True,
            "silentReport": False,
            "parentSrcPaths":  "C:/ProgramData/IvySoft/YSTool/Freedom",
            "filterType": "include",
            "filterPattern": lambda _: True,
        },
        {
            "type": "file",
            "categoryName": "Tangent",
            "versionFind": lambda parentSrcPath: parentSrcPath.parts[2][5:],
            "enabled": True,
            "recursiveCopy": True,
            "silentReport": False,
            "parentSrcPaths": "D:/Program Files/Tangent/TArchT20*",
            "filterType": "include",
            "filterPattern": ["SYS/*.lay", "SYS/tangent.cuix", "sys20x64/*.dwt", "sys24x64/*.dwt", "sys24x64/Tch.tmn"],
        },
        {
            "type": "file",
            "categoryName": "Asset",
            "versionFind": "Generic",
            "enabled": True,
            "recursiveCopy": True,
            "silentReport": False,
            "parentSrcPaths": "F:/Asset/AutoCAD",
            "filterType": "include",
            "filterPattern": lambda _: True,
        },
    ]
)

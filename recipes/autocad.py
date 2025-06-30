import os
from pathlib import Path

import util
from profileBackup.backup import Profile

versionDict = [ # {{{
    {
        "releaseYear": 2025,
        "internalRNumber": "R25.0",
        "note": 'The latest release as of your current query. Files saved in 2025 cannot be opened by older versions without using "Save As" to an earlier format.',
    },
    {
        "releaseYear": 2024,
        "internalRNumber": "R24.3",
        "note": "Generally compatible with 2018-2023 files for reading/writing. Introduced minor updates to the DWG format.",
    },
    {
        "releaseYear": 2023,
        "internalRNumber": "R24.2",
        "note": "Part of the 2018-2024 DWG format family. Significant performance improvements and new features.",
    },
    {
        "releaseYear": 2022,
        "internalRNumber": "R24.1",
        "note": "Continues to use the AC1038 format. Focused on automation, collaboration, and connectivity.",
    },
    {
        "releaseYear": 2021,
        "internalRNumber": "R24.0",
        "note": "The first release in the R24.x series. This version and subsequent ones (up to 2024) primarily use the same core DWG format, AC1038.",
    },
    {
        "releaseYear": 2020,
        "internalRNumber": "R23.1",
        "note": "Minor update to the 2018 format.",
    },
    {
        "releaseYear": 2019,
        "internalRNumber": "R23.0",
        "note": "Introduced features like DWG Compare.",
    },
    {
        "releaseYear": 2018,
        "internalRNumber": "R22.0",
        "note": "**Major DWG file format change (AC1032).** Files saved in 2018 or later cannot be opened by versions older than 2018 without conversion. This format is still widely used and supported.",
    },
    {
        "releaseYear": 2017,
        "internalRNumber": "R21.0",
        "note": "Part of the 2013-2017 DWG format family.",
    },
    {"releaseYear": 2016, "internalRNumber": "R20.1", "note": ""},
    {"releaseYear": 2015, "internalRNumber": "R20.0", "note": ""},
    {"releaseYear": 2014, "internalRNumber": "R19.1", "note": ""},
    {
        "releaseYear": 2013,
        "internalRNumber": "R19.0",
        "note": "**Major DWG file format change (AC1027).** Files saved in 2013 or later cannot be opened by versions older than 2013 without conversion.",
    },
    {
        "releaseYear": 2012,
        "internalRNumber": "R18.2",
        "note": "Part of the 2010-2012 DWG format family.",
    },
    {"releaseYear": 2011, "internalRNumber": "R18.1", "note": ""},
    {
        "releaseYear": 2010,
        "internalRNumber": "R18.0",
        "note": "**Major DWG file format change (AC1024).**",
    },
    {
        "releaseYear": 2009,
        "internalRNumber": "R17.2",
        "note": "Part of the 2007-2009 DWG format family.",
    },
    {"releaseYear": 2008, "internalRNumber": "R17.1", "note": ""},
    {
        "releaseYear": 2007,
        "internalRNumber": "R17.0",
        "note": "**Major DWG file format change (AC1021).**",
    },
    {
        "releaseYear": 2006,
        "internalRNumber": "R16.2",
        "note": "Part of the 2004-2006 DWG format family.",
    },
    {"releaseYear": 2005, "internalRNumber": "R16.1", "note": ""},
    {
        "releaseYear": 2004,
        "internalRNumber": "R16.0",
        "note": "**Major DWG file format change (AC1018).**",
    },
    {
        "releaseYear": 2002,
        "internalRNumber": "R15.0a",
        "note": "Update to the 2000 format.",
    },
    {
        "releaseYear": "2000i",
        "internalRNumber": "R15.0a",
        "note": "Internet-focused version.",
    },
    {
        "releaseYear": 2000,
        "internalRNumber": "R15.0",
        "note": "**Major DWG file format change (AC1015).** Often considered a turning point for modern AutoCAD.",
    },
    {
        "releaseYear": "R14",
        "internalRNumber": "R14.0",
        "note": 'The last release before the "year-based" naming convention.',
    },
    {"releaseYear": "R13", "internalRNumber": "R13.0", "note": ""},
    {"releaseYear": "R12", "internalRNumber": "R12.0", "note": ""},
    {"releaseYear": "R11", "internalRNumber": "R11.0", "note": ""},
    {"releaseYear": "R10", "internalRNumber": "R10.0", "note": ""},
    {"releaseYear": "R9", "internalRNumber": "R9.0", "note": ""},
    {"releaseYear": "R2.6", "internalRNumber": "R2.6", "note": ""},
    {"releaseYear": "R2.5", "internalRNumber": "R2.5", "note": ""},
    {"releaseYear": "R2.1", "internalRNumber": "R2.1", "note": ""},
    {
        "releaseYear": "R1.0",
        "internalRNumber": "R1.0",
        "note": "The very first release.",
    },
] # }}}
def internalRNumberToYear(internalRNumber): # {{{
    for versionInfo in versionDict:
        if versionInfo["internalRNumber"] == internalRNumber:
            return versionInfo["releaseYear"]
    return ""
# }}}
appDataPath = Path(os.getenv("APPDATA")).parent # type: ignore
installPathStr, enabledChk = util.regQueryData(r"HKEY_LOCAL_MACHINE/SOFTWARE/SolidWorks/SOLIDWORKS \d+/Setup", "SolidWorks Folder")
Profile(
    profileName="AutoCAD",
    enabled=True,
    categories=[
        {
            "type": "file",
            "categoryName": "Plot",
            "enabled": True,
            "recursiveCopy": True,
            "silentReport": False,
            "parentSrcPaths": appDataPath.glob("Roaming/Autodesk/AutoCAD*/*/*"),
            "filterType": "include",
            "filterPattern": lambda srcPath: srcPath.name.lower()
            not in [
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
            ]
            and (
                srcPath.name.lower()
                in [
                    "0___hq.pc3",
                    "acad.cuix",
                    "acadm.cuix",
                    "profile.aws",
                    "fixedprofile.aws",
                ]
                or srcPath.suffix == ".ctb"
                or srcPath.suffix == ".stb"
            ),
        },
        {
            "type": "file",
            "categoryName": "YSTool",
            "enabled": True,
            "recursiveCopy": True,
            "silentReport": False,
            "parentSrcPaths": "C:/ProgramData/IvySoft/YSTool/Freedom",
            "filterType": "include",
            "filterPattern": lambda _: True,
        },
        {
            "type": "file",
            "categoryName": "Tangent",
            "enabled": True,
            "recursiveCopy": True,
            "silentReport": False,
            "parentSrcPaths": "D:/Program Files/Tangent/TArchT20*",
            "filterType": "include",
            "filterPattern": [
                "SYS/*.lay",
                "SYS/tangent.cuix",
                "sys20x64/*.dwt",
                "sys24x64/*.dwt",
                "sys24x64/Tch.tmn",
            ],
        },
        {
            "type": "file",
            "categoryName": "Asset",
            "enabled": True,
            "recursiveCopy": True,
            "silentReport": False,
            "parentSrcPaths": "F:/Asset/AutoCAD",
            "filterType": "include",
            "filterPattern": lambda _: True,
        },
        {
            "type": "registry",
            "categoryName": "Generic Settings",
            "enabled": enabledChk,
            "recursiveCopy": True,
            "silentReport": False,
            "stripePathValue": True,
            "parentPaths": r"HKEY_CURRENT_USER/Software/Autodesk/AutoCAD/R[0-9.]+",
            "filterType": "exclude",
            "filterPattern": [
                r"\\LastLaunchedLanguage",
                r"\\3DGS Configuration$",
                r"\\Applications$",
                r"\\AssemblyMap$",
                r"\\AutodeskApps$",
                r"\\DwgConvert$",
                r"\\DwgConvert$",
                r"\\Loaded$",
                r"\\MiniDump$",
                r"\\*UILOCALE$",
            ],
        },
    ],
)

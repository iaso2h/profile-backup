import os
import re
from pathlib import Path
from threading import local

import util
from profileBackup.backup import Profile

versionDict = [ # {{{
    {
        "releaseYear": 2026,
        "internalNumber": "30",
        "note": "Expected next major release (likely in early 2026). Files saved in Inventor 2026 will generally not be directly compatible with older versions without using 'Save As' to an earlier format.",
    },
    {
        "releaseYear": 2025,
        "internalNumber": "29",
        "note": "The latest stable release as of July 2025. Files saved in Inventor 2025 cannot be opened by Inventor 2024 or older without using 'Save As' to an earlier format. Features focus on interoperability, cloud integration, and continued performance gains.",
    },
    {
        "releaseYear": 2024,
        "internalNumber": "28",
        "note": "Introduced significant enhancements to user experience, drawing environment, and large assembly performance. Files saved in Inventor 2024 are generally compatible for reading with Inventor 2022 and 2023, but saving in 2024 prevents direct opening in 2023 or older without 'Save As'.",
    },
    {
        "releaseYear": 2023,
        "internalNumber": "27",
        "note": "Focused on automation tools, 'Model States' enhancements, and continued performance improvements across various workflows. Files saved in Inventor 2023 can be opened by 2022, but not 2021 or older versions directly.",
    },
    {
        "releaseYear": 2022,
        "internalNumber": "26",
        "note": "**Major file format change.** Files saved in Inventor 2022 or newer cannot be opened directly by Inventor 2021 or older versions. This version brought a new GPU-accelerated graphics engine, significant graphical, and performance improvements.",
    },
    {
        "releaseYear": 2021,
        "internalNumber": "25",
        "note": "Continued improvements in performance, 'AnyCAD' functionality, and drawing productivity features. Files saved in 2021 are generally compatible for reading with 2018-2020 versions, but saving in 2021 requires 'Save As' for older versions to open.",
    },
    {
        "releaseYear": 2020,
        "internalNumber": "24",
        "note": "Introduced new features like 'Unwrap' and enhancements to 'Frame Generator', along with factory design utilities improvements. Part of the 2018-2021 file format compatibility family.",
    },
    {
        "releaseYear": 2019,
        "internalNumber": "23",
        "note": "Focused on productivity tools, large assembly performance, and 'Model Based Definition' (MBD) enhancements. Generally compatible with 2018 files, but files saved in 2019 cannot be opened by 2017 or older without conversion.",
    },
    {
        "releaseYear": 2018,
        "internalNumber": "22",
        "note": "**Major file format change.** Files saved in Inventor 2018 or newer cannot be opened directly by Inventor 2017 or older versions. This was a significant update to the core data structure, improving performance and data handling across the software.",
    },
    {
        "releaseYear": 2017,
        "internalNumber": "21",
        "note": "Introduced 'Model Based Definition' (MBD), mesh handling, and enhanced 'AnyCAD' functionality for importing data from other CAD systems. Part of the 2015-2017 file format compatibility group.",
    },
    {
        "releaseYear": 2016,
        "internalNumber": "20",
        "note": "Further integration with AutoCAD and other Autodesk products, and improved performance for large assemblies and sketching. Generally compatible with 2015 and 2017 files.",
    },
    {
        "releaseYear": 2015,
        "internalNumber": "19",
        "note": "Focused on improved user experience, sketching, and drawing environments, including a redesigned home screen. Files saved in 2015 are not directly compatible with 2013 or older without 'Save As'.",
    },
    {
        "releaseYear": 2014,
        "internalNumber": "18",
        "note": "Introduced 'Autodesk Inventor Publisher' integration and significant enhancements to large assembly performance with 'Express Mode'.",
    },
    {
        "releaseYear": 2013,
        "internalNumber": "17",
        "note": "**Major file format change.** Files saved in Inventor 2013 or newer cannot be opened directly by Inventor 2012 or older versions. This release brought a substantial update to the underlying data architecture.",
    },
    {
        "releaseYear": 2012,
        "internalNumber": "16",
        "note": "Introduced 'Direct Edit' functionality, 'iLogic' improvements, and significant enhancements to sketching and drawing environments. Part of the 2010-2012 file format compatibility group.",
    },
    {
        "releaseYear": 2011,
        "internalNumber": "15",
        "note": "Enhanced parametric design, 'Freeform' tools, and assembly patterning. Generally compatible with 2010 and 2012 files.",
    },
    {
        "releaseYear": 2010,
        "internalNumber": "14",
        "note": "**Major file format change.** Files saved in Inventor 2010 or newer cannot be opened directly by Inventor 2009 or older versions. This was a substantial overhaul, particularly for large assembly performance and the introduction of the ribbon interface.",
    },
    {
        "releaseYear": 2009,
        "internalNumber": "13",
        "note": "Focused on user productivity, sustainability features, and expanded content libraries. Generally compatible with 2007 and 2008 files.",
    },
    {
        "releaseYear": 2008,
        "internalNumber": "12",
        "note": "Improved assembly design, drawing capabilities, and marked the beginning of the consistent 'Inventor 20XX' naming convention. Part of the 2007-2009 file format group.",
    },
    {
        "releaseYear": 2007,
        "internalNumber": "11",
        "note": "Introduced 'Adaptive Design', improved large assembly handling, and enhanced sheet metal features. The last version to use 'Inventor X' naming before 'Inventor 2008'.",
    },
    {
        "releaseYear": 2006,
        "internalNumber": "10",
        "note": "Enhanced collaboration tools, stress analysis capabilities, and 'iParts'/'iAssemblies'. Part of the 2004-2006 file format group.",
    },
    {
        "releaseYear": 2005,
        "internalNumber": "9",
        "note": "Improved modeling tools, drawing sheet formats, and weldment environments.",
    },
    {
        "releaseYear": 2004,
        "internalNumber": "8",
        "note": "**Major file format change.** Files saved in Inventor 2004 or newer cannot be opened directly by Inventor 2003 or older versions. This was a foundational format update, introducing many modern Inventor capabilities. Marketed as Inventor 2004.",
    },
    {
        "releaseYear": 2003,
        "internalNumber": "7",
        "note": "Introduced 'Design Accelerator', enhanced sheet metal capabilities, and improved weldments. Marketed as Inventor 7.",
    },
    {
        "releaseYear": 2002,
        "internalNumber": "6",
        "note": "Significant improvements in drawing views, presentation files, and large assembly management. Marketed as Inventor 6.",
    },
    {
        "releaseYear": 2001,
        "internalNumber": "5",
        "note": "Introduced enhanced assembly features and the 'Design Doctor' for troubleshooting models. Includes updates like Inventor 5.3. Marketed as Inventor 5.",
    },
    {
        "releaseYear": 2001,
        "internalNumber": "4",
        "note": "Improvements to sketching, part modeling, and initial introduction of 'AnyCAD' concepts for reading other CAD formats. Marketed as Inventor 4.",
    },
    {
        "releaseYear": 2000,
        "internalNumber": "3",
        "note": "Further development of the core parametric engine and assembly constraints. Marketed as Inventor 3.",
    },
    {
        "releaseYear": 2000,
        "internalNumber": "2",
        "note": "Early improvements focusing on drawing creation and basic assembly features. Marketed as Inventor 2.",
    },
    {
        "releaseYear": 1999,
        "internalNumber": "1",
        "note": "The very first release of Autodesk Inventor, laying the groundwork for parametric 3D design on the Windows platform. Marketed as Inventor 1.",
    },
] # }}}


localeDicts = [  # {{{
    {
        "localeId": "409",
        "abbreviation": "ENU",
        "languageName": "English",
        "sourceUrl": "https://www.autodesk.com/support/technical/article/caas/sfdcarticles/sfdcarticles/List-of-Local-Language-Code-for-Autodesk-Products.html",
    },
    {
        "localeId": "407",
        "abbreviation": "DEU",
        "languageName": "German",
        "sourceUrl": "https://www.autodesk.com/support/technical/article/caas/sfdcarticles/sfdcarticles/List-of-Local-Language-Code-for-Autodesk-Products.html",
    },
    {
        "localeId": "040C",
        "abbreviation": "FRA",
        "languageName": "French",
        "sourceUrl": "https://www.autodesk.com/support/technical/article/caas/sfdcarticles/sfdcarticles/List-of-Local-Language-Code-for-Autodesk-Products.html",
    },
    {
        "localeId": "410",
        "abbreviation": "ITA",
        "languageName": "Italian",
        "sourceUrl": "https://www.autodesk.com/support/technical/article/caas/sfdcarticles/sfdcarticles/List-of-Local-Language-Code-for-Autodesk-Products.html",
    },
    {
        "localeId": "040A",
        "abbreviation": "ESP",
        "languageName": "Spanish",
        "sourceUrl": "https://www.autodesk.com/support/technical/article/caas/sfdcarticles/sfdcarticles/List-of-Local-Language-Code-for-Autodesk-Products.html",
    },
    {
        "localeId": "415",
        "abbreviation": "PLK",
        "languageName": "Polish",
        "sourceUrl": "https://www.autodesk.com/support/technical/article/caas/sfdcarticles/sfdcarticles/List-of-Local-Language-Code-for-Autodesk-Products.html",
    },
    {
        "localeId": "040E",
        "abbreviation": "HUN",
        "languageName": "Hungarian",
        "sourceUrl": "https://www.autodesk.com/support/technical/article/caas/sfdcarticles/sfdcarticles/List-of-Local-Language-Code-for-Autodesk-Products.html",
    },
    {
        "localeId": "405",
        "abbreviation": "CSY",
        "languageName": "Czech",
        "sourceUrl": "https://www.autodesk.com/support/technical/article/caas/sfdcarticles/sfdcarticles/List-of-Local-Language-Code-for-Autodesk-Products.html",
    },
    {
        "localeId": "419",
        "abbreviation": "RUS",
        "languageName": "Rusian",
        "sourceUrl": "https://www.autodesk.com/support/technical/article/caas/sfdcarticles/sfdcarticles/List-of-Local-Language-Code-for-Autodesk-Products.html",
    },
    {
        "localeId": "416",
        "abbreviation": "PTB",
        "languageName": "Brazilian Portuguese",
        "sourceUrl": "https://www.autodesk.com/support/technical/article/caas/sfdcarticles/sfdcarticles/List-of-Local-Language-Code-for-Autodesk-Products.html",
    },
    {
        "localeId": "804",
        "abbreviation": "CHS",
        "languageName": "Simplified Chinese",
        "sourceUrl": "https://www.autodesk.com/support/technical/article/caas/sfdcarticles/sfdcarticles/List-of-Local-Language-Code-for-Autodesk-Products.html",
    },
    {
        "localeId": "404",
        "abbreviation": "CHT",
        "languageName": "Traditional Chinese",
        "sourceUrl": "https://www.autodesk.com/support/technical/article/caas/sfdcarticles/sfdcarticles/List-of-Local-Language-Code-for-Autodesk-Products.html",
    },
    {
        "localeId": "412",
        "abbreviation": "KOR",
        "languageName": "Korean",
        "sourceUrl": "https://www.autodesk.com/support/technical/article/caas/sfdcarticles/sfdcarticles/List-of-Local-Language-Code-for-Autodesk-Products.html",
    },
    {
        "localeId": "411",
        "abbreviation": "JPN",
        "languageName": "Japanese",
        "sourceUrl": "https://www.autodesk.com/support/technical/article/caas/sfdcarticles/sfdcarticles/List-of-Local-Language-Code-for-Autodesk-Products.html",
    },
]
# }}}


def internalNumberToYear(internalNumber) -> str:  # {{{
    for versionInfo in versionDict:
        if versionInfo["internalNumber"] == internalNumber:
            return str(versionInfo["releaseYear"])
    return ""


# }}}
def internalLanuageCodeToName(localeId) -> str:  # {{{
    for localeInfo in localeDicts:
        if localeInfo["localeId"] == localeId:
            return "Inventor_{}".format(localeInfo["abbreviation"])
    return ""


# }}}

appDataPath = Path(os.getenv("APPDATA")).parent  # type: ignore
installPathStr, enabledChk = util.regQueryData(
    r"HKEY_LOCAL_MACHINE/SOFTWARE/Autodesk/Inventor/RegistryVersion[0-9.]+", "InstallLocation"
)


internalNumberPat = re.compile(r"RegistryVersion(\d+).?.*")
def getReleaseVersion(keyRelPath):
    keyComponents = keyRelPath.split("\\")
    result = internalNumberPat.match(keyComponents[-1])
    if not result:
        releaseVersion = ""
    else:
        releaseVersion = "_" + result.group(1)
    KeyCompoentTail = "Inventor{}".format(
        internalNumberToYear(releaseVersion),
    )

    return KeyCompoentTail


Profile(
    profileName="Inventor",
    enabled=True,
    categories=[
        {
            "type": "file",
            "categoryName": "Preferences",
            "enabled": True,
            "recursiveCopy": True,
            "silentReport": False,
            "parentSrcPaths": appDataPath.glob("Roaming/Autodesk/Inventor *"),
            "filterType": "include",
            "filterPattern": [
                "InventorCustomization.xml",
                "UserApplicationOptions.xml",
            ]
        },
        {
            "type": "registry",
            "categoryName": "Settings",
            "enabled": enabledChk,
            "recursiveCopy": True,
            "silentReport": False,
            "stripePathValue": True,
            "parentPaths": r"HKEY_CURRENT_USER/Software/Autodesk/Inventor/RegistryVersion[0-9.]+",
            "filterType": "include",
            "filterPattern": [
                ".*"
            ],
            "keyPathNamingConvention": getReleaseVersion,
        },
    ],
)

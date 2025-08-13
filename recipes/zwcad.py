import os
from pathlib import Path

import util
from profileBackup.backup import Profile

appDataPath = Path(os.getenv("APPDATA")).parent  # type: ignore
installPathStr, enabledChk = util.regQueryData(
    r"HKEY_LOCAL_MACHINE/SOFTWARE/ZWSOFT/ZWCAD/\d+/.*", "Location"
)


def getReleaseVersion(keyRelPath):
    keyComponents = keyRelPath.split("\\")
    releaseVersion = keyComponents[-1][keyComponents[-1].index(":") + 1 :]
    KeyCompoentTail = "{}_{}".format(
        internalRNumberToYear(keyComponents[-2]),
        internalLanuageCodeToName(releaseVersion),
    )

    return KeyCompoentTail


Profile(
    profileName="ZWCAD",
    # enabled=enabledChk,
    enabled=True,
    categories=[
        {
            "type": "registry",
            "categoryName": "Generic Settings",
            "enabled": enabledChk,
            "recursiveCopy": True,
            "silentReport": False,
            "stripePathValue": True,
            "parentPaths": r"HKEY_CURRENT_USER/Software/ZWSOFT/ZWCAD/\d+/.*",
            "filterType": "exclude",
            "filterPattern": [
                r"\\Recent File List",
                r"\\Settings\\UserConfig\\Config$",
                r"\\Settings\\UserConfig\\OnlineUpdate\\dayCheck",
                r"\\Settings\\UserConfig\\OnlineUpdate\\downloadPath",
                r"\\Settings\\UserConfig\\OnlineUpdate\\ignoreVersion",
                r"\\Settings\\UserConfig\\OnlineUpdate\\lastCheckDay",
                r"\\Settings\\UserConfig\\OnlineUpdate\\lastCheckMonth",
                r"\\Settings\\UserConfig\\OnlineUpdate\\lastCheckYear",
                r"\\Settings\\UserConfig\\OnlineUpdate\\upLoadInfo",
                r"\\Settings\\UserConfig\\ToolBox$",
                r"\\UpdateUserFile$",
                r"\\*LogFilePath$",
            ],
            "keyPathNamingConvention": None,
        },
    ],
)

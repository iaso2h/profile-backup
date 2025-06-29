import util
from backup import Profile
from pathlib import Path
installPathStr, enabledChk = util.regQueryData(r"HKEY_CURRENT_USER\Software\Vivaldi", "DestinationFolder")

Profile(
    profileName="Vivaldi",
    enabled=enabledChk,
    categories=[
        {
            "type": "file",
            "categoryName": "Profile",
            "enabled": enabledChk,
            "recursiveCopy": True,
            "silentReport": False,
            "parentSrcPaths": Path(installPathStr, "User Data/Default"),
            "filterType": "include",
            "filterPattern": [
                "Preferences",
                "Shortcuts",
            ]
        },
    ]
)

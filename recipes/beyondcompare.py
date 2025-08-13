import os
from profileBackup.backup import Profile
from pathlib import Path
appDataPath = Path(os.getenv('APPDATA')).parent

Profile(
    profileName="ByondCompare",
    enabled=True,
    categories=[
        {
            "type": "file",
            "categoryName": "Profile",
            "enabled": True,
            "recursiveCopy": True,
            "silentReport": False,
            "parentSrcPaths": Path(appDataPath, "Roaming/Scooter Software/Beyond Compare 5"),
            "filterType": "include",
            "filterPattern": [
                "BCColors.xml",
                "BCCommands.xml",
                "BCPreferences.xml",
            ]
        },
    ]
)

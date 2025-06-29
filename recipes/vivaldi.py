import os
from backup import Profile
from pathlib import Path
appDataPath = Path(os.getenv('APPDATA')).parent

Profile(
    profileName="Vivaldi",
    enabled=True,
    categories=[
        {
            "type": "file",
            "categoryName": "Profile",
            "versionFind": "Generic",
            "enabled": True,
            "recursiveCopy": True,
            "silentReport": False,
            "parentSrcPaths": Path(appDataPath, "Local/Vivaldi/User Data/Default"),
            "filterType": "include",
            "filterPattern": [
                "Preferences",
                "Shortcuts",
            ]
        },
    ]
)

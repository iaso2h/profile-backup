import os
from profileBackup.backup import Profile
from pathlib import Path
appDataPath = Path(os.getenv('APPDATA')).parent

Profile(
    profileName="Typora",
    enabled=True,
    categories=[
        {
            "type": "file",
            "categoryName": "Profile",
            "enabled": True,
            "recursiveCopy": True,
            "silentReport": False,
            "parentSrcPaths": Path(appDataPath, "Roaming/typora"),
            "filterType": "include",
            "filterPattern": [
                "Preferences",
                "conf\conf.user.json",
                "themes/onedark",
                "themes/onedark.css",
            ]
        },
    ]
)

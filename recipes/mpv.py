import os
from profileBackup.backup import Profile
from pathlib import Path
appDataPath = Path(os.getenv('APPDATA')).parent

Profile(
    profileName="MPV",
    enabled=True,
    categories=[
        {
            "type": "file",
            "categoryName": "Profile",
            "enabled": True,
            "recursiveCopy": True,
            "silentReport": False,
            "parentSrcPaths": Path(appDataPath, "Roaming/mpv"),
            "filterType": "exclude",
            "filterPattern": [
                "*cache",
            ]
        },
    ]
)

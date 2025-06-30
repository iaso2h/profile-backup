import os
from profileBackup.backup import Profile
from pathlib import Path
appDataPath = Path(os.getenv('APPDATA')).parent

Profile(
    profileName="PowerToys",
    enabled=True,
    categories=[
        {
            "type": "file",
            "categoryName": "Profile",
            "enabled": True,
            "recursiveCopy": True,
            "silentReport": False,
            "parentSrcPaths": Path(appDataPath, "Local/Microsoft/PowerToys"),
            "filterType": "exclude",
            "filterPattern": [
                "**/*log*",
                "Updates",
            ]
        },
    ]
)

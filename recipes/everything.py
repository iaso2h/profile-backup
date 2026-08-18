import os
from profileBackup.backup import Profile
from pathlib import Path
appDataPath = Path(os.getenv('APPDATA')).parent

Profile(
    profileName="Everything",
    enabled=True,
    categories=[
        {
            "type": "file",
            "categoryName": "Profile",
            "enabled": True,
            "recursiveCopy": True,
            "silentReport": False,
            "parentSrcPaths": Path(appDataPath, "Roaming/Everything"),
            "filterType": "exclude",
            "filterPattern": [
                "Logs",
                "Search History.csv",
                "Run History.csv",
                "*.backup.*"
            ]
        },
    ]
)

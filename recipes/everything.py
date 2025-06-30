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
            "categoryName": "Main",
            "enabled": True,
            "recursiveCopy": True,
            "silentReport": False,
            "parentSrcPaths": Path(appDataPath, "Roaming/Everything"),
            "filterType": "exclude",
            "filterPattern": [
                "Search History.csv",
                "Run History.csv"
            ]
        },
    ]
)

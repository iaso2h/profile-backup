import os
from backup import Profile
from pathlib import Path
appDataPath = Path(os.getenv('APPDATA')).parent

Profile(
    profileName="TubesT",
    enabled=True,
    categories=[
        {
            "type": "file",
            "categoryName": "Main",
            "enabled": True,
            "recursiveCopy": True,
            "silentReport": False,
            "parentSrcPaths": appDataPath.glob("Roaming/Friendess/Tubest/*"),
            "filterType": "include",
            "filterPattern": lambda srcPath: srcPath.suffix.lower() == ".config",
        },
    ]
)

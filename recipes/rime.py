import os
from backup import Profile
from pathlib import Path
appDataPath = Path(os.getenv('APPDATA')).parent

Profile(
    profileName="Rime",
    enabled=True,
    categories=[
        {
            "type": "file",
            "categoryName": "Main",
            "versionFind": "Generic",
            "enabled": True,
            "recursiveCopy": True,
            "silentReport": False,
            "parentSrcPaths": Path(appDataPath, "Roaming/Rime"),
            "filterType": "exclude",
            "filterPattern": [
                "build",
                "sync",
                "sounds",
                "*.userdb",
                "clipboard.json",
                "history.json",
                "file.json",
                "installation.yaml",
                "user.yaml",
                "weasel.yaml",
            ],
        },
    ]
)

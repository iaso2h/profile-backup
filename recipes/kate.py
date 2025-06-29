import os
from backup import Profile
from pathlib import Path
appDataPath = Path(os.getenv('APPDATA')).parent

# TODO: no shortcut backup yet
Profile(
    profileName="Kate",
    enabled=True,
    categories=[
        {
            "type": "file",
            "categoryName": "Profile",
            "enabled": True,
            "recursiveCopy": True,
            "silentReport": False,
            "parentSrcPaths": Path(appDataPath, "Local"),
            "filterType": "include",
            "filterPattern": [
                "katerc",
                "katevirc",
            ]
        },
    ]
)

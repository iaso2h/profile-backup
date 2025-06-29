import os
from backup import Profile
from pathlib import Path
appDataPath = Path(os.getenv('APPDATA')).parent

Profile(
    profileName="XnViewMP",
    enabled=True,
    categories=[
        {
            "type": "file",
            "categoryName": "Main",
            "versionFind": "Generic",
            "enabled": True,
            "recursiveCopy": True,
            "silentReport": False,
            "parentSrcPaths": Path(appDataPath, "Roaming/XnViewMP"),
            "filterType": "exclude",
            "filterPattern": [ "*.db"],
        },
    ]
)

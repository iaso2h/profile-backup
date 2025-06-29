import os
from backup import Profile
from pathlib import Path
appDataPath = Path(os.getenv('APPDATA')).parent

Profile(
    profileName="Idea",
    enabled=True,
    categories=[
        {
            "type": "file",
            "categoryName": "Profile",
            "versionFind": "Generic",
            "enabled": True,
            "recursiveCopy": True,
            "silentReport": False,
            "parentSrcPaths": Path.home(),
            "filterType": "include",
            "filterPattern": [
                ".vimrc",
                ".ideavimrc",
            ]
        },
    ]
)

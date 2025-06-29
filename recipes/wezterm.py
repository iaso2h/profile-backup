import os
from backup import Profile
from pathlib import Path
appDataPath = Path(os.getenv('APPDATA')).parent

Profile(
    profileName="Wezterm",
    enabled=True,
    categories=[
        {
            "type": "file",
            "categoryName": "Profile",
            "enabled": True,
            "recursiveCopy": True,
            "silentReport": False,
            "parentSrcPaths": Path.home(),
            "filterType": "include",
            "filterPattern": [
                ".wezterm.lua",
            ]
        },
    ]
)

import os
from profileBackup.backup import Profile
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
        {
            "type": "file",
            "categoryName": "Shortcuts",
            "enabled": True,
            "recursiveCopy": True,
            "silentReport": False,
            "parentSrcPaths": Path(appDataPath, "Local/kxmlgui5/kate"),
            "filterType": "include",
            "filterPattern": [
                "kateui.rc",
            ]
        },
    ]
)

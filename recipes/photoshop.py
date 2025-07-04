import os
from profileBackup.backup import Profile
from pathlib import Path
appDataPath = Path(os.getenv('APPDATA')).parent

Profile(
    profileName="Photoshop",
    enabled=True,
    categories=[
        {
            "type": "file",
            "categoryName": "Main",
            "enabled": True,
            "recursiveCopy": True,
            "silentReport": False,
            "parentSrcPaths": appDataPath.glob(
                "Roaming/Adobe/Adobe Photoshop */Adobe Photoshop * Settings"
                ),
            "filterType": "include",
            "filterPattern": [
                "WorkSpaces*/*",
                "Menu Customization.psp",
                "Keyboard Shortcuts.psp",
                ],
        },
    ]
)

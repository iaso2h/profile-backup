import os
from backup import Profile
from pathlib import Path
appDataPath = Path(os.getenv('APPDATA')).parent

Profile(
    profileName="Powershell",
    enabled=True,
    categories=[
        {
            "type": "file",
            "categoryName": "Profile",
            "enabled": True,
            "recursiveCopy": True,
            "silentReport": False,
            "parentSrcPaths": Path(Path.home(), "Documents/WindowsPowerShell"),
            "filterType": "exclude",
            "filterPattern": [
                "profile.ps1",
                "Modules"
            ]
        },
    ]
)

import os
from profileBackup.backup import Profile
from pathlib import Path
appDataPath = Path(os.getenv('APPDATA')).parent

Profile(
    profileName="3ds Max",
    enabled=True,
    categories=[
        {
            "type": "file",
            "categoryName": "Workspace",
            "enabled": True,
            "recursiveCopy": True,
            "silentReport": False,
            "parentSrcPaths": appDataPath.glob("Local/Autodesk/3dsMax/*/*/*/UI/Workspaces"),
            "filterType": "exclude",
            "filterPattern": lambda srcPath: srcPath.is_dir()
                or str.startswith(srcPath.name, "Workspace"),
        },
        {
            "type": "file",
            "categoryName": "User Settings",
            "enabled": True,
            "recursiveCopy": True,
            "silentReport": False,
            "parentSrcPaths": Path.home().glob("Autodesk/3ds Max*/User Settings"),
            "filterType": "include",
            "filterPattern": lambda _: True,
        },
    ]
)

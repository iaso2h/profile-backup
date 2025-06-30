import os
from profileBackup.backup import Profile
from pathlib import Path
appDataPath = Path(os.getenv('APPDATA')).parent

Profile(
    profileName="Blender",
    enabled=True,
    categories=[
        {
            "type": "file",
            "categoryName": "Main",
            "enabled": True,
            "recursiveCopy": True,
            "silentReport": False,
            "parentSrcPaths": appDataPath.glob(
                "Roaming/Blender Foundation/Blender/*/scripts/presets/keyconfig"
                ),
            "filterType": "include",
            "filterPattern": [ "*.py", ],
        },
    ]
)

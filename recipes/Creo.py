import os
from profileBackup.backup import Profile
from pathlib import Path
appDataPath = Path(os.getenv("APPDATA")).parent  # type: ignore

Profile(
    profileName="Creo",
    enabled=True,
    categories=[
        {
            "type": "file",
            "categoryName": "Configuration",
            "enabled": True,
            "recursiveCopy": True,
            "silentReport": False,
            "parentSrcPaths": Path("C:/Users/Public/Documents"),
            "filterType": "include",
            "filterPattern": ["config.pro"]
        },
        {
            "type": "file",
            "categoryName": "User Interface",
            "enabled": True,
            "recursiveCopy": True,
            "silentReport": False,
            "parentSrcPaths": Path(appDataPath, "Roaming/PTC/ProENGINEER/Wildfire/.wf/.Settings"),
            "filterType": "include",
            "filterPattern": ["creo_parametric_customization.ui"]
        },
    ]
)

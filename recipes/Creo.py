import os
from profileBackup.backup import Profile
from pathlib import Path

import util

appDataPath = Path(os.getenv("APPDATA")).parent  # type: ignore

commonFilesPathStr, enabledChk = util.regQueryData(r"HKEY_LOCAL_MACHINE/SOFTWARE/PTC/PTC Creo Parametric/[0-9.]+", "InstallDir")


Profile(
    profileName="Creo Parametric",
    enabled=True,
    categories=[
        {
            "type": "file",
            "categoryName": "Configuration",
            "enabled": True,
            "recursiveCopy": True,
            "silentReport": False,
            "parentSrcPaths": Path.home(),
            "filterType": "include",
            "filterPattern": ["config.pro"]
        },
        {
            "type": "file",
            "categoryName": "Common Files",
            "enabled": True,
            "recursiveCopy": True,
            "silentReport": False,
            "parentSrcPaths": Path(commonFilesPathStr, "text"),
            "filterType": "include",
            "filterPattern": [
                "syscol.scl"
            ]
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

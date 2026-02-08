import os
from profileBackup.backup import Profile
from pathlib import Path
appDataPath = Path(os.getenv('APPDATA')).parent

Profile(
    profileName="obsidian",
    enabled=True,
    categories=[
        {
            "type": "file",
            "categoryName": "Profile",
            "enabled": True,
            "recursiveCopy": True,
            "silentReport": False,
            "parentSrcPaths": Path(appDataPath, "Local/ObsidianPlugins/Data/Profiles"),
            "filterType": "exclude",
            "filterPattern": [
                "Generic/plugins/copilot/data.json",
                "Generic/plugins/recent-files-obsidian/data.json"
            ]
        },
    ]
)

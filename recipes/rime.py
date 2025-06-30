import util
from backup import Profile
from pathlib import Path
rimeUserFolder, enabledChk = util.regQueryData(r"HKEY_CURRENT_USER/Software/Rime/Weasel", "RimeUserDir")

Profile(
    profileName="Rime",
    enabled=enabledChk,
    categories=[
        {
            "type": "file",
            "categoryName": "Main",
            "enabled": True,
            "recursiveCopy": True,
            "silentReport": False,
            "parentSrcPaths": Path(rimeUserFolder),
            "filterType": "exclude",
            "filterPattern": [
                "build",
                "sync",
                "sounds",
                "*.userdb",
                "clipboard.json",
                "history.json",
                "file.json",
                "installation.yaml",
                "user.yaml",
                "weasel.yaml",
            ],
        },
    ]
)

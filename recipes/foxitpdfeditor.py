import util
from pathlib import Path
from profileBackup.backup import Profile

Profile(
    profileName="Foxit PDF Editor",
    enabled=True,
    categories=[
        {
            "type": "registry",
            "categoryName": "Generic Settings",
            "enabled": True,
            "recursiveCopy": True,
            "silentReport": False,
            "stripePathValue": True,
            "parentPaths": r"HKEY_CURRENT_USER/Software/Foxit Software/Foxit PDF Editor/Classic/[A-Za-z0-9.]+",
            "filterType": "include",
            "filterPattern": [
                r"\\.*",
            ]
        },
        {
            "type": "file",
            "categoryName": "Ribbon",
            "enabled": False,
            "recursiveCopy": True,
            "silentReport": False,
            "parentSrcPaths": r"C:/Users/Hashub/AppData/Roaming/Foxit Software/Classic/Foxit PDF Editor",
            "filterType": "include",
            "filterPattern": [
                "RibbonCustom.xml",
            ]
        },
    ]
)

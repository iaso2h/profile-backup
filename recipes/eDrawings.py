from profileBackup.backup import Profile

Profile(
    profileName="eDrawings",
    enabled=True,
    categories=[
        {
            "type": "registry",
            "categoryName": "Settings",
            "enabled": True,
            "recursiveCopy": False,
            "silentReport": False,
            "stripePathValue": True,
            "parentPaths": r"HKEY_CURRENT_USER/Software/eDrawings/e\d+/General",
            "filterType": "include",
            "filterPattern": [
                r".*",
            ],
        },
    ]
)

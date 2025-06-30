from backup import Profile

Profile(
    profileName="StartAllBack",
    enabled=True,
    categories=[
        {
            "type": "registry",
            "categoryName": "Settings",
            "enabled": True,
            "recursiveCopy": False,
            "silentReport": False,
            "stripePathValue": True,
            "parentPaths": r"HKEY_CURRENT_USER/Software/StartIsBack",
            "filterType": "include",
            "filterPattern": [
                r".*",
            ],
        },
    ]
)

import util
from pathlib import Path
from profileBackup.backup import Profile
installPathStr, enabledChk = util.regQueryData(r"HKEY_LOCAL_MACHINE/SOFTWARE/SolidWorks/SOLIDWORKS \d+/Setup", "SolidWorks Folder")

filterPatternDict = {
    "Generic Settings": [
        r"\\Assemblies", # System Options -> Assemblies
        r"\\Auto Dimension Drawing", # https://www.youtube.com/watch?v=giXSQvOizAs
        r"\\Auto Dimension Sketch",
        r"\\AutoFix",
        r"\\Collab", # https://blogs.solidworks.com/solidworksblog/2024/05/whats-new-in-solidworks-with-the-platform-roles.html
        r"\\Color Schemes\05 - Scheme Folder",
        r"\\Colors",
        # r"\\ContentManager", # https://help.solidworks.com/2024/english/ccsupplier/c_content_manager_container.html
        r"\\Crosshatch",
        r"\\Dimensions", # Document Properties -> Dimensions
        r"\\DockingPaneLayouts",
        r"\\Document Templates", # System Options -> Default Templates
        r"\\Drawings", # System Options -> Drawings
        r"\\Edges", # System Options -> Display
        r"\\Export Settings", # System Options -> Export
        r"\\ExtReferences", # System Options -> External References
        r"\\FeatureWorks", # https://www.youtube.com/watch?v=avoSOXkGyxk
        r"\\General$", # System Options -> General
        r"\\General\\FeatureMgr", # System Options -> FeatureManager
        r"\\Import\\IgesSettings", # System Options -> Export -> IGES 5.x
        r"\\ImportSettings", # System Options -> Import
        r"\\Ink Markup", # System Options -> Import
        r"\\LineFont", # Documen Properties(Drawing) -> Line Font
        r"\\LineFontWeight", # Documen Properties(Drawing) -> Line Font
        r"\\Pen Sketch", # Command Manager -> Sketch Ink
        r"\\Performance", # System Options -> Performance
        r"\\Plane",
        # r"\\PlasticsMode",
        r"\\Quick View", # https://support.hawkridgesys.com/hc/en-us/articles/203486053-SOLIDWORKS-File-Opening-Modes-Resolved-Lightweight-Large-Design-Review-Detailing-Quick-View#:~:text=unless%20otherwise%20specified.-,quick%20view,-Quick%20View%20mode
        r"\\Regeneration",
        # r"\\Routing",
        r"\\SheetMetal",
        # r"\\Simulation$",
        # r"\\SW on ACIS",
        r"\\Toolbars",
        r"\\User Interface\\CustomMenu",
        r"\\PropManager", # https://help.solidworks.com/2024/English/SolidWorks/sldworks/r_pm_overview.htm
        r"\\PowerSelect", # https://www.youtube.com/watch?v=seE2FyUKYnI
        r"\\SymCheck",

    ],
    "Menu Customization": [
        r"\\Menu Customizations",
    ],
    "Mouse": [
        r"\\Mouse Gestures",
    ],
    "Keyboard Shorcuts": [
        r"\\Customization",
    ],
    "Toolbar Layout": [
        r"\\User Interface",
    ],
}
filterPatternCollapsed = [item for sublist in filterPatternDict.values() for item in sublist]

Profile(
    profileName="SOLIDWORKS",
    enabled=enabledChk,
    categories=[
        {
            "type": "file",
            "categoryName": "Program Files",
            "enabled": enabledChk,
            "recursiveCopy": True,
            "silentReport": False,
            "parentSrcPaths": Path(installPathStr),
            "filterType": "include",
            "filterPattern": [
                "lang/*/calloutformat.txt",
                "lang/*/calloutformat_2.txt",
                "lang/*/calloutformat_3.txt",
                "data/ttfontratiomap.txt",
                "data/drawfontmap.txt",
            ],
        },
        {
            "type": "registry",
            "categoryName": "Registry All-in-One",
            "enabled": enabledChk,
            "recursiveCopy": True,
            "silentReport": False,
            "stripePathValue": True,
            "parentPaths": r"HKEY_CURRENT_USER/Software/SolidWorks/SOLIDWORKS \d+",
            "filterType": "include",
            "filterPattern": [".*"]
        },
        {
            "type": "registry",
            "categoryName": "Generic Settings",
            "enabled": enabledChk,
            "recursiveCopy": True,
            "silentReport": False,
            "stripePathValue": True,
            "parentPaths": r"HKEY_CURRENT_USER/Software/SolidWorks/SOLIDWORKS \d+",
            "filterType": "include",
            "filterPattern": filterPatternDict["Generic Settings"],
        },
        {
            "type": "registry",
            "categoryName": "Menu Customization",
            "enabled": enabledChk,
            "recursiveCopy": True,
            "silentReport": False,
            "stripePathValue": True,
            "parentPaths": r"HKEY_CURRENT_USER/Software/SolidWorks/SOLIDWORKS \d+",
            "filterType": "include",
            "filterPattern": filterPatternDict["Menu Customization"],
        },
        {
            "type": "registry",
            "categoryName": "Mouse",
            "enabled": enabledChk,
            "recursiveCopy": True,
            "silentReport": False,
            "stripePathValue": True,
            "parentPaths": r"HKEY_CURRENT_USER/Software/SolidWorks/SOLIDWORKS \d+",
            "filterType": "include",
            "filterPattern": filterPatternDict["Mouse"],
        },
        {
            "type": "registry",
            "categoryName": "Keyboard Shorcuts",
            "enabled": enabledChk,
            "recursiveCopy": True,
            "silentReport": False,
            "stripePathValue": True,
            "parentPaths": r"HKEY_CURRENT_USER/Software/SolidWorks/SOLIDWORKS \d+",
            "filterType": "include",
            "filterPattern": filterPatternDict["Keyboard Shorcuts"],
        },
        {
            "type": "registry",
            "categoryName": "Toolbar Layout",
            "enabled": enabledChk,
            "recursiveCopy": True,
            "silentReport": False,
            "stripePathValue": True,
            "parentPaths": r"HKEY_CURRENT_USER/Software/SolidWorks/SOLIDWORKS \d+",
            "filterType": "include",
            "filterPattern": filterPatternDict["Toolbar Layout"],
        },
    ]
)

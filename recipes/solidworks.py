import winreg
from backup import Profile

Profile(
    profileName="SOLIDWORKS",
    enabled=True,
    categories=[
        {
            "type": "file",
            "categoryName": "Language Configs",
            "versionFind": lambda _: winreg.QueryValueEx(
                winreg.OpenKey(
                    winreg.HKEY_LOCAL_MACHINE,
                    "SOFTWARE\\SolidWorks\\IM"
                    ),
                "IMSchedulerVersion"
                )[0][:4],
            "enabled": True,
            "recursiveCopy": True,
            "silentReport": False,
            "parentSrcPaths": "D:/**/SOLIDWORKS Corp/SOLIDWORKS/lang/*",
            "filterType": "include",
            "filterPattern": [
                "calloutformat.txt",
                "calloutformat_2.txt",
                "calloutformat_3.txt",
            ],
        },
        {
            "type": "file",
            "categoryName": "Data",
            "versionFind": lambda _: winreg.QueryValueEx(
                    winreg.OpenKey(
                        winreg.HKEY_LOCAL_MACHINE,
                        "SOFTWARE\\SolidWorks\\IM"
                        ),
                    "IMSchedulerVersion"
                    )[0][:4],
            "enabled": True,
            "recursiveCopy": True,
            "silentReport": False,
            "parentSrcPaths": "D:/**/SOLIDWORKS Corp/SOLIDWORKS/data",
            "filterType": "include",
            "filterPattern": [
                "ttfontratiomap.txt",
                "drawfontmap.txt",
            ],
        },
        {
            "type": "registry",
            "categoryName": "Generic Settings",
            "enabled": True,
            "recursiveCopy": True,
            "silentReport": False,
            "parentPaths": r"HKEY_CURRENT_USER\Software\SolidWorks\SOLIDWORKS \d+",
            "filterType": "include",
            "filterPattern": [
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
        },
    ]
)

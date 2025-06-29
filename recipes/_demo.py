import os
from backup import Profile
from pathlib import Path
appDataPath = Path(os.getenv('APPDATA')).parent

Profile(
    profileName="demoBackup",
    enabled=False,
    categories=[
        {
            "type": "file",
            "categoryName": "Main",
            # 1. function with `parentSrcPath` as parameter
            # 2. string
            "enabled": False,
            "recursiveCopy": True,
            "silentReport": False,
            # 1. Path object
            # 2. Path glob generator
            # 3. string contains parent source directory(support */** wildcard character)
            "parentSrcPaths": Path(Path("__file__").resolve().parent, "profileBackup", "tests", "test_backup_src_files"),
            # 1. "exclude"
            # 2. "include"
            "filterType": "exclude",
            # 1. function with `parentSrcPath` as parameter, return boolean
            # 2. list contains string(support */** wildcard character)
            "filterPattern": lambda srcPath: srcPath.is_dir()
                or str.startswith(srcPath.name, "Workspace"),
        },
    ]
)

# File: profileBakup
# Author: iaso2h
# Description: Back up software profiles on Windows
# Version: 0.1.25
# Last Modified: 2025-06-29
from pathlib import Path
import sys
import argparse
import pytest

sys.path.insert(0, str(Path(__file__).parent.parent.resolve()))
sys.path.insert(0, str(Path(__file__).parent.resolve()))

import recipes
import backup
import cli


if __name__ == "__main__":
    if len(sys.argv) >= 2:
        argParser = argparse.ArgumentParser()
        argParser.add_argument("-d", "--debug", action="store_true")
        args = argParser.parse_args()
        if args.debug:
            # from pathlib import Path
            # print(Path(Path(Path("__file__").resolve().parent), "profileBackup", "tests", "test_backup_dst_files"))
            pytest.main(["-vv"])
    else:
        cli.program()

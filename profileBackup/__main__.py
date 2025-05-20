# File: profileBakup
# Author: iaso2h
# Description: Backup software profiles on Windows
# Version: 0.1.9
# Last Modified: 2025-05-20

# import logging
import sys
import cli
import argparse
import pytest

# TODO: adapt user name in new desitionation


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

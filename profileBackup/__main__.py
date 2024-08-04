# File: profileBakup
# Author: iaso2h
# Description: Backup software profiles on Windows
# Version: 0.0.12
# Last Modified: 2024-06-25

# import logging
import sys
import cli

# TODO: adapt user name in new desitionation


if __name__ == "__main__":
    if len(sys.argv) >= 2:
        import argparse
        argParser = argparse.ArgumentParser()
        argParser.add_argument("-d", "--debug", action="store_true")
        args = argParser.parse_args()
        if args.debug:
            print()
    else:
        cli.standardRun()

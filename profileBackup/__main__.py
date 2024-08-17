# File: profileBakup
# Author: iaso2h
# Description: Backup software profiles on Windows
# Version: 0.0.20
# Last Modified: 2024-08-17

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
            import config
            import backup
            for s in config.softwareConfigs:
                if not s.name.lower().startswith("demo"):
                    s.enabled = False
                else:
                    s.enabled = True

            backup.Backup.updateEnabledList()


    cli.standardRun()

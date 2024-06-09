# File: profileBakup
# Author: iaso2h
# Description: Backup software profiles on Windows
# Version: 0.0.6
# Last Modified: 2024-06-09

# import logging
import cli

from icecream import ic

# TODO: adapt user name in new desitionation


if __name__ == "__main__":
    ic.configureOutput(includeContext=True)
    cli.standardRun()

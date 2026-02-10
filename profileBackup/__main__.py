#!/usr/bin/env python3
"""
Profile Backup Tool

A utility for backing up software profiles on Windows systems.
Supports various applications and provides both sync and update modes.
"""

import argparse
import sys
from pathlib import Path

from . import cli, __version__
import recipes


def parse_args() -> argparse.Namespace:
    """
    Parse command line arguments for the profile backup utility.

    Returns:
        argparse.Namespace: Parsed command line arguments containing:
            - debug (bool): Flag to enable debug mode
            - version (str): Displays version information when invoked

    The function sets up argument parsing with:
        - Optional debug flag (-d/--debug)
        - Version display (-v/--version)
        - Interactive mode as default behavior
    """
    parser = argparse.ArgumentParser(
        description="Backup software profiles on Windows",
        epilog="Run without arguments to start in interactive mode.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        add_help=True
    )
    
    parser.add_argument(
        "-d", "--debug",
        action="store_true",
        help="Run tests in debug mode and enable verbose logging"
    )
    
    parser.add_argument(
        "-v", "--version",
        action="version",
        version=f"%(prog)s {__version__}"
    )
    
    return parser.parse_args()


def main() -> int:
    """
    Main entry point for the profile backup tool.

    Returns:
        int: Exit code (0 for success, non-zero for errors)
    """
    args = parse_args()

    if args.debug:
        import pytest
        return pytest.main(["-vv"])

    cli.program()
    return 0


if __name__ == "__main__":
    sys.exit(main())
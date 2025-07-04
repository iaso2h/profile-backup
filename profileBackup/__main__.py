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
    """Parse and return command line arguments."""
    parser = argparse.ArgumentParser(
        description="Backup software profiles on Windows",
        epilog="Run without arguments to start in interactive mode."
    )
    parser.add_argument(
        "-d", "--debug",
        action="store_true",
        help="Run tests in debug mode"
    )
    parser.add_argument(
        "-v", "--version",
        action="version",
        version=f"{__version__}"
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

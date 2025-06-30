import pytest
import sys
import os
from pathlib import Path
from unittest.mock import MagicMock

# Create a mock config module
mock_config = MagicMock()
mock_config.LOGPATH = Path(os.getcwd(), "log.yaml")
mock_config.SILENTMODE = False
mock_config.EXPORTLOG = False

# Add the mock config module to sys.modules
sys.modules['config'] = mock_config

# Now import the function to test
from util import regGlobKeyRelPaths

@pytest.mark.parametrize("key_pattern, expected_paths, expected_root", [
    (
        r"HKEY_LOCAL_MACHINE/SOFTWARE/SolidWorks/SOLIDWORKS \d+/Setup",
        ['SOFTWARE\\SolidWorks\\SOLIDWORKS 2024\\Setup'],
        "HKEY_LOCAL_MACHINE"
    ),
    (
        r"HKEY_CURRENT_USER/Software/Vivaldi",
        ['Software\\Vivaldi'],
        "HKEY_CURRENT_USER"
    ),
    (
        r"HKEY_CURRENT_USER/Software/StartIsBack",
        ['Software\\StartIsBack'],
        "HKEY_CURRENT_USER"
    ),
    (
        r"HKEY_CURRENT_USER/Software/Foxit Software/Foxit PDF Editor/Classic/[0-9.]+",
        ['Software\\Foxit Software\\Foxit PDF Editor\\Classic\\13.0'],
        "HKEY_CURRENT_USER"
    ),
    (
        r"HKEY_CURRENT_USER/Software/Autodesk/AutoCAD/R[0-9.]+",
        ['Software\\Autodesk\\AutoCAD\\R24.3'],
        "HKEY_CURRENT_USER"
    ),
    (
        r"HKEY_CURRENT_USER/Software/Autodesk/AutoCAD/R[0-9.]+/ACAD-\d+:804",
        ['Software\\Autodesk\\AutoCAD\\R24.3\\ACAD-7101:804'],
        "HKEY_CURRENT_USER"
    ),
    (
        r"HKEY_CURRENT_USER/Software/Autodesk/AutoCAD/R[0-9.]+/ACAD-\d+:\d+",
        ['Software\\Autodesk\\AutoCAD\\R24.3\\ACAD-7101:409', 'Software\\Autodesk\\AutoCAD\\R24.3\\ACAD-7101:804'],
        "HKEY_CURRENT_USER"
    ),
    (
        r"HKEY_CURRENT_USER/foobar",
        [],
        None
    ),
    (
        r"HKEY_CURRENT_USER/Software/Microsoft/.*/^[0-9.]{3}$",
        ['Software\\Microsoft\\Office\\9.0', 'Software\\Microsoft\\TabletTip\\1.7', 'Software\\Microsoft\\VBA\\7.1'],
        "HKEY_CURRENT_USER"
    ),
])


def test_regGlobKeyRelPaths(key_pattern, expected_paths, expected_root):
    """Test regGlobKeyRelPaths function with various registry key patterns."""
    paths, root = regGlobKeyRelPaths(key_pattern)
    assert paths == expected_paths
    assert root == expected_root


# Additional test cases for error conditions
def test_regGlobKeyRelPaths_invalid_input():
    """Test regGlobKeyRelPaths with invalid inputs."""
    # Test with non-string input
    with pytest.raises(ValueError, match="string value is expected from key path pattern."):
        regGlobKeyRelPaths(123)

    # Test with path starting with backslash or forward slash
    with pytest.raises(ValueError, match="the keypath must not start with a backslash."):
        regGlobKeyRelPaths(r"\HKEY_CURRENT_USER/Software")
    with pytest.raises(ValueError, match="the keypath must not start with a backslash."):
        regGlobKeyRelPaths("/HKEY_CURRENT_USER/Software")

    # Test with only root key
    with pytest.raises(ValueError, match="invalid key path pattern, it must contain at least one key component separated by \"/\" delimiter."):
        regGlobKeyRelPaths("HKEY_CURRENT_USER")

    # Test with invalid root key
    with pytest.raises(ValueError, match='invalid registry hkey "INVALID_KEY" for key path pattern.'):
        regGlobKeyRelPaths("INVALID_KEY/Software")

    # Test with regex in root key
    with pytest.raises(ValueError, match='invalid registry hkey "HKEY_.*_USER" for key path pattern.'):
        regGlobKeyRelPaths(r"HKEY_.*_USER/Software")

    # Test for non-existent path
    paths, root = regGlobKeyRelPaths("HKEY_CURRENT_USER/NonExistentPath")
    assert paths == []
    assert root == None

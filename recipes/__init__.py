import os
import importlib
from pathlib import Path


# Automatically import all submodules in the recipes package
package_dir = Path(__file__).parent
for file in package_dir.glob("*.py"):
    if file.name.startswith("_") or file.name == "__init__.py":
        continue
    module_name = f"{__name__}.{file.stem}"
    importlib.import_module(module_name)

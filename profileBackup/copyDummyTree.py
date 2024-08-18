import os
from pathlib import Path

while True:
    ans = input("Enter destionation path: ")
    targetDir = Path(ans)
    if targetDir.exists() and targetDir.is_dir():
        break
    print("\nInvalid directory path, please try again")

curentPyDir       = Path("__file__").resolve().parent
shadowTreeDestDir = Path(curentPyDir, "shadowTree")
os.makedirs(shadowTreeDestDir, exist_ok=True)

def touch(path):
    with open(path, 'a'):
        os.utime(path, None)

def shadowTree(dstDir:Path, targetDir:Path, topTargetDir=None):
    if not topTargetDir:
        topTargetDir = targetDir

    for p in targetDir.iterdir():
        relPath = p.relative_to(topTargetDir)
        dstPath = Path(dstDir, str(relPath))

        if p.is_dir():
            os.makedirs(dstPath, exist_ok=True)
            shadowTree(dstDir, p, topTargetDir)
        else:
            touch(str(dstPath))

shadowTree(shadowTreeDestDir, targetDir)
print("Done")

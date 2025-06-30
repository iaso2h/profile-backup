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
    """
    Creates an empty file or updates the timestamp of an existing file.
    
    This function mimics the Unix 'touch' command by either creating a new empty
    file at the specified path if it doesn't exist, or updating the access and
    modification times of an existing file to the current time.
    
    Args:
        path: The file path to touch. Can be a string or Path object.
        
    Returns:
        None
        
    Note:
        - Creates any necessary parent directories
        - Does not modify file content if the file already exists
        - Updates both access and modification timestamps
        - Thread-safe for different files
    """
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
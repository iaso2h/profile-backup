import psutil

def findRemovableDrive():
    # Credit: https://stackoverflow.com/questions/12266211/python-windows-list-only-usb-removable-drives
    externals = [i.mountpoint for i in psutil.disk_partitions() if "removable" in i.opts]
    if len(externals) != 0:
        return externals[0][:1]
    else:
        return

drive = findRemovableDrive()
# TODO: prompt the user to store the profile in the removable drive

import config
import backup


import psutil
import os
import beaupy
import beaupy.spinners as sp
from typing import List
from enum import Enum
from pathlib import Path

def findRemovableDrive() -> List[str]:
    # Credit: https://stackoverflow.com/questions/12266211/python-windows-list-only-usb-removable-drives
    return ["[blue]" + i.mountpoint + "\\[/blue]" for i in psutil.disk_partitions() if "removable" in i.opts]

def findAllDrives() -> List[str]:
    import psutil
    drps = psutil.disk_partitions()
    return list(map(lambda i: i[:1], [dp.device for dp in drps if dp.fstype == 'NTFS']))


print = backup.console.print

def keyboardInterruptExit() -> None:
    print("[red]Interrupt by user[/red]")
    raise SystemExit(1)


def abortExit() -> None:
    print("[red]Abort by user[/red]")
    raise SystemExit(1)


cwd = os.getcwd()
class PreservedDstAns(Enum):
    CWD    = f"[blue]Current Working Directory([yellow]{cwd}\\\\[/yellow][blue])[/blue]"
    CUSTOM = "[blue]Custom Path[/blue]"
preservedDsts = [
        PreservedDstAns.CWD.value,
        PreservedDstAns.CUSTOM.value,
        ]
beaupy.Config.raise_on_interrupt = True
beaupy.Config.raise_on_escape    = True

def parse():

    if not backup.DRYRUN:
        os.makedirs(str(backup.DESTPATH), exist_ok=True)

    for i in config.softwareConfigs:
        if i.ticked:
            i.backup()


def standardRun() -> None:
    # Select software
    print("Select the software to back up:")
    # Initial ticked value: make all software ticked beforehand
    for i in config.softwareConfigs:
        i.ticked = True

    try:
        softwareChoice = beaupy.select_multiple(
                backup.Backup.softwareNameList,
                tick_character = '■',
                ticked_indices = list(range(len(backup.Backup.softwareNameList))),
                minimal_count  = 1,
                return_indices = True
        )
        backup.Backup.softwareNameTickedList = [config.softwareConfigs[i].name for i in softwareChoice if config.softwareConfigs[i].ticked]


    except KeyboardInterrupt:
        keyboardInterruptExit()
    except beaupy.Abort:
        abortExit()
    except Exception as e:
        print(e)
        SystemExit(1)


    # Select destination path
    drivePaths = findRemovableDrive()
    alldrives  = findAllDrives()
    print("Choose the path to store the files:")
    drivePaths.extend(preservedDsts)
    try:
        ans = beaupy.select(drivePaths, return_index=True) # type: list
    except KeyboardInterrupt:
        keyboardInterruptExit()
    except beaupy.Abort:
        abortExit()
    except Exception as e:
        print(e)
        SystemExit(1)

    # Get destination path
    if ans == len(drivePaths) - 2:
        # Current working directory
        backup.DESTPATH = Path(cwd, "Profiles")
    elif ans == len(drivePaths) - 1:
        # Custom path
            try:
                while True:
                    dst = Path(
                                beaupy.prompt(
                                    "Input the parent folder path you want store your profiles in:",
                                    # initial_value=cwd
                                )
                            )
                    if not dst.exists():
                        anchorPath = Path(dst.anchor)

                        if not anchorPath.exists() or \
                                str(anchorPath)[-1:] != "\\" or \
                                str(anchorPath).upper() not in alldrives:
                            print("[red]Invalid path. Please try again.[/red]")
                        else:
                            ans = beaupy.confirm(
                                    f"Folder [yellow]{str(dst)}[/yellow] doesn't exist, do you want to create one?",
                                    default_is_yes=True,
                                    yes_text="[blue]Yes[/blue]",
                                    no_text="[blue]No[/blue]",
                                )
                            if ans:
                                backup.DESTPATH = Path(dst, "Profiles")
                                break
                            else:
                                abortExit()
                    else:
                        break
            except KeyboardInterrupt:
                keyboardInterruptExit()
            except beaupy.Abort:
                abortExit()
            except Exception as e:
                print(e)
                SystemExit(1)
    else:
        # Removable drives
        backup.DESTPATH = Path(str(drivePaths[ans])[6:10], "Profiles")

    # Ask if to run in dry mode
    try:
        backup.DRYRUN = beaupy.confirm(
                f"Backing up file in [yellow]{backup.DESTPATH}[/yellow]. Do you want to run in [purple bold]dry run mode[/purple bold] first?",
                yes_text="[blue]Yes. I just want to preview the result.[/blue]",
                no_text="[blue]No. Back up my files right away.[/blue]",
                default_is_yes=True,
            )
    except KeyboardInterrupt:
        keyboardInterruptExit()
    except beaupy.Abort:
        abortExit()
    except Exception as e:
        print(e)
        SystemExit(1)


    spinner = sp.Spinner(sp.DOTS, "Parsing...\n")
    spinner.start()
    parse()
    spinner.stop()

    if backup.Backup.totalBackupCount > 0 and backup.DRYRUN:
        backup.Backup.totalBackupCount = 0 # Rest the total count
        confirmRun()
    else:
        print("\n[purple bold]Everything is up-to-date! You're good to go.[/purple bold]")


def confirmRun():
    try:
        backup.DRYRUN = not beaupy.confirm(
                f"End the [purple bold]dry run mode[/purple bold] and confirm the whole backup process?",
                yes_text="[blue]Yes[/blue]",
                no_text="[blue]No[/blue]",
                default_is_yes=True,
            )
    except KeyboardInterrupt:
        keyboardInterruptExit()
    except beaupy.Abort:
        abortExit()
    except Exception as e:
        print(e)
        SystemExit(1)

    if not backup.DRYRUN:
        spinner = sp.Spinner(sp.DOTS, "Parsing...\n")
        spinner.start()
        parse()
        spinner.stop()

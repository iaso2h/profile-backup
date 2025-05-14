import config
import backup

import psutil
import os
import beaupy
import send2trash
from typing import Tuple
from enum import Enum
from pathlib import Path

print = backup.console.print

def findRemovableDrive() -> Tuple[list[str], list[str]]:
    # Credit: https://stackoverflow.com/questions/12266211/python-windows-list-only-usb-removable-drives
    removableDriveRaw = [i.mountpoint for i in psutil.disk_partitions() if "removable" in i.opts]
    removableDriveRich = list(map(lambda i: "[blue]" + i + "[/blue]", removableDriveRaw))
    return removableDriveRaw, removableDriveRich

def findAllDrives() -> list[str]:
    import psutil
    drps = psutil.disk_partitions()
    return list(map(lambda i: i[:1], [dp.device for dp in drps if dp.fstype == 'NTFS']))

def keyboardInterruptExit() -> None:
    print("[red]Interrupt by user[/red]")
    raise SystemExit(1)


def abortExit() -> None:
    print("[red]Abort by user[/red]")
    raise SystemExit(1)


cwd = os.getcwd()
beaupy.Config.raise_on_interrupt = True
beaupy.Config.raise_on_escape    = True


def parseBackupFiles():

    if not backup.DRYRUN:
        os.makedirs(str(backup.DESTPATH), exist_ok=True)

    for p in config.profileConfigs:
        if p.ticked:
            p.backup()
    backup.Profile.reportBackupCount()

    # Print out the files to delete in synchronizing mode
    if not backup.DRYRUN and backup.COPYSYNC and backup.Profile.syncFilesToDelete != {}:
        obsoleteNonEmptyChk = False
        for software in backup.Profile.syncFilesToDelete.keys():
            for versionStr in backup.Profile.syncFilesToDelete[software].keys():
                print(f"\n[green bold]{software} {versionStr}[/green bold]:")
                for f in backup.Profile.syncFilesToDelete[software][versionStr]:
                    print(f"  [red]{f}[/red]")

        # Prompt to delete the files
        if obsoleteNonEmptyChk:
            ans = False
            try:
                ans = beaupy.confirm(
                        "[white]The files listed above is going to be deleted, are you sure?[/white]",
                        yes_text="[blue]Yes[/blue]",
                        no_text="[blue]No[/blue]",
                        default_is_yes=False,
                    )
            except KeyboardInterrupt:
                keyboardInterruptExit()
            except beaupy.Abort:
                abortExit()
            except Exception as e:
                print(e)
                SystemExit(1)

            if ans:
                for software in backup.Profile.syncFilesToDelete.keys():
                    for versionStr in backup.Profile.syncFilesToDelete[software].keys():
                        send2trash.send2trash(backup.Profile.syncFilesToDelete[software][versionStr])

                print("[bold purple]All obsolete files/directories have been removed[/bold purple]")


def program() -> None:
    print(findRemovableDrive())
    # Select copy mmode
    print("[white]Select copy mode[/white]")

    copyMode = ["Sync", "Update"]
    ans = 0
    try:
        ans = beaupy.select(copyMode, return_index=True) # type: ignore
    except KeyboardInterrupt:
        keyboardInterruptExit()
    except beaupy.Abort:
        abortExit()
    except Exception as e:
        print(e)
        SystemExit(1)
    if ans == 0:
        backup.COPYSYNC = True
    else:
        backup.COPYSYNC = False


    # Select profile
    print("[white]Select profile to back up:[/white]")
    try:
        # Show profiles that has been enabled and make all profiles ticked by default
        softwareChoice = beaupy.select_multiple(
                list(map(lambda b: str(b.name), backup.Profile.profileEnabledList)),
                tick_character = '■',
                ticked_indices = list(range(len(backup.Profile.profileEnabledList))),
                minimal_count  = 1,
        )

        # Update ticked state for enabled Backup objects
        for i in config.profileConfigs:
            if i.name in softwareChoice:
                i.ticked = True
            else:
                i.ticked = False
        backup.Profile.updateTickedList()

    except KeyboardInterrupt:
        keyboardInterruptExit()
    except beaupy.Abort:
        abortExit()
    except Exception as e:
        print(e)
        SystemExit(1)


    # Select destination path
    dstPathsRich, dstPathsRaw = findRemovableDrive()
    dstPathsRich.append(f"[blue]Current Working Directory([yellow]{cwd}\\\\[/yellow][blue])[/blue]") # len()-2 index
    dstPathsRich.append("[blue]Custom Path[/blue]") # len()-1 index
    alldrives  = findAllDrives()
    ans = len(dstPathsRich) - 2 # Default value destination
    print("[white]Choose the path to store the files:[/white]")
    try:
        ans = beaupy.select(dstPathsRich, return_index=True) # type: ignore
    except KeyboardInterrupt:
        keyboardInterruptExit()
    except beaupy.Abort:
        abortExit()
    except Exception as e:
        print(e)
        SystemExit(1)

    # Get destination path
    if ans == len(dstPathsRich) - 2:
        # Current working directory
        backup.DESTPATH = Path(cwd, "Profiles")
    elif ans == len(dstPathsRich) - 1:
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
                        backup.DESTPATH = Path(dst, "Profiles")
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
        backup.DESTPATH = Path(str(dstPathsRaw[ans]), "Profiles") # type: ignore


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


    parseBackupFiles()

    if backup.Profile.totalBackupCount > 0 and backup.DRYRUN:
        backup.Profile.totalBackupCount = 0 # Rest the total count
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
        parseBackupFiles()

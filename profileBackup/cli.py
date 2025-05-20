import config
import backup
import util

import psutil
import os
import beaupy
import send2trash
from typing import Tuple
from pathlib import Path

print = util.print

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


beaupy.Config.raise_on_interrupt = True
beaupy.Config.raise_on_escape    = True


def parseBackupFiles(profilesChosen: list[backup.Profile]): #  {{{

    if not config.DRYRUN:
        os.makedirs(str(config.DESTPATH), exist_ok=True)


    config.EXPORTLOG = True
    for profile in profilesChosen:
        if not profile.enabled:
            continue


        print(f"\n{util.getTimeStamp()}[white]Checking up [green bold]{profile.profileName}[/green bold][/white]...")
        for category in profile.categories:
            if category.enabled:
                category.backup()
        print(f"{util.getTimeStamp()}[white]{profile.foundFileMessage} [purple bold]{profile.backupCount}[/purple bold] files of [blue bold]{util.humanReadableSize(profile.backupSize)}[/blue bold] for [green bold]{profile.profileName}[/green bold].[/white]")
    print(f"\n{util.getTimeStamp()}[white]{backup.Profile.foundFileMessage} [purple bold]{backup.Profile.totalBackupCount}[/purple bold] files of [blue bold]{util.humanReadableSize(backup.Profile.totalBackupSize)}[/blue bold] for [green bold]{profilesChosen}[/green bold].[/white]\n\n\n\n\n")
    # TODO: including deletion message during synchronization
    config.EXPORTLOG = False

    # Print out the files to delete in synchronizing mode
    if not config.DRYRUN and config.COPYSYNC and backup.Category.syncFilesToDelete != {}:
        obsoleteNonEmptyChk = False
        for software in backup.Category.syncFilesToDelete.keys():
            for parentSrcPath in backup.Category.syncFilesToDelete[software].keys():
                print(f"\n[green bold]{software} {parentSrcPath}[/green bold]:")
                for f in backup.Category.syncFilesToDelete[software][parentSrcPath]:
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
                for software in backup.Category.syncFilesToDelete.keys():
                    for parentSrcPath in backup.Category.syncFilesToDelete[software].keys():
                        send2trash.send2trash(backup.Category.syncFilesToDelete[software][parentSrcPath])

                print("[bold purple]All obsolete files/directories have been removed[/bold purple]")
# }}}


def program() -> None:
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

    config.COPYSYNC = True if ans == 0 else False

    # Select profile
    print("[white]Select profile to back up:[/white]")
    profileChoices = [p.name for p in backup.Profile.profileDict if p.enabled]
    try:
        profilesChosen = beaupy.select_multiple(
                profileChoices,
                tick_character = '■',
                ticked_indices = list(range(len(profileChoices))),
                minimal_count  = 1,
        )

    except KeyboardInterrupt:
        keyboardInterruptExit()
    except beaupy.Abort:
        abortExit()
    except Exception as e:
        print(e)
        SystemExit(1)


    # Select destination path
    dstPathsRich, dstPathsRaw = findRemovableDrive()
    dstPathsRich.append(f"[blue]Current Working Directory([yellow]{config.CWD}\\\\[/yellow][blue])[/blue]") # len()-2 index
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
        config.DESTPATH = Path(config.CWD, "Profiles")
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
                                config.DESTPATH = Path(dst, "Profiles")
                                break
                            else:
                                abortExit()
                    else:
                        config.DESTPATH = Path(dst, "Profiles")
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
        config.DESTPATH = Path(str(dstPathsRaw[ans]), "Profiles") # type: ignore


    # Ask if to run in dry mode
    try:
        config.DRYRUN = beaupy.confirm(
                f"Backing up file in [yellow]{config.DESTPATH}[/yellow]. Do you want to run in [purple bold]dry run mode[/purple bold] first?",
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


    parseBackupFiles(profilesChosen) # type: ignore

    if backup.Category.totalBackupCount > 0 and config.DRYRUN:
        backup.Category.totalBackupCount = 0 # Rest the total count
        confirmRun(profilesChosen) # type: ignore
    else:
        print("\n[purple bold]Everything is up-to-date! You're good to go.[/purple bold]")


def confirmRun(profilesChosen: list[backup.Profile]):
    try:
        config.DRYRUN = not beaupy.confirm(
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

    if not config.DRYRUN:
        parseBackupFiles(profilesChosen) # type: ignore

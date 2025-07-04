import psutil
import os
import beaupy
from typing import Tuple
from pathlib import Path

from . import backup
import config
import util

print = util.print

def findRemovableDrive() -> Tuple[list[str], list[str]]: #  {{{
    """
    Identifies all removable drives connected to the system.

    Uses psutil to detect USB/removable drives and returns both raw and formatted
    versions of the drive paths.

    Returns:
        Tuple[list[str], list[str]]: A tuple containing:
            - List of formatted drive paths (with ANSI color codes)
            - List of raw drive paths

    Note:
        Credit: https://stackoverflow.com/questions/12266211/python-windows-list-only-usb-removable-drives
    """
    removableDriveRaw = [i.mountpoint for i in psutil.disk_partitions() if "removable" in i.opts]
    removableDriveRich = list(map(lambda i: "[blue]" + i, removableDriveRaw))
    return removableDriveRich, removableDriveRaw
    # }}}

def findAllDrives() -> list[str]: # {{{
    """
    Lists all NTFS-formatted drives on the system.

    Uses psutil to detect all disk partitions and filters for NTFS file systems.

    Returns:
        list[str]: List of drive letters (e.g., ['C', 'D', 'E'])

    Note:
        Only returns the first character of each device path (the drive letter).
    """
    import psutil
    drps = psutil.disk_partitions()
    return list(map(lambda i: i[:1], [dp.device for dp in drps if dp.fstype == 'NTFS']))
# }}}

def keyboardInterruptExit() -> None: # {{{
    """
    Handles keyboard interrupt (Ctrl+C) events.

    Prints a formatted error message and exits the program with status code 1.
    """
    print("[red]Interrupt by user[/red]")
    raise SystemExit(1)
# }}}

def abortExit() -> None: # {{{
    """
    Handles user-initiated abort operations.

    Prints a formatted error message and exits the program with status code 1.
    Typically triggered when the user selects to abort an operation.
    """
    print("[red]Abort by user[/red]")
    raise SystemExit(1)
# }}}


# Configurations for beaupy
beaupy.Config.raise_on_interrupt = True
beaupy.Config.raise_on_escape    = True


def deleteObsoleteDstFiles(): # {{{
    syncFileToDeleteChk = False
    for profileName, syncFilesToDeleteByParent in backup.FileCategory.syncFilesToDelete.items():
        for parentPath, fileToDelete in syncFilesToDeleteByParent.items():
            if fileToDelete:
                if not syncFileToDeleteChk:
                    syncFileToDeleteChk = True
                    print(f"\n{util.getTimeStamp()}[red bold]Files listed below doesn't exist in source paths.[/red bold]")

                print(f"[white]{util.getTimeStamp()}[green bold]{profileName}[/green bold] [yellow]{parentPath}:[/yellow][/white]")
                for f in fileToDelete:
                    fileSize = util.humanReadableSize(f.stat().st_size)
                    print(f"  [red]{f}[/red][blue]({fileSize})[/blue]")

    # Prompt to delete the files
    if not syncFileToDeleteChk:
        return

    ans = False
    try:
        ans = beaupy.confirm(
                "[white]The files in destination listed above aren't found in the source paths. Do you want to delete them?[/white]",
                yes_text="[blue]Yes[/blue]",
                no_text="[blue]No[/blue]",
                default_is_yes=False,
            )
    except (KeyboardInterrupt, beaupy.Abort) as e:
        if isinstance(e, KeyboardInterrupt):
            keyboardInterruptExit()
        else:
            abortExit()


    if not ans:
        return


    deleteCountTotal = 0
    deleteSizeTotal  = 0
    deleteCountByProfile = 0
    deleteSizeByProfile  = 0
    print("\n\n\n\n\n")
    config.EXPORTLOG = True

    for profileName, syncFilesToDeleteByParent in backup.FileCategory.syncFilesToDelete.items():
        if not any(list(syncFilesToDeleteByParent.values())):
            continue

        print(f"  [red]Deleting files/dirs from Profile {profileName}[/red]")
        deleteCountByProfile = 0
        deleteSizeByProfile  = 0
        for parentPath, fileToDelete in syncFilesToDeleteByParent.items():
            print(f"    [red]Deleting files/dirs from Profile {profileName} [yellow]{parentPath}[/yellow][/red]")
            deleteCountByParentDst = 0
            deleteSizeByParentDst  = 0
            if fileToDelete:
                for f in fileToDelete:
                    deleteCountByParentDst += 1
                    if f.is_dir():
                        os.rmdir(f)
                    else:
                        deleteSizeByParentDst += f.stat().st_size
                        os.remove(f)

                deleteSizeByProfile += deleteSizeByParentDst
                deleteCountByProfile += deleteCountByParentDst
                print(f"    [red][purple]{deleteCountByParentDst}[/purple] files/dirs of [blue]{util.humanReadableSize(deleteSizeByParentDst)}[/blue] deleted from: [yellow]{parentPath}[/yellow][/red]")


        deleteCountTotal += deleteCountByProfile
        deleteSizeTotal += deleteSizeByProfile
        print(f"  [red][purple]{deleteCountByProfile}[/purple] files/dirs of [blue]{util.humanReadableSize(deleteSizeByProfile)}[/blue] deleted from Profile: {profileName}[/red]")


    print(f"[bold red][purple bold]{deleteCountTotal}[/purple bold] obsolete files/directories of [blue bold]{util.humanReadableSize(deleteSizeTotal)}[/blue bold] have been removed[/bold red]")

    config.EXPORTLOG = False
# }}}

def parseBackupFiles(profileNamesChosen: list[backup.Profile]): #  {{{

    if not config.DRYRUN:
        os.makedirs(str(config.DESTPATH), exist_ok=True)


    config.EXPORTLOG = True

    profileNamesValid = [] # Chosen profiles names that have backup files being processed
    for profileName in profileNamesChosen:
        profile = backup.Profile.profileDict[profileName]
        if not profile.enabled:
            continue

        bufferOutput = []
        bufferOutput.append(f"\n{util.getTimeStamp()}[white]Checking up [green bold]{profile.profileName}[/green bold][/white]...")
        for category in profile.categories:
            # Only backup enabled categories because some categories may not have any backup files during pre-processing and turned disabled
            if not category.enabled:
                continue
            category.backup(bufferOutput)

        # Remove some buffer report if there is no backup files found
        if profile.backupCount > 0:
            bufferOutput.append("\n{}[white]{} [purple bold]{}[/purple bold] files/registry sets of [blue bold]{}[/blue bold] for [green bold]{}[/green bold].[/white]".format(
                    util.getTimeStamp(),
                    profile.foundFileMessage,
                    profile.backupCount,
                    util.humanReadableSize(profile.backupSize),
                    profile.profileName
                )
            )

            profileNamesValid.append(profileName)
        else:
            bufferOutput.append("\n{}[white]Skipped [purple bold]{}[/purple bold] files/registry sets of [blue bold]{}[/blue bold] for [green bold]{}[/green bold].[/white]".format(
                    util.getTimeStamp(),
                    profile.backupCount,
                    util.humanReadableSize(profile.backupSize),
                    profile.profileName
                )
            )
            bufferOutput.append(f"\n{util.getTimeStamp()}[white]Skipped [purple bold]{profile.backupCount}[/purple bold] files/registry sets of [blue bold]{util.humanReadableSize(profile.backupSize)}[/blue bold] for [green bold]{profile.profileName}[/green bold].[/white]")
            bufferOutput[0] = bufferOutput[0].replace("Checking up", "Skipped")

        # Print out the report
        for line in bufferOutput:
            print(line)

    print(
        "\n{}[white]{} [purple bold]{}[/purple bold] files of [blue bold]{}[/blue bold] for [green bold]{}[/green bold] out of [purple bold]{}[/purple bold] chosen profiles.[/white]\n\n\n\n\n".format(
            util.getTimeStamp(),
            backup.Profile.foundFileMessage,
            backup.Profile.totalBackupCount,
            util.humanReadableSize(backup.Profile.totalFileBackupSize),
            profileNamesValid,
            len(profileNamesChosen)
        )
    )

    config.EXPORTLOG = False
# }}}

def endDryrunPrompt(profilesChosen: list[backup.Profile]): # {{{
    try:
        config.DRYRUN = not beaupy.confirm(
            f"End the [purple bold]dry run mode[/purple bold] and confirm the whole backup process?",
            yes_text="[blue]Yes[/blue]",
            no_text="[blue]No[/blue]",
            default_is_yes=True,
        )
    except (KeyboardInterrupt, beaupy.Abort) as e:
        if isinstance(e, KeyboardInterrupt):
            keyboardInterruptExit()
        else:
            abortExit()

    if not config.DRYRUN:
        backup.Profile.updateFoundFileMessage()
        for profileName in backup.Profile.profileDict.keys():
            backup.Profile.profileDict[profileName].backupCount = 0
            backup.Profile.profileDict[profileName].backupSize = 0
        backup.Profile.totalBackupCount = 0
        backup.Profile.totalFileBackupSize = 0
        parseBackupFiles(profilesChosen)
# }}}

def program() -> None: # {{{
    # Select copy mode
    print("[white]Select copy mode[/white]")

    copyMode = ["Sync", "Update"]
    ans = 0
    try:
        ans = beaupy.select(copyMode, return_index=True) # type: ignore
    except (KeyboardInterrupt, beaupy.Abort) as e:
        if isinstance(e, KeyboardInterrupt):
            keyboardInterruptExit()
        else:
            abortExit()

    config.COPYSYNC = True if ans == 0 else False

    # Select profile
    print("[white]Select profile to back up:[/white]")
    profileNamePoll = [k for k, v in backup.Profile.profileDict.items() if v.enabled]
    try:
        profileNamesChosen = beaupy.select_multiple(
            profileNamePoll,
            tick_character = '■',
            ticked_indices = list(range(len(profileNamePoll))),
            minimal_count  = 1,
        )

    except (KeyboardInterrupt, beaupy.Abort) as e:
        if isinstance(e, KeyboardInterrupt):
            keyboardInterruptExit()
        else:
            abortExit()


    # Select destination path
    dstPathsRich, dstPathsRaw = findRemovableDrive()
    dstPathsRich.append(f"[blue]Current Working Directory([yellow]{config.CWD}\\\\[/yellow])[/blue]") # len()-2 index
    dstPathsRich.append("[blue]Custom Path[/blue]") # len()-1 index
    alldrives  = findAllDrives()
    ans = len(dstPathsRich) - 2 # Default value destination
    print("[white]Choose the path to store the files:[/white]")
    try:
        ans = beaupy.select(dstPathsRich, return_index=True) # type: ignore
    except (KeyboardInterrupt, beaupy.Abort) as e:
        if isinstance(e, KeyboardInterrupt):
            keyboardInterruptExit()
        else:
            abortExit()

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
            except (KeyboardInterrupt, beaupy.Abort) as e:
                if isinstance(e, KeyboardInterrupt):
                    keyboardInterruptExit()
                else:
                    abortExit()
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
    except (KeyboardInterrupt, beaupy.Abort) as e:
        if isinstance(e, KeyboardInterrupt):
            keyboardInterruptExit()
        else:
            abortExit()
    backup.Profile.updateFoundFileMessage()


    parseBackupFiles(profileNamesChosen) # type: ignore

    if config.DRYRUN:
        if backup.FileCategory.totalBackupCount > 0:
            backup.FileCategory.totalBackupCount = 0 # Rest the total count
            endDryrunPrompt(profileNamesChosen) # type: ignore

        deleteObsoleteDstFiles()
    else:
        deleteObsoleteDstFiles()
        print("\n[purple bold]Everything is already up-to-date! You're good to go.[/purple bold]")
# }}}
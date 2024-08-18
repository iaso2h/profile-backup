# Introduction
Back up profiles from all kind of softwares. The location of these software profiles somewhat resemble dotfiles on Linux but they are tend to be scattered at different locations, hence I wrote this app to tackle the chaos. Right now it's built on Windows only and targetting softwares on Windows.
# Supported software
* 3ds Max
* Blender
* AutoCAD
* Photoshop
* Everything
* TubesT
# Roadmap
* ~~Recursive copy~~
* ~~Make it into an interactive CLI app~~
* ~~Store the software info into JSON file~~(It's non-goal, because you won't be able to set up a python function to maximize the versibility in that way)
* ~~Update/Sync mode~~
* Restore
* Move overrided file to trash before restoring
* Automatically adapt user name on different computer or operating system
* Linux support
* Edit file by condition or by platform
* dotfiles support
# How to use
## 1. Install dependencies
* psutil(for drive detection)
* beaupy(for interactive CLI)
* pytest(for test)
* send2trash(safely delete files in synmode)

Recommend installing all dependencies via conda
```shell
conda create --name <nameOfEnv> --file requriements.txt
```
## 2. Run the app in terminal
```shell
python profileBackup
```

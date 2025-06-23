# Introduction
Back up profiles from all kind of profiles. The location of these profiles somewhat resemble dotfiles on Linux but they are tend to be scattered at different locations, hence I wrote this app to tackle the chaos. Right now it's built on Windows only and targetting softwares on Windows.
# Supported software
* 3ds Max
* SolidWorks
* Blender
* AutoCAD
* Photoshop
* Everything
* TubesT
# Roadmap
* Include recipe:
    * Vivaldi broswer preferences and highlighted note data
    * WPS
    * Foxit
    * Rime
    * Windows themes
    * Anytext Searcher
    * Adobe series?
    * Jetbrain editor?
    * CAXA
    * FreeCAD
    * Kate
    * MindManager
    * OBS studio
    * Obsidian
    * PixPin
    * AutoHotkey
    * Wezterm
    * MPV
    * XnViewMP
    * Rime

* ~~Recursive copy~~
* ~~Make it into an interactive CLI app~~
* ~~Store the software info into JSON file~~(It's non-goal, because you won't be able to set up a python function to maximize the versibility in that way)
* ~~Update/Sync mode~~
* Global balcklist
* Restore
* Move overrided file to trash before restoring
* Automatically adapt user name on different computer or operating system
* Linux support
* Edit file by condition or by platform
* dotfiles support
# How to use
## 1. Install dependencies
Recommend installing all dependencies via conda
```shell
conda env create -f environment.yml
```
## 2. Run the app in terminal
```shell
python profileBackup
```

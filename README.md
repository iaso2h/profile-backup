# Introduction
Back up profiles from all kind of profiles. The location of these profiles somewhat resemble dotfiles on Linux but they are tend to be scattered at different locations, hence I wrote this app to tackle the chaos. Right now it's built on Windows only and targetting softwares on Windows.
# Roadmap
* Recipe TODO :
    * Vivaldi broswer preferences and highlighted note data
    * WPS
    * Foxit
    * Windows themes
    * Adobe series?
    * CAXA
    * FreeCAD
    * OBS studio
    * Obsidian
    * AutoHotkey

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
# Setbacks
* Registry backup isn't test on 32-bit Windows.
# How to use
## 1. Install dependencies and activate conda environment
```shell
conda env create -f environment.yml
conda activate profileBackup
```
## 2. Run the app in terminal
```shell
python -m profileBackup
```
## 3. Test and Debug：
```shell
python -m profileBackup --debug
```
## 4. Update dependencies：
```shell
conda env update -f environment.yml
```

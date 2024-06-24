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
* ~~Make it into an interactive CLI app~~
* ~~Store the software info into JSON file~~(Non-goal. You cannot pass in customized python function)
* Automatically adapt user name on different computer
# How to use
## 1. Install dependencies
* psutil(for drive detection)
* icecream(for debug print)
* beaupy(for interactive CLI)

You can install all dependencies via pip or conda(recommanded)
```shell
pip install -r requirements.txt
```
or
```shell
conda create --name <nameOfEnv> --file requriements.txt
```
## 2. Run the app in terminal
```shell
python profileBackup
```

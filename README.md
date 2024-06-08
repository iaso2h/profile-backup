# Introduction
Back up profiles from all kind of softwares. The location of these software profiles somewhat resemble dotfiles on Linux but they are tend to be scattered at different locations, hence I wrote this app to tackle the chaos. Right now it's built on Windows only and targetting softwares on Windows.
# Supported software
* 3ds Max
* AutoCAD
# Roadmap
* ~~Make it into an interactive CLI app~~
* Store the software info into JSON file
* Automatically adapt user name on different computer
# How to use
## Dependencies
* psutil(for drive detection)
* icecream(for debug print)
* beaupy(for interactive CLI)
You install all dependencies via pip or conda(recommanded)
```shell
pip install -r requirements.txt
```
or
```shell
conda create --name <nameOfEnv> --file requriements.txt
```
## Run the app in terminal
```shell
python profileBackup
```

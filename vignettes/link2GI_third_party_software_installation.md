---
author: "Chris Reudenbach"
title: "Installation guide for 3rd party Software related to link2GI"
date: "2020-12-04"
editor_options:
  chunk_output_type: console
output:
  html_document: 
    theme: united
    toc: yes
  rmarkdown: default
  pdf_document:
    latex_engine: xelatex
    toc: yes
urlcolor: blue
vignette: >
  %\VignetteIndexEntry{Installation guide for third party software related to link2GI}
  %\VignetteEncoding{UTF-8}{inputenc}\
  %\VignetteEngine{knitr::knitr}
---



# Vignette info
The vignette is a slighly adapted version of the installation guide of the archived RQGIS package. Big thanks to [Patrick Schratz](https://github.com/pat-s) and [Jannis Muenchow](https://github.com/jannes-m).


This vignette guides you through the installation process of QGIS, GRASS- and SAGA-GIS as well as Orfeo Toolbox on three different platforms (Windows, Mac, Linux). 
Following the instructions should ensure that link2GI will work properly. 
With the exception of SAGA, we recommend to use the latest stable version of all software packages.


Overall,link2GI allows access to more than some thousands of reliable and well known geoalgorithms.
However, please note that the number of geoalgorithms you can use linking with link2GI depends on the platform, the system architecture, the selection of installed third-party providers and software package versions.

# Windows

There are at least two options to install QGIS on Windows. The first option is the Standalone Installer from the [QGIS installer page](https://www.qgis.org/en/site/forusers/download.html). 
This will install QGIS along with the third-party providers GRASS and SAGA. 
However, if you would like to use even more third-party providers (e.g., GDAL, GEOS, Taudem, OTB, PostGIS, etc.), it is strongly recommended to use the OSGeo4W Network installer. 
This installer is available on the [QGIS installer page](https://www.qgis.org/en/site/forusers/download.html) as well as on the [OSGeo4W-website](http://trac.osgeo.org/osgeo4w/). 

To install OSGeo4W, the easiest way is to to use the Express installation:

![](https://raw.githubusercontent.com/r-spatial/link2GI/master/figures/00_express.PNG)


We are sorry, that our installer uses a German dialog.
Apparently, OSGeo4w automatically uses the language in accordance with the system locale. Nevertheless, it should be easy enough to follow our steps.

1. Select "Express Installation" 
2. Next check "QGIS, GRASS and GDAL"
3. Start the installation.

This installation is more or less equivalent to the standalone QGIS-installation.
However, to have the full choice of available geoalgorithms provided by QGIS, we recommend to use the  advanced settings of the OSGeo4W Network installer:

1. Run the OSGeo4W setup and choose the advanced installation. 

![](https://raw.githubusercontent.com/r-spatial/link2GI/master/figures/01_initial_setup.PNG)

2. Subsequently, accept the default settings until you reach the “Select Packages” window.

![](https://raw.githubusercontent.com/r-spatial/link2GI/master/figures/04_select_packages.PNG)

3. After expanding the command line utilities, we choose several commandline tools such as gdal and python-core (of course, you can select other components as well):

![](https://raw.githubusercontent.com/r-spatial/link2GI/master/figures/05_command_line_utilties.PNG)

Additionally, we choose the Desktop-GIS GRASS 7, the Orfeo Toolbox, SAGA, and QGIS (even if not directly supported by link2GI right now). Here, we install both the most recent QGIS 3.x version as well as the long-term release. ** Do not install QGIS 2.x**

4. We leave the rest as is, click "Next" and wait for the OSGeo4W-suggestions which we accept.

![](https://raw.githubusercontent.com/r-spatial/link2GI/master/figures/08_suggestions.PNG)

Clicking "Next" once more will start the download and installation process which might take a while.

If you wish to to modify, uninstall or update some of the installed components, you might do so later on by running `../OSGeo4W/bin/osgeo4w-setup.exe`. 
Alternatively, you can download and run again the latest [OSGeo4W-executable](http://trac.osgeo.org/osgeo4w/).

If you additionally want to use the **LiDAR processing tools** (LAStools), please follow the steps found [here](https://rapidlasso.com/2013/09/29/how-to-install-lastools-toolbox-in-qgis/).

# Linux

## Ubuntu

### QGIS (and GRASS)
If you install QGIS with the built-in software managers, you will most likely get an outdated QGIS version. 
To install more recent QGIS releases, we recommend to follow the installation instructions from the [QGIS installers website](https://www.qgis.org/en/site/forusers/alldownloads.html). 

Here, we just describe exemplarily the QGIS installation procedure under Debian/Ubuntu following the description found [here](https://www.qgis.org/en/site/forusers/alldownloads.html#debian-ubuntu).
link2GI should work as well with Linux derivatives other than Ubuntu. However, just Ubuntu , Debian and Manjaro are tested.

Open a terminal window. First of all, make sure to remove QGIS and GRASS packages you may have installed before from other repositories:

<!--http://gis.stackexchange.com/questions/167472/qgis-2-8-ubuntu-14-04-installation-issues-terminal-command-attempts-to-install-->
```sh 
sudo apt-get --purge remove qgis
sudo apt autoremove --purge-remove 
sudo apt autoremove
sudo apt-get update
```

Next add the correct repository to `/etc/apt/sources.list`.
Here, we use the current long-term release (3.10):

**QGIS 3.10.x for Ubuntu 20.04**



```sh
# install necessary tools
sudo apt install gnupg software-properties-common

#add the required keys
wget -qO - https://qgis.org/downloads/qgis-2020.gpg.key | sudo gpg --no-default-keyring --keyring gnupg-ring:/etc/apt/trusted.gpg.d/qgis-archive.gpg --import

# verify the key 
gpg --fingerprint 51F523511C7028C3

# add the repository
# if there are problems 
sudo add-apt-repository "deb https://qgis.org/debian lsb_release -c -s main"

# if there are problems use the hard-wired "focal" release
sudo add-apt-repository "deb https://qgis.org/debian focal main"


```

After that, we can install QGIS and GRASS saga and Orfeo Toolbox as follows:

```sh
# install qgis grass otb saga

sudo apt update
sudo apt install qgis python-qgis qgis-plugin-grass grass saga otb-bin
```

If you would like to use another SAGA version, you need to compile it yourself (see [here](https://sourceforge.net/p/saga-gis/wiki/Compiling%20SAGA%20on%20Linux/)). 
Please note, however, that QGIS currently only supports the SAGA LTR 2.3.x as far as we know.


## Arch Linux

You can install various QGIS, SAGA and GRASS versions from the [Arch User Repository](https://aur.archlinux.org/). 
We recommend to use the [yaourt](https://wiki.archlinux.de/title/Yaourt) package manager.

## Fedora

You can install current stable QGIS, SAGA and GRASS GIS versions via the standard software package manager:

```sh
sudo dnf install qgis-python qgis qgis-grass saga grass-gui grass-libs
```

# Mac OSX

## SAGA

There is no binary install of SAGA for macOS.
We recommend to use the bottle installation from `homebrew`:

```sh
# brew tap osgeo4mac
brew install saga-gis-lts
brew link saga-gis-lts --force
```

If you do not link SAGA with force, QGIS will not be able to detect SAGA.

Alternatively, you can compile SAGA from source from the [SAGA website](https://sourceforge.net/projects/saga-gis/files/). 
However, this is tedious and QGIS2 only supports the SAGA LTS version.

## GRASS

You can install GRASS6 and GRASS7 via `homebrew`:

```sh
# brew tap osgeo4mac
brew install grass6 grass7
```

The binary GRASS installation can be found [here](https://grass.osgeo.org/download/software/mac-osx/). 
However, we recommend to use the `homebrew` approach.

When installing GRASS7 independently of QGIS via `homebrew`, please make sure to install it **before** you have installed QGIS.
Only this way, the path for the QGIS processing plugin will be updated.
Otherwise, the GRASS installation will work, however, `GRASS7Utils.grassPath()`, a QGIS function that links to the GRASS installation, gives back a non-existing path such as `/Applications/Grass-7.0.app/Contents/MacOS` which in turn prevents GRASS algorithms from working from within QGIS.

## QGIS

Two options exist installing QGIS on macOS

1. Using `homebrew` (**recommended**)

```
# brew tap osgeo4mac
brew install qgis3
```

Check `brew info qgis3` for more available options. 
However, if you use them, QGIS will be compiled from source which may take > 30 min (depending on your system).
Otherwise, pre-built bottles (= binaries) will be used which speeds up the installation process a lot.

**Note:** Make sure to install SAGA and GRASS before QGIS so that QGIS finds the correct paths.

2. Install QGIS binary from Kyngchaos

Homebrew also supports [GUI installations](https://caskroom.github.io/). 
QGIS can be installed using 

```sh
brew cask install qgis
```

from the command line if homebrew is installed and configured correctly. 
If you do not (want to) use `homebrew`, you can install the QGIS binary from [https://www.qgis.org/en/site/forusers/download.html](https://www.qgis.org/en/site/forusers/download.html). 
This is exactly the same as calling `brew cask install qgis`.

If you choose this option, you will get the following error messages during QGIS processing

```
QSqlDatabase: QSQLITE driver not loaded
QSqlDatabase: available drivers:
QSqlQuery::prepare: database not open
[1] "ERROR: Opening of authentication db FAILED"
[2] "WARNING: Auth db query exec() FAILED"
```

These messages **DO NOT affect link2GI usage**. 

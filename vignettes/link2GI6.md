---
author: "Chris Reudenbach"
title: "Installation guide for link2GI related Software"
date: "2023-10-30"
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
  %\VignetteIndexEntry{Installation guide for link2GI related Software}
  %\VignetteEncoding{UTF-8}{inputenc}\
  %\VignetteEngine{knitr::knitr}
---



# Vignette Info
The vignette is a slightly modified version of the installation guide of the archived RQGIS package. Many thanks to [Patrick Schratz](https://github.com/pat-s) and [Jannis Muenchow](https://github.com/jannes-m).


This vignette guides you through the installation process of QGIS, GRASS and SAGA-GIS as well as Orfeo Toolbox and GDAL on three different platforms (Windows, Mac, Linux). 
Following the instructions should ensure that link2GI works properly. 
With the exception of SAGA (SAGA is quite idiosyncratic in terms of interface compatibility), we recommend using the latest stable version of all software packages.


Overall, link2GI provides access to more than a few thousand reliable and well-known geoalgorithms. Please note, however, that the number of geoalgorithms you can use with link2GI depends on the platform, the system architecture, the selection of installed third-party providers, and the software package versions.


# Windows

## QGIS, GRASS and SAGA
There are at least two ways to install QGIS on Windows. The first option is the standalone installer from the [QGIS installer page] (https://www.qgis.org/en/site/forusers/download.html). 
This will install QGIS along with the third party GRASS and SAGA. 
If you want to use even more third party applications (e.g. GDAL, GEOS, Taudem, OTB, PostGIS, etc.) it is strongly recommended to use the OSGeo4W network installer. 
This installer is available on the [QGIS installer page](https://www.qgis.org/en/site/forusers/download.html) as well as on the [OSGeo4W website](https://trac.osgeo.org/osgeo4w/). 

**NOTE: Do not install QGIS 2.x**.

The easiest way to install OSGeo4W is to use the express installation:

![](https://raw.githubusercontent.com/r-spatial/link2GI/master/figures/00_express.PNG)


Sorry for the German dialogs.  However, it should be easy enough to follow our steps.

1. Select "Express Installation 
2. Next select "QGIS, GRASS and GDAL
3. Start the installation.

This installation is more or less the same as the standalone QGIS installation.
However, to get the full range of geoalgorithms available in QGIS, we recommend using the advanced settings of the OSGeo4W Network installer:

1. Run the OSGeo4W setup and select the advanced installation. 

![](https://raw.githubusercontent.com/r-spatial/link2GI/master/figures/01_initial_setup.PNG)

2. Accept the default settings until you reach the "Select Packages" window.

![](https://raw.githubusercontent.com/r-spatial/link2GI/master/figures/04_select_packages.PNG)

3. After expanding the command line utilities, we select several command line tools such as gdal and python-core (of course, you can select other components as well):

![](https://raw.githubusercontent.com/r-spatial/link2GI/master/figures/05_command_line_utilties.PNG)

Additionally we choose the Desktop-GIS GRASS, the Orfeo Toolbox, SAGA and QGIS (even if they are not directly supported by link2GI at the moment). We install the latest QGIS 3.x version as well as the long term release. 



4. Leave the rest as it is, click "Next" and wait for the OSGeo4W suggestions, which we accept.

![](https://raw.githubusercontent.com/r-spatial/link2GI/master/figures/08_suggestions.PNG)

Clicking "Next" again will start the download and installation process, which may take a while.

If you want to change, uninstall or update some of the installed components, you can do so later by running `../OSGeo4W/bin/osgeo4w-setup.exe`. 
Alternatively, you can download the latest [OSGeo4W-executable] (https://trac.osgeo.org/osgeo4w/) and run it again.

If you also want to use the **LiDAR processing tools** (LAStools), please follow the steps found [here](https://rapidlasso.de/how-to-install-lastools-toolbox-in-qgis/).

## Orfeo Toolbox

Please [download](https://www.orfeo-toolbox.org/download/) the latest (currently OTB-7.2.0-Win64) or a suitable version of the Orfeo Toolbox software. Then follow the OTB team's advice:

"We provide standalone binaries for Windows that do not require administrative privileges. Download the archive below (32 or 64 bit) and extract it to a location of your choice. Double-click monteverdi.bat to start Monteverdi, or mapla.bat for the OTB application browser. Please refer to the [CookBook](https://www.orfeo-toolbox.org/CookBook/Installation.html#windows) for detailed installation instructions.

# Linux

## Ubuntu

If you install QGIS using the built-in software managers, you will most likely end up with an outdated version of QGIS. 
To install more recent versions of QGIS, we recommend following the installation instructions from the [QGIS installers website] (https://www.qgis.org/en/site/forusers/download.html). 

Here we only describe the installation of QGIS on Debian/Ubuntu as described [here](https://www.qgis.org/en/site/forusers/alldownloads.html#debian-ubuntu).
link2GI should also work on Linux distributions other than Ubuntu. However, only Ubuntu, Debian and Manjaro have been tested.

Open a terminal window. First, make sure to remove any QGIS and GRASS packages you may have previously installed from other repositories:

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

The binary GRASS installation can be found [here](https://grass.osgeo.org/download/mac/). 
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




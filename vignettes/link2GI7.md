---
author: "Chris Reudenbach"
title: "Installation guide for link2GI related software"
date: "2025-12-25"
editor_options:
  chunk_output_type: console
output:
  html_document: 
    theme: united
    toc: true
  rmarkdown: default
  pdf_document:
    latex_engine: xelatex
    toc: true
urlcolor: blue
vignette: >
  %\VignetteIndexEntry{link2GI – Installation guide for related software}
  %\VignetteEncoding{UTF-8}{inputenc}\
  %\VignetteEngine{knitr::knitr}
---



# Vignette scope

This vignette describes how to install and configure external GIS software required by **link2GI**.
It is based on the former *RQGIS* installation guide, with updates and simplifications.

The goal is a **robust command-line–accessible GIS stack** usable from R via `link2GI`.
The exact set of available algorithms depends on platform, architecture, and software versions.


# Windows

## QGIS, GRASS and SAGA

On Windows, **OSGeo4W** is the recommended installation method.
It provides QGIS together with GRASS, SAGA, GDAL, OTB and many other tools.

Standalone QGIS installers work, but provide fewer third-party components.

> **Do not install QGIS 2.x.**

The OSGeo4W installer is available from:

* [https://www.qgis.org/download/](https://www.qgis.org/download/)
* [https://trac.osgeo.org/osgeo4w/](https://trac.osgeo.org/osgeo4w/)

### Express installation (minimal)

![Express setup dialog showing the minimal project configuration options.](https://raw.githubusercontent.com/r-spatial/link2GI/master/figures/00_express.PNG)

Steps:

1. Select **Express Installation**
2. Choose **QGIS, GRASS and GDAL**
3. Start installation

This setup is comparable to the standalone QGIS installer.

### Advanced installation (recommended)

For full command-line access and additional tools, use **Advanced Installation**.

![Initial setup screen selecting GIS backends and defining search locations.](https://raw.githubusercontent.com/r-spatial/link2GI/master/figures/01_initial_setup.PNG)

Proceed with defaults until **Select Packages**.

![Package selection screen for choosing available GIS APIs to link.](https://raw.githubusercontent.com/r-spatial/link2GI/master/figures/04_select_packages.PNG)

Select command-line tools such as **GDAL**, **Python**, and others as needed:

![Command-line utilities configuration screen for external GIS tools.](https://raw.githubusercontent.com/r-spatial/link2GI/master/figures/05_command_line_utilties.PNG)

Additionally select:

* GRASS Desktop GIS
* Orfeo Toolbox
* SAGA GIS
* QGIS (LTR + current release recommended)

Accept suggested dependencies:

![Suggestions screen listing recommended next steps after project initialization.](https://raw.githubusercontent.com/r-spatial/link2GI/master/figures/08_suggestions.PNG)

The download and installation process may take some time.

Installed components can later be modified via:

```
OSGeo4W/bin/osgeo4w-setup.exe
```

### Optional: LAStools

For LiDAR processing support:
[https://rapidlasso.de/how-to-install-lastools-toolbox-in-qgis/](https://rapidlasso.de/how-to-install-lastools-toolbox-in-qgis/)

---

## Orfeo Toolbox (Windows)

Download the latest Windows binary from:
[https://www.orfeo-toolbox.org/download/](https://www.orfeo-toolbox.org/download/)

OTB binaries do not require administrative privileges.
Extract the archive and use `otbcli_*` tools directly or via `link2GI`.

See the official cookbook for details:
[https://www.orfeo-toolbox.org/CookBook/Installation.html#windows](https://www.orfeo-toolbox.org/CookBook/Installation.html#windows)

---

# Linux

## Ubuntu / Debian

Distribution repositories usually ship outdated QGIS versions.
Use the official QGIS repositories instead:

[https://www.qgis.org/download/](https://www.qgis.org/download/)

Remove old installations first:

```sh
sudo apt-get --purge remove qgis
sudo apt autoremove --purge-remove
sudo apt autoremove
sudo apt update
```

Add the official QGIS repository (example for Ubuntu 22.04):

```sh
sudo apt install gnupg software-properties-common

wget -qO - https://qgis.org/downloads/qgis-2020.gpg.key \
  | sudo gpg --no-default-keyring \
    --keyring gnupg-ring:/etc/apt/trusted.gpg.d/qgis-archive.gpg \
    --import

sudo add-apt-repository "deb https://qgis.org/debian focal main"
```

Install required software:

```sh
sudo apt update
sudo apt install qgis qgis-plugin-grass grass saga otb-bin python-qgis
```

**Note:**
QGIS currently supports only **SAGA LTS 2.3.x**.
Other versions require manual compilation.

---

## Arch Linux

QGIS, GRASS and SAGA are available via the **AUR**:
[https://aur.archlinux.org/](https://aur.archlinux.org/)

---

## Fedora

Install via the system package manager:

```sh
sudo dnf install qgis qgis-python qgis-grass saga grass-gui grass-libs
```

---

# macOS

## SAGA

There is no official SAGA binary for macOS.
Use Homebrew LTS builds:

```sh
brew install saga-gis-lts
brew link saga-gis-lts --force
```

Forced linking is required so QGIS can detect SAGA.

---

## GRASS

Install GRASS via Homebrew:

```sh
brew install grass grass7 grass8
```

Binary installers are available at:
[https://grass.osgeo.org/download/mac/](https://grass.osgeo.org/download/mac/)

Homebrew installations are recommended.

---

## QGIS

Two options exist:

### Homebrew (recommended)

```sh
brew install qgis3
```

Prebuilt bottles are used by default.
Source builds may take >30 minutes.

**Important:**
Install **GRASS and SAGA before QGIS** so that paths are detected correctly.

---


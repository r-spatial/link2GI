# Installation guide for link2GI related software

## Vignette scope

This vignette describes **recommended default installation paths** for
external GIS software that can be linked from R via **link2GI**.

`link2GI` is designed to **detect and use many non-default
installations** (custom folders, portable archives, OSGeo4W, Homebrew,
system packages, etc.). The instructions below therefore **do not define
requirements**, but provide a **stable baseline** that tends to work
well across typical user setups.

The goal is a **command-line–accessible GIS stack** usable from R. The
concrete set of available algorithms depends on operating system,
architecture, and software versions.

## Windows

### Recommended baseline: OSGeo4W

On Windows, **OSGeo4W** is the most robust default because it provides a
coherent stack: **QGIS**, **GRASS**, **SAGA**, **GDAL**, **Python**, and
optional components.

Standalone QGIS installers can work, but they may not provide the same
breadth of CLI tools or consistent environment setup.

OSGeo4W installers:

- <https://www.qgis.org/download/>
- <https://trac.osgeo.org/osgeo4w/>

#### Express installation (minimal)

If you just need the main stack quickly:

1.  Select **Express Installation**
2.  Choose at least **QGIS** (and optionally the GRASS-related
    components)
3.  Install

This is comparable to a standalone QGIS installation, but still uses
OSGeo4W as the base.

#### Advanced installation (recommended)

For full CLI availability and best interoperability, use **Advanced
Installation** and ensure the following are selected:

- **QGIS** (LTR recommended; current release optional)
- **GRASS GIS**
- **GDAL**
- **Python** (OSGeo4W Python)
- **SAGA GIS** (if you want SAGA via OSGeo4W)
- Optional: additional libraries/tools as needed

Installed components can later be modified via:

    OSGeo4W/bin/osgeo4w-setup.exe

##### Important note: starting R/RStudio

For GRASS/QGIS/SAGA tools installed via OSGeo4W, the most robust
approach is to start R (or RStudio) **from the OSGeo4W Shell**, because
it sets PATH/PROJ/GDAL variables consistently for the session. `link2GI`
can often recover missing variables, but the shell startup is the clean
baseline.

### Orfeo Toolbox (Windows)

OTB is often installed **separately** from OSGeo4W.

- Download Windows binaries: <https://www.orfeo-toolbox.org/download/>
- Extract to a user-writable folder (no admin privileges required)

`link2GI` can link portable OTB installs by pointing `searchLocation` to
the folder containing the OTB launcher or binaries.

OTB installation notes:
<https://www.orfeo-toolbox.org/CookBook/Installation.html#windows>

## Linux

### Baseline: use distribution packages where possible

On Linux, system packages are usually the most predictable baseline for
linking CLI tools. However, distribution QGIS versions can be outdated
on some distros. If you need newer QGIS, use the official QGIS
repositories.

#### Ubuntu / Debian

Remove older QGIS installations (optional but recommended if you had
mixed sources):

``` sh
sudo apt-get --purge remove qgis
sudo apt autoremove --purge-remove
sudo apt autoremove
sudo apt update
```

Add the official QGIS repository. **Note:** the distro codename in the
repository line must match your system (e.g., `jammy` for Ubuntu 22.04,
`noble` for 24.04). Use the QGIS download page to copy the correct
current instructions:

<https://www.qgis.org/download/>

Install core components:

``` sh
sudo apt update
sudo apt install qgis grass saga gdal-bin python3-qgis
```

Good catch — you’re right. `otb` via distro packages is **almost always
outdated** and often missing features. Here is a **clean, future-proof
replacement text** you can drop into the vignette.

------------------------------------------------------------------------

### Orfeo Toolbox (Linux)

Distribution packages of **Orfeo Toolbox (OTB)** are **often outdated**
and may lag several major releases behind the official project. For
reproducible and feature-complete workflows, the **official OTB binary
distribution is strongly recommended**.

[OTB Download](https://www.orfeo-toolbox.org/download/) the latest Linux
binaries and choose the archive matching your architecture (typically
`x86_64`). Example installation to a user directory:

``` sh
mkdir -p ~/apps
cd ~/apps
tar -xzf OTB-*-Linux64.tar.gz
```

This creates a self-contained OTB directory with no administrative
privileges required, e.g.:

    ~/apps/OTB-9.0.0-Linux64/

The official OTB binaries ship with their own internal libraries and do
**not** require system GDAL or PROJ installations to match versions.
This makes them particularly robust on shared systems and HPC
environments.

##### Note on SAGA versions

SAGA packaging and version alignment differs across distributions and
QGIS builds. If you require a specific SAGA version, treat it as a
**separate toolchain** and link it explicitly (standalone build / custom
install). `link2GI` should be able to discover it if the `saga_cmd`
binary is reachable or provided via `searchLocation`.

### Arch Linux

Many GIS packages are available via pacman and/or AUR. Use your standard
workflow (pacman/AUR helper).

AUR: <https://aur.archlinux.org/>

### Fedora

Install via the system package manager:

``` sh
sudo dnf install qgis qgis-python qgis-grass saga grass-gui grass-libs gdal
```

(Exact package names can vary slightly across Fedora versions.)

## macOS

On macOS, Homebrew is the most consistent baseline for a CLI-accessible
GIS stack.

### GDAL

``` sh
brew install gdal
```

### GRASS

``` sh
brew install grass
```

Binary installers are also available:
<https://grass.osgeo.org/download/mac/>

### SAGA

SAGA availability on macOS varies. If you need SAGA and it is available
via your Homebrew setup, install it there. Some users rely on LTS builds
if available:

``` sh
brew install saga-gis-lts
brew link saga-gis-lts --force
```

(If Homebrew does not provide a formula in your environment, treat SAGA
as optional.)

### QGIS

Two common options:

#### Homebrew

``` sh
brew install qgis
```

**Important:** install CLI dependencies (GDAL/GRASS/SAGA, if needed)
first, so paths are detected consistently.

## Practical guidance: “default” vs. “anything else”

**Default recommendation:** Use the platform’s “coherent stack”
installer where available:

- Windows: OSGeo4W (+ optional standalone OTB)
- Linux: distro packages / official QGIS repo (+ optional standalone
  OTB)
- macOS: Homebrew (+ optional QGIS)

**Non-default setups:** Portable archives, custom install folders,
multiple coexisting versions, and HPC module environments are common.
`link2GI` is intended to handle these cases by searching and linking
binaries explicitly (instead of assuming a single global PATH).

If you run into a mismatch between R-linked libraries (e.g., `sf`,
`terra`) and the system CLI tools, prefer a workflow where you **link
and log the exact CLI binaries** used for processing, and treat R-side
I/O as separate steps.

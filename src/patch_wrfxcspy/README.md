# WRFxCSPY

All-in-one coupler for [COSIPY](https://cosipy.readthedocs.io) and WRF.

## Pre-requisites

1. Install patcher dependencies

Perl version 5.26.3 or greater must be installed on your system. Depending on your distribution, you may also need to install the following:

* perl-time-piece
* perl-path-tiny

For NHR@FAU users:

```console
spack install perl@5.36.0 %gcc@8.5.0
spack install perl-path-tiny perl-time-piece %gcc@8.5.0 ^perl@5.36.0
module load perl/5.36 perl-path-tiny perl-time-piece
```

## Patching

1. Download WRF, the NoahMP submodule, COSIPY, and the coupler:
```console
bash patch_wrfxcspy.sh --install-all -i ./foo/bar/
```

2. Install WRF's dependencies and edit ``build_wrf.sh`` to point to the right paths for COSIPY_API, NETCDF, HDF5, and DIR. Make sure WRF's dependencies are correctly installed.

3. Load environment variables, configure and patch WRF:
```console
bash patch_wrfxcspy -e -c -p -i ./foo/bar/WRF/
```

Depending on your system and access permissions, you may need to run ``build_wrf.sh`` separately.

4. Build WRF: ``./compile em_real >& log.compile``

**Arguments:**

```properties
-i, --input <file>  Source WRF directory, relative to current working directory.
--install-all       Download WRF, NoahMP submodule, COSIPY, and the WRFxCSPY coupler. This will not build WRF for you.
--install-wrf       Download only WRF and NoahMP drivers. This will not build WRF for you.
--install-cosipy    Download only COSIPY.
--install-coupler   Install only the coupler code.
--wrf-branch <str>  Name of WRF branch on GitHub. Defaults to 'release-v4.6.1'.
-c, --configure     Create new WRF configuration script.
-p, --patch         Patch COSIPY into WRF. Does not check if already patched!
-e, --env           Load environment variables.
-v, --verbose       Prints log messages to stderr.
```

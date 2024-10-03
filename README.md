# WRF x COSIPY

All-in-one coupler for [COSIPY](https://cosipy.readthedocs.io) and WRF.

# Instructions

1. Download WRF and COSIPY:
```
bash patch_wrfxcspy.sh --install-wrf --install-cosipy -i ./foo/bar/
```

2. Install WRF's dependencies and edit `build_wrf.sh` to point to the right paths for COSIPY_API, NETCDF, HDF5, and DIR. Make WRF's dependencies are correctly installed.

3. Load environment variables, configure and patch WRF:
```
bash patch_wrfxcspy -e -c -p -i ./foo/bar/WRF
```

4. Build WRF: `./compile em_real >& log.compile`

**Arguments:**

-i, --input [file]      Source WRF directory, relative to current working directory.
-c, --configure         Create new WRF configuration script.
-d, --delete            Run make clean in source directory.
-p, --patch             Patch COSIPY into WRF. Does not check if already patched.
-e, --env               Load environment variables.
-v, --verbose           Prints log messages to stderr.
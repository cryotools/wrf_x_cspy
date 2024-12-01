#!/bin/bash -l
# Copyright 2024 WRFxCSPY Contributors

# Ensure COSIPY_API, NETCDF, HDF5, and DIR point to the correct paths.

USER_PATH="/home/atuin/<group>/<account>/software"

# Load HPC modules
module purge
module load intelmpi/2021.6.0 intel/2021.4.0
module load user-spack
module load time

# Coupler
export COSIPY_API="${USER_PATH}/path/to/WRF/COUPLER"
export COSIPY_DIR="${USER_PATH}/path/to/WRF/cosipy"
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${COSIPY_API}

# WRF options
export WRFIO_NCD_LARGE_FILE_SUPPORT=1
export WRF_EM_CORE=1
export WRF_NMM_CORE=0
export WRF_CHEM=0

# NetCDF
export NETCDF="${USER_PATH}/path/to/netcdf"
export NETCDF_LIB="-L${NETCDF}/lib -lnetcdf -lnetcdff -Wl,-rpath,${NETCDF}/lib"
export NETCDF_INC="-I${NETCDF_C_ROOT}/include"
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${NETCDF}/lib

# NetCDF options
export NETCDF_ROOT=$NETCDF
export NETCDF_classic=1
export NETCDF4=1

# HDF5
export HDF5="${USER_PATH}/path/to/hdf5"
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${HDF5}/lib

# Libraries
export DIR="${USER_PATH}/WRF/Build_WRF/LIBRARIES"
export JASPERLIB=${DIR}/grib2/lib
export JASPERINC=${DIR}/grib2/include

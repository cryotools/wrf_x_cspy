#!/bin/bash -l
# Copyright 2024 WRFxCSPY Contributors

# Ensure COSIPY_API, NETCDF, HDF5, and JASPER_ROOT point to the correct paths.

# Load HPC modules
module purge
module load user-spack
module load intelmpi/2021.6.0 intel/2021.4.0
module load perl-time-piece perl-path-tiny
module load jasper
module load time

# Set library paths here
export COSIPY_API="/path/to/WRF/COUPLER"
export COSIPY_DIR="/path/to/WRF/cosipy"
export NETCDF="/path/to/netcdf"
export HDF5="/path/to/hdf5"
# export JASPER_ROOT="/path/to/jasper"

# Coupler
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${COSIPY_API}

# WRF options
export WRFIO_NCD_LARGE_FILE_SUPPORT=1
export WRF_EM_CORE=1
export WRF_NMM_CORE=0
export WRF_CHEM=0

# NetCDF
export NETCDF_LIB="-L${NETCDF}/lib -lnetcdf -lnetcdff -Wl,-rpath,${NETCDF}/lib"
export NETCDF_INC="-I${NETCDF_C_ROOT}/include"
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${NETCDF}/lib

# NetCDF options
export NETCDF_classic=0
export NETCDF4=1

# HDF5
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${HDF5}/lib

# Libraries
export JASPERLIB=${JASPER_DIR}/lib
export JASPERINC=${JASPER_DIR}/include

#!/bin/bash -l
# Copyright 2024 WRFxCSPY Contributors

# Ensure COSIPY_API, NETCDF, HDF5, and DIR point to the correct paths.

USER_PATH="/home/atuin/<group>/<account>/software"

# Load HPC modules
module purge
module load hdf5/1.10.7-impi-intel netcdf-c netcdf-fortran/4.5.3-intel gcc/9.4.0 cmake
module load mkl/2022.1.0 m4/1.4.19
module load user-spack
module load time

# Coupler
export COSIPY_API="${USER_PATH}/path/to/WRF/COUPLER"
export COSIPY_DIR="${USER_PATH}/path/to/WRF/cosipy"
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${COSIPY_API}
export LIBRARY_PATH=${LIBRARY_PATH}:${COSIPY_API}
export PATH=${PATH}:${COSIPY_API}

# WRF options
export WRFIO_NCD_LARGE_FILE_SUPPORT=1
export WRF_EM_CORE=1
export WRF_NMM_CORE=0
export WRF_CHEM=0

# NetCDF
export NETCDF_LIB="-L${NETCDF_C_ROOT}/lib -lnetcdf -L${NETCDF_FORTRAN_ROOT}/lib -lnetcdff -Wl,-rpath,${NETCDF_C_ROOT}/lib"
export NETCDF_INC="-I${NETCDF_C_ROOT}/include -I${NETCDF_FORTRAN_ROOT}/include"
export NETCDF="${USER_PATH}/privat/netcdf"
export PATH=${NETCDF}/bin:${PATH}
export LIBRARY_PATH=$NETCDF_C_ROOT/lib:$LIBRARY_PATH
export LIBRARY_PATH=$NETCDF_FORTRAN_ROOT/lib:$LIBRARY_PATH

# NetCDF options
export NETCDF_ROOT=$NETCDF
export NETCDF_classic=1
export NETCDF4=1

# HDF5
export HDF5="${USER_PATH}/privat/hdf5"
export DEP_LIB_PATH=-L${HDF5}/lib
export PATH=${HDF5}/bin:${PATH}

# Libraries
export DIR="${USER_PATH}/WRF/Build_WRF/LIBRARIES"
export JASPERLIB=${DIR}/grib2/lib
export JASPERINC=${DIR}/grib2/include
export PATH=${DIR}/mpich/bin:${PATH}

# Compilers
export CC=icc
export CXX=icpc
export CFLAGS='-O3 -xHost -ip -no-prec-div -static-intel'
export CXXFLAGS='-O3 -xHost -ip -no-prec-div -static-intel'

export F77=ifort
export FC=ifort
export F90=ifort
export FFLAGS='-O3 -xHost -ip -no-prec-div -static-intel'

export CPP='icc -E'
export CXXCPP='icpc -E'

export LDFLAGS=-L${DIR}/grib2/lib
export CPPFLAGS=-I${DIR}/grib2/include

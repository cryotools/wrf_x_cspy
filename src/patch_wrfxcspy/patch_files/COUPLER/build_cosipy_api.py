'''
    Script to build a dynamic library using CFFI for WRF_X_CSPY coupling
    Based on call_py_fort (https://github.com/nbren12/call_py_fort)
    Emily Collier 2021
'''

import cffi

# New Foreign Function Interface object
ffibuilder = cffi.FFI()

# Declare python function
header = """
extern int call_cosipy_wrf( int *, int*, int*, float *, int*, float *, float *, \
                   float *, float *, float *, float *, float *, float *, float *, \
                   float *, float *, float *, float *, float *, float *, float *, \
                   float *, float *, float *, float *, float *, float *, float *, \
                   float *, float *, float *, float *, float *, float *, float *, \
                   float *, float *, float *, float *, float *, float *, float *, \
                   float *, float *, float *, float *, int*,    float *, float *, \
                   float *, float *, float *);
"""

with open("cosipy_wrf_plugin.h", "w") as f:
    f.write(header)

ffibuilder.embedding_api(header)

ffibuilder.set_source("cosipy_plugin", r'''
    #include "cosipy_wrf_plugin.h"
''')

# Provide initialization-time Python source code
# TO DO: fill with user supplied paths
module = """
from cosipy_plugin import ffi

#add cosipy and coupler to sys.path
import sys
sys.path.append("/home/titan/gwgk/gwgi18/PRIVATE/KONWIHR/WRF_COSIPY/COUPLER")
sys.path.append("/home/titan/gwgk/gwgi18/PRIVATE/KONWIHR/WRF_COSIPY/cosipy")
import call_cosipy_wrf as coupler


#use decorator to attach corresponding python function
@ffi.def_extern(error=1)
def call_cosipy_wrf(*args):
    coupler.cosipy_wrf(*args, ffi=ffi)
    return 0
"""
ffibuilder.embedding_init_code(module)

# compile DLL
# TO DO: extension should be platform dependent
ffibuilder.compile(target="libcosipywrf.so", verbose=True)

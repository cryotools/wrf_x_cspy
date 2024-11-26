"""
    Python coupling code to call 'COupled Snowpack and Ice surface energy
    and MAss balance glacier model in Python' (COSIPY) from WRF
    
    Snippets borrowed from call_py_fort (https://github.com/nbren12/call_py_fort)
    
    Emily Collier 2021
"""

from cosipy.cpkernel.cosipy_core import cosipy_core
import numpy as np


#dict mapping expected ctypes to np dtypes
ctype2dtype = {'int':np.dtype(np.int32),'float':np.dtype('f4')}

def ptr_2_np(ffi, ptr, length):
    """ retrieve np from c pointer (only 1D arrays, no safety checks) """
    T = ffi.getctype(ffi.typeof(ptr).item)
    return np.frombuffer(ffi.buffer(ptr, length * ffi.sizeof(T)), ctype2dtype[T])
    

def cosipy_wrf(ITIMESTEP_ptr,J_ptr,I_ptr,DT_ptr,max_layers_ptr,HGT_ptr,SLOPE_ptr,
               ASPECT_ptr,ZLVL_ptr,T_ptr,RH_ptr,PRES_ptr,WS_ptr,SWDWN_ptr,LWDWN_ptr,
               RAINBL_ptr,SNOWBL_ptr,SNOWH_ptr,SNOW_ptr,TSK_ptr,LWOUT_ptr,H_ptr,LE_ptr,
               B_ptr,QRR_ptr,MB_ptr,SURFMB_ptr,INTMB_ptr,Q_ptr,SNOWH_CSPY_ptr,TTLH_ptr,
               TS_ptr,ALBEDO_ptr,ME_ptr,EVAP_ptr,SUBLIM_ptr,COND_ptr,DEPO_ptr,REFR_ptr,
               SUBM_ptr,SURFM_ptr,Z0_ptr,NEW_SNOW_HEIGHT_ptr,NEW_SNOW_TIMESTAMP_ptr,
               OLD_SNOW_TIMESTAMP_ptr,MOL_ptr,NLAYERS_ptr,LAYER_HEIGHT_ptr,
               LAYER_RHO_ptr,LAYER_T_ptr,LAYER_LWC_ptr,LAYER_IF_ptr,ffi=None):	
    """ 
        Pass WRF forcing and state vars to COSIPY and glacier variables back to WRF
    """     
    # Convert C pointer to numpy 
    lenarr      = 1
    ITIMESTEP   = ptr_2_np(ffi, ITIMESTEP_ptr, (lenarr))-1   #zero indexing    
    J 		= ptr_2_np(ffi, J_ptr, (lenarr))  
    I 		= ptr_2_np(ffi, I_ptr, (lenarr))  
    DT 		= ptr_2_np(ffi, DT_ptr, (lenarr)) 
    max_layers 	= ptr_2_np(ffi, max_layers_ptr, (lenarr))
    
    #HGT        = ptr_2_np(ffi, HGT_ptr, (lenarr))  
    SLOPE       = ptr_2_np(ffi, SLOPE_ptr, (lenarr))  
    ASPECT      = ptr_2_np(ffi, ASPECT_ptr, (lenarr))
    ZLVL 	= ptr_2_np(ffi, ZLVL_ptr, (lenarr))  
    T 		= ptr_2_np(ffi, T_ptr, (lenarr))  
    RH          = ptr_2_np(ffi, RH_ptr, (lenarr)) 
    PRES 	= ptr_2_np(ffi, PRES_ptr, (lenarr))  
    WS 		= ptr_2_np(ffi, WS_ptr, (lenarr))   
    SWDWN 	= ptr_2_np(ffi, SWDWN_ptr, (lenarr))  
    LWDWN 	= ptr_2_np(ffi, LWDWN_ptr, (lenarr))  
    RAINBL 	= ptr_2_np(ffi, RAINBL_ptr, (lenarr)) 
    SNOWBL 	= ptr_2_np(ffi, SNOWBL_ptr, (lenarr))  
    SNOWH 	= ptr_2_np(ffi, SNOWH_ptr, (lenarr))  
    SNOW 	= ptr_2_np(ffi, SNOW_ptr, (lenarr))  
    TSK 	= ptr_2_np(ffi, TSK_ptr, (lenarr)) 


    # Initialize DATA
    DATA = get_WRF_forcing(ITIMESTEP,DT,max_layers,SLOPE,ASPECT,ZLVL,T,RH,
                           PRES,WS,SWDWN,LWDWN,RAINBL,SNOWBL,SNOWH,SNOW,TSK)

    # Initialize GRID_RESTART
    GRID_RESTART = None
    max_layers   = int(max_layers)
    if ITIMESTEP > 0:
        NLAYERS      = ptr_2_np(ffi, NLAYERS_ptr, (lenarr))     #Initialized as -9999
        NEW_SNOW_HEIGHT 	= ptr_2_np(ffi, NEW_SNOW_HEIGHT_ptr, (lenarr)) 
        NEW_SNOW_TIMESTAMP 	= ptr_2_np(ffi, NEW_SNOW_TIMESTAMP_ptr, (lenarr)) 
        OLD_SNOW_TIMESTAMP 	= ptr_2_np(ffi, OLD_SNOW_TIMESTAMP_ptr, (lenarr))  
        LAYER_HEIGHT    	= ptr_2_np(ffi, LAYER_HEIGHT_ptr, (max_layers))  
        LAYER_RHO       	= ptr_2_np(ffi, LAYER_RHO_ptr, (max_layers))  
        LAYER_T         	= ptr_2_np(ffi, LAYER_T_ptr, (max_layers))  
        LAYER_LWC       	= ptr_2_np(ffi, LAYER_LWC_ptr, (max_layers)) 	
        LAYER_IF        	= ptr_2_np(ffi, LAYER_IF_ptr, (max_layers)) 	
        GRID_RESTART = get_WRF_restart(NLAYERS,NEW_SNOW_HEIGHT,NEW_SNOW_TIMESTAMP,OLD_SNOW_TIMESTAMP,
                                       LAYER_HEIGHT,LAYER_RHO,LAYER_T,LAYER_LWC,LAYER_IF)


    # Run cosipy
    _,_,_,_,_,_,LWOUT,H,LE,B,QRR,MB,SURFMB,Q,SNOWHEIGHT,TTLHEIGHT,TS,ALBEDO,NLAYERS, \
        ME,INTMB,EVAP,SUBLIM,COND,DEPO,REFR,SUBM,Z0,SURFM,NEW_SNOW_HEIGHT,           \
        NEW_SNOW_TIMESTAMP,OLD_SNOW_TIMESTAMP,MOL,LAYER_HEIGHT,LAYER_RHO,            \
        LAYER_T,LAYER_LWC,_,_,LAYER_IF,_,_,_,_,_ = cosipy_core(DATA, J, I, GRID_RESTART) 
    

    # Update C pointers -- COSIPY returns float64 
    ffi.buffer(LWOUT_ptr)[:] 			= np.float32(LWOUT)
    ffi.buffer(H_ptr)[:] 			= np.float32(H)
    ffi.buffer(LE_ptr)[:] 			= np.float32(LE)
    ffi.buffer(B_ptr)[:] 			= np.float32(B)
    ffi.buffer(QRR_ptr)[:] 			= np.float32(QRR)
    ffi.buffer(SNOWH_CSPY_ptr)[:]		= np.float32(SNOWHEIGHT)
    ffi.buffer(TTLH_ptr)[:]			= np.float32(TTLHEIGHT)
    ffi.buffer(TS_ptr)[:] 			= np.float32(TS)
    ffi.buffer(ALBEDO_ptr)[:] 			= np.float32(ALBEDO)
    ffi.buffer(ME_ptr)[:] 			= np.float32(ME)
    ffi.buffer(Z0_ptr)[:] 			= np.float32(Z0)
    ffi.buffer(NEW_SNOW_HEIGHT_ptr)[:] 		= np.float32(NEW_SNOW_HEIGHT)
    ffi.buffer(NEW_SNOW_TIMESTAMP_ptr)[:] 	= np.float32(NEW_SNOW_TIMESTAMP)
    ffi.buffer(OLD_SNOW_TIMESTAMP_ptr)[:] 	= np.float32(OLD_SNOW_TIMESTAMP)
    ffi.buffer(MOL_ptr)[:] 			= np.float32(MOL)
    
    ffi.buffer(Q_ptr)[:] 			= np.float32(Q)
    ffi.buffer(EVAP_ptr)[:] 			= np.float32(EVAP)
    ffi.buffer(SUBLIM_ptr)[:] 			= np.float32(SUBLIM)
    ffi.buffer(COND_ptr)[:] 			= np.float32(COND)
    ffi.buffer(DEPO_ptr)[:] 			= np.float32(DEPO)
    ffi.buffer(REFR_ptr)[:] 			= np.float32(REFR)
    ffi.buffer(SUBM_ptr)[:] 			= np.float32(SUBM)
    ffi.buffer(SURFM_ptr)[:] 			= np.float32(SURFM)
    
    # Cumulative mass fluxes    
    ffi.buffer(MB_ptr)[:] 			= ptr_2_np(ffi, MB_ptr, (lenarr)) + np.float32(MB)
    ffi.buffer(SURFMB_ptr)[:] 			= ptr_2_np(ffi, SURFMB_ptr, (lenarr)) + np.float32(SURFMB)
    ffi.buffer(INTMB_ptr)[:] 			= ptr_2_np(ffi, INTMB_ptr, (lenarr)) + np.float32(INTMB)
    
    # Subsurface fields
    T = ffi.getctype(ffi.typeof(LAYER_HEIGHT_ptr).item)
    lenarr = max_layers*ffi.sizeof(T)
    ffi.buffer(NLAYERS_ptr)[:]			= np.int32(NLAYERS)
    ffi.buffer(LAYER_HEIGHT_ptr, lenarr)[:] 	= np.float32(LAYER_HEIGHT)
    ffi.buffer(LAYER_RHO_ptr, lenarr)[:] 	= np.float32(LAYER_RHO)
    ffi.buffer(LAYER_T_ptr, lenarr)[:] 	        = np.float32(LAYER_T)
    ffi.buffer(LAYER_LWC_ptr, lenarr)[:] 	= np.float32(LAYER_LWC)
    ffi.buffer(LAYER_IF_ptr, lenarr)[:] 	= np.float32(LAYER_IF)
    
    
# Wrapper for passing data    
class NestedNamespace:
    def __init__(self, dictionary, **kwargs):
        for key, value in dictionary.items():
            if isinstance(value, dict):
                self.__setattr__(key, NestedNamespace(value))
            else:
                self.__setattr__(key, value)    
    def __iter__(self):
        return iter(self.__dict__)
    def __getitem__(self, key):
        return self.__dict__[key]	    
    
    
def get_WRF_forcing(ITIMESTEP,DT,max_layers,SLOPE,ASPECT,ZLVL,T,RH,PRES,WS,SWDWN,
                    LWDWN,RAINBL,SNOWBL,SNOWH,SNOW,TSK):
    """
        create forcing dataset accessible by DATA.<varname>.values for consistency with 
        offline cosipy usage of xarray
	accessing NestedNamespace is 10x faster to create and 50x faster than xarray or pandas
    """
    DATA = {"time":{'values':ITIMESTEP},
            "DT":{'values':DT},
            "max_layers":{'values':max_layers},
            "SLOPE":{'values':SLOPE},
            "ASPECT":{'values':ASPECT},
            "ZLVL":{'values':ZLVL},
            "T2":{'values':T},
            "RH2":{'values':RH},
            "PRES":{'values':PRES},
            "U2":{'values':WS},
            "G":{'values':SWDWN},
            "LWin":{'values':LWDWN},
            "RRR":{'values':RAINBL},
            "SNOWFALL":{'values':SNOWBL},
            "SNOWHEIGHT":{'values':SNOWH},
            "SWE":{'values':SNOW},
            "TSK":{'values':TSK}}
    return NestedNamespace(DATA)   


def get_WRF_restart(NLAYERS,NEW_SNOW_HEIGHT,NEW_SNOW_TIMESTAMP,OLD_SNOW_TIMESTAMP,
                    LAYER_HEIGHT,LAYER_RHO,LAYER_T,LAYER_LWC,LAYER_IF):
    """ return GRID restart """
    GRID_RESTART = {"NLAYERS":{'values':NLAYERS},
                    "new_snow_height":{'values':np.float64(NEW_SNOW_HEIGHT[0])},   
                    "new_snow_timestamp":{'values':np.float64(NEW_SNOW_TIMESTAMP[0])}, 
                    "old_snow_timestamp":{'values':np.float64(OLD_SNOW_TIMESTAMP[0])}, 
                    "LAYER_HEIGHT":{'values':np.float64(LAYER_HEIGHT)},
                    "LAYER_RHO":{'values':np.float64(LAYER_RHO)},
                    "LAYER_T":{'values':np.float64(LAYER_T)},
                    "LAYER_LWC":{'values':np.float64(LAYER_LWC)},
                    "LAYER_IF":{'values':np.float64(LAYER_IF)}}
    return NestedNamespace(GRID_RESTART)





!------------------------------------------------------------------------------- 
!-------------------------------------------------------------------------------   
  !Fortran coupling code to call 'COupled Snowpack and Ice surface energy
  !and MAss balance glacier model in Python'(COSIPY) from WRF
  !Emily Collier 2021   
!------------------------------------------------------------------------------- 
!------------------------------------------------------------------------------- 

MODULE cosipy_wrf
  
  USE, INTRINSIC :: ISO_C_BINDING, only: C_INT, C_FLOAT
  IMPLICIT NONE 
    
  ! Declare interface for CFFI c-style binding  
  INTERFACE
    SUBROUTINE call_cosipy_wrf(ITIMESTEP,J,I,DT,max_layers,HGT,SLOPE,ASPECT,ZLVL,   &
                   T,RH,PRES,WS,SWDWN,LWDWN,RAINBL,SNOWBL,SNOWH,SNOW,TSK,LWOUT_CSPY,&
		   H_CSPY,LE_CSPY,B_CSPY,QRR_CSPY,MB_CSPY,SURFMB_CSPY,INTMB_CSPY,   &
		   Q_CSPY,SNOWH_CSPY,TTLH_CSPY,TS_CSPY,ALB_CSPY,ME_CSPY,EVAP_CSPY,  &
		   SUBLIM_CSPY,COND_CSPY,DEPO_CSPY,REFR_CSPY,SUBM_CSPY,SURFM_CSPY,  &
		   Z0_CSPY,NSH_CSPY,NST_CSPY,OST_CSPY,MOL_CSPY,NLAYERS_CSPY,        &
		   LAYER_HEIGHT_CSPY,LAYER_RHO_CSPY,LAYER_T_CSPY,LAYER_LWC_CSPY,    &
		   LAYER_IF_CSPY) bind (c)
      USE ISO_C_BINDING
      IMPLICIT NONE 
      INTEGER(C_INT) :: ITIMESTEP,J,I,max_layers,NLAYERS_CSPY
    
      REAL(C_FLOAT)  :: DT,HGT,SLOPE,ASPECT,ZLVL,T,RH,PRES,WS,SWDWN,LWDWN,RAINBL,  &
                        SNOWBL,SNOWH,SNOW,TSK,LWOUT_CSPY,H_CSPY,LE_CSPY,B_CSPY,    &
			QRR_CSPY,MB_CSPY,SURFMB_CSPY,INTMB_CSPY,Q_CSPY,SNOWH_CSPY, &
			TTLH_CSPY,TS_CSPY,ALB_CSPY,ME_CSPY,EVAP_CSPY,SUBLIM_CSPY,  &
			COND_CSPY,DEPO_CSPY,REFR_CSPY,SUBM_CSPY,SURFM_CSPY,Z0_CSPY,&
			NSH_CSPY,NST_CSPY,OST_CSPY,MOL_CSPY
      REAL(C_FLOAT), DIMENSION(1:max_layers) :: LAYER_HEIGHT_CSPY,LAYER_RHO_CSPY,  &
                        LAYER_T_CSPY,LAYER_LWC_CSPY,LAYER_IF_CSPY
    END SUBROUTINE call_cosipy_wrf
     
  END INTERFACE
  
!------------------------------------------------------------------------------- 
CONTAINS  
!------------------------------------------------------------------------------- 

SUBROUTINE WRF_RH(ims,ime,jms,jme,T,Q,P,RH)
  IMPLICIT NONE
  
  INTEGER, INTENT(IN) :: ims,ime,jms,jme
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(IN) :: T, Q, P
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: RH
  REAL, PARAMETER :: pq0 = 379.90516
  REAL, PARAMETER :: a2 = 17.2693882
  REAL, PARAMETER :: a3 = 273.16
  REAL, PARAMETER :: a4 = 35.86
  
  RH = Q * 100. / ( (pq0 / P) * EXP(a2 * (T - a3) / (T - a4)) )
  WHERE (RH > 100.) RH = 100.
  WHERE (RH < 0.) RH = 0.
    
END SUBROUTINE WRF_RH

!------------------------------------------------------------------------------- 

SUBROUTINE WRF_ASPECT(ims,ime,jms,jme,HGT,ASPECT)
  USE module_model_constants, ONLY: DEGRAD
  IMPLICIT NONE
  
  !Code from xarray-spatial
  INTEGER, INTENT(IN) :: ims,ime,jms,jme
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(IN) :: HGT
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: ASPECT
  REAL :: a,b,c,d,f,g,h,ii, dz_dx, dz_dy, tmp
  INTEGER :: J,I
  
  DO J = jms+1,jme-1
    DO I = ims+1,ime-1
      a = HGT(I-1, J-1)
      b = HGT(I,   J-1)
      c = HGT(I+1, J-1)
      d = HGT(I-1, J)
      f = HGT(I+1, J)
      g = HGT(I-1, J+1)
      h = HGT(I,   J+1)
      ii= HGT(I+1, J+1)

      dz_dx = ((c + 2 * f + ii) - (a + 2 * d + g)) / 8.
      dz_dy = ((g + 2 * h + ii) - (a + 2 * b + c)) / 8.  

      IF(dz_dx .eq. 0. .and. dz_dy .eq. 0)THEN	!flat
        ASPECT(I,J) = SQRT(-1.)				
      ELSE
        tmp = ATAN2(dz_dy, -1.*dz_dx)/DEGRAD
        IF(tmp .lt. 0.)THEN
          ASPECT(I,J) = 90. - tmp		
        ELSE IF(tmp .gt. 90.)THEN
          ASPECT(I,J) = 360. - tmp + 90.
        ELSE
          ASPECT(I,J) = 90. - tmp
	END IF
      END IF    
    END DO
  END DO
    
END SUBROUTINE WRF_ASPECT

!------------------------------------------------------------------------------- 

SUBROUTINE glacier_mass_balance(ITIMESTEP,CURR_SECS,DT,ims,ime,jms,jme,kms,kme,its,ite,     &
               jts,jte,ISICE,max_layers_cspy,run_cspy,reset_cspy,                           &
	       XLAT,XLONG,HGT,SLOPE,ASPECT,IVGTYP,DZ_LML,T_LML,Q_LML,P_LML,U_LML,V_LML,     &
	       SWDWN,LWDWN,CUMPREC_CSPY,CUMSNOW_CSPY,RAINBL,SRBL,SNOWH,SNOW,SNOWC,TSK,      &
	       LWOUT_CSPY,H_CSPY,LE_CSPY,B_CSPY,QRR_CSPY,MB_CSPY,SURFMB_CSPY,INTMB_CSPY,    &
	       Q_CSPY,SNOWH_CSPY,TTLH_CSPY,TS_CSPY,ALB_CSPY,ME_CSPY,EVAP_CSPY,SUBLIM_CSPY,  &
	       COND_CSPY,DEPO_CSPY,REFR_CSPY,SUBM_CSPY,SURFM_CSPY,Z0_CSPY,NSH_CSPY,NST_CSPY,&
	       OST_CSPY,MOL_CSPY,                                                           &
	       NLAYERS_CSPY,LAYER_HEIGHT_CSPY,LAYER_RHO_CSPY,LAYER_T_CSPY,LAYER_LWC_CSPY,   &
	       LAYER_IF_CSPY,                                                               &
	       NSOIL,ZS,DZS,HFX,LH,QFX,GRDFLX,QSFC,Z0,ZNT,ALBEDO,EMISS,UDRUNOFF,SFCRUNOFF,  &
	       SMOIS,SH2O,TSLB,TMN,ACSNOW,ACSNOM,CMXY,CHXY,T2MBXY,Q2MBXY,CHBXY,CHB2XY)

  USE module_model_constants, ONLY: DEGRAD
  IMPLICIT NONE
  
! TIME 
  INTEGER, INTENT(IN) :: ITIMESTEP 					! current timestep number
  REAL, INTENT(IN) :: CURR_SECS		  				! Integration time [s]
  REAL, INTENT(IN) :: DT		  				! time since last cosipy call [s]
  INTEGER, INTENT(IN) :: run_cspy 			   	        ! 1-OFFLINE, 2-INTERACTIVE
  LOGICAL, INTENT(IN) :: reset_cspy 			   	        ! flag for resetting cum. mb vars

! DIMS
  INTEGER, INTENT(IN) :: ims,ime,jms,jme,kms,kme			! start, end indices of i,j,k in memory
  INTEGER, INTENT(IN) :: its,ite,jts,jte				! start, end indices of i,j in tile
  INTEGER, INTENT(IN) :: max_layers_cspy 				! max number of layers in COSIPY
  INTEGER, INTENT(IN) :: NSOIL		 				! number of soil layers in Noah-MP
  REAL, DIMENSION(1:NSOIL), INTENT(IN) ::  ZS                           ! depths of centres of soil layers [m]
  REAL, DIMENSION(1:NSOIL), INTENT(IN) ::  DZS                          ! thicknesses of soil layers [m]
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(IN) :: XLAT			! latitude [deg]
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(IN) :: XLONG		! longitude [deg]
  
! WRF VARS
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(IN) :: HGT			! terrain height [m]
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(IN) :: SLOPE		! terrain slope [rad?]
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: ASPECT		! aspect [deg]
  INTEGER, DIMENSION(ims:ime, jms:jme), INTENT(IN) :: IVGTYP		! land use classification 
  INTEGER, INTENT(IN) :: ISICE		                                ! land use category for land ice

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) :: DZ_LML	! lowest model level (LML) thickness [m]
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) :: T_LML	! LML air temperature [K]
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) :: Q_LML	! LML water vapour mixing ratio [kg/kg]
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) :: P_LML	! LML pressure [Pa]
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) :: U_LML,V_LML ! LML winds
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(IN) :: SWDWN		! inc. shortwave radiation [W/m2]
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(IN) :: LWDWN		! inc. longwave radiation [W/m2]
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(IN) :: RAINBL		! precipitation [mm w.e.]
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(IN) :: SRBL	    	        ! frozen fraction 
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: SNOWH		! snow depth [m]
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: SNOW		! snow water equivalent [kg/m2]
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: SNOWC		! snow cover flag [0/1]	
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: TSK		! surface temperature [K]  
  
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: HFX       	! sensible heat flux [W m-2]
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: QFX       	! latent heat flux [kg s-1 m-2]
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: LH        	! latent heat flux [W m-2]
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: GRDFLX    	! ground/snow heat flux [W m-2]
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: SFCRUNOFF 	! accumulated surface runoff [m]
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: UDRUNOFF  	! accumulated sub-surface runoff [m]
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: ALBEDO    	! total grid albedo []
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: ACSNOM    	! accumulated snow melt [kg/m2]
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: ACSNOW    	! accumulated snow [kg/m2]
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: EMISS     	! surface bulk emissivity
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: QSFC      	! bulk surface specific humidity
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: Z0        	! combined z0 sent to coupled model
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: ZNT       	! combined z0 sent to coupled model
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: TMN		! deep soil temperature [K]
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: CMXY   		! bulk momentum drag coefficient
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: CHXY   		! bulk sensible heat exchange  
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: T2MBXY   		! 2m temperature of bare ground part [K]
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: Q2MBXY    	! 2m mixing ratio of bare ground part 
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: CHBXY   		! sensible heat exchange coefficient
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: CHB2XY   		! bare 2m exchange coefficient
  
  REAL, DIMENSION(ims:ime, 1:NSOIL, jms:jme), INTENT(INOUT) ::  SMOIS   ! volumetric soil moisture [m3/m3]
  REAL, DIMENSION(ims:ime, 1:NSOIL, jms:jme), INTENT(INOUT) ::  SH2O    ! volumetric liquid soil moisture [m3/m3]
  REAL, DIMENSION(ims:ime, 1:NSOIL, jms:jme), INTENT(INOUT) ::  TSLB    ! soil temperature [K]  
  
  
! COSIPY VARS
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: LWOUT_CSPY	! outgoing longwave radiation [W/m2]
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: H_CSPY 		! sensible heat flux [W/m2]
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: LE_CSPY 		! latent heat flux [W/m2]
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: B_CSPY 		! ground heat flux [W/m2]
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: QRR_CSPY 		! rain heat flux [W/m2]
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: MB_CSPY 		! mass balance [m w.e.]
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: SURFMB_CSPY 	! surface mass balance [m w.e.]
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: INTMB_CSPY 	! internal mass balance [m w.e.]
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: Q_CSPY 		! run off [m w.e.]
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: SNOWH_CSPY 	! snow height [m]
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: TTLH_CSPY 	! total domain height [m]
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: TS_CSPY 		! surface temperature [K]
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: ALB_CSPY		! glacier surface albedo [0-1]
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: ME_CSPY 		! available melt energy [W/m2]
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: EVAP_CSPY 	! evaporation [m w.e.]
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: SUBLIM_CSPY 	! sublimation [m w.e.]
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: COND_CSPY 	! condensation [m w.e.]
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: DEPO_CSPY 	! deposition [m w.e.]
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: REFR_CSPY		! refreezing [m w.e.]
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: SUBM_CSPY 	! subsurface melt [m w.e.]
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: SURFM_CSPY 	! surface melt [m w.e.]
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: Z0_CSPY		! roughness length
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: NSH_CSPY		! new snow height [m w.e.]
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: NST_CSPY		! new snow timestamp [s]
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: OST_CSPY		! old snow timestamp [s]
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: MOL_CSPY          ! MO length (local)
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: CUMPREC_CSPY      ! 1-min precipitation totals
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: CUMSNOW_CSPY      ! 1-min snowfall totals
  

  !Only 3D vars needed for integration
  INTEGER, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: NLAYERS_CSPY 				! num. used levels
  REAL, DIMENSION(ims:ime, 1:max_layers_cspy, jms:jme), INTENT(INOUT) :: LAYER_HEIGHT_CSPY	! layer height [m]
  REAL, DIMENSION(ims:ime, 1:max_layers_cspy, jms:jme), INTENT(INOUT) :: LAYER_RHO_CSPY		! density [kg/m3]
  REAL, DIMENSION(ims:ime, 1:max_layers_cspy, jms:jme), INTENT(INOUT) :: LAYER_T_CSPY		! temperature [K]
  REAL, DIMENSION(ims:ime, 1:max_layers_cspy, jms:jme), INTENT(INOUT) :: LAYER_LWC_CSPY		! liquid water [kg/m2]
  REAL, DIMENSION(ims:ime, 1:max_layers_cspy, jms:jme), INTENT(INOUT) :: LAYER_IF_CSPY		! ice fraction [-]

!LOCAL
  REAL, PARAMETER :: VKC    = 0.41      !von Karman constant
  REAL, PARAMETER :: TFRZ   = 273.16    !freezing/melting point [K]
  REAL, PARAMETER :: HSUB   = 2.8340E06 !latent heat of sublimation [j/kg]
  REAL, PARAMETER :: HVAP   = 2.5104E06 !latent heat of vaporization [j/kg]
  REAL, PARAMETER :: CPAIR  = 1004.67   !heat capacity dry air at const pres [j/kg/k]
  REAL, PARAMETER :: RAIR   = 287.058   !gas constant for dry air [j/kg/k]
  REAL, PARAMETER :: RSURF  = 1.0       !ground surface resistance [s/m]
  REAL, PARAMETER :: RHOW   = 1000.     !density of water [kg/m3]
  REAL, PARAMETER :: MPE    = 1.E-6
  
  INTEGER :: J, I, K, IZ, FSNO		!LOOP INDICES
  REAL, DIMENSION(ims:ime, jms:jme) :: RH, ZLVL, WINDSPEED, PRES  			!LOCAL COPIES OF FORCING DATA   
  REAL, DIMENSION(1:max_layers_cspy) :: LAYER_CUMDEPTH_CSPY				!CUMULATIVE DEPTH IN THE SUBSURFACE
  REAL :: Q_CSPY_LOC, EVAP_CSPY_LOC, SUBLIM_CSPY_LOC, COND_CSPY_LOC, DEPO_CSPY_LOC,  &	!TIMESTEP MASS FLUXES
          REFR_CSPY_LOC, SUBM_CSPY_LOC, SURFM_CSPY_LOC, SNOWBL_LOC, RAINBL_LOC, RHO_FRSH_SNW
  REAL :: SNDPTH, SWE, E_VAP, Z, Z0T, Z0Q, RHOAIR, LV, PHI_M_Z, PHI_TQ_Z, &	        !FEEDBACKS
          PHI_M_Z0, PHI_TQ_Z0, CMFM, CHFH, CQFQ, CD, CQB2, MR_SFC

!----------------------------------PROGRAM START ----------------------------------------

! Initialization
  IF (ITIMESTEP .eq. 1) THEN
  
    CALL WRF_ASPECT(ims,ime,jms,jme,HGT,ASPECT)
    
    LWOUT_CSPY = SQRT(-1.)
    H_CSPY = SQRT(-1.)
    LE_CSPY = SQRT(-1.)
    B_CSPY = SQRT(-1.)
    MB_CSPY = SQRT(-1.)
    SURFMB_CSPY = SQRT(-1.)
    INTMB_CSPY = SQRT(-1.)
    Q_CSPY = SQRT(-1.)
    SNOWH_CSPY = SQRT(-1.)
    TTLH_CSPY = SQRT(-1.)
    TS_CSPY = SQRT(-1.)
    ALB_CSPY = SQRT(-1.)
    ME_CSPY = SQRT(-1.)
    EVAP_CSPY = SQRT(-1.)
    SUBLIM_CSPY = SQRT(-1.)
    COND_CSPY = SQRT(-1.)
    DEPO_CSPY = SQRT(-1.)
    REFR_CSPY = SQRT(-1.)
    SUBM_CSPY = SQRT(-1.)
    SURFM_CSPY = SQRT(-1.)
    Z0_CSPY = SQRT(-1.)
    NSH_CSPY = SQRT(-1.)	
    NST_CSPY = SQRT(-1.)	
    OST_CSPY = SQRT(-1.)
    MOL_CSPY = SQRT(-1.)
        
    NLAYERS_CSPY = -9999
    LAYER_HEIGHT_CSPY = SQRT(-1.)
    LAYER_RHO_CSPY = SQRT(-1.)
    LAYER_T_CSPY = SQRT(-1.)
    LAYER_LWC_CSPY = SQRT(-1.)   
    LAYER_IF_CSPY = SQRT(-1.)   
    
    ! Initialize cumulative mass fluxes
    WHERE(IVGTYP .EQ. ISICE) 
      MB_CSPY = 0.
      SURFMB_CSPY = 0.
      INTMB_CSPY = 0.
      Q_CSPY = 0.
      EVAP_CSPY = 0.
      SUBLIM_CSPY = 0.
      COND_CSPY = 0.
      DEPO_CSPY = 0.
      REFR_CSPY = 0.
      SUBM_CSPY = 0.
      SURFM_CSPY = 0.
    END WHERE    
    
  END IF

!---------------------------------------------------------------------  

! Convert WRF forcing 
  
  ! Relative humidity
  CALL WRF_RH(ims,ime,jms,jme,T_LML(:,1,:),Q_LML(:,1,:),P_LML(:,1,:), RH)
  
  ! Forcing height
  ZLVL = 0.5 * DZ_LML(:,1,:)
  
  ! Windspeed
  WINDSPEED = SQRT(U_LML(:,1,:)**2 + V_LML(:,1,:)**2)	
  WHERE(WINDSPEED .LT. 1.) 
      WINDSPEED = 1.
  END WHERE			

  ! Pressure [hPa]
  PRES = P_LML(:,1,:)/100.
    
!---------------------------------------------------------------------    

! Main loop over WRF tile 
  DO J=jts,jte
    DO I=its,ite  
        
        ! Check if glacierized grid cell
	IF(IVGTYP(I,J) .EQ. ISICE)THEN 
	  
	  ! Compute  instantaneous fresh snow density	  
          RHO_FRSH_SNW = MAX((109.+6.*(T_LML(I,1,J)-TFRZ)+26.*SQRT(WINDSPEED(I,J))), 50.)
	  
	  !Update cumulative precipitation with converting snowfall from mm w.e. to m depth
	  CUMPREC_CSPY(I,J) = CUMPREC_CSPY(I,J) + RAINBL(I,J)	
	  CUMSNOW_CSPY(I,J) = CUMSNOW_CSPY(I,J) + ((RAINBL(I,J)*SRBL(I,J))/1000.)*(RHOW/RHO_FRSH_SNW)
	  
	  !Local precipitation variables -- only pass 10 minute totals
	  RAINBL_LOC = 0.
	  SNOWBL_LOC = 0.
	  IF(MOD(CURR_SECS,600.) .eq. 0)THEN
	    RAINBL_LOC = CUMPREC_CSPY(I,J)
	    SNOWBL_LOC = CUMSNOW_CSPY(I,J)
	    CUMPREC_CSPY(I,J) = 0.
	    CUMSNOW_CSPY(I,J) = 0.  
	  END IF
		   
          ! Run cosipy
	  CALL call_cosipy_wrf(                                                       &
             ITIMESTEP,J,I,DT,max_layers_cspy,HGT(I,J),SLOPE(I,J)/DEGRAD,ASPECT(I,J), &
	     ZLVL(I,J),T_LML(I,1,J),RH(I,J),PRES(I,J),WINDSPEED(I,J),SWDWN(I,J),      &
	     LWDWN(I,J),RAINBL_LOC,SNOWBL_LOC,SNOWH(I,J),SNOW(I,J),TSK(I,J),          &
	     LWOUT_CSPY(I,J),H_CSPY(I,J),LE_CSPY(I,J),B_CSPY(I,J),QRR_CSPY(I,J),      &
	     MB_CSPY(I,J),SURFMB_CSPY(I,J),INTMB_CSPY(I,J),Q_CSPY_LOC,SNOWH_CSPY(I,J),&
	     TTLH_CSPY(I,J),TS_CSPY(I,J),ALB_CSPY(I,J),ME_CSPY(I,J),EVAP_CSPY_LOC,    &
	     SUBLIM_CSPY_LOC,COND_CSPY_LOC,DEPO_CSPY_LOC,REFR_CSPY_LOC,SUBM_CSPY_LOC, &
	     SURFM_CSPY_LOC,Z0_CSPY(I,J),NSH_CSPY(I,J),NST_CSPY(I,J),OST_CSPY(I,J),   &
	     MOL_CSPY(I,J),NLAYERS_CSPY(I,J),LAYER_HEIGHT_CSPY(I,:,J),                &
	     LAYER_RHO_CSPY(I,:,J),LAYER_T_CSPY(I,:,J),LAYER_LWC_CSPY(I,:,J),         &
	     LAYER_IF_CSPY(I,:,J))
	       
	       
	  ! Cumulative mass fluxes
	  ! MB, SURFMB, INTMB are always cumulative (in call_cosipy_wrf.py)
	  ! Reset if following history write
	  IF(reset_cspy)THEN			
             Q_CSPY(I,J)        = 0.
             EVAP_CSPY(I,J)	= 0.
             SUBLIM_CSPY(I,J)   = 0.
             COND_CSPY(I,J)	= 0.
             DEPO_CSPY(I,J)	= 0.
             REFR_CSPY(I,J)	= 0.
             SUBM_CSPY(I,J)	= 0.
             SURFM_CSPY(I,J)	= 0.
          END IF
          Q_CSPY(I,J)           = Q_CSPY(I,J)      + Q_CSPY_LOC
          EVAP_CSPY(I,J)	= EVAP_CSPY(I,J)   + EVAP_CSPY_LOC
          SUBLIM_CSPY(I,J)      = SUBLIM_CSPY(I,J) + SUBLIM_CSPY_LOC
          COND_CSPY(I,J)	= COND_CSPY(I,J)   + COND_CSPY_LOC
          DEPO_CSPY(I,J)	= DEPO_CSPY(I,J)   + DEPO_CSPY_LOC
          REFR_CSPY(I,J)	= REFR_CSPY(I,J)   + REFR_CSPY_LOC
          SUBM_CSPY(I,J)	= SUBM_CSPY(I,J)   + SUBM_CSPY_LOC
          SURFM_CSPY(I,J)	= SURFM_CSPY(I,J)  + SURFM_CSPY_LOC

	  ! Update WRF variables if interactive simulation
	  IF(run_cspy .EQ. 2)THEN  

	     ! SNOW
	     SNDPTH = 0.
	     SWE = 0.
	     FSNO = 0
	     K = 1
	     DO WHILE (LAYER_RHO_CSPY(I,K,J) .LT. 900.)
	       SNDPTH 		= SNDPTH + LAYER_HEIGHT_CSPY(I,K,J)
	       SWE 		= SWE + (LAYER_HEIGHT_CSPY(I,K,J) * LAYER_RHO_CSPY(I,K,J))
	       FSNO 		= 1
	       K 		= K + 1
	     END DO
             SNOWC    (I,J)	= FSNO
             SNOW     (I,J)	= SWE
             SNOWH    (I,J)	= SNDPTH	
	     
	     
	     ! FLXS IN WRF + TO ATMOS EXCEPT GHF
	     LV = HVAP
	     IF(TS_CSPY(I,J) .LT. TFRZ) LV = HSUB
             TSK      (I,J)	= TS_CSPY(I,J)
             HFX      (I,J)	= -1.*H_CSPY(I,J)				!NMP:(TGB - SFCTMP)
             LH       (I,J)	= -1.*LE_CSPY(I,J)				!NMP:(ESTG*RHSUR - EAIR)
             QFX      (I,J)	= -1.*LE_CSPY(I,J)/LV 
             GRDFLX   (I,J)	= B_CSPY(I,J) 		   			!NMP:(TGB - STC(ISNOW+1))
	     
	     E_VAP = 6.112
	     IF(TS_CSPY(I,J) .LT. TFRZ) E_VAP = 6.112 * EXP((22.46*(TS_CSPY(I,J)-273.16))/((TS_CSPY(I,J)-0.55)))
	     E_VAP = E_VAP*100.	!convert to Pa
             QSFC     (I,J)	= 0.622 * E_VAP / (P_LML(I,1,J) - 0.378*E_VAP)
             Z0       (I,J)	= Z0_CSPY(I,J)
             ZNT      (I,J)	= Z0_CSPY(I,J)
             ALBEDO   (I,J)	= ALB_CSPY(I,J)
             EMISS    (I,J)	= 0.99


             ! SUBSURFACE
             SMOIS(I,1:NSOIL,J)	= 1.0	! Specify totally saturated
             SH2O (I,1:NSOIL,J)	= 0.0	! Specify totally frozen
	     
	     LAYER_CUMDEPTH_CSPY 	= 0.
	     LAYER_CUMDEPTH_CSPY(1) 	= LAYER_HEIGHT_CSPY(I,1,J)
	     DO K = 2, NLAYERS_CSPY(I,J)
	       LAYER_CUMDEPTH_CSPY(K) 	= LAYER_HEIGHT_CSPY(I,K,J) + LAYER_CUMDEPTH_CSPY(K-1)
	     END DO
	     
	     DO K = 1, NSOIL
	       Z 		= ZS(K) + (DZS(K)*0.5)
               IZ 		= MINLOC(ABS(LAYER_CUMDEPTH_CSPY - Z), 1)
               TSLB   (I,K,J)	= LAYER_T_CSPY(I,IZ,J)
	     END DO
	     IZ 		= MINLOC(ABS(LAYER_CUMDEPTH_CSPY - 8.), 1)	!NMP: ZBOT is 8m
	     TMN      (I,J)	= LAYER_T_CSPY(I,IZ,J)				
	     
	     
	     ! CUMULATIVE
	     IF(SNDPTH .GT. 0.)THEN
               UDRUNOFF(I,J)	= UDRUNOFF(I,J)  + (Q_CSPY_LOC*1000.)	!convert [m w.e.] back to [mm w.e.]
	     ELSE
               SFCRUNOFF(I,J)	= SFCRUNOFF(I,J) + (Q_CSPY_LOC*1000.)	   
	     END IF
             ACSNOW   (I,J)	= ACSNOW(I,J) + (SNOWBL_LOC*1000.)*(RHO_FRSH_SNW/RHOW)	
             ACSNOM   (I,J)	= ACSNOM(I,J) + (SUBM_CSPY_LOC+SURFM_CSPY_LOC)*1000.	   


	     ! SFC-ATMOS EXCHANGE
             Z0T = Z0_CSPY(I,J)/100.
	     Z0Q = Z0_CSPY(I,J)/10.
	     
	     RHOAIR = P_LML(I,1,J) / (RAIR * (T_LML(I,1,J) * (1. + 0.608 * Q_LML(I,1,J))))
	     
	     !------------ Lowest half model level
	     Z = ZLVL(I,J)
	     
	     !MO stability functions
	     CALL GET_PHI(Z,MOL_CSPY(I,J),PHI_M_Z,PHI_TQ_Z)
	     CALL GET_PHI(Z0_CSPY(I,J),MOL_CSPY(I,J),PHI_M_Z0,PHI_TQ_Z0)

             CMFM = LOG(Z / Z0_CSPY(I,J)) - PHI_M_Z - PHI_M_Z0
             CHFH = LOG(Z / Z0T) - PHI_TQ_Z - PHI_TQ_Z0

             IF(ABS(CMFM) .LT. MPE) CMFM = MPE			!Borrowed from NMP
             IF(ABS(CHFH) .LT. MPE) CHFH = MPE

	     !Exchange coefficients
	     CMXY(I,J)	= VKC**2 / CMFM**2
	     CHXY(I,J)	= VKC * SQRT(CMXY(I,J)) / CHFH 
	     CHBXY(I,J)	= CHXY(I,J)
	     
	     !------------ 2m
	     Z = 2. + Z0_CSPY(I,J)
	     
	     CALL GET_PHI(Z,MOL_CSPY(I,J),PHI_M_Z,PHI_TQ_Z)	

             CMFM = LOG(Z / Z0_CSPY(I,J)) - PHI_M_Z - PHI_M_Z0
             CHFH = LOG((2.0 + Z0T) / Z0T) - PHI_TQ_Z - PHI_TQ_Z0
             CQFQ = LOG((2.0 + Z0Q) / Z0Q) - PHI_TQ_Z - PHI_TQ_Z0
	    
             IF(ABS(CMFM) .LT. MPE) CMFM = MPE
             IF(ABS(CHFH) .LT. MPE) CHFH = MPE
             IF(ABS(CQFQ) .LT. MPE) CQFQ = MPE
	     
             CD          = VKC**2 / CMFM**2
             CHB2XY(I,J) = VKC * SQRT(CD) / CHFH
             CQB2        = VKC * SQRT(CD) / CQFQ

	     IF (CHB2XY(I,J).LT.1.E-5) THEN
               T2MBXY (I,J)	= TS_CSPY(I,J)
	     ELSE   								
	       ! TO DO: Consider LE
               T2MBXY (I,J)	= TS_CSPY(I,J) + H_CSPY(I,J)/(RHOAIR*CPAIR*WINDSPEED(I,J)) * 1./CHB2XY(I,J)
	     END IF
	   
	     MR_SFC 		= QSFC(I,J) / (1. - QSFC(I,J)) ! Q2MBXY is mixing ratio
	     IF (CQB2.LT.1.E-5) THEN
               Q2MBXY (I,J)	= MR_SFC
	     ELSE
               Q2MBXY (I,J)	= MR_SFC + LE_CSPY(I,J)/(LV*RHOAIR*WINDSPEED(I,J)) * 1./CQB2
	     END IF 
	   
	  END IF	! IF INTERACTIVE
	    	  
	END IF		! IF ISICE
    
    END DO		! I loop
  END DO		! J loop
      
!---------------------------------------------------------------------              

END SUBROUTINE glacier_mass_balance

!---------------------------------------------------------------------              

SUBROUTINE GET_PHI(Z,L,PHI_M,PHI_TQ)
  IMPLICIT NONE
  
  REAL, INTENT(IN) :: Z, L
  REAL, INTENT(OUT) :: PHI_M, PHI_TQ
  REAL :: MOZ,TMP1,TMP2,TMP3
  
  PHI_M  = 0.
  PHI_TQ = 0.
  MOZ    = MIN(Z/L, 1.)		!Cheap trick from NMP
  
  IF (MOZ .LT. 0.) THEN
      TMP1   = (1. - 16.*MOZ)**0.25
      TMP2   = LOG((1.+TMP1*TMP1)/2.)
      TMP3   = LOG((1.+TMP1)/2.)
      PHI_M  = 2.*TMP3 + TMP2 - 2.*ATAN(TMP1) + 1.5707963
      PHI_TQ = 2.*TMP2
  ELSE
    PHI_M  = -5.*MOZ
    PHI_TQ = PHI_M  
  END IF  
 
END SUBROUTINE GET_PHI

!---------------------------------------------------------------------              

END MODULE cosipy_wrf

===1===
     &        ,max_cspy_layers,run_cspy,reset_cspy,CUMPREC_CSPY                     &
     &        ,CUMSNOW_CSPY,ASPECT_CSPY,LWOUT_CSPY,H_CSPY,LE_CSPY,B_CSPY,QRR_CSPY   &
     &        ,MB_CSPY,SURFMB_CSPY,INTMB_CSPY,Q_CSPY,SNOWH_CSPY,TTLH_CSPY,TS_CSPY   &
     &        ,ALB_CSPY,ME_CSPY,EVAP_CSPY,SUBLIM_CSPY,COND_CSPY,DEPO_CSPY,REFR_CSPY &
     &        ,SUBM_CSPY,SURFM_CSPY,Z0_CSPY,NSH_CSPY,NST_CSPY,OST_CSPY,MOL_CSPY     &
     &        ,NLAYERS_CSPY,LAYER_HEIGHT_CSPY,LAYER_RHO_CSPY,LAYER_T_CSPY           &
     &        ,LAYER_LWC_CSPY,LAYER_IF_CSPY                                         &
===1===
===2===
  INTEGER, INTENT(IN)    :: max_cspy_layers, run_cspy
  LOGICAL, INTENT(IN)    :: reset_cspy
  INTEGER, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: NLAYERS_CSPY  
  REAL,    DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: CUMPREC_CSPY,CUMSNOW_CSPY,ASPECT_CSPY,LWOUT_CSPY,H_CSPY, &
  			     LE_CSPY,B_CSPY,QRR_CSPY,MB_CSPY,SURFMB_CSPY,INTMB_CSPY,Q_CSPY,SNOWH_CSPY,TTLH_CSPY,  &
			     TS_CSPY,ALB_CSPY,ME_CSPY,EVAP_CSPY,SUBLIM_CSPY,COND_CSPY,DEPO_CSPY,REFR_CSPY,        &
			     SUBM_CSPY,SURFM_CSPY,Z0_CSPY,NSH_CSPY,NST_CSPY,OST_CSPY,MOL_CSPY
  REAL,    DIMENSION(ims:ime, 1:max_cspy_layers, jms:jme), INTENT(INOUT) :: LAYER_HEIGHT_CSPY,LAYER_RHO_CSPY,     &
                                                                   LAYER_T_CSPY,LAYER_LWC_CSPY,LAYER_IF_CSPY
===2===
===3===
           CALL wrf_debug(100,'before COSIPY call')

	   ! Check if running COSIPY for this domain
	   IF(run_cspy .GT. 0)THEN
             CALL glacier_mass_balance(ITIMESTEP,curr_secs,DTBL,ims,ime,jms,jme,kms,kme,i_start(ij),     &!Time, space
	         i_end(ij),j_start(ij),j_end(ij),ISICE,max_cspy_layers,run_cspy,reset_cspy,              &          
		 XLAT,XLONG,HT,slope,ASPECT_CSPY,IVGTYP,DZ8W,T_PHY,QV_CURR,P8W,U_PHY,V_PHY,SWDOWN,GLW,   &!Forcing
		 CUMPREC_CSPY,CUMSNOW_CSPY,RAINBL,SR,SNOWH,SNOW,SNOWC,TSK,                               &
		 LWOUT_CSPY,H_CSPY,LE_CSPY,B_CSPY,QRR_CSPY,MB_CSPY,SURFMB_CSPY,INTMB_CSPY,Q_CSPY,        &!CSPY
		 SNOWH_CSPY,TTLH_CSPY,TS_CSPY,ALB_CSPY,ME_CSPY,EVAP_CSPY,SUBLIM_CSPY,COND_CSPY,DEPO_CSPY,&
		 REFR_CSPY,SUBM_CSPY,SURFM_CSPY,Z0_CSPY,NSH_CSPY,NST_CSPY,OST_CSPY,MOL_CSPY,             &
		 NLAYERS_CSPY,LAYER_HEIGHT_CSPY,LAYER_RHO_CSPY,LAYER_T_CSPY,LAYER_LWC_CSPY,LAYER_IF_CSPY,& 
		 num_soil_layers,ZS,DZS,HFX,LH,QFX,GRDFLX,QSFC,Z0,ZNT,ALBEDO,EMISS,UDRUNOFF,SFCRUNOFF,   &!Interactive
		 SMOIS,SH2O,TSLB,TMN,ACSNOW,ACSNOM,CMXY,CHXY,T2MBXY,Q2MBXY,CHBXY,CHB2XY)
           END IF
===3===

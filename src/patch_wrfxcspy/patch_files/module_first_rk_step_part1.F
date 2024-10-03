===1===
    TYPE(WRFU_Time)                :: histTime
    TYPE(WRFU_TimeInterval)        :: dtInt
    LOGICAL                        :: reset_cspy
===1===
===2===
      !Check if timestep immediately following a history write
      CALL WRFU_ALARMGET( grid%alarms( HISTORY_ALARM ), prevringtime=histTime)
      CALL domain_clock_get ( grid, current_time=currentTime, time_step=dtInt)
      reset_cspy = ( currentTime .lt. histTime + dtInt ) 
===2===
===3===
     &        ,max_cspy_layers=config_flags%max_cspy_layers,run_cspy=config_flags%run_cspy         &
     &        ,reset_cspy=reset_cspy,CUMPREC_CSPY=grid%CUMPREC_CSPY,CUMSNOW_CSPY=grid%CUMSNOW_CSPY &
     &        ,ASPECT_CSPY=grid%ASPECT_CSPY,LWOUT_CSPY=grid%LWOUT_CSPY,H_CSPY=grid%H_CSPY          &
     &        ,LE_CSPY=grid%LE_CSPY,B_CSPY=grid%B_CSPY,QRR_CSPY=grid%QRR_CSPY                      &
     &        ,MB_CSPY=grid%MB_CSPY,SURFMB_CSPY=grid%SURFMB_CSPY,INTMB_CSPY=grid%INTMB_CSPY        &
     &        ,Q_CSPY=grid%Q_CSPY,SNOWH_CSPY=grid%SNOWH_CSPY,TTLH_CSPY=grid%TTLH_CSPY              &
     &        ,TS_CSPY=grid%TS_CSPY,ALB_CSPY=grid%ALB_CSPY,ME_CSPY=grid%ME_CSPY                    &
     &        ,EVAP_CSPY=grid%EVAP_CSPY,SUBLIM_CSPY=grid%SUBLIM_CSPY,COND_CSPY=grid%COND_CSPY      &
     &        ,DEPO_CSPY=grid%DEPO_CSPY,REFR_CSPY=grid%REFR_CSPY,SUBM_CSPY=grid%SUBM_CSPY          &
     &        ,SURFM_CSPY=grid%SURFM_CSPY,Z0_CSPY=grid%Z0_CSPY,NSH_CSPY=grid%NSH_CSPY              &
     &        ,NST_CSPY=grid%NST_CSPY,OST_CSPY=grid%OST_CSPY,MOL_CSPY=grid%MOL_CSPY                &
     &        ,NLAYERS_CSPY=grid%NLAYERS_CSPY,LAYER_HEIGHT_CSPY=grid%LAYER_HEIGHT_CSPY             &
     &        ,LAYER_RHO_CSPY=grid%LAYER_RHO_CSPY,LAYER_T_CSPY=grid%LAYER_T_CSPY                   &
     &        ,LAYER_LWC_CSPY=grid%LAYER_LWC_CSPY,LAYER_IF_CSPY=grid%LAYER_IF_CSPY                 &
===3===

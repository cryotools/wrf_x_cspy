===1===
	 ELSE

	   !Annoying but necessary bypass of Noah-MP update (for cspyt > 0)
           TRAD   = TSK(I,J)
           EMISSI = EMISS(I,J)
           FSA    = undefined_value
           FSR    = undefined_value
           FIRA   = undefined_value
           FSH    = HFX(I,J)
           FGEV   = LH(I,J)
           SSOIL  = GRDFLX(I,J)
           ESOIL  = QFX(I,J)
           SAG    = undefined_value
           SALB   = ALBEDO(I,J)
           Z0WRF  = Z0(I,J)

           FSNO   = SNOWC(I,J)  
	   ISNOW  = 0 
	   ZSNSO  = undefined_value
	   SNICE  = undefined_value
	   SNLIQ  = undefined_value
	   TAUSS  = undefined_value
	   
           FPICE  = 0.
           QSNBOT = 0.
	   PONDING  = 0.
	   PONDING1 = 0.
	   PONDING2 = 0.
           RUNSF  = 0.
           RUNSB  = 0.

           CHB2 = CHB2XY(I,J)
           T2MB = T2MBXY(I,J)
           Q2MB = Q2MBXY(I,J)/(1.0 + Q2MBXY(I,J))   ! specific humidity expected
         END IF
===1===

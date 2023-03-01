SUBROUTINE run(gws_l, sws_l, sm_l, epv_l, gw_sm_interconnectivity_l)
	USE sm_helpers, only: kUS

	IMPLICIT NONE

	REAL(8) :: L, sw_et_deficit, excess_gw_vol, sm_eq, k_inf, inf, excess_p, inf_deficit, sw_inf, &
		k_inf_gw, inf_gw, et_deficit, sw_et, interconnectivity_ratio
	REAL(8), INTENT(INOUT) :: gws_l(:,:), sws_l(:,:), sm_l(:,:), epv_l(:,:), gw_sm_interconnectivity_l(:)
	INTEGER :: e, t

	CHARACTER(255) :: Qin_file, Qout_file, Qdiff_file

	tlocal_start = timefetch()

	CALL logger%log(logger%info, "initializing run")

	gws = gws_l
	sws = sws_l
	sm = sm_l
	epv = epv_l
	gw_sm_interconnectivity = gw_sm_interconnectivity_l

	CALL logger%log(logger%moreinfo, "run initialised")

	CALL logger%log(logger%moreinfo, "starting the solver")
	!$OMP PARALLEL DO SHARED(gws, sws, sm, epv, gw_dis, sw_dis, sm_dis, Qin, Qout) &
	!$OMP PRIVATE(L, sw_et_deficit, excess_gw_vol, sm_eq, k_inf, inf, excess_p, inf_deficit, sw_inf, k_inf_gw, inf_gw) &
	!$OMP PRIVATE(et_deficit, sw_et, interconnectivity_ratio)
	DO e = 1, elems
		CALL logger%log(logger%trace, "outer loop entered. elem ", e)
		DO t = 2, nts+1
		!NOMP CRITICAL
			CALL logger%log(logger%trace, "inner loop entered. ts", t-1)
			CALL logger%log(logger%debug, "gok", gok(e))
			CALL logger%log(logger%debug, "bot", bot(e))
			IF(.NOT. chd(e)) THEN
				L = gok(e) - gws(e,t-1) !prev. GW depth
				IF(L<0 .OR. L==0) THEN !NO UZ case
					CALL logger%log(logger%trace, "noUZ entered")
					CALL logger%log(logger%debug, "gws is ", gws(e,t-1))
					CALL logger%log(logger%debug, "sws is ", sws(e,t-1))
					CALL logger%log(logger%debug, "sm is ", sm(e,t-1))
					excess_gw_vol = -L*n(e) + sm(e,t-1)
					gws(e,t) = gok(e)
					sm(e,t) = 0
					epv(e,t) = 0
					sws(e,t) = sws(e,t-1) + excess_gw_vol + p(e,t)*dt
					CALL logger%log(logger%debug, "excess_gw_vol ", excess_gw_vol)
					CALL logger%log(logger%debug, "gws after +p ", gws(e,t))
					CALL logger%log(logger%debug, "sws after +p", sws(e,t))
					CALL logger%log(logger%debug, "sm after +p", sm(e,t))
					!ET extraction
					IF (sws(e,t)>et(e,t)*dt) THEN
						sws(e,t) = sws(e,t) - et(e,t)*dt
					ELSE
						sw_et_deficit = et(e,t)*dt - sws(e,t)
						sws(e,t) = 0
						gws(e,t) = gws(e,t) - (sw_et_deficit/n(e))
						epv(e,t) = (gok(e) - gws(e,t))*n(e)
					END IF
					CALL logger%log(logger%debug, "gws after -et ", gws(e,t))
					CALL logger%log(logger%debug, "sws after -et", sws(e,t))
					CALL logger%log(logger%debug, "sm after -et", sm(e,t))
					!calc storage discharges
					gw_dis(e,t) = (gws(e,t) - gws(e,t-1))*n(e)
					sm_dis(e,t) = (sm(e,t)) - sm(e,t-1)
					sw_dis(e,t) = sws(e,t) - sws(e,t-1)
					Qin(e,t) = p(e,t)*dt - et(e,t)*dt
					Qout(e,t) = gw_dis(e,t) + sw_dis(e,t) + sm_dis(e,t)
					sw_et_deficit = 0
				ELSE
					! CALL logger%log(logger%trace, "UZ entered")
					!P dist and SW push
					CALL logger%log(logger%debug, "L is", L)
					CALL logger%log(logger%debug, "P is", p(e,t)*dt)
					CALL logger%log(logger%debug, "sm is", sm(e,t-1))
					CALL logger%log(logger%debug, "epv is", epv(e,t-1))
					CALL logger%log(logger%debug, "sm/epv", sm(e,t-1)/epv(e,t-1))
					k_inf = kUS(MIN(sm(e,t-1)/epv(e,t-1), 1.0)*n(e), k(e)) !calc K from wetness at the begining of this dt i.e. END of last dt
					CALL logger%log(logger%debug, "got k", k_inf)
					inf = MIN(k_inf*dt, p(e,t)*dt)
					CALL logger%log(logger%debug, "inf aka p_sm is ", inf)
					excess_p = p(e,t)*dt - inf
					CALL logger%log(logger%debug, "excess p aka p_sw is ", excess_p)
					CALL logger%log(logger%debug, "sws is", sws(e,t-1))
					CALL logger%log(logger%debug, "ET is", et(e,t)*dt)
					sw_et = MIN(sws(e,t-1)+excess_p, et(e,t)*dt)
					inf_deficit = k_inf*dt - inf
					CALL logger%log(logger%debug, "inf_deficit", inf_deficit)
					sw_inf = MIN(inf_deficit, sws(e,t-1)+excess_p-sw_et)
					CALL logger%log(logger%debug, "sw_inf", sw_inf)
					sws(e,t) = sws(e,t-1) - sw_inf + excess_p - sw_et
					et_deficit = et(e,t)*dt - sw_et
					IF(gws(e,t-1) <= bot(e)) THEN
						et_deficit = 0
					END IF
					CALL logger%log(logger%debug, "sw et removed", sw_et)
					CALL logger%log(logger%debug, "sws calcd", sws(e,t))
					sm(e,t) = sm(e,t-1) + inf + sw_inf - et_deficit
					CALL logger%log(logger%debug, "sm et removed", et_deficit)
					CALL logger%log(logger%debug, "sm calcd", sm(e,t))
					sm_eq = vanG% integrate(L, 0.0_8)
					CALL logger%log(logger%debug, "gws is ", gws(e,t-1))
					CALL logger%log(logger%debug, "vanGI_fgsl called. sm_eq is ", sm_eq)
					! consider capping gw_sm_interconnectivity to L
					IF(inf /= 0) THEN
						gw_sm_interconnectivity(e) = MAX((gw_sm_interconnectivity(e) + k_inf*dt), 0.0)
					END IF
					k_inf_gw = kUS(MIN(sm(e,t)/epv(e,t-1), 1.0)*n(e), k(e)) !calc K from current wetness (after P and SW inf)
					interconnectivity_ratio = MIN(1.0, MAX(gw_sm_interconnectivity(e)/abs(L), vanG% theta_r+macropore_inf_degree(e)))
					inf_gw = MIN((sm(e,t)-sm_eq)*interconnectivity_ratio, (k_inf_gw*dt)*interconnectivity_ratio, (sm(e,t)-sm_eq)) !IF sm<sm_eq, inf_gw is -ve ...
					IF(gws(e,t-1) + inf_gw/n(e) < bot(e)) THEN
						inf_gw = - MIN(abs((gws(e,t-1) - bot(e)))*n(e), abs(k_inf_gw*dt))
					END IF
					IF(inf_gw < 0) THEN
						gw_sm_interconnectivity(e) = (gw_sm_interconnectivity(e) + inf_gw)
					END IF
					CALL logger%log(logger%debug, "gw_sm_interconnectivity is", gw_sm_interconnectivity(e))
					CALL logger%log(logger%debug, "interconnectivity_ratio is", interconnectivity_ratio)
					CALL logger%log(logger%debug, "k_inf_gw is", k_inf_gw)
					CALL logger%log(logger%debug, "inf_gw is", inf_gw)
					sm(e,t) = sm(e,t) - inf_gw !... deficit sm gets added to sm from gw
					CALL logger%log(logger%debug, "sm recalcd ", sm(e,t))
					gws(e,t) = gws(e,t-1) + inf_gw/n(e) !... and subtracted from gw
					CALL logger%log(logger%debug, "gws calcd", gws(e,t))
					IF(gws(e,t)>gok(e)) THEN
						excess_gw_vol = (gws(e,t)-gok(e))*n(e) + sm(e,t)
						gws(e,t) = gok(e)
						sm(e,t) = 0
						sws(e,t) = sws(e,t) + excess_gw_vol
						CALL logger%log(logger%debug, "gws recalcd", gws(e,t))
					END IF
					epv(e,t) = (gok(e) - gws(e,t))*n(e)
					IF(sm(e,t)>epv(e,t)) THEN
						sws(e,t) = sws(e,t) + (sm(e,t)-epv(e,t))
						sm(e,t) = epv(e,t)
					END IF
					L = gok(e) - gws(e,t)
					sm_eq = vanG% integrate(L, 0.0_8) !!!gw-sm balancing: consider adding a convergence criteria here
					CALL logger%log(logger%debug, "new sm_eq", sm_eq)
					k_inf_gw = kUS(MIN(sm(e,t)/epv(e,t), 1.0)*n(e), k(e))*dt - MAX(inf_gw, 0.00) !subtract k_inf_gw alREADy utilized and allow freely capilary rise beyond k_inf_gw
					CALL logger%log(logger%debug, "k_inf_gw remaining", k_inf_gw)
					interconnectivity_ratio = MIN(1.0, MAX(gw_sm_interconnectivity(e)/abs(L), vanG% theta_r+macropore_inf_degree(e)))
					inf_gw = MIN((sm(e,t)-sm_eq)*interconnectivity_ratio, (MAX(k_inf_gw*dt,0.0))*interconnectivity_ratio, (sm(e,t)-sm_eq))
					IF(gws(e,t) + inf_gw/n(e) < bot(e)) THEN
						inf_gw = - MIN(abs((gws(e,t) - bot(e)))*n(e), k_inf_gw*dt)
						IF(sm(e,t)<0) THEN
							sm(e,t) = 0
						END IF
					END IF
					IF(inf_gw < 0) THEN
						gw_sm_interconnectivity(e) = (gw_sm_interconnectivity(e) + inf_gw)
					END IF
					CALL logger%log(logger%debug, "addnl inf_gw", inf_gw)
					sm(e,t) = sm(e,t) - inf_gw
					gws(e,t) = gws(e,t) + inf_gw/n(e)
					CALL logger%log(logger%debug, "sm-gw balanced", sm(e,t), gws(e,t))

					epv(e,t) = (gok(e) - gws(e,t))*n(e)
					gw_dis(e,t) = (gws(e,t) - gws(e,t-1))*n(e)
					sw_dis(e,t) = sws(e,t) - (sws(e,t-1))
					sm_dis(e,t) = sm(e,t) - sm(e,t-1)
					Qin(e,t) = p(e,t)*dt - et(e,t)*dt
					Qout(e,t) = gw_dis(e,t) + sw_dis(e,t) + sm_dis(e,t)
				END IF
			ELSE
				excess_gw_vol = sm(e,t-1)
				gws(e,t) = gws(e,t-1)
				sm(e,t) = 0
				epv(e,t) = 0
				sws(e,t) = sws(e,t-1) + p(e,t)*dt - et(e,t)*dt + excess_gw_vol		
				gw_dis(e,t) = 0
				sw_dis(e,t) = sws(e,t) - sws(e,t-1)
				sm_dis(e,t) = 0
				Qin(e,t) = p(e,t)*dt - et(e,t)*dt
				Qout(e,t) = gw_dis(e,t) + sw_dis(e,t) + sm_dis(e,t)
			END IF
		!NOMP END CRITICAL
		END DO
	END DO
	!$OMP END PARALLEL DO

	CALL logger%log(logger%moreinfo, "solver terminated successfully")

	!$OMP WORKSHARE
	Qdiff = Qin - Qout
	gws_l = gws
	sws_l = sws
	sm_l = sm
	epv_l = epv
	!$OMP END WORKSHARE

	CALL logger%log(logger%moreinfo, "writing output files")

	CALL logger%log(logger%moreinfo, "output files written successfully")
	
	tlocal_end = timefetch()

	write(strbuffer,*) (tlocal_end-tlocal_start)
	strbuffer = "run terminated successfully in "//TRIM(strbuffer)//" s"//achar(10)
	CALL logger%log(logger%info, strbuffer)

	tglobal_end = timefetch()
	telapsed = tglobal_end - tglobal_start
	WRITE(strbuffer,*) (tlocal_end-tlocal_start)
	strbuffer = "Program terminated successfully in "//TRIM(strbuffer)//" s"
	CALL logger%log(logger%info, TRIM(strbuffer))
	flush(logger%unit)
END SUBROUTINE
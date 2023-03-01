SUBROUTINE solve(e, first_run, lateral_GW_flux, lateral_SW_flux)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: e
    LOGICAL, INTENT(IN) :: first_run
    REAL(REAL128), INTENT(IN), DIMENSION(time% Lnts), OPTIONAL :: lateral_GW_flux, lateral_SW_flux

    REAL(REAL64), POINTER :: porosity_gwbnd, porosity_gwbnd_above, porosity_gwbnd_below
    REAL(REAL128) :: ET, P
    REAL(REAL32) :: dt
    INTEGER(INT32), POINTER :: t
    TYPE(Csm), POINTER :: pSM_

    REAL(REAL128) :: infiltration_deficit, et_sw, excess_precipitation, infiltration_sw, et_deficit, prev_gw_storage
    INTEGER(INT16) :: itr

    IF(first_run) THEN
        ET = EXTF% et(e, time% Gts) * time% Ldt% total_seconds()
        P = EXTF% p(e, time% Gts) * time% Ldt% total_seconds()
    ELSE
        ET = 0.0
        P = 0.0
    END IF

    dt = time% Ldt% total_seconds()
    t => time% Lts

    porosity_gwbnd_above => UZ_(e)% SM(UZ_(e)% gws_bnd_smid-1)% porosity
    porosity_gwbnd => UZ_(e)% SM(UZ_(e)% gws_bnd_smid)% porosity
    porosity_gwbnd_below => UZ_(e)% SM(UZ_(e)% gws_bnd_smid+1)% porosity

    DO t = 2, time% Lnts
        time% Lts = t

        IF (.NOT. GW% chd(e)) THEN
        !### free GW boundary case
            CALL UZ_(e)% resolve(e, UZ, GW, time, solver_settings)

            IF (UZ_(e)% isactive) THEN
            !## case 1: UZ is active
                pSM_ => UZ_(e)% SM(1)

                pSM_% kUS_inf = pSM_% vanG% kUS(MIN((pSM_% Lstorage(t-1) / pSM_% Lepv(t-1)* pSM_% porosity), pSM_% porosity), pSM_% ks)

                pSM_% infiltration = MIN(P, pSM_% kUS_inf * dt)
                excess_precipitation = P - pSM_% infiltration
                infiltration_deficit = (pSM_% kUS_inf * dt) - pSM_% infiltration

                IF(pSM_% infiltration /= 0.0) pSM_% IC = MAX(pSM_% IC + pSM_% kUS_inf * dt, 0.0)
                pSM_% IC_ratio = MIN(1.0, MAX(pSM_% IC / ABS(pSM_% RWubound - pSM_% RWubound), pSM_% vanG% theta_r))

                et_sw = MIN(SW% Lstorage(e,t-1) + excess_precipitation, ET)
                infiltration_sw = MIN(SW% Lstorage(e,t-1) + excess_precipitation - et_sw, infiltration_deficit)

                SW% Lstorage(e,t) = SW% Lstorage(e,t-1) + excess_precipitation - et_sw - infiltration_sw

                et_deficit = ET - et_sw
                IF(GW% Lstorage(e,t-1) + (et_deficit / porosity_gwbnd) < UZ% bot(UZ% nlay, e) .OR. GW% Lstorage(e,t-1) == UZ% bot(UZ% nlay, e)) &
                    et_deficit = (GW% Lstorage(e,t-1) - UZ% bot(UZ% nlay, e)) * porosity_gwbnd ! no ET extraction from GW if GW is below UZ bottom

                pSM_% Lstorage(t) = pSM_% Lstorage(t-1) + pSM_% infiltration + infiltration_sw - et_deficit
                IF(pSM_% Lstorage(t) > pSM_% Lepv(t)) THEN
                    SW% Lstorage(e,t) = SW% Lstorage(e,t) + (pSM_% Lstorage(t) - pSM_% Lepv(t))
                    pSM_% Lstorage(t) = pSM_% Lepv(t)
                END IF

                CALL UZ_(e)% solve(e, UZ, time, first_run=.TRUE.)

                pSM_ => UZ_(e)% SM(UZ_(e)% gws_bnd_smid)
                GW% Lstorage(e,t) = GW% Lstorage(e,t-1) + (pSM_% exfiltration / pSM_% porosity)

                CALL UZ_(e)% resolve(e, UZ, GW, time, solver_settings)
                IF(.NOT. UZ_(e)% isactive) CALL solve(e, first_run=.FALSE.)

                prev_gw_storage = 0.0
                itr = 0
                DO WHILE(ABS(GW% Lstorage(e,t) - prev_gw_storage) > solver_settings% sm_gw_fluctuation_tolerance .AND. itr < solver_settings% max_iterations)
                    prev_gw_storage = GW% Lstorage(e,t)
                    itr = itr + 1
                    CALL UZ_(e)% solve(e, UZ, time, first_run=.FALSE.)
                    CALL UZ_(e)% resolve(e, UZ, GW, time, solver_settings)
                END DO

            ! calculate discharges
                pSM_ => UZ_(e)% SM(UZ_(e)% gws_bnd_smid)
                IF(GW% Lstorage(e,t-1) < pSM_% ADubound .AND. GW% Lstorage(e,t) > pSM_% ADubound) THEN
                    GW% Ldischarge(e,t) = (GW% Lstorage(e,t) - GW% Lstorage(e,t-1)) * porosity_gwbnd
                ELSE IF(GW% Lstorage(e,t-1) > pSM_% ADubound) THEN
                ! GW fell a layer
                    GW% Ldischarge(e,t) = (pSM_% ADubound - GW% Lstorage(e,t-1)) * porosity_gwbnd_above + &
                        (GW% Lstorage(e,t) - pSM_% ADubound) * porosity_gwbnd
                ELSE
                ! GW rose a layer
                    GW% Ldischarge(e,t) = pSM_% ADlbound - GW% Lstorage(e,t-1) * porosity_gwbnd_below + &
                        (GW% Lstorage(e,t) - pSM_% ADlbound) * porosity_gwbnd
                END IF
                SW% Ldischarge(e,t) = SW% Lstorage(e,t) - SW% Lstorage(e,t-1)
                UZ% Ldischarge(e,t) = UZ% Lstorage(e,t) - UZ% Lstorage(e,t-1)

            ELSE
            !## case 2: UZ is inactive
                ! transfer excess GW storage and p to SW storage, set GW and UZ_Albound to GSL, and set UZ thickness to 0
                IF(GW% Lstorage(e,t) > UZ% top(e)) THEN
                    SW% Lstorage(e,t) = SW% Lstorage(e,t-1) + (GW% Lstorage(e,t) - UZ% top(e)) * porosity_gwbnd + P
                    GW% Lstorage(e,t) = UZ% top(e)
                ELSE IF(GW% Lstorage(e,t) == UZ% top(e)) THEN
                    SW% Lstorage(e,t) = SW% Lstorage(e,t-1) + P
                ELSE
                    CALL UZ_(e)% resolve(e, UZ, GW, time, solver_settings)
                    CALL solve(e, first_run=.TRUE.)
                END IF

                ! ET extraction from SW storage and GW storage if ET > SM storage
                IF (SW% Lstorage(e,t) > ET) THEN
                    SW% Lstorage(e,t) = SW% Lstorage(e,t) - ET
                ELSE
                    SW% Lstorage(e,t) = 0.0
                    GW% Lstorage(e,t) = GW% Lstorage(e,t) - ((ET - SW% Lstorage(e,t)) / porosity_gwbnd)
                    CALL UZ_(e)% resolve(e, UZ, GW, time, solver_settings)
                    ! CALL solve(e)
                END IF

                ! calculate discharges
                pSM_ => UZ_(e)% SM(UZ_(e)% gws_bnd_smid)
                IF(GW% Lstorage(e,t-1) < pSM_% ADubound .AND. GW% Lstorage(e,t) > pSM_% ADubound) THEN
                    GW% Ldischarge(e,t) = (GW% Lstorage(e,t) - GW% Lstorage(e,t-1)) * porosity_gwbnd
                ELSE IF(GW% Lstorage(e,t-1) > pSM_% ADubound) THEN
                ! GW fell a layer
                    GW% Ldischarge(e,t) = (pSM_% ADubound - GW% Lstorage(e,t-1)) * porosity_gwbnd_above + &
                        (GW% Lstorage(e,t) - pSM_% ADubound) * porosity_gwbnd
                ELSE
                ! GW rose a layer
                    GW% Ldischarge(e,t) = pSM_% ADlbound - GW% Lstorage(e,t-1) * porosity_gwbnd_below + &
                        (GW% Lstorage(e,t) - pSM_% ADlbound) * porosity_gwbnd
                END IF
                SW% Ldischarge(e,t) = SW% Lstorage(e,t) - SW% Lstorage(e,t-1)
                UZ% Ldischarge(e,t) = UZ% Lstorage(e,t) - UZ% Lstorage(e,t-1)

            END IF

        ELSE
        !### CHD case
            CONTINUE

        END IF

        ! fluxes are negative when mass leaves the element in GWSWEX; i.e. fluxes are negative when the storage in GWSWEX is greater than external storage
        IF(PRESENT(lateral_GW_flux)) GW% Lstorage(e,t) = GW% Lstorage(e,t) + lateral_GW_flux(time% Lts)
        IF(PRESENT(lateral_SW_flux)) SW% Lstorage(e,t) = SW% Lstorage(e,t) + lateral_SW_flux(time% Lts)

    END DO

    GW% Gstorage(e, time% Gts) = GW% Lstorage(e, time% Lnts)
    SW% Gstorage(e, time% Gts) = SW% Lstorage(e, time% Lnts)
    UZ% Gstorage(e, time% Gts) = UZ% Lstorage(e, time% Lnts)

END SUBROUTINE solve





SUBROUTINE solve_ts()

    IMPLICIT NONE
   
    ! SCHEDULE(DYNAMIC) OR SCHEDULE(GUIDED) OR SCHEDULE(DYNAMIC): http://www.inf.ufsc.br/~bosco.sobral/ensino/ine5645/OpenMP_Dynamic_Scheduling.pdf || 
    !   https://610yilingliu.github.io/2020/07/15/ScheduleinOpenMP/

    ! PRIVATE types: https://stackoverflow.com/a/15309556/19053317 || https://www.openmp.org/wp-content/uploads/OMP-Users-Monthly-Telecon-20211210.pdf ||
    ! https://fortran-lang.discourse.group/t/newbie-question-use-openmp-with-derived-type/4651/10#:~:text=I%20actually%20don%E2%80%99t,all%20the%20components.

    ! $OMP PARALLEL DO PRIVATE(e, t, l, time% scratch_dt, time% scratch_td) LASTPRIVATE(time% Lts) SHARED(GW, SW, UZ, UZ_, EXTF, solver_settings, nelements, paths, logger, &
    !   $OMP & time% Gstart, time% Gstop, time% Gts, time% Gdt, time% Gnts, time% Lstart, time% Lstop, time% Ldt, time% Lnts, time% current, time% wall_start, &
    !   $OMP & time% wall_elapsed, time% elapsed)
    DO e = 1, nelements

        CALL solve(e, first_run=.TRUE.)

    END DO
    ! $OMP END PARALLEL DO
END SUBROUTINE solve_ts





SUBROUTINE resolve_ts(GWS_ext, SWS_ext)
    REAL(REAL64), INTENT(IN), DIMENSION(nelements, time% Lnts) :: GWS_ext, SWS_ext
    REAL(REAL128), DIMENSION(time% Lnts) :: lateral_GW_flux, lateral_SW_flux
    REAL(REAL128) :: GW_residual, SW_residual
    INTEGER :: itr

    ! $OMP PARALLEL DO PRIVATE(e, t, l, time% scratch_dt, time% scratch_td, lateral_GW_flux, lateral_SW_flux, GW_residual, SW_residual, itr) LASTPRIVATE(time% Lts) &
    !   $OMP & SHARED(GW, SW, UZ, UZ_, EXTF, solver_settings, nelements, paths, logger, time% wall_elapsed, time% elapsed, GWS_ext, SWS_ext, &
    !   $OMP & time% Gstart, time% Gstop, time% Gts, time% Gdt, time% Gnts, time% Lstart, time% Lstop, time% Ldt, time% Lnts, time% current, time% wall_start)
    DO e = 1, nelements
    ! TODO: consider having CHD case here (i.e. not checking if GWSWEX GW matches GW_ext storage if CHD) instead of in solve()
        itr = 0

        ! fluxes are negative when mass leaves the element in GWSWEX; i.e. fluxes are negative when the storage in GWSWEX is greater than external storage
        lateral_GW_flux = GWS_ext(e, :) - GW% Lstorage(e, :)
        lateral_SW_flux = SWS_ext(e, :) - SW% Lstorage(e, :)

        GW_residual = GWS_ext(e, time% Lnts) - GW% Lstorage(e, time% Lnts)
        SW_residual = SWS_ext(e, time% Lnts) - SW% Lstorage(e, time% Lnts)

        DO WHILE(itr < solver_settings% max_iterations .AND. &
                (GW_residual > solver_settings% gw_tolerance .OR. SW_residual > solver_settings% sw_tolerance))

            itr = itr + 1

            CALL solve(e, .TRUE., lateral_GW_flux, lateral_SW_flux)

        END DO

    END DO
    ! $OMP END PARALLEL DO
END SUBROUTINE resolve_ts
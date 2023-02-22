SUBROUTINE solve(e, lateral_GW_flux, lateral_SW_flux)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: e

    REAL(REAL128) :: ET, P, porosity_sm1

    REAL(REAL128), INTENT(IN), DIMENSION(time% Lnts), OPTIONAL :: lateral_GW_flux, lateral_SW_flux

    ET = EXTF% et(e, time% Gts) * time% Ldt% total_seconds()
    P = EXTF% p(e, time% Gts) * time% Ldt% total_seconds()
    porosity_sm1 = UZ_(e)% SM(1)% porosity

    DO t = 2, time% Lnts
        time% Lts = t

        IF (.NOT. GW% chd(e)) THEN
        !### free GW boundary case
            CALL UZ_(e)% resolve(UZ, GW, time, e)

            IF (UZ_(e)% isactive) THEN
            !## case 1: UZ is active
                CALL UZ_(e)% solve(UZ, GW, time, e)

            ELSE
            !## case 2: UZ is inactive
                ! transfer excess GW storage and p to SW storage, set GW and UZ_Albound to GSL, and set UZ thickness to 0
                SW% Lstorage(e, time% Lts) = SW% Lstorage(e, time% Lts-1) - UZ_(e)% thickness * porosity_sm1 + P
                GW% Lstorage(e, time% Lts) = UZ_(e)% Aubound
                UZ_(e)% Albound = UZ_(e)% Aubound
                UZ_(e)% thickness = 0.0

                ! ET extraction from SW storage and GW storage if ET > SM storage
                IF (SW% Lstorage(e, time% Lts) > ET) THEN
                    SW% Lstorage(e, time% Lts) = SW% Lstorage(e, time% Lts) - ET
                ELSE
                    SW% Lstorage(e, time% Lts) = 0.0
                    GW% Lstorage(e, time% Lts) = GW% Lstorage(e, time% Lts) - ((ET - SW% Lstorage(e, time% Lts)) / porosity_sm1)
                    CALL UZ_(e)% resolve(UZ, GW, time, e)
                END IF
                ! calculate discharges
                ! TODO: modify GW discharge calculation to account for porosity from all affected layers: use gws_bnd_smid or gws_bnd flag for this
                GW% Ldischarge(e, time% Lts) = (GW% Lstorage(e, time% Lts) - GW% Lstorage(e, time% Lts-1)) * porosity_sm1
                SW% Ldischarge(e, time% Lts) = SW% Lstorage(e, time% Lts) - SW% Lstorage(e, time% Lts-1)
                UZ% Ldischarge(e, time% Lts) = UZ% Lstorage(e, time% Lts) - UZ% Lstorage(e, time% Lts-1)

            END IF

        ELSE
        !### CHD case
            CONTINUE

        END IF

        ! fluxes are negative when mass leaves the element in GWSWEX; i.e. fluxes are negative when the storage in GWSWEX is greater than external storage
        IF(PRESENT(lateral_GW_flux)) GW% Lstorage(e, time% Lts) = GW% Lstorage(e, time% Lts) + lateral_GW_flux(time% Lts)
        IF(PRESENT(lateral_SW_flux)) SW% Lstorage(e, time% Lts) = SW% Lstorage(e, time% Lts) + lateral_SW_flux(time% Lts)

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

        CALL solve(e)

    END DO
    ! $OMP END PARALLEL DO
END SUBROUTINE solve_ts





SUBROUTINE resolve(GWS_ext, SWS_ext)
    REAL(REAL64), INTENT(IN), DIMENSION(nelements, time% Lnts) :: GWS_ext, SWS_ext
    REAL(REAL128), DIMENSION(time% Lnts) :: lateral_GW_flux, lateral_SW_flux
    REAL(REAL128) :: GW_residual, SW_residual
    INTEGER :: itr

    ! $OMP PARALLEL DO PRIVATE(e, t, l, time% scratch_dt, time% scratch_td, lateral_GW_flux, lateral_SW_flux, GW_residual, SW_residual, itr) LASTPRIVATE(time% Lts) &
    !   $OMP & SHARED(GW, SW, UZ, UZ_, EXTF, solver_settings, nelements, paths, logger, time% wall_elapsed, time% elapsed, GWS_ext, SWS_ext, &
    !   $OMP & time% Gstart, time% Gstop, time% Gts, time% Gdt, time% Gnts, time% Lstart, time% Lstop, time% Ldt, time% Lnts, time% current, time% wall_start)
    DO e = 1, nelements
        itr = 0

        ! fluxes are negative when mass leaves the element in GWSWEX; i.e. fluxes are negative when the storage in GWSWEX is greater than external storage
        lateral_GW_flux = GWS_ext(e, :) - GW% Lstorage(e, :)
        lateral_SW_flux = SWS_ext(e, :) - SW% Lstorage(e, :)

        GW_residual = GWS_ext(e, time% Lnts) - GW% Lstorage(e, time% Lnts)
        SW_residual = SWS_ext(e, time% Lnts) - SW% Lstorage(e, time% Lnts)

        DO WHILE(itr < solver_settings% max_iterations .AND. &
                (GW_residual > solver_settings% GW_tolerance .OR. SW_residual > solver_settings% SW_tolerance))

            itr = itr + 1

            CALL solve(e, lateral_GW_flux, lateral_SW_flux)

        END DO

    END DO
    ! $OMP END PARALLEL DO
END SUBROUTINE resolve
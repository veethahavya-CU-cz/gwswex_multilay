SUBROUTINE solve_ts(auto_advance)

    IMPLICIT NONE

    REAL(REAL128) :: ET, P, porosity_sm1

    LOGICAL, INTENT(IN), OPTIONAL :: auto_advance
   
    ! SCHEDULE(DYNAMIC) OR SCHEDULE(GUIDED) OR SCHEDULE(DYNAMIC)
    ! http://www.inf.ufsc.br/~bosco.sobral/ensino/ine5645/OpenMP_Dynamic_Scheduling.pdf || https://610yilingliu.github.io/2020/07/15/ScheduleinOpenMP/

     ! $OMP PARALLEL DO PRIVATE(e,t)
    DO e = 1, nelements

        ET = EXTF% et(e, time% Gts) * time% Ldt% total_seconds()
        P = EXTF% p(e, time% Gts) * time% Ldt% total_seconds()
        porosity_sm1 = UZ_(e)% SM(1)% porosity

        DO t = 2, time% Lnts
            IF(PRESENT(auto_advance)) THEN
                IF(auto_advance) THEN
                    time% current = time% current + time% Ldt
                END IF
            ELSE
                time% current = time% current + time% Ldt
            END IF

            IF (.NOT. GW% chd(e)) THEN
            !### free GW boundary case
                CALL UZ_(e)% resolve(UZ, GW, time, e)

                IF (UZ_(e)% isactive) THEN
                ! case 1: UZ is active
                    CALL UZ_(e)% solve(UZ, GW, time, e)

                ELSE
                ! case 2: UZ is inactive
                    ! transfer excess GW storage and p to SW storage, set GW and UZ_Albound to GSL, and set UZ thickness to 0
                    SW% Lstorage(e, t) = SW% Lstorage(e, t-1) - UZ_(e)% thickness * porosity_sm1 + P
                    GW% Lstorage(e, t) = UZ_(e)% Aubound
                    UZ_(e)% Albound = UZ_(e)% Aubound
                    UZ_(e)% thickness = 0.0

                    ! ET extraction from SW storage and GW storage if ET > SM storage
                    IF (SW% Lstorage(e, t) > ET) THEN
                        SW% Lstorage(e, t) = SW% Lstorage(e, t) - ET
                    ELSE
                        SW% Lstorage(e, t) = 0.0
                        GW% Lstorage(e, t) = GW% Lstorage(e, t) - ((ET - SW% Lstorage(e, t)) / porosity_sm1)
                        CALL UZ_(e)% resolve(UZ, GW, time, e)
                    END IF
                    ! calculate discharges
                    ! TODO: modify GW discharge calculation to account for porosity from all affected layers: use gws_bnd_smid or gws_bnd flag for this
                    GW% Ldischarge(e, t) = (GW% Lstorage(e, t) - GW% Lstorage(e, t-1)) * porosity_sm1
                    SW% Ldischarge(e, t) = SW% Lstorage(e, t) - SW% Lstorage(e, t-1)
                    UZ% Ldischarge(e, t) = UZ% Lstorage(e, t) - UZ% Lstorage(e, t-1)

                END IF

            ELSE
            !### CHD case
                CONTINUE

            END IF
        END DO
    END DO
    ! TODO: transfer Lstorages to Gstorages at the end of timestep (for GW, SW, UZ, and all active SM layers)
    ! $OMP END PARALLEL DO
END SUBROUTINE solve_ts
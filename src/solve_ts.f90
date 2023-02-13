SUBROUTINE solve_ts()
! $OMP PARALLEL DO PRIVATE(e,t)
    ! SCHEDULE(DYNAMIC) OR SCHEDULE(GUIDED) OR SCHEDULE(DYNAMIC)
    ! http://www.inf.ufsc.br/~bosco.sobral/ensino/ine5645/OpenMP_Dynamic_Scheduling.pdf || https://610yilingliu.github.io/2020/07/15/ScheduleinOpenMP/
DO e = 1, nelements
    DO t = 2, time% Lnts
        IF (.NOT. EXTF% chd(e)) THEN
            UZ_(e)% thickness = GW% Lstorage(e,t-1)

        ELSE


        END IF
    END DO
END DO
! TODO: transfer Lstorages to Gstorages at the end of timestep (for GW, SW, UZ, and all active SM layers)
! $OMP END PARALLEL DO
END SUBROUTINE solve_dt
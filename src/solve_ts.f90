SUBROUTINE solve_dt()
DO e = 1, nelements
    DO t = 2, time% Lnts
        IF (.NOT. EXTF% chd(e)) THEN
            UZ_(e)% thickness = GW% Lstorage(e,t-1)

        ELSE


        END IF
    END DO
END DO
END SUBROUTINE solve_dt
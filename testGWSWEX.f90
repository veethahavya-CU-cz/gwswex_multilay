MODULE wrapper
    USE model , ONLY: build, init_ts, solve_e !, resolve_ts
    USE iso_c_binding

	IMPLICIT NONE

    INTEGER, PARAMETER :: STRLEN = 256

    CONTAINS
    SUBROUTINE init(config_path)

        IMPLICIT NONE

        CHARACTER(LEN=*), INTENT(IN) :: config_path
        CHARACTER(LEN=STRLEN) :: Fyaml_path

        Fyaml_path = TRIM(ADJUSTL(config_path))


        CALL build(TRIM(Fyaml_path))

    END SUBROUTINE init



    SUBROUTINE run(gw_ini, sw_ini)
    USE model, ONLY: init_ts, solve_e, time

        IMPLICIT NONE

        REAL(8), DIMENSION(:), INTENT(INOUT), OPTIONAL :: gw_ini, sw_ini

        CALL init_ts(gw_ini=gw_ini, sw_ini=sw_ini, auto_advance=.FALSE., first_run=.TRUE.)

        CALL solve_e()

        ! DO WHILE(time% Gts < 12)
        !     CALL init_ts(auto_advance=.TRUE.)
        !     CALL solve_ts()
        ! END DO

        DO WHILE(time% Gts < time% Gnts .OR. time% Gts == time% Gnts)
            CALL init_ts(auto_advance=.TRUE.)
            CALL solve_e()
        END DO

    END SUBROUTINE run


        ! SUBROUTINE resolve(GWS_ext, SWS_ext)
        !     USE model, ONLY: resolve_ts

        !     IMPLICIT NONE

        !     REAL(8), DIMENSION(:,:), INTENT(INOUT) :: GWS_ext, SWS_ext

        !     CALL resolve_ts(GWS_ext, SWS_ext)
        ! END SUBROUTINE resolve

        SUBROUTINE pass_vars(gws, sws, sms, epv)
            USE model, ONLY: GW, SW, UZ

            IMPLICIT NONE

            REAL(8), DIMENSION(:,:), INTENT(INOUT) :: gws, sws, sms, epv

            gws = GW% Gstorage
            sws = SW% Gstorage
            sms = UZ% Gstorage
            epv = UZ% Gepv
        END SUBROUTINE pass_vars

END MODULE wrapper





PROGRAM GWSWEX
	USE wrapper, ONLY: init, run
    USE iso_c_binding

	IMPLICIT NONE

    CALL init('/home/gwswex_dev/gwswex_multilay/test.yml')

    CALL run()

END PROGRAM GWSWEX
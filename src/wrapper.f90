MODULE GWSWEX
	USE model , ONLY: build, init_ts, solve_e !, resolve_ts

	IMPLICIT NONE

    INTEGER, PARAMETER :: STRLEN = 256

    CONTAINS
    
    SUBROUTINE init(config_path, gw_ini, sw_ini)

        IMPLICIT NONE

        CHARACTER(LEN=*), INTENT(IN) :: config_path
        REAL(8), DIMENSION(:), INTENT(IN) :: gw_ini, sw_ini

        CHARACTER(LEN=STRLEN) :: Fyaml_path

        Fyaml_path = TRIM(ADJUSTL(config_path))


        CALL build(TRIM(Fyaml_path))

        CALL init_ts(gw_ini=gw_ini, sw_ini=sw_ini, auto_advance=.FALSE., first_run=.TRUE.)

        CALL solve_e() !#FIXME: DO NOT SOLVE HERE, JUST INITIALIZE

    END SUBROUTINE init

! #TODO: add a function to get nelements, tstart, and tstop


    FUNCTION get_Lnts() RESULT(Lnts)
        USE model, ONLY: time

        IMPLICIT NONE

        INTEGER :: Lnts

        Lnts = time% Lnts
    END FUNCTION get_Lnts


    FUNCTION get_Gts() RESULT(Gts)
        USE model, ONLY: time

        IMPLICIT NONE

        INTEGER :: Gts

        Gts = time% Gts
    END FUNCTION get_Gts


    FUNCTION get_curr_time_unix() RESULT(unix_time)
        USE model, ONLY: time

        IMPLICIT NONE
! #FIXME: report time in seconds since start of simulation like mf6 and dfm
        INTEGER :: unix_time

        unix_time = time% current% secondsSinceEpoch()
    END FUNCTION get_curr_time_unix



    SUBROUTINE grab_result(code, result)
        USE model, ONLY: GW, SW, UZ, UZ_

        IMPLICIT NONE

        CHARACTER(LEN=*), INTENT(IN) :: code
        REAL(8), DIMENSION(:,:), INTENT(INOUT) :: result

        SELECT CASE(code)
            CASE('gws_l')
                result = GW % Lstorage(:,2:)
            CASE('gws_g')
                result = GW % Gstorage(:,2:)
            CASE('gw_dis_l')
                result = GW % Ldischarge(:,2:)
            CASE('gw_dis_g')
                result = GW % Gdischarge(:,2:)
            CASE('sws_l')
                result = SW % Lstorage(:,2:)
            CASE('sws_g')
                result = SW % Gstorage(:,2:)
            CASE('sw_dis_l')
                result = SW % Ldischarge(:,2:)
            CASE('sw_dis_g')
                result = SW % Gdischarge(:,2:)
            CASE('uzs_l')
                result = UZ % Lstorage(:,2:)
            CASE('uzs_g')
                result = UZ % Gstorage(:,2:)
            CASE('uz_dis_l')
                result = UZ % Ldischarge(:,2:)
            CASE('uz_dis_g')
                result = UZ % Gdischarge(:,2:)
            CASE('epv_l')
                result = UZ % Lepv(:,2:)
            CASE('epv_g')
                result = UZ % Gepv(:,2:)
            CASE DEFAULT
                WRITE(*,*) 'UNKNOWN CODE: ', code
                result(:,:) = 0.0
        END SELECT
    END SUBROUTINE grab_result




    SUBROUTINE update(auto_advance)
        USE model, ONLY: init_ts, solve_e

        IMPLICIT NONE

        LOGICAL(1), INTENT(IN) :: auto_advance

        CALL init_ts(auto_advance=LOGICAL(auto_advance, KIND=4))
        CALL solve_e()
    END SUBROUTINE update



    SUBROUTINE update_ini(auto_advance, gw_ini, sw_ini)
        USE model, ONLY: init_ts, solve_e

        IMPLICIT NONE

        LOGICAL(1), INTENT(IN) :: auto_advance
        REAL(8), DIMENSION(:), INTENT(IN), OPTIONAL :: gw_ini, sw_ini

        CALL init_ts(gw_ini=gw_ini, sw_ini=sw_ini, auto_advance=LOGICAL(auto_advance, KIND=4))
        CALL solve_e()
    END SUBROUTINE update_ini



    SUBROUTINE run()
    USE model, ONLY: init_ts, solve_e, time

        IMPLICIT NONE

        DO WHILE(time% Gts < time% Gnts .OR. time% Gts == time% Gnts)
            CALL init_ts(auto_advance=.TRUE.)
            CALL solve_e()
        END DO

    END SUBROUTINE run



    SUBROUTINE resolve(GWS_ext, SWS_ext)
        USE model, ONLY: resolve_l

        IMPLICIT NONE

        REAL(8), DIMENSION(:,:), INTENT(IN) :: GWS_ext, SWS_ext

        CALL resolve_l(GWS_ext, SWS_ext)
    END SUBROUTINE resolve



    SUBROUTINE pass_vars(gws, sws, sms, epv)
        USE model, ONLY: GW, SW, UZ

        IMPLICIT NONE

        REAL(8), DIMENSION(:,:), INTENT(INOUT) :: gws, sws, sms, epv

        gws = GW% Gstorage
        sws = SW% Gstorage
        sms = UZ% Gstorage
        epv = UZ% Gepv
    END SUBROUTINE pass_vars

    SUBROUTINE pass_vars_nlay(gws, sws, sms, epv)
        USE model, ONLY: GW, SW, UZ, UZ_, nelements

        IMPLICIT NONE

        REAL(8), DIMENSION(:,:), INTENT(INOUT) :: gws, sws
        REAL(8), DIMENSION(:,:,:), INTENT(INOUT) :: sms, epv

        INTEGER :: l, e

        gws = GW% Gstorage
        sws = SW% Gstorage

        DO e = 1, nelements
            DO l = 1, UZ% nlay
                sms(l,e,:) = UZ_(e)% SM(l)% Gstorage(:)
            END DO
        END DO
        epv = REAL(UZ% Gepvnl, 8)
        ! sms = UZ% Gstorage
        ! epv = UZ% Gepv
    END SUBROUTINE pass_vars_nlay



    SUBROUTINE pass_dis(gw_dis, uz_dis, sw_dis, qdiff_l) !, qin_l, qout_l
        USE model, ONLY: GW, SW, UZ, Qdiff, Qin, Qout

        IMPLICIT NONE

        REAL(8), DIMENSION(:,:), INTENT(INOUT) :: gw_dis, uz_dis, sw_dis, qdiff_l
        ! REAL(8), DIMENSION(:,:), INTENT(INOUT), OPTIONAL :: qin_l, qout_l

        gw_dis = GW% Gdischarge
        uz_dis = UZ% Gdischarge
        sw_dis = SW% Gdischarge
        qdiff_l = Qin - Qout

        ! IF (PRESENT(qin_l)) qin_l = Qin
        ! IF (PRESENT(qout_l)) qout_l = Qout

    END SUBROUTINE pass_dis



    SUBROUTINE finalize()
        USE model, ONLY: fin

        CALL fin()

    END SUBROUTINE finalize


END MODULE GWSWEX
MODULE GWSWEX
	USE model , ONLY: build, init_ts, solve_ts, resolve_ts
    USE iso_c_binding
    
    ! TODO: WHILE(time% Gts < time% Gnts) run GWSWEX

	IMPLICIT NONE

    CONTAINS


        SUBROUTINE wrap_init(Fyaml_path_c) BIND(C, name='initialize')
            USE model, ONLY: build
            USE iso_c_binding, only: c_char

            IMPLICIT NONE

            CHARACTER(LEN=1, KIND=c_char), INTENT(IN), TARGET :: Fyaml_path_c(256)
            CHARACTER, DIMENSION(256) :: Fyaml_path_f
            CHARACTER(256), POINTER :: Fyaml_path

            INTEGER :: i

            ! Fyaml_path => Fyaml_path_c(1)
            ALLOCATE(Fyaml_path)
            Fyaml_path_f = Fyaml_path_c
            ! WRITE(Fyaml_path, *) Fyaml_path_f

            DO i = 1, 256
                Fyaml_path(i:i) = Fyaml_path_f(i)
            END DO

            ! write(*,*) "Fyaml_path: ", Fyaml_path
            ! WRITE(*,*) "Fyaml_path: ", TRIM(Fyaml_path)
            ! WRITE(*,*) "Fyaml_path_f: ", Fyaml_path_f
            ! WRITE(*,*) "Fyaml_path_c: ", Fyaml_path_c

            CALL build(TRIM(Fyaml_path))
            
        END SUBROUTINE wrap_init


        SUBROUTINE wrap_run(gw_ini_c, sw_ini_c) BIND(C, name='solve')

            USE model, ONLY: init_ts, solve_ts
            USE iso_c_binding, only: c_double
            USE iso_fortran_env, only: REAL64

            IMPLICIT NONE

            !custom type usage (!but not def) possible here
            !TYPE(Clogger), DIMENSION(7) :: logger_array
            REAL(c_double), DIMENSION(:), INTENT(INOUT) :: gw_ini_c, sw_ini_c
            REAL(REAL64), DIMENSION(:), ALLOCATABLE :: gw_ini, sw_ini

            ALLOCATE(gw_ini(SIZE(gw_ini_c)), sw_ini(SIZE(sw_ini_c)))
            gw_ini = gw_ini_c
            sw_ini = sw_ini_c

            CALL init_ts(gw_ini, sw_ini, auto_advance=.FALSE.)

            CALL solve_ts()
        END SUBROUTINE wrap_run


        SUBROUTINE wrap_resolve(GWS_ext, SWS_ext) BIND(C, name='resolve')
            USE model, ONLY: resolve_ts
            IMPLICIT NONE
            !custom type usage (!but not def) possible here
            !TYPE(Clogger), DIMENSION(7) :: logger_array
            REAL(8), DIMENSION(:,:), INTENT(INOUT) :: GWS_ext, SWS_ext

            CALL resolve_ts(GWS_ext, SWS_ext)
        END SUBROUTINE wrap_resolve

END MODULE GWSWEX
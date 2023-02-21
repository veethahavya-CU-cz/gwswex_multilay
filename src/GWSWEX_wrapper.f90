MODULE GWSWEX
	USE GWSWEX_model, only: build, init_ts, solve_ts
    
    ! TODO: WHILE(time% Gts < time% Gnts) run GWSWEX

	IMPLICIT NONE

    CONTAINS
        SUBROUTINE initialize(fyaml_path, gok_l, bot_l, n_l, k_l, macropore_inf_degree_l, vanG_pars_l, chd_l, p_l, et_l)
            !USE iso_c_binding, only: c_char

            IMPLICIT NONE
            !CHARACTER(kind=c_char), DIMENSION(*), INTENT(IN) :: fyaml_path
            CHARACTER(*), INTENT(IN) :: fyaml_path
            LOGICAL, INTENT(IN)  :: chd_l(:)
	        REAL(8), INTENT(IN) :: gok_l(:), bot_l(:), n_l(:), k_l(:), macropore_inf_degree_l(:), vanG_pars_l(4), p_l(:,:), et_l(:,:)

            CALL build(fyaml_path, gok_l, bot_l, n_l, k_l, macropore_inf_degree_l)
            CALL init(chd_l, p_l, et_l)
        END SUBROUTINE initialize


        SUBROUTINE finalize(gws_l, sws_l, sm_l, epv_l, gw_sm_interconnectivity_l)
            IMPLICIT NONE
            !custom type usage (!but not def) possible here
            !TYPE(Clogger), DIMENSION(7) :: logger_array
            REAL(8), INTENT(INOUT) :: gws_l(:,:), sws_l(:,:), sm_l(:,:), epv_l(:,:), gw_sm_interconnectivity_l(:)

            CALL run(gws_l, sws_l, sm_l, epv_l, gw_sm_interconnectivity_l(:))
        END SUBROUTINE finalize

        SUBROUTINE fetch_1d(attr, res)
            USE GWSWEX_model, only: gw_sm_interconnectivity

            IMPLICIT NONE
            CHARACTER(*), INTENT(IN) :: attr
            REAL(8), INTENT(INOUT) :: res(:)
            
            SELECT CASE(attr)
                CASE('gw_sm_interconnectivity')
                    res = gw_sm_interconnectivity
                CASE DEFAULT
                    ERROR STOP 'unknown attribute'
            END SELECT
        END SUBROUTINE fetch_1d

        SUBROUTINE fetch_2d(attr, res)
            USE GWSWEX_model, only: gw_dis, sw_dis, sm_dis, Qin, Qout, Qdiff
            IMPLICIT NONE
            CHARACTER(*), INTENT(IN) :: attr
            REAL(8), INTENT(INOUT) :: res(:,:)
            
            SELECT CASE(attr)
                CASE('gw_dis')
                    res = gw_dis
                CASE('sw_dis')
                    res = sw_dis
                CASE('sm_dis')
                    res = sm_dis
                CASE('Qin')
                    res = Qin
                CASE('Qout')
                    res = Qout
                CASE('Qdiff')
                    res = Qdiff
                CASE DEFAULT
                    ERROR STOP 'ERROR: unknown attribute'
            END SELECT

        END SUBROUTINE fetch_2d
END MODULE GWSWEX
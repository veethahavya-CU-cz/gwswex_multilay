MODULE Msolver
    USE iso_fortran_env, ONLY: REAL32, REAL64, REAL128, INT8, INT16, INT32, INT64

    TYPE Csettings
        INTEGER(INT16) :: max_iterations
        REAL(REAL64) :: gw_tolerance, sw_tolerance, sm_gw_fluctuation_tolerance
        REAL(REAL64), DIMENSION(:), ALLOCATABLE :: pet_intensities
        INTEGER(INT8), DIMENSION(:), ALLOCATABLE :: pet_nts
        LOGICAL :: stabalize_sm_gw
    END TYPE Csettings
END MODULE Msolver

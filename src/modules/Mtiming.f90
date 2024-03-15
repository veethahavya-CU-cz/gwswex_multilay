MODULE Mtiming
    ! handles model and clock timing operations and manipulations
       USE iso_fortran_env, ONLY: REAL32, REAL64, REAL128, INT8, INT16, INT32, INT64
       USE datetime_module, ONLY: datetime, timedelta
    
       IMPLICIT NONE
    
       TYPE Ctime
          ! to store and access temporal information of the model
          TYPE(datetime) :: Gstart, Gstop, Lstart, Lstop, current, wall_start, scratch_dt
          TYPE(timedelta) :: Gdt, Ldt, elapsed, wall_elapsed, scratch_td
          INTEGER(INT32) :: Gnts, Gts, Lnts
          INTEGER(INT32), POINTER :: Lts
       END TYPE Ctime
    
       ! # ADD: a pass procedure to move a ts ahead
    
END MODULE Mtiming
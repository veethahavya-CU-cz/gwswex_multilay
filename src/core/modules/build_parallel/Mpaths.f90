MODULE Mpaths
    ! to store and access model file/dir paths
       ! root: 		absolute path to the root directory of the model
       ! input: 		absolute path to the input directory of the model
       ! output: 		absolute path to the output directory of the model
       USE iso_fortran_env, ONLY: REAL32, REAL64, REAL128, INT8, INT16, INT32, INT64
    
       IMPLICIT NONE
    
       INTEGER, PARAMETER  :: STRLEN=256
    
       TYPE Cpaths
          CHARACTER(LEN=STRLEN) :: root, input, output, config
       END TYPE Cpaths
    
END MODULE Mpaths
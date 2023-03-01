! This file is part of the logger Module - Mlogger. It defines the procedures contained within the module.

SUBROUTINE init(self)
! initializes the logger object and opens the log file
    CLASS(Clogger), INTENT(INOUT) :: self

    self% debug = 3
    self% trace = 2
    self% moreinfo = 1
	self% info = 0
	self% warn = -1
	self% error = -2
	self% fatal = -3

    OPEN(UNIT=self% unit, FILE=self% fpath, STATUS='REPLACE', ACTION='WRITE')
END SUBROUTINE init


FUNCTION lv_name(lv)
! returns the name of the log level
    INTEGER(INT8), INTENT(IN) :: lv
    CHARACTER(len=10) :: lv_name

    SELECT CASE(lv)
    CASE(3)
        lv_name = 'DEBUG'    
    CASE(2)
        lv_name = 'TRACE'
    CASE(1)
        lv_name = 'INFO'
    CASE(0)
        lv_name = 'INFO'
    CASE(-1)
        lv_name = 'WARN'
    CASE(-2)
        lv_name = 'ERROR'
    CASE(-3)
        lv_name = 'FATAL'
    END SELECT 
END FUNCTION lv_name


SUBROUTINE log_real(self, lv, msg, val, addnl_val)
! logs a real value
    CLASS(Clogger), INTENT(INOUT) :: self
    INTEGER(INT8), INTENT(IN) :: lv
    CHARACTER(len=*), INTENT(IN) :: msg
    CHARACTER(len=32) :: buffer
    REAL(REAL64), INTENT(IN) :: val
    REAL(REAL64), OPTIONAL :: addnl_val

    self% timer = self% timer% now()
    buffer = "["//TRIM(lv_name(lv))//"] " // "["//TRIM(self% timer% strftime("%H:%M:%S"))//"]"

    IF (lv < self% level .OR. lv == self% level) THEN
        IF (PRESENT(addnl_val)) THEN
            WRITE(self% unit,*) buffer, ": ", msg, val, addnl_val
        ELSE
            WRITE(self% unit,*) buffer, ": ", msg, val
        ENDIF
    END IF
END SUBROUTINE log_real


SUBROUTINE log_int(self, lv, msg, val, addnl_val)
! logs an integer value
    CLASS(Clogger), INTENT(INOUT) :: self
    INTEGER(INT8), INTENT(IN) :: lv
    CHARACTER(len=*), INTENT(IN) :: msg
    CHARACTER(len=32) :: buffer
    INTEGER, INTENT(IN) :: val
    INTEGER, OPTIONAL :: addnl_val

    self% timer = self% timer% now()
    buffer = "["//TRIM(lv_name(lv))//"] " // "["//TRIM(self% timer% strftime("%H:%M:%S"))//"]"

    IF (lv < self% level .OR. lv == self% level) THEN
        IF (PRESENT(addnl_val)) THEN
            WRITE(self% unit,*) buffer, ": ", msg, val, addnl_val
        ELSE
            WRITE(self% unit,*) buffer, ": ", msg, val
        ENDIF
    END IF
END SUBROUTINE log_int


SUBROUTINE log_str(self, lv, msg)
! logs a string
    CLASS(Clogger), INTENT(INOUT) :: self
    INTEGER(INT8), INTENT(IN) :: lv
    CHARACTER(len=*), INTENT(IN) :: msg
    CHARACTER(len=32) :: buffer

    self% timer = self% timer% now()
    buffer = "["//TRIM(lv_name(lv))//"] " // "["//TRIM(self% timer% strftime("%H:%M:%S"))//"]"

    IF (lv < self% level .OR. lv == self% level) THEN
        WRITE(self% unit,*) buffer, ": ", msg
    END IF
END SUBROUTINE log_str
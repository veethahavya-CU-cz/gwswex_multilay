! #TODO: nest all logger% debug called within an ifdef
MODULE Mlogger
! handles logging operations
    USE iso_fortran_env, ONLY: REAL32, REAL64, REAL128, INT8, INT16, INT32, INT64
    USE datetime_module, ONLY: datetime

    IMPLICIT NONE

    INTEGER, PARAMETER  :: STRLEN = 256

    TYPE Clogger
        ! to store and access logging information of the model
        INTEGER(INT8) :: unit, level, info, moreinfo, trace, debug, warn, error, fatal
        CHARACTER(LEN=STRLEN) :: fpath, fname
        TYPE(datetime) :: timer
    CONTAINS
        PROCEDURE :: init
        PROCEDURE :: log_real32
        PROCEDURE :: log_real64
        PROCEDURE :: log_real128
        PROCEDURE :: log_int
        PROCEDURE :: log_int8
        PROCEDURE :: log_int_middle
        PROCEDURE :: log_int8_middle
        PROCEDURE :: log_str
        GENERIC :: log => log_real32, log_real64, log_real128, log_int, log_int8, log_int_middle, log_int8_middle, log_str
    END TYPE Clogger

CONTAINS
    ! This file is part of the logger Module - Mlogger. It defines the procedures contained within the module.

    SUBROUTINE init(self)
        ! initializes the logger object and opens the log file
        CLASS(Clogger), INTENT(INOUT) :: self

        self%debug = 3
        self%trace = 2
        self%moreinfo = 1
        self%info = 0
        self%warn = -1
        self%error = -2
        self%fatal = -3

        OPEN (UNIT=self%unit, FILE=self%fpath, STATUS='REPLACE', ACTION='WRITE')
    END SUBROUTINE init

    FUNCTION lv_name(lv)
        ! returns the name of the log level
        INTEGER(INT8), INTENT(IN) :: lv
        CHARACTER(len=10) :: lv_name

        SELECT CASE (lv)
        CASE (3)
            lv_name = 'DEBUG'
        CASE (2)
            lv_name = 'TRACE'
        CASE (1)
            lv_name = 'INFO'
        CASE (0)
            lv_name = 'INFO'
        CASE (-1)
            lv_name = 'WARN'
        CASE (-2)
            lv_name = 'ERROR'
        CASE (-3)
            lv_name = 'FATAL'
        END SELECT
    END FUNCTION lv_name

    SUBROUTINE log_real32(self, lv, msg, val, addnl_val)
        ! logs a real value
        USE iso_fortran_env, ONLY: REAL32

        CLASS(Clogger), INTENT(INOUT) :: self
        INTEGER(INT8), INTENT(IN) :: lv
        CHARACTER(len=*), INTENT(IN) :: msg
        CHARACTER(len=32) :: buffer
        REAL(REAL32), INTENT(IN) :: val
        REAL(REAL32), OPTIONAL :: addnl_val

        IF (lv < self%level .OR. lv == self%level) THEN
            self%timer = self%timer%now()
            buffer = "["//TRIM(lv_name(lv))//"] "//"["//TRIM(ADJUSTL(self%timer%strftime("%H:%M:%S")))//"]"
            IF (PRESENT(addnl_val)) THEN
                WRITE (self%unit, *) TRIM(ADJUSTL(buffer)), ": ", msg, val, addnl_val
            ELSE
                WRITE (self%unit, *) TRIM(ADJUSTL(buffer)), ": ", msg, val
            END IF
        END IF
    END SUBROUTINE log_real32

    SUBROUTINE log_real64(self, lv, msg, val, addnl_val)
        ! logs a real value
        USE iso_fortran_env, ONLY: REAL64

        CLASS(Clogger), INTENT(INOUT) :: self
        INTEGER(INT8), INTENT(IN) :: lv
        CHARACTER(len=*), INTENT(IN) :: msg
        CHARACTER(len=32) :: buffer
        REAL(REAL64), INTENT(IN) :: val
        REAL(REAL64), OPTIONAL :: addnl_val

        IF (lv < self%level .OR. lv == self%level) THEN
            self%timer = self%timer%now()
            buffer = "["//TRIM(lv_name(lv))//"] "//"["//TRIM(ADJUSTL(self%timer%strftime("%H:%M:%S")))//"]"
            IF (PRESENT(addnl_val)) THEN
                WRITE (self%unit, *) TRIM(ADJUSTL(buffer)), ": ", msg, val, addnl_val
            ELSE
                WRITE (self%unit, *) TRIM(ADJUSTL(buffer)), ": ", msg, val
            END IF
        END IF
    END SUBROUTINE log_real64

    SUBROUTINE log_real128(self, lv, msg, val, addnl_val1, addnl_val2, addnl_val3)
        ! logs a real value
        USE iso_fortran_env, ONLY: REAL128

        CLASS(Clogger), INTENT(INOUT) :: self
        INTEGER(INT8), INTENT(IN) :: lv
        CHARACTER(len=*), INTENT(IN) :: msg
        CHARACTER(len=32) :: buffer
        REAL(REAL128), INTENT(IN) :: val
        REAL(REAL128), OPTIONAL :: addnl_val1, addnl_val2, addnl_val3

        IF (lv < self%level .OR. lv == self%level) THEN
            self%timer = self%timer%now()
            buffer = "["//TRIM(lv_name(lv))//"] "//"["//TRIM(ADJUSTL(self%timer%strftime("%H:%M:%S")))//"]"
            IF (PRESENT(addnl_val1)) THEN
                IF (PRESENT(addnl_val2)) THEN
                    IF (PRESENT(addnl_val3)) THEN
                        WRITE (self%unit, *) TRIM(ADJUSTL(buffer)), ": ", msg, val, addnl_val1, addnl_val2, addnl_val3
                    ELSE
                        WRITE (self%unit, *) TRIM(ADJUSTL(buffer)), ": ", msg, val, addnl_val1, addnl_val2
                    END IF
                ELSE
                    WRITE (self%unit, *) TRIM(ADJUSTL(buffer)), ": ", msg, val, addnl_val1
                END IF
            ELSE
                WRITE (self%unit, *) TRIM(ADJUSTL(buffer)), ": ", msg, val
            END IF
        END IF
    END SUBROUTINE log_real128

    SUBROUTINE log_int(self, lv, msg, val, addnl_val)
        ! logs an integer value
        CLASS(Clogger), INTENT(INOUT) :: self
        INTEGER(INT8), INTENT(IN) :: lv
        CHARACTER(len=*), INTENT(IN) :: msg
        CHARACTER(len=32) :: buffer
        INTEGER, INTENT(IN) :: val
        INTEGER, OPTIONAL :: addnl_val

        IF (lv < self%level .OR. lv == self%level) THEN
            self%timer = self%timer%now()
            buffer = "["//TRIM(lv_name(lv))//"] "//"["//TRIM(ADJUSTL(self%timer%strftime("%H:%M:%S")))//"]"
            IF (PRESENT(addnl_val)) THEN
                WRITE (self%unit, *) TRIM(ADJUSTL(buffer)), ": ", msg, val, addnl_val
            ELSE
                WRITE (self%unit, *) TRIM(ADJUSTL(buffer)), ": ", msg, val
            END IF
        END IF
    END SUBROUTINE log_int

    SUBROUTINE log_int8(self, lv, msg, val, addnl_val)
        ! logs an integer value
        USE iso_fortran_env, ONLY: INT8

        CLASS(Clogger), INTENT(INOUT) :: self
        INTEGER(INT8), INTENT(IN) :: lv
        CHARACTER(len=*), INTENT(IN) :: msg
        CHARACTER(len=32) :: buffer
        INTEGER(INT8), INTENT(IN) :: val
        INTEGER(INT8), OPTIONAL :: addnl_val

        IF (lv < self%level .OR. lv == self%level) THEN
            self%timer = self%timer%now()
            buffer = "["//TRIM(lv_name(lv))//"] "//"["//TRIM(ADJUSTL(self%timer%strftime("%H:%M:%S")))//"]"
            IF (PRESENT(addnl_val)) THEN
                WRITE (self%unit, *) TRIM(ADJUSTL(buffer)), ": ", msg, val, addnl_val
            ELSE
                WRITE (self%unit, *) TRIM(ADJUSTL(buffer)), ": ", msg, val
            END IF
        END IF
    END SUBROUTINE log_int8

    SUBROUTINE log_int_middle(self, lv, msg1, val, msg2)
        ! logs an integer value
        CLASS(Clogger), INTENT(INOUT) :: self
        INTEGER(INT8), INTENT(IN) :: lv
        CHARACTER(len=*), INTENT(IN) :: msg1, msg2
        CHARACTER(len=32) :: buffer
        INTEGER, INTENT(IN) :: val

        IF (lv < self%level .OR. lv == self%level) THEN
            self%timer = self%timer%now()
            buffer = "["//TRIM(lv_name(lv))//"] "//"["//TRIM(ADJUSTL(self%timer%strftime("%H:%M:%S")))//"]"
            WRITE (self%unit, *) TRIM(ADJUSTL(buffer)), ": ", msg1, val, msg2
        END IF
    END SUBROUTINE log_int_middle

    SUBROUTINE log_int8_middle(self, lv, msg1, val, msg2)
        ! logs an integer value
        USE iso_fortran_env, ONLY: INT8

        CLASS(Clogger), INTENT(INOUT) :: self
        INTEGER(INT8), INTENT(IN) :: lv
        CHARACTER(len=*), INTENT(IN) :: msg1, msg2
        CHARACTER(len=32) :: buffer
        INTEGER(INT8), INTENT(IN) :: val

        IF (lv < self%level .OR. lv == self%level) THEN
            self%timer = self%timer%now()
            buffer = "["//TRIM(lv_name(lv))//"] "//"["//TRIM(ADJUSTL(self%timer%strftime("%H:%M:%S")))//"]"
            WRITE (self%unit, *) TRIM(ADJUSTL(buffer)), ": ", msg1, val, msg2
        END IF
    END SUBROUTINE log_int8_middle

    SUBROUTINE log_str(self, lv, msg)
        ! logs a string
        CLASS(Clogger), INTENT(INOUT) :: self
        INTEGER(INT8), INTENT(IN) :: lv
        CHARACTER(len=*), INTENT(IN) :: msg
        CHARACTER(len=32) :: buffer

        IF (lv < self%level .OR. lv == self%level) THEN
            self%timer = self%timer%now()
            buffer = "["//TRIM(lv_name(lv))//"] "//"["//TRIM(ADJUSTL(self%timer%strftime("%H:%M:%S")))//"]"
            WRITE (self%unit, *) TRIM(ADJUSTL(buffer)), ": ", msg
        END IF
    END SUBROUTINE log_str

END MODULE Mlogger

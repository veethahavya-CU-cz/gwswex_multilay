SUBROUTINE init_ts(dt)
!TODO: take Lnts, p, and et as args instead
	USE datetime_module, ONLY: timedelta, datetime

	IMPLICIT NONE

	INTEGER(INT16) :: dt
	CHARACTER(LEN=256) :: strbuffer

	time% Ldt = timedelta(seconds = dt)

	IF (time% Ldt > (time% Lstop - time% Lstart)) THEN
		WRITE(strbuffer, *) "The simulation period is too short for the given time step size. Resetting the time step size to local simulation period length."
		CALL logger% log(logger% warn, TRIM(strbuffer))
		time% Ldt = time% Lstop - time% Lstart
		time% Lnts = 1
	END IF

	time% scratch_td = time% Lstop - time% Lstart
	time% Lnts = INT(time% scratch_td% total_seconds() / time% Ldt% total_seconds(), KIND=INT32)
	time% Ldt = timedelta(seconds = (time% scratch_td% total_seconds() / time% Lnts))

	WRITE(strbuffer, *) "Initializing local simulation period with ", time% Lnts, " time steps."
	CALL logger% log(logger% INFO, TRIM(strbuffer))

	IF (time% Ldt% total_seconds() /= dt) THEN
		WRITE(strbuffer, *) "Time step size has been adjusted to ", time% Ldt% total_seconds(), "s from the original value of ", dt, "s."
		CALL logger% log(logger% INFO, TRIM(strbuffer))
	END IF

	ALLOCATE(GW% Lstorage(nelements, time% Lnts+1), UZ% Lstorage(nelements, time% Lnts+1), UZ% Lepv(nelements, time% Lnts+1), SW% Lstorage(nelements, time% Lnts+1))
	ALLOCATE(GW% discharge(nelements, time% Lnts+1), UZ% discharge(nelements, time% Lnts+1), SW% discharge(nelements, time% Lnts+1))
	DO e = 1, nelements
		DO l = 1, UZ_(e)% nlay
			IF (UZ_(e)% SM(l)% isactive) THEN
				ALLOCATE(UZ_(e)% SM(l)% Lstorage(time% Lnts+1))
				! ALLOCATE(UZ_(e)% SM(l)% discharge(time% Lnts+1))
			END IF
		END DO
	END DO

	! set local storage to the global storage of last dt (or initial conditions for first dt)
	GW% Lstorage(:, 1) = GW% Gstorage(:, time% Gts)
	UZ% Lstorage(:, 1) = UZ% Gstorage(:, time% Gts)
	SW% Lstorage(:, 1) = SW% Gstorage(:, time% Gts)
	
	FLUSH(logger% unit)
END SUBROUTINE
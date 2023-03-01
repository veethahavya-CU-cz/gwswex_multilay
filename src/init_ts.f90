SUBROUTINE init_ts(gw_ini, sw_ini, auto_advance)
	USE datetime_module, ONLY: timedelta, datetime

	IMPLICIT NONE

	LOGICAL, INTENT(IN), OPTIONAL :: auto_advance ! auto advances by default

	INTEGER(INT16) :: idx
	CHARACTER(LEN=256) :: strbuffer

	REAL(REAL64), INTENT(INOUT), DIMENSION(nelements) :: gw_ini, sw_ini
	REAL(REAL32) :: ipet_ll, ipet_ul, pet_intensity

	! advance the global timestep and deallocate the local storages, epvs, and discharges from the previous timestep 
	IF(PRESENT(auto_advance)) THEN
		IF(auto_advance) THEN
			time% Gts = time% Gts + 1
			DEALLOCATE(GW% Lstorage, UZ% Lstorage, UZ% Lepv, SW% Lstorage, GW% Ldischarge, UZ% Ldischarge, SW% Ldischarge)
			! $OMP PARALLEL DO
			DO e = 1, nelements
				DO l = 1, UZ_(e)% nlay
					IF (UZ_(e)% SM(l)% isactive) THEN
						DEALLOCATE(UZ_(e)% SM(l)% Lstorage)
						DEALLOCATE(UZ_(e)% SM(l)% Lepv)
						! DEALLOCATE(UZ_(e)% SM(l)% Ldischarge)
					END IF
				END DO
			END DO
			! $OMP END PARALLEL DO
		END IF
	ELSE
		time% Gts = time% Gts + 1
		DEALLOCATE(GW% Lstorage, UZ% Lstorage, UZ% Lepv, SW% Lstorage, GW% Ldischarge, UZ% Ldischarge, SW% Ldischarge)
		! $OMP PARALLEL DO
		DO e = 1, nelements
			DO l = 1, UZ_(e)% nlay
				IF (UZ_(e)% SM(l)% isactive) THEN
					DEALLOCATE(UZ_(e)% SM(l)% Lstorage)
					DEALLOCATE(UZ_(e)% SM(l)% Lepv)
					! DEALLOCATE(UZ_(e)% SM(l)% Ldischarge)
				END IF
			END DO
		END DO
		! $OMP END PARALLEL DO
	END IF

	time% Lstart = time% Gstart + timedelta(seconds = time% Gdt% total_seconds() * time% Gnts-1)
	time% current = time% Lstart
	time% elapsed = time% current - time% Gstart
	time% wall_elapsed = time% wall_start% now() - time% wall_start
	time% Lstop = time% Lstart + time% Gdt
	

	CALL logger% log(logger% INFO, "Local simulation period starts at " // TRIM(time% Lstart% strftime("%Y-%m-%d %H:%M:%S")))


	! calculate the average PET intensity over the spatial domain for this Gts
	pet_intensity = (SUM(ABS(EXTF% p(:, time% Gts))) + SUM(ABS(EXTF% et(:, time% Gts)))) / nelements
write(*,*) pet_intensity
	! set the local timestep size based on the average PET intensity
	ipet_ll = 0.0
	DO idx = 1, SIZE(solver_settings% pet_intensities)
		ipet_ul = solver_settings% pet_intensities(idx)
		IF (pet_intensity >= ipet_ll .AND. pet_intensity < ipet_ul) THEN
			time% Lnts = solver_settings% pet_nts(idx)
			EXIT
		ELSE
			ipet_ll = ipet_ul
		END IF
	END DO

	time% Ldt = timedelta(seconds = (time% scratch_td% total_seconds() / time% Lnts))

	IF (time% Ldt > (time% Lstop - time% Lstart)) THEN
		WRITE(strbuffer, *) "The local simulation period is too short for the given time step size. Resetting the time step size to local simulation period length."
		CALL logger% log(logger% warn, TRIM(strbuffer))
		time% Ldt = time% Lstop - time% Lstart
		time% Lnts = 1
	END IF

	WRITE(strbuffer, *) "Initializing local simulation period with a timestep of ", time% Ldt% total_seconds(), " s."
	CALL logger% log(logger% INFO, strbuffer)

	IF((.NOT. PRESENT(auto_advance)) .OR. (.NOT. auto_advance))	ALLOCATE(GW% Lstorage(nelements, time% Lnts+1), &
		UZ% Lstorage(nelements, time% Lnts+1), UZ% Lepv(nelements, time% Lnts+1), SW% Lstorage(nelements, time% Lnts+1), &
		GW% Ldischarge(nelements, time% Lnts+1), UZ% Ldischarge(nelements, time% Lnts+1), SW% Ldischarge(nelements, time% Lnts+1))

	! $OMP PARALLEL DO
	DO e = 1, nelements
		DO l = 1, UZ_(e)% nlay
			IF (UZ_(e)% SM(l)% isactive) THEN
				ALLOCATE(UZ_(e)% SM(l)% Lstorage(time% Lnts+1), UZ_(e)% SM(l)% Lepv(time% Lnts+1)) ! UZ_(e)% SM(l)% Ldischarge(time% Lnts+1)
				UZ_(e)% SM(l)% Lstorage(1) = UZ_(e)% SM(l)% Gstorage(time% Gts-1)
			END IF
		END DO
	END DO
	! $OMP END PARALLEL DO


	! set local storage to the global storage of last dt (or initial conditions for first dt)
	GW% Lstorage(:, 1) = gw_ini
	UZ% Lstorage(:, 1) = UZ% Gstorage(:, time% Gts-1)
	SW% Lstorage(:, 1) = sw_ini
	
	FLUSH(logger% unit)
END SUBROUTINE
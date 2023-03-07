SUBROUTINE init_ts(auto_advance, gw_ini, sw_ini, first_run)
	USE datetime_module, ONLY: timedelta, datetime
	USE Mstorages, ONLY: Csm

	IMPLICIT NONE

	LOGICAL, INTENT(IN) :: auto_advance
	REAL(REAL64), INTENT(IN), DIMENSION(:), OPTIONAL :: gw_ini, sw_ini
	LOGICAL, INTENT(IN), OPTIONAL :: first_run ! auto advances by default

	INTEGER(INT16) :: idx
	CHARACTER(LEN=STRLEN) :: strbuffer

	REAL(REAL32) :: ipet_ll, ipet_ul, pet_intensity

	TYPE(Csm), POINTER :: pSM_
!TODO: rethink auto advance and keep only first_run flag (and add option to set SM ini too)?
	! advance the global timestep and deallocate the local storages, epvs, and discharges from the previous timestep 
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


	time% Lstart = time% Gstart + timedelta(seconds = time% Gdt% total_seconds() * (time% Gts-2))
	time% elapsed = time% Lstart - time% Gstart
	time% wall_elapsed = time% wall_start% now() - time% wall_start
	time% Lstop = time% Lstart + time% Gdt
	time% Lts = 1

	CALL logger% log(logger% INFO, "Local simulation period starts at " // TRIM(time% Lstart% strftime("%Y-%m-%d %H:%M:%S")))
	CALL logger% log(logger% INFO, "Gts = ", time% Gts)

	! calculate the average PET intensity over the spatial domain for this Gts
	pet_intensity = (SUM(ABS(EXTF% p(:, time% Gts))) + SUM(ABS(EXTF% et(:, time% Gts)))) / nelements
	CALL logger% log(logger% DEBUG, "Average PET intensity over the spatial domain is ", pet_intensity)

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
	CALL logger% log(logger% INFO, "Automatically calculated Local timestep size is ", time% Lnts)

	time% scratch_td = time% Lstop - time% Lstart
	time% Ldt = timedelta(seconds = (time% scratch_td% total_seconds() / time% Lnts))

	IF (time% Ldt > (time% Lstop - time% Lstart)) THEN
		CALL logger% log(logger% WARN, "The local simulation period is too short for the given time step size. Resetting the time step size to local simulation period length.")
		time% Ldt = time% Lstop - time% Lstart
		time% Lnts = 1
	END IF

	WRITE(strbuffer, *) "Initializing local simulation period with a timestep of ", time% Ldt% total_seconds(), " s."
	CALL logger% log(logger% INFO, strbuffer)

	! allocate local storages, epvs, and discharges for the current timestep
	CALL logger% log(logger% DEBUG, "Allocating local storages, epvs, and discharges for the current timestep")

	IF(auto_advance .OR. first_run) THEN
		ALLOCATE(GW% Lstorage(nelements, time% Lnts+1), UZ% Lstorage(nelements, time% Lnts+1), UZ% Lepv(nelements, time% Lnts+1), SW% Lstorage(nelements, time% Lnts+1), &
			GW% Ldischarge(nelements, time% Lnts+1), UZ% Ldischarge(nelements, time% Lnts+1), SW% Ldischarge(nelements, time% Lnts+1))

		! $OMP PARALLEL DO
		DO e = 1, nelements
			DO l = 1, UZ_(e)% nlay
				IF (UZ_(e)% SM(l)% isactive) THEN
					pSM_ => UZ_(e)% SM(l)
					
					ALLOCATE(pSM_% Lstorage(time% Lnts+1), pSM_% Lepv) ! UZ_(e)% SM(l)% Ldischarge(time% Lnts+1)

					pSM_% Lstorage(1) = pSM_% Gstorage(time% Gts-1)
					pSM_% Lepv = ABS(pSM_% RWubound - pSM_% RWlbound) * pSM_% porosity

					CALL logger% log(logger% DEBUG, "Rbounds: ", pSM_% RWubound, pSM_% RWlbound)

					WRITE(strbuffer, *) "SM ", l, " Lstorage_ini = ", pSM_% Lstorage(1), " Lepv_ini = ", pSM_% Lepv
					CALL logger% log(logger% DEBUG, strbuffer)
				END IF
			END DO
		END DO
		! $OMP END PARALLEL DO
	ELSE
		! $OMP PARALLEL DO
		DO e = 1, nelements
			DO l = 1, UZ_(e)% nlay
				IF (UZ_(e)% SM(l)% isactive) THEN
					pSM_ => UZ_(e)% SM(l)

					pSM_% Lstorage(1) = pSM_% Gstorage(time% Gts-1)
					pSM_% Lepv = ABS(pSM_% RWubound - pSM_% RWlbound) * pSM_% porosity

					CALL logger% log(logger% DEBUG, "Rbounds: ", pSM_% RWubound, pSM_% RWlbound)

					WRITE(strbuffer, *) "SM ", l, " Lstorage_ini = ", pSM_% Lstorage(1), " Lepv_ini = ", pSM_% Lepv
					CALL logger% log(logger% DEBUG, strbuffer)
				END IF
			END DO
		END DO
		! $OMP END PARALLEL DO
	
	END IF

	! set local storage to the global storage of last dt (or initial conditions for first dt)
	CALL logger% log(logger% TRACE, "Initialising local storages")
! write(*,*) time% Gts
	IF(PRESENT(gw_ini)) THEN
		GW% Lstorage(:, 1) = gw_ini
	ELSE
		GW% Lstorage(:, 1) = GW% Gstorage(:, time% Gts-1)
	END IF

	IF(PRESENT(sw_ini)) THEN
		SW% Lstorage(:, 1) = sw_ini
	ELSE
		SW% Lstorage(:, 1) = SW% Gstorage(:, time% Gts-1)
	END IF

	UZ% Lstorage(:, 1) = UZ% Gstorage(:, time% Gts-1)
	
	FLUSH(logger% unit)
END SUBROUTINE
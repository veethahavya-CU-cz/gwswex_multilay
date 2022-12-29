d = 0
DO i = UZ% nlay, 1
    SM(i)% depth = UZ% depth * UZ% depth_weights(i)
    SM(i)% lbound = d
    d = d + SM(i)% depth
    SM(i)% ubound = d
    SM(i)% eq = SM(i)%vanG% integrate(SM(i)%ubound, SM(i)%lbound)
END DO



! build.f90
	DO e = 1, nelements
		! allocate and initialize UZ layer counter for each element
		UZ_(e)% nlay = 0
		DO l = 1, UZ% nlay
			IF (UZ% layer(l)% isactive(e)) THEN
				UZ_(e)% nlay = UZ_(e)% nlay + 1
			END IF
		END DO
		! allocate the SM layers for each element
		ALLOCATE(UZ_(e)% SM(UZ_(e)% nlay))
		tmpcnt = 1
		DO l = 1, UZ% nlay
			IF (UZ% layer(l)% isactive(e)) THEN
				UZ_(e)% SM(tmpcnt)% lid = l
				tmpcnt = tmpcnt + 1
			END IF
		END DO

		! set the boundary conditions for each SM layer at build time
		DO smn = 1, UZ_(e)% nlay
			! default - inactive
			UZ_(e)% SM(smn)% isactive = .FALSE.
			UZ_(e)% SM(smn)% gse_bound = .FALSE.
			UZ_(e)% SM(smn)% gw_bound = .FALSE.
			IF (GW% Gstorage(e,1) < UZ% layer(UZ_(e)% SM(smn)% lid)% Aubound(e)) THEN
			!activate the layer if the GW storage is less than the Aubound of the layer i.e. GWS lies in or under the layer
				UZ_(e)% SM(smn)% isactive = .TRUE.
				IF (smn == 1) THEN
					UZ_(e)% SM(smn)% gse_bound = .TRUE.
					UZ_(e)% SM(smn)% Rubound = GW% Gstorage(e,1) - UZ%layer(UZ_(e)% SM(smn)% lid)% Aubound(e) ! ub = GWS - L_Aub
					UZ_(e)% SM(smn)% Rlbound = GW% Gstorage(e,1) - max(GW% Gstorage(e,1), UZ% layer(UZ_(e)% SM(smn)% lid)% Albound(e)) ! ub = GWS - max(GWS, L_Alb)
					IF (GW% Gstorage(e,1) > UZ% layer(UZ_(e)% SM(smn)% lid)% Albound(e)) THEN
						UZ_(e)% Albound = GW% Gstorage(e,1)
						UZ_(e)% SM(smn)% gw_bound = .TRUE.
						CONTINUE
					END IF
				ELSE
					UZ_(e)% SM(smn)% Rubound = GW% Gstorage(e,1) - UZ%layer(UZ_(e)% SM(smn)% lid)% Aubound(e) ! ub = GWS - L_Aub
					UZ_(e)% SM(smn)% Rlbound = GW% Gstorage(e,1) - max(GW% Gstorage(e,1), UZ% layer(UZ_(e)% SM(smn)% lid)% Albound(e)) ! ub = GWS - max(GWS, L_Alb)
					IF (GW% Gstorage(e,1) > UZ% layer(UZ_(e)% SM(smn)% lid)% Albound(e)) THEN
						UZ_(e)% Albound = GW% Gstorage(e,1)
						UZ_(e)% SM(smn)% gw_bound = .TRUE.
						CONTINUE
					END IF
				END IF
			END IF
			! set the UZ properties for each SM layer
			UZ_(e)% SM(smn)% vanG => UZ% layer(UZ_(e)% SM(smn)% lid)% vanG
			UZ_(e)% SM(smn)% ks = UZ% layer(UZ_(e)% SM(smn)% lid)% ks(e)
			UZ_(e)% SM(smn)% porosity = UZ% layer(UZ_(e)% SM(smn)% lid)% porosity(e)
		END DO
		UZ_(e)% thickness = UZ_(e)% Aubound - GW% Gstorage(e,1)
		IF (.NOT. UZ_(e)% SM(1)% isactive) THEN
			UZ_(e)% isactive = .FALSE.
		END IF
	END DO
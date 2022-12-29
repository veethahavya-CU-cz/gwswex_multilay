SUBROUTINE init(self, UZ, e)
    !USE Mstorages, ONLY: Cuz, Cuz_

    CLASS(Cuz_), INTENT(INOUT) :: self
    TYPE(Cuz), INTENT(IN) :: UZ
    INTEGER(INT8) :: l, tmpcnt
    INTEGER(INT32), INTENT(IN) :: e

    ! allocate and initialize UZ layer counter for each element
    self% nlay = 0
    DO l = 1, UZ% nlay
        IF (UZ% layer(l)% isactive(e)) THEN
            self% nlay = self% nlay + 1
        END IF
    END DO
    ! allocate the SM layers for each element
    ALLOCATE(self% SM(self% nlay))
    tmpcnt = 1
    DO l = 1, UZ% nlay
        IF (UZ% layer(l)% isactive(e)) THEN
            self% SM(tmpcnt)% lid = l
            tmpcnt = tmpcnt + 1
        END IF
    END DO
END SUBROUTINE init


SUBROUTINE setup(self, UZ, GW, e)
    !USE Mstorages, ONLY: Cuz, Cuz_
    USE Mtiming, ONLY : Ctime

    CLASS(Cuz_), INTENT(INOUT) :: self
    TYPE(Cuz), INTENT(IN) :: UZ
    TYPE(Cgw), INTENT(IN) :: GW
    INTEGER(INT8) :: smn
    INTEGER(INT32), INTENT(IN) :: e

    ! set the boundary conditions for each SM layer at build time
    DO smn = 1, self% nlay
        ! default - inactive
        self% SM(smn)% isactive = .FALSE.
        self% SM(smn)% gw_bound = .FALSE.
        IF (GW% Gstorage(e,1) < UZ% layer(self% SM(smn)% lid)% Aubound(e)) THEN
        !activate the layer if the GW storage is less than the Aubound of the layer i.e. GWS lies in or under the layer
            self% SM(smn)% isactive = .TRUE.
            self% SM(smn)% Rubound = GW% Gstorage(e,1) - UZ% layer(self% SM(smn)% lid)% Aubound(e) ! ub = GWS - L_Aub
            self% SM(smn)% Rlbound = GW% Gstorage(e,1) - max(GW% Gstorage(e,1), UZ% layer(self% SM(smn)% lid)% Albound(e)) ! ub = GWS - max(GWS, L_Alb)
            IF (GW% Gstorage(e,1) > UZ% layer(self% SM(smn)% lid)% Albound(e)) THEN
                self% Albound = GW% Gstorage(e,1)
                self% SM(smn)% gw_bound = .TRUE.
                self% gws_bnd_lid = self% SM(smn)% lid
                self% gws_bnd_smid = smn
                CONTINUE
            END IF
        END IF
        ! set the UZ properties for each SM layer
        self% SM(smn)% vanG => UZ% layer(self% SM(smn)% lid)% vanG
        self% SM(smn)% ks = UZ% layer(self% SM(smn)% lid)% ks(e)
        self% SM(smn)% porosity = UZ% layer(self% SM(smn)% lid)% porosity(e)
    END DO

    self% thickness = self% Aubound - GW% Gstorage(e,1)
    IF (.NOT. self% SM(1)% isactive) THEN
        self% isactive = .FALSE.
    END IF
!TODO: init SM storages and set ICs
END SUBROUTINE setup



SUBROUTINE resolve(self, UZ, GW, time, e)
    !USE Mstorages, ONLY: Cuz, Cuz_
    USE Mtiming, ONLY : Ctime

    CLASS(Cuz_), INTENT(INOUT) :: self
    TYPE(Cuz), INTENT(IN) :: UZ
    TYPE(Cgw), INTENT(IN) :: GW
    TYPE(Ctime), INTENT(IN) :: time
    INTEGER(INT8) :: smn
    INTEGER(INT32), INTENT(IN) :: e

    IF (GW% Lstorage(e,time% Lts) < UZ% layer(self% gws_bnd_smid)% Aubound(e)) THEN
    ! case when GW has decreased from last dt
        IF (GW% Lstorage(e,time% Lts) > UZ% layer(self% gws_bnd_smid)% Albound(e)) THEN !.AND. self% SM(self% gws_bnd_smid)% gw_bound
        ! case where GW is still in the layer
            self% SM(self% gws_bnd_smid)% Rubound = GW% Lstorage(e,time% Lts) - UZ% layer(self% gws_bnd_smid)% Aubound(e) ! ub = GWS - L_Aub
            self% SM(self% gws_bnd_smid)% Rlbound = GW% Lstorage(e,time% Lts) - max(GW% Lstorage(e,time% Lts), UZ% layer(self% gws_bnd_smid)% Albound(e)) ! ub = GWS - max(GWS, L_Alb)
            ! exit
        ELSE
        ! case where GW has moved to a lower layer
            self% SM(self% gws_bnd_smid)% gw_bound = .FALSE.
            self% SM(self% gws_bnd_smid)% Rubound = GW% Lstorage(e,time% Lts) - UZ% layer(self% gws_bnd_smid)% Aubound(e) ! ub = GWS - L_Aub
            self% SM(self% gws_bnd_smid)% Rlbound = GW% Lstorage(e,time% Lts) - UZ% layer(self% gws_bnd_smid)% Albound(e) ! ub = GWS - L_Alb
            DO smn = self% gws_bnd_smid+1, self% nlay
            ! resolve all layers below until gw_bnd layer
			! TODO: resolve the storages of these layers - set them to eq? (where to draw this from - GW? or does MF6 already do this? CHECK)
                IF (GW% Lstorage(e,time% Lts) < UZ% layer(smn)% Aubound(e)) THEN
                    IF (.NOT. self% SM(smn)% isactive) THEN
                        self% SM(smn)% isactive = .TRUE.
                        self% SM(self% gws_bnd_smid)% Rubound = GW% Lstorage(e,time% Lts) - UZ% layer(self% gws_bnd_smid)% Aubound(e) ! ub = GWS - L_Aub
                        self% SM(self% gws_bnd_smid)% Rlbound = GW% Lstorage(e,time% Lts) - max(GW% Lstorage(e,time% Lts), UZ% layer(self% gws_bnd_smid)% Albound(e)) ! ub = GWS - max(GWS, L_Alb)
                        IF (GW% Lstorage(e,time% Lts) > UZ% layer(self% SM(smn)% lid)% Albound(e)) THEN
                            self% Albound = GW% Gstorage(e,time% Lts)
                            self% SM(smn)% gw_bound = .TRUE.
                            self% gws_bnd_lid = self% SM(smn)% lid
                            self% gws_bnd_smid = smn
                            CONTINUE
                        END IF
                    END IF
                END IF
            END DO
        END IF
    ELSE
        ! case when GW has increased from last dt
		! TODO: resolve the storages of this layer
		self% SM((self% gws_bnd_smid))% isactive = .FALSE.
		self% SM((self% gws_bnd_smid))% Rubound = 0.0
		self% SM((self% gws_bnd_smid))% Rlbound = 0.0
		DO smn = self% gws_bnd_smid-1, 1, -1
		! resolve all layers up until gws_bnd layer or untiiiil UZ is fully wet, i.e. until 1st layer is unactive
			IF (GW% Lstorage(e,time% Lts) < UZ% layer(smn)% Aubound(e)) THEN
				self% SM(self% gws_bnd_smid)% Rubound = GW% Lstorage(e,time% Lts) - UZ% layer(self% gws_bnd_smid)% Aubound(e) ! ub = GWS - L_Aub
				self% SM(self% gws_bnd_smid)% Rlbound = GW% Lstorage(e,time% Lts) - max(GW% Lstorage(e,time% Lts), UZ% layer(self% gws_bnd_smid)% Albound(e)) ! ub = GWS - max(GWS, L_Alb)
				IF (GW% Lstorage(e,time% Lts) > UZ% layer(self% SM(smn)% lid)% Albound(e)) THEN
					self% Albound = GW% Gstorage(e,time% Lts)
					self% SM(smn)% gw_bound = .TRUE.
					self% gws_bnd_lid = self% SM(smn)% lid
					self% gws_bnd_smid = smn
					CONTINUE
				END IF
			ELSE
			! TODO: resolve the storages of this layer - transfer the storage (-theta_r? how does MF6 handle Sy i.e. amt of water released per unit drop in water table) to upper layer
				self% SM(smn)% isactive = .FALSE.
				self% SM(smn)% Rubound = 0.0
				self% SM(smn)% Rlbound = 0.0
			END IF
		END DO
    END IF

END SUBROUTINE resolve


SUBROUTINE solve_dt_uz()

END SUBROUTINE solve_dt_uz
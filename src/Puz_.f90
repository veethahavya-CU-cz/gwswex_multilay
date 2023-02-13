SUBROUTINE init(self, UZ, GW, time, e)
    ! USE Mstorages, ONLY: Cuz, Cuz_
    USE Mtiming, ONLY : Ctime

    CLASS(Cuz_), INTENT(INOUT) :: self
    TYPE(Cuz), INTENT(INOUT) :: UZ
    TYPE(Cgw), INTENT(IN) :: GW
    TYPE(Ctime), INTENT(IN) :: time
    INTEGER(INT32), INTENT(IN) :: e
    INTEGER(INT8) :: ln, smn, tmpcnt
    REAL(REAL128) :: UZ_storage_sum

    UZ_storage_sum = 0.0

    ! calculate and allocate the number oƒ active SM layers for element e
    self% nlay = 0
    DO ln = 1, UZ% nlay
        IF (UZ% layer(ln)% isactive(e)) THEN
            self% nlay = self% nlay + 1
        END IF
    END DO
    ALLOCATE(self% SM(self% nlay))

    ! associte the SM layers with the respective UZ layers and allocate the SM storages for all active layers for element e
    tmpcnt = 1
    DO ln = 1, UZ% nlay
        IF (UZ% layer(ln)% isactive(e)) THEN
            self% SM(tmpcnt)% lid = ln
            ALLOCATE(self% SM(tmpcnt)% Gstorage(time% Gnts))
            ! deactivate the SM layer and set the GW bound flag to false by default
            self% SM(tmpcnt)% isactive = .FALSE.
            self% SM(tmpcnt)% gw_bound = .FALSE.
            tmpcnt = tmpcnt + 1
        END IF
    END DO

    ! initialize active SM layer bounds, set initial storages (SMeq), and flag GW bound SM layer for element e
    DO smn = 1, self% nlay
        ! set the UZ properties for each SM layer
        self% SM(smn)% vanG => UZ% layer(self% SM(smn)% lid)% vanG
        self% SM(smn)% ks => UZ% layer(self% SM(smn)% lid)% ks(e)
        self% SM(smn)% porosity => UZ% layer(self% SM(smn)% lid)% porosity(e)

        IF (GW% Gstorage(e,1) < UZ% layer(self% SM(smn)% lid)% Aubound(e)) THEN
        ! activate the layer if the GW storage is less than the Aubound of the layer i.e. GWS lies in or under the layer
            self% SM(smn)% isactive = .TRUE.
            ALLOCATE(self% SM(smn)% Rubound, self% SM(smn)% Rlbound)
            ! calculate the relative bounds for SMeq calculation
            self% SM(smn)% Rubound = GW% Gstorage(e,1) - UZ% layer(self% SM(smn)% lid)% Aubound(e) ! ub = GWS - L_Aub
            self% SM(smn)% Rlbound = GW% Gstorage(e,1) - max(GW% Gstorage(e,1), UZ% layer(self% SM(smn)% lid)% Albound(e)) ! ub = GWS - max(GWS, L_Alb)

            ! set the initial condition for each SM layer to SMeq
            ! TODO: check how the bounds work with vanGI calc on a seperate py script
            self% SM(smn)% Gstorage(1) = self% SM(smn)% vanG% integrate(self% SM(smn)% Rubound, self% SM(smn)% Rlbound)
            UZ_storage_sum = UZ_storage_sum + self% SM(smn)% Gstorage(1)

            IF (GW% Gstorage(e,1) > UZ% layer(self% SM(smn)% lid)% Albound(e) .OR. GW% Gstorage(e,1) == UZ% layer(self% SM(smn)% lid)% Albound(e)) THEN
            ! set the GW bound flag if the GWS lies above the Albound of the layer i.e. GWS lies within the layer and skip checking the underlying SM layers
                self% Albound = GW% Gstorage(e,1)
                self% SM(smn)% gw_bound = .TRUE.
                self% gws_bnd_lid = self% SM(smn)% lid
                self% gws_bnd_smid = smn
                CONTINUE
            END IF
        ELSE
            self% SM(smn)% Gstorage(1) = 0.0
        END IF
    END DO

    ! calculate the UZ storage for this element by summing all active SM storages
    UZ% Gstorage(e,1) = UZ_storage_sum

    ! set the bounds of UZ and calculate its thickness if UZ is active for element e
    IF (.NOT. self% SM(1)% isactive) THEN
        self% isactive = .FALSE.
    ELSE
        self% Aubound => UZ% layer(self% SM(1)% lid)% Aubound(e)
    self% Albound = GW% Gstorage(e,1)
    self% thickness = self% Aubound - GW% Gstorage(e,1)
    END IF


END SUBROUTINE init



SUBROUTINE resolve(self, UZ, GW, time, e)
    !USE Mstorages, ONLY: Cuz, Cuz_
    USE Mtiming, ONLY : Ctime

    CLASS(Cuz_), INTENT(INOUT) :: self
    TYPE(Cuz), INTENT(IN) :: UZ
    TYPE(Cgw), INTENT(IN) :: GW
    TYPE(Ctime), INTENT(IN) :: time
    INTEGER(INT8) :: smn
    INTEGER(INT32), INTENT(IN) :: e

    IF (.NOT. ( (GW% Lstorage(e,time% Lts) < UZ% layer(self% gws_bnd_smid)% Aubound(e)) .AND. (GW% Lstorage(e,time% Lts) > UZ% layer(self% gws_bnd_smid)% Albound(e)) ) ) THEN
    ! if GWS does not lie within gws_bnd_smid layer
        IF (GW% Lstorage(e,time% Lts) < UZ% layer(self% gws_bnd_smid)% Albound(e)) THEN
        ! if GWS lies under gws_bnd layer, i.e. GWS is falling
            ! recalculate the bounds of the previously GW bound layer and deactivate the gw_bound flag
            self% SM(self% gws_bnd_smid)% Rubound = GW% Lstorage(e,time% Lts) - UZ% layer(self% gws_bnd_lid)% Aubound(e) ! ub = GWS - L_Aub
            self% SM(self% gws_bnd_smid)% Rlbound = GW% Lstorage(e,time% Lts) - UZ% layer(self% gws_bnd_lid)% Albound(e) ! ub = GWS - L_Alb
            self% SM(self% gws_bnd_smid)% gw_bound = .FALSE.
            
            ! activate the underlying layers until the GW bound layer is discovered, set the bounds, set the GW bound flag, and calculate the respective storages
            DO smn = self% gws_bnd_smid+1, self% nlay
                self% SM(smn)% isactive = .TRUE.
                ALLOCATE(self% SM(smn)% Rubound, self% SM(smn)% Rlbound)
                self% SM(smn)% Rubound = GW% Lstorage(e,time% Lts) - UZ% layer(self% SM(smn)% lid)% Aubound(e) ! ub = GWS - L_Aub
                self% SM(smn)% Rlbound = GW% Lstorage(e,time% Lts) - max(GW% Lstorage(e,time% Lts), UZ% layer(self% SM(smn)% lid)% Albound(e)) ! ub = GWS - max(GWS, L_Alb)
                IF (GW% Lstorage(e,time% Lts) > UZ% layer(self% SM(smn)% lid)% Albound(e)) THEN
                    self% SM(smn)% gw_bound = .TRUE.
                    self% gws_bnd_smid = smn
                    self% gws_bnd_lid = self% SM(self% gws_bnd_smid)% lid
                    ALLOCATE(self% SM(smn)% Lstorage(time% Lnts))
                    self% SM(smn)% Lstorage(time% Lts) = self% SM(smn)% vanG% integrate(self% SM(smn)% Rubound, self% SM(smn)% Rlbound)
                    ! TODO: subtract this (SMeq) from the GW storage ?(until a convergence threshold is reached i.e. GWS is not changing by a significant amount)
                    EXIT
                ELSE
                    ALLOCATE(self% SM(smn)% Lstorage(time% Lnts))
                    self% SM(smn)% Lstorage(time% Lts) = self% SM(smn)% vanG% integrate(self% SM(smn)% Rubound, self% SM(smn)% Rlbound)
                    ! TODO: subtract this (SMeq) from the GW storage ?(until a convergence threshold is reached i.e. GWS is not changing by a significant amount)
                END IF
            END DO

        ELSE
        ! if GWS lies above gws_bnd layer, i.e. GWS is rising
            !  deactivate the the previously GW bound layer
            self% SM(self% gws_bnd_smid)% gw_bound = .FALSE.
            self% SM(self% gws_bnd_smid)% isactive = .FALSE.
            ! TODO: transfer the Lstorage of the previously GW bound layer to the GW storage ?(until a convergence threshold is reached i.e. GWS is not changing by a significant amount)
            DEALLOCATE(self% SM(self% gws_bnd_smid)% Lstorage, self% SM(self% gws_bnd_smid)% Rubound, self% SM(self% gws_bnd_smid)% Rlbound)

            DO smn = self% gws_bnd_smid-1, 1, -1
                IF (GW% Lstorage(e,time% Lts) < UZ% layer(self% SM(smn)% lid)% Aubound(e)) THEN
                    self% SM(smn)% Rubound = GW% Lstorage(e,time% Lts) - UZ% layer(self% SM(smn)% lid)% Aubound(e) ! ub = GWS - L_Aub
                    self% SM(smn)% Rlbound = GW% Lstorage(e,time% Lts) - max(GW% Lstorage(e,time% Lts), UZ% layer(self% SM(smn)% lid)% Albound(e)) ! ub = GWS - max(GWS, L_Alb)
                    self% SM(smn)% gw_bound = .TRUE.
                    self% gws_bnd_smid = smn
                    self% gws_bnd_lid = self% SM(self% gws_bnd_smid)% lid
                    ALLOCATE(self% SM(smn)% Lstorage(time% Lnts))
                    self% SM(smn)% Lstorage(time% Lts) = self% SM(smn)% vanG% integrate(self% SM(smn)% Rubound, self% SM(smn)% Rlbound)
                    ! TODO: subtract this (SMeq) from the GW storage ?(until a convergence threshold is reached i.e. GWS is not changing by a significant amount)
                    EXIT
                ELSE
                    self% SM(smn)% gw_bound = .FALSE.
                    self% SM(smn)% isactive = .FALSE.
                    ! TODO: subtract this (SMeq) from the GW storage ?(until a convergence threshold is reached i.e. GWS is not changing by a significant amount)
                    DEALLOCATE(self% SM(smn)% Lstorage, self% SM(smn)% Rubound, self% SM(smn)% Rlbound)
                END IF
            END DO
        END IF
    ELSE
        self% SM(self% gws_bnd_smid)% Rubound = GW% Lstorage(e,time% Lts) - UZ% layer(self% gws_bnd_lid)% Aubound(e) ! ub = GWS - L_Aub
        self% SM(self% gws_bnd_smid)% Rlbound = GW% Lstorage(e,time% Lts) - UZ% layer(self% gws_bnd_lid)% Albound(e) ! ub = GWS - L_Alb
        ! TODO: calculate the new SMeq?
    END IF

END SUBROUTINE resolve


! SUBROUTINE solve_dt()

! END SUBROUTINE solve_dt
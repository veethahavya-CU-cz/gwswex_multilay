SUBROUTINE init(self, e, UZ, GW, time)
    ! USE Mstorages, ONLY: Cuz, Cuz_
    USE Mtiming, ONLY : Ctime

    CLASS(Cuz_), INTENT(INOUT) :: self
    INTEGER(INT32), INTENT(IN) :: e
    TYPE(Cuz), INTENT(INOUT) :: UZ
    TYPE(Cgw), INTENT(IN) :: GW
    TYPE(Ctime), INTENT(IN) :: time

    INTEGER(INT8) :: ln, smn, tmpcnt
    REAL(REAL128) :: UZ_storage_sum

    TYPE(Csm), POINTER :: pSM_

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
            ALLOCATE(self% SM(tmpcnt)% lid)
            ! associate the SM layer with the respective UZ layer
            self% SM(tmpcnt)% lid = ln
            tmpcnt = tmpcnt + 1
        END IF
    END DO


    ! initialize active SM layer bounds, initialize storages (to SMeq), and flag GW bound SM layer for element e
    DO smn = 1, self% nlay
        pSM_ => self% SM(smn)

        ! deactivate the SM layer and set the GW bound flag to false by default   
        pSM_% isactive = .FALSE.
        pSM_% gw_bound = .FALSE.

        ALLOCATE(pSM_% Gstorage(time% Gnts))

        ! set the UZ properties for each SM layer
        pSM_% vanG => UZ% layer(pSM_% lid)% vanG
        pSM_% ks => UZ% layer(pSM_% lid)% ks(e)
        pSM_% porosity => UZ% layer(pSM_% lid)% porosity(e)

        pSM_% ADlbound => UZ% layer(pSM_% lid)% Albound(e)
        pSM_% ADubound => UZ% layer(pSM_% lid)% Aubound(e)

        IF (GW% Gstorage(e,1) < pSM_% ADubound) THEN
        ! activate the layer if the GW storage is less than the Aubound of the layer i.e. GWS lies in or under the layer
            pSM_% isactive = .TRUE.
            ALLOCATE(pSM_% RWubound, pSM_% RWlbound, pSM_% EQstorage, pSM_% exfiltration, &
                pSM_% kUS_inf, pSM_% kUS_exf, pSM_% IC, pSM_% IC_ratio)
            ! calculate the relative bounds for SMeq calculation
            pSM_% RWubound = GW% Gstorage(e,1) - pSM_% ADubound ! ub = GWS - L_Aub
            pSM_% RWlbound = GW% Gstorage(e,1) - MAX(GW% Gstorage(e,1), pSM_% ADlbound) ! ub = GWS - MAX(GWS, L_Alb)

            ! set the initial condition for each SM layer to SMeq
            CALL pSM_% vanG% setvars()
            pSM_% Gstorage(1) = pSM_% vanG% integrate(pSM_% RWubound, pSM_% RWlbound)

            UZ_storage_sum = UZ_storage_sum + pSM_% Gstorage(1)
            IF (GW% Gstorage(e,1) > pSM_% ADlbound .OR. GW% Gstorage(e,1) == pSM_% ADlbound) THEN
            ! set the GW bound flag if the GWS lies above the Albound of the layer i.e. GWS lies within the layer and skip checking the underlying SM layers
                self% Albound => GW% Gstorage(e,1)
                pSM_% gw_bound = .TRUE.
                self% gws_bnd_lid = pSM_% lid
                self% gws_bnd_smid = smn
                CONTINUE
            END IF
        ELSE
            pSM_% Gstorage(1) = 0.0
        END IF
    END DO

    ! calculate the UZ storage for this element by summing all active SM storages
    UZ% Gstorage(e,1) = UZ_storage_sum

    ! set the bounds of UZ and calculate its thickness if UZ is active for element e
    IF (.NOT. self% SM(1)% isactive) THEN
        self% isactive = .FALSE.
    ELSE
        self% isactive = .TRUE.
    END IF

END SUBROUTINE init


! Lstorage and Lepv are ALLOCATED in init_ts() if SM(i)% isactive
SUBROUTINE resolve(self, e, UZ, GW, time, solver_settings)
    !USE Mstorages, ONLY: Cuz, Cuz_
    USE Mtiming, ONLY : Ctime

    CLASS(Cuz_), INTENT(INOUT) :: self
    INTEGER(INT32), INTENT(IN) :: e
    TYPE(Cuz), INTENT(INOUT) :: UZ
    TYPE(Cgw), INTENT(INOUT) :: GW
    TYPE(Ctime), INTENT(IN) :: time
    TYPE(Csettings), INTENT(IN) :: solver_settings

    INTEGER(INT8) :: smn
    INTEGER(INT16) :: itr
    REAL(REAL128) :: prev_sm_storage

    TYPE(Csm), POINTER :: pSM_

    pSM_ => self% SM(self% gws_bnd_smid)

    IF (.NOT. ( (GW% Lstorage(e,time% Lts) < pSM_% ADubound) .AND. (GW% Lstorage(e,time% Lts) > pSM_% ADlbound) ) ) THEN
    ! if GWS does NOT lie within gws_bnd_smid layer
        IF (GW% Lstorage(e,time% Lts) < pSM_% ADlbound) THEN
        ! if GWS lies under gws_bnd layer, i.e. GWS is falling
            ! recalculate the bounds of the previously GW bound layer and deactivate the gw_bound flag
            pSM_% RWubound = GW% Lstorage(e,time% Lts) - pSM_% ADubound ! ub = GWS - L_Aub
            pSM_% RWlbound = GW% Lstorage(e,time% Lts) - pSM_% ADlbound ! ub = GWS - L_Alb
            pSM_% gw_bound = .FALSE.
            pSM_% Lepv = ABS(pSM_% RWubound - pSM_% RWlbound) * pSM_% porosity
            
            ! activate the underlying layers until the GW bound layer is discovered, set the bounds, set the GW bound flag, and calculate the respective storages
            DO smn = self% gws_bnd_smid+1, self% nlay
                pSM_ => self% SM(smn)

                pSM_% isactive = .TRUE.
                ALLOCATE(pSM_% RWubound, pSM_% RWlbound, pSM_% Lstorage(time% Lnts), pSM_% Lepv(time% Lnts), &
                    pSM_% IC, pSM_% IC_ratio, pSM_% EQstorage, pSM_% exfiltration, pSM_% kUS_inf, pSM_% kUS_exf)
                pSM_% RWubound = GW% Lstorage(e,time% Lts) - pSM_% ADubound ! ub = GWS - L_Aub
                pSM_% RWlbound = GW% Lstorage(e,time% Lts) - MAX(GW% Lstorage(e,time% Lts), pSM_% ADlbound) ! ub = GWS - MAX(GWS, L_Alb)
                pSM_% Lepv(time% Lnts) = ABS(pSM_% RWubound - pSM_% RWlbound) * pSM_% porosity
                pSM_% IC = ABS(pSM_% RWubound - pSM_% RWlbound) / pSM_% vanG% theta_r
                IF (GW% Lstorage(e,time% Lts) > pSM_% ADlbound) THEN
                    pSM_% gw_bound = .TRUE.
                    self% gws_bnd_smid = smn
                    self% gws_bnd_lid = self% SM(self% gws_bnd_smid)% lid
                    CALL pSM_% vanG% setvars()
                    pSM_% Lstorage(time% Lts) = pSM_% vanG% integrate(pSM_% RWubound, pSM_% RWlbound)
                    ! iteratively calculate the storage of the layer until the consecutive change storage is within the tolerance
                    prev_sm_storage = 0.0
                    itr = 0
                    DO WHILE(itr < solver_settings% max_iterations .AND. (ABS(pSM_% Lstorage(time% Lts) - prev_sm_storage) > solver_settings% sm_gw_fluctuation_tolerance))
                        GW% Lstorage(e,time% Lts) = GW% Lstorage(e,time% Lts) - (pSM_% Lstorage(time% Lts) - prev_sm_storage) / pSM_% porosity
                        IF (.NOT. GW% Lstorage(e,time% Lts) > pSM_% ADlbound) CALL self% resolve(e, UZ, GW, time, solver_settings)
                        pSM_% RWlbound = GW% Lstorage(e,time% Lts) - GW% Lstorage(e,time% Lts)
                        prev_sm_storage = pSM_% Lstorage(time% Lts)
                        pSM_% Lstorage(time% Lts) = pSM_% vanG% integrate(pSM_% RWubound, pSM_% RWlbound)
                        itr = itr + 1
                    END DO
                    pSM_% Lepv(time% Lnts) = ABS(pSM_% RWubound - pSM_% RWlbound) * pSM_% porosity
                    EXIT
                ELSE
                    CALL pSM_% vanG% setvars()
                    pSM_% Lstorage(time% Lts) = pSM_% vanG% integrate(pSM_% RWubound, pSM_% RWlbound)
                    ! iteratively calculate the storage of the layer until the consecutive change storage is within the tolerance
                    prev_sm_storage = 0.0
                    itr = 0
                    DO WHILE(itr < solver_settings% max_iterations .AND. (ABS(pSM_% Lstorage(time% Lts) - prev_sm_storage) > solver_settings% sm_gw_fluctuation_tolerance))
                        GW% Lstorage(e,time% Lts) = GW% Lstorage(e,time% Lts) - (pSM_% Lstorage(time% Lts) - prev_sm_storage) / pSM_% porosity
                        IF (.NOT. GW% Lstorage(e,time% Lts) > pSM_% ADlbound) CALL self% resolve(e, UZ, GW, time, solver_settings)
                        pSM_% RWlbound = GW% Lstorage(e,time% Lts) - GW% Lstorage(e,time% Lts)
                        prev_sm_storage = pSM_% Lstorage(time% Lts)
                        pSM_% Lstorage(time% Lts) = pSM_% vanG% integrate(pSM_% RWubound, pSM_% RWlbound)
                        itr = itr + 1
                    END DO
                    pSM_% Lepv(time% Lnts) = ABS(pSM_% RWubound - pSM_% RWlbound) * pSM_% porosity
                END IF
            END DO

        ELSE
        ! if GWS lies above gws_bnd layer, i.e. GWS is rising
            pSM_ => self% SM(self% gws_bnd_smid)
            ! deactivate the the previously GW bound layer
            pSM_% gw_bound = .FALSE.
            pSM_% isactive = .FALSE.
            GW% Lstorage(e,time% Lts) = GW% Lstorage(e,time% Lts) + (pSM_% Lstorage(time% Lts) / pSM_% porosity)
            DEALLOCATE(pSM_% RWubound, pSM_% RWlbound, pSM_% Lstorage, pSM_% Lepv, pSM_% EQstorage, pSM_% exfiltration, pSM_% kUS_inf, pSM_% kUS_exf, pSM_% IC, pSM_% IC_ratio)
            IF(ALLOCATED(pSM_% infiltration)) DEALLOCATE(pSM_% infiltration)

            ! deactivate underlying layers until GWS bound layer is discovered and flagged
            DO smn = self% gws_bnd_smid-1, 1, -1
                pSM_ => self% SM(smn)

                IF (GW% Lstorage(e,time% Lts) < pSM_% ADubound) THEN
                    pSM_% RWubound = GW% Lstorage(e,time% Lts) - pSM_% ADubound ! ub = GWS - L_Aub
                    pSM_% RWlbound = GW% Lstorage(e,time% Lts) - MAX(GW% Lstorage(e,time% Lts), pSM_% ADlbound) ! ub = GWS - MAX(GWS, L_Alb)
                    pSM_% Lepv(time% Lnts) = ABS(pSM_% RWubound - pSM_% RWlbound) * pSM_% porosity
                    pSM_% gw_bound = .TRUE.
                    self% gws_bnd_smid = smn
                    self% gws_bnd_lid = self% SM(self% gws_bnd_smid)% lid
                    IF(pSM_% Lstorage(time% Lnts) > pSM_% Lepv(time% Lnts)) THEN
                        prev_sm_storage = 0.0
                        itr = 0
                        DO WHILE(itr < solver_settings% max_iterations .AND. (ABS(pSM_% Lstorage(time% Lts) - prev_sm_storage) > solver_settings% sm_gw_fluctuation_tolerance))
                            GW% Lstorage(e,time% Lts) = GW% Lstorage(e,time% Lts) + ((pSM_% Lstorage(time% Lnts) - prev_sm_storage) - pSM_% Lepv(time% Lnts)) / pSM_% porosity
                            pSM_% Lstorage = pSM_% Lepv
                            IF (.NOT. GW% Lstorage(e,time% Lts) < pSM_% ADubound) CALL self% resolve(e, UZ, GW, time, solver_settings)
                            pSM_% RWubound = GW% Lstorage(e,time% Lts) - pSM_% ADubound
                            pSM_% Lepv(time% Lnts) = ABS(pSM_% RWubound - pSM_% RWlbound) * pSM_% porosity
                            itr = itr + 1
                        END DO
                    END IF
                    EXIT
                ELSE
                    pSM_% gw_bound = .FALSE.
                    pSM_% isactive = .FALSE.
                    GW% Lstorage(e,time% Lts) = GW% Lstorage(e,time% Lts) + (pSM_% Lstorage(time% Lnts) / pSM_% porosity)
                    DEALLOCATE(pSM_% RWubound, pSM_% RWlbound, pSM_% Lstorage, pSM_% Lepv, &
                        pSM_% EQstorage, pSM_% exfiltration, pSM_% kUS_inf, pSM_% kUS_exf, pSM_% IC, pSM_% IC_ratio)
                    IF(ALLOCATED(pSM_% infiltration)) DEALLOCATE(pSM_% infiltration)
                END IF
            END DO
        END IF
    ELSE
    ! if GWS lies within the bounds of the gws_bnd layer
        pSM_ => self% SM(self% gws_bnd_smid)

        pSM_% RWubound = GW% Lstorage(e,time% Lts) - pSM_% ADubound ! ub = GWS - L_Aub
        pSM_% RWlbound = GW% Lstorage(e,time% Lts) - pSM_% ADlbound ! ub = GWS - L_Alb
        pSM_% Lepv = ABS(pSM_% RWubound - pSM_% RWlbound) * pSM_% porosity
    END IF

    IF(.NOT. ALLOCATED(self% SM(1)% infiltration)) ALLOCATE(self% SM(1)% infiltration)

    UZ% Lstorage(e,time% Lts) = 0.0
    UZ% Lepv(e,time% Lts) = 0.0
    DO smn = 1, self% gws_bnd_smid
        IF (self% SM(smn)% isactive) THEN
            UZ% Lstorage(e,time% Lts) = UZ% Lstorage(e,time% Lts) + self% SM(smn)% Lstorage(time% Lts)
            UZ% Lepv(e,time% Lts) = UZ% Lepv(e,time% Lts) + self% SM(smn)% Lepv(time% Lts)
        END IF
    END DO

! set the bounds of UZ and calculate its thickness if UZ is active for element e
    IF (.NOT. self% SM(1)% isactive) THEN
        self% isactive = .FALSE.
    ELSE
        self% isactive = .TRUE.
    END IF
    self% Albound => GW% Lstorage(e,time% Lts)

END SUBROUTINE resolve



SUBROUTINE solve(self, e, UZ, time, first_run)

    USE Mtiming, ONLY : Ctime

    INTEGER(INT32), INTENT(IN) :: e
    CLASS(Cuz_), INTENT(INOUT) :: self
    TYPE(Cuz), INTENT(INOUT) :: UZ
    TYPE(Ctime), INTENT(IN) :: time
    LOGICAL, INTENT(IN) :: first_run

    TYPE(Csm), POINTER :: pSM_, pSM_prev_
    REAL(REAL32) :: dt
    INTEGER(INT32), POINTER :: t
    INTEGER(INT8) :: smn

    dt = time% Ldt% total_seconds()
    t => time% Lts


    smn = 1
    pSM_ => self% SM(smn)

    CALL pSM_% vanG% setvars()
    pSM_% EQstorage = pSM_% vanG% integrate(pSM_% RWubound, pSM_% RWlbound)

    IF(first_run) THEN
        IF(pSM_% infiltration /= 0.0) THEN
            pSM_% IC = MAX(pSM_% IC + pSM_% kUS_inf * dt, 0.0)
        ELSE
            pSM_% IC = MAX(pSM_% IC - pSM_% kUS_inf * dt, 0.0)
        END IF

        pSM_% IC_ratio = MIN(1.0, MAX(pSM_% IC / ABS(pSM_% RWubound - pSM_% RWubound), pSM_% vanG% theta_r))

        pSM_% kUS_exf = pSM_% vanG% kUS(MIN(pSM_% Lstorage(t) / pSM_% Lepv(t), 1.0) * pSM_% porosity, pSM_% ks)
    ELSE
        pSM_% kUS_exf = pSM_% kUS_exf - MAX(pSM_% exfiltration, 0.0)
    END IF

    pSM_% exfiltration = MIN(pSM_% Lstorage(t) - pSM_% EQstorage, pSM_% kUS_exf * dt) * pSM_% IC_ratio ! negative exfiltration (i.e. infiltration from underlying layers) is not capped to always allow replenishment of SM until SMeq is reached as it is based on relatively instanteous capilary forces

    pSM_% Lstorage(t) = pSM_% Lstorage(t) - pSM_% exfiltration


    smn = 2
    DO WHILE(self% SM(smn)% isactive)
        pSM_ => self% SM(smn)
        pSM_prev_ => self% SM(smn-1)

        pSM_% Lstorage(t) = pSM_% Lstorage(t-1) + pSM_prev_% exfiltration

        CALL pSM_% vanG% setvars()
        pSM_% EQstorage = pSM_% vanG% integrate(pSM_% RWubound, pSM_% RWlbound)

        IF(first_run) THEN
            pSM_% kUS_inf = pSM_% vanG% kUS(MIN(pSM_% Lstorage(t-1) / pSM_% Lepv(t-1), 1.0) * pSM_% porosity, pSM_% ks)

            IF(pSM_prev_% exfiltration > 0.0) THEN
                pSM_% IC = MAX(pSM_% IC + pSM_% kUS_inf * dt, 0.0)
            ELSE
                pSM_% IC = MAX(pSM_% IC - pSM_% kUS_inf * dt, 0.0)
            END IF

            pSM_% IC_ratio = MIN(1.0, MAX(pSM_% IC / ABS(pSM_% RWubound - pSM_% RWubound), pSM_% vanG% theta_r))

            pSM_% kUS_exf = pSM_% vanG% kUS(MIN(pSM_% Lstorage(t) / pSM_% Lepv(t), 1.0) * pSM_% porosity, pSM_% ks)
        ELSE
            pSM_% kUS_exf = pSM_% kUS_exf - MAX(pSM_% exfiltration, 0.0)
            ! TODO: replace kUS_exf with exf_capacity and kUS_inf with inf_capacity
        END IF

        pSM_% exfiltration = MIN(pSM_% Lstorage(t) - pSM_% EQstorage, pSM_% kUS_exf * dt) * pSM_% IC_ratio
        IF(pSM_% Lstorage(t) > pSM_% Lepv(t)) pSM_% exfiltration = pSM_% exfiltration + (pSM_% Lstorage(t) - pSM_% Lepv(t))
        pSM_% Lstorage(t) = pSM_% Lstorage(t) - pSM_% exfiltration

        IF(pSM_% gw_bound) EXIT
        ! TODO: limit exfiltration to (GWS - BOT) * porosity if GWS bnd and GWS - exf < BOT and make appropriate changes in solve()
        
        smn = smn + 1
    END DO

    UZ% Lstorage(e,time% Lts) = 0.0
    DO smn = 1, self% gws_bnd_smid
        IF (self% SM(smn)% isactive) THEN
            UZ% Lstorage(e,time% Lts) = UZ% Lstorage(e,time% Lts) + self% SM(smn)% Lstorage(t)
        END IF
    END DO

END SUBROUTINE solve
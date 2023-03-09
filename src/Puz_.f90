SUBROUTINE activate(pSM_, nts, gw_bound, sm1)
    TYPE(Csm), POINTER, INTENT(INOUT) :: pSM_

    INTEGER(INT32), INTENT(IN), OPTIONAL :: nts
    LOGICAL, INTENT(IN), OPTIONAL :: gw_bound, sm1

    ALLOCATE(pSM_% Lepv, pSM_% RWubound, pSM_% RWlbound, pSM_% EQstorage, pSM_% exfiltration, &
        pSM_% kUS_inf, pSM_% kUS_exf, pSM_% IC, pSM_% IC_ratio)

    IF(PRESENT(nts)) ALLOCATE(pSM_% Lstorage(nts+1))

    IF(PRESENT(sm1)) THEN
        IF(sm1) ALLOCATE(pSM_% infiltration)
    END IF

    pSM_% isactive = .TRUE.

    IF(PRESENT(gw_bound)) pSM_% gw_bound = gw_bound

END SUBROUTINE activate


SUBROUTINE deactivate(pSM_, gw_bound, sm1)
    TYPE(Csm), POINTER, INTENT(INOUT) :: pSM_

    LOGICAL, INTENT(IN), OPTIONAL :: gw_bound, sm1

    DEALLOCATE(pSM_% Lstorage, pSM_% Lepv, pSM_% RWubound, pSM_% RWlbound, pSM_% EQstorage, pSM_% exfiltration, &
        pSM_% kUS_inf, pSM_% kUS_exf, pSM_% IC, pSM_% IC_ratio)

    IF(PRESENT(sm1)) THEN
        IF(sm1) DEALLOCATE(pSM_% infiltration)
    END IF

    pSM_% isactive = .FALSE.
    
    IF(PRESENT(gw_bound)) pSM_% gw_bound = gw_bound

END SUBROUTINE deactivate




SUBROUTINE init(self, e, UZ, GW, time)
    ! USE Mstorages, ONLY: Cuz, Cuz_
    USE Mtiming, ONLY : Ctime

    CLASS(Cuz_), INTENT(INOUT) :: self
    INTEGER(INT32), INTENT(IN) :: e
    TYPE(Cuz), INTENT(INOUT) :: UZ
    TYPE(Cgw), INTENT(IN) :: GW
    TYPE(Ctime), INTENT(IN) :: time

    INTEGER(INT8) :: ln, smn, tmpcnt
    LOGICAL :: check

    TYPE(Csm), POINTER :: pSM_
    INTEGER(INT32), POINTER :: t

    CHARACTER(LEN=STRLEN) :: strbuffer

    t => time% Lts

    UZ% Gstorage(e,1) = 0.0

    CALL plogger_Mstorages% log(plogger_Mstorages% TRACE, "Initializing SM layers")

    ! calculate and allocate the number oƒ active SM layers for element e
    self% nlay = 0
    DO ln = 1, UZ% nlay
        IF (UZ% layer(ln)% isactive(e)) THEN
            self% nlay = self% nlay + 1
        END IF
    END DO
    ALLOCATE(self% SM(self% nlay))
    WRITE(strbuffer, *) "Number of active SM layers for element ", e, " is ", self% nlay
    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, strbuffer)

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

        CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Allocating Global SM storages")
        ALLOCATE(pSM_% Gstorage(time% Gnts+1))

        ! set the UZ properties for each SM layer
        CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Setting UZ properties (vanG, ks, and bounds) for SM ", smn)
        pSM_% vanG => UZ% layer(pSM_% lid)% vanG
        pSM_% ks => UZ% layer(pSM_% lid)% ks(e)
        pSM_% porosity => UZ% layer(pSM_% lid)% porosity(e)

        pSM_% ADlbound => UZ% layer(pSM_% lid)% Albound(e)
        pSM_% ADubound => UZ% layer(pSM_% lid)% Aubound(e)

        IF (GW% Gstorage(e,1) < pSM_% ADubound) THEN
        ! activate the layer if the GW storage is less than the Aubound of the layer i.e. GWS lies in or under the layer
            IF(smn == 1) check = .TRUE.
            CALL activate(pSM_, sm1=check)
            CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "SM ", smn, " is active")
            
            ! calculate the relative bounds for SMeq calculation
            pSM_% RWubound = GW% Gstorage(e,1) - pSM_% ADubound ! ub = GWS - L_Aub
            pSM_% RWlbound = GW% Gstorage(e,1) - MAX(GW% Gstorage(e,1), pSM_% ADlbound) ! ub = GWS - MAX(GWS, L_Alb)
            CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Rbounds: ", pSM_% RWubound, pSM_% RWlbound)

            ! set the initial condition for each SM layer to SMeq
            CALL pSM_% vanG% setvars()
            pSM_% Gstorage(1) = pSM_% vanG% integrate(pSM_% RWubound, pSM_% RWlbound)
            CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Setting SM_ini. SMeq = ", pSM_% Gstorage(1))

            UZ% Gstorage(e,1) = UZ% Gstorage(e,1) + pSM_% Gstorage(1)

            IF (GW% Gstorage(e,1) > pSM_% ADlbound .OR. GW% Gstorage(e,1) == pSM_% ADlbound) THEN
            ! set the GW bound flag if the GWS lies above the Albound of the layer i.e. GWS lies within the layer and skip checking the underlying SM layers
                self% Albound => GW% Gstorage(e,1)
                pSM_% gw_bound = .TRUE.
                self% gws_bnd_smid = smn
                CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "SM layer is GW bound")
            END IF
        ELSE
            pSM_% Gstorage(1) = 0.0
            CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "SM ", smn, " is inactive")
        END IF
    END DO

    ! calculate the UZ storage for this element by summing all active SM storages
    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "UZ ini = ", UZ% Gstorage(e,1))

    ! set the bounds of UZ and calculate its thickness if UZ is active for element e
    IF (.NOT. self% SM(1)% isactive) THEN
        self% isactive = .FALSE.
        CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "UZ is inactive")
    ELSE
        self% isactive = .TRUE.
        CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "UZ is active")
    END IF

END SUBROUTINE init


! Lstorage and Lepv are ALLOCATED in init_ts() if SM(i)% isactive
SUBROUTINE resolve(self, e, UZ, GW, time, solver_settings)
    !USE Mstorages, ONLY: Cuz, Cuz_
    USE Mtiming, ONLY : Ctime
    USE iso_fortran_env, ONLY : INT32

    CLASS(Cuz_), INTENT(INOUT) :: self
    INTEGER(INT32), INTENT(IN) :: e
    TYPE(Cuz), INTENT(INOUT) :: UZ
    TYPE(Cgw), INTENT(INOUT) :: GW
    TYPE(Ctime), INTENT(IN) :: time
    TYPE(Csettings), INTENT(IN) :: solver_settings

    INTEGER(INT8) :: smn
    INTEGER(INT16) :: itr
    REAL(REAL128) :: prev_sm_storage
    LOGICAL :: check

    TYPE(Csm), POINTER :: pSM_
    INTEGER(INT32), POINTER :: t

    t => time% Lts
! write(*,*) "in resolve"
    CALL plogger_Mstorages% log(plogger_Mstorages% TRACE, "*** in UZ_resolve ***")
    CALL plogger_Mstorages% log(plogger_Mstorages% TRACE, "Resolving GW bound SM layer: e,t = ", e, t-1)
! write(*,*) time% Gts
    check = GW% Lstorage(e,t) > UZ% top(e) .OR. GW% Lstorage(e,t) == UZ% top(e)
    IF(check) THEN
write(*,*) "GW storage is greater than UZ top", GW% Lstorage(e,t), t, time% Gts
        DO smn = 1, self% gws_bnd_smid
            pSM_ => self% SM(smn)

            IF(pSM_% isactive) THEN
                GW% Lstorage(e,t) = GW% Lstorage(e,t) + (pSM_% Lstorage(t) / pSM_% porosity)
            END IF

            IF(smn == 1) check = .TRUE.
            CALL deactivate(pSM_, sm1=check)

            self% isactive = .FALSE.
write(*,*) "UZ is not active anymore", self% isactive
            UZ% Lstorage(e,t) = 0.0
            UZ% Lepv(e,t) = 0.0

            CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "UZ is not active anymore")

            RETURN
        END DO
    END IF

    pSM_ => self% SM(self% gws_bnd_smid)
    check = ((GW% Lstorage(e,t) < pSM_% ADubound) .OR. GW% Lstorage(e,t) == pSM_% ADubound) .AND. ((GW% Lstorage(e,t) > pSM_% ADlbound) .OR. GW% Lstorage(e,t) == pSM_% ADlbound)

    IF(check) THEN
    ! if GWS lies within the bounds of the gws_bnd layer
        pSM_% RWubound = GW% Lstorage(e,t) - pSM_% ADubound ! ub = GWS - L_Aub
        pSM_% RWlbound = GW% Lstorage(e,t) - MAX(GW% Lstorage(e,t), pSM_% ADlbound) ! ub = GWS - L_Alb
        pSM_% Lepv = ABS(pSM_% RWubound - pSM_% RWlbound) * pSM_% porosity

        CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Rbounds: ", pSM_% RWubound, pSM_% RWlbound)
        CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Lstorage = ", pSM_% Lstorage(t))
        CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "ePV = ", pSM_% Lepv)
    END IF

    IF (.NOT. check) THEN
    ! if GWS does NOT lie within gws_bnd_smid layer
        CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "GWS does not lie within the previously flaagged layer - SM", self% gws_bnd_smid)
        CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "GW = ", GW% Lstorage(e,t))
        CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "GW_bnd SM Abounds ", pSM_% ADubound, pSM_% ADlbound)
write(*,*) "GW = ", GW% Lstorage(e,t)
write(*,*) "GW_bnd SM Abounds ", pSM_% ADubound, pSM_% ADlbound

        IF (GW% Lstorage(e,t) < pSM_% ADlbound) THEN
        ! if GWS lies under gws_bnd layer, i.e. GWS is falling
            CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "GWS is falling")
write(*,*) "GWS is falling", GW% Lstorage(e,t), t, time% Gts
            ! recalculate the bounds of the previously GW bound layer and deactivate the gw_bound flag
            pSM_% RWubound = GW% Lstorage(e,t) - pSM_% ADubound ! ub = GWS - L_Aub
            pSM_% RWlbound = GW% Lstorage(e,t) - MAX(GW% Lstorage(e,t), pSM_% ADlbound) ! ub = GWS - L_Alb
            CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Rbounds: ", pSM_% RWubound, pSM_% RWlbound)
            pSM_% gw_bound = .FALSE.
            pSM_% Lepv = ABS(pSM_% RWubound - pSM_% RWlbound) * pSM_% porosity
            CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Lepv = ", pSM_% Lepv)
        
            ! activate the underlying layers until the GW bound layer is discovered, set the bounds, set the GW bound flag, and calculate the respective storages
            CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Activating underlying layers until GW bound layer is discovered")
            DO smn = self% gws_bnd_smid+1, self% nlay
                pSM_ => self% SM(smn)

                IF(smn == 1) check = .TRUE.
                CALL activate(pSM_, nts=time% Lnts, sm1=check)
                CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "SM", smn, " is active. Allocating SM vars")

                pSM_% RWubound = GW% Lstorage(e,t) - pSM_% ADubound ! ub = GWS - L_Aub
                pSM_% RWlbound = GW% Lstorage(e,t) - MAX(GW% Lstorage(e,t), pSM_% ADlbound) ! ub = GWS - MAX(GWS, L_Alb)
                CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Rbounds: ", pSM_% RWubound, pSM_% RWlbound)

                pSM_% Lepv = ABS(pSM_% RWubound - pSM_% RWlbound) * pSM_% porosity
                CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Lepv = ", pSM_% Lepv)
! write(*,*) "here" 
                IF (GW% Lstorage(e,t) > pSM_% ADlbound) THEN
                    pSM_% gw_bound = .TRUE.
                    self% gws_bnd_smid = smn
                    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "SM", smn, " is GW bound")
! write(*,*) "here2" 
                    CALL pSM_% vanG% setvars()
! write(*,*) "Rbounds: ", pSM_% RWubound, pSM_% RWlbound
                    pSM_% Lstorage(t) = pSM_% vanG% integrate(pSM_% RWubound, pSM_% RWlbound)
                    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "SMeq = ", pSM_% Lstorage(t))
! write(*,*) "here3" 
                    ! iteratively calculate the storage of the layer until the consecutive change storage is within the tolerance
                    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "balancing GW-SM storages")
                    itr = 0
                    prev_sm_storage = 0.0
! write(*,*) "itr = ", itr, "max_iterations", solver_settings% max_iterations
                    DO WHILE((itr < solver_settings% max_iterations) .AND. (ABS(pSM_% Lstorage(t) - prev_sm_storage) > solver_settings% sm_gw_fluctuation_tolerance))
! write(*,*) "itr = ", itr
                        GW% Lstorage(e,t) = GW% Lstorage(e,t) - (pSM_% Lstorage(t) - prev_sm_storage) / pSM_% porosity
                        ! IF (.NOT. GW% Lstorage(e,t) > pSM_% ADlbound) CALL self% resolve(e, UZ, GW, time, solver_settings)
                        pSM_% RWlbound = GW% Lstorage(e,t) - MAX(GW% Lstorage(e,t), pSM_% ADlbound)
                        pSM_% RWubound = GW% Lstorage(e,t) - pSM_% ADubound
                        pSM_% Lstorage(t) = pSM_% vanG% integrate(pSM_% RWubound, pSM_% RWlbound)
                        prev_sm_storage = pSM_% Lstorage(t)
                        itr = itr + 1
                    END DO
! write(*,*) "here4" 
                    IF(t /= 1) pSM_% Lstorage(t-1) = pSM_% Lstorage(t)

                    pSM_% Lepv = ABS(pSM_% RWubound - pSM_% RWlbound) * pSM_% porosity
                    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Rbounds: ", pSM_% RWubound, pSM_% RWlbound)
                    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Lstorage = ", pSM_% Lstorage(t))
                    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "GWS = ", GW% Lstorage(e,t))
                    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "ePV = ", pSM_% Lepv)
                    EXIT ! exit SM scanning DO loop
                ELSE
                    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "SM", smn, " is not GW bound")

                    CALL pSM_% vanG% setvars()
                    pSM_% Lstorage(t) = pSM_% vanG% integrate(pSM_% RWubound, pSM_% RWlbound)
                    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "SMeq = ", pSM_% Lstorage(t))

                    ! iteratively calculate the storage of the layer until the consecutive change storage is within the tolerance
                    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "balancing GW-SM storages")
                    prev_sm_storage = 0.0
                    itr = 0
                    check = (itr < solver_settings% max_iterations) .AND. (ABS(pSM_% Lstorage(t) - prev_sm_storage) > solver_settings% sm_gw_fluctuation_tolerance)
                    DO WHILE(check)
                        GW% Lstorage(e,t) = GW% Lstorage(e,t) - (pSM_% Lstorage(t) - prev_sm_storage) / pSM_% porosity
                        IF (.NOT. GW% Lstorage(e,t) > pSM_% ADlbound) CALL self% resolve(e, UZ, GW, time, solver_settings)
                        pSM_% RWlbound = GW% Lstorage(e,t) - MAX(GW% Lstorage(e,t), pSM_% ADlbound)
                        pSM_% RWubound = GW% Lstorage(e,t) - pSM_% ADubound
                        pSM_% Lstorage(t) = pSM_% vanG% integrate(pSM_% RWubound, pSM_% RWlbound)
                        prev_sm_storage = pSM_% Lstorage(t)
                        itr = itr + 1
                    END DO

                    pSM_% Lepv = ABS(pSM_% RWubound - pSM_% RWlbound) * pSM_% porosity
                    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Rbounds: ", pSM_% RWubound, pSM_% RWlbound)
                    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Lstorage = ", pSM_% Lstorage(t))
                    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "GWS = ", GW% Lstorage(e,t))
                    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "ePV = ", pSM_% Lepv)
                END IF
            END DO

        ELSE
        ! if GWS lies above gws_bnd layer, i.e. GWS is rising
            CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "GWS is rising")
write(*,*) "GWS is rising"
            pSM_ => self% SM(self% gws_bnd_smid)
            ! deactivate the the previously GW bound layer
            GW% Lstorage(e,t) = GW% Lstorage(e,t) + (pSM_% Lstorage(t) / pSM_% porosity)

            IF(smn == 1) check = .TRUE.
            CALL deactivate(pSM_, gw_bound=.FALSE., sm1=check)

            CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "SM", self% gws_bnd_smid, " is not active anymore")
            CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Transferred SM storage to GW and deallocated SM vars for SM", self% gws_bnd_smid)

            ! deactivate underlying layers until GW bound layer is discovered and flagged
            CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Deactivating underlying layers until GW bound layer is discovered")
            DO smn = self% gws_bnd_smid-1, 1, -1
                pSM_ => self% SM(smn)

                IF (GW% Lstorage(e,t) < pSM_% ADubound) THEN
                    pSM_% gw_bound = .TRUE.
                    self% gws_bnd_smid = smn
                    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "SM", smn, " is now GW bound")
! write(*,*) "*"
                    pSM_% RWubound = GW% Lstorage(e,t) - pSM_% ADubound ! ub = GWS - L_Aub
                    pSM_% RWlbound = GW% Lstorage(e,t) - MAX(GW% Lstorage(e,t), pSM_% ADlbound) ! ub = GWS - MAX(GWS, L_Alb)
                    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Rbounds ", pSM_% RWubound, pSM_% RWlbound)

                    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "SMeq = ", pSM_% Lstorage(t))

                    pSM_% Lepv = ABS(pSM_% RWubound - pSM_% RWlbound) * pSM_% porosity
                    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "ePV = ", pSM_% Lepv)

                    IF(pSM_% Lstorage(t) > pSM_% Lepv) THEN
                        CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "SM > ePV! Transferring excess storage to GW and balancing GW-SM storages")
                        prev_sm_storage = 0.0
                        itr = 0
                        DO WHILE((itr < solver_settings% max_iterations) .AND. (ABS(pSM_% Lstorage(t) - prev_sm_storage) > solver_settings% sm_gw_fluctuation_tolerance))
                            GW% Lstorage(e,t) = GW% Lstorage(e,t) + ((pSM_% Lstorage(t) - prev_sm_storage) - pSM_% Lepv) / pSM_% porosity
                            pSM_% Lstorage(t) = pSM_% Lepv
                            IF (.NOT. GW% Lstorage(e,t) < pSM_% ADubound) CALL self% resolve(e, UZ, GW, time, solver_settings)
                            pSM_% RWubound = GW% Lstorage(e,t) - pSM_% ADubound
                            pSM_% RWlbound = GW% Lstorage(e,t) - MAX(GW% Lstorage(e,t), pSM_% ADlbound)
                            pSM_% Lepv = ABS(pSM_% RWubound - pSM_% RWlbound) * pSM_% porosity
                            itr = itr + 1
                        END DO
                        CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Rbounds: ", pSM_% RWubound, pSM_% RWlbound)
                        CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Lstorage = ", pSM_% Lstorage(t))
                        CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "GWS = ", GW% Lstorage(e,t))
                        CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "ePV = ", pSM_% Lepv)
                    END IF
                    EXIT
                ELSE
                    GW% Lstorage(e,t) = GW% Lstorage(e,t) + (pSM_% Lstorage(t) / pSM_% porosity)
write(*,*) "***"
                    IF(smn == 1) check = .TRUE.
                    CALL deactivate(pSM_, gw_bound=.FALSE., sm1=check)

                    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "SM", smn, " is not active anymore")
                END IF
            END DO
        END IF
    END IF

! set the bounds of UZ and calculate its thickness if UZ is active for element e
    IF (.NOT. self% SM(1)% isactive) THEN
        self% isactive = .FALSE.
        UZ% Lstorage(e,t) = 0.0
        UZ% Lepv(e,t) = 0.0
        CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "UZ is not active anymore")
    ELSE
        self% isactive = .TRUE.
        CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "UZ is still active")
        
        IF(.NOT. ALLOCATED(self% SM(1)% infiltration)) ALLOCATE(self% SM(1)% infiltration)

        UZ% Lstorage(e,t) = 0.0
        UZ% Lepv(e,t) = 0.0
        DO smn = 1, self% gws_bnd_smid
            IF (self% SM(smn)% isactive) THEN
                UZ% Lstorage(e,t) = UZ% Lstorage(e,t) + self% SM(smn)% Lstorage(t)
                UZ% Lepv(e,t) = UZ% Lepv(e,t) + self% SM(smn)% Lepv
            END IF
        END DO

        CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "UZ Lstorage = ", UZ% Lstorage(e,t))
        CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "UZ Lepv = ", UZ% Lepv(e,t))
    END IF
    self% Albound => GW% Lstorage(e,t)

    CALL plogger_Mstorages% log(plogger_Mstorages% TRACE, "*** leaving UZ_solve ***")

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

    CALL plogger_Mstorages% log(plogger_Mstorages% TRACE, "*** in UZ_solve ***")
    CALL plogger_Mstorages% log(plogger_Mstorages% TRACE, "Solving GW bound SM layer: e,t = ", e, t-1)

    smn = 1
    pSM_ => self% SM(smn)
    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "** SM = ", smn, " **")

    CALL pSM_% vanG% setvars()
    pSM_% EQstorage = pSM_% vanG% integrate(pSM_% RWubound, pSM_% RWlbound)
    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "SMeq = ", pSM_% EQstorage)

    IF(first_run) THEN
        IF(pSM_% infiltration /= 0.0) THEN
            pSM_% IC = MAX(pSM_% IC + pSM_% kUS_inf * dt, 0.0)
        ELSE
            pSM_% IC = MAX(pSM_% IC - pSM_% kUS_inf * dt, 0.0)
        END IF

        pSM_% IC_ratio = MIN(1.0, MAX(pSM_% IC / ABS(pSM_% RWubound - pSM_% RWlbound), pSM_% vanG% theta_r))

        pSM_% kUS_exf = pSM_% vanG% kUS((pSM_% Lstorage(t) / pSM_% Lepv) * pSM_% porosity, pSM_% ks)
    ELSE
        pSM_% kUS_exf = MAX(pSM_% kUS_exf - MAX(pSM_% exfiltration, 0.0), 0.0)
    END IF

    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "IC, IC_ratio = ", pSM_% IC, pSM_% IC_ratio)
    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "kUS_exf = ", pSM_% kUS_exf)
    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Lstorage was ", pSM_% Lstorage(t))

    pSM_% exfiltration = MIN((pSM_% Lstorage(t) - pSM_% EQstorage) * pSM_% IC_ratio, (pSM_% kUS_exf * dt) * pSM_% IC_ratio, (pSM_% Lstorage(t) - pSM_% EQstorage)) ! negative exfiltration (i.e. infiltration from underlying layers) is not capped to always allow replenishment of SM until SMeq is reached as it is based on relatively instanteous capilary forces

    pSM_% Lstorage(t) = pSM_% Lstorage(t) - pSM_% exfiltration

    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "exfiltration = ", pSM_% exfiltration)
    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Lstorage is ", pSM_% Lstorage(t))
    
    DO smn = 2, self% gws_bnd_smid
        pSM_ => self% SM(smn)
        pSM_prev_ => self% SM(smn-1)
        CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "** SM = ", smn, " **")
        CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Lstorage was ", pSM_% Lstorage(t-1))

        pSM_% Lstorage(t) = pSM_% Lstorage(t-1) + pSM_prev_% exfiltration
        CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Lstorage after infiltration (exf. from prev.) is ", pSM_% Lstorage(t))

        CALL pSM_% vanG% setvars()
        pSM_% EQstorage = pSM_% vanG% integrate(pSM_% RWubound, pSM_% RWlbound)
        CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "SMeq = ", pSM_% EQstorage)

        IF(first_run) THEN
            pSM_% kUS_inf = pSM_% vanG% kUS((pSM_% Lstorage(t-1) / pSM_% Lepv) * pSM_% porosity, pSM_% ks)

            IF(pSM_prev_% exfiltration > 0.0) THEN
                pSM_% IC = MAX(pSM_% IC + pSM_% kUS_inf * dt, 0.0)
            ELSE
                pSM_% IC = MAX(pSM_% IC - pSM_% kUS_inf * dt, 0.0)
            END IF

            pSM_% IC_ratio = MIN(1.0, MAX(pSM_% IC / ABS(pSM_% RWubound - pSM_% RWlbound), pSM_% vanG% theta_r))

            pSM_% kUS_exf = pSM_% vanG% kUS((pSM_% Lstorage(t) / pSM_% Lepv) * pSM_% porosity, pSM_% ks)
        ELSE
            pSM_% kUS_exf = MAX(pSM_% kUS_exf - MAX(pSM_% exfiltration, 0.0), 0.0)
            ! TODO: replace kUS_exf with exf_capacity and kUS_inf with inf_capacity
        END IF

        CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "IC, IC_ratio = ", pSM_% IC, pSM_% IC_ratio)
        CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "kUS_exf = ", pSM_% kUS_exf)


        pSM_% exfiltration = MIN((pSM_% Lstorage(t) - pSM_% EQstorage) * pSM_% IC_ratio, (pSM_% kUS_exf * dt) * pSM_% IC_ratio, (pSM_% Lstorage(t) - pSM_% EQstorage)) ! negative exfiltration (i.e. infiltration from underlying layers) is not capped to always allow replenishment of SM until SMeq is reached as it is based on relatively instanteous capilary forces
        IF(pSM_% Lstorage(t) > pSM_% Lepv) pSM_% exfiltration = pSM_% exfiltration + (pSM_% Lstorage(t) - pSM_% Lepv)

        pSM_% Lstorage(t) = pSM_% Lstorage(t) - pSM_% exfiltration

        CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "exfiltration = ", pSM_% exfiltration)
        CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Lstorage is ", pSM_% Lstorage(t))

        IF(pSM_% gw_bound) EXIT
        ! TODO: limit exfiltration to (GWS - BOT) * porosity if GWS bnd and GWS - exf < BOT and make appropriate changes in solve()

    END DO

    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "UZ Lstorage was ", UZ% Lstorage(e,t-1))
! write(*,*) "calc UZS"
    UZ% Lstorage(e,t) = 0.0
    DO smn = 1, self% gws_bnd_smid
        IF (self% SM(smn)% isactive) THEN
            UZ% Lstorage(e,t) = UZ% Lstorage(e,t) + self% SM(smn)% Lstorage(t)
        END IF
    END DO
! write(*,*) "done calc UZS"
    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "UZ Lstorage is ", UZ% Lstorage(e,t))
    CALL plogger_Mstorages% log(plogger_Mstorages% TRACE, "*** leaving UZ_solve ***")

END SUBROUTINE solve
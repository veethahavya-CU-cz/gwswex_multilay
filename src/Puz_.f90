SUBROUTINE activate(pSM_, nts, gw_bound, sm1)
    TYPE(Csm), POINTER, INTENT(INOUT) :: pSM_

    INTEGER(INT32), INTENT(IN), OPTIONAL :: nts
    LOGICAL, INTENT(IN), OPTIONAL :: gw_bound, sm1

    ALLOCATE(pSM_% Lepv, pSM_% RWubound, pSM_% RWlbound, pSM_% EQstorage, pSM_% exfiltration, &
        pSM_% inf_cap, pSM_% exf_cap, pSM_% IC, pSM_% IC_ratio)

    IF(PRESENT(nts)) ALLOCATE(pSM_% Lstorage(nts+1))

    IF(PRESENT(sm1)) THEN
        IF(sm1 .AND. (.NOT. ALLOCATED(pSM_% infiltration))) ALLOCATE(pSM_% infiltration)
    END IF

    pSM_% isactive = .TRUE.

    IF(PRESENT(gw_bound)) pSM_% gw_bound = gw_bound

END SUBROUTINE activate


SUBROUTINE deactivate(pSM_, gw_bound, sm1)
    TYPE(Csm), POINTER, INTENT(INOUT) :: pSM_

    LOGICAL, INTENT(IN), OPTIONAL :: gw_bound, sm1

    DEALLOCATE(pSM_% Lstorage, pSM_% Lepv, pSM_% RWubound, pSM_% RWlbound, pSM_% EQstorage, pSM_% exfiltration, &
        pSM_% inf_cap, pSM_% exf_cap, pSM_% IC, pSM_% IC_ratio)

    IF(PRESENT(sm1)) THEN
        IF(sm1 .AND. (ALLOCATED(pSM_% infiltration))) DEALLOCATE(pSM_% infiltration)
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

    CHARACTER(LEN=STRLEN) :: strbuffer

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

            pSM_% IC = pSM_% vanG% theta_r
            pSM_% IC_ratio = pSM_% IC / ABS(pSM_% RWubound - pSM_% RWlbound)

            UZ% Gstorage(e,1) = UZ% Gstorage(e,1) + pSM_% Gstorage(1)

            IF (GW% Gstorage(e,1) > pSM_% ADlbound .OR. GW% Gstorage(e,1) == pSM_% ADlbound) THEN
            ! set the GW bound flag if the GWS lies above the Albound of the layer i.e. GWS lies within the layer and skip checking the underlying SM layers
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
SUBROUTINE resolve(self, e, t, UZ, GW, SW, time, SS)
    !USE Mstorages, ONLY: Cuz, Cuz_
    USE Mtiming, ONLY : Ctime
    USE iso_fortran_env, ONLY : INT32

    CLASS(Cuz_), INTENT(INOUT) :: self
    INTEGER(INT32), INTENT(IN) :: e, t
    TYPE(Cuz), INTENT(INOUT) :: UZ
    TYPE(Cgw), INTENT(INOUT) :: GW
    TYPE(Csw), INTENT(INOUT) :: SW
    TYPE(Ctime), INTENT(IN) :: time ! #TODO: remove after removing comments
    TYPE(Csettings), INTENT(IN) :: SS

    INTEGER(INT8) :: smn
    INTEGER(INT16) :: itr
    REAL(REAL128) :: prev_sm_storage
    LOGICAL :: check

    TYPE(Csm), POINTER :: pSM_
! #TODO: calculate thickness of SM and store in a var; reduces calcs for epv and others

! !write(*,*) "in resolve"
    CALL plogger_Mstorages% log(plogger_Mstorages% TRACE, "*** in UZ_resolve ***")
    CALL plogger_Mstorages% log(plogger_Mstorages% TRACE, "Resolving GW bound SM layer: e,t = ", e, t-1)
! !write(*,*) time% Gts
    check = GW% Lstorage(e,t) > UZ% top(e) .OR. GW% Lstorage(e,t) == UZ% top(e)
    IF(check) THEN
! !write(*,*) "GW storage is greater than UZ top", GW% Lstorage(e,t), t, time% Gts
        DO smn = self% gws_bnd_smid, 1, -1
            pSM_ => self% SM(smn)
            IF(pSM_% isactive) THEN
! write(*,*) "SM is active", smn
                GW% Lstorage(e,t) = GW% Lstorage(e,t) + (pSM_% Lstorage(t) / pSM_% porosity)
! IF(ABS(GW% Lstorage(e,t) - GW% Lstorage(e,t-1)) > 0.25) write(*,*) "*1", time% Gts, time% Lts, GW% Lstorage(e,t-1), GW% Lstorage(e,t)
                CALL deactivate(pSM_, sm1=(smn == 1))
! write(*,*) "SM is not active anymore", smn, pSM_% isactive
            END IF
        END DO

        self% isactive = .FALSE.
! write(*,*) "UZ is not active anymore", self% isactive
        UZ% Lstorage(e,t) = 0.0
        UZ% Lepv(e,t) = 0.0
! !write(*,*) self% SM(1)% porosity
! !write(*,*) GW% Lstorage(e,t), UZ% top(e)
        IF(GW% Lstorage(e,t) > UZ% top(e)) THEN
            SW% Lstorage(e,t) = SW% Lstorage(e,t) + (GW% Lstorage(e,t) - UZ% top(e)) * self% SM(1)% porosity ! #TODO: check if this is correct; considering possible layer, much like GW discharge calc
            GW% Lstorage(e,t) = UZ% top(e)
        END IF

        CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "UZ is not active anymore")
! write(*,*) "UZ is not active anymore", self% isactive
        RETURN
! write(*,*) "not returning"
    END IF

    IF(check .AND. (.NOT. self% isactive)) RETURN
! write(*,*) "not returning"
    pSM_ => self% SM(self% gws_bnd_smid)
    check = ((GW% Lstorage(e,t) < pSM_% ADubound) .OR. GW% Lstorage(e,t) == pSM_% ADubound) .AND. ((GW% Lstorage(e,t) > pSM_% ADlbound) .OR. GW% Lstorage(e,t) == pSM_% ADlbound)

    IF(check) THEN
    ! if GWS lies within the bounds of the gws_bnd layer
        pSM_ => self% SM(self% gws_bnd_smid)

        IF(.NOT. pSM_% isactive) CALL activate(pSM_, sm1=(self% gws_bnd_smid == 1), gw_bound=.TRUE., nts=time% Lnts) ! #TODO: set Lstorage to SMeq and balance
        
        pSM_% RWubound = GW% Lstorage(e,t) - pSM_% ADubound ! ub = GWS - L_Aub
        pSM_% RWlbound = GW% Lstorage(e,t) - MAX(GW% Lstorage(e,t), pSM_% ADlbound) ! ub = GWS - L_Alb
        pSM_% Lepv = ABS(pSM_% RWubound - pSM_% RWlbound) * pSM_% porosity

        CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Rbounds: ", pSM_% RWubound, pSM_% RWlbound)
        CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Lstorage = ", pSM_% Lstorage(t))
        CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "ePV = ", pSM_% Lepv)

        DO smn = self% gws_bnd_smid-1, 1, -1
            pSM_ => self% SM(smn)
            pSM_% RWubound = GW% Lstorage(e,t) - pSM_% ADubound ! ub = GWS - L_Aub
            pSM_% RWlbound = GW% Lstorage(e,t) - pSM_% ADlbound ! ub = GWS - L_Alb
            pSM_% Lepv = ABS(pSM_% RWubound - pSM_% RWlbound) * pSM_% porosity
        END DO
    END IF

    IF (.NOT. check) THEN
    ! if GWS does NOT lie within gws_bnd_smid layer
        CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "GWS does not lie within the previously flaagged layer - SM", self% gws_bnd_smid)
        CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "GW = ", GW% Lstorage(e,t))
        CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "GW_bnd SM Abounds ", pSM_% ADubound, pSM_% ADlbound)
!write(*,*) "GW = ", GW% Lstorage(e,t)
!write(*,*) "GW_bnd SM Abounds ", pSM_% ADubound, pSM_% ADlbound

        IF (GW% Lstorage(e,t) < pSM_% ADlbound) THEN
        ! if GWS lies under gws_bnd layer, i.e. GWS is falling
            CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "GWS is falling")
!write(*,*) "GWS is falling", GW% Lstorage(e,t), t, time% Gts
            pSM_ => self% SM(self% gws_bnd_smid)
            IF(.NOT. pSM_% isactive) CALL activate(pSM_, sm1=(self% gws_bnd_smid == 1), gw_bound=.TRUE., nts=time% Lnts) ! #TODO: set Lstorage to SMeq and balance
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

                CALL activate(pSM_, nts=time% Lnts, sm1=(smn == 1))
                CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "SM", smn, " is active. Allocating SM vars")

                pSM_% RWubound = GW% Lstorage(e,t) - pSM_% ADubound ! ub = GWS - L_Aub
                pSM_% RWlbound = GW% Lstorage(e,t) - MAX(GW% Lstorage(e,t), pSM_% ADlbound) ! ub = GWS - MAX(GWS, L_Alb)
                CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Rbounds: ", pSM_% RWubound, pSM_% RWlbound)

                pSM_% Lepv = ABS(pSM_% RWubound - pSM_% RWlbound) * pSM_% porosity
                CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Lepv = ", pSM_% Lepv)
! !write(*,*) "here" 
                IF (GW% Lstorage(e,t) > pSM_% ADlbound) THEN
                    pSM_% gw_bound = .TRUE.
                    self% gws_bnd_smid = smn
                    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "SM", smn, " is GW bound")
! !write(*,*) "here2" 
! write(*,*) "sm, gw", pSM_% Lstorage(t), GW% Lstorage(e,t)
                    CALL pSM_% vanG% setvars()
! !write(*,*) "Rbounds: ", pSM_% RWubound, pSM_% RWlbound
                    pSM_% Lstorage(t) = pSM_% vanG% integrate(pSM_% RWubound, pSM_% RWlbound)
                    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "SMeq = ", pSM_% Lstorage(t))

                    GW% Lstorage(e,t) = GW% Lstorage(e,t) - (pSM_% Lstorage(t) / pSM_% porosity) 
                    IF (.NOT. GW% Lstorage(e,t) > pSM_% ADlbound) CALL self% resolve(e, t, UZ, GW, SW, time, SS)
                    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "GW = ", GW% Lstorage(e,t))
! !write(*,*) "here3" 
! write(*,*) "sm, gw", pSM_% Lstorage(t), GW% Lstorage(e,t)
                    ! iteratively calculate the storage of the layer until the consecutive change storage is within the tolerance
!                     CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "balancing GW-SM storages")
!                     itr = 1
!                     prev_sm_storage = 0.0
! ! !write(*,*) "itr = ", itr, "max_iterations", SS% max_iterations
!                     ! #TODO: VERIFY THIS
!                     DO WHILE((itr < SS% max_iterations) .AND. (ABS(pSM_% Lstorage(t) - prev_sm_storage) > SS% sm_gw_fluctuation_tolerance))
! ! write(*,*) "itr = ", itr, ABS(pSM_% Lstorage(t) - prev_sm_storage)
! ! write(*,*) "sm, gw", pSM_% Lstorage(t), GW% Lstorage(e,t)
!                         pSM_% RWlbound = GW% Lstorage(e,t) - MAX(GW% Lstorage(e,t), pSM_% ADlbound)
!                         pSM_% RWubound = GW% Lstorage(e,t) - pSM_% ADubound
!                         prev_sm_storage = pSM_% Lstorage(t)
!                         pSM_% Lstorage(t) = pSM_% vanG% integrate(pSM_% RWubound, pSM_% RWlbound)
!                         GW% Lstorage(e,t) = GW% Lstorage(e,t) - (pSM_% Lstorage(t) - prev_sm_storage) / pSM_% porosity
!                         IF (.NOT. GW% Lstorage(e,t) > pSM_% ADlbound) CALL self% resolve(e, t, UZ, GW, SW, time, SS)
!                         itr = itr + 1
! ! write(*,*) ABS(pSM_% Lstorage(t) - prev_sm_storage)
!                     END DO
! !write(*,*) "here4" 
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
!                     CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "balancing GW-SM storages")
!                     prev_sm_storage = 0.0
!                     itr = 1
!                     ! check = (itr < SS% max_iterations) .AND. (ABS(pSM_% Lstorage(t) - prev_sm_storage) > SS% sm_gw_fluctuation_tolerance)
!                     ! #TODO: VERIFY THIS
!                     DO WHILE((itr < SS% max_iterations) .AND. (ABS(pSM_% Lstorage(t) - prev_sm_storage) > SS% sm_gw_fluctuation_tolerance))
! ! write(*,*) "itr = *", itr, ABS(pSM_% Lstorage(t) - prev_sm_storage)
! ! write(*,*) "sm, gw", pSM_% Lstorage(t), GW% Lstorage(e,t)
!                         pSM_% RWlbound = GW% Lstorage(e,t) - MAX(GW% Lstorage(e,t), pSM_% ADlbound)
!                         pSM_% RWubound = GW% Lstorage(e,t) - pSM_% ADubound
!                         prev_sm_storage = pSM_% Lstorage(t)
!                         pSM_% Lstorage(t) = pSM_% vanG% integrate(pSM_% RWubound, pSM_% RWlbound)
!                         GW% Lstorage(e,t) = GW% Lstorage(e,t) - (pSM_% Lstorage(t) - prev_sm_storage) / pSM_% porosity
!                         IF (.NOT. GW% Lstorage(e,t) > pSM_% ADlbound) CALL self% resolve(e, t, UZ, GW, SW, time, SS)
!                         itr = itr + 1
! ! write(*,*) ABS(pSM_% Lstorage(t) - prev_sm_storage)
!                     END DO

                    pSM_% Lepv = ABS(pSM_% RWubound - pSM_% RWlbound) * pSM_% porosity
                    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Rbounds: ", pSM_% RWubound, pSM_% RWlbound)
                    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Lstorage = ", pSM_% Lstorage(t))
                    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "GWS = ", GW% Lstorage(e,t))
                    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "ePV = ", pSM_% Lepv)
                END IF
            END DO

        ELSE IF(GW% Lstorage(e,t) > pSM_% ADubound) THEN
        ! if GWS lies above gws_bnd layer, i.e. GWS is rising
            CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "GWS is rising")
!write(*,*) "GWS is rising"
            pSM_ => self% SM(self% gws_bnd_smid)
            ! deactivate the the previously GW bound layer
            GW% Lstorage(e,t) = GW% Lstorage(e,t) + (pSM_% Lstorage(t) / pSM_% porosity)
! IF(ABS(GW% Lstorage(e,t) - GW% Lstorage(e,t-1)) > 0.25) write(*,*) "*5", time% Gts, time% Lts, GW% Lstorage(e,t-1), GW% Lstorage(e,t)
            CALL deactivate(pSM_, gw_bound=.TRUE., sm1=(self% gws_bnd_smid==1))

            CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "SM", self% gws_bnd_smid, " is not active anymore")
            CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Transferred SM storage to GW and deallocated SM vars for SM", self% gws_bnd_smid)

            ! deactivate underlying layers until GW bound layer is discovered and flagged
            CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Deactivating underlying layers until GW bound layer is discovered")
            inner: DO smn = self% gws_bnd_smid-1, 1, -1
                pSM_ => self% SM(smn)
! write(*,*) GW% Lstorage(e,t), pSM_% ADubound
                IF (GW% Lstorage(e,t) < pSM_% ADubound) THEN
                    pSM_% gw_bound = .TRUE.
                    self% gws_bnd_smid = smn
                    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "SM", smn, " is now GW bound")

                    pSM_% RWubound = GW% Lstorage(e,t) - pSM_% ADubound ! ub = GWS - L_Aub
                    pSM_% RWlbound = GW% Lstorage(e,t) - MAX(GW% Lstorage(e,t), pSM_% ADlbound) ! ub = GWS - MAX(GWS, L_Alb)
                    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Rbounds ", pSM_% RWubound, pSM_% RWlbound)

                    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "SMeq = ", pSM_% Lstorage(t))

                    pSM_% Lepv = ABS(pSM_% RWubound - pSM_% RWlbound) * pSM_% porosity
                    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "ePV = ", pSM_% Lepv)

! write(*,*) "here", smn, pSM_% Lstorage(t), pSM_% Lepv, GW% Lstorage(e,t)
!                     IF(pSM_% Lstorage(t) > pSM_% Lepv) THEN
!                         CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "SM > ePV! Transferring excess storage to GW and balancing GW-SM storages")
!                         prev_sm_storage = 0.0
!                         itr = 0
! write(*,*) "here2", SS% sm_gw_fluctuation_tolerance, SS% max_iterations
!                         DO WHILE((itr < SS% max_iterations) .AND. (ABS(pSM_% Lstorage(t) - prev_sm_storage) > SS% sm_gw_fluctuation_tolerance))
!                             GW% Lstorage(e,t) = GW% Lstorage(e,t) + ((pSM_% Lstorage(t) - prev_sm_storage) - pSM_% Lepv) / pSM_% porosity
!                             pSM_% Lstorage(t) = pSM_% Lepv
!                             IF (.NOT. GW% Lstorage(e,t) < pSM_% ADubound) CALL self% resolve(e, t, UZ, GW, SW, time, SS)
!                             IF(.NOT. self% isactive) RETURN
!                             pSM_% RWubound = GW% Lstorage(e,t) - pSM_% ADubound
!                             pSM_% RWlbound = GW% Lstorage(e,t) - MAX(GW% Lstorage(e,t), pSM_% ADlbound)
!                             pSM_% Lepv = ABS(pSM_% RWubound - pSM_% RWlbound) * pSM_% porosity
!                             itr = itr + 1
!                         END DO
!                         CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Rbounds: ", pSM_% RWubound, pSM_% RWlbound)
!                         CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Lstorage = ", pSM_% Lstorage(t))
!                         CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "GWS = ", GW% Lstorage(e,t))
!                         CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "ePV = ", pSM_% Lepv)
!                     END IF
! write(*,*) "here2", smn
                    EXIT inner
                ELSE
                    GW% Lstorage(e,t) = GW% Lstorage(e,t) + (pSM_% Lstorage(t) / pSM_% porosity)
! IF(ABS(GW% Lstorage(e,t) - GW% Lstorage(e,t-1)) > 0.25) write(*,*) "*7", time% Gts, time% Lts, GW% Lstorage(e,t-1), GW% Lstorage(e,t)
! write(*,*) "***" ! FIXME: possible segfault here!! check logic
                    CALL deactivate(pSM_, gw_bound=.FALSE., sm1=(smn == 1))

                    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "SM", smn, " is not active anymore")
                END IF
            END DO inner
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
! write(*,*) "here3"
        IF(.NOT. ALLOCATED(self% SM(1)% infiltration)) ALLOCATE(self% SM(1)% infiltration)

        UZ% Lstorage(e,t) = 0.0
        UZ% Lepv(e,t) = 0.0
        DO smn = 1, self% gws_bnd_smid
            IF (self% SM(smn)% isactive) THEN
                UZ% Lstorage(e,t) = UZ% Lstorage(e,t) + self% SM(smn)% Lstorage(t)
                UZ% Lepv(e,t) = UZ% Lepv(e,t) + self% SM(smn)% Lepv
            END IF
        END DO
! write(*,*) "here3"
        CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "UZ Lstorage = ", UZ% Lstorage(e,t))
        CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "UZ Lepv = ", UZ% Lepv(e,t))
    END IF

    CALL plogger_Mstorages% log(plogger_Mstorages% TRACE, "*** leaving UZ_solve ***")
! write(*,*) "DONE"
END SUBROUTINE resolve



SUBROUTINE solve(self, e, t, dt, UZ, GW, SW, time, SS, first_run)

    USE Mtiming, ONLY : Ctime

    CLASS(Cuz_), INTENT(INOUT) :: self

    INTEGER(INT32), INTENT(IN) :: e, t
    REAL(REAL32), INTENT(IN) :: dt
    TYPE(Cuz), INTENT(INOUT) :: UZ
    TYPE(Cgw), INTENT(INOUT) :: GW
    TYPE(Csw), INTENT(INOUT) :: SW
    TYPE(Ctime), INTENT(IN) :: time
    TYPE(Csettings), INTENT(IN) :: SS
    
    LOGICAL, INTENT(IN) :: first_run

    TYPE(Csm), POINTER :: pSM_, pSM_prev_
    INTEGER(INT8) :: smn

    CALL plogger_Mstorages% log(plogger_Mstorages% TRACE, "*** in UZ_solve ***")
    CALL plogger_Mstorages% log(plogger_Mstorages% TRACE, "Solving GW bound SM layer: e,t = ", e, t-1)

    smn = 1
    pSM_ => self% SM(smn)
    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "** SM = ", smn, " **")

    CALL pSM_% vanG% setvars()
    pSM_% EQstorage = pSM_% vanG% integrate(pSM_% RWubound, pSM_% RWlbound)
    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "SMeq = ", pSM_% EQstorage)

    IF(first_run) THEN
        ! IF(pSM_% infiltration /= 0.0) THEN
        !     pSM_% IC = MAX(pSM_% IC + pSM_% inf_cap, 0.0) ! #TODO: cap to thickness of SM layer?
        ! ELSE
        !     pSM_% IC = MAX(pSM_% IC - pSM_% inf_cap, 0.0)
        ! END IF

        ! pSM_% IC_ratio = MIN(1.0, MAX(pSM_% IC / ABS(pSM_% RWubound - pSM_% RWlbound), 0.1))

        pSM_% exf_cap = pSM_% vanG% kUS((pSM_% Lstorage(t) / pSM_% Lepv) * pSM_% porosity, pSM_% ks) * dt
    ELSE
        pSM_% exf_cap = MAX(pSM_% exf_cap - MAX(pSM_% exfiltration, 0.0), 0.0)
    END IF

    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "IC, IC_ratio = ", pSM_% IC, pSM_% IC_ratio)
    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "exf_cap = ", pSM_% exf_cap)
    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Lstorage was ", pSM_% Lstorage(t))
! pSM_% IC_ratio = 1.0
    pSM_% exfiltration = MIN((pSM_% Lstorage(t) - pSM_% EQstorage) * pSM_% IC_ratio, (pSM_% exf_cap * pSM_% IC_ratio)) ! negative exfiltration (i.e. infiltration from underlying layers) is not capped to always allow replenishment of SM until SMeq is reached as it is based on relatively instanteous capilary forces
! , (pSM_% Lstorage(t) - pSM_% EQstorage)
    ! #TODO: verify above - compare to original

    pSM_% Lstorage(t) = pSM_% Lstorage(t) - pSM_% exfiltration

!     IF((pSM_% Lstorage(t) > pSM_% Lepv) .AND. (.NOT. pSM_% gw_bound)) THEN
!         SW% Lstorage(e,t) = SW% Lstorage(e,t) + (pSM_% Lstorage(t) - pSM_% Lepv)
!         pSM_% Lstorage(t) = pSM_% Lepv
!     END IF

!     IF((pSM_% Lstorage(t) > pSM_% Lepv) .AND. (first_run) .AND. (pSM_% gw_bound)) THEN
!         pSM_% exfiltration = pSM_% exfiltration + (pSM_% Lstorage(t) - pSM_% Lepv)
!         pSM_% Lstorage(t) = pSM_% Lepv
!     END IF
! ! TODO: verify: both cases above and below
!     IF((pSM_% Lstorage(t) > pSM_% Lepv) .AND. (.NOT. first_run) .AND. (pSM_% gw_bound)) THEN
!         GW% Lstorage(e,t) = GW% Lstorage(e,t) + (pSM_% Lstorage(t) - pSM_% Lepv) / pSM_% porosity
! IF(ABS(GW% Lstorage(e,t) - GW% Lstorage(e,t-1)) > 0.25) write(*,*) "*8", time% Gts, time% Lts, GW% Lstorage(e,t-1), GW% Lstorage(e,t)
!         pSM_% Lstorage(t) = pSM_% Lepv
!     END IF

    IF(pSM_% Lstorage(t) == pSM_% Lepv) THEN
        GW% Lstorage(e,t) = UZ% top(e)
        pSM_% Lstorage(t) = 0.0 ! #TODO: verify
        CALL self% resolve(e, t, UZ, GW, SW, time, SS)
        IF(.NOT. self% isactive) RETURN 
    END IF

    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "exfiltration = ", pSM_% exfiltration)
    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Lstorage is ", pSM_% Lstorage(t))
    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "ePV = ", pSM_% Lepv)
    
    DO smn = 2, self% gws_bnd_smid
        pSM_ => self% SM(smn)
        IF(pSM_% isactive) THEN
            pSM_prev_ => self% SM(smn-1)
            CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "** SM = ", smn, " **")
            CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Lstorage was ", pSM_% Lstorage(t-1))
            CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "exfiltration from prev. SM = ", pSM_prev_% exfiltration)
            CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "ePV = ", pSM_% Lepv)

            pSM_% Lstorage(t) = pSM_% Lstorage(t-1) + pSM_prev_% exfiltration
            CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Lstorage after infiltration (exf. from prev.) is ", pSM_% Lstorage(t))

            CALL pSM_% vanG% setvars()
            pSM_% EQstorage = pSM_% vanG% integrate(pSM_% RWubound, pSM_% RWlbound)
            CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "SMeq = ", pSM_% EQstorage)

            IF(first_run) THEN
                pSM_% inf_cap = pSM_% vanG% kUS((pSM_% Lstorage(t-1) / pSM_% Lepv) * pSM_% porosity, pSM_% ks) * dt

                IF(pSM_prev_% exfiltration > 0.0) THEN
                    pSM_% IC = MIN(MAX(pSM_% IC + pSM_% inf_cap, 0.0), ABS(pSM_% RWubound - pSM_% RWlbound))
                ELSE
                    pSM_% IC = MIN(MAX(pSM_% IC - pSM_% inf_cap, 0.0), ABS(pSM_% RWubound - pSM_% RWlbound))
                END IF

                pSM_% IC_ratio = MIN(1.0, MAX(pSM_% IC / ABS(pSM_% RWubound - pSM_% RWlbound), 0.1))

                pSM_% exf_cap = pSM_% vanG% kUS((pSM_% Lstorage(t) / pSM_% Lepv) * pSM_% porosity, pSM_% ks) * dt
            END IF
            
            
            CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "IC, IC_ratio = ", pSM_% IC, pSM_% IC_ratio)
            CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "exf_cap = ", pSM_% exf_cap)
            
    ! pSM_% IC_ratio = 1.0        
            IF(.NOT. first_run) THEN
                pSM_% exf_cap = MAX(pSM_% exf_cap - MAX(pSM_% exfiltration, 0.0), 0.0)
            END IF

            pSM_% exfiltration = MIN((pSM_% Lstorage(t) - pSM_% EQstorage) * pSM_% IC_ratio, (pSM_% exf_cap * pSM_% IC_ratio)) ! negative exfiltration (i.e. infiltration from underlying layers) is not capped to always allow replenishment of SM until SMeq is reached as it is based on relatively instanteous capilary forces
            ! , (pSM_% Lstorage(t) - pSM_% EQstorage)

            CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "exfiltration was ", pSM_% exfiltration)

            IF(pSM_% Lstorage(t) > pSM_% Lepv) pSM_% exfiltration = pSM_% exfiltration + (pSM_% Lstorage(t) - pSM_% Lepv)

            pSM_% Lstorage(t) = pSM_% Lstorage(t) - pSM_% exfiltration

            CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "exfiltration, after ePV balancing = ", pSM_% exfiltration)
            CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Lstorage is ", pSM_% Lstorage(t))

            IF(pSM_% gw_bound) EXIT
            ! #TODO: limit exfiltration to (GWS - BOT) * porosity if GWS bnd and GWS - exf < BOT and make appropriate changes in solve()

        END IF
    END DO

    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "UZ Lstorage was ", UZ% Lstorage(e,t-1))
! !write(*,*) "calc UZS"
    UZ% Lstorage(e,t) = 0.0
    DO smn = 1, self% gws_bnd_smid
        IF (self% SM(smn)% isactive) THEN
            UZ% Lstorage(e,t) = UZ% Lstorage(e,t) + self% SM(smn)% Lstorage(t)
        END IF
    END DO
! !write(*,*) "done calc UZS"
    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "UZ Lstorage is ", UZ% Lstorage(e,t))
    CALL plogger_Mstorages% log(plogger_Mstorages% TRACE, "*** leaving UZ_solve ***")


    ! pSM_ => self% SM(self% gws_bnd_smid)
    ! IF(pSM_% exfiltration > 0.1) write(*,*) "exf: ", time% Gts, time% Lts, REAL(pSM_% exfiltration,4), REAL(pSM_% Lstorage(t),4), REAL(pSM_% Lepv,4), REAL(pSM_% exf_cap,4), REAL(pSM_% IC_ratio,4), self% gws_bnd_smid
END SUBROUTINE solve


SUBROUTINE solve_again(self, e, t, dt, UZ, GW, SW, time, SS)

    USE Mtiming, ONLY : Ctime

    CLASS(Cuz_), INTENT(INOUT) :: self

    INTEGER(INT32), INTENT(IN) :: e, t
    REAL(REAL32), INTENT(IN) :: dt
    TYPE(Cuz), INTENT(INOUT) :: UZ
    TYPE(Cgw), INTENT(INOUT) :: GW
    TYPE(Csw), INTENT(INOUT) :: SW
    TYPE(Ctime), INTENT(IN) :: time
    TYPE(Csettings), INTENT(IN) :: SS

    TYPE(Csm), POINTER :: pSM_, pSM_prev_
    INTEGER(INT8) :: smn

    CALL plogger_Mstorages% log(plogger_Mstorages% TRACE, "*** in UZ_solve-again ***")
    CALL plogger_Mstorages% log(plogger_Mstorages% TRACE, "Solving GW bound SM layer: e,t = ", e, t-1)

    smn = 1
    pSM_ => self% SM(smn)
    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "** SM = ", smn, " **")

    CALL pSM_% vanG% setvars()
    pSM_% EQstorage = pSM_% vanG% integrate(pSM_% RWubound, pSM_% RWlbound)
    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "SMeq = ", pSM_% EQstorage)

    pSM_% exf_cap = MAX(pSM_% exf_cap - MAX(pSM_% exfiltration, 0.0), 0.0)

    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "IC, IC_ratio = ", pSM_% IC, pSM_% IC_ratio)
    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "exf_cap = ", pSM_% exf_cap)
    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Lstorage was ", pSM_% Lstorage(t))
! pSM_% IC_ratio = 1.0
    pSM_% exfiltration = MIN((pSM_% Lstorage(t) - pSM_% EQstorage) * pSM_% IC_ratio, (pSM_% exf_cap * pSM_% IC_ratio)) ! negative exfiltration (i.e. infiltration from underlying layers) is not capped to always allow replenishment of SM until SMeq is reached as it is based on relatively instanteous capilary forces
! #TODO: verify above - compare to original
! , (pSM_% Lstorage(t) - pSM_% EQstorage)
    pSM_% Lstorage(t) = pSM_% Lstorage(t) - pSM_% exfiltration

!     IF((pSM_% Lstorage(t) > pSM_% Lepv) .AND. (.NOT. pSM_% gw_bound)) THEN
!         SW% Lstorage(e,t) = SW% Lstorage(e,t) + (pSM_% Lstorage(t) - pSM_% Lepv)
!         pSM_% Lstorage(t) = pSM_% Lepv
!     END IF

!     IF((pSM_% Lstorage(t) > pSM_% Lepv) .AND. (first_run) .AND. (pSM_% gw_bound)) THEN
!         pSM_% exfiltration = pSM_% exfiltration + (pSM_% Lstorage(t) - pSM_% Lepv)
!         pSM_% Lstorage(t) = pSM_% Lepv
!     END IF
! ! #TODO: verify: both cases above and below
!     IF((pSM_% Lstorage(t) > pSM_% Lepv) .AND. (.NOT. first_run) .AND. (pSM_% gw_bound)) THEN
!         GW% Lstorage(e,t) = GW% Lstorage(e,t) + (pSM_% Lstorage(t) - pSM_% Lepv) / pSM_% porosity
! IF(ABS(GW% Lstorage(e,t) - GW% Lstorage(e,t-1)) > 0.25) write(*,*) "*8", time% Gts, time% Lts, GW% Lstorage(e,t-1), GW% Lstorage(e,t)
!         pSM_% Lstorage(t) = pSM_% Lepv
!     END IF

    IF(pSM_% Lstorage(t) == pSM_% Lepv) THEN
        GW% Lstorage(e,t) = UZ% top(e)
        pSM_% Lstorage(t) = 0.0 ! #TODO: verify
        CALL self% resolve(e, t, UZ, GW, SW, time, SS)
        IF(.NOT. self% isactive) RETURN 
    END IF

    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "exfiltration = ", pSM_% exfiltration)
    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Lstorage is ", pSM_% Lstorage(t))
    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "ePV = ", pSM_% Lepv)
    
    DO smn = 2, self% gws_bnd_smid
        pSM_ => self% SM(smn)
        IF(pSM_% isactive) THEN
            pSM_prev_ => self% SM(smn-1)
            CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "** SM = ", smn, " **")
            CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "exfiltration from prev. SM = ", pSM_prev_% exfiltration)
            CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "ePV = ", pSM_% Lepv)

            pSM_% Lstorage(t) = pSM_% Lstorage(t) + pSM_prev_% exfiltration
            CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Lstorage after infiltration (exf. from prev.) is ", pSM_% Lstorage(t))

            CALL pSM_% vanG% setvars()
            pSM_% EQstorage = pSM_% vanG% integrate(pSM_% RWubound, pSM_% RWlbound)
            CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "SMeq = ", pSM_% EQstorage)
            
            CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "IC, IC_ratio = ", pSM_% IC, pSM_% IC_ratio)
            CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "exf_cap = ", pSM_% exf_cap)
            
    ! pSM_% IC_ratio = 1.0        
            pSM_% exf_cap = MAX(pSM_% exf_cap - MAX(pSM_% exfiltration, 0.0), 0.0)
            
            pSM_% exfiltration = MIN((pSM_% Lstorage(t) - pSM_% EQstorage) * pSM_% IC_ratio, (pSM_% exf_cap * pSM_% IC_ratio)) ! negative exfiltration (i.e. infiltration from underlying layers) is not capped to always allow replenishment of SM until SMeq is reached as it is based on relatively instanteous capilary forces
! , (pSM_% Lstorage(t) - pSM_% EQstorage)
            CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "exfiltration was ", pSM_% exfiltration)

            IF(pSM_% Lstorage(t) > pSM_% Lepv) pSM_% exfiltration = pSM_% exfiltration + (pSM_% Lstorage(t) - pSM_% Lepv)

            pSM_% Lstorage(t) = pSM_% Lstorage(t) - pSM_% exfiltration

            CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "exfiltration, after ePV balancing = ", pSM_% exfiltration)
            CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Lstorage is ", pSM_% Lstorage(t))

            IF(pSM_% gw_bound) EXIT
            ! #TODO: limit exfiltration to (GWS - BOT) * porosity if GWS bnd and GWS - exf < BOT and make appropriate changes in solve()

        END IF
    END DO

    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "UZ Lstorage was ", UZ% Lstorage(e,t-1))
! !write(*,*) "calc UZS"
    UZ% Lstorage(e,t) = 0.0
    DO smn = 1, self% gws_bnd_smid
        IF (self% SM(smn)% isactive) THEN
            UZ% Lstorage(e,t) = UZ% Lstorage(e,t) + self% SM(smn)% Lstorage(t)
        END IF
    END DO
! !write(*,*) "done calc UZS"
    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "UZ Lstorage is ", UZ% Lstorage(e,t))
    CALL plogger_Mstorages% log(plogger_Mstorages% TRACE, "*** leaving UZ_solve-again ***")


    ! pSM_ => self% SM(self% gws_bnd_smid)
    ! IF(pSM_% exfiltration > 0.1) write(*,*) "exf: ", time% Gts, time% Lts, REAL(pSM_% exfiltration,4), REAL(pSM_% Lstorage(t),4), REAL(pSM_% Lepv,4), REAL(pSM_% exf_cap,4), REAL(pSM_% IC_ratio,4), self% gws_bnd_smid
END SUBROUTINE solve_again
MODULE Mstorages
    ! handles core GWSWEX operations and manipulations
    USE iso_fortran_env, ONLY: REAL32, REAL64, REAL128, INT8, INT16, INT32, INT64
    USE Muz, ONLY: CvanG, Clayer
    USE Msolver, ONLY: Csettings
    USE Mlogger, ONLY: Clogger

    IMPLICIT NONE

    TYPE(Clogger), POINTER :: plogger_Mstorages
    INTEGER, PARAMETER  :: STRLEN=256


    TYPE Cstorage
        ! base storage type
        ! storage:											[m] 	[e,t]								{relation defined by the child type}
        ! Ldischarge: 										[m/s] 	[e,t]								{ - }
        REAL(REAL128), DIMENSION(:,:), ALLOCATABLE :: Gstorage, Lstorage
        REAL(REAL128), DIMENSION(:,:), ALLOCATABLE :: Ldischarge, Gdischarge

    END TYPE Cstorage

    TYPE, EXTENDS(Cstorage) :: Cuz
        ! to store and access unsturated zone storages
        ! storages (locally relative):						[m] 										{0 - epv}
        ! epv (effective pore volume): 						[m] 	[e,t]								{(Aubounds-Albounds)*porosity}
        ! nlay (number of real vertical layers): 			[-]		[-] 								{1 - 128}
        INTEGER(INT8) :: nlay
        TYPE(Clayer), DIMENSION(:), ALLOCATABLE :: layer
        REAL(REAL64), DIMENSION(:), POINTER :: top
        REAL(REAL64), DIMENSION(:,:), POINTER :: bot
        REAL(REAL128), DIMENSION(:,:), ALLOCATABLE :: Gepv, Lepv
        REAL(REAL128), DIMENSION(:,:,:), ALLOCATABLE :: Gepvnl
    END TYPE Cuz

    TYPE, EXTENDS(Cstorage) :: Cgw
        ! to store, access, and manipulate surface-water storage and parameters
        ! storages:											[m] 										{aquifer bottom elevation - ground surface elevation; relative to defined datum, e.g.: masl}
        ! chd (constant head boundary): 					[-] 	[e]									{T/F}
        LOGICAL(KIND=1), DIMENSION(:), ALLOCATABLE :: chd
    END TYPE Cgw

    TYPE, EXTENDS(Cstorage) :: Csw
        ! to store, access, and manipulate surface-water storage and parameters
        ! storages (locally relative):						[m] 										{m above ground surface elevation}
    END TYPE Csw


    TYPE :: Csm
        ! to store, access, and manipulate soil moisture parameters of discrete model layers and elements
        ! eq (sm at equilibrium):							[m] 	[-]									{0 - epv}
        ! epv (effective pore volume): 						[m] 	[t]									{(Rubound-Rlbound)*porosity}
        ! Rubound, Rlbound (rel. upper and lower bounds): 	[m]		[t]									{relative to the water table (0 m)}
        ! saturation: 										[m3/m3]	[t]									{vanG% theta_r - vanG% theta_s}
        ! saturation_ratio: 								[m3/m3]	[t]									{(vanG% theta_s) - 1}
        ! IC (interconnectivity coefficient): 				[m]		[t]									{0 - thickness}
        ! ICrat (interconnectivity ratio): 					[-]		[t]									{0 - 1}
        ! ks (saturated hydraulic conductivity): 			[m/s] 	[-]									{ - }
        ! kus (unsaturated hydraulic conductivity): 		[m/s] 	[t]									{ - }
        ! porosity: 										[m3/m3] [-]									{0 - 1}
        LOGICAL :: isactive, gw_bound
        INTEGER(INT8), ALLOCATABLE :: lid
        TYPE(CvanG), POINTER :: vanG
        REAL(REAL64), POINTER :: ks, porosity
        REAL(REAL128), DIMENSION(:), ALLOCATABLE :: Gstorage, Lstorage
        REAL(REAL128), ALLOCATABLE :: Lepv
        REAL(REAL64), ALLOCATABLE :: RWubound, RWlbound
        REAL(REAL64), POINTER :: ADubound, ADlbound
        REAL(REAL128), ALLOCATABLE :: EQstorage, infiltration, exfiltration, inf_cap, exf_cap
        REAL(REAL128), ALLOCATABLE :: IC, IC_ratio

        ! CONTAINS
        ! 	PROCEDURE, PASS :: activate
        ! 	PROCEDURE, PASS :: deactivate
    END TYPE Csm

    TYPE Cuz_
        ! to store, access, and manipulate lumped unsaturated zone parameters
        ! nlay (number of real vertical layers): 			[-] 	[-]									{1 - 128}
        ! Aubound, Albound (abs. upper and lower bounds): 	[m]		[nlay]								{relative to the defined datum}
        ! # ADD: option to specify whether the layers are physical or virtual to enable single vanGenuchten parameter set for all layers
        TYPE(Csm), DIMENSION(:), POINTER :: SM ! 				[nlay]
        INTEGER(INT8) :: nlay, gws_bnd_smid
        ! REAL(REAL64), POINTER :: Aubound
        ! REAL(REAL128) :: Albound
        LOGICAL :: isactive
    CONTAINS
        PROCEDURE, PASS :: init
        PROCEDURE, PASS :: resolve
        PROCEDURE, PASS :: solve
        PROCEDURE, PASS :: solve_again
    END TYPE Cuz_

    TYPE Cext_forcings
        ! to store, access, and manipulate external forcings
        ! p (precipitation): 								[m/s]										{ - }
        ! et (evapotranspiration): 							[m/s]										{ - }
        REAL(REAL64), DIMENSION(:,:), POINTER :: p, et
    END TYPE Cext_forcings

CONTAINS

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


    ! #TODO: turn this into a pass procedure so that it can be called from solve() directly and also from uz% resolve(), thus avoiding for the call_resolve flag
    SUBROUTINE stabalize_sm_gw(e, t, pSM_, GW, SS, mode, call_resolve, prev_sm_in)
        INTEGER(INT32), INTENT(IN) :: e, t
        TYPE(Csm), POINTER, INTENT(INOUT) :: pSM_
        TYPE(Cgw), INTENT(INOUT) :: GW
        TYPE(Csettings), INTENT(IN) :: SS
        CHARACTER(LEN=*), INTENT(IN) :: mode
        LOGICAL, INTENT(INOUT) :: call_resolve
        REAL(REAL128), INTENT(IN), OPTIONAL :: prev_sm_in

        INTEGER(INT16) :: itr
        REAL(REAL128) :: prev_sm

        itr = 1
        prev_sm = 0.0
        IF(PRESENT(prev_sm_in)) prev_sm = prev_sm_in

        SELECT CASE(TRIM(mode))

            CASE('EQ')
                DO WHILE((itr < SS% max_iterations) .AND. (ABS(pSM_% Lstorage(t) - prev_sm) > SS% sm_gw_fluctuation_tolerance))
                    pSM_% RWlbound = GW% Lstorage(e,t) - MAX(GW% Lstorage(e,t), pSM_% ADlbound)
                    pSM_% RWubound = GW% Lstorage(e,t) - pSM_% ADubound
                    prev_sm = pSM_% Lstorage(t)
                    pSM_% Lstorage(t) = pSM_% vanG% integrate(pSM_% RWubound, pSM_% RWlbound)
                    GW% Lstorage(e,t) = GW% Lstorage(e,t) - (pSM_% Lstorage(t) - prev_sm) / pSM_% porosity
                    IF (.NOT. GW% Lstorage(e,t) > pSM_% ADlbound) THEN
                        call_resolve = .TRUE.
                        RETURN
                    END IF
                    itr = itr + 1
                END DO

            CASE('EPV')
                DO WHILE((itr < SS% max_iterations) .AND. (ABS(pSM_% Lstorage(t) - prev_sm) > SS% sm_gw_fluctuation_tolerance))
                    GW% Lstorage(e,t) = GW% Lstorage(e,t) + ((pSM_% Lstorage(t) - prev_sm) - pSM_% Lepv) / pSM_% porosity
                    pSM_% Lstorage(t) = pSM_% Lepv
                    IF (.NOT. GW% Lstorage(e,t) < pSM_% ADubound) THEN
                        call_resolve = .TRUE.
                        RETURN
                    END IF
                    pSM_% RWubound = GW% Lstorage(e,t) - pSM_% ADubound
                    pSM_% RWlbound = GW% Lstorage(e,t) - MAX(GW% Lstorage(e,t), pSM_% ADlbound)
                    pSM_% Lepv = ABS(pSM_% RWubound - pSM_% RWlbound) * pSM_% porosity
                    itr = itr + 1
                END DO

            CASE DEFAULT
                ERROR STOP "Unknown mode in stabalize_sm_gw"

        END SELECT
    END SUBROUTINE stabalize_sm_gw



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
            ! #FIXME, #PONDER: couple porosity and theta_s of vanG and thus eliminate per element (ks and) porosity declaration? OR maintain bimodal porosity?

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

                pSM_% IC = pSM_% vanG% theta_r ! #FIXME, #PONDER: set min IC to something representative of theta_r (+2: 640, 655)
                pSM_% IC_ratio =MIN(pSM_% IC / ABS(pSM_% RWubound - pSM_% RWlbound), 0.1)
                ! #TODO: take min IC_ratio as input parameter #PONDER: could a higher IC-ratio_min act as a proxy for macropore inf? (+2: 644, 661)

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
                SW% Lstorage(e,t) = SW% Lstorage(e,t) + (GW% Lstorage(e,t) - UZ% top(e)) * self% SM(1)% porosity
                GW% Lstorage(e,t) = UZ% top(e)
            END IF

            CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "UZ is not active anymore")
            ! write(*,*) "UZ is not active anymore", self% isactive
            RETURN
            ! write(*,*) "not returning"
        END IF

        check = GW% Lstorage(e,t) > UZ% top(e) .OR. GW% Lstorage(e,t) == UZ% top(e)
        IF(check .AND. (.NOT. self% isactive)) RETURN
        ! write(*,*) "not returning"

        pSM_ => self% SM(self% gws_bnd_smid)
        check = ((GW% Lstorage(e,t) < pSM_% ADubound) .OR. GW% Lstorage(e,t) == pSM_% ADubound) .AND. ((GW% Lstorage(e,t) > pSM_% ADlbound) .OR. GW% Lstorage(e,t) == pSM_% ADlbound)
        IF(check) THEN
            ! if GWS lies within the bounds of the gws_bnd layer
            pSM_ => self% SM(self% gws_bnd_smid)

            check = .FALSE.
            IF(.NOT. pSM_% isactive) THEN
                CALL activate(pSM_, sm1=(self% gws_bnd_smid == 1), gw_bound=.TRUE., nts=time% Lnts)
                check = .TRUE.
            END IF

            pSM_% RWubound = GW% Lstorage(e,t) - pSM_% ADubound ! ub = GWS - L_Aub
            pSM_% RWlbound = GW% Lstorage(e,t) - MAX(GW% Lstorage(e,t), pSM_% ADlbound) ! ub = GWS - L_Alb
            pSM_% Lepv = ABS(pSM_% RWubound - pSM_% RWlbound) * pSM_% porosity

            CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Rbounds: ", pSM_% RWubound, pSM_% RWlbound)
            CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Lstorage = ", pSM_% Lstorage(t))
            CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "ePV = ", pSM_% Lepv)

            IF (check) THEN
                CALL pSM_% vanG% setvars()
                pSM_% Lstorage(t) = pSM_% vanG% integrate(pSM_% RWubound, pSM_% RWlbound)
                CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "SMeq = ", pSM_% Lstorage(t))

                GW% Lstorage(e,t) = GW% Lstorage(e,t) - (pSM_% Lstorage(t) / pSM_% porosity)
                IF (.NOT. GW% Lstorage(e,t) > pSM_% ADlbound) CALL self% resolve(e, t, UZ, GW, SW, time, SS)
            END IF

            DO smn = self% gws_bnd_smid-1, 1, -1
                pSM_ => self% SM(smn)
                pSM_% RWubound = GW% Lstorage(e,t) - pSM_% ADubound ! ub = GWS - L_Aub
                pSM_% RWlbound = GW% Lstorage(e,t) - pSM_% ADlbound ! ub = GWS - L_Alb
                pSM_% Lepv = ABS(pSM_% RWubound - pSM_% RWlbound) * pSM_% porosity
            END DO
        END IF

        pSM_ => self% SM(self% gws_bnd_smid)
        check = ((GW% Lstorage(e,t) < pSM_% ADubound) .OR. GW% Lstorage(e,t) == pSM_% ADubound) .AND. ((GW% Lstorage(e,t) > pSM_% ADlbound) .OR. GW% Lstorage(e,t) == pSM_% ADlbound)
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
                check = .FALSE.
                IF(.NOT. pSM_% isactive) THEN
                    CALL activate(pSM_, sm1=(self% gws_bnd_smid == 1), gw_bound=.TRUE., nts=time% Lnts)
                    check = .TRUE.
                END IF
                ! recalculate the bounds of the previously GW bound layer and deactivate the gw_bound flag
                pSM_% RWubound = GW% Lstorage(e,t) - pSM_% ADubound ! ub = GWS - L_Aub
                pSM_% RWlbound = GW% Lstorage(e,t) - MAX(GW% Lstorage(e,t), pSM_% ADlbound) ! ub = GWS - L_Alb
                CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Rbounds: ", pSM_% RWubound, pSM_% RWlbound)
                pSM_% gw_bound = .FALSE.
                pSM_% Lepv = ABS(pSM_% RWubound - pSM_% RWlbound) * pSM_% porosity
                CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Lepv = ", pSM_% Lepv)

                IF (check) THEN
                    CALL pSM_% vanG% setvars()
                    pSM_% Lstorage(t) = pSM_% vanG% integrate(pSM_% RWubound, pSM_% RWlbound)
                    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "SMeq = ", pSM_% Lstorage(t))

                    GW% Lstorage(e,t) = GW% Lstorage(e,t) - (pSM_% Lstorage(t) / pSM_% porosity)
                    IF (.NOT. GW% Lstorage(e,t) > pSM_% ADlbound) CALL self% resolve(e, t, UZ, GW, SW, time, SS)
                END IF

                ! activate the underlying layers until the GW bound layer is discovered, set the bounds, set the GW bound flag, and calculate the respective storages
                CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Activating underlying layers until GW bound layer is discovered")
                DO smn = self% gws_bnd_smid+1, self% nlay
                    pSM_ => self% SM(smn)

                    CALL activate(pSM_, nts=time% Lnts, sm1=(smn == 1))
                    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "SM", smn, " is active. Allocating SM vars")

                    pSM_% RWubound = GW% Lstorage(e,t) - pSM_% ADubound ! ub = GWS - L_Aub
                    pSM_% RWlbound = GW% Lstorage(e,t) - MAX(GW% Lstorage(e,t), pSM_% ADlbound) ! ub = GWS - MAX(GWS, L_Alb)
                    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Rbounds: ", pSM_% RWubound, pSM_% RWlbound)

                    CALL pSM_% vanG% setvars()
                    pSM_% Lstorage(t) = pSM_% vanG% integrate(pSM_% RWubound, pSM_% RWlbound)
                    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "SMeq = ", pSM_% Lstorage(t))

                    GW% Lstorage(e,t) = GW% Lstorage(e,t) - (pSM_% Lstorage(t) / pSM_% porosity)
                    IF (.NOT. GW% Lstorage(e,t) > pSM_% ADlbound) CALL self% resolve(e, t, UZ, GW, SW, time, SS)

                    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "GW = ", GW% Lstorage(e,t))
                    IF(SS% stabalize_sm_gw) THEN
                        CALL stabalize_sm_gw(e, t, pSM_, GW, SS, 'EQ', check)
                        IF(check) CALL self% resolve(e, t, UZ, GW, SW, time, SS)
                    END IF

                    IF(t /= 1) pSM_% Lstorage(t-1) = pSM_% Lstorage(t)

                    pSM_% Lepv = ABS(pSM_% RWubound - pSM_% RWlbound) * pSM_% porosity
                    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Rbounds: ", pSM_% RWubound, pSM_% RWlbound)
                    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Lstorage = ", pSM_% Lstorage(t))
                    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "GWS = ", GW% Lstorage(e,t))
                    CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "ePV = ", pSM_% Lepv)

                    IF (GW% Lstorage(e,t) > pSM_% ADlbound) THEN
                        pSM_% gw_bound = .TRUE.
                        self% gws_bnd_smid = smn
                        CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "SM", smn, " is GW bound")
                        EXIT ! exit SM scanning DO loop
                    END IF
                END DO

            ELSE IF(GW% Lstorage(e,t) > pSM_% ADubound) THEN
                ! if GWS lies above gws_bnd layer, i.e. GWS is rising
                CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "GWS is rising")
                !write(*,*) "GWS is rising"
                pSM_ => self% SM(self% gws_bnd_smid)
                ! deactivate the the previously GW bound layer
                GW% Lstorage(e,t) = GW% Lstorage(e,t) + (pSM_% Lstorage(t) / pSM_% porosity) ! #PONDER: send to upper layer instead of GW?
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


                        IF(pSM_% Lstorage(t) > pSM_% Lepv .AND. SS% stabalize_sm_gw) THEN
                            CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "SM > ePV! Transferring excess storage to GW and balancing GW-SM storages")
                            IF(SS% stabalize_sm_gw) THEN
                                CALL stabalize_sm_gw(e, t, pSM_, GW, SS, 'EPV', check)
                                IF(check) CALL self% resolve(e, t, UZ, GW, SW, time, SS)
                                IF(.NOT. self% isactive) RETURN
                            END IF
                        END IF

                        EXIT inner
                    ELSE
                        GW% Lstorage(e,t) = GW% Lstorage(e,t) + (pSM_% Lstorage(t) / pSM_% porosity)

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



    SUBROUTINE solve(self, e, t, dt, UZ, GW, SW, time, SS)

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

        CALL plogger_Mstorages% log(plogger_Mstorages% TRACE, "*** in UZ_solve ***")
        CALL plogger_Mstorages% log(plogger_Mstorages% TRACE, "Solving GW bound SM layer: e,t = ", e, t-1)

        smn = 1
        pSM_ => self% SM(smn)
        CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "** SM = ", smn, " **")

        CALL pSM_% vanG% setvars()
        pSM_% EQstorage = pSM_% vanG% integrate(pSM_% RWubound, pSM_% RWlbound)
        CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "SMeq = ", pSM_% EQstorage)


        pSM_% exf_cap = pSM_% vanG% kUS((pSM_% Lstorage(t) / pSM_% Lepv) * pSM_% porosity, pSM_% ks) * dt
    

        CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "IC, IC_ratio = ", pSM_% IC, pSM_% IC_ratio)
        CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "exf_cap = ", pSM_% exf_cap)
        CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Lstorage was ", pSM_% Lstorage(t))

        pSM_% exfiltration = MIN((pSM_% Lstorage(t) - pSM_% EQstorage) * pSM_% IC_ratio, (pSM_% exf_cap * pSM_% IC_ratio), (pSM_% Lstorage(t) - pSM_% EQstorage))
        ! negative exfiltration (i.e. infiltration from underlying layers) is not capped to always allow replenishment of SM until SMeq is reached as it is based on relatively instanteous capilary forces
        ! #VERIFY: compare to original (+3: 669, 739, 786)

        pSM_% Lstorage(t) = pSM_% Lstorage(t) - pSM_% exfiltration

        IF((pSM_% Lstorage(t) > pSM_% Lepv) .AND. (.NOT. pSM_% gw_bound)) THEN
            SW% Lstorage(e,t) = SW% Lstorage(e,t) + (pSM_% Lstorage(t) - pSM_% Lepv)
            pSM_% Lstorage(t) = pSM_% Lepv
        END IF

        IF((pSM_% Lstorage(t) > pSM_% Lepv) .AND. (pSM_% gw_bound)) THEN
            pSM_% exfiltration = pSM_% exfiltration + (pSM_% Lstorage(t) - pSM_% Lepv)
            pSM_% Lstorage(t) = pSM_% Lepv
        END IF
        ! #VERIFY

        IF(pSM_% Lstorage(t) == pSM_% Lepv) THEN
            GW% Lstorage(e,t) = UZ% top(e)
            pSM_% Lstorage(t) = 0.0 ! #VERIFY
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

                pSM_% inf_cap = pSM_% vanG% kUS((pSM_% Lstorage(t-1) / pSM_% Lepv) * pSM_% porosity, pSM_% ks) * dt

                IF(pSM_prev_% exfiltration > 0.0) THEN
                    pSM_% IC = MIN(MAX(pSM_% IC + pSM_% inf_cap, 0.0), ABS(pSM_% RWubound - pSM_% RWlbound))
                ELSE
                    pSM_% IC = MIN(MAX(pSM_% IC - pSM_% inf_cap, 0.0), ABS(pSM_% RWubound - pSM_% RWlbound))
                END IF

                pSM_% IC_ratio = MIN(1.0, MAX(pSM_% IC / ABS(pSM_% RWubound - pSM_% RWlbound), 0.1))

                pSM_% exf_cap = pSM_% vanG% kUS((pSM_% Lstorage(t) / pSM_% Lepv) * pSM_% porosity, pSM_% ks) * dt


                CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "IC, IC_ratio = ", pSM_% IC, pSM_% IC_ratio)
                CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "exf_cap = ", pSM_% exf_cap)

                pSM_% exfiltration = MIN((pSM_% Lstorage(t) - pSM_% EQstorage) * pSM_% IC_ratio, (pSM_% exf_cap * pSM_% IC_ratio), (pSM_% Lstorage(t) - pSM_% EQstorage))
                ! negative exfiltration (i.e. infiltration from underlying layers) is not capped to always allow replenishment of SM until SMeq is reached as it is based on relatively instanteous capilary forces

                CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "exfiltration was ", pSM_% exfiltration)

                IF(pSM_% Lstorage(t) > pSM_% Lepv) pSM_% exfiltration = pSM_% exfiltration + (pSM_% Lstorage(t) - pSM_% Lepv)

                pSM_% Lstorage(t) = pSM_% Lstorage(t) - pSM_% exfiltration

                CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "exfiltration, after ePV balancing = ", pSM_% exfiltration)
                CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Lstorage is ", pSM_% Lstorage(t))

                IF(pSM_% gw_bound) EXIT
                ! #FIXME: limit exfiltration to (GWS - BOT) * porosity if GWS bnd and GWS - exf < BOT and make appropriate changes in solve() (+1: 799)

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

        pSM_% exfiltration = MIN((pSM_% Lstorage(t) - pSM_% EQstorage) * pSM_% IC_ratio, (pSM_% exf_cap * pSM_% IC_ratio), (pSM_% Lstorage(t) - pSM_% EQstorage)) 
        ! negative exfiltration (i.e. infiltration from underlying layers) is not capped to always allow replenishment of SM until SMeq is reached as it is based on relatively instanteous capilary forces
        pSM_% Lstorage(t) = pSM_% Lstorage(t) - pSM_% exfiltration

        IF((pSM_% Lstorage(t) > pSM_% Lepv) .AND. (.NOT. pSM_% gw_bound)) THEN
            SW% Lstorage(e,t) = SW% Lstorage(e,t) + (pSM_% Lstorage(t) - pSM_% Lepv)
            pSM_% Lstorage(t) = pSM_% Lepv
        END IF

        ! #VERIFY
        IF((pSM_% Lstorage(t) > pSM_% Lepv) .AND. (pSM_% gw_bound)) THEN
            GW% Lstorage(e,t) = GW% Lstorage(e,t) + (pSM_% Lstorage(t) - pSM_% Lepv) / pSM_% porosity
            pSM_% Lstorage(t) = pSM_% Lepv
        END IF

        IF(pSM_% Lstorage(t) == pSM_% Lepv) THEN
            GW% Lstorage(e,t) = UZ% top(e)
            pSM_% Lstorage(t) = 0.0 ! #VERIFY
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


                pSM_% exf_cap = MAX(pSM_% exf_cap - MAX(pSM_% exfiltration, 0.0), 0.0)

                pSM_% exfiltration = MIN((pSM_% Lstorage(t) - pSM_% EQstorage) * pSM_% IC_ratio, (pSM_% exf_cap * pSM_% IC_ratio), (pSM_% Lstorage(t) - pSM_% EQstorage))
                ! negative exfiltration (i.e. infiltration from underlying layers) is not capped to always allow replenishment of SM until SMeq is reached as it is based on relatively instanteous capilary forces

                CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "exfiltration was ", pSM_% exfiltration)

                IF(pSM_% Lstorage(t) > pSM_% Lepv) pSM_% exfiltration = pSM_% exfiltration + (pSM_% Lstorage(t) - pSM_% Lepv)

                pSM_% Lstorage(t) = pSM_% Lstorage(t) - pSM_% exfiltration

                CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "exfiltration, after ePV balancing = ", pSM_% exfiltration)
                CALL plogger_Mstorages% log(plogger_Mstorages% DEBUG, "Lstorage is ", pSM_% Lstorage(t))

                IF(pSM_% gw_bound) EXIT

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

END MODULE Mstorages

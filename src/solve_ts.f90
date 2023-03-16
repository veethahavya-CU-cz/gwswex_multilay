RECURSIVE SUBROUTINE solve(e, t, dt, P, ET, lateral_GW_flux, lateral_SW_flux)

    IMPLICIT NONE

    INTEGER(INT32), INTENT(IN) :: e, t
    REAL(REAL32), INTENT(IN) :: dt
    REAL(REAL128), INTENT(IN) :: P, ET
    REAL(REAL128), INTENT(IN), OPTIONAL :: lateral_GW_flux, lateral_SW_flux


    REAL(REAL64), POINTER :: porosity_gwbnd, porosity_gwbnd_above, porosity_gwbnd_below

    TYPE(Cuz_), POINTER :: pUZ_
    TYPE(Csm), POINTER :: pSM_

    ! REAL(REAL128), DIMENSION(:,:), POINTER :: pGW, pSW, pUZ

    REAL(REAL128) :: infiltration_deficit, et_sw, excess_precipitation, infiltration_sw, et_deficit, prev_gw_storage, excess_sm, theta
    INTEGER :: itr

    CHARACTER(LEN=STRLEN) :: strbuffer

    excess_sm = 0.0

    time% Lts = t
    time% current = time% current + time% Ldt

    ! IF(UZ_(e)% SM(UZ_(e)% gws_bnd_smid-1)% isactive) porosity_gwbnd_above => UZ_(e)% SM(UZ_(e)% gws_bnd_smid-1)% porosity
    porosity_gwbnd => UZ_(e)% SM(UZ_(e)% gws_bnd_smid)% porosity
    ! IF(UZ_(e)% SM(UZ_(e)% gws_bnd_smid+1)% isactive) porosity_gwbnd_below => UZ_(e)% SM(UZ_(e)% gws_bnd_smid+1)% porosity

    CALL logger% log(logger% TRACE, "*** in solve_main ***")
    CALL logger% log(logger% TRACE, "Solving: e,t = ", e, t-1)

    IF (.NOT. GW% chd(e)) THEN
    !### free GW boundary case

        IF (UZ_(e)% isactive) THEN
!write(*,*) UZ_(e)% isactive
        !## case 1: UZ is active
            CALL logger% log(logger% TRACE, "UZ is active")

            pSM_ => UZ_(e)% SM(1)

            CALL logger% log(logger% DEBUG, "SM1, ePV = ", pSM_% Lstorage(t-1), pSM_% Lepv)

            theta = (pSM_% Lstorage(t-1) / pSM_% Lepv)* pSM_% porosity
            pSM_% inf_cap = pSM_% vanG% kUS(theta, pSM_% ks) * dt

            CALL logger% log(logger% DEBUG, "theta, kUS = ", theta, pSM_% vanG% kUS(theta, pSM_% ks))
            CALL logger% log(logger% DEBUG, "theoritical inf_cap = ", pSM_% inf_cap)

            theta = ((pSM_% Lstorage(t-1) + pSM_% inf_cap) / pSM_% Lepv)* pSM_% porosity
            pSM_% exf_cap = pSM_% vanG% kUS(theta, pSM_% ks) * dt

            IF(P /= 0.0) THEN
                pSM_% IC = MAX(pSM_% IC + pSM_% inf_cap, 0.0) ! #TODO: cap to thickness of SM layer?
            ELSE
                pSM_% IC = MAX(pSM_% IC - pSM_% inf_cap, 0.0)
            END IF
            pSM_% IC_ratio = MIN(1.0, MAX(pSM_% IC / ABS(pSM_% RWubound - pSM_% RWlbound), 0.1))

            CALL pSM_% vanG% setvars()
            pSM_% EQstorage = pSM_% vanG% integrate(pSM_% RWubound, pSM_% RWlbound) 
            pSM_% exfiltration = MIN((pSM_% Lstorage(t-1) + pSM_% inf_cap - pSM_% EQstorage) * pSM_% IC_ratio, (pSM_% exf_cap * pSM_% IC_ratio))

            pSM_% inf_cap = MIN(pSM_% inf_cap, (pSM_% Lepv - pSM_% Lstorage(t-1) + pSM_% exfiltration))
            CALL logger% log(logger% DEBUG, "new theta, practical inf_cap = ", theta, pSM_% inf_cap)

            pSM_% infiltration = MIN(P, pSM_% inf_cap)
            CALL logger% log(logger% DEBUG, "P, infiltration = ", P, pSM_% infiltration)

            excess_precipitation = P - pSM_% infiltration
            infiltration_deficit = (pSM_% inf_cap - pSM_% infiltration)
            CALL logger% log(logger% DEBUG, "excess_precipitation, infiltration_deficit = ", excess_precipitation, infiltration_deficit)

            et_sw = MIN(SW% Lstorage(e,t-1) + excess_precipitation, ET)
            infiltration_sw = MIN(SW% Lstorage(e,t-1) + excess_precipitation - et_sw, infiltration_deficit)
            CALL logger% log(logger% DEBUG, "ET, et_sw = ", ET, et_sw)
            CALL logger% log(logger% DEBUG, "infiltration_sw = ", infiltration_sw)

            SW% Lstorage(e,t) = SW% Lstorage(e,t-1) + excess_precipitation - et_sw - infiltration_sw

            CALL logger% log(logger% DEBUG, "SW was = ", SW% Lstorage(e,t-1))
            CALL logger% log(logger% DEBUG, "SW is = ", SW% Lstorage(e,t))

            et_deficit = ET - et_sw
            IF(((GW% Lstorage(e,t-1) + (et_deficit / porosity_gwbnd)) < UZ% bot(UZ% nlay, e)) .OR. (GW% Lstorage(e,t-1) == UZ% bot(UZ% nlay, e))) &
                et_deficit = (GW% Lstorage(e,t-1) - UZ% bot(UZ% nlay, e)) * porosity_gwbnd ! no ET extraction from GW if GW is below UZ bottom
            CALL logger% log(logger% DEBUG, "et_deficit = ", et_deficit)

            pSM_% Lstorage(t) = pSM_% Lstorage(t-1) + pSM_% infiltration + infiltration_sw - et_deficit
            CALL logger% log(logger% DEBUG, "SM1, after inf. = ", pSM_% Lstorage(t-1), pSM_% Lstorage(t))
            
            ! IF(pSM_% Lstorage(t) > pSM_% Lepv) THEN
            !     IF(.NOT. UZ_(e)% gws_bnd_smid == 1) THEN
            !         SW% Lstorage(e,t) = SW% Lstorage(e,t) + (pSM_% Lstorage(t) - pSM_% Lepv)
            !         pSM_% Lstorage(t) = pSM_% Lepv
            !     ELSE
            !         excess_sm = pSM_% Lstorage(t) - pSM_% Lepv
            !         pSM_% Lstorage(t) = pSM_% Lepv
            !     END IF
            ! END IF
            CALL logger% log(logger% DEBUG, "SM1, SW = ", pSM_% Lstorage(t), SW% Lstorage(e,t))

            CALL UZ_(e)% solve(e, t, dt, UZ, GW, SW, time, SS, first_run=.TRUE.)

            CALL logger% log(logger% DEBUG, "SM1, ePV = ", pSM_% Lstorage(t), pSM_% Lepv)

            pSM_ => UZ_(e)% SM(UZ_(e)% gws_bnd_smid)
            GW% Lstorage(e,t) = GW% Lstorage(e,t-1) + (pSM_% exfiltration / pSM_% porosity) ! (pSM_% exfiltration + excess_sm) / pSM_% porosity
! IF(ABS(GW% Lstorage(e,t) - GW% Lstorage(e,t-1)) > 0.25) write(*,*) "1", time% Gts, time% Lts, GW% Lstorage(e,t-1), GW% Lstorage(e,t)
            CALL logger% log(logger% DEBUG, "SM_exf_gwbnd = ", pSM_% exfiltration)
            CALL logger% log(logger% DEBUG, "GW_inf = ", (pSM_% exfiltration / pSM_% porosity))
            CALL logger% log(logger% DEBUG, "GW was ", GW% Lstorage(e,t-1))
            CALL logger% log(logger% DEBUG, "GW is ", GW% Lstorage(e,t))
! WRITE(*,*) "in1"
            CALL UZ_(e)% resolve(e, t, UZ, GW, SW, time, SS)
            IF(.NOT. UZ_(e)% isactive) RETURN ! #TODO: calc discharges and then return
! WRITE(*,*) "out1"
            CALL UZ_(e)% solve_again(e, t, dt, UZ, GW, SW, time, SS) ! #FIXME: need to transfer SM(gws_bnd_smid) to GW after solve!
! write(*,*) "*"
pSM_ => UZ_(e)% SM(UZ_(e)% gws_bnd_smid)
! write(*,*) "*", pSM_% exfiltration
            GW% Lstorage(e,t) = GW% Lstorage(e,t) + (pSM_% exfiltration / pSM_% porosity) ! (pSM_% exfiltration + excess_sm) / pSM_% porosity
! write(*,*) "*"
! IF(ABS(GW% Lstorage(e,t) - GW% Lstorage(e,t-1)) > 0.25) write(*,*) "2", time% Gts, time% Lts, GW% Lstorage(e,t-1), GW% Lstorage(e,t)
            CALL logger% log(logger% DEBUG, "GW_inf = ", (pSM_% exfiltration / pSM_% porosity))
            CALL logger% log(logger% DEBUG, "GW was ", GW% Lstorage(e,t-1))
            CALL logger% log(logger% DEBUG, "GW is ", GW% Lstorage(e,t))

! WRITE(*,*) "in2"
            CALL UZ_(e)% resolve(e, t, UZ, GW, SW, time, SS)
! WRITE(*,*) "out2"
    ! !write(*,*) "here", t
            ! #TODO: CHECK!!!!!!!!!!
            ! ! ! prev_gw_storage = 0.0
            ! ! ! itr = 0
            ! ! ! DO WHILE(ABS(GW% Lstorage(e,t) - prev_gw_storage) > SS% sm_gw_fluctuation_tolerance .AND. itr < SS% max_iterations)
            ! ! !     CALL logger% log(logger% DEBUG, "** itr = ", itr, " **")
            ! ! !     prev_gw_storage = GW% Lstorage(e,t)
            ! ! !     itr = itr + 1
            ! ! !     CALL UZ_(e)% solve(e, t, dt, UZ, GW, SW, time, SS, first_run=.FALSE.)
            ! ! !     CALL UZ_(e)% resolve(e, t, UZ, GW, SW, time, SS)
            ! ! !     IF(.NOT. UZ_(e)% isactive) RETURN ! #TODO: calc discharges and then return
            ! ! ! END DO
            ! ! ! CALL logger% log(logger% DEBUG, "balanced SM-GW storage with ", itr, " iterations")

            ! pSM_ => UZ_(e)% SM(1)
            ! IF(pSM_% Lstorage(t) == pSM_% Lepv) THEN
            !     GW% Lstorage(e,t) = UZ% top(e)
            !     CALL UZ_(e)% resolve(e, t, UZ, GW, SW, time, SS)
            !     IF(.NOT. UZ_(e)% isactive) RETURN ! #TODO: calc discharges and then return
            ! END IF
    !                 CALL logger% log(logger% DEBUG, "GW, UZ = ", GW% Lstorage(e,t), UZ% Lstorage(e,t))
    ! !write(*,*) "*"

        ! calculate discharges
            ! pSM_ => UZ_(e)% SM(UZ_(e)% gws_bnd_smid)

            ! IF(GW% Lstorage(e,t-1) < pSM_% ADubound .AND. GW% Lstorage(e,t) > pSM_% ADubound) THEN
            !     GW% Ldischarge(e,t) = (GW% Lstorage(e,t) - GW% Lstorage(e,t-1)) * porosity_gwbnd
            ! ELSE IF(GW% Lstorage(e,t-1) > pSM_% ADubound) THEN
            ! ! GW fell a layer
            !     GW% Ldischarge(e,t) = (pSM_% ADubound - GW% Lstorage(e,t-1)) * porosity_gwbnd_above + &
            !         (GW% Lstorage(e,t) - pSM_% ADubound) * porosity_gwbnd
            ! ELSE IF(GW% Lstorage(e,t-1) < pSM_% ADlbound) THEN
            ! ! GW rose a layer
            !     GW% Ldischarge(e,t) = pSM_% ADlbound - GW% Lstorage(e,t-1) * porosity_gwbnd_below + &
            !         (GW% Lstorage(e,t) - pSM_% ADlbound) * porosity_gwbnd
            ! END IF
            ! SW% Ldischarge(e,t) = SW% Lstorage(e,t) - SW% Lstorage(e,t-1)
            ! UZ% Ldischarge(e,t) = UZ% Lstorage(e,t) - UZ% Lstorage(e,t-1)

        ELSE
        !## case 2: UZ is inactive
!write(*,*) "no UZ"
!write(*,*) "GW, SW before calc = ", GW% Lstorage(e,t-1), SW% Lstorage(e,t-1)
!write(*,*) "UZ top = ", UZ% top(e)
!write(*,*) "UZ = ", UZ% Lstorage(e,t-1)
            ! transfer excess GW storage and p to SW storage, set GW and UZ_Albound to GSL, and set UZ thickness to 0
            IF(GW% Lstorage(e,t-1) > UZ% top(e)) THEN
                SW% Lstorage(e,t) = SW% Lstorage(e,t-1) + ((GW% Lstorage(e,t-1) - UZ% top(e)) * porosity_gwbnd) + P
                GW% Lstorage(e,t) = UZ% top(e)
                UZ% Lstorage(e,t) = 0.0
                UZ% Lepv = 0.0
            ELSE IF(GW% Lstorage(e,t-1) == UZ% top(e)) THEN
                SW% Lstorage(e,t) = SW% Lstorage(e,t-1) + P
                GW% Lstorage(e,t) = UZ% top(e)
                UZ% Lstorage(e,t) = 0.0
                UZ% Lepv = 0.0
            ELSE
!write(*,*) "GW < UZ top", GW% Lstorage(e,t-1), UZ% top(e)
                CALL UZ_(e)% resolve(e, t, UZ, GW, SW, time, SS)
                IF(.NOT. UZ_(e)% isactive) CALL solve(e, t, dt, P, ET)
            END IF

            CALL logger% log(logger% DEBUG, "GW, SW before calc = ", GW% Lstorage(e,t-1), SW% Lstorage(e,t-1))
            CALL logger% log(logger% DEBUG, "GW, SW after calc = ", GW% Lstorage(e,t), SW% Lstorage(e,t))
            CALL logger% log(logger% DEBUG, "UZ = ", UZ% Lstorage(e,t))

!write(*,*) "GW, SW after calc = ", GW% Lstorage(e,t), SW% Lstorage(e,t)
!write(*,*) "UZ = ", UZ% Lstorage(e,t)
!write(*,*) "P, ET = ", P, ET
            ! ET extraction from SW storage and GW storage if ET > SM storage
            IF (SW% Lstorage(e,t) > ET) THEN
                SW% Lstorage(e,t) = SW% Lstorage(e,t) - ET
            ELSE
                GW% Lstorage(e,t) = GW% Lstorage(e,t) - ((ET - SW% Lstorage(e,t)) / porosity_gwbnd)
! IF(ABS(GW% Lstorage(e,t) - GW% Lstorage(e,t-1)) > 0.25) write(*,*) "5", time% Gts, time% Lts, GW% Lstorage(e,t-1), GW% Lstorage(e,t)
                SW% Lstorage(e,t) = 0.0
                CALL UZ_(e)% resolve(e, t, UZ, GW, SW, time, SS)
                IF(.NOT. UZ_(e)% isactive) CALL solve(e, t, dt, P=0.0_REAL128, ET=0.0_REAL128) ! #TODO: assess if necessary
            END IF
!write(*,*) "GW, SW after ET = ", GW% Lstorage(e,t), SW% Lstorage(e,t)
!write(*,*) "UZ status = ", UZ_(e)% isactive
            ! calculate discharges
            ! pSM_ => UZ_(e)% SM(UZ_(e)% gws_bnd_smid)
            ! IF(GW% Lstorage(e,t-1) < pSM_% ADubound .AND. GW% Lstorage(e,t) > pSM_% ADubound) THEN
            !     GW% Ldischarge(e,t) = (GW% Lstorage(e,t) - GW% Lstorage(e,t-1)) * porosity_gwbnd
            ! ELSE IF(GW% Lstorage(e,t-1) > pSM_% ADubound) THEN
            ! ! GW fell a layer
            !     GW% Ldischarge(e,t) = (pSM_% ADubound - GW% Lstorage(e,t-1)) * porosity_gwbnd_above + &
            !         (GW% Lstorage(e,t) - pSM_% ADubound) * porosity_gwbnd
            ! ELSE
            ! ! GW rose a layer
            !     GW% Ldischarge(e,t) = pSM_% ADlbound - GW% Lstorage(e,t-1) * porosity_gwbnd_below + &
            !         (GW% Lstorage(e,t) - pSM_% ADlbound) * porosity_gwbnd
            ! END IF
            ! SW% Ldischarge(e,t) = SW% Lstorage(e,t) - SW% Lstorage(e,t-1)
            ! UZ% Ldischarge(e,t) = UZ% Lstorage(e,t) - UZ% Lstorage(e,t-1)

        END IF

    ELSE
    !### CHD case
        CONTINUE

    END IF
    ! !write(*,*) "*"
    ! fluxes are negative when mass leaves the element in GWSWEX; i.e. fluxes are negative when the storage in GWSWEX is greater than external storage
    IF(PRESENT(lateral_GW_flux)) GW% Lstorage(e,t) = GW% Lstorage(e,t) + lateral_GW_flux
    IF(PRESENT(lateral_SW_flux)) SW% Lstorage(e,t) = SW% Lstorage(e,t) + lateral_SW_flux
! write(*,*) "^"
END SUBROUTINE solve






SUBROUTINE solve_t(e, lateral_GW_flux, lateral_SW_flux)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: e
    REAL(REAL128), INTENT(IN), DIMENSION(:), OPTIONAL :: lateral_GW_flux, lateral_SW_flux

    REAL(REAL128) :: ET, P
    REAL(REAL32) :: dt
    INTEGER(INT32) :: t, l

    TYPE(Csm), POINTER :: pSM_


    ET = EXTF% et(e, time% Gts) * time% Ldt% total_seconds()
    P = EXTF% p(e, time% Gts) * time% Ldt% total_seconds()

    CALL logger% log(logger% DEBUG, "ET, P = ", ET, P)

    dt = time% Ldt% total_seconds()
    time% current = time% Lstart

    time% Lts = 1

    t = 1
 
    CALL UZ_(e)% resolve(e, t, UZ, GW, SW, time, SS)
! !write(*,*) "time% Lnts = ", time% Lnts

    DO t = 2, time% Lnts + 1
        CALL solve(e, t, dt, P, ET)
! !write(*,*) "*"
    END DO

! write(*,*) "*"
    GW% Gstorage(e, time% Gts) = GW% Lstorage(e, time% Lnts+1)
    SW% Gstorage(e, time% Gts) = SW% Lstorage(e, time% Lnts+1)
    UZ% Gstorage(e, time% Gts) = UZ% Lstorage(e, time% Lnts+1)
    UZ% Gepv(e, time% Gts) = UZ% Lepv(e, time% Lnts+1)
! write(*,*) "**"  
    DO l = 1, UZ_(e)% gws_bnd_smid
        pSM_ => UZ_(e)% SM(l)

        IF (UZ_(e)% SM(l)% isactive) THEN
            pSM_% Gstorage(time% Gts) = pSM_% Lstorage(time% Lnts+1)
UZ% Gepvnl(l,e,time% Gts) = pSM_% Lepv
        ELSE
            pSM_% Gstorage(time% Gts) = 0.0_REAL128
        END IF
    END DO
! write(*,*) "***"
    CALL logger% log(logger% TRACE, "*** leaving solve_main ***")
    FLUSH(logger% unit)
END SUBROUTINE solve_t





SUBROUTINE solve_e()

    IMPLICIT NONE

    INTEGER(INT32) :: e

    ! SCHEDULE(DYNAMIC) OR SCHEDULE(GUIDED) OR SCHEDULE(DYNAMIC): http://www.inf.ufsc.br/~bosco.sobral/ensino/ine5645/OpenMP_Dynamic_Scheduling.pdf || 
    !   https://610yilingliu.github.io/2020/07/15/ScheduleinOpenMP/

    ! PRIVATE types: https://stackoverflow.com/a/15309556/19053317 || https://www.openmp.org/wp-content/uploads/OMP-Users-Monthly-Telecon-20211210.pdf ||
    ! https://fortran-lang.discourse.group/t/newbie-question-use-openmp-with-derived-type/4651/10#:~:text=I%20actually%20don%E2%80%99t,all%20the%20components.

    ! $OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(e)
    DO e = 1, nelements

        CALL solve_t(e)

    END DO
    ! $OMP END PARALLEL DO
END SUBROUTINE solve_e





! SUBROUTINE resolve(GWS_ext, SWS_ext)
!     REAL(REAL64), INTENT(IN), DIMENSION(nelements, time% Lnts) :: GWS_ext, SWS_ext
!     REAL(REAL128), DIMENSION(time% Lnts) :: lateral_GW_flux, lateral_SW_flux
!     REAL(REAL128) :: GW_residual, SW_residual
!     INTEGER :: itr

!     ! $OMP PARALLEL DO PRIVATE(e, t, l, time% scratch_dt, time% scratch_td, lateral_GW_flux, lateral_SW_flux, GW_residual, SW_residual, itr) LASTPRIVATE(time% Lts) &
!     !   $OMP & SHARED(GW, SW, UZ, UZ_, EXTF, SS, nelements, paths, logger, time% wall_elapsed, time% elapsed, GWS_ext, SWS_ext, &
!     !   $OMP & time% Gstart, time% Gstop, time% Gts, time% Gdt, time% Gnts, time% Lstart, time% Lstop, time% Ldt, time% Lnts, time% current, time% wall_start)
!     DO e = 1, nelements
!     ! #TODO: consider having CHD case here (i.e. not checking if GWSWEX GW matches GW_ext storage if CHD) instead of in solve()
!         itr = 0

!         ! fluxes are negative when mass leaves the element in GWSWEX; i.e. fluxes are negative when the storage in GWSWEX is greater than external storage
!         lateral_GW_flux = GWS_ext(e, :) - GW% Lstorage(e, :)
!         lateral_SW_flux = SWS_ext(e, :) - SW% Lstorage(e, :)

!         GW_residual = GWS_ext(e, time% Lnts) - GW% Lstorage(e, time% Lnts)
!         SW_residual = SWS_ext(e, time% Lnts) - SW% Lstorage(e, time% Lnts)

!         DO WHILE(itr < SS% max_iterations .AND. &
!                 (GW_residual > SS% gw_tolerance .OR. SW_residual > SS% sw_tolerance))

!             itr = itr + 1

!             CALL solve(e, .TRUE., lateral_GW_flux, lateral_SW_flux)

!         END DO

!     END DO
!     ! $OMP END PARALLEL DO
! END SUBROUTINE resolve
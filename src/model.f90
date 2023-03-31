! # TODO: nest all debugging logger calls under ifdef for better performance; ref: https://genomeek.wordpress.com/2012/02/16/using-fortran-preprocessor-1/

! #FIXME: make sure model works for nlay == 1

MODULE model
    USE iso_fortran_env, ONLY: REAL32, REAL64, REAL128, INT8, INT16, INT32, INT64
    USE Mpaths, ONLY: Cpaths
    USE Mlogger, ONLY: Clogger
    USE Mtiming, ONLY: Ctime
    USE Muz, ONLY: Clayer
    USE Mstorages, ONLY: Cuz, Cgw, Csw, Cuz_, Csm, Cext_forcings
    USE Msolver, ONLY: Csettings

    IMPLICIT NONE

    TYPE(Cpaths), ALLOCATABLE :: paths
    TYPE(Clogger), POINTER :: logger

    INTEGER(INT32), ALLOCATABLE  :: nelements !#ADD: add var(s) to store elem id from MF6 and DFM
    TYPE(Ctime), ALLOCATABLE :: time

    TYPE(Cuz), ALLOCATABLE :: UZ
    TYPE(Cgw), ALLOCATABLE :: GW
    TYPE(Csw), ALLOCATABLE :: SW

    TYPE(Cuz_), DIMENSION(:), ALLOCATABLE :: UZ_

    TYPE(Cext_forcings), ALLOCATABLE :: EXTF

    TYPE(Csettings), ALLOCATABLE :: SS

    INTEGER, PARAMETER  :: lu=42, tu=99, STRLEN=256, CHUNKSIZE=INT(10000/8) ! set chunksize to nelements/nprocs

    REAL(REAL128), DIMENSION(:,:), ALLOCATABLE :: Qin, Qout, Qdiff

CONTAINS
    ! addnl. structural shorthands:
    !	+ yp - yaml object, parent
    !	+ yc - yaml object, child
    !	+ F - file



    SUBROUTINE build(Fyaml_path)
        USE YAMLInterface, DISABLED => STRLEN
        USE YAMLRead
        USE datetime_module, only: strptime, timedelta, datetime
        USE Muz, only: plogger_Muz
        USE Mstorages, only: plogger_Mstorages

        IMPLICIT NONE

        CHARACTER(LEN=*), INTENT(IN) :: Fyaml_path

        TYPE(YAMLHandler) :: fyaml
        TYPE(YAMLMap) :: yp_model, yp_paths, yp_util
        TYPE(YAMLMap) :: yc_model_domain, yc_path_dirs, yc_path_files, yc_util_logger, yc_model_ic, yc_model_bnd, yc_model_extf, yc_model_solver
        TYPE(YAMLMap), DIMENSION(:), ALLOCATABLE :: yc_model_domain_lays
        INTEGER :: ires, Gdt_copy

        INTEGER(INT32) :: e, l

        CHARACTER(LEN=STRLEN) :: strbuffer, strbuffer2, fpath
        REAL(REAL64), DIMENSION(4) :: vanG_pars
        REAL(REAL64), DIMENSION(:), ALLOCATABLE :: r64temp1d
        REAL(REAL64), DIMENSION(:,:), ALLOCATABLE :: r64temp2d
        INTEGER(INT8) :: i8temp

        ALLOCATE(paths, time, nelements, UZ, GW, SW, EXTF, SS)

        ! time% wall_start = time% wall_start% now()

        paths% config = TRIM(Fyaml_path)

        ! start reading the yaml file
        fyaml = yaml_open_file(Fyaml_path)
        ires = 0

        ! read, check, and store the paths
        yp_paths = yaml_start_from_map(fyaml, 'paths')

        yc_path_dirs = yp_paths% value_map('dirs')
        yc_path_files = yp_paths% value_map('files')

        WRITE(strbuffer, *) yc_path_dirs% value_str("root", ires)
        paths% root = strbuffer
        IF (ires /= 0) THEN
            ERROR STOP "ERROR: root path not defined in config file"
        END IF

        WRITE(strbuffer, *) yc_path_dirs% value_str("input", ires)
        paths% input = strbuffer
        IF (ires /= 0) THEN
            ERROR STOP "ERROR: input path not defined in config file"
        END IF

        WRITE(strbuffer, *) yc_path_dirs% value_str("output", ires)
        paths% output = strbuffer
        IF (ires /= 0) THEN
            ERROR STOP "ERROR: output path not defined in config file"
        END IF

        CALL yc_path_dirs% destroy()
        CALL yp_paths% destroy()


        ! read and set the logger level and initialize the logger
        yp_util = yaml_start_from_map(fyaml, 'utils')
        yc_util_logger = yp_util% value_map('logger')

        ALLOCATE(logger)
        logger% level = INT(yc_util_logger% value_int("level", ires), kind=INT8)
        IF (ires /= 0) THEN
            ERROR STOP "ERROR: logger level not found/incorrect in config file"
        END IF
        logger% unit = lu

        WRITE(strbuffer, *) yc_util_logger% value_str("fname", ires)
        logger% fname = strbuffer
        IF (ires /= 0) THEN
            logger% fname = "GWSWEX.log"
        END IF
        logger% fpath = TRIM(ADJUSTL(paths% output))//"/"//TRIM(ADJUSTL(logger% fname))

        CALL logger% init()
        plogger_Muz => logger
        plogger_Mstorages => logger
        CALL logger% log(logger% INFO, "Initializing model from config file")

        CALL yc_util_logger% destroy()
        CALL yp_util% destroy()


        ! read and set the model parameters
        yp_model = yaml_start_from_map(fyaml, 'model')
        yc_model_domain = yp_model% value_map('domain')

        nelements = yc_model_domain% value_int("nelements", ires)
        IF (ires /= 0) THEN
            ERROR STOP "ERROR: nelements not defined in config file"
            CALL logger% log(logger% fatal, "nelements not defined in config file")
        END IF
        WRITE(strbuffer, *) "Initializing model with ", nelements, " elements"
        CALL logger% log(logger% MOREINFO, TRIM(ADJUSTL(strbuffer)))

        ! allocate the UZ_
        ALLOCATE(UZ_(nelements))

        time% Gdt = timedelta(seconds = yc_model_domain% value_int("dt", ires))
        IF (ires /= 0) THEN
            ERROR STOP "ERROR: dt not defined in config file"
            CALL logger% log(logger%fatal, "dt not defined in config file")
        END IF
        WRITE(strbuffer, *) "Initializing model with a global time step of", time% Gdt% total_seconds(), " s."
        CALL logger% log(logger% MOREINFO, TRIM(ADJUSTL(strbuffer)))

        WRITE(strbuffer, *) yc_model_domain% value_str("tstart", ires)
        IF (ires /= 0) THEN
            ERROR STOP "ERROR: tstart not defined in config file"
            CALL logger% log(logger%fatal, "tstart not defined in config file")
        END IF
        time% Gstart = strptime(strbuffer, "%Y%m%d %H%M%S")
        WRITE(strbuffer, *) "Simulation start time: ", time% Gstart% isoformat()
        CALL logger% log(logger% MOREINFO, TRIM(ADJUSTL(strbuffer)))

        WRITE(strbuffer, *) yc_model_domain% value_str("tstop", ires)
        IF (ires /= 0) THEN
            ERROR STOP "ERROR: tstop not defined in config file"
            CALL logger% log(logger%fatal, "tstop not defined in config file")
        END IF
        time% Gstop = strptime(strbuffer, "%Y%m%d %H%M%S")
        WRITE(strbuffer, *) "Simulation stop time: ", time% Gstop% isoformat()
        CALL logger% log(logger% MOREINFO, TRIM(ADJUSTL(strbuffer)))

        time% scratch_td = time% Gstop - time% Gstart
        time% Gnts = INT(time% scratch_td% total_seconds()/time% Gdt% total_seconds(), kind=INT32)
        IF (time% Gnts < 0) THEN
            ERROR STOP "tstop < tstart"
            CALL logger% log(logger%fatal, "tstop < tstart")
        END IF

        Gdt_copy = time% Gdt% total_seconds()
        time% Gdt = timedelta(seconds = (time% scratch_td% total_seconds() / time% Gnts))
        IF(time% Gdt% total_seconds() /= Gdt_copy) THEN
            WRITE(strbuffer, *) "Provided Gdt does not result in a whole nunmber of global timesteps. Adjusting Gdt to ", time% Gdt% total_seconds(), " s."
            CALL logger% log(logger% WARN, TRIM(ADJUSTL(strbuffer)))
        END IF

        WRITE(strbuffer, *) "Model will run for ", time% Gnts, " time steps"
        CALL logger% log(logger% MOREINFO, TRIM(ADJUSTL(strbuffer)))

        time% current = time% Gstart
        time% elapsed = timedelta(seconds = 0)
        time% Gts = 2

        time% Lstart = time% Gstart
        time% Lstop = time% Gstart + time% Gdt
        ALLOCATE(time% Lts)

        ! allocating global storages
        CALL logger% log(logger% DEBUG, "Allocating global UZ, GW and SW storages")
        ALLOCATE(GW% Gstorage(nelements, time% Gnts+1), UZ% Gstorage(nelements, time% Gnts+1), UZ% Gepv(nelements, time% Gnts+1), SW% Gstorage(nelements, time% Gnts+1))

        UZ% nlay = yc_model_domain% value_int("nlay", ires)
        IF (ires /= 0) THEN
            ERROR STOP "ERROR: nlay not defined in config file"
            CALL logger% log(logger%fatal, "nlay not defined in config file")
        END IF
        WRITE(strbuffer, *) "Initializing model with ", UZ% nlay, " layers"
        CALL logger% log(logger% MOREINFO, TRIM(ADJUSTL(strbuffer)))
        WRITE(strbuffer, *) "Model initialized with ", UZ% nlay, " layers"
        CALL logger% log(logger% MOREINFO, TRIM(ADJUSTL(strbuffer)))

        ! allocate the UZ layers, top, and bottom
        ALLOCATE(UZ% layer(UZ% nlay), UZ% top(nelements), UZ% bot(UZ% nlay, nelements))

        IF (yc_path_files% value_int("DMN.TOP", ires) == 0) THEN
            UZ% top = yc_model_domain% value_double_1d("top", ires)
        ELSE
            WRITE(strbuffer, *) yc_model_domain% value_str("top", ires)
            fpath = TRIM(ADJUSTL(paths% input))//"/"//TRIM(ADJUSTL(strbuffer))
            OPEN(UNIT=tu, FILE=TRIM(fpath), FORM='UNFORMATTED')
            READ(UNIT=tu) UZ% top
            CLOSE (UNIT=tu)
        END IF

        IF (yc_path_files% value_int("DMN.BOT", ires) == 0) THEN
            UZ% bot = yc_model_domain% value_double_2d("bot", ires)
        ELSE

            WRITE(strbuffer, *) yc_model_domain% value_str("bot", ires)
            fpath = TRIM(ADJUSTL(paths% input))//"/"//TRIM(ADJUSTL(strbuffer))
            OPEN(UNIT=tu, FILE=TRIM(fpath), FORM='UNFORMATTED')
            READ(tu) UZ% bot
            CLOSE (UNIT=tu)
        END IF

        CALL logger% log(logger% MOREINFO, "Layer elevations read")

        ALLOCATE(yc_model_domain_lays(UZ% nlay))

        ! read the UZ layer properties (from config file or specified unformatted file)
        DO l = 1, UZ% nlay

            WRITE(strbuffer2, *) l
            WRITE(strbuffer, *) "layer", ADJUSTL(TRIM(strbuffer2))

            yc_model_domain_lays(l) = yc_model_domain% value_map(ADJUSTL(TRIM(strbuffer)))

            WRITE(strbuffer, *) yc_model_domain_lays(l)% value_str("name", ires)
            UZ% layer(l)% name = TRIM(strbuffer)

            ALLOCATE(UZ% layer(l)% isactive(nelements))

            IF (yc_path_files% value_int("DMN.LAY.ACT", ires) == 0) THEN
                UZ% layer(l)% isactive = yc_model_domain_lays(l)% value_int_1d("isactive", ires)
            ELSE
                WRITE(strbuffer, *) yc_model_domain_lays(l)% value_str("isactive", ires)
                fpath = TRIM(ADJUSTL(paths% input))//"/"//ADJUSTL(TRIM(strbuffer))
                OPEN(UNIT=tu, FILE=TRIM(fpath), ACTION='READ', FORM='UNFORMATTED')
                READ(tu) UZ% layer(l)% isactive
                CLOSE (UNIT=tu)
            END IF
            ! #ADD, #PONDER: check if all underlying layers are active when one layer is declared as active

            ALLOCATE(UZ% layer(l)% Aubound(nelements), UZ% layer(l)% Albound(nelements))
            IF (l == 1) THEN
                UZ% layer(l)% Aubound => UZ% top
            ELSE
                UZ% layer(l)% Aubound => UZ% bot(l-1, :)
            END IF
            UZ% layer(l)% Albound => UZ% bot(l, :)
            ! #ADD: check if all bots lie below the tops or are at least equal to the top

            ! read the vanGenuchten parameters
            vanG_pars = yc_model_domain_lays(l)% value_double_1d("vanG", ires)

            ALLOCATE(UZ% layer(l)% vanG)
            ALLOCATE(UZ% layer(l)% vanG% alpha, UZ% layer(l)% vanG% n, UZ% layer(l)% vanG% m, UZ% layer(l)% vanG% theta_r, UZ% layer(l)% vanG% theta_s)

            UZ% layer(l)% vanG% alpha = vanG_pars(1)
            UZ% layer(l)% vanG% n = vanG_pars(2)
            UZ% layer(l)% vanG% theta_r = vanG_pars(3)
            UZ% layer(l)% vanG% theta_s = vanG_pars(4)
            CALL UZ% layer(l)% vanG% init()

            ALLOCATE(UZ% layer(l)% ks(nelements), UZ% layer(l)% porosity(nelements))

            IF (yc_path_files% value_int("DMN.LAY.KS", ires) == 0) THEN
                UZ% layer(l)% ks = yc_model_domain_lays(l)% value_double_1d("ks", ires)
            ELSE
                WRITE(strbuffer, *) yc_model_domain_lays(l)% value_str("ks", ires)
                fpath = TRIM(ADJUSTL(paths% input))//"/"//ADJUSTL(TRIM(strbuffer))
                OPEN(UNIT=tu, FILE=TRIM(fpath), ACTION='READ', FORM='UNFORMATTED')
                READ(tu) UZ% layer(l)% ks
                CLOSE (UNIT=tu)
            END IF

            IF (yc_path_files% value_int("DMN.LAY.POR", ires) == 0) THEN
                UZ% layer(l)% porosity = yc_model_domain_lays(l)% value_double_1d("porosity", ires)
            ELSE
                WRITE(strbuffer, *) yc_model_domain_lays(l)% value_str("porosity", ires)
                fpath = TRIM(ADJUSTL(paths% input))//"/"//ADJUSTL(TRIM(strbuffer))
                OPEN(UNIT=tu, FILE=TRIM(fpath), ACTION='READ', FORM='UNFORMATTED')
                READ(tu) UZ% layer(l)% porosity
                CLOSE (UNIT=tu)
            END IF

            CALL yc_model_domain_lays(l)% destroy()

            WRITE(strbuffer, *) "Layer properties read for layer", l
            CALL logger% log(logger% TRACE, TRIM(ADJUSTL(strbuffer)))
        END DO

        CALL logger% log(logger% MOREINFO, "Layer properties read")


        CALL yc_model_domain% destroy()

        ! read boundary conditions
        ALLOCATE(GW% chd(nelements))
        yc_model_bnd = yp_model% value_map('boundary conditions')

        IF (yc_path_files% value_int("BND.CHD", ires) == 0) THEN
            GW% chd = yc_model_bnd% value_int_1d("GW_CHD", ires)
        ELSE
            WRITE(strbuffer, *) yc_model_bnd% value_str("GW CHD", ires)
            fpath = TRIM(ADJUSTL(paths% input))//"/"//ADJUSTL(TRIM(strbuffer))
            OPEN(UNIT=tu, FILE=TRIM(fpath), FORM='UNFORMATTED', ACTION='READ')
            READ(tu) GW% chd
            CLOSE (UNIT=tu)
        END IF

        CALL yc_model_bnd% destroy()

        yc_model_ic = yp_model% value_map('initial conditions')

        ALLOCATE(r64temp1d(nelements))
        IF (yc_path_files% value_int("IC.GW", ires) == 0) THEN
            GW% Gstorage(:, 1) = yc_model_ic% value_double_1d("GW", ires)
        ELSE
            WRITE(strbuffer, *) yc_model_ic% value_str("GW", ires)
            fpath = TRIM(ADJUSTL(paths% input))//"/"//ADJUSTL(TRIM(strbuffer))
            OPEN(UNIT=tu, FILE=TRIM(fpath), FORM='UNFORMATTED', ACTION='READ')
            READ(tu) r64temp1d
            GW% Gstorage(:, 1) = r64temp1d
            CLOSE (UNIT=tu)
        END IF

        IF (yc_path_files% value_int("IC.SW", ires) == 0) THEN
            SW% Gstorage(:, 1) = yc_model_ic% value_double_1d("SW", ires)
        ELSE
            WRITE(strbuffer, *) yc_model_ic% value_str("SW", ires)
            fpath = TRIM(ADJUSTL(paths% input))//"/"//ADJUSTL(TRIM(strbuffer))
            OPEN(UNIT=tu, FILE=TRIM(fpath), FORM='UNFORMATTED', ACTION='READ')
            READ(tu) r64temp1d
            SW% Gstorage(:, 1) = r64temp1d
            CLOSE (UNIT=tu)
        END IF

        DEALLOCATE(r64temp1d)
        CALL yc_model_ic% destroy()

        ! read external forcings
        yc_model_extf = yp_model% value_map("external forcings")
        ALLOCATE(EXTF% p(nelements, time% Gnts+1), EXTF% et(nelements, time% Gnts+1))

        IF (yc_path_files% value_int("EXTF.p", ires) == 0) THEN
            EXTF% p(:,2:time% Gnts+1) = yc_model_extf% value_double_2d("p", ires)
        ELSE
            WRITE(strbuffer, *) yc_model_extf% value_str("p", ires)
            fpath = TRIM(ADJUSTL(paths% input))//"/"//ADJUSTL(TRIM(strbuffer))
            OPEN(UNIT=tu, FILE=TRIM(fpath), FORM='UNFORMATTED', ACTION='READ')
            READ(tu) EXTF% p(:,2:time% Gnts+1)
            CLOSE (UNIT=tu)
        END IF

        IF (yc_path_files% value_int("EXTF.et", ires) == 0) THEN
            EXTF% et(:,2:time% Gnts+1) = yc_model_extf% value_double_2d("et", ires)
        ELSE
            WRITE(strbuffer, *) yc_model_extf% value_str("et", ires)
            fpath = TRIM(ADJUSTL(paths% input))//"/"//ADJUSTL(TRIM(strbuffer))
            OPEN(UNIT=tu, FILE=TRIM(fpath), FORM='UNFORMATTED', ACTION='READ')
            READ(tu) EXTF% et(:,2:time% Gnts+1)
            CLOSE (UNIT=tu)
        END IF
        CALL yc_model_extf% destroy()

        ! read solver settings
        yc_model_solver = yp_model% value_map("solver settings")

        i8temp = SIZE(yc_model_solver% value_double_1d("pet_intensities", ires))
        ALLOCATE(SS% pet_intensities(i8temp), SS% pet_nts(i8temp))

        SS% pet_intensities = yc_model_solver% value_double_1d("pet_intensities", ires)
        SS% pet_nts = yc_model_solver% value_int_1d("pet_nts", ires)

        CALL yc_model_solver% destroy()

        ! #TODO: read [gw_tolerance, sw_tolerance, sm_gw_fluctuation_tolerance]
        ! #ADD: add option under utils to set the number of OMP threads, OMP type, chunksize, and precision for input files, vars, and output files
        ALLOCATE(Qin(nelements, time% Gnts+1), Qout(nelements, time% Gnts+1), Qdiff(nelements, time% Gnts+1))
        ALLOCATE(GW% Gdischarge(nelements, time% Gnts+1), SW% Gdischarge(nelements, time% Gnts+1), UZ% Gdischarge(nelements, time% Gnts+1))
        SS% max_iterations = 100
        SS% sm_gw_fluctuation_tolerance = 1.0E-9
        SS% gw_tolerance = 9.0E-3
        SS% sw_tolerance = 1.0E-1
        SS% stabalize_sm_gw = .FALSE.

        CALL yc_path_files% destroy()
        CALL yp_model% destroy()

        !$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(e) SCHEDULE(DYNAMIC)
        DO e = 1, nelements
            CALL UZ_(e)% init(e, UZ, GW, time)
        END DO
        !$OMP END PARALLEL DO

        ALLOCATE(UZ% Gepvnl(UZ% nlay, nelements, time% Gnts+1))

        CALL yaml_close_file(fyaml)

        CALL logger% log(logger%INFO, "Model built successfully")
        FLUSH(logger% unit)

    END SUBROUTINE



    SUBROUTINE init_ts(auto_advance, gw_ini, sw_ini, first_run)
        USE datetime_module, ONLY: timedelta, datetime
        USE Mstorages, ONLY: Csm

        IMPLICIT NONE

        LOGICAL, INTENT(IN) :: auto_advance
        REAL(REAL64), INTENT(IN), DIMENSION(:), OPTIONAL :: gw_ini, sw_ini
        LOGICAL, INTENT(IN), OPTIONAL :: first_run ! auto advances by default

        INTEGER(INT16) :: idx
        CHARACTER(LEN=STRLEN) :: strbuffer
        INTEGER(INT32) :: e, l

        REAL(REAL32) :: ipet_ll, ipet_ul, pet_intensity

        TYPE(Csm), POINTER :: pSM_

        LOGICAL :: chk

        !#PONDER: rethink auto advance and keep only first_run flag (and add option to set SM ini too)?
        ! advance the global timestep and deallocate the local storages, epvs, and discharges from the previous timestep
        IF(auto_advance) THEN
            time% Gts = time% Gts + 1
            DEALLOCATE(GW% Lstorage, UZ% Lstorage, UZ% Lepv, SW% Lstorage, GW% Ldischarge, UZ% Ldischarge, SW% Ldischarge)
            !$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(e) SCHEDULE(DYNAMIC)
            DO e = 1, nelements
                DO l = 1, UZ_(e)% nlay
                    IF (UZ_(e)% SM(l)% isactive) THEN
                        DEALLOCATE(UZ_(e)% SM(l)% Lstorage)
                        ! DEALLOCATE(UZ_(e)% SM(l)% Ldischarge)
                    END IF
                END DO
            END DO
            !$OMP END PARALLEL DO
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
        DO idx = 1, SIZE(SS% pet_intensities)
            ipet_ul = SS% pet_intensities(idx)
            IF (pet_intensity >= ipet_ll .AND. pet_intensity < ipet_ul) THEN
                time% Lnts = SS% pet_nts(idx)
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

        chk = .FALSE.
        IF (PRESENT(first_run)) chk = first_run

        IF(auto_advance .OR. chk) THEN
            ALLOCATE(GW% Lstorage(nelements, time% Lnts+1), UZ% Lstorage(nelements, time% Lnts+1), UZ% Lepv(nelements, time% Lnts+1), SW% Lstorage(nelements, time% Lnts+1), &
                GW% Ldischarge(nelements, time% Lnts+1), UZ% Ldischarge(nelements, time% Lnts+1), SW% Ldischarge(nelements, time% Lnts+1))
            !$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(e) SCHEDULE(DYNAMIC)
            DO e = 1, nelements
                DO l = 1, UZ_(e)% gws_bnd_smid
                    IF (UZ_(e)% SM(l)% isactive) THEN
                        pSM_ => UZ_(e)% SM(l)

                        IF(.NOT. ALLOCATED(pSM_% Lstorage)) ALLOCATE(pSM_% Lstorage(time% Lnts+1)) ! UZ_(e)% SM(l)% Ldischarge(time% Lnts+1)

                        pSM_% Lstorage(1) = pSM_% Gstorage(time% Gts-1)

                        ! #TODO: check redundancy
                        pSM_% Lepv = ABS(pSM_% RWubound - pSM_% RWlbound) * pSM_% porosity

                        CALL logger% log(logger% DEBUG, "Rbounds: ", pSM_% RWubound, pSM_% RWlbound)

                        WRITE(strbuffer, *) "SM ", l, " Lstorage_ini = ", pSM_% Lstorage(1), " Lepv_ini = ", pSM_% Lepv
                        CALL logger% log(logger% DEBUG, strbuffer)
                    END IF
                END DO
            END DO
            !$OMP END PARALLEL DO
        ELSE
            !$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(e) SCHEDULE(DYNAMIC)
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
            !$OMP END PARALLEL DO
        END IF

        ! set local storage to the global storage of last dt (or initial conditions for first dt)
        CALL logger% log(logger% TRACE, "Initialising local storages")
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



    RECURSIVE SUBROUTINE solve(e, t, dt, P, ET, lateral_GW_flux, lateral_SW_flux)

        IMPLICIT NONE

        INTEGER(INT32), INTENT(IN) :: e, t
        REAL(REAL32), INTENT(IN) :: dt
        REAL(REAL128), INTENT(IN) :: P, ET
        REAL(REAL64), INTENT(IN), OPTIONAL :: lateral_GW_flux, lateral_SW_flux


        REAL(REAL64), POINTER :: porosity_gwbnd, porosity_gwbnd_above, porosity_gwbnd_below

        TYPE(Cuz_), POINTER :: pUZ_
        TYPE(Csm), POINTER :: pSM_

        ! REAL(REAL128), DIMENSION(:,:), POINTER :: pGW, pSW, pUZ

        REAL(REAL128) :: infiltration_deficit, et_sw, excess_precipitation, infiltration_sw, et_deficit, prev_gw_storage, excess_sm, theta, prev_inf_cap
        INTEGER :: itr, sgn, l

        CHARACTER(LEN=STRLEN) :: strbuffer
        LOGICAL :: check

        excess_sm = 0.0_REAL128

        time% Lts = t
        time% current = time% current + time% Ldt

        ! IF(UZ_(e)% SM(UZ_(e)% gws_bnd_smid-1)% isactive) porosity_gwbnd_above => UZ_(e)% SM(UZ_(e)% gws_bnd_smid-1)% porosity
        porosity_gwbnd => UZ_(e)% SM(UZ_(e)% gws_bnd_smid)% porosity
        ! IF(UZ_(e)% SM(UZ_(e)% gws_bnd_smid+1)% isactive) porosity_gwbnd_below => UZ_(e)% SM(UZ_(e)% gws_bnd_smid+1)% porosity

        CALL logger% log(logger% TRACE, "*** in solve_main ***")
        CALL logger% log(logger% TRACE, "Solving: e,t = ", e, t-1)

        IF (UZ_(e)% isactive) THEN
            !## case 1: UZ is active
            CALL logger% log(logger% TRACE, "UZ is active")

            pSM_ => UZ_(e)% SM(1)

            CALL logger% log(logger% DEBUG, "SM1, ePV = ", pSM_% Lstorage(t-1), pSM_% Lepv)

            prev_inf_cap = 0.0_REAL128
            itr = 1

            theta = (pSM_% Lstorage(t-1) / pSM_% Lepv)* pSM_% porosity
            pSM_% inf_cap = pSM_% vanG% kUS(theta, pSM_% ks) * dt

            CALL logger% log(logger% DEBUG, "theta, kUS = ", theta, pSM_% vanG% kUS(theta, pSM_% ks))
            CALL logger% log(logger% DEBUG, "theoritical inf_cap = ", pSM_% inf_cap)

            DO WHILE (ABS(pSM_% inf_cap - prev_inf_cap) > SS% sm_gw_fluctuation_tolerance .AND. itr < SS% max_iterations)

                theta = ((pSM_% Lstorage(t-1) + pSM_% inf_cap) / pSM_% Lepv)* pSM_% porosity
                pSM_% exf_cap = pSM_% vanG% kUS(theta, pSM_% ks) * dt

                IF(P /= 0.0) THEN
                    pSM_% IC = MIN(MAX(pSM_% IC + pSM_% inf_cap, 0.0), ABS(pSM_% RWubound - pSM_% RWlbound))
                ELSE
                    pSM_% IC = MIN(MAX(pSM_% IC - pSM_% inf_cap, 0.0), ABS(pSM_% RWubound - pSM_% RWlbound))
                END IF
                pSM_% IC_ratio = MIN(1.0, MAX(pSM_% IC / ABS(pSM_% RWubound - pSM_% RWlbound), 0.1))

                CALL pSM_% vanG% setvars()
                pSM_% EQstorage = pSM_% vanG% integrate(pSM_% RWubound, pSM_% RWlbound)
                pSM_% exfiltration = MIN((pSM_% Lstorage(t-1) + pSM_% inf_cap - pSM_% EQstorage) * pSM_% IC_ratio, (pSM_% exf_cap * pSM_% IC_ratio))

                pSM_% inf_cap = MIN(pSM_% inf_cap, (pSM_% Lepv - pSM_% Lstorage(t-1) + pSM_% exfiltration))
                CALL logger% log(logger% DEBUG, "new theta, practical inf_cap = ", theta, pSM_% inf_cap)

                itr = itr + 1
                prev_inf_cap = pSM_% inf_cap

                theta = (pSM_% Lstorage(t-1) / pSM_% Lepv)* pSM_% porosity
                pSM_% inf_cap = pSM_% vanG% kUS(theta, pSM_% ks) * dt

            END DO

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

            CALL logger% log(logger% DEBUG, "SM1, SW = ", pSM_% Lstorage(t), SW% Lstorage(e,t))

            CALL UZ_(e)% solve(e, t, dt, UZ, GW, SW, time, SS)

            CALL logger% log(logger% DEBUG, "SM1, ePV = ", pSM_% Lstorage(t), pSM_% Lepv)

            pSM_ => UZ_(e)% SM(UZ_(e)% gws_bnd_smid)
            GW% Lstorage(e,t) = GW% Lstorage(e,t-1) + (pSM_% exfiltration / pSM_% porosity) ! (pSM_% exfiltration + excess_sm) / pSM_% porosity
            CALL logger% log(logger% DEBUG, "SM_exf_gwbnd = ", pSM_% exfiltration)
            CALL logger% log(logger% DEBUG, "GW_inf = ", (pSM_% exfiltration / pSM_% porosity))
            CALL logger% log(logger% DEBUG, "GW was ", GW% Lstorage(e,t-1))
            CALL logger% log(logger% DEBUG, "GW is ", GW% Lstorage(e,t))

            CALL UZ_(e)% resolve(e, t, UZ, GW, SW, time, SS)
            IF(.NOT. UZ_(e)% isactive) RETURN ! #FIXME: calc discharges and then return

            CALL UZ_(e)% solve_again(e, t, dt, UZ, GW, SW, time, SS)

            pSM_ => UZ_(e)% SM(UZ_(e)% gws_bnd_smid)
            CALL logger% log(logger% DEBUG, "*GW_inf = ", (pSM_% exfiltration / pSM_% porosity))
            CALL logger% log(logger% DEBUG, "*GW was ", GW% Lstorage(e,t))

            GW% Lstorage(e,t) = GW% Lstorage(e,t) + (pSM_% exfiltration / pSM_% porosity) ! (pSM_% exfiltration + excess_sm) / pSM_% porosity

            CALL logger% log(logger% DEBUG, "*GW is ", GW% Lstorage(e,t))

            CALL UZ_(e)% resolve(e, t, UZ, GW, SW, time, SS)

            ! #TODO: replace with UZ_% stabilize_sm_gw() method
            ! prev_gw_storage = 0.0
            ! itr = 0
            ! DO WHILE(ABS(GW% Lstorage(e,t) - prev_gw_storage) > SS% sm_gw_fluctuation_tolerance .AND. itr < SS% max_iterations)
            !     CALL logger% log(logger% DEBUG, "** itr = ", itr, " **")
            !     prev_gw_storage = GW% Lstorage(e,t)
            !     itr = itr + 1
            !     CALL UZ_(e)% solve_again(e, t, dt, UZ, GW, SW, time, SS)
            !     CALL UZ_(e)% resolve(e, t, UZ, GW, SW, time, SS)
            !     IF(.NOT. UZ_(e)% isactive) RETURN ! #NOTE: calc discharges and then return
            ! END DO
            ! CALL logger% log(logger% DEBUG, "balanced SM-GW storage with ", itr, " iterations")
            ! pSM_ => UZ_(e)% SM(UZ_(e)% gws_bnd_smid)
            ! GW% Lstorage(e,t) = GW% Lstorage(e,t) + (pSM_% exfiltration / pSM_% porosity)

            ! pSM_ => UZ_(e)% SM(1)
            ! IF(pSM_% Lstorage(t) == pSM_% Lepv) THEN
            !     GW% Lstorage(e,t) = UZ% top(e)
            !     CALL UZ_(e)% resolve(e, t, UZ, GW, SW, time, SS)
            !     IF(.NOT. UZ_(e)% isactive) RETURN ! #NOTE: calc discharges and then return
            ! END IF
            !                 CALL logger% log(logger% DEBUG, "GW, UZ = ", GW% Lstorage(e,t), UZ% Lstorage(e,t))


            ! fluxes are negative when mass leaves the element in GWSWEX; i.e. fluxes are negative when the storage in GWSWEX is greater than external storage
            IF(PRESENT(lateral_SW_flux)) SW% Lstorage(e,t) = SW% Lstorage(e,t) + lateral_SW_flux
            IF(PRESENT(lateral_GW_flux)) THEN
                GW% Lstorage(e,t) = GW% Lstorage(e,t) + lateral_GW_flux
                CALL UZ_(e)% resolve(e, t, UZ, GW, SW, time, SS)
            END IF

        ELSE
            !## case 2: UZ is inactive
            ! transfer excess GW storage and p to SW storage, set GW and UZ_Albound to GSL, and set UZ thickness to 0
            IF(GW% Lstorage(e,t-1) > UZ% top(e)) THEN
                SW% Lstorage(e,t) = SW% Lstorage(e,t-1) + ((GW% Lstorage(e,t-1) - UZ% top(e)) * porosity_gwbnd) + P
                GW% Lstorage(e,t) = UZ% top(e)
                UZ% Lstorage(e,t) = 0.0_REAL128
                UZ% Lepv = 0.0_REAL128
            ELSE IF(GW% Lstorage(e,t-1) == UZ% top(e)) THEN
                SW% Lstorage(e,t) = SW% Lstorage(e,t-1) + P
                GW% Lstorage(e,t) = UZ% top(e)
                UZ% Lstorage(e,t) = 0.0_REAL128
                UZ% Lepv = 0.0_REAL128
            ELSE
                CALL UZ_(e)% resolve(e, t, UZ, GW, SW, time, SS)
                IF(.NOT. UZ_(e)% isactive) CALL solve(e, t, dt, P, ET)
            END IF

            CALL logger% log(logger% DEBUG, "GW, SW before calc = ", GW% Lstorage(e,t-1), SW% Lstorage(e,t-1))
            CALL logger% log(logger% DEBUG, "GW, SW after calc = ", GW% Lstorage(e,t), SW% Lstorage(e,t))
            CALL logger% log(logger% DEBUG, "UZ = ", UZ% Lstorage(e,t))


            ! ET extraction from SW storage and GW storage if ET > SM storage
            IF (SW% Lstorage(e,t) > ET) THEN
                SW% Lstorage(e,t) = SW% Lstorage(e,t) - ET
            ELSE
                GW% Lstorage(e,t) = GW% Lstorage(e,t) - ((ET - SW% Lstorage(e,t)) / porosity_gwbnd)
                SW% Lstorage(e,t) = 0.0_REAL128
                CALL UZ_(e)% resolve(e, t, UZ, GW, SW, time, SS)
                IF(.NOT. UZ_(e)% isactive) CALL solve(e, t, dt, P=0.0_REAL128, ET=0.0_REAL128) ! #VERIFY: assess if necessary
            END IF

            IF(PRESENT(lateral_SW_flux)) SW% Lstorage(e,t) = SW% Lstorage(e,t) + lateral_SW_flux
            IF(PRESENT(lateral_GW_flux)) THEN
                GW% Lstorage(e,t) = GW% Lstorage(e,t) + lateral_GW_flux
                CALL UZ_(e)% resolve(e, t, UZ, GW, SW, time, SS)
            END IF

        END IF

        ! calculate discharges
        ! #FIXME: account for lateral discharges in discharge calc (+1)
        pSM_ => UZ_(e)% SM(UZ_(e)% gws_bnd_smid)
        IF((GW% Lstorage(e,t-1) < pSM_% ADubound .OR. GW% Lstorage(e,t-1) == pSM_% ADubound) .AND. (GW% Lstorage(e,t-1) > pSM_% ADlbound .OR. GW% Lstorage(e,t-1) == pSM_% ADlbound)) THEN
            GW% Ldischarge(e,t) = (GW% Lstorage(e,t) - GW% Lstorage(e,t-1)) * pSM_% porosity
        ELSE
            sgn = SIGN(1_INT8, UZ_(e)% prev_gws_bnd_smid - UZ_(e)% gws_bnd_smid)
            GW% Ldischarge(e,t) = 0.0_REAL128

            DO l = UZ_(e)% prev_gws_bnd_smid, UZ_(e)% gws_bnd_smid, -sgn
                pSM_ => UZ_(e)% SM(l)

                IF(sgn == -1) THEN
                    GW% Ldischarge(e,t) = GW% Ldischarge(e,t) + (MAX(GW% Lstorage(e,t), pSM_% ADlbound) - MIN(GW% Lstorage(e,t-1), pSM_% ADubound)) * pSM_% porosity
                ELSE
                    GW% Ldischarge(e,t) = GW% Ldischarge(e,t) + (MIN(GW% Lstorage(e,t), pSM_% ADubound) - MAX(GW% Lstorage(e,t-1), pSM_% ADlbound)) * pSM_% porosity
                END IF
                
                CALL logger% log(logger% DEBUG, "GW_dis = ", GW% Ldischarge(e,t))
            END DO
        END IF
        SW% Ldischarge(e,t) = SW% Lstorage(e,t) - SW% Lstorage(e,t-1)
        UZ% Ldischarge(e,t) = UZ% Lstorage(e,t) - UZ% Lstorage(e,t-1)

        IF(PRESENT(lateral_SW_flux)) SW% Ldischarge(e,t) = SW% Ldischarge(e,t) - lateral_SW_flux
        IF(PRESENT(lateral_GW_flux)) GW% Ldischarge(e,t) = GW% Ldischarge(e,t) - lateral_GW_flux      

        CALL logger% log(logger% DEBUG, "**********")
        CALL logger% log(logger% DEBUG, "P, ET = ", P, ET)
        CALL logger% log(logger% DEBUG, "GW(t-1), GW(t) = ", GW% Lstorage(e,t-1), GW% Lstorage(e,t))
        CALL logger% log(logger% DEBUG, "GWBND ADlbound, ADubound = ", pSM_% ADlbound, pSM_% ADubound)
        CALL logger% log(logger% DEBUG, "SW(t-1), SW(t) = ", SW% Lstorage(e,t-1), SW% Lstorage(e,t))
        CALL logger% log(logger% DEBUG, "UZ(t-1), UZ(t) = ", UZ% Lstorage(e,t-1), UZ% Lstorage(e,t))
        CALL logger% log(logger% DEBUG, "GW, SW, UZ discharges = ", GW% Ldischarge(e,t), SW% Ldischarge(e,t), UZ% Ldischarge(e,t))
        CALL logger% log(logger% DEBUG, "Qdiff, Qin, Qout = ", (P-ET) - (GW% Ldischarge(e,t) + SW% Ldischarge(e,t) + UZ% Ldischarge(e,t)), P-ET, (GW% Ldischarge(e,t) + SW% Ldischarge(e,t) + UZ% Ldischarge(e,t)))
        CALL logger% log(logger% DEBUG, "**********")

    END SUBROUTINE solve






    SUBROUTINE solve_t(e, lateral_GW_flux, lateral_SW_flux)

        IMPLICIT NONE

        INTEGER, INTENT(IN) :: e
        REAL(REAL64), INTENT(IN), DIMENSION(:), OPTIONAL :: lateral_GW_flux, lateral_SW_flux

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
        ! #VERIFY: check if resolving UZ here, i.e. changing GW% Lstorage(1) leads to MB calc errors because it may change the GW storage in the first time step, i.e. GW_ini
        CALL UZ_(e)% resolve(e, t, UZ, GW, SW, time, SS)

        DO t = 2, time% Lnts + 1
            IF(.NOT. (PRESENT(lateral_GW_flux) .AND. PRESENT(lateral_SW_flux))) THEN
                CALL solve(e, t, dt, P, ET)
            ELSE IF (PRESENT(lateral_GW_flux) .AND. PRESENT(lateral_SW_flux)) THEN
                CALL solve(e, t, dt, P, ET, lateral_GW_flux=lateral_GW_flux(time% Lts), lateral_SW_flux=lateral_SW_flux(time% Lts))
            ELSE IF (PRESENT(lateral_GW_flux)) THEN
                CALL solve(e, t, dt, P, ET, lateral_GW_flux=lateral_GW_flux(time% Lts))
            ELSE IF (PRESENT(lateral_SW_flux)) THEN
                CALL solve(e, t, dt, P, ET, lateral_SW_flux=lateral_SW_flux(time% Lts))
            END IF
        END DO

        GW% Gstorage(e, time% Gts) = GW% Lstorage(e, time% Lnts+1)
        SW% Gstorage(e, time% Gts) = SW% Lstorage(e, time% Lnts+1)
        UZ% Gstorage(e, time% Gts) = UZ% Lstorage(e, time% Lnts+1)
        DO l = 1, UZ_(e)% nlay
            pSM_ => UZ_(e)% SM(l)
            IF (UZ_(e)% SM(l)% isactive) THEN
                pSM_% Gstorage(time% Gts) = pSM_% Lstorage(time% Lnts+1)
                UZ% Gepvnl(l,e,time% Gts) = pSM_% Lepv
            ELSE
                pSM_% Gstorage(time% Gts) = 0.0_REAL128
                UZ% Gepvnl(l,e,time% Gts) = 0.0_REAL128
            END IF
        END DO

        GW% Gdischarge(e, time% Gts) = SUM(GW% Ldischarge(e, 2:time% Lnts+1))
        SW% Gdischarge(e, time% Gts) = SW% Gstorage(e, time% Gts) - SW% Gstorage(e, time% Gts-1) ! #HACK: to avoid mysterious error in SW% Gdischarge calc. where SW% Gdischarge == GW% Gstorage(t-1) even though SW% Ldischarge is calcd properly
        UZ% Gdischarge(e, time% Gts) = SUM(UZ% Ldischarge(e, 2:time% Lnts+1))

        UZ% Gepv(e, time% Gts) = UZ% Lepv(e, time% Lnts+1)

        Qin(e, time% Gts) = (EXTF% p(e, time% Gts) * time% Gdt% total_seconds()) - (EXTF% et(e, time% Gts) * time% Gdt% total_seconds())
        Qout(e, time% Gts) = GW% Gdischarge(e, time% Gts) + SW% Gdischarge(e, time% Gts) + UZ% Gdischarge(e, time% Gts)
        
        CALL logger% log(logger% TRACE, "*** leaving solve_main ***")
        FLUSH(logger% unit)
    END SUBROUTINE solve_t





    SUBROUTINE solve_e(lateral_GW_flux, lateral_SW_flux)

        IMPLICIT NONE

        REAL(REAL128), INTENT(IN), DIMENSION(:), OPTIONAL :: lateral_GW_flux, lateral_SW_flux

        INTEGER(INT32) :: e

        ! SCHEDULE(DYNAMIC) OR SCHEDULE(GUIDED) OR SCHEDULE(DYNAMIC): http://www.inf.ufsc.br/~bosco.sobral/ensino/ine5645/OpenMP_Dynamic_Scheduling.pdf ||
        !   https://610yilingliu.github.io/2020/07/15/ScheduleinOpenMP/

        ! PRIVATE types: https://stackoverflow.com/a/15309556/19053317 || https://www.openmp.org/wp-content/uploads/OMP-Users-Monthly-Telecon-20211210.pdf ||
        ! https://fortran-lang.discourse.group/t/newbie-question-use-openmp-with-derived-type/4651/10#:~:text=I%20actually%20don%E2%80%99t,all%20the%20components.

        !$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(e) SCHEDULE(DYNAMIC)
        DO e = 1, nelements

            CALL solve_t(e)

        END DO
        !$OMP END PARALLEL DO
    END SUBROUTINE solve_e





    SUBROUTINE resolve_l(lateral_GW_flux, lateral_SW_flux)

        REAL(REAL64), INTENT(IN), DIMENSION(:,:) :: lateral_GW_flux, lateral_SW_flux

        INTEGER :: e

        !$OMP PARALLEL DO 
        DO e = 1, nelements

            IF (.NOT. GW% chd(e)) THEN
            !### free GW boundary case
                ! fluxes are negative when mass leaves the element in GWSWEX; i.e. fluxes are negative when the storage in GWSWEX is greater than external storage
                CALL solve_t(e, lateral_GW_flux=lateral_GW_flux(e,:), lateral_SW_flux=lateral_SW_flux(e,:))

            END IF

        END DO
        !$OMP END PARALLEL DO

    END SUBROUTINE resolve_l



    SUBROUTINE fin()

        DEALLOCATE(paths, time, nelements, UZ, GW, SW, EXTF, SS)
        CALL logger% log(logger% INFO, "Deallocated memory. Exiting...")

    END SUBROUTINE fin

END MODULE model
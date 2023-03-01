! addnl. structural shorthands:
!	+ yp - yaml object, parent
!	+ yc - yaml object, child
!	+ F - file



SUBROUTINE build(Fyaml_path)
	USE YAMLInterface
	USE YAMLRead
	USE datetime_module, only: strptime, timedelta, datetime

	IMPLICIT NONE

	CHARACTER(256), INTENT(IN) :: Fyaml_path

	TYPE(YAMLHandler) :: fyaml
	TYPE(YAMLMap) :: yp_model, yp_paths, yp_util
	TYPE(YAMLMap) :: yc_model_domain, yc_path_dirs, yc_path_files, yc_util_logger, yc_model_domain_lays_vanG, yc_model_ic, &
					 yc_model_bnd, yc_model_extf, yc_model_solver
	TYPE(YAMLMap), DIMENSION(:), ALLOCATABLE :: yc_model_domain_lays
	INTEGER :: ires, Gdt_copy

	CHARACTER(256) :: strbuffer, strbuffer2, fpath
	REAL(REAL64), DIMENSION(4) :: vanG_pars
	REAL(REAL64), DIMENSION(:), ALLOCATABLE :: r64temp1d
	REAL(REAL64), DIMENSION(:,:), ALLOCATABLE :: r64temp2d
	INTEGER(INT8) :: i8temp

	time% wall_start = time% wall_start% now()

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
	logger% level = INT(yc_util_logger% value_int("level", ires), kind=INT8)
	IF (ires /= 0) THEN
		ERROR STOP "ERROR: logger level not found/incorrect in config file"
	END IF
	logger% unit = tu

	WRITE(strbuffer, *) yc_util_logger% value_str("fname", ires)
	logger% fname = strbuffer
	IF (ires /= 0) THEN
		logger% fname = "GWSWEX.log"
	END IF
	logger% fpath = TRIM(ADJUSTL(paths% output))//"/"//TRIM(ADJUSTL(logger% fname))

	CALL logger% init()
	CALL logger% log(logger% info, "Initializing model from config file")

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
	CALL logger% log(logger% moreinfo, TRIM(ADJUSTL(strbuffer)))

	! allocate the UZ_
	ALLOCATE(UZ_(nelements))

	time% Gdt = timedelta(seconds = yc_model_domain% value_int("dt", ires))
	IF (ires /= 0) THEN
		ERROR STOP "ERROR: dt not defined in config file"
		CALL logger% log(logger%fatal, "dt not defined in config file")
	END IF
	WRITE(strbuffer, *) "Initializing model with a global time step of", time% Gdt% total_seconds(), " s."
	CALL logger% log(logger% moreinfo, TRIM(ADJUSTL(strbuffer)))

	WRITE(strbuffer, *) yc_model_domain% value_str("tstart", ires)
	IF (ires /= 0) THEN
		ERROR STOP "ERROR: tstart not defined in config file"
		CALL logger% log(logger%fatal, "tstart not defined in config file")
	END IF
	time% Gstart = strptime(strbuffer, "%Y%m%d %H%M%S")
	WRITE(strbuffer, *) "Simulation start time: ", time% Gstart% isoformat()
	CALL logger% log(logger% moreinfo, TRIM(ADJUSTL(strbuffer)))

	WRITE(strbuffer, *) yc_model_domain% value_str("tstop", ires)
	IF (ires /= 0) THEN
		ERROR STOP "ERROR: tstop not defined in config file"
		CALL logger% log(logger%fatal, "tstop not defined in config file")
	END IF
	time% Gstop = strptime(strbuffer, "%Y%m%d %H%M%S")
	WRITE(strbuffer, *) "Simulation stop time: ", time% Gstop% isoformat()
	CALL logger% log(logger% moreinfo, TRIM(ADJUSTL(strbuffer)))

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
	CALL logger% log(logger% moreinfo, TRIM(ADJUSTL(strbuffer)))

	time% current = time% Gstart
	time% elapsed = timedelta(seconds = 0)
	time% Gts = 1

	time% Lstart = time% Gstart
	time% Lstop = time% Gstart + time% Gdt
	ALLOCATE(time% Lts)
	time% Lts = 1

	! allocating global storages
	CALL logger% log(logger% debug, "Allocating global UZ, GW and SW storages")
	ALLOCATE(GW% Gstorage(nelements, time% Gnts+1), UZ% Gstorage(nelements, time% Gnts+1), UZ% Gepv(nelements, time% Gnts+1), SW% Gstorage(nelements, time% Gnts+1))

	UZ% nlay = yc_model_domain% value_int("nlay", ires)
	IF (ires /= 0) THEN
		ERROR STOP "ERROR: nlay not defined in config file"
		CALL logger% log(logger%fatal, "nlay not defined in config file")
	END IF
	WRITE(strbuffer, *) "Initializing model with ", UZ% nlay, " layers"
	CALL logger% log(logger% moreinfo, TRIM(ADJUSTL(strbuffer)))
	WRITE(strbuffer, *) "Model initialized with ", UZ% nlay, " layers"
	CALL logger% log(logger% moreinfo, TRIM(ADJUSTL(strbuffer)))

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

	CALL logger% log(logger% moreinfo, "Layer elevations read")
	FLUSH(logger% unit)

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
		! (?) TODO: check if all underlying layers are active when one layer is declared as active

		ALLOCATE(UZ% layer(l)% Aubound(nelements), UZ% layer(l)% Albound(nelements))
		IF (l == 1) THEN
			UZ% layer(l)% Aubound => UZ% top
		ELSE
			UZ% layer(l)% Aubound => UZ% bot(l-1, :)
		END IF
		UZ% layer(l)% Albound => UZ% bot(l, :)
		! TODO: check if all bots lie below the tops or are at least equal to the top

		! read the vanGenuchten parameters
		vanG_pars = yc_model_domain_lays(l)% value_double_1d("vanG", ires)

		ALLOCATE(UZ% layer(l)% vanG)
		ALLOCATE(UZ% layer(l)% vanG% alpha, UZ% layer(l)% vanG% n, UZ% layer(l)% vanG% theta_r, UZ% layer(l)% vanG% theta_s)

		UZ% layer(l)% vanG% alpha = vanG_pars(1)
		UZ% layer(l)% vanG% n = vanG_pars(2)
		UZ% layer(l)% vanG% theta_r = vanG_pars(3)
		UZ% layer(l)% vanG% theta_s = vanG_pars(4)

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
		CALL logger% log(logger% debug, TRIM(ADJUSTL(strbuffer)))
	END DO

	CALL logger% log(logger% moreinfo, "Layer properties read")


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
	ALLOCATE(EXTF% p(nelements, time% Gnts), EXTF% et(nelements, time% Gnts))

	IF (yc_path_files% value_int("EXTF.p", ires) == 0) THEN
		EXTF% p = yc_model_extf% value_double_2d("p", ires)
	ELSE
		WRITE(strbuffer, *) yc_model_extf% value_str("p", ires)
		fpath = TRIM(ADJUSTL(paths% input))//"/"//ADJUSTL(TRIM(strbuffer))
		OPEN(UNIT=tu, FILE=TRIM(fpath), FORM='UNFORMATTED', ACTION='READ')
		READ(tu) EXTF% p
		CLOSE (UNIT=tu)
	END IF

	IF (yc_path_files% value_int("EXTF.et", ires) == 0) THEN
		EXTF% et = yc_model_extf% value_double_2d("et", ires)
	ELSE
		WRITE(strbuffer, *) yc_model_extf% value_str("et", ires)
		fpath = TRIM(ADJUSTL(paths% input))//"/"//ADJUSTL(TRIM(strbuffer))
		OPEN(UNIT=tu, FILE=TRIM(fpath), FORM='UNFORMATTED', ACTION='READ')
		READ(tu) EXTF% et
		CLOSE (UNIT=tu)
	END IF
	CALL yc_model_extf% destroy()

	! read solver settings
	yc_model_solver = yp_model% value_map("solver settings")

	i8temp = SIZE(yc_model_solver% value_double_1d("pet_intensities", ires))
	ALLOCATE(solver_settings% pet_intensities(i8temp), solver_settings% pet_nts(i8temp))

	solver_settings% pet_intensities = yc_model_solver% value_double_1d("pet_intensities", ires)
	solver_settings% pet_nts = yc_model_solver% value_int_1d("pet_nts", ires)

	CALL yc_model_solver% destroy()

	! TODO: read [gw_tolerance, sw_tolerance, sm_gw_fluctuation_tolerance]
	! TODO: add option under utils to set the number of OMP threads and precision for input files, vars, and output files

	CALL yc_path_files% destroy()
	CALL yp_model% destroy()

	! $OMP PARALLEL DO
	DO e = 1, nelements
write(*,*) "!!! loop start !!!"
		CALL UZ_(e)% init(e, UZ, GW, time)
		write(*,*) "!!! called !!!"
	END DO
	! $OMP END PARALLEL DO
write(*,*) "!!! DONE !!!"

	CALL yaml_close_file(fyaml)

	CALL logger% log(logger%info, "Model built successfully")

	FLUSH(logger% unit)

END SUBROUTINE
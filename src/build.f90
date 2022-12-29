! addnl. structural shorthands:
!	+ yp - yaml object, parent
!	+ yc - yaml object, child
!	+ F - file



SUBROUTINE build(Fyaml_path)
	USE YAMLInterface
	USE YAMLRead
	USE datetime_module, only: strptime, timedelta, datetime

	IMPLICIT NONE

	CHARACTER(*), INTENT(IN) :: fyaml_path

	TYPE(YAMLHandler) :: fyaml
	TYPE(YAMLMap) :: yp_model, yp_paths, yp_util
	TYPE(YAMLMap) :: yc_model_domain, yc_path_dirs, yc_path_files, yc_util_logger, yc_model_domain_lays_vanG, yc_model_domain_ic
	TYPE(YAMLMap), DIMENSION(:), ALLOCATABLE :: yc_model_domain_lays
	INTEGER :: ires

	LOGICAL :: chk

	CHARACTER(255) :: strbuffer, fpath

	paths% config = TRIM(fyaml_path)



	! start reading the yaml file
	fyaml = yaml_open_file(fyaml_path)


	! read, check, and store the paths
	yp_paths = yaml_start_from_map(fyaml, 'paths')
	yc_path_dirs = yp_paths% value_map('dirs')
	yc_path_files = yp_paths% value_map('files')

	WRITE(strbuffer, *) yc_path_dirs% value_str("root", ires)
	paths% root = strbuffer
	IF (ires /= 0) THEN
		ERROR STOP "ERROR: root path not found in config file"
	END IF
	INQUIRE(FILE=TRIM(paths% root), EXIST=chk)
	IF (.NOT. chk) THEN
		ERROR STOP "ERROR: specified root dir does not exist"
	END IF

	WRITE(strbuffer, *) yc_path_dirs% value_str("input", ires)
	paths% input = strbuffer
	IF (ires /= 0) THEN
		ERROR STOP "ERROR: input path not found in config file"
	END IF
	INQUIRE(FILE=TRIM(paths% input), EXIST=chk)
	IF (.NOT. chk) THEN
		ERROR STOP "ERROR: specified input dir does not exist"
	END IF

	WRITE(strbuffer, *) yc_path_dirs% value_str("output", ires)
	paths% output = strbuffer
	IF (ires /= 0) THEN
		ERROR STOP "ERROR: output path not found in config file"
	END IF
	INQUIRE(FILE=TRIM(paths% output), EXIST=chk)
	IF (.NOT. chk) THEN
		ERROR STOP "ERROR: specified output dir does not exist"
	END IF

	WRITE(strbuffer, *) yc_path_files% value_str("log", ires)
	logger% fpath = strbuffer
	IF (ires /= 0) THEN
		logger% fpath = 'GWSWEX.log'
	END IF
	logger% fpath = TRIM(paths% output)//TRIM(logger% fpath)

	CALL yc_path_dirs% destroy()
	CALL yp_paths% destroy()


	! read and set the logger level and init logger
    yp_util = yaml_start_from_map(fyaml, 'utils')
	yc_util_logger = yp_util% value_map('logger')
	logger% level = INT(yc_util_logger% value_int("level", ires), kind=INT8)
	IF (ires /= 0) THEN
		ERROR STOP "ERROR: logger level not found/incorrect in config file"
	END IF
	logger% unit = tu
	CALL logger% init()
	CALL logger% log(logger% info, "Initializing model from config file")
	CALL yp_util% destroy()
	CALL yc_util_logger% destroy()


	! read and set the model parameters
	yp_model = yaml_start_from_map(fyaml, 'model')
	yc_model_domain = yp_model% value_map('domain')
	
	nelements = yc_model_domain% value_int("nelements", ires)
	IF (ires /= 0) THEN
		ERROR STOP "ERROR: nelements not defined in config file"
		CALL logger% log(logger% fatal, "nelements not defined in config file")
	END IF
	WRITE(strbuffer, *) "Initializing model with ", nelements, " elements"
	CALL logger% log(logger% moreinfo, TRIM(strbuffer))

	! allocate the UZ_
	ALLOCATE(UZ_(nelements))

	time% Gdt = timedelta(seconds = yc_model_domain% value_int("dt", ires))
	IF (ires /= 0) THEN
		ERROR STOP "ERROR: dt not defined in config file"
		CALL logger% log(logger%fatal, "dt not defined in config file")
	END IF
	WRITE(strbuffer, *) "Initializing model with a global time step of", time% Gdt% total_seconds(), " s."
	CALL logger% log(logger% moreinfo, TRIM(strbuffer))

	WRITE(strbuffer, *) yc_model_domain% value_str("tstart", ires)
	IF (ires /= 0) THEN
		ERROR STOP "ERROR: tstart not defined in config file"
		CALL logger% log(logger%fatal, "tstart not defined in config file")
	END IF
	time% Gstart = strptime(strbuffer, "%Y%m%d %H%M%S")
	WRITE(strbuffer, *) "Model start time: ", time% Gstart% isoformat()
	CALL logger% log(logger% moreinfo, TRIM(strbuffer))

	WRITE(strbuffer, *) yc_model_domain% value_str("tend", ires)
	IF (ires /= 0) THEN
		ERROR STOP "ERROR: tstop not defined in config file"
		CALL logger% log(logger%fatal, "tstop not defined in config file")
	END IF
	WRITE(strbuffer, *) "Model end time: ", time% Gend% isoformat()
	CALL logger% log(logger% moreinfo, TRIM(strbuffer))

	time% Gend = strptime(strbuffer, "%Y%m%d %H%M%S")
	time% scratch_td = time% Gend - time% Gstart
	time% Gnts = INT(time% scratch_td% total_seconds()/time% Gdt% total_seconds(), kind=INT32)
	IF (time% Gnts < 0) THEN
		ERROR STOP "ERROR: tstop < tstart"
		CALL logger% log(logger%fatal, "tstop < tstart")
	END IF
	WRITE(strbuffer, *) "Model will run for ", time% Gnts, " time steps"
	CALL logger% log(logger% moreinfo, TRIM(strbuffer))
	time% Lstart = time% Gstart
	time% Lend = time% Gstart + time% Gdt
	time% Gts = 1
	
	! allocating global storages
	CALL logger% log(logger% debug, "Allocating global UZ, GW and SW storages")
	ALLOCATE(GW% Gstorage(nelements, time% Gnts+1), UZ% Gstorage(nelements, time% Gnts+1), UZ% Gepv(nelements, time% Gnts+1), SW% Gstorage(nelements, time% Gnts+1))
	DO e = 1, nelements
		DO l = 1, UZ_(e)% nlay
			ALLOCATE(UZ_(e)% SM(l)% Gstorage(nelements, time% Gnts+1))
		END DO
	END DO

	UZ% nlay = yc_model_domain% value_int("nlay", ires)
	IF (ires /= 0) THEN
		ERROR STOP "ERROR: nlay not defined in config file"
		CALL logger% log(logger%fatal, "nlay not defined in config file")
	END IF
	WRITE(strbuffer, *) "Initializing model with ", UZ% nlay, " layers"
	CALL logger% log(logger% moreinfo, TRIM(strbuffer))

	! allocate the UZ layers, top, and bottom
	ALLOCATE(UZ% layer(UZ% nlay), UZ% top(nelements), UZ% bot(UZ% nlay, nelements))

	! read the UZ layer elevations
	UZ% top = yc_model_domain% value_double_1d("top", ires)
	IF (ires /= 0) THEN
		WRITE(strbuffer, *) yc_model_domain% value_str("top", ires)
		fpath = TRIM(strbuffer)
		IF (ires /= 0) THEN
			ERROR STOP "ERROR: top not defined in config file"
			CALL logger% log(logger%fatal, "top not defined in config file")
		END IF
		fpath = TRIM(paths% input)//TRIM(fpath)
		OPEN(UNIT=tu, FILE=TRIM(fpath), FORM='UNFORMATTED', ACTION='READ', IOSTAT=ires)
		READ(tu, *) UZ% top
		CLOSE (UNIT=tu)
		IF (ires /= 0) THEN
			ERROR STOP "ERROR: top file not found"
			CALL logger% log(logger%fatal, "top file not found")
		END IF
	END IF

	UZ% bot = yc_model_domain% value_double_2d("bot", ires)
	IF (ires /= 0) THEN
		WRITE(strbuffer, *) yc_model_domain% value_str("bot", ires)
		fpath = TRIM(strbuffer)
		IF (ires /= 0) THEN
			ERROR STOP "ERROR: bot not defined in config file"
			CALL logger% log(logger%fatal, "bot not defined in config file")
		END IF
		OPEN(UNIT=tu, FILE=TRIM(fpath), FORM='UNFORMATTED', ACTION='READ', IOSTAT=ires)
		READ(tu, *) UZ% bot
		CLOSE (UNIT=tu)
		IF (ires /= 0) THEN
			ERROR STOP "ERROR: bot file not found"
			CALL logger% log(logger%fatal, "bot file not found")
		END IF
	END IF

	ALLOCATE(yc_model_domain_lays(UZ% nlay))

	DO l = 1, UZ% nlay
		WRITE(strbuffer, *) 'layer', l

		yc_model_domain_lays(l) = yc_model_domain% value_map(strbuffer)

		WRITE(strbuffer, *) yc_model_domain_lays(l)% value_str("name", ires)
		UZ% layer(l)% name = TRIM(strbuffer)
		IF (ires /= 0) THEN
			ERROR STOP "ERROR: undefined name for " // TRIM(strbuffer) // " in config file"
			CALL logger% log(logger%fatal, "undefined name for " // TRIM(strbuffer) // " in config file")
		END IF

		ALLOCATE(UZ% layer(l)% isactive(nelements))
		UZ% layer(l)% isactive = yc_model_domain_lays(l)% value_int_1d("isactive", ires)
		IF (ires /= 0) THEN
			WRITE(strbuffer, *) yc_model_domain% value_str("isactive", ires)
			fpath = TRIM(strbuffer)
			IF (ires /= 0) THEN
				ERROR STOP "ERROR: undefined isactive in config file"
				CALL logger% log(logger%fatal, "undefined isactive for " // TRIM(strbuffer) // " in config file")
			END IF
			OPEN(UNIT=tu, FILE=TRIM(fpath), FORM='UNFORMATTED', ACTION='READ', IOSTAT=ires)
			READ(tu, *) UZ% layer(l)% isactive
			CLOSE (UNIT=tu)
			IF (ires /= 0) THEN
				ERROR STOP "ERROR: undefined isactive in " // TRIM(strbuffer) // " in config file"
				CALL logger% log(logger%fatal, "undefined isactive for " // TRIM(strbuffer) // " in config file")
			END IF
		END IF
		
		ALLOCATE(UZ% layer(l)% Aubound(nelements), UZ% layer(l)% Albound(nelements))
		IF (l == 1) THEN
			UZ% layer(l)% Aubound => UZ% top
		ELSE
			UZ% layer(l)% Aubound => UZ% bot(l-1, :)
		END IF
		UZ% layer(l)% Albound => UZ% bot(l, :)

		yc_model_domain_lays_vanG = yc_model_domain_lays(l)% value_map("vanG")
		IF (ires /= 0) THEN
			ERROR STOP "ERROR: undefined vanG for " // TRIM(strbuffer) // " in config file"
			CALL logger% log(logger%fatal, "undefined vanG for " // TRIM(strbuffer) // " in config file")
		END IF
		UZ% layer(l)% vanG% alpha = yc_model_domain_lays_vanG% value_double("alpha", ires)
		IF (ires /= 0) THEN
			ERROR STOP "ERROR: undefined vanG_alpha for " // TRIM(strbuffer) // " in config file"
			CALL logger% log(logger%fatal, "undefined vanG_alpha for " // TRIM(strbuffer) // " in config file")
		END IF
		UZ% layer(l)% vanG% n = yc_model_domain_lays_vanG% value_double("n", ires)
		IF (ires /= 0) THEN
			ERROR STOP "ERROR: undefined vanG_n for " // TRIM(strbuffer) // " in config file"
			CALL logger% log(logger%fatal, "undefined vanG_n for " // TRIM(strbuffer) // " in config file")
		END IF
		UZ% layer(l)% vanG% m = yc_model_domain_lays_vanG% value_double("m", ires)
		IF (ires /= 0) THEN
			ERROR STOP "ERROR: undefined vanG_m for " // TRIM(strbuffer) // " in config file"
			CALL logger% log(logger%fatal, "undefined vanG_m for " // TRIM(strbuffer) // " in config file")
		END IF
		UZ% layer(l)% vanG% theta_r = yc_model_domain_lays_vanG% value_double("theta_r", ires)
		IF (ires /= 0) THEN
			ERROR STOP "ERROR: undefined vanG_theta_r for " // TRIM(strbuffer) // " in config file"
			CALL logger% log(logger%fatal, "undefined vanG_theta_r for " // TRIM(strbuffer) // " in config file")
		END IF
		UZ% layer(l)% vanG% theta_s = yc_model_domain_lays_vanG% value_double("theta_s", ires)
		IF (ires /= 0) THEN
			ERROR STOP "ERROR: undefined vanG_theta_s for " // TRIM(strbuffer) // " in config file"
			CALL logger% log(logger%fatal, "undefined vanG_theta_s for " // TRIM(strbuffer) // " in config file")
		END IF

		ALLOCATE(UZ% layer(l)% ks(nelements), UZ% layer(l)% porosity(nelements))
		UZ% layer(l)% ks = yc_model_domain_lays(l)% value_double_1d("ks", ires)
		IF (ires /= 0) THEN
			ERROR STOP "ERROR: undefined ks for " // TRIM(strbuffer) // " in config file"
			CALL logger% log(logger%fatal, "undefined ks for " // TRIM(strbuffer) // " in config file")
		END IF
		UZ% layer(l)% porosity = yc_model_domain_lays(l)% value_double_1d("porosity", ires)
		IF (ires /= 0) THEN
			ERROR STOP "ERROR: undefined porosity for " // TRIM(strbuffer) // " in config file"
			CALL logger% log(logger%fatal, "undefined porosity for " // TRIM(strbuffer) // " in config file")
		END IF

		CALL yc_model_domain_lays_vanG% destroy()
		CALL yc_model_domain_lays(l)% destroy()
	END DO

	ALLOCATE(EXTF% chd(nelements))
	EXTF% chd = yc_model_domain% value_int_1d("GW_chd", ires)
	IF (ires /= 0) THEN
		WRITE(strbuffer, *) yc_model_domain% value_str("GW_chd", ires)
		fpath = TRIM(strbuffer)
		IF (ires /= 0) THEN
			ERROR STOP "ERROR: undefined GW_chd in config file"
			CALL logger% log(logger%fatal, "undefined GW_chd in config file")
		END IF
		OPEN(UNIT=tu, FILE=TRIM(fpath), FORM='UNFORMATTED', ACTION='READ', IOSTAT=ires)
		READ(tu, *) EXTF% chd
		CLOSE (UNIT=tu)
		IF (ires /= 0) THEN
			ERROR STOP "ERROR: error opening GW_chd file"
			CALL logger% log(logger%fatal, "error opening GW_chd file")
		END IF
	END IF

	yc_model_domain_ic = yc_model_domain% value_map("initial_conditions")
	GW% Gstorage(:, 1) = yc_model_domain_ic% value_double_1d("GW", ires)
	IF (ires /= 0) THEN
		WRITE(strbuffer, *) yc_model_domain_ic% value_str("GW", ires)
		fpath = TRIM(strbuffer)
		IF (ires /= 0) THEN
			ERROR STOP "ERROR: undefined GW_ini in config file"
			CALL logger% log(logger%fatal, "undefined GW_ini in config file")
		END IF
		OPEN(UNIT=tu, FILE=TRIM(fpath), FORM='UNFORMATTED', ACTION='READ', IOSTAT=ires)
		READ(tu, *) GW% Gstorage(:, 1)
		CLOSE (UNIT=tu)
		IF (ires /= 0) THEN
			ERROR STOP "ERROR: error reading GW_ini in config file"
			CALL logger% log(logger%fatal, "error reading GW_ini in config file")
		END IF
	END IF

	SW% Gstorage(:, 1) = yc_model_domain_ic% value_double_1d("SW", ires)
	IF (ires /= 0) THEN
		WRITE(strbuffer, *) yc_model_domain_ic% value_str("SW", ires)
		fpath = TRIM(strbuffer)
		IF (ires /= 0) THEN
			ERROR STOP "ERROR: undefined SW_ini in config file"
			CALL logger% log(logger%fatal, "undefined SW_ini in config file")
		END IF
		OPEN(UNIT=tu, FILE=TRIM(fpath), FORM='UNFORMATTED', ACTION='READ', IOSTAT=ires)
		READ(tu, *) SW% Gstorage(:, 1)
		CLOSE (UNIT=tu)
		IF (ires /= 0) THEN
			ERROR STOP "ERROR: error reading SW_ini in config file"
			CALL logger% log(logger%fatal, "error reading SW_ini in config file")
		END IF
	END IF

	UZ% Gstorage(:, 1) = yc_model_domain_ic% value_double_1d("UZ", ires)
	IF (ires /= 0) THEN
		WRITE(strbuffer, *) yc_model_domain_ic% value_str("UZ", ires)
		! TODO: allow setting UZ to 'eq' to internally calculate UZ from GWS and vanG
		fpath = TRIM(strbuffer)
		IF (ires /= 0) THEN
			ERROR STOP "ERROR: undefined UZ_ini in config file"
			CALL logger% log(logger%fatal, "undefined UZ_ini in config file")
		END IF
		OPEN(UNIT=tu, FILE=TRIM(fpath), FORM='UNFORMATTED', ACTION='READ', IOSTAT=ires)
		READ(tu, *) UZ% Gstorage(:, 1)
		CLOSE (UNIT=tu)
		IF (ires /= 0) THEN
			ERROR STOP "ERROR: error reading UZ_ini in config file"
			CALL logger% log(logger%fatal, "error reading UZ_ini in config file")
		END IF
	END IF


	CALL yc_model_domain% destroy()
	CALL yp_model% destroy()

	DO e = 1, nelements
		CALL UZ_(e)% init(UZ, e)
		CALL UZ_(e)% setup(UZ, GW, e)
	END DO

	CALL yaml_close_file(fyaml)

	CALL logger% log(logger%info, "Model built successfully")
	FLUSH(logger% unit)

	! TODO: INITIAL CONDITIONS
	!store at Gstorage(1,:) and point Lstorage(1,:) to it during build. at init, point the Lstorage to the Gstorage(2,:)
	!store the 1st ts from Lstorage into Gstorage(2,:)

END SUBROUTINE
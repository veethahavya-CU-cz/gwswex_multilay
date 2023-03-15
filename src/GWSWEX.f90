! variable shorthands:
!	+ G 	- 	global, L - local
!	+ R 	- 	relative, A - absolute
!	+ [] 	- 	units/dimensions (S.I. units/dimensions of the model: e - element, t - timestep)
!	+ {} 	- 	valid range/definition

! structural shorthands:
!	+ M 	- 	module
!	+ C 	- 	class

! file shorthands:
!	+ P 	- 	procedures


! #TODO: include a helper subroutine here to pass a variable as pointer to GWSWEX wrapper


MODULE Mtiming
! handles model and clock timing operations and manipulations
	USE iso_fortran_env, ONLY: REAL32, REAL64, REAL128, INT8, INT16, INT32, INT64
	USE datetime_module, ONLY: datetime, timedelta

	IMPLICIT NONE

	TYPE Ctime
	! to store and access temporal information of the model
		TYPE(datetime) :: Gstart, Gstop, Lstart, Lstop, current, wall_start, scratch_dt
		TYPE(timedelta) :: Gdt, Ldt, elapsed, wall_elapsed, scratch_td
		INTEGER(INT32) :: Gnts, Gts, Lnts
		INTEGER(INT32), POINTER :: Lts
	END TYPE Ctime

	! #TODO: add a pass procedure to move a ts ahead

END MODULE Mtiming





MODULE Mpaths
! to store and access model file/dir paths
	! root: 		absolute path to the root directory of the model
	! input: 		absolute path to the input directory of the model
	! output: 		absolute path to the output directory of the model
	USE iso_fortran_env, ONLY: REAL32, REAL64, REAL128, INT8, INT16, INT32, INT64

	IMPLICIT NONE

	INTEGER, PARAMETER  :: STRLEN=256

	TYPE Cpaths
		CHARACTER(LEN=STRLEN) :: root, input, output, config
	END TYPE Cpaths

END MODULE Mpaths





MODULE Mlogger
! handles logging operations
	USE iso_fortran_env, ONLY: REAL32, REAL64, REAL128, INT8, INT16, INT32, INT64
	USE datetime_module, ONLY: datetime

	IMPLICIT NONE

	INTEGER, PARAMETER  :: STRLEN=256

	TYPE Clogger
	! to store and access logging information of the model
		INTEGER(INT8) :: unit, level, info, moreinfo, trace, debug, warn, error, fatal
		CHARACTER(LEN=STRLEN) :: fpath, fname
		TYPE(datetime) :: timer
		CONTAINS
			PROCEDURE :: init
			PROCEDURE :: log_real32
			PROCEDURE :: log_real64
			PROCEDURE :: log_real128
			PROCEDURE :: log_int
			PROCEDURE :: log_int8
			PROCEDURE :: log_int_middle
			PROCEDURE :: log_int8_middle
			PROCEDURE :: log_str
			GENERIC :: log => log_real32, log_real64, log_real128, log_int, log_int8, log_int_middle, log_int8_middle, log_str
	END TYPE Clogger

	CONTAINS
		INCLUDE 'Plogger.f90'

END MODULE Mlogger





MODULE Muz
! handles van Genuchten and -Mualem model operations and manipulations
	USE iso_fortran_env, ONLY: REAL32, REAL64, REAL128, INT8, INT16, INT32, INT64
	USE YAMLInterface
	USE YAMLRead
	USE Mlogger, ONLY: Clogger

	IMPLICIT NONE

	REAL(REAL64), POINTER :: alpha, n, m, theta_r, theta_s
	TYPE(Clogger), POINTER :: plogger_Muz
	
	TYPE CvanG
	! to store, access, and calculate van Genuchten and -Mualem model parameters
		! saturation: [m3/m3]
		REAL(REAL64), POINTER :: alpha, n, m, theta_r, theta_s
		CONTAINS
			PROCEDURE, PASS :: init
			PROCEDURE, PASS :: setvars
			PROCEDURE, NOPASS :: integrate
			PROCEDURE, PASS :: kUS
	END TYPE CvanG

	TYPE Clayer
	! to store, access, and manipulate UZ layer properties
		CHARACTER(LEN=64) :: name
		LOGICAL, DIMENSION(:), ALLOCATABLE :: isactive
		REAL(REAL64), DIMENSION(:), POINTER :: Aubound, Albound
		TYPE(CvanG), POINTER :: vanG
		REAL(REAL64), DIMENSION(:), POINTER :: ks, porosity
	END TYPE Clayer

	CONTAINS
		INCLUDE 'PvanGI_M.f90'

END MODULE Muz





MODULE Msolver
	USE iso_fortran_env, ONLY: REAL32, REAL64, REAL128, INT8, INT16, INT32, INT64

	TYPE Csettings
		INTEGER(INT16) :: max_iterations
		REAL(REAL64) :: gw_tolerance, sw_tolerance, sm_gw_fluctuation_tolerance
		REAL(REAL64), DIMENSION(:), ALLOCATABLE :: pet_intensities
		INTEGER(INT8), DIMENSION(:), ALLOCATABLE :: pet_nts
	END TYPE Csettings
END MODULE Msolver





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
		REAL(REAL128), DIMENSION(:,:), ALLOCATABLE :: Ldischarge

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
		LOGICAL, DIMENSION(:), ALLOCATABLE :: chd
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
		!? #TODO: add option to specify whether the layers are physical or virtual to enable single vanGenuchten parameter set for all layers
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
		INCLUDE 'Puz_.f90'

END MODULE Mstorages





MODULE model
	USE iso_fortran_env, ONLY: REAL32, REAL64, REAL128, INT8, INT16, INT32, INT64
	USE Mpaths, ONLY: Cpaths
	USE Mlogger, ONLY: Clogger
	USE Mtiming, ONLY: Ctime
	USE Muz, ONLY: Clayer
	USE Mstorages, ONLY: Cuz, Cgw, Csw, Cuz_, Csm, Cext_forcings
	USE Msolver, ONLY: Csettings

	IMPLICIT NONE

	TYPE(Cpaths) :: paths
	TYPE(Clogger), POINTER :: logger

	INTEGER(INT32)  :: nelements !#TODO: add var(s) to store elem id from MF6 and DFM
	TYPE(Ctime) :: time

	TYPE(Cuz) :: UZ
	TYPE(Cgw) :: GW
	TYPE(Csw) :: SW

	TYPE(Cuz_), DIMENSION(:), ALLOCATABLE :: UZ_

	TYPE(Cext_forcings) :: EXTF

	TYPE(Csettings) :: SS
	
	INTEGER, PARAMETER  :: lu=42, tu=99, STRLEN=256

	CONTAINS
		INCLUDE 'build.f90'
		INCLUDE 'init_ts.f90'
		INCLUDE 'solve_ts.f90'

END MODULE model
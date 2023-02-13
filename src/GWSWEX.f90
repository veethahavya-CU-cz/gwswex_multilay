! variable shorthands:
!	+ G 	- 	global, L - local
!	+ R 	- 	relative, A - absolute
!	+ [] 	- 	units/dimensions (S.I. units/dimensions of the model: e - element, t - timestep)
!	+ {} 	- 	valid range/definition

! structural shorthands:
!	+ M 	- 	module
!	+ C 	- 	class

! file shorthands:
!	+ P 	- 	procedure





MODULE Mtiming
! handles model and clock timing operations and manipulations
	USE iso_fortran_env, ONLY: REAL32, REAL64, REAL128, INT8, INT16, INT32, INT64
	USE omp_lib, ONLY: omp_get_wtime
	USE datetime_module, ONLY: datetime, timedelta

	IMPLICIT NONE

	TYPE Ctime
	! to store and access temporal information of the model
		TYPE(datetime) :: Gstart, Gstop, Lstart, Lstop, scratch_dt
		TYPE(timedelta) :: Gdt, Ldt, scratch_td
		INTEGER(INT32) :: Gnts, Lnts, Gts, Lts
	END TYPE Ctime

	CONTAINS
		FUNCTION timefetch()
		! fetches wall clock time via OMP library
			IMPLICIT NONE
			REAL(REAL64) :: timefetch
			timefetch = omp_get_wtime()
		END FUNCTION timefetch

END MODULE Mtiming





MODULE Mpaths
! to store and access model file/dir paths
	! root: 		absolute path to the root directory of the model
	! idir: 		absolute path to the input directory of the model
	! odir: 		absolute path to the output directory of the model
	USE iso_fortran_env, ONLY: REAL32, REAL64, REAL128, INT8, INT16, INT32, INT64

	IMPLICIT NONE

	TYPE Cpaths
		CHARACTER(255) :: root, input, output, config
	END TYPE Cpaths

END MODULE Mpaths





MODULE Mlogger
! handles logging operations
	USE iso_fortran_env, ONLY: REAL32, REAL64, REAL128, INT8, INT16, INT32, INT64
	USE datetime_module, ONLY: datetime

	IMPLICIT NONE

	TYPE Clogger
	! to store and access logging information of the model
		INTEGER(INT8) :: unit, level, info, moreinfo, trace, debug, warn, error, fatal
		CHARACTER(LEN=256) :: fname, fpath
		TYPE(datetime) :: timer
		CONTAINS
			PROCEDURE :: init
			PROCEDURE :: log_real
			PROCEDURE :: log_int
			PROCEDURE :: log_str
			GENERIC :: log => log_real, log_int, log_str
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
	TYPE(Clogger) :: logger
	
	TYPE CvanG
	! to store, access, and calculate van Genuchten and -Mualem model parameters
		! saturation: [m3/m3]
		REAL(REAL64), POINTER :: alpha, n, m, theta_r, theta_s
		CONTAINS
			PROCEDURE, PASS :: init
			PROCEDURE, PASS :: setvars
			PROCEDURE, NOPASS :: integrate
			PROCEDURE, NOPASS :: kUS
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





MODULE Mstorages
! handles core GWSWEX operations and manipulations
	USE iso_fortran_env, ONLY: REAL32, REAL64, REAL128, INT8, INT16, INT32, INT64
	USE Muz, ONLY: CvanG, Clayer

	IMPLICIT NONE


	TYPE Cstorage
	! base storage type
		! storage:											[m] 	[e,t]								{relation defined by the child type}
		! discharge: 										[m/s] 	[e,t]								{ - }
		REAL(REAL128), DIMENSION(:,:), ALLOCATABLE :: Gstorage, Lstorage
		REAL(REAL128), DIMENSION(:,:), ALLOCATABLE :: discharge

	END TYPE Cstorage

	TYPE, EXTENDS(Cstorage) :: Cuz
	! to store and access unsturated zone storages
		! storages (locally relative):						[m] 										{0 - epv}
		! epv (effective pore volume): 						[m] 	[e,t]								{(Aubounds-Albounds)*porosity}
		! nlay (number of real vertical layers): 			[-]		[-] 								{1 - 128}
		INTEGER(INT8), POINTER :: nlay
		TYPE(Clayer), DIMENSION(:), ALLOCATABLE :: layer
		REAL(REAL64), DIMENSION(:), POINTER :: top
		REAL(REAL64), DIMENSION(:,:), POINTER :: bot
		REAL(REAL128), DIMENSION(:,:), ALLOCATABLE :: Gepv, Lepv
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


	TYPE :: Csm_
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
		INTEGER(INT8) :: lid
		TYPE(CvanG), POINTER :: vanG
		LOGICAL :: isactive, gw_bound
		REAL(REAL128), DIMENSION(:), ALLOCATABLE :: Lstorage, Gstorage !, discharge
		REAL(REAL64), ALLOCATABLE :: Rubound, Rlbound
		! REAL(REAL128) :: eq, ini
		REAL(REAL128), ALLOCATABLE :: saturation, saturation_ratio
		REAL(REAL128), ALLOCATABLE :: IC, ICrat
		REAL(REAL64), POINTER :: ks, porosity
		REAL(REAL128), ALLOCATABLE :: kus
	END TYPE Csm_

	TYPE Cuz_
	! to store, access, and manipulate lumped unsaturated zone parameters
		! nlay (number of real vertical layers): 			[-] 	[-]									{1 - 128}
		! Aubound, Albound (abs. upper and lower bounds): 	[m]		[nlay]								{relative to the defined datum}
		!? TODO: vnaly (number of virtual vertical layers): [-] 	[-]									{1 - 128; nlay*_int_}
		!? TODO: dz_weights (vlayer thickness weights): 	[-] 	[nlay]								{0 - 1; sum = 1}
		TYPE(Csm_), DIMENSION(:), ALLOCATABLE :: SM ! 				[nlay]
		INTEGER(INT8) :: nlay, gws_bnd_lid, gws_bnd_smid
		REAL(REAL64), POINTER :: Aubound, Albound
		REAL(REAL128) :: thickness
		LOGICAL :: isactive
		CONTAINS
			PROCEDURE, PASS :: init
			PROCEDURE, PASS :: resolve
			! PROCEDURE, PASS :: solve_dt
	END TYPE Cuz_

	TYPE Cext_forcings
	! to store, access, and manipulate external forcings
		! p (precipitation): 								[m/s]										{ - }
		! et (evapotranspiration): 							[m/s]										{ - }
		REAL(REAL64), DIMENSION(:,:), ALLOCATABLE :: p, et
	END TYPE Cext_forcings

	CONTAINS
		INCLUDE 'Puz_.f90'

END MODULE Mstorages





MODULE GWSWEX_model
	USE iso_fortran_env, ONLY: REAL32, REAL64, REAL128, INT8, INT16, INT32, INT64
	USE Mpaths, ONLY: Cpaths
	USE Mlogger, ONLY: Clogger
	USE Mtiming, ONLY: Ctime
	USE Muz, ONLY: Clayer
	USE Mstorages, ONLY: Cuz, Cgw, Csw, Cuz_, Cext_forcings

	IMPLICIT NONE

	TYPE(Cpaths) :: paths
	TYPE(Clogger) :: logger

	INTEGER(INT32)  :: nelements, e, t, l
	TYPE(Ctime) :: time

	TYPE(Cuz) :: UZ
	TYPE(Cgw) :: GW
	TYPE(Csw) :: SW

	TYPE(Cuz_), DIMENSION(:), ALLOCATABLE :: UZ_

	TYPE(Cext_forcings) :: EXTF
	
	INTEGER, PARAMETER  :: lu=42, tu=99

	CONTAINS
		INCLUDE 'build.f90'
		INCLUDE 'init_ts.f90'
		! !INCLUDE 'solve_ts.f90'

END MODULE GWSWEX_model
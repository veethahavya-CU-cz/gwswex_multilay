! This file is part of the vanGI_M module - MvanGI_M. It defines the procedures contained within the module.

SUBROUTINE init(self)
! initializes the van Genuchten model parameters
	CLASS(CvanG) :: self

	self% m = (1-(1/self% n))

	ALLOCATE(alpha, n, m, theta_r, theta_s)
END SUBROUTINE init


SUBROUTINE setvars(self)
! sets the van Genuchten model parameters for the module from the CvanG class variables
	CLASS(CvanG) :: self

	alpha => self% alpha
	n => self% n
	m => self% m
	theta_r => self% theta_r
	theta_s => self% theta_s
END SUBROUTINE setvars


FUNCTION kUS(s, ks)
! van Genuchten-Mualem model for unsaturated hydraulic conductivity - accepts a scalar saturation value and returns a scalar unsaturated hydraulic conductivity value
		REAL(REAL128), INTENT(IN) :: s
		REAL(REAL64), INTENT(IN) :: ks
		REAL(REAL128) :: kUS, sat
		REAL(REAL32), PARAMETER :: sat_relaxation = 1e-3

		! saturation relaxation introduced for numerical stability in cases where sat <= theta_r to avoid returning a kUS value of 0
		IF(s < theta_r .OR. s == theta_r) THEN
			sat = ((sat_relaxation)/(theta_s-theta_r))
			CALL plogger_Muz% log(plogger_Muz% trace, "saturation relaxation applied")
		ELSE
			sat = ((s-theta_r)/(theta_s-theta_r))
		END IF
		kUS = ks*sat*((1-(1-(sat)**(1/m))**m)**2)

		! fix for cases where kUS is NaN, i.e. when s >= theta_s
		IF(ISNAN(kUS)) THEN
			kUS = ks
			CALL plogger_Muz% log(plogger_Muz% trace, "kUS is NaN, setting kUS = ks")
		END IF
END FUNCTION kUS


FUNCTION theta_c(h_c, ptr_c) BIND(C)
! van Genuchten model for soil moisture content - accepts a scalar matric head value (z-formulation) and returns a scalar soil moisture content value
	USE, INTRINSIC :: iso_c_binding
	USE fgsl
	
	REAL(c_double), value :: h_c
	TYPE(c_ptr), value :: ptr_c
	REAL(c_double), pointer :: ptr_f
	REAL(c_double) :: theta_c

	CALL c_f_pointer(ptr_c, ptr_f)

	theta_c = theta_r + ((theta_s - theta_r)/((1+(alpha*(abs(h_c)))**n))**m)
END FUNCTION theta_c


FUNCTION integrate(ub, lb)
! integrates the van Genuchten model to calculate total depth-averaged soil moisture content for a given depth below the ground surface using the Fortran-GSL library [https://github.com/reinh-bader/fgsl]
	USE fgsl
	USE, INTRINSIC :: iso_c_binding

	REAL(REAL64) :: integrate
	REAL(REAL64), INTENT(IN) :: ub, lb
	INTEGER(fgsl_size_t), PARAMETER :: nmax=1000
	REAL(fgsl_double), TARGET :: ptr
	REAL(fgsl_double) :: result, error
	INTEGER(fgsl_int) :: status
	TYPE(c_ptr) :: cptr
	TYPE(fgsl_function) :: f_obj
	TYPE(fgsl_integration_workspace) :: wk

	ptr = 1.0D0
	cptr = c_loc(ptr)
	f_obj = fgsl_function_init(theta_c, cptr)
	wk = fgsl_integration_workspace_alloc(nmax)

	status = fgsl_integration_qags(f_obj, ub, lb, 0.0_fgsl_double, 1.0e-7_fgsl_double, nmax, wk, result, error)
	IF (status /= 0) THEN
		CALL plogger_Muz% log(plogger_Muz% fatal, "ERROR: fgsl integration failed")
		ERROR STOP "ERROR: integration failed"
	END IF
	integrate = result

	CALL fgsl_function_free(f_obj)
	CALL fgsl_integration_workspace_free(wk)

END FUNCTION integrate
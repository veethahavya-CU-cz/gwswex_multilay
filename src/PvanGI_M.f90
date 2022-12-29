! This file is part of the vanGI_M module - MvanGI_M. It defines the procedures contained within the module.

SUBROUTINE init(self, ymodel_vanG_args, loggerobj)
! initializes the van Genuchten model parameters by reading them from the config file
	USE Mlogger, only : Clogger

	CLASS(CvanG) :: self
	CLASS(Clogger), INTENT(IN) :: loggerobj
	TYPE(YAMLMap), INTENT(INOUT) :: ymodel_vanG_args
	INTEGER :: ires
	CHARACTER(len=128) :: logbuffer

	logger = loggerobj

	self% alpha = ymodel_vanG_args% value_double('alpha', ires)
	IF (ires /= 0) THEN
		CALL logger% log(logger% error, "vanG model parameter - alpha not found in config file")
		ERROR STOP "ERROR: vanG model parameter - alpha not found in config file"
	END IF

	self% n = ymodel_vanG_args% value_double('n', ires)
	IF (ires /= 0) THEN
		CALL logger% log(logger% error, "vanG model parameter - n not found in config file")
		ERROR STOP "ERROR: vanG model parameter - n not found in config file"
	END IF

	self% m = (1-(1/self% n))
	IF (ires /= 0) THEN
		CALL logger% log(logger% error, "vanG model parameter - m not found in config file")
		ERROR STOP "ERROR: vanG model parameter - m not found in config file"
	END IF

	self% theta_r = ymodel_vanG_args% value_double('theta_r', ires)
	IF (ires /= 0) THEN
		CALL logger% log(logger% error, "vanG model parameter - theta_r not found in config file")
		ERROR STOP "ERROR: vanG model parameter - theta_r not found in config file"
	END IF

	self% theta_s = ymodel_vanG_args% value_double('theta_s', ires)
	IF (ires /= 0) THEN
		CALL logger% log(logger% error, "vanG model parameter - theta_s not found in config file")
		ERROR STOP "ERROR: vanG model parameter - theta_s not found in config file"
	END IF

	WRITE(*, logbuffer) "vanG pars initialized:   ", "alpha = ", self% alpha, " n = ", self% n, " m = ", self% m, " theta_r = ", self% theta_r, " theta_s = ", self% theta_s
	CALL logger% log(logger% trace, TRIM(logbuffer))
	
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
		REAL(REAL128), INTENT(IN) :: s, ks
		REAL(REAL128) :: kUS, sat
		REAL(REAL32), PARAMETER :: sat_relaxation = 1e-3

		! saturation relaxation introduced for numerical stability in cases where sat <= theta_r to avoid returning a kUS value of 0
		IF(s < theta_r .OR. s == theta_r) THEN
			sat = ((sat_relaxation)/(theta_s-theta_r))
			CALL logger% log(logger% trace, "saturation relaxation applied")
		ELSE
			sat = ((s-theta_r)/(theta_s-theta_r))
		END IF
		kUS = ks*sat*((1-(1-(sat)**(1/m))**m)**2)

		! fix for cases where kUS is NaN, i.e. when s >= theta_s
		IF(ISNAN(kUS)) THEN
			kUS = ks
			CALL logger% log(logger% trace, "kUS is NaN, setting kUS = ks")
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


FUNCTION integrate(d, u)
! integrates the van Genuchten model to calculate total depth-averaged soil moisture content for a given depth below the ground surface using the Fortran-GSL library [https://github.com/reinh-bader/fgsl]
	USE fgsl
	USE, INTRINSIC :: iso_c_binding

	REAL(REAL64) :: integrate
	REAL(REAL64), INTENT(IN) :: d, u
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

	status = fgsl_integration_qags(f_obj, -d, u, 0.0_fgsl_double, 1.0e-7_fgsl_double, nmax, wk, result, error)
	IF (status /= 0) THEN
		CALL logger% log(logger% fatal, "ERROR: fgsl integration failed")
		ERROR STOP "ERROR: integration failed"
	END IF
	integrate = result

	CALL fgsl_function_free(f_obj)
	CALL fgsl_integration_workspace_free(wk)

END FUNCTION integrate
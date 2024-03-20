MAKE := make
.PHONY: build debug parallel serial debug_parallel debug_serial install clean prune
LOGFILE := make.log

# Define compiler
FC := /usr/bin/gfortran
PYTHON := /usr/bin/python3
F2PY := $(PYTHON) -m numpy.f2py


# Fetch host details dynamically
SYS_ARCH := $(shell uname -m)
OS_TYPE := $(shell uname -s | tr '[:upper:]' '[:lower:]')
ifeq ($(OS_TYPE),darwin)
	OS_TYPE := darwin
else
	OS_TYPE := linux-gnu
endif
PY_VERSION := $(shell python -c 'import sys; print(sys.version_info.major * 10 + sys.version_info.minor)')


# Define source paths
SRC_ROOT := ./src/
SRC_PATH := $(SRC_ROOT)/core/
SRC_MODULES_PATH := $(SRC_ROOT)/core/modules/

# Define out paths
OUT_BIN_PATH := ./bin/
OUT_LIB_PATH := ./lib/
# Create output directories if necessary
$(shell mkdir -p $(OUT_BIN_PATH) $(OUT_LIB_PATH))

# Define install path
INSTALL_DIR := /opt/gwswex/


# Define source files
SRC_MODULE_NAMES := Mtiming.f90 Mpaths.f90 Mlogger.f90 Muz.f90 Msolver.f90 Mstorages.f90 model.f90
SRC_NAMES := gwswex.f90
SRC_WRAPPER_NAMES := wrapper.f90


# Define target names
TARGET_LIB_TAG := libgwswex
TARGET_EXE_TAG := gwswex
TARGET_F2PY_TAG := gwswex_f2pywrapper


# Define flags
FFLAGS := -ffree-line-length-1024 -Wall -Wno-conversion -Wno-conversion-extra -Wno-tabs -O3 -fPIC -march=znver2 -mtune=znver2
OMP_FLAGS := -fopenmp
DEBUG_FLAGS := -g -fbacktrace -fcheck=all

F2PYFLAGS := --verbose --fcompiler=gnu95 --opt='-O3' --f90flags='-march=znver2 -mtune=znver2' --build-dir .f2py_scratch
F2PYFLAGS_OMP := --f90flags='-fopenmp'
F2PYFLAGS_DEBUG := --debug-capi


# Define include paths
INCLUDES := -I/usr/local/include -I/usr/local/include/yaml-fortran -I/usr/local/include/fgsl/ -I.


# Define library paths
LIB_PATHS := -L/usr/local/lib/
LIBS := -lfgsl -lgsl -lgslcblas -lm -lyaml-interface -lyaml-read -lyaml-wrapper -lyaml-cpp -ldatetime
LIB_OMP := -L/usr/local/lib/ -lgomp

LD_FLAGS = $(LIB_PATHS) $(LIBS)

LD_FLAGS_PY := -L/usr/lib/x86_64-linux-gnu/ -lpython3.9


# Export necessary environment variables
export LDFLAGS = -Wl,-rpath=$(OUT_LIB_PATH)
export NPY_DISTUTILS_APPEND_FLAGS = 1


# Define dynamic variables
# Define build path
BUILD_PATH = ./build/$(CASE)

# Define source files with paths
oSRC_MODULES := $(addprefix $(SRC_MODULES_PATH), $(SRC_MODULE_NAMES))
oSRC := $(addprefix $(SRC_PATH), $(SRC_NAMES))
oSRC_WRAPPER := $(addprefix $(SRC_PATH), $(SRC_WRAPPER_NAMES))

SRC_MODULES = $(addprefix $(BUILD_PATH)/, $(SRC_MODULE_NAMES))
SRC = $(addprefix $(BUILD_PATH)/, $(SRC_NAMES))
SRC_WRAPPER = $(addprefix $(BUILD_PATH)/, $(SRC_WRAPPER_NAMES))

# Define object files
OBJ_NAMES = $(SRC_MODULE_NAMES:.f90=.o)
OBJS := $(BUILD_PATH)/$(OBJ_NAMES)

# Define target names
TARGET_BASENAME_F2PY = $(TARGET_F2PY_TAG)
TARGET_LIB_F2PY = $(addprefix $(OUT_LIB_PATH), $(TARGET_BASENAME_F2PY).cpython-$(PY_VERSION)-$(SYS_ARCH)-$(OS_TYPE).so)

TARGET_LIB_NAME = $(TARGET_LIB_TAG).so
TARGET_EXE_NAME = $(TARGET_EXE_TAG)
TARGET_LIB = $(addprefix $(OUT_LIB_PATH), $(TARGET_LIB_NAME))
TARGET_EXE = $(addprefix $(OUT_BIN_PATH), $(TARGET_EXE_NAME))




build:
	@ $(MAKE) _prep_build CASE=$(CASE) > $(LOGFILE) 2>&1
	@ echo "\n\n\n" >> $(LOGFILE)

	$(eval FFLAGS += $(OMP_FLAGS))
	$(eval LD_FLAGS += $(LIB_OMP))
	$(eval F2PYFLAGS += $(F2PYFLAGS_OMP))

	@ $(MAKE) _build CASE=$(CASE) >> $(LOGFILE) 2>&1 && echo "Build successful!" || echo "Build failed... \nCheck $(LOGFILE) for details"


parallel: CASE = parallel
parallel:
	@ $(MAKE) _prep_build CASE=$(CASE) > $(LOGFILE) 2>&1

	$(eval FFLAGS += $(OMP_FLAGS))
	$(eval LD_FLAGS += $(LIB_OMP))
	$(eval F2PYFLAGS += $(F2PYFLAGS_OMP))

	@ $(MAKE) _build CASE=$(CASE) \
		TARGET_BASENAME_F2PY=$(TARGET_F2PY_TAG)_$(subst /,_,$(CASE))  \
		TARGET_LIB_NAME=$(TARGET_LIB_TAG)_$(subst /,_,$(CASE)).so \
		TARGET_EXE_NAME=$(TARGET_EXE_TAG)_$(subst /,_,$(CASE))


serial: CASE = serial
serial:
	@ $(MAKE) _prep_build CASE=$(CASE) > $(LOGFILE) 2>&1

	@ $(MAKE) _build CASE=$(CASE) \
		TARGET_BASENAME_F2PY=$(TARGET_F2PY_TAG)_$(subst /,_,$(CASE))  \
		TARGET_LIB_NAME=$(TARGET_LIB_TAG)_$(subst /,_,$(CASE)).so \
		TARGET_EXE_NAME=$(TARGET_EXE_TAG)_$(subst /,_,$(CASE))


debug: CASE = debug
debug:
	@ $(MAKE) _prep_build CASE=$(CASE) > $(LOGFILE) 2>&1

	$(eval FFLAGS += $(OMP_FLAGS) $(DEBUG_FLAGS))
	$(eval LD_FLAGS += $(LIB_OMP))
	$(eval F2PYFLAGS += $(F2PYFLAGS_OMP) $(F2PYFLAGS_DEBUG))

	@ $(MAKE) _build CASE=$(CASE) \
		TARGET_BASENAME_F2PY=$(TARGET_F2PY_TAG)_$(subst /,_,$(CASE))  \
		TARGET_LIB_NAME=$(TARGET_LIB_TAG)_$(subst /,_,$(CASE)).so \
		TARGET_EXE_NAME=$(TARGET_EXE_TAG)_$(subst /,_,$(CASE))


debug_parallel: CASE = debug/parallel
debug_parallel:
	@ $(MAKE) _prep_build CASE=$(CASE) > $(LOGFILE) 2>&1

	$(eval FFLAGS += $(OMP_FLAGS) $(DEBUG_FLAGS))
	$(eval LD_FLAGS += $(LIB_OMP))
	$(eval F2PYFLAGS += $(F2PYFLAGS_OMP) $(F2PYFLAGS_DEBUG))
	
	@ $(MAKE) _build CASE=$(CASE) \
		TARGET_BASENAME_F2PY=$(TARGET_F2PY_TAG)_$(subst /,_,$(CASE))  \
		TARGET_LIB_NAME=$(TARGET_LIB_TAG)_$(subst /,_,$(CASE)).so \
		TARGET_EXE_NAME=$(TARGET_EXE_TAG)_$(subst /,_,$(CASE))


debug_serial: CASE = debug/serial
debug_serial:
	@ $(MAKE) _prep_build CASE=$(CASE) > $(LOGFILE) 2>&1

	$(eval FFLAGS += $(DEBUG_FLAGS))
	$(eval F2PYFLAGS += $(F2PYFLAGS_DEBUG))

	@ $(MAKE) _build CASE=$(CASE) \
		TARGET_BASENAME_F2PY=$(TARGET_F2PY_TAG)_$(subst /,_,$(CASE))  \
		TARGET_LIB_NAME=$(TARGET_LIB_TAG)_$(subst /,_,$(CASE)).so \
		TARGET_EXE_NAME=$(TARGET_EXE_TAG)_$(subst /,_,$(CASE))


install:
	@ mkdir -p $(INSTALL_DIR)/lib $(INSTALL_DIR)/bin $(INSTALL_DIR)/src/core/modules $(INSTALL_DIR)/src/gwswex_pywrapper > $(LOGFILE) 2>&1
	@ sudo cp $(OUT_LIB_PATH)/*.so $(INSTALL_DIR)/lib/
	@ sudo cp $(OUT_BIN_PATH)/* $(INSTALL_DIR)/bin/
	@ sudo cp $(SRC_MODULES_PATH)/*.f90 $(INSTALL_DIR)/src/core/modules/
	@ sudo cp $(SRC_PATH)/*.f90 $(INSTALL_DIR)/src/core/
	@ sudo cp $(SRC_ROOT)/gwswex_pywrapper/*.py $(INSTALL_DIR)/src/gwswex_pywrapper/

uninstall:
	@ sudo rm -rf $(INSTALL_DIR) > $(LOGFILE) 2>&1

clean:
	@ rm -rf $(BUILD_PATH) $(OUT_BIN_PATH) $(OUT_LIB_PATH) > $(LOGFILE) 2>&1

prune:
	@ find BUILD_PATH_BASE -type f -name "*.o" -exec rm -f {} + > $(LOGFILE) 2>&1
	@ find BUILD_PATH_BASE -type f -name "*.mod" -exec rm -f {} +
	@ find /path/to/start/dir -type d -name ".f2py_scratch" -exec rm -rf {} +



_prep_build:
	mkdir -p $(BUILD_PATH)

	cp $(oSRC_MODULES) $(BUILD_PATH)
	cp $(oSRC) $(BUILD_PATH)
	cp $(oSRC_WRAPPER) $(BUILD_PATH)


_build: $(OBJS) $(TARGET_LIB_F2PY) $(TARGET_LIB) $(TARGET_EXE)
$(OBJS): $(SRC_MODULES)
	@ echo "\n\n\n" >> $(LOGFILE)

	cd $(BUILD_PATH); \
		$(FC) -c $(SRC_MODULE_NAMES) $(INCLUDES) $(LD_FLAGS) $(FFLAGS)


$(TARGET_LIB_F2PY): $(OBJS) $(SRC_WRAPPER)
	@ echo "\n\n\n" >> $(LOGFILE)

	cd $(BUILD_PATH) && \
		$(F2PY) $(F2PYFLAGS) -c -m $(TARGET_BASENAME_F2PY) $(INCLUDES) $(LD_FLAGS) $(OBJ_NAMES) $(SRC_WRAPPER_NAMES)
	
	cp $(BUILD_PATH)/$(TARGET_BASENAME_F2PY).cpython-$(PY_VERSION)-$(SYS_ARCH)-$(OS_TYPE).so $(OUT_LIB_PATH)/


$(TARGET_LIB): $(OBJS) $(SRC_WRAPPER)
	@ echo "\n\n\n" >> $(LOGFILE)

	cd $(BUILD_PATH) && \
		$(FC) -shared -o $(TARGET_LIB_NAME) $(OBJ_NAMES) $(SRC_WRAPPER_NAMES) $(INCLUDES) $(LD_FLAGS) $(FFLAGS)
	
	cp $(BUILD_PATH)/$(TARGET_LIB_NAME) $(OUT_LIB_PATH)/


$(TARGET_EXE): $(OBJS) $(SRC)
	@ echo "\n\n\n" >> $(LOGFILE)

	cd $(BUILD_PATH) && \
		$(FC) -o $(TARGET_EXE_NAME) $(OBJ_NAMES) $(SRC_NAMES) $(INCLUDES) $(LIB_PATHS) $(LD_FLAGS) $(FFLAGS)
	
	cp $(BUILD_PATH)/$(TARGET_EXE_NAME) $(OUT_BIN_PATH)/

	@ echo "\n\n\n" >> $(LOGFILE)
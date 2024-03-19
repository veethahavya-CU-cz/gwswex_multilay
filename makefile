MAKE = make
.PHONY: all debug install clean prune

# Define compiler
FC = /usr/bin/gfortran
PYTHON = /usr/bin/python3
F2PY = $(PYTHON) -m numpy.f2py

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
SRC_PATH := $(SRC_ROOT)core/
SRC_MODULES_PATH := $(SRC_ROOT)core/modules/

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

# Define flags
FFLAGS := -ffree-line-length-1024 -Wall -Wno-conversion -Wno-conversion-extra -Wno-tabs -O3 -fPIC -march=znver2 -mtune=znver2
OMP_FLAGS := -fopenmp
DEBUG_FLAGS := -g -fbacktrace -fcheck=all
F2PYFLAGS = --verbose --fcompiler=gnu95 --opt='-O3' --f90flags='-march=znver2 -mtune=znver2' --build-dir .f2py_scratch

# Define include paths
INCLUDES := -I/usr/local/include -I/usr/local/include/yaml-fortran -I/usr/local/include/fgsl/
INCLUDES_NUMPY := -I$(shell $(PYTHON) -c "import numpy; print(numpy.get_include())")
INCLUDES_F2PY := -I$(shell $(PYTHON) -c "import numpy.f2py; print(numpy.f2py.get_include())")

# Define library paths
LIB_PATHS := -L/usr/local/lib/
LIBS := -lfgsl -lgsl -lgslcblas -lm -lyaml-interface -lyaml-read -lyaml-wrapper -lyaml-cpp -ldatetime
LIB_OMP = -L/usr/local/lib/ -lgomp
LIB_PATH_PY := -L/usr/lib/x86_64-linux-gnu/
LIB_PY := -lpython3.9
LD_FLAGS_PY := -L$(LIB_PATH_PY) $(LIB_PY)

# Define source files with paths
SRC_MODULES := $(addprefix $(SRC_MODULES_PATH), $(SRC_MODULE_NAMES))
SRC := $(addprefix $(SRC_PATH), $(SRC_NAMES))
SRC_WRAPPER := $(addprefix $(SRC_PATH), $(SRC_WRAPPER_NAMES))

# Define build paths
BUILD_PARALLEL_PATH := ./build_parallel/
BUILD_SERIAL_PATH := ./build_serial/
BUILD_DEBUG_PARALLEL_PATH := ./build_debug_parallel/
BUILD_DEBUG_SERIAL_PATH := ./build_debug_serial/

# Define object files
OBJ_NAMES := $(SRC_MODULE_NAMES:.f90=.o)
OBJS_PARALLEL := $(addprefix $(BUILD_PARALLEL_PATH), $(OBJ_NAMES))
OBJS_SERIAL := $(addprefix $(BUILD_SERIAL_PATH), $(OBJ_NAMES))
OBJS_DEBUG_PARALLEL := $(addprefix $(BUILD_DEBUG_PARALLEL_PATH), $(OBJ_NAMES))
OBJS_DEBUG_SERIAL := $(addprefix $(BUILD_DEBUG_SERIAL_PATH), $(OBJ_NAMES))

# Define targets
TARGET_LIB_NAME := libgwswex
TARGET_EXE_NAME := gwswex
TARGET_BASENAME_F2PY := gwswex_f2pywrapper

#TODO: change path to 'build..."
# Define targets with paths
TARGET_LIB_F2PY := $(addprefix $(OUT_LIB_PATH), $(TARGET_BASENAME_F2PY).cpython-$(PY_VERSION)-$(SYS_ARCH)-$(OS_TYPE).so)
TARGET_LIB := $(addprefix $(OUT_LIB_PATH), $(TARGET_LIB_NAME).so)
TARGET_EXE := $(addprefix $(OUT_BIN_PATH), $(TARGET_EXE_NAME))

TARGET_LIB_F2PY_PARALLEL := $(addprefix $(OUT_LIB_PATH), $(TARGET_BASENAME_F2PY)_parallel.cpython-$(PY_VERSION)-$(SYS_ARCH)-$(OS_TYPE).so)
TARGET_LIB_PARALLEL := $(addprefix $(OUT_LIB_PATH), $(TARGET_LIB_NAME)_parallel.so)
TARGET_EXE_PARALLEL := $(addprefix $(OUT_BIN_PATH), $(TARGET_EXE_NAME)_parallel)

TARGET_LIB_F2PY_SERIAL := $(addprefix $(OUT_LIB_PATH), $(TARGET_BASENAME_F2PY)_serial.cpython-$(PY_VERSION)-$(SYS_ARCH)-$(OS_TYPE).so)
TARGET_LIB_SERIAL := $(addprefix $(OUT_LIB_PATH), $(TARGET_LIB_NAME)_serial.so)
TARGET_EXE_SERIAL := $(addprefix $(OUT_BIN_PATH), $(TARGET_EXE_NAME)_serial)

# Define debug targets with paths
DEBUG_LIB_F2PY := $(addprefix $(OUT_LIB_PATH), $(TARGET_BASENAME_F2PY)_debugger.cpython-$(PY_VERSION)-$(SYS_ARCH)-$(OS_TYPE).so)
DEBUG_LIB := $(addprefix $(OUT_LIB_PATH), $(TARGET_LIB_NAME)_debugger.so)
DEBUG_EXE := $(addprefix $(OUT_BIN_PATH), $(TARGET_LIB_NAME)_debugger)

DEBUG_LIB_F2PY_PARALLEL := $(addprefix $(OUT_LIB_PATH), $(TARGET_BASENAME_F2PY)_parallel_debugger.cpython-$(PY_VERSION)-$(SYS_ARCH)-$(OS_TYPE).so)
DEBUG_LIB_PARALLEL := $(addprefix $(OUT_LIB_PATH), $(TARGET_LIB_NAME)_parallel_debugger.so)
DEBUG_EXE_PARALLEL := $(addprefix $(OUT_BIN_PATH), $(TARGET_LIB_NAME)_parallel_debugger)

DEBUG_LIB_F2PY_SERIAL := $(addprefix $(OUT_LIB_PATH), $(TARGET_BASENAME_F2PY)_serial_debugger.cpython-$(PY_VERSION)-$(SYS_ARCH)-$(OS_TYPE).so)
DEBUG_LIB_SERIAL := $(addprefix $(OUT_LIB_PATH), $(TARGET_LIB_NAME)_serial_debugger.so)
DEBUG_EXE_SERIAL := $(addprefix $(OUT_BIN_PATH), $(TARGET_LIB_NAME)_serial_debugger)

# Export necessary environment variables
export LDFLAGS := -Wl,-rpath=$(OUT_LIB_PATH)
export NPY_DISTUTILS_APPEND_FLAGS := 1

# Default build target
all:
	$(MAKE) parallel

parallel:
	mkdir -p $(BUILD_PARALLEL_PATH)
	cp $(SRC_MODULES) $(BUILD_PARALLEL_PATH)
	cp $(SRC) $(BUILD_PARALLEL_PATH)
	cp $(SRC_WRAPPER) $(BUILD_PARALLEL_PATH)
	$(MAKE) build_parallel

serial:
	mkdir -p $(BUILD_SERIAL_PATH)
	cp $(SRC_MODULES) $(BUILD_SERIAL_PATH)
	cp $(SRC) $(BUILD_SERIAL_PATH)
	cp $(SRC_WRAPPER) $(BUILD_SERIAL_PATH)
	$(MAKE) build_serial

debug:
	$(MAKE) debug_parallel

debug_parallel:
	mkdir -p $(BUILD_DEBUG_PARALLEL_PATH)
	cp $(SRC_MODULES) $(BUILD_DEBUG_PARALLEL_PATH)
	cp $(SRC) $(BUILD_DEBUG_PARALLEL_PATH)
	cp $(SRC_WRAPPER) $(BUILD_DEBUG_PARALLEL_PATH)
	$(MAKE) build_debug_parallel

debug_serial:
	mkdir -p $(BUILD_DEBUG_SERIAL_PATH)
	cp $(SRC_MODULES) $(BUILD_DEBUG_SERIAL_PATH)
	cp $(SRC) $(BUILD_DEBUG_SERIAL_PATH)
	cp $(SRC_WRAPPER) $(BUILD_DEBUG_SERIAL_PATH)
	$(MAKE) build_debug_serial

install:
	mkdir -p $(INSTALL_DIR)/lib $(INSTALL_DIR)/bin $(INSTALL_DIR)/src/core/modules $(INSTALL_DIR)/src/gwswex_pywrapper
	sudo cp $(OUT_LIB_PATH)/*.so $(INSTALL_DIR)/lib/
	sudo cp $(OUT_BIN_PATH)/* $(INSTALL_DIR)/bin/
	sudo cp $(SRC_MODULES_PATH)/*.f90 $(INSTALL_DIR)/src/core/modules/
	sudo cp $(SRC_PATH)/*.f90 $(INSTALL_DIR)/src/core/
	sudo cp $(SRC_ROOT)/gwswex_pywrapper/*.py $(INSTALL_DIR)/src/gwswex_pywrapper/

uninstall:
	sudo rm -rf $(INSTALL_DIR)

clean:
	rm -rf $(BUILD_PARALLEL_PATH) $(BUILD_DEBUG_SERIAL_PATH) $(BUILD_SERIAL_PATH) $(BUILD_DEBUG_PARALLEL_PATH) $(OUT_BIN_PATH) $(OUT_LIB_PATH) $(SRC_ROOT)/.f2py_scratch

prune:
	rm -f $(OBJS_PARALLEL) $(OBJS_SERIAL) $(OBJS_DEBUG_PARALLEL) $(OBJS_DEBUG_SERIAL) $(BUILD_PARALLEL_PATH)/*.mod $(BUILD_SERIAL_PATH)/*.mod $(BUILD_DEBUG_PARALLEL_PATH)/*.mod $(BUILD_DEBUG_SERIAL_PATH)/*.mod $(SRC_PATH)*.mod



# BUILD CANDIDATES

build_parallel: $(TARGET_LIB_F2PY_PARALLEL) $(TARGET_LIB_PARALLEL) $(TARGET_EXE_PARALLEL)
$(OBJS_PARALLEL):
	cd $(BUILD_PARALLEL_PATH) && \
		$(FC) -c $(SRC_MODULE_NAMES) $(INCLUDES) $(LIB_PATHS) $(LIBS) $(LIB_OMP) $(FFLAGS) $(OMP_FLAGS)

$(TARGET_LIB_F2PY_PARALLEL): $(OBJS_PARALLEL)
	cd $(BUILD_PARALLEL_PATH) && \
		$(F2PY) $(F2PYFLAGS) --f90flags='$(OMP_FLAGS)' \
			-c -m $(TARGET_BASENAME_F2PY)_parallel -I. $(INCLUDES) $(LIB_PATHS) $(LIBS) $(LIB_OMP) $(OBJ_NAMES) $(SRC_WRAPPER_NAMES)
	
	cp $(BUILD_PARALLEL_PATH)/$(TARGET_BASENAME_F2PY)_parallel.cpython-$(PY_VERSION)-$(SYS_ARCH)-$(OS_TYPE).so $(OUT_LIB_PATH)/

$(TARGET_LIB_PARALLEL): $(OBJS_PARALLEL) $(BUILD_PARALLEL_PATH)/$(SRC_WRAPPER_NAMES)
	cd $(BUILD_PARALLEL_PATH) && \
		$(FC) -shared -o $@ $(OBJ_NAMES) $(SRC_WRAPPER_NAMES) $(INCLUDES) -I. $(LIB_PATHS) $(LIBS) $(LIB_OMP) $(FFLAGS) $(OMP_FLAGS)

$(TARGET_EXE_PARALLEL): $(OBJS_PARALLEL) $(BUILD_PARALLEL_PATH)/$(SRC_NAMES)
	cd $(BUILD_PARALLEL_PATH) && \
		$(FC) -o $@ $(OBJ_NAMES) $(SRC_NAMES) $(INCLUDES) -I. $(LIB_PATHS) $(LIBS) $(LIB_OMP) $(FFLAGS) $(OMP_FLAGS)


build_serial: $(TARGET_LIB_F2PY_SERIAL) $(TARGET_LIB_SERIAL) $(TARGET_EXE_SERIAL)
$(OBJS_SERIAL):
	cd $(BUILD_SERIAL_PATH) && $(FC) -c $(SRC_MODULE_NAMES) $(INCLUDES) $(LIB_PATHS) $(LIBS) $(FFLAGS)

$(TARGET_LIB_F2PY_SERIAL): $(OBJS_SERIAL)
	cd $(BUILD_SERIAL_PATH) && $(F2PY) $(F2PYFLAGS) -c -m $(TARGET_BASENAME_F2PY)_serial -I$(BUILD_SERIAL_PATH) $(INCLUDES) $(LIB_PATHS) $(LIBS) $^ $(SRC_WRAPPER)
	cp $(TARGET_BASENAME_F2PY)_serial.cpython-$(PY_VERSION)-$(SYS_ARCH)-$(OS_TYPE).so $(OUT_LIB_PATH)/

$(TARGET_LIB_SERIAL): $(OBJS_SERIAL) $(SRC_WRAPPER)
	cd $(BUILD_SERIAL_PATH) && $(FC) -shared -o $@ $^ $(INCLUDES) -I$(SRC_MODULES_PATH) $(LIB_PATHS) $(LIBS) $(FFLAGS)

$(TARGET_EXE_SERIAL): $(OBJS_SERIAL) $(SRC)
	cd $(BUILD_SERIAL_PATH) && $(FC) -o $@ $^ $(INCLUDES) -I$(SRC_MODULES_PATH) $(LIB_PATHS) $(LIBS) $(FFLAGS)
	mv *.mod $(SRC_PATH)

build_debug_parallel: $(DEBUG_LIB_F2PY_PARALLEL) $(DEBUG_LIB_PARALLEL) $(DEBUG_EXE_PARALLEL)
$(OBJS_DEBUG_PARALLEL):
	cd $(BUILD_DEBUG_PARALLEL_PATH) && $(FC) -c $(SRC_MODULE_NAMES) $(INCLUDES) $(LIB_PATHS) $(LIBS) $(LIB_OMP) $(LIB_PATH_PY) $(LIB_PY) $(FFLAGS) $(OMP_FLAGS) $(DEBUG_FLAGS)

$(DEBUG_LIB_F2PY_PARALLEL): $(OBJS_DEBUG_PARALLEL)
	cd $(BUILD_DEBUG_PARALLEL_PATH) && $(F2PY) -c -m --debug-capi $(TARGET_BASENAME_F2PY)_parallel $(F2PYFLAGS) --f90flags='$(OMP_FLAGS)' --f90flags='-g' -I$(BUILD_DEBUG_PARALLEL_PATH)_parallel_debugger $(INCLUDES) $(LIB_PATHS) $(LIBS) $(LIB_OMP) $^ $(SRC_WRAPPER)
	cp $(TARGET_BASENAME_F2PY)_parallel_debugger.cpython-$(PY_VERSION)-$(SYS_ARCH)-$(OS_TYPE).so $(OUT_LIB_PATH)/

$(DEBUG_LIB_PARALLEL): $(OBJS_DEBUG_PARALLEL) $(SRC_WRAPPER)
	$(FC) -shared -o $@ $^ $(INCLUDES) -I$(SRC_MODULES_PATH) $(LIB_PATHS) $(LIBS) $(LIB_OMP) $(LIB_PATH_PY) $(LIB_PY) $(FFLAGS) $(OMP_FLAGS) $(DEBUG_FLAGS)

$(DEBUG_EXE_PARALLEL): $(OBJS_DEBUG_PARALLEL) $(SRC)
	$(FC) -o $@ $^ $(INCLUDES) -I$(SRC_MODULES_PATH) $(LIB_PATHS) $(LIBS) $(LIB_OMP) $(LIB_PATH_PY) $(LIB_PY) $(FFLAGS) $(OMP_FLAGS) $(DEBUG_FLAGS)
	mv *.mod $(SRC_PATH)

build_debug_serial: $(DEBUG_LIB_F2PY_SERIAL) $(DEBUG_LIB_SERIAL) $(DEBUG_EXE_SERIAL)
$(OBJS_DEBUG_SERIAL):
	cd $(BUILD_DEBUG_SERIAL_PATH) && $(FC) -c $(SRC_MODULE_NAMES) $(INCLUDES) $(LIB_PATHS) $(LIBS) $(LIB_PATH_PY) $(LIB_PY) $(FFLAGS) $(DEBUG_FLAGS)

$(DEBUG_LIB_F2PY_SERIAL): $(OBJS_DEBUG_SERIAL)
	cd $(BUILD_DEBUG_SERIAL_PATH) && $(F2PY) -c -m --debug-capi $(TARGET_BASENAME_F2PY)_serial $(F2PYFLAGS) --f90flags='-g' -I$(BUILD_DEBUG_SERIAL_PATH)_serial_debugger $(INCLUDES) $(LIB_PATHS) $(LIBS) $^ $(SRC_WRAPPER)
	cp $(TARGET_BASENAME_F2PY)_serial_debugger.cpython-$(PY_VERSION)-$(SYS_ARCH)-$(OS_TYPE).so $(OUT_LIB_PATH)/

$(DEBUG_LIB_SERIAL): $(OBJS_DEBUG_SERIAL) $(SRC_WRAPPER)
	$(FC) -shared -o $@ $^ $(INCLUDES) -I$(SRC_MODULES_PATH) $(LIB_PATHS) $(LIBS) $(LIB_PATH_PY) $(LIB_PY) $(FFLAGS) $(DEBUG_FLAGS)

$(DEBUG_EXE_SERIAL): $(OBJS_DEBUG_SERIAL) $(SRC)
	$(FC) -o $@ $^ $(INCLUDES) -I$(SRC_MODULES_PATH) $(LIB_PATHS) $(LIBS) $(LIB_PATH_PY) $(LIB_PY) $(FFLAGS) $(DEBUG_FLAGS)
	mv *.mod $(SRC_PATH)
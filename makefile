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


# Define flags
FFLAGS := -ffree-line-length-1024 \
	-Wall -Wno-conversion -Wno-conversion-extra -Wno-tabs \
	-O3 -fPIC -ftree-vectorize -frecursive \
	-march=znver2 -mtune=znver2

DEBUG_FLAGS := -g

F2PYFLAGS = --verbose \
	--fcompiler=gnu95 \
	--opt='-O3' \
	--f90flags='-march=znver2 -mtune=znver2 -ftree-vectorize -frecursive' \
	--build-dir ./src/f2py_scratch


# Define include paths
INCLUDES := -I/usr/local/include \
			-I/usr/local/include/yaml-fortran \
			-I/usr/local/include/fgsl/

ICLUDES_NUMPY := -I$(shell $(PYTHON) -c "import numpy; print(numpy.get_include())")

INCLUDES_F2PY := -I$(shell $(PYTHON) -c "import numpy.f2py; print(numpy.f2py.get_include())")


# Define library paths
LIB_PATHS := -L/usr/local/lib/

# Get Python library path dynamically
LIB_PATH_PY := $(shell $(PYTHON)-config --prefix)/lib

# Define libraries
LIBS := -lfgsl -lgsl \
		-lgslcblas -lm \
		-lyaml-interface -lyaml-read -lyaml-wrapper -lyaml-cpp \
		-ldatetime

# Get Python libraries dynamically
LIBS_PY := $(shell $(PYTHON)-config --libs)

# LIBS_PY: Adjust for potential prefix variations on different systems
ifeq ($(wildcard $(LIB_PATH_PY)/libpython*.so),)
    LIB_PY := $(shell $(PYTHON)-config --ldflags)
endif



# Define source paths
SRC_MODULES_PATH := ./src/modules/
SRC_PATH := ./src/

# Define out paths
OUT_BIN_PATH := ./bin/
OUT_LIB_PATH := ./lib/


# Define source files
SRC_MODULE_NAMES := Mtiming.f90 Mpaths.f90 Mlogger.f90 Muz.f90 Msolver.f90 Mstorages.f90 model.f90
SRC_NAMES := gwswex.f90
SRC_WRAPPER_NAMES := wrapper.f90

# Define source files with paths
SRC_MODULES := $(addprefix $(SRC_MODULES_PATH), $(SRC_MODULE_NAMES))
SRC := $(addprefix $(SRC_PATH), $(SRC_NAMES))
SRC_WRAPPER := $(addprefix $(SRC_PATH), $(SRC_WRAPPER_NAMES))


# Define object files
OBJS := $(SRC_MODULES:.f90=.o)

# Define targets
TARGET_LIB_NAME := libgwswex
TARGET_EXE_NAME := gwswex
TARGET_BASENAME_F2PY := gwswex_pywrapper

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
DEBUG_TARGET_LIB_F2PY := $(addprefix $(OUT_LIB_PATH), $(TARGET_BASENAME_F2PY)_debugger.cpython-$(PY_VERSION)-$(SYS_ARCH)-$(OS_TYPE).so)
DEBUG_TARGET_LIB := $(addprefix $(OUT_LIB_PATH), $(TARGET_LIB_NAME)_debugger.so)
DEBUG_TARGET_EXE := $(addprefix $(OUT_LIB_PATH), $(TARGET_LIB_NAME)_debugger)

DEBUG_TARGET_LIB_F2PY_PARALLEL := $(addprefix $(OUT_LIB_PATH), $(TARGET_BASENAME_F2PY)_parallel_debugger.cpython-$(PY_VERSION)-$(SYS_ARCH)-$(OS_TYPE).so)
DEBUG_TARGET_LIB_PARALLEL := $(addprefix $(OUT_LIB_PATH), $(TARGET_LIB_NAME)_parallel_debugger.so)
DEBUG_TARGET_EXE_PARALLEL := $(addprefix $(OUT_LIB_PATH), $(TARGET_LIB_NAME)_parallel_debugger)

DEBUG_TARGET_LIB_F2PY_SERIAL := $(addprefix $(OUT_LIB_PATH), $(TARGET_BASENAME_F2PY)_serial_debugger.cpython-$(PY_VERSION)-$(SYS_ARCH)-$(OS_TYPE).so)
DEBUG_TARGET_LIB_SERIAL := $(addprefix $(OUT_LIB_PATH), $(TARGET_LIB_NAME)_serial_debugger.so)
DEBUG_TARGET_EXE_SERIAL := $(addprefix $(OUT_LIB_PATH), $(TARGET_LIB_NAME)_serial_debugger)




# Export necessary environment variables
export LDFLAGS := -Wl,-rpath=$(OUT_LIB_PATH)
export NPY_DISTUTILS_APPEND_FLAGS := 1



# Default build target
all: $(TARGET_LIB_F2PY)   $(TARGET_LIB)   $(TARGET_EXE)

# Compile object files for support modules with OpenMP
$(SRC_MODULES_PATH)%.o: $(SRC_MODULES_PATH)%.f90
	$(FC) -c $< -o $@ \
		$(INCLUDES) -I$(SRC_MODULES_PATH) \
		$(LIB_PATHS) $(LIBS) -lgomp \
		$(FFLAGS) -fopenmp
	
	mv *.mod $(SRC_MODULES_PATH)


# Compile f2py library with OpenMP
$(TARGET_LIB_F2PY): $(OBJS)
	$(F2PY) $(F2PYFLAGS) --f90flags='-fopenmp' \
		-c -m $(TARGET_BASENAME_F2PY) \
		-I$(SRC_MODULES_PATH) $(INCLUDES) \
		$(LIB_PATHS) $(LIBS) -lgomp \
		$^ $(SRC_WRAPPER)
	
	mv $(TARGET_BASENAME_F2PY).cpython-$(PY_VERSION)-$(SYS_ARCH)-$(OS_TYPE).so \
		$(OUT_LIB_PATH)/$(TARGET_BASENAME_F2PY).cpython-$(PY_VERSION)-$(SYS_ARCH)-$(OS_TYPE).so


# Compile the (dynamic) library with OpenMP
$(TARGET_LIB): $(OBJS) $(SRC)
	$(FC) -shared -o $@ $^ \
		$(INCLUDES) -I$(SRC_MODULES_PATH) \
		$(LIB_PATHS) $(LIBS) -lgomp \
		$(FFLAGS) -fopenmp

# Compile the program with OpenMP
$(TARGET_EXE): $(OBJS) $(SRC)
	$(FC) -o $@ $^ \
		$(INCLUDES) -I$(SRC_MODULES_PATH) \
		$(LIB_PATHS) $(LIBS) -lgomp \
		$(FFLAGS) -fopenmp

	mv *.mod $(SRC_PATH)



parallel: $(TARGET_LIB_F2PY_PARALLEL)   $(TARGET_LIB_PARALLEL)   $(TARGET_EXE_PARALLEL)

# Compile object files for support modules with OpenMP
$(SRC_MODULES_PATH)%.o: $(SRC_MODULES_PATH)%.f90
	$(FC) -c $< -o $@ \
		$(INCLUDES) -I$(SRC_MODULES_PATH) \
		$(LIB_PATHS) $(LIBS) -lgomp \
		$(FFLAGS) -fopenmp
	
	mv *.mod $(SRC_MODULES_PATH)


# Compile f2py library with OpenMP
$(TARGET_LIB_F2PY_PARALLEL): $(OBJS)
	$(F2PY) $(F2PYFLAGS) --f90flags='-fopenmp' \
		-c -m $(TARGET_BASENAME_F2PY) \
		-I$(SRC_MODULES_PATH) $(INCLUDES) \
		$(LIB_PATHS) $(LIBS) -lgomp \
		$^ $(SRC_WRAPPER)

	mv *.mod $(SRC_PATH)
	mv $(TARGET_BASENAME_F2PY).cpython-$(PY_VERSION)-$(SYS_ARCH)-$(OS_TYPE).so \
		$(OUT_LIB_PATH)/$(TARGET_BASENAME_F2PY)_parallel.cpython-$(PY_VERSION)-$(SYS_ARCH)-$(OS_TYPE).so

# Compile the (dynamic) library with OpenMP
$(TARGET_LIB_PARALLEL): $(OBJS) $(SRC)
	$(FC) -shared -o $@ $^ \
		$(INCLUDES) -I$(SRC_MODULES_PATH) \
		$(LIB_PATHS) $(LIBS) -lgomp \
		$(FFLAGS) -fopenmp

# Compile the program with OpenMP
$(TARGET_EXE_PARALLEL): $(OBJS) $(SRC)
	$(FC) -o $@ $^ \
		$(INCLUDES) -I$(SRC_MODULES_PATH) \
		$(LIB_PATHS) $(LIBS) -lgomp \
		$(FFLAGS) -fopenmp
	
	mv *.mod $(SRC_PATH)



serial: $(TARGET_LIB_F2PY_SERIAL)   $(TARGET_LIB_SERIAL)   $(TARGET_EXE_SERIAL)

# Compile object files for support modules without OpenMP
$(SRC_MODULES_PATH)%.o: $(SRC_MODULES_PATH)%.f90
	$(FC) -c $< -o $(SRC_MODULES_PATH)$*.o \
		$(INCLUDES) -I$(SRC_MODULES_PATH) \
		$(LIB_PATHS) $(LIBS) \
		$(FFLAGS)
	
	mv *.mod $(SRC_MODULES_PATH)


# Compile f2py library without OpenMP
$(TARGET_LIB_F2PY_SERIAL): $(OBJS)
	$(F2PY) $(F2PYFLAGS) \
		-c -m $(TARGET_BASENAME_F2PY) \
		-I$(SRC_MODULES_PATH) $(INCLUDES) \
		$(LIB_PATHS) $(LIBS) \
		$^ $(SRC_WRAPPER)

	mv *.mod $(SRC_PATH)
	mv $(TARGET_BASENAME_F2PY).cpython-$(PY_VERSION)-$(SYS_ARCH)-$(OS_TYPE).so \
		$(OUT_LIB_PATH)/$(TARGET_BASENAME_F2PY)_serial.cpython-$(PY_VERSION)-$(SYS_ARCH)-$(OS_TYPE).so

# Compile the (dynamic) library without OpenMP
$(TARGET_LIB_SERIAL): $(OBJS) $(SRC)
	$(FC) -shared -o $@ $^ \
		$(INCLUDES) -I$(SRC_MODULES_PATH) \
		$(LIB_PATHS) $(LIBS) \
		$(FFLAGS)

# Compile the program without OpenMP
$(TARGET_EXE_SERIAL): $(OBJS) $(SRC)
	$(FC) -o $@ $^ \
		$(INCLUDES) -I$(SRC_MODULES_PATH) \
		$(LIB_PATHS) $(LIBS) \
		$(FFLAGS)
	
	mv *.mod $(SRC_PATH)



# Default debug build target
debug: $(DEBUG_TARGET_LIB_F2PY)   $(DEBUG_TARGET_LIB)   $(DEBUG_TARGET_EXE)

# Compile object files for support modules with OpenMP
$(SRC_MODULES_PATH)%.o: $(SRC_MODULES_PATH)%.f90
	$(FC) -c $< -o $(SRC_MODULES_PATH)$*.o \
		$(INCLUDES) -I$(SRC_MODULES_PATH) \
		$(LIB_PATHS) $(LIBS) -lgomp \
		$(FFLAGS) $(DEBUG_FLAGS) -fopenmp
	
	mv *.mod $(SRC_MODULES_PATH)


# Compile f2py library with OpenMP
$(DEBUG_TARGET_LIB_F2PY): $(OBJS)
	$(F2PY) $(F2PYFLAGS) --f90flags='-fopenmp' \
		-c -m $(TARGET_BASENAME_F2PY) \
		-I$(SRC_MODULES_PATH) $(INCLUDES) \
		$(LIB_PATHS) $(LIBS) -lgomp \
		$^ $(SRC_WRAPPER)

	mv *.mod $(SRC_PATH)
	mv $(TARGET_BASENAME_F2PY).cpython-$(PY_VERSION)-$(SYS_ARCH)-$(OS_TYPE).so \
		$(OUT_LIB_PATH)/$(TARGET_BASENAME_F2PY)_debugger.cpython-$(PY_VERSION)-$(SYS_ARCH)-$(OS_TYPE).so

# Compile the (dynamic) library with OpenMP
$(DEBUG_TARGET_LIB): $(OBJS) $(SRC)
	$(FC) -shared -o $@ $^ \
		$(INCLUDES) -I$(SRC_MODULES_PATH) \
		$(LIB_PATHS) $(LIBS) -lgomp \
		$(FFLAGS) $(DEBUG_FLAGS) -fopenmp

# Compile the program with OpenMP
$(DEBUG_TARGET_EXE): $(OBJS) $(SRC)
	$(FC) -o $@ $^ \
		$(INCLUDES) -I$(SRC_MODULES_PATH) \
		$(LIB_PATHS) $(LIBS) -lgomp \
		$(FFLAGS) $(DEBUG_FLAGS) -fopenmp
	
	mv *.mod $(SRC_PATH)



debug_parallel: $(DEBUG_TARGET_LIB_F2PY_PARALLEL)   $(DEBUG_TARGET_LIB_PARALLEL)   $(DEBUG_TARGET_EXE_PARALLEL)

# Compile object files for support modules with OpenMP
$(SRC_MODULES_PATH)%.o: $(SRC_MODULES_PATH)%.f90
	$(FC) -c $< -o $(SRC_MODULES_PATH)$*.o \
		$(INCLUDES) -I$(SRC_MODULES_PATH) \
		$(LIB_PATHS) $(LIBS) -lgomp \
		$(FFLAGS) $(DEBUG_FLAGS) -fopenmp
	
	mv *.mod $(SRC_MODULES_PATH)


# Compile f2py library with OpenMP
$(DEBUG_TARGET_LIB_F2PY_PARALLEL): $(OBJS)
	$(F2PY) $(F2PYFLAGS) --f90flags='-fopenmp' \
		-c -m $(TARGET_BASENAME_F2PY) \
		-I$(SRC_MODULES_PATH) $(INCLUDES) \
		$(LIB_PATHS) $(LIBS) -lgomp \
		$^ $(SRC_WRAPPER)

	mv *.mod $(SRC_PATH)
	mv $(TARGET_BASENAME_F2PY).cpython-$(PY_VERSION)-$(SYS_ARCH)-$(OS_TYPE).so \
		$(OUT_LIB_PATH)/$(TARGET_BASENAME_F2PY)_parallel_debugger.cpython-$(PY_VERSION)-$(SYS_ARCH)-$(OS_TYPE).so

# Compile the (dynamic) library with OpenMP
$(DEBUG_TARGET_LIB_PARALLEL): $(OBJS) $(SRC)
	$(FC) -shared -o $@ $^ \
		$(INCLUDES) -I$(SRC_MODULES_PATH) \
		$(LIB_PATHS) $(LIBS) -lgomp \
		$(FFLAGS) $(DEBUG_FLAGS) -fopenmp

# Compile the program with OpenMP
$(DEBUG_TARGET_EXE_PARALLEL): $(OBJS) $(SRC)
	$(FC) -o $@ $^ \
		$(INCLUDES) -I$(SRC_MODULES_PATH) \
		$(LIB_PATHS) $(LIBS) -lgomp \
		$(FFLAGS) $(DEBUG_FLAGS) -fopenmp
	
	mv *.mod $(SRC_PATH)



debug_serial: $(DEBUG_TARGET_LIB_F2PY_SERIAL)   $(DEBUG_TARGET_LIB_SERIAL)   $(DEBUG_TARGET_EXE_SERIAL)

# Compile object files for support modules without OpenMP
$(SRC_MODULES_PATH)%.o: $(SRC_MODULES_PATH)%.f90
	$(FC) -c $< -o $(SRC_MODULES_PATH)$*.o \
		$(INCLUDES) -I$(SRC_MODULES_PATH) \
		$(LIB_PATHS) $(LIBS) \
		$(FFLAGS) $(DEBUG_FLAGS)
	
	mv *.mod $(SRC_MODULES_PATH)


# Compile f2py library without OpenMP
$(DEBUG_TARGET_LIB_F2PY_SERIAL): $(OBJS)
	$(F2PY) $(F2PYFLAGS) \
		-c -m $(TARGET_BASENAME_F2PY) \
		-I$(SRC_MODULES_PATH) $(INCLUDES) \
		$(LIB_PATHS) $(LIBS) \
		$^ $(SRC_WRAPPER)

	mv *.mod $(SRC_PATH)
	mv $(TARGET_BASENAME_F2PY).cpython-$(PY_VERSION)-$(SYS_ARCH)-$(OS_TYPE).so \
		$(OUT_LIB_PATH)/$(TARGET_BASENAME_F2PY)_serial_debugger.cpython-$(PY_VERSION)-$(SYS_ARCH)-$(OS_TYPE).so

# Compile the (dynamic) library without OpenMP
$(DEBUG_TARGET_LIB_SERIAL): $(OBJS) $(SRC)
	$(FC) -shared -o $@ $^ \
		$(INCLUDES) -I$(SRC_MODULES_PATH) \
		$(LIB_PATHS) $(LIBS) \
		$(FFLAGS) $(DEBUG_FLAGS)

# Compile the program without OpenMP
$(DEBUG_TARGET_EXE_SERIAL): $(OBJS) $(SRC)
	$(FC) -o $@ $^ \
		$(INCLUDES) -I$(SRC_MODULES_PATH) \
		$(LIB_PATHS) $(LIBS) \
		$(FFLAGS) $(DEBUG_FLAGS)
	
	mv *.mod $(SRC_PATH)



clean:
	rm -f $(OBJS) \
		$(TARGET_LIB_F2PY) $(TARGET_LIB) $(TARGET_EXE) \
		$(TARGET_LIB_F2PY_PARALLEL) $(TARGET_LIB_PARALLEL) $(TARGET_EXE_PARALLEL) \
		$(TARGET_LIB_F2PY_SERIAL) $(TARGET_LIB_SERIAL) $(TARGET_EXE_SERIAL) \
		$(DEBUG_TARGET_LIB_F2PY) $(DEBUG_TARGET_LIB) $(DEBUG_TARGET_EXE) \
		$(DEBUG_TARGET_LIB_F2PY_PARALLEL) $(DEBUG_TARGET_LIB_PARALLEL) $(DEBUG_TARGET_EXE_PARALLEL) \
		$(DEBUG_TARGET_LIB_F2PY_SERIAL) $(DEBUG_TARGET_LIB_SERIAL) $(DEBUG_TARGET_EXE_SERIAL)

	rm -rf $(SRC_MODULES_PATH)*.mod
	rm -rf $(SRC_PATH)*.mod
	rm -rf ./src/f2py_scratch
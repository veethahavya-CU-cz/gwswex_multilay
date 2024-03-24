# Define compilers
MAKE_EXE ?= make
FC := /usr/bin/gfortran
PYTHON := /usr/bin/python3
F2PY := $(PYTHON) -m numpy.f2py


# Define source paths
SRC_ROOT := ./src/
SRC_PATH := $(SRC_ROOT)/core/
SRC_MODULES_PATH := $(SRC_ROOT)/core/modules/

# Define program and library names
PROGRAM_TAG := gwswex
LIB_TAG := lib$(PROGRAM_TAG)
F2PY_MODULE_TAG := $(PROGRAM_TAG)_f2pywrapper

# Define source files
SRC_MODULE_NAMES := Mtiming.f90 Mpaths.f90 Mlogger.f90 Muz.f90 Msolver.f90 Mstorages.f90 model.f90
SRC_NAMES := gwswex.f90
SRC_WRAPPER_NAMES := wrapper.f90


###########################################################################################################################################

# Define output paths
OUT_PATH = $(BUILD_PATH)
OUT_BIN_PATH = $(OUT_PATH)/bin/
OUT_LIB_PATH = $(OUT_PATH)/lib/
OUT_INCLUDE_PATH = $(OUT_PATH)/include/

# Define installation path
INSTALL_DIR ?= /opt/$(PROGRAM_TAG)/


# Define compiler flags
FFLAGS += -ffree-line-length-1024 -Wall -Wno-conversion -Wno-conversion-extra -Wno-tabs -O3 -fPIC -march=znver2 -mtune=znver2
FFLAGS_OMP := -fopenmp
DEBUG_FLAGS += -g -fbacktrace -fcheck=all

F2PYFLAGS += --verbose --fcompiler=gnu95 --opt='-O3' --f90flags='-march=znver2 -mtune=znver2' --build-dir .f2py_scratch
F2PYFLAGS_OMP := --f90flags='-fopenmp'
F2PYFLAGS_DEBUG := --debug-capi


# Define include paths
INCLUDES += -I/usr/local/include -I/usr/local/include/yaml-fortran -I/usr/local/include/fgsl/ -I.


# Define library linking paths and names
LIB_PATHS := /usr/local/lib/
LIBS := -lfgsl -lgsl -lgslcblas -lm -lyaml-interface -lyaml-read -lyaml-wrapper -lyaml-cpp -ldatetime

LD_FLAGS += -L$(LIB_PATHS) $(LIBS)

LIB_OMP := -L/usr/local/lib/ -lgomp
LIB_PY += -L/usr/lib/x86_64-linux-gnu/ -lpython3.9

###########################################################################################################################################

# MAKE_EXE definitions
.PHONY: build debug parallel serial debug_parallel debug_serial install clean prune
LOGFILE := make.log


# Fetch and write compiler details
MAKE_VERSION := $(shell $(MAKE_EXE) --version | head -n 1)
$(shell echo "Make [$(MAKE_EXE)] version: $(MAKE_VERSION)\n" > $(LOGFILE))

FC_VERSION := $(shell $(FC) --version | head -n 1)
$(shell echo "Fortran Compiler [$(FC)] : $(FC_VERSION)\n" >> $(LOGFILE))

PYTHON_VERSION := $(shell $(PYTHON) --version)
$(shell echo "Python [$(PYTHON)] version: $(PYTHON_VERSION)" >> $(LOGFILE))
NUMPY_VERSION := $(shell $(PYTHON) -c 'import numpy; print(numpy.__version__)')
$(shell echo "Numpy version: $(NUMPY_VERSION)" >> $(LOGFILE))
F2PY_VERSION := $(shell $(F2PY) -v | head -n 1)
$(shell echo "F2PY version: $(F2PY_VERSION)\n\n\n" >> $(LOGFILE))


# Fetch host details dynamically
SYS_ARCH := $(shell uname -m)
OS_TYPE := $(shell uname -s | tr '[:upper:]' '[:lower:]')
ifeq ($(OS_TYPE),darwin)
	OS_TYPE := darwin
else
	OS_TYPE := linux-gnu
endif
pv1 := $(shell python -c 'import sys; print(sys.version_info.major)')
pv2 += $(shell python -c 'import sys; print(sys.version_info.minor)')
PY_VERSION := $(strip $(pv1))$(strip $(pv2))


# Export necessary environment variables
export LDFLAGS = -Wl,-rpath=$(OUT_LIB_PATH)
export NPY_DISTUTILS_APPEND_FLAGS = 1


# Define dynamic variables

# Define build path
BUILD_PATH = ./build/$(CASE)
$(shell mkdir -p $(BUILD_PATH))

# Define source files with paths
oSRC_MODULES := $(addprefix $(SRC_MODULES_PATH), $(SRC_MODULE_NAMES))
oSRC := $(addprefix $(SRC_PATH), $(SRC_NAMES))
oSRC_WRAPPER := $(addprefix $(SRC_PATH), $(SRC_WRAPPER_NAMES))

SRC_MODULES := $(addprefix $(BUILD_PATH)/, $(SRC_MODULE_NAMES))
SRC = $(addprefix $(BUILD_PATH)/, $(SRC_NAMES))
SRC_WRAPPER = $(addprefix $(BUILD_PATH)/, $(SRC_WRAPPER_NAMES))

# Define object files
OBJ_NAMES = $(SRC_MODULE_NAMES:.f90=.o)
OBJS := $(patsubst %,$(BUILD_PATH)/%,$(notdir $(OBJ_NAMES)))

# Define .mod files
MOD_NAMES = $(SRC_MODULE_NAMES:.f90=.mod)
MOD_NAMES=$(echo "$MOD_NAMES" | tr '[:upper:]' '[:lower:]')
MODS := $(patsubst %,$(BUILD_PATH)/%,$(notdir $(MOD_NAMES)))


# Define target names
PROGRAM_NAME = $(PROGRAM_TAG)
PROGRAM = $(addprefix $(OUT_BIN_PATH), $(PROGRAM_NAME))

F2PY_MODULE_NAME = $(F2PY_MODULE_TAG)
F2PY_MODULE = $(addprefix $(OUT_LIB_PATH), $(F2PY_MODULE_NAME).cpython-$(PY_VERSION)-$(SYS_ARCH)-$(OS_TYPE).so)

LIB_NAME = $(LIB_TAG).so
LIB = $(addprefix $(OUT_LIB_PATH), $(LIB_NAME))
LIB_STATIC_NAME = $(LIB_TAG).a
LIB_STATIC = $(addprefix $(OUT_LIB_PATH), $(LIB_STATIC_NAME))


# Define default target
.DEFAULT_GOAL := build

build:
	@ $(MAKE_EXE) _prep_build >> $(LOGFILE) 2>&1
	@ echo "\n\n\n" >> $(LOGFILE)

	$(eval FFLAGS += $(FFLAGS_OMP))
	$(eval LD_FLAGS += $(LIB_OMP))
	$(eval F2PYFLAGS += $(F2PYFLAGS_OMP))

	@ $(MAKE_EXE) _build \
		FFLAGS="$(FFLAGS)" \
		LD_FLAGS="$(LD_FLAGS)" \
		F2PYFLAGS="$(F2PYFLAGS)" \
	 >> $(LOGFILE) 2>&1 && echo "Build successful!" || echo "Build failed... \nCheck $(LOGFILE) for details"


parallel: CASE = parallel
parallel:
	@ $(MAKE_EXE) _prep_build CASE=$(CASE) >> $(LOGFILE) 2>&1

	$(eval FFLAGS += $(FFLAGS_OMP))
	$(eval LD_FLAGS += $(LIB_OMP))
	$(eval F2PYFLAGS += $(F2PYFLAGS_OMP))

	@ $(MAKE_EXE) _build CASE=$(CASE) \
		FFLAGS="$(FFLAGS)" \
		LD_FLAGS="$(LD_FLAGS)" \
		F2PYFLAGS="$(F2PYFLAGS)" \
		F2PY_MODULE_NAME=$(F2PY_MODULE_TAG)_$(subst /,_,$(CASE)) \
		LIB_NAME=$(LIB_TAG)_$(subst /,_,$(CASE)).so \
		LIB_STATIC_NAME=$(LIB_TAG)_$(subst /,_,$(CASE)).a \
		PROGRAM_NAME=$(PROGRAM_TAG)_$(subst /,_,$(CASE)) \
	 >> $(LOGFILE) 2>&1 && echo "Build successful!" || echo "Build failed... \nCheck $(LOGFILE) for details"


serial: CASE = serial
serial:
	@ $(MAKE_EXE) _prep_build CASE=$(CASE) >> $(LOGFILE) 2>&1

	@ $(MAKE_EXE) _build CASE=$(CASE) \
		F2PY_MODULE_NAME=$(F2PY_MODULE_TAG)_$(subst /,_,$(CASE)) \
		LIB_NAME=$(LIB_TAG)_$(subst /,_,$(CASE)).so \
		LIB_STATIC_NAME=$(LIB_TAG)_$(subst /,_,$(CASE)).a \
		PROGRAM_NAME=$(PROGRAM_TAG)_$(subst /,_,$(CASE)) \
	 >> $(LOGFILE) 2>&1 && echo "Build successful!" || echo "Build failed... \nCheck $(LOGFILE) for details"


debug: CASE = debug
debug:
	@ $(MAKE_EXE) _prep_build CASE=$(CASE) >> $(LOGFILE) 2>&1

	$(eval FFLAGS += $(FFLAGS_OMP) $(DEBUG_FLAGS))
	$(eval LD_FLAGS += $(LIB_OMP) $(LIB_PY))
	$(eval F2PYFLAGS += $(F2PYFLAGS_OMP) $(F2PYFLAGS_DEBUG))

	@ $(MAKE_EXE) _build CASE=$(CASE) \
		FFLAGS="$(FFLAGS)" \
		LD_FLAGS="$(LD_FLAGS)" \
		F2PYFLAGS="$(F2PYFLAGS)" \
		F2PY_MODULE_NAME=$(F2PY_MODULE_TAG)_$(subst /,_,$(CASE))  \
		LIB_NAME=$(LIB_TAG)_$(subst /,_,$(CASE)).so \
		LIB_STATIC_NAME=$(LIB_TAG)_$(subst /,_,$(CASE)).a \
		PROGRAM_NAME=$(PROGRAM_TAG)_$(subst /,_,$(CASE)) \
	 >> $(LOGFILE) 2>&1 && echo "Build successful!" || echo "Build failed... \nCheck $(LOGFILE) for details"


debug_parallel: CASE = debug/parallel
debug_parallel:
	@ $(MAKE_EXE) _prep_build CASE=$(CASE) >> $(LOGFILE) 2>&1

	$(eval FFLAGS += $(FFLAGS_OMP) $(DEBUG_FLAGS))
	$(eval LD_FLAGS += $(LIB_OMP) $(LIB_PY))
	$(eval F2PYFLAGS += $(F2PYFLAGS_OMP) $(F2PYFLAGS_DEBUG))
	
	@ $(MAKE_EXE) _build CASE=$(CASE) \
		FFLAGS="$(FFLAGS)" \
		LD_FLAGS="$(LD_FLAGS)" \
		F2PYFLAGS="$(F2PYFLAGS)" \
		F2PY_MODULE_NAME=$(F2PY_MODULE_TAG)_$(subst /,_,$(CASE))  \
		LIB_NAME=$(LIB_TAG)_$(subst /,_,$(CASE)).so \
		LIB_STATIC_NAME=$(LIB_TAG)_$(subst /,_,$(CASE)).a \
		PROGRAM_NAME=$(PROGRAM_TAG)_$(subst /,_,$(CASE)) \
	 >> $(LOGFILE) 2>&1 && echo "Build successful!" || echo "Build failed... \nCheck $(LOGFILE) for details"


debug_serial: CASE = debug/serial
debug_serial:
	@ $(MAKE_EXE) _prep_build CASE=$(CASE) >> $(LOGFILE) 2>&1

	$(eval FFLAGS += $(DEBUG_FLAGS))
	$(eval LD_FLAGS += $(LIB_PY))
	$(eval F2PYFLAGS += $(F2PYFLAGS_DEBUG))

	@ $(MAKE_EXE) _build CASE=$(CASE) \
		F2PY_MODULE_NAME=$(F2PY_MODULE_TAG)_$(subst /,_,$(CASE))  \
		LIB_NAME=$(LIB_TAG)_$(subst /,_,$(CASE)).so \
		LIB_STATIC_NAME=$(LIB_TAG)_$(subst /,_,$(CASE)).a \
		PROGRAM_NAME=$(PROGRAM_TAG)_$(subst /,_,$(CASE)) \
	 >> $(LOGFILE) 2>&1 && echo "Build successful!" || echo "Build failed... \nCheck $(LOGFILE) for details"


install: install1
install1:
	@if [ -d "$(BUILD_PATH)" ]; then \
        if [ -z "$(shell ls -A $(BUILD_PATH))" ]; then \
            echo "Error: $(PROGRAM_TAG) [$(CASE)] is not yet built!\nRun make [build_case] and try again."; \
            exit 1; \
        else \
            : ; \
        fi \
    else \
        echo "Error: $(PROGRAM_TAG) [$(CASE)] is not yet built!\nRun make [build_case] and try again."; \
    fi
	$(eval INSTALL_DIR := .)
	@ mkdir -p $(INSTALL_DIR)/lib $(INSTALL_DIR)/bin $(INSTALL_DIR)/include
	@ cp $(OUT_LIB_PATH)/*.so $(INSTALL_DIR)/lib/
	@ cp $(OUT_BIN_PATH)/* $(INSTALL_DIR)/bin/
	@ cp $(OUT_INCLUDE_PATH)/*.mod $(INSTALL_DIR)/include/
	@ echo 'export LD_LIBRARY_PATH=$(subst  ,:,$(LIB_PATHS))$$LD_LIBRARY_PATH' >> ~/.bashrc
	@ export LD_LIBRARY_PATH=$(subst  ,:,$(LIB_PATHS))$$LD_LIBRARY_PATH
	@ echo Exported $(LIB_PATHS) to LD_LIBRARY_PATH and added the export to ~/.bashrc, to make it persistent. Add it to other shell rc-files if necessary. 
install2:
	@ if [ `id -u` -eq 0 ]; then \
        $(MAKE) -s install2_root; \
    else \
        $(MAKE) -s install2_user; \
    fi
install2_user:
	@ sudo mkdir -p $(INSTALL_DIR)/lib $(INSTALL_DIR)/bin $(INSTALL_DIR)/include $(INSTALL_DIR)/src/core/modules $(INSTALL_DIR)/src/pywrapper/ >> $(LOGFILE) 2>&1
	@ sudo cp $(OUT_LIB_PATH)/*.so $(INSTALL_DIR)/lib/ >> $(LOGFILE) 2>&1
	@ sudo cp $(OUT_BIN_PATH)/* $(INSTALL_DIR)/bin/ >> $(LOGFILE) 2>&1
	@ sudo cp $(OUT_INCLUDE_PATH)/*.mod $(INSTALL_DIR)/include/ >> $(LOGFILE) 2>&1
	@ sudo cp $(SRC_PATH)/*.f90 $(INSTALL_DIR)/src/core/ >> $(LOGFILE) 2>&1
	@ sudo cp $(SRC_ROOT)/pywrapper/*.py $(INSTALL_DIR)/src/pywrapper/ >> $(LOGFILE) 2>&1
	@ echo "Installation successful!" >> $(LOGFILE) 2>&1
	@ echo "Installation successful!"
install2_root:
	@ mkdir -p $(INSTALL_DIR)/lib $(INSTALL_DIR)/bin $(INSTALL_DIR)/include $(INSTALL_DIR)/src/core/modules $(INSTALL_DIR)/src/pywrapper/ >> $(LOGFILE) 2>&1
	@ cp $(OUT_LIB_PATH)/*.so $(INSTALL_DIR)/lib/ >> $(LOGFILE) 2>&1
	@ cp $(OUT_BIN_PATH)/* $(INSTALL_DIR)/bin/ >> $(LOGFILE) 2>&1
	@ cp $(OUT_INCLUDE_PATH)/*.mod $(INSTALL_DIR)/include/ >> $(LOGFILE) 2>&1
	@ cp $(SRC_PATH)/*.f90 $(INSTALL_DIR)/src/core/ >> $(LOGFILE) 2>&1
	@ cp $(SRC_ROOT)/pywrapper/*.py $(INSTALL_DIR)/src/pywrapper/ >> $(LOGFILE) 2>&1
	@ echo "Installation successful!" >> $(LOGFILE) 2>&1
	@ echo "Installation successful!"

uninstall: uninstall2
uninstall1:
	$(MAKE) -s uninstall_user INSTALL_DIR=. >> $(LOGFILE)
uninstall2:
	@ if [ `id -u` -eq 0 ]; then \
		$(MAKE) -s uninstall_root; \
	else \
		$(MAKE) -s uninstall_user; \
	fi
uninstall_user:
	@ sudo rm -rf $(INSTALL_DIR) >> $(LOGFILE) 2>&1 && echo "Uninstallation successful." || (echo "Uninstallation failed... \nCheck $(LOGFILE) for details" && exit 1)
uninstall_root:
	@ rm -rf $(INSTALL_DIR) >> $(LOGFILE) 2>&1 && echo "Uninstallation successful." || (echo "Uninstallation failed... \nCheck $(LOGFILE) for details" && exit 1)

clean: clean1
clean1:
	@ find $(BUILD_PATH) -type f -name "*.f90" -exec rm -f {} + >> $(LOGFILE) 2>&1
	@ find $(BUILD_PATH) -type f -name "*.o" -exec rm -f {} + >> $(LOGFILE) 2>&1
	@ rm -f $(BUILD_PATH)/*.mod >> $(LOGFILE) 2>&1
	@ find $(BUILD_PATH) -type d -name ".f2py_scratch" -exec rm -rf {} + >> $(LOGFILE) 2>&1 && echo "Cleaned lightly."
clean2:
	@ rm -rf $(BUILD_PATH) $(OUT_BIN_PATH) $(OUT_LIB_PATH) $(OUT_INCLUDE_PATH) $(LOGFILE)
	@ rm -rf ./bin ./lib ./include && echo "Cleaned."

prune:
	@ find $(BUILD_PATH) -type f -name "*.f90" -exec rm -f {} + >> $(LOGFILE) 2>&1
	@ find $(BUILD_PATH) -type d -name ".f2py_scratch" -exec rm -rf {} + >> $(LOGFILE) 2>&1 && echo "Pruned lightly."



_prep_build:
	mkdir -p $(BUILD_PATH)
	mkdir -p $(OUT_BIN_PATH) $(OUT_LIB_PATH) $(OUT_INCLUDE_PATH)

	cp $(oSRC_MODULES) $(BUILD_PATH)
	cp $(oSRC) $(BUILD_PATH)
	cp $(oSRC_WRAPPER) $(BUILD_PATH)


_build: $(OBJS) $(F2PY_MODULE) $(LIB) $(LIB_STATIC) $(PROGRAM)
$(SRC_MODULES): $(oSRC_MODULES)
	@ cp $< $@ >> $(LOGFILE)
$(SRC): $(oSRC)
	@ cp $< $@ >> $(LOGFILE)
$(SRC_WRAPPER): $(oSRC_WRAPPER)
	@ cp $< $@ >> $(LOGFILE)

$(OUT_INCLUDE_PATH):
	@ mkdir -p $(OUT_INCLUDE_PATH) >> $(LOGFILE)
$(OUT_LIB_PATH):
	@ mkdir -p $(OUT_LIB_PATH) >> $(LOGFILE)
$(OUT_BIN_PATH):
	@ mkdir -p $(OUT_BIN_PATH) >> $(LOGFILE)


$(OBJS): $(BUILD_PATH)/%.o: $(BUILD_PATH)/%.f90
	$(FC)   -c $< -J $(@D) -o $@   $(INCLUDES) $(LD_FLAGS) $(FFLAGS)
	@ echo "" >> $(LOGFILE)

$(MODS): $(BUILD_PATH)/%.mod: $(BUILD_PATH)/%.f90
	@ echo "" >> $(LOGFILE)
	$(FC)   -c $< -J $(@D) -o $(@:.mod=.o)   $(INCLUDES) $(LD_FLAGS) $(FFLAGS)


$(F2PY_MODULE): $(OBJS) $(MODS) $(SRC_WRAPPER) $(OUT_LIB_PATH) $(OUT_INCLUDE_PATH)
	@ echo "" >> $(LOGFILE)
	cp $(BUILD_PATH)/*.mod $(OUT_INCLUDE_PATH)
	@ echo "\n\n\n" >> $(LOGFILE)


	cd $(BUILD_PATH) && \
		$(F2PY)   -c $(OBJ_NAMES) $(SRC_WRAPPER_NAMES) -m $(F2PY_MODULE_NAME)   $(F2PYFLAGS) $(INCLUDES) $(LD_FLAGS)
	
	@ echo "" >> $(LOGFILE)
	mv $(BUILD_PATH)/$(F2PY_MODULE_NAME).cpython-$(PY_VERSION)-$(SYS_ARCH)-$(OS_TYPE).so $(OUT_LIB_PATH)


$(LIB): $(OBJS) $(SRC_WRAPPER) $(OUT_LIB_PATH)
	@ echo "\n\n\n" >> $(LOGFILE)

	cd $(BUILD_PATH) && \
		$(FC)   $(OBJ_NAMES) -shared -o $(LIB_NAME)   $(SRC_WRAPPER_NAMES) $(INCLUDES) $(LD_FLAGS) $(FFLAGS)
	
	@ echo "" >> $(LOGFILE)
	mv $(BUILD_PATH)/$(LIB_NAME) $(OUT_LIB_PATH)


$(LIB_STATIC): $(OBJS) $(SRC_WRAPPER) $(OUT_LIB_PATH)
	@ echo "\n\n\n" >> $(LOGFILE)

	cd $(BUILD_PATH) && \
		$(FC)   -c $(SRC_WRAPPER_NAMES) -o $(SRC_WRAPPER_NAMES:.f90=.o)   $(INCLUDES) $(LD_FLAGS) $(FFLAGS)
	
	cd $(BUILD_PATH) && \
		ar rcs $(LIB_STATIC_NAME) $(OBJ_NAMES) $(SRC_WRAPPER_NAMES:.f90=.o)
	
	@ echo "" >> $(LOGFILE)
	mv $(BUILD_PATH)/$(LIB_STATIC_NAME) $(OUT_LIB_PATH)


$(PROGRAM): $(OBJS) $(SRC) $(OUT_BIN_PATH)
	@ echo "\n\n\n" >> $(LOGFILE)

	cd $(BUILD_PATH) && \
		$(FC)    $(OBJ_NAMES) $(SRC_NAMES) -o $(PROGRAM_NAME)   $(INCLUDES) $(LD_FLAGS) $(FFLAGS)
	
	@ echo "" >> $(LOGFILE)
	mv $(BUILD_PATH)/$(PROGRAM_NAME) $(OUT_BIN_PATH)

	@ echo "\n\n\n" >> $(LOGFILE)
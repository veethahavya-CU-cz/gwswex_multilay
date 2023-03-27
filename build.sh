#!/bin/bash

function help() {
    echo 'Usage: ./build.sh [-p] [-n]'
    echo ' -p: compile with OpenMP'
    echo ' -n: compile without OpenMP'
    echo ' -d: compile standalone program for debugging'
    echo ' -h: print this help'
    exit 0
}

while getopts ':pndh' opt; do
    case $opt in
        p)
            cd src/
            echo '========================================  Compiling GWSWEX fortran module  ========================================' &> ../build.log
            gfortran -c Mtiming.f90 Mpaths.f90 Mlogger.f90 Muz.f90 Msolver.f90 Mstorages.f90 model.f90 \
                -I/usr/local/include -I/usr/local/include/yaml-fortran -I/usr/local/include/fgsl/ \
                -L/usr/local/lib/ -lfgsl -lgomp -lyaml-interface -lyaml-read -lyaml-wrapper -lyaml-cpp -ldatetime \
                -fopenmp -Wall -Wno-conversion -Wno-conversion-extra -Wno-tabs -O3 -ftree-vectorize -frecursive -fPIC -march=znver2 -mtune=znver2 -ffree-line-length-1024\
                >>../build.log 2>&1
            # -pedantic
            if [ $? -eq 0 ]; then
                echo $'========================================  Successfully compiled GWSWEX fortran module  ======================================== \n\n\n' >> ../build.log
                echo '========================================  Compiling GWSWEX python wrapper  ========================================' >> ../build.log
                export LDFLAGS=-Wl,-rpath=../libs/
                export NPY_DISTUTILS_APPEND_FLAGS=1
                (f2py3 --verbose -c -m gwswex_wrapper --build-dir f2py_scratch --fcompiler=gnu95 --f90flags='-fopenmp -march=znver2 -mtune=znver2  -ftree-vectorize -frecursive' --opt='-O3' \
                    -I. -I/usr/local/include -I/usr/local/include/yaml-fortran -I/usr/local/include/fgsl/ \
                    -L/usr/local/lib/ -lfgsl -lgomp -lyaml-interface -lyaml-read -lyaml-wrapper -lyaml-cpp -ldatetime \
                    Mtiming.o Mpaths.o Mlogger.o Muz.o Msolver.o Mstorages.o model.o GWSWEX_wrapper.f90) >>../build.log 2>&1
                    if [ $? -eq 0 ]; then
                        echo $'========================================  Successfully compiled GWSWEX python wrapper  ======================================== \n\n\n' >> ../build.log
                        (rm -f ../libs/*.so 2>/dev/null && mv gwswex_wrapper*.so ../libs/ 2>/dev/null) \
                            || (mkdir -p ../libs && mv gwswex_wrapper*.so ../libs/ && echo 'library directory created' >> ../build.log)
                        echo '*** placed the module library in libs/' >> ../build.log
                        echo '*** cleaning up' >> ../build.log
                        rm -rf *.mod *.o *.pyf f2py_scratch 2>/dev/null
                        cd ../
                        echo 'Build Successful'
                    else
                        echo $'========================================  Failed to compile GWSWEX python wrapper  ======================================== \n\n\n' >> ../build.log
                        echo 'Build Failed!'
                        exit 1
                    fi
            else
                echo $'========================================  Failed to compile GWSWEX fortran module  ======================================== \n\n\n' >> ../build.log
                echo 'Build Failed!'
                exit 1
            fi
            ;;
        n)
            cd src/
            echo '========================================  Compiling GWSWEX fortran module  ========================================' &> ../build.log
            gfortran -c Mtiming.f90 Mpaths.f90 Mlogger.f90 Muz.f90 Msolver.f90 Mstorages.f90 model.f90 \
                -I/usr/local/include -I/usr/local/include/yaml-fortran -I/usr/local/include/fgsl/ \
                -L/usr/local/lib/ -lfgsl -lyaml-interface -lyaml-read -lyaml-wrapper -lyaml-cpp -ldatetime \
                -Wall -Wno-conversion -Wno-conversion-extra -Wno-tabs -O3 -fPIC -march=znver2 -mtune=znver2 -ffree-line-length-1024\
                 >>../build.log 2>&1
                 # -pedantic
            if [ $? -eq 0 ]; then
                echo $'========================================  Successfully compiled GWSWEX fortran module  ======================================== \n\n\n' >> ../build.log
                echo '========================================  Compiling GWSWEX python wrapper  ========================================' >> ../build.log
                export LDFLAGS=-Wl,-rpath=../libs/
                export NPY_DISTUTILS_APPEND_FLAGS=1
                (f2py3 --verbose -c -m gwswex_wrapper --build-dir f2py_scratch --fcompiler=gnu95 --f90flags='-march=znver2 -mtune=znver2' --opt='-O3' \
                    -I. -I/usr/local/include -I/usr/local/include/yaml-fortran -I/usr/local/include/fgsl/ \
                    -L/usr/local/lib/ -lfgsl -lyaml-interface -lyaml-read -lyaml-wrapper -lyaml-cpp -ldatetime \
                    Mtiming.o Mpaths.o Mlogger.o Muz.o Msolver.o Mstorages.o model.o GWSWEX_wrapper.f90) >>../build.log 2>&1
                    if [ $? -eq 0 ]; then
                        echo $'========================================  Successfully compiled GWSWEX python wrapper  ======================================== \n\n\n' >> ../build.log
                        (rm -f ../libs/*.so 2>/dev/null && mv gwswex_wrapper*.so ../libs/ 2>/dev/null) \
                            || (mkdir -p ../libs && mv gwswex_wrapper*.so ../libs/ && echo 'library directory created' >> ../build.log)
                        echo '*** placed the module library in libs/' >> ../build.log
                        echo '*** cleaning up' >> ../build.log
                        rm -rf *.mod *.o *.pyf f2py_scratch 2>/dev/null
                        cd ../
                        echo 'Build Successful'
                    else
                        echo $'========================================  Failed to compile GWSWEX python wrapper  ======================================== \n\n\n' >> ../build.log
                        echo 'Build Failed!'
                        exit 1
                    fi
            else
                echo $'========================================  Failed to compile GWSWEX fortran module  ======================================== \n\n\n' >> ../build.log
                echo 'Build Failed!'
                exit 1
            fi
            ;;
        d)
            cd src/
            echo '========================================  Compiling GWSWEX fortran module  ========================================' &> ../build.log
            gfortran -c Mtiming.f90 Mpaths.f90 Mlogger.f90 Muz.f90 Msolver.f90 Mstorages.f90 model.f90 -g -fbacktrace -fcheck=all \
                -I/usr/local/include -I/usr/local/include/yaml-fortran -I/usr/local/include/fgsl/ \
                -L/usr/local/lib/ -lfgsl -lgomp -lyaml-interface -lyaml-read -lyaml-wrapper -lyaml-cpp -ldatetime \
                -fopenmp -Wall -Wno-conversion -Wno-conversion-extra -Wno-tabs -O3 -fPIC -march=znver2 -mtune=znver2 -ffree-line-length-1024\
                >>../build.log 2>&1
            # -pedantic -fsanitize=address,zero,undefined
            if [ $? -eq 0 ]; then
                echo $'========================================  Successfully compiled GWSWEX fortran module  ======================================== \n\n\n' >> ../build.log
                echo '========================================  Compiling GWSWEX debugger  ========================================' >> ../build.log
                export LDFLAGS=-Wl,-rpath=../libs/
                export NPY_DISTUTILS_APPEND_FLAGS=1
                (gfortran Mtiming.o Mpaths.o Mlogger.o Muz.o Msolver.o Mstorages.o model.o ../testGWSWEX.f90 -o GWSWEX_debugger -g -fopenmp -march=znver2 -mtune=znver2 -O3 \
                    -I. -I/usr/local/include -I/usr/local/include/yaml-fortran -I/usr/local/include/fgsl/ \
                    -L/usr/local/lib/ -lfgsl -lgomp -lyaml-interface -lyaml-read -lyaml-wrapper -lyaml-cpp -ldatetime) >>../build.log 2>&1
                    if [ $? -eq 0 ]; then
                        echo $'========================================  Successfully compiled GWSWEX debugger  ======================================== \n\n\n' >> ../build.log
                        (rm -f ../libs/*.so 2>/dev/null && mv GWSWEX_debugger ../bin/ 2>/dev/null) \
                            || (mkdir -p ../bin && mv GWSWEX_debugger ../bin/ && echo 'binaries directory created' >> ../build.log)
                        echo '*** placed the module library in bin/' >> ../build.log
                        echo '*** cleaning up' >> ../build.log
                        rm -rf *.mod *.o 2>/dev/null
                        cd ../
                        echo 'Build Successful'
                    else
                        echo $'========================================  Failed to compile GWSWEX python wrapper  ======================================== \n\n\n' >> ../build.log
                        echo 'Build Failed!'
                        exit 1
                    fi
            else
                echo $'========================================  Failed to compile GWSWEX fortran module  ======================================== \n\n\n' >> ../build.log
                echo 'Build Failed!'
                exit 1
            fi
            ;;
        h)
        help
        ;;
    esac
done

# Defaults to compiling with OpenMP
if (( $OPTIND == 1 )); then
	cd src/
    echo '========================================  Compiling GWSWEX fortran module  ========================================' &> ../build.log
    gfortran -c Mtiming.f90 Mpaths.f90 Mlogger.f90 Muz.f90 Msolver.f90 Mstorages.f90 model.f90 \
        -I/usr/local/include -I/usr/local/include/yaml-fortran -I/usr/local/include/fgsl/ \
        -L/usr/local/lib/ -lfgsl -lgomp -lyaml-interface -lyaml-read -lyaml-wrapper -lyaml-cpp -ldatetime \
        -fopenmp -Wall -Wno-conversion -Wno-conversion-extra -Wno-tabs -O3 -fPIC -march=znver2 -mtune=znver2 -ffree-line-length-1024\
        >>../build.log 2>&1
        # -pedantic
    if [ $? -eq 0 ]; then
        echo $'========================================  Successfully compiled GWSWEX fortran module  ======================================== \n\n\n' >> ../build.log
        echo '========================================  Compiling GWSWEX python wrapper  ========================================' >> ../build.log
        export LDFLAGS=-Wl,-rpath=../libs/
        export NPY_DISTUTILS_APPEND_FLAGS=1
        (f2py3 --verbose -c -m gwswex_wrapper --build-dir f2py_scratch --fcompiler=gnu95 --f90flags='-fopenmp -march=znver2 -mtune=znver2' --opt='-O3' \
            -I. -I/usr/local/include -I/usr/local/include/yaml-fortran -I/usr/local/include/fgsl/ \
            -L/usr/local/lib/ -lfgsl -lgomp -lyaml-interface -lyaml-read -lyaml-wrapper -lyaml-cpp -ldatetime \
            Mtiming.o Mpaths.o Mlogger.o Muz.o Msolver.o Mstorages.o model.o GWSWEX_wrapper.f90) >>../build.log 2>&1
            if [ $? -eq 0 ]; then
                echo $'========================================  Successfully compiled GWSWEX python wrapper  ======================================== \n\n\n' >> ../build.log
                (rm -f ../libs/*.so 2>/dev/null && mv gwswex_wrapper*.so ../libs/ 2>/dev/null) \
                    || (mkdir -p ../libs && mv gwswex_wrapper*.so ../libs/ && echo 'library directory created' >> ../build.log)
                echo '*** placed the module library in libs/' >> ../build.log
                echo '*** cleaning up' >> ../build.log
                rm -rf *.mod *.o *.pyf f2py_scratch 2>/dev/null
                cd ../
                echo 'Build Successful'
            else
                echo $'========================================  Failed to compile GWSWEX python wrapper  ======================================== \n\n\n' >> ../build.log
                echo 'Build Failed!'
                exit 1
            fi
    else
        echo $'========================================  Failed to compile GWSWEX fortran module  ======================================== \n\n\n' >> ../build.log
        echo 'Build Failed!'
        exit 1
    fi
fi
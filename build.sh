#!/bin/bash

function help() {
    echo 'Usage: ./build.sh [-p] [-n]'
    echo ' -p: compile with OpenMP'
    echo ' -n: compile without OpenMP'
    echo ' -h: print this help'
    exit 0
}

while getopts ':pnh' opt; do
    case $opt in
        p)
            cd src/
            echo '========================================  Compiling GWSWEX fortran module  ========================================' &> ../build.log
            gfortran -c GWSWEX.f90 \
                -L/usr/local/lib/ -lfgsl -lgomp -lyaml-interface -lyaml-read -lyaml-wrapper -lyaml-cpp -ldatetime \
                -I/usr/local/include -I/usr/local/include/yaml-fortran -I/usr/local/include/fgsl/ \
                -fopenmp -Wall -Wno-conversion -Wno-tabs -pedantic -O3 -fPIC -march=znver2 -mtune=znver2 -ffree-line-length-1024\
                >>../build.log 2>&1
            if [ $? -eq 0 ]; then
                echo $'========================================  Successfully compiled GWSWEX fortran module  ======================================== \n\n\n' >> ../build.log
                echo '========================================  Compiling GWSWEX python wrapper  ========================================' >> ../build.log
                export LDFLAGS=-Wl,-rpath=../libs/
                export NPY_DISTUTILS_APPEND_FLAGS=1
                (f2py --verbose -c -m gwswex_wrapper --build-dir f2py_scratch --fcompiler=gnu95 --f90flags='-fopenmp -march=znver2 -mtune=znver2' --opt='-O3' \
                    -I. -I/usr/local/include -I/usr/local/include/yaml-fortran -I/usr/local/include/fgsl/ \
                    -L/usr/local/lib/ -lfgsl -lgomp -lyaml-interface -lyaml-read -lyaml-wrapper -lyaml-cpp -ldatetime \
                    GWSWEX.o GWSWEX_wrapper.f90) >>../build.log 2>&1
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
            gfortran -c GWSWEX.f90 \
                -L/usr/local/lib/ -lfgsl -lyaml-interface -lyaml-read -lyaml-wrapper -lyaml-cpp -ldatetime \
                -I/usr/local/include -I/usr/local/include/yaml-fortran -I/usr/local/include/fgsl/ \
                -fopenmp -Wall -Wno-conversion -Wno-tabs -pedantic -O3 -fPIC -march=znver2 -mtune=znver2 -ffree-line-length-1024\
                 >>../build.log 2>&1
            if [ $? -eq 0 ]; then
                echo $'========================================  Successfully compiled GWSWEX fortran module  ======================================== \n\n\n' >> ../build.log
                echo '========================================  Compiling GWSWEX python wrapper  ========================================' >> ../build.log
                export LDFLAGS=-Wl,-rpath=../libs/
                export NPY_DISTUTILS_APPEND_FLAGS=1
                (f2py --verbose -c -m gwswex_wrapper --build-dir f2py_scratch --fcompiler=gnu95 --f90flags='-march=znver2 -mtune=znver2' --opt='-O3' \
                    -I. -I/usr/local/include -I/usr/local/include/yaml-fortran -I/usr/local/include/fgsl/ \
                    -L/usr/local/lib/ -lfgsl -lyaml-interface -lyaml-read -lyaml-wrapper -lyaml-cpp -ldatetime \
                    GWSWEX.o GWSWEX_wrapper.f90) >>../build.log 2>&1
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
        h)
        help
        ;;
    esac
done

# Defaults to compiling with OpenMP
if (( $OPTIND == 1 )); then
	cd src/
    echo '========================================  Compiling GWSWEX fortran module  ========================================' &> ../build.log
    gfortran -c GWSWEX.f90 \
        -L/usr/local/lib/ -lfgsl -lgomp -lyaml-interface -lyaml-read -lyaml-wrapper -lyaml-cpp -ldatetime \
        -I/usr/local/include -I/usr/local/include/yaml-fortran -I/usr/local/include/fgsl/ \
        -fopenmp -Wall -Wno-conversion -Wno-tabs -pedantic -O3 -fPIC -march=znver2 -mtune=znver2 -ffree-line-length-1024\
        >>../build.log 2>&1
    if [ $? -eq 0 ]; then
        echo $'========================================  Successfully compiled GWSWEX fortran module  ======================================== \n\n\n' >> ../build.log
        echo '========================================  Compiling GWSWEX python wrapper  ========================================' >> ../build.log
        export LDFLAGS=-Wl,-rpath=../libs/
        export NPY_DISTUTILS_APPEND_FLAGS=1
        (f2py --verbose -c -m gwswex_wrapper --build-dir f2py_scratch --fcompiler=gnu95 --f90flags='-fopenmp -march=znver2 -mtune=znver2' --opt='-O3' \
            -I. -I/usr/local/include -I/usr/local/include/yaml-fortran -I/usr/local/include/fgsl/ \
            -L/usr/local/lib/ -lfgsl -lgomp -lyaml-interface -lyaml-read -lyaml-wrapper -lyaml-cpp -ldatetime \
            GWSWEX.o GWSWEX_wrapper.f90) >>../build.log 2>&1
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
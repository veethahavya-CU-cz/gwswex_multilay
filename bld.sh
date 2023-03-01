# gfortran -c GWSWEX.f90 -L/usr/local/lib/ -lfgsl -lgomp -lyaml-interface -lyaml-read -lyaml-wrapper -lyaml-cpp -ldatetime -I/usr/local/include -I/usr/local/include/yaml-fortran -I/usr/local/include/fgsl/ -fopenmp -Wall -Wno-conversion -Wno-conversion-extra -Wno-tabs -O3 -fPIC -march=znver2 -mtune=znver2 -ffree-line-length-1024

# gfortran -shared -fPIC -o gwswex.so GWSWEX.f90 -L/usr/local/lib/ -lfgsl -lgomp -lyaml-interface -lyaml-read -lyaml-wrapper -lyaml-cpp -ldatetime -I/usr/local/include -I/usr/local/include/yaml-fortran -I/usr/local/include/fgsl/ -fopenmp -Wall -Wno-conversion -Wno-conversion-extra -Wno-tabs -O3 -fPIC -march=znver2 -mtune=znver2 -ffree-line-length-1024

while getopts ':cr' opt; do
    case $opt in
        c)
            clear
            rm ../libs/gwswex_cdll_wrapper.so
            gfortran -c GWSWEX.f90 -L/usr/local/lib/ -lfgsl -lgomp -lyaml-interface -lyaml-read -lyaml-wrapper -lyaml-cpp -ldatetime -I/usr/local/include -I/usr/local/include/yaml-fortran -I/usr/local/include/fgsl/ -fopenmp -Wall -Wno-conversion -Wno-conversion-extra -Wno-tabs -O3 -fPIC -march=znver2 -mtune=znver2 -ffree-line-length-1024
            gfortran -shared -fPIC -o gwswex_cdll_wrapper.so GWSWEX_cdll_wrapper.f90 GWSWEX.o -L/usr/local/lib/ -lfgsl -lgomp -lyaml-interface -lyaml-read -lyaml-wrapper -lyaml-cpp -ldatetime -I/usr/local/include -I/usr/local/include/yaml-fortran -I/usr/local/include/fgsl/ -fopenmp -Wall -Wno-conversion -Wno-conversion-extra -Wno-tabs -O3 -fPIC -march=znver2 -mtune=znver2 -ffree-line-length-1024
            mv gwswex_cdll_wrapper.so ../libs/gwswex_cdll_wrapper.so
            python ../run_via_cdll.py
            rm *.mod *.o
            ;;
        r)
            rm ../libs/gwswex_cdll_wrapper.so
            gfortran -c GWSWEX.f90 -L/usr/local/lib/ -lfgsl -lgomp -lyaml-interface -lyaml-read -lyaml-wrapper -lyaml-cpp -ldatetime -I/usr/local/include -I/usr/local/include/yaml-fortran -I/usr/local/include/fgsl/ -fopenmp -Wall -Wno-conversion -Wno-conversion-extra -Wno-tabs -O3 -fPIC -march=znver2 -mtune=znver2 -ffree-line-length-1024
            gfortran -shared -fPIC -o gwswex_cdll_wrapper.so GWSWEX_cdll_wrapper.f90 GWSWEX.o -L/usr/local/lib/ -lfgsl -lgomp -lyaml-interface -lyaml-read -lyaml-wrapper -lyaml-cpp -ldatetime -I/usr/local/include -I/usr/local/include/yaml-fortran -I/usr/local/include/fgsl/ -fopenmp -Wall -Wno-conversion -Wno-conversion-extra -Wno-tabs -O3 -fPIC -march=znver2 -mtune=znver2 -ffree-line-length-1024
            mv gwswex_cdll_wrapper.so ../libs/gwswex_cdll_wrapper.so
            python ../run_via_cdll.py
            rm *.mod *.o
            ;;
    esac
done

if (( $OPTIND == 1 )); then
    gfortran -c GWSWEX.f90 -L/usr/local/lib/ -lfgsl -lgomp -lyaml-interface -lyaml-read -lyaml-wrapper -lyaml-cpp -ldatetime -I/usr/local/include -I/usr/local/include/yaml-fortran -I/usr/local/include/fgsl/ -fopenmp -Wall -Wno-conversion -Wno-conversion-extra -Wno-tabs -O3 -fPIC -march=znver2 -mtune=znver2 -ffree-line-length-1024
    gfortran -shared -fPIC -o gwswex_cdll_wrapper.so GWSWEX_cdll_wrapper.f90 GWSWEX.o -L/usr/local/lib/ -lfgsl -lgomp -lyaml-interface -lyaml-read -lyaml-wrapper -lyaml-cpp -ldatetime -I/usr/local/include -I/usr/local/include/yaml-fortran -I/usr/local/include/fgsl/ -fopenmp -Wall -Wno-conversion -Wno-conversion-extra -Wno-tabs -O3 -fPIC -march=znver2 -mtune=znver2 -ffree-line-length-1024
    mv gwswex_cdll_wrapper.so ../libs/gwswex_cdll_wrapper.so
    rm *.mod *.o
fi
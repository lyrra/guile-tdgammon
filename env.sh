
# For different implemetation of BLAS see https://wiki.debian.org/DebianScience/LinearAlgebraLibraries
# If selecting openBLAS, for debian install package libopenblas-pthread-dev
#
# point out your blas-library: libblas.so, libatlas.so, /usr/lib/lapack/cygblas-0.dll etc
#export GUILE_FFI_CBLAS_LIBNAME=/usr/lib/libblas.so
export GUILE_FFI_CBLAS_LIBNAME=/usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so

# C modules used by td-gammon
export GUILE_FFI_NNGPU_LIBNAME=lib/gpu/libnn_gpu.so
export GUILE_FFI_NNPUBEVAL_LIBNAME=lib/pubeval/libnn_pubeval.so

# path to guile-ffi-cblas git repository (https://github.com/lloda/guile-ffi-cblas)
GUILE_CODE_LOAD_PATH="-L $HOME/git/guile-ffi-cblas/mod -L $HOME/git"


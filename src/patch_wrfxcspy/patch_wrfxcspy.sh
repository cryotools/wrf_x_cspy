#!/bin/bash -l
# Copyright 2024 Nicolas Gampierakis

INPUT=""
CONFIGURE=0
INSTALL_ALL=0
INSTALL_WRF=0
INSTALL_COSIPY=0
INSTALL_COUPLER=0
WRF_BRANCH="release-v4.6.1"
DELETE=0
ENVIRONMENT=0
PATCH=0
BUILD_WRF=""
VERBOSE=0

DisplayHelp() {
    intro_tag="$(basename "$0") [-h] [-i n] -- wrapper for patching WRF"
    blurb="Example: Load environment variables, create configuration file, and patch:"
    example_command="\$ bash ./patch_wrfxcspy.sh -e -c -p -i '/foo/bar/'"
    options="OPTIONS
    -i, --input <file>      Source WRF directory, relative to current working directory.
    --install-all           Download WRF, NoahMP submodule, COSIPY, and the WRFxCSPY coupler. Does not build WRF.
    --install-wrf           Download only WRF and the NoahMP submodule. Does not build WRF.
    --install-cosipy        Download only COSIPY.
    --install-coupler       Download and build only the coupler code.
    --wrf-branch <str>      Name of WRF branch on GitHub. Defaults to 'release-v4.6.1'.
    -c, --configure         Create and patch WRF configuration script.
    -d, --delete            Run make clean in source directory.
    -p, --patch             Patch COSIPY into WRF.
    -e, --env               Load environment variables.
    -b, --build <case>      Compile WRF case.
    -v, --verbose           Prints log messages to stderr.
    "
    printf "\n%s\n\n%s\n%s\n\n%s\n" "$intro_tag" "$blurb" "$example_command" "$options"
}

#######################################
# Prints a log message to stderr if the verbosity flag is non-zero.
#
# Arguments:
#   String with logged message.
#######################################
log() {
    if [[ $VERBOSE -eq 1 ]]; then
        printf '\n%s\n' "$@" >&2
    fi
}

# Argument parsing
ARGS=$(getopt -o 'hi:cdpeb:v' --long 'help,input:,clean,delete,patch,env,build:,verbose,install-all,install-wrf,install-cosipy,install-coupler,wrf-branch:' -- "$@") || exit
eval "set -- $ARGS"
while true; do
    case $1 in
    -h | --help)
        DisplayHelp
        exit 0
        ;;
    -i | --input)
        INPUT=$2
        shift 2
        ;;
    -c | --configure)
        CONFIGURE=1
        shift
        ;;
    -d | --delete)
        DELETE=1
        shift
        ;;
    -p | --patch)
        PATCH=1
        shift
        ;;
    -e | --env)
        ENVIRONMENT=1
        shift
        ;;
    -b | --build)
        BUILD_WRF=$2
        shift 2
        ;;
    -v | --verbose)
        ((VERBOSE++))
        shift
        ;;
    --install-all)
        INSTALL_ALL=1
        shift
        ;;
    --install-wrf)
        INSTALL_WRF=1
        shift
        ;;
    --install-cosipy)
        INSTALL_COSIPY=1
        shift
        ;;
    --install-coupler)
        INSTALL_COUPLER=1
        shift
        ;;
    --wrf-branch)
        WRF_BRANCH=$2
        shift 2
        ;;

    --)
        shift
        break
        ;;
    *) exit 1 ;; # error
    esac
done
remaining=("$@")

if [ -z "${INPUT}" ]; then
    INPUT="${PWD}/${INPUT}"
fi
log "WRF directory: ${INPUT}"
current_dir=${PWD}

if [[ $INSTALL_ALL -eq 1 ]]; then
    INSTALL_WRF=1
    INSTALL_COSIPY=1
    INSTALL_COUPLER=1
fi

readonly INSTALL_ALL
readonly INSTALL_WRF
readonly INSTALL_COSIPY
readonly INSTALL_COUPLER
readonly CONFIGURE
readonly DELETE
readonly ENVIRONMENT
readonly VERBOSE
readonly INPUT
readonly WRF_BRANCH

# Get WRF, COSIPY
if [[ $INSTALL_COSIPY -eq 1 ]]; then
    log "Download COSIPY"
    git clone https://github.com/cryotools/cosipy.git "${INPUT}/cosipy"
fi

if [[ $INSTALL_WRF -eq 1 ]]; then
    log "Download WRF"
    # recurse submodules is necessary for loading noahmp driver
    git clone --recurse-submodules https://github.com/wrf-model/WRF.git --branch "${WRF_BRANCH}" "${INPUT}/WRF"
fi

if [[ $INSTALL_COUPLER -eq 1 ]]; then
    log "Install coupler"
    # mkdir -p "${INPUT}/COUPLER/"
    cp -RT "${PWD}/patch_files/COUPLER/" "${INPUT}/COUPLER/"
    current_dir=${PWD}
    cd "${INPUT}/COUPLER/" || exit
    python build_cosipy_api.py || exit
    cd "${current_dir}" || exit
fi

# Paths must be edited by user
if [[ $ENVIRONMENT -eq 1 ]]; then
    log "Load environment variables (not exported to your current shell)"
    . build_wrf.sh
fi

# Clean make files
if [[ $DELETE -eq 1 ]]; then
    log "Clean make files"
    "${INPUT}"/clean -a
fi

# Run WRF configuration
if [[ $CONFIGURE -eq 1 ]]; then
    log "Run WRF configuration"
    cd "${INPUT}" || exit
    ./configure
    cd "${current_dir}" || exit
    # add coupler flags to configure.wrf - these are overwritten when running ./configure
    log "Add coupler flags to configure.wrf..."
    sed -i '/ LIB_EXTERNAL    =/{n;/COSIPY_API/! s/$/ -L\$(COSIPY_API)/;}' "${INPUT}"/configure.wrf
    sed -i '/ LIB_EXTERNAL    =/{n;/lcosipywrf/! s/$/ -lcosipywrf/;}' "${INPUT}"/configure.wrf
    # sed -i '/ LIB_EXTERNAL    =/{n;/lcosipywrf/! s/$/ -L\\$(COSIPY_API) -lcosipywrf/;}' configure.wrf

    # replace lhdf5hl_fortran with ldhf5_hl_fortran
    log "Replace lhdf5hl_fortran with ldhf5_hl_fortran..."
    for file in "${INPUT}"/configure.wrf "${INPUT}"/compile "${INPUT}"/arch/Config.pl; do
        sed -i '/lhdf5hl_fortran/ s/lhdf5hl_fortran/lhdf5_hl_fortran/' "${file}"
    done
fi

# Patch WRF with coupled model
if [[ $PATCH -eq 1 ]]; then
    log "Patching WRF_X_CSPY..."
    perl backend_patcher.pl --input "${INPUT}"
fi

if [[ $BUILD_WRF ]]; then
    log "Building WRF..."
    date=$(date +%Y%m%d_%H00)
    cd "${INPUT}" || exit
    ./compile "${BUILD_WRF}" >&log.compile_"${date}"
    cd "${current_dir}" || exit
fi

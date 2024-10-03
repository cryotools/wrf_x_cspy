#!/bin/bash -l
# Copyright 2024 Nicolas Gampierakis

INPUT=""
CONFIGURE=0
WRF_INSTALL=0
COSIPY_INSTALL=0
DELETE=0
ENVIRONMENT=0
PATCH=0
VERBOSE=0
YEAR=$(date +%Y)

DisplayHelp(){
    intro_tag="$(basename "$0") [-h] [-i n] -- wrapper for patching WRF"
    blurb="Example: Load environment variables, create configuration file, and patch:"
    example_command="\$ bash ./patch_wrfxcspy.sh -e -c -p"
    options="OPTIONS
    -i, --input [file]      Source WRF directory, relative to current working directory.
    --install-wrf           Download WRF. This will not build WRF for you.
    --install-cosipy        Download COSIPY.
    -c, --configure         Create new WRF configuration script.
    -d, --delete            Run make clean in source directory.
    -p, --patch             Patch COSIPY into WRF. Does not check if already patched.
    -e, --env               Load environment variables.
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
log () {
    if [[ $VERBOSE -eq 1 ]]; then
        printf '\n%s\n' "$@">&2
    fi
}

#######################################
# Get patch label
#
# Arguments:
#   Label prefix
#######################################
get_label () {
    label="$@WRF_X_CSPY EC${YEAR}"
}


# Argument parsing
ARGS=$(getopt -o 'hi:cdpev' --long 'help,input:,clean,delete,patch,env,verbose,install-cosipy,install-wrf' -- "$@") || exit
eval "set -- $ARGS"
while true; do
    case $1 in
        (-h|--help)
            DisplayHelp; exit 0;;
        (-i|--input)
            INPUT=$2; shift 2;;
        (-c|--configure)
            CONFIGURE=1; shift;;
        (-d|--delete)
            DELETE=1; shift;;
        (-p|--patch)
            PATCH=1; shift;;
        (-e|--env)
            ENVIRONMENT=1; shift;;
        (-v|--verbose)
            ((VERBOSE++)); shift;;
        (--install-cosipy)
            COSIPY_INSTALL=1; shift;;
        (--install-wrf)
            WRF_INSTALL=1; shift;;

        (--)  shift; break;;
        (*)   exit 1;; # error
    esac
done
remaining=("$@")

if [[ $INPUT ]]; then
    INPUT=${INPUT}
else
    INPUT="${PWD}/${INPUT}"
fi
log "WRF directory: ${INPUT}"


readonly CONFIGURE
readonly DELETE
readonly ENVIRONMENT
readonly VERBOSE
readonly YEAR

# Get WRF, COSIPY
if [[ $COSIPY_INSTALL -eq 1 ]]; then
    log "Download COSIPY"
    git clone https://github.com/cryotools/cosipy.git ${INPUT}/cosipy
fi

if [[ $WRF_INSTALL -eq 1 ]]; then
    log "Download WRF"
    # git clone git@github.com:wrf-model/WRF.git --branch v4.6.0
    git clone https://github.com/wrf-model/WRF.git --branch v4.6.0 ${INPUT}/WRF
    INPUT=${INPUT}
fi

readonly INPUT

# Paths must be edited by user
if [[ $ENVIRONMENT -eq 1 ]]; then
    log "Load environment variables"
    source build_wrf.sh
fi

# Clean make files
if [[ $DELETE -eq 1 ]]; then
    log "Clean make files"
    "${INPUT}"/clean -a
fi

# Run WRF configuration
if [[ $CONFIGURE -eq 1 ]]; then
    log "Run WRF configuration"
    "${INPUT}"/configure
    # add coupler flags to configure.wrf - these are overwritten when running ./configure
    log "Add coupler flags to configure.wrf..."
    sed -i '/ LIB_EXTERNAL    =/{n;/COSIPY_API/! s/$/ -L\$(COSIPY_API)/;}' "${INPUT}"/configure.wrf
    sed -i '/ LIB_EXTERNAL    =/{n;/lcosipywrf/! s/$/ -lcosipywrf/;}' "${INPUT}"/configure.wrf
    # sed -i '/ LIB_EXTERNAL    =/{n;/lcosipywrf/! s/$/ -L\\$(COSIPY_API) -lcosipywrf/;}' configure.wrf

    # replace lhdf5hl_fortran with ldhf5_hl_fortran
    log "Replace lhdf5hl_fortran with ldhf5_hl_fortran..."
    for file in "${INPUT}"/configure.wrf "${INPUT}"/compile "${INPUT}"/arch/Config.pl
    do
        sed -i '/lhdf5hl_fortran/ s/lhdf5hl_fortran/lhdf5_hl_fortran/' $file
    done
fi

# Patch WRF with coupled model
if [[ $PATCH -eq 1 ]]; then
    log "Patching WRF_X_CSPY..."
    perl backend_patcher.pl -i ${INPUT}
fi

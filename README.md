# WRFxCSPY

All-in-one coupler for [COSIPY](https://cosipy.readthedocs.io) and [WRF](https://github.com/wrf-model/WRF).

## Pre-requisites

1. Install patcher dependencies.

Perl version 5.26.3 or greater must be installed on your system.
Depending on your distribution, you may also need to install the following:

* perl-time-piece
* perl-path-tiny

If you are using spack, this might look something like this:

```console
spack install perl@5.36.0 %gcc@8.5.0
spack install perl-path-tiny perl-time-piece %gcc@8.5.0 ^perl@5.36.0
module load perl/5.36 perl-path-tiny perl-time-piece
```

2. Install CFFI to build the coupler.

If you are using conda/mamba to manage your environments, CFFI should already be installed.

If you are using a pip venv:
```console
pip install cffi
```

## Patching

1. Activate your preferred python environment, with CFFI installed.

2. Install WRF's dependencies.

3. Use the script to download WRF, the NoahMP submodule, COSIPY, and the coupler:
```console
./patch_wrfxcspy.sh --install-all -i ./desired/path/to/installation/folder/
```

4. Edit ``build_wrf.sh`` to point to the right paths for ``COSIPY_API``, ``COSIPY_DIR``, ``NETCDF``, ``HDF5``, and ``JASPER_DIR``.
**Note that environment variables loaded by the ``-e, --env`` flag`` are not exported to your current shell.**

Alternatively, you can load your own build file using ``source /path/to/build_wrf.sh``, and export ``COSIPY_API`` and ``COSIPY_DIR`` to your ``LD_LIBRARY_PATH``.
Do this now and omit the ``-e, --env`` flag from subsequent commands.

5. Configure and patch WRF:
```console
./patch_wrfxcspy -e -c -p -i ./path/to/WRF/
```

6. Build WRF: 

```console
# If using the supplied build_wrf.sh
./patch_wrfxcspy -e -i ./path/to/WRF/ -b "em_real"

# If using your own build file
cd /path/to/wrf
source /path/to/build_wrf.sh
./compile em_real >& log.compile
```

**Arguments:**

```properties
-i, --input <file>  Source WRF directory, relative to current working directory.
--install-all       Download WRF, NoahMP submodule, COSIPY, and the WRFxCSPY coupler. This will not build WRF for you.
--install-wrf       Download only WRF and NoahMP drivers. This will not build WRF for you.
--install-cosipy    Download only COSIPY.
--install-coupler   Install only the coupler code.
--wrf-branch <str>  Name of WRF branch on GitHub. Defaults to 'release-v4.6.1'.
-c, --configure     Create new WRF configuration script.
-p, --patch         Patch COSIPY into WRF.
-e, --env           Load environment variables.
-v, --verbose       Prints log messages to stderr.
```

## Adding Patches

You can also view this documentation by running ``perldoc backend_patcher.pl``.

Patching a file in ``backend_patcher.pl`` always follows the same template.
Note that brackets in the string being matched must be escaped with ``\\``.

```perl
my $input_file = "${input_dir}path/to/file";
$check_file = get_file_is_safe($input_file); # file exists and isn't patched
if ($check_file) {
    copy_file($input_file); # backup original file
    $string_match = "happy birthday"; # whitespace-sensitive
    $label        = set_patch_label();  # if you want to include a signature
    $whitespace = " " x 5 # if you want to include whitespace
    # patch method goes here
}
```

### Add a line

You can add individual lines by hardcoding the script:

```perl
$string_match = "happy birthday"; # whitespace-sensitive
$string_new = "${label}${whitespace}to you!\nhappy birthday";
add_line_to_file( $input_file, $string_match, $string_new, "p" );
```
``add_line_to_file`` takes three modes: "p" (prepend), "a" (append), "r" (replace).

### Add from patch file

For more complex patching, add a file with an identical name to the one you wish to patch under ``./patch_files``.
Separate hunks using ``===N===``, where N is an index.

```
===1===
some string
and another
===1===
===2===
a second paragraph
    with whitespace
===2===
```

Use the index number to refer directly to the hunk you wish to add:

```perl
$patch_file = "${patch_dir}file";
$string_match = "some string"; # whitespace-sensitive
patch_from_file_array( $input_file, $patch_file, $string_match, 1, "a" );
```

``patch_from_file_array`` takes three modes: "p" (prepend), "a" (append), "r" (replace).

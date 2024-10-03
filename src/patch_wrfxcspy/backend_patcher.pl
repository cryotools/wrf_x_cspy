#!/usr/bin/perl -i
# Copyright 2024 Nicolas Gampierakis

use strict;
use warnings;
use Time::Piece;
use Path::Tiny;
use File::Copy;
use Getopt::Long;

my $input_dir      = "./";
my $patch_dir      = "./patch_files/";
my $user_arguments = GetOptions( "input=s" => \$input_dir );

my $label;
my $patch_path;

my $whitespace;
my @array;
my @file;

sub get_year {
    my $t = Time::Piece->new();
    return $t->year;
}

=head1 patch_wrfxcspy
=cut

sub set_patch_label {
    my ( $prefix, $year ) = @_;
    $prefix //= "#";
    $year   //= 1;
    my $label = "${prefix}WRF_X_CSPY";
    if ( $year == 1 ) {
        $year  = get_year();
        $label = "${label} EC${year}\n";
    }
    else {
        $label = "${label}\n";
    }

    return $label;
}

sub set_label_wrap {
    my ( $text, $prefix ) = @_;
    $prefix //= "#";
    my $label_start = set_patch_label($prefix);
    my $label_end   = set_patch_label( $prefix, 0 );
    $text = "${label_start}${text}${label_end}";
    return $text;

}

sub open_file {
    my $input_file = $_[0];
    rename( $input_file, $input_file . '.bak' );
    open my $in,  '+<', "$input_file.bak" or die "Can't read file: $!";
    open my $out, '+>', "$input_file"     or die "Can't read file: $!";

    return ( $in, $out );
}

sub open_file_tie {
    tie my @file, 'Tie::File', $_[0] or die "Can't read file: $!";

    # untie @file;
    return @file;
}

=head2 I<Regex Functions>

=over

=item regex_prepend_to_line($match_string, $new_text)

Prepends missing text before matching string.

=item regex_append_to_line($match_string, $new_text)

Appends missing text after matching string.

=back

=cut

sub regex_prepend_to_line {
    my ( $match_string, $new_text ) = @_;
    s/$match_string(?!.*$match_string)/$new_text\n$&/s;
}

sub regex_append_to_line {
    my ( $match_string, $new_text ) = @_;
    my $check;

    s/($match_string.*)/$match_string\n$new_text/g;
}

sub regex_prepend_to_line_number {
    my ( $match_string, $new_text ) = @_;

    s/$match_string(?!.*$match_string)(?!.*!\n)/#$&/s;

}

=head2 I<File Editing>

=over

=item add_line_to_file($file_path, $match_string, $new_text, $mode)

Add line to file.

Parameters:

=over

=item path to file

=item string to match

=item missing text

=item edit mode: 'p' -> prepend, 'a' -> append, 'n' -> prepend by n lines.

=back

=item patch_from_file($file_path, $patch_path, $match_string, $mode)

Patches text from another file.

Parameters:

=over

=item path to file

=item path to patch file (automatically generated)

=item string to match

=item edit mode: 'p' -> prepend, 'a' -> append, 'n' -> prepend by n lines.

=back

=back

=cut

sub add_line_to_file {
    my ( $file_path, $match_string, $new_text, $mode ) = @_;
    my ( $in, $out ) = open_file($file_path);
    while (<$in>) {
        if ( $mode eq "p" ) {
            regex_prepend_to_line( $match_string, $new_text );
        }
        elsif ( $mode eq "n" ) {
            regex_prepend_to_line_number( $match_string, $new_text );
        }
        elsif ( $mode eq "a" ) {
            regex_append_to_line( $match_string, $new_text );
        }
        else {
            die "Mode $! not supported.";
        }

        print $out $_;
    }
    close $in;
    close $out;
}

sub patch_from_file {
    my ( $file_path, $patch_path, $match_string, $mode ) = @_;
    my ( $in, $out ) = open_file($file_path);
    my $patch_text = path($patch_path)->slurp;
    $patch_text = set_label_wrap( $patch_text, "#" );
    while (<$in>) {
        if ( $mode eq "p" ) {
            regex_prepend_to_line( $match_string, $patch_text );
        }
        elsif ( $mode eq "a" ) {
            regex_append_to_line( $match_string, $patch_text );
        }
        else {
            die "Mode $! not supported.";
        }

        print $out $_;
    }

    # }
    close $out;
    close $patch_text;
}

sub patch_from_file_array {

    # hunks are separated by ===N=== in the corresponding patch file.
    my ( $file_path, $patch_path, $match_string, $index, $mode ) = @_;
    my ( $in, $out ) = open_file($file_path);
    my $patch_contents = path($patch_path)->slurp;
    my $patch_text;
    while (<$in>) {

        @array      = split( /(={3,}?${index}={3,}\n.*?)/, $patch_contents );
        $patch_text = set_label_wrap( $array[2], "!" );
        if ( $mode eq "p" ) {
            regex_prepend_to_line( $match_string, $patch_text );
        }
        elsif ( $mode eq "a" ) {
            regex_append_to_line( $match_string, $patch_text );
        }
        else {
            die "Mode $! not supported.";
        }

        print $out $_;
    }

    close $out;
    close $patch_text;
}

# ###############
# PATCHER LOGIC
# ###############

# Patch instructions

# Patching is based on matching strings, not line numbers. Each patch
# requires a source file path, a matching string, and either a string or
# path to a patch file.

# You can add individual lines to a file.
# Use "a" for append or "p" for prepend.

# my $file  = "${input_dir}foo/bar";
# my $string_match = "happy birthday";
# my $new_text   = "to you!";
# add_line_to_file( $file, $string_match, $new_text, 'a' );

# You can patch hunks from files in the patch_files directory.
# Hunks must be separated with "===N===", where N is an index.
# Use "a" for append or "p" for prepend.

# my $file  = "${input_dir}foo/bar";
# my $patch_file = "${patch_dir}bar";
# my $string_match = "happy birthday";
# patch_from_file_array( $file, $patch_file, $string_match, 1, "p" );

# PHYS/MAKEFILE - add coupler
my $coupler_file  = "${input_dir}phys/Makefile";
my $coupler_match = "module_sf_noahmp_glacier.o \\";
my $coupler_new   = "\tmodule_sf_COSIPY.o ";
add_line_to_file( $coupler_file, $coupler_match, $coupler_new, 'a' );

# PATCH MODULE SURFACE DRIVER
my $driver_file = "${input_dir}phys/module_surface_driver.F";
my $driver_match =
  "!  This driver calls subroutines for the surface parameterizations.";

# my $driver_match = "parameterizations.";
$patch_path = "${patch_dir}module_surface_driver.F";
$label      = set_patch_label( "!", 1 );

my $driver_new = "${label}   USE cosipy_wrf, ONLY: glacier_mass_balance\n";
add_line_to_file( $driver_file, $driver_match, $driver_new, "p" );

$driver_match = "! variables below are optional";
$whitespace   = " " x 15;
$driver_new   = "${label}${whitespace}run_cspy, &";
add_line_to_file( $driver_file, $driver_match, $driver_new, "p" );

$driver_match =
"     &          ,ua_phys,flx4,fvb,fbur,fgsn                                  &
";
patch_from_file_array( $driver_file, $patch_path, $driver_match, 1, "a" );
$driver_match = "! Variables for multi-layer UCM";
patch_from_file_array( $driver_file, $patch_path, $driver_match, 2, "p" );
$driver_match = "!jref: sfc diagnostics\n";
patch_from_file_array( $driver_file, $patch_path, $driver_match, 3, "p" );

# PATCH NOAHMP DRIVER
$driver_file  = "${input_dir}phys/noahmp/drivers/wrf/module_sf_noahmpdrv.F";
$driver_match = "!Optional Detailed Precipitation Partitioning Inputs";
$label        = set_patch_label( "!", 1 );
$driver_new =
"${label}    INTEGER, INTENT(IN) :: run_cspy\t\t!COSIPY call: 0=not called; 1=offline; 2=interactive/bypass NOAHMP_GLACIER\n";
add_line_to_file( $driver_file, $driver_match, $driver_new, "p" );

$driver_match = "its,ite,  jts,jte,  kts,kte,";
$whitespace   = " " x 15;
$driver_new   = "${whitespace}${label}${whitespace}run_cspy, &";
add_line_to_file( $driver_file, $driver_match, $driver_new, "a" );

$driver_match =
"CALL NOAHMP_OPTIONS_GLACIER(IOPT_ALB  ,IOPT_SNF  ,IOPT_TBOT, IOPT_STC, IOPT_GLA )";
$whitespace = " " x 5;
$driver_new = "${whitespace}${label}${whitespace}IF(run_cspy .LT. 2)THEN";
add_line_to_file( $driver_file, $driver_match, $driver_new, "a" );

my $patch_file = "${patch_dir}module_sf_noahmpdrv.F";
$driver_match = "FSNO   = 1.0";
patch_from_file_array( $driver_file, $patch_file, $driver_match, 1, "p" );

$driver_match = "         QFX(I,J) = ESOIL";
$driver_new   = "IF(run_cspy .LT. 2) Z0WRF  = 0.002	${label}";
add_line_to_file( $driver_file, $driver_match, $driver_new, "p" );

# PATCH RUN
my $namelist_file  = "${input_dir}run/namelist.input";
my $namelist_match = " &dynamics";
my $namelist_new =
  " &cosipy\n max_cspy_layers = 200,\n run_cspy = 0, 1,\n \\\n";
add_line_to_file( $namelist_file, $namelist_match, $namelist_new, "p" );

# PATCH REGISTRY
my $dimspec_file  = "${input_dir}Registry/registry.dimspec";
my $dimspec_match = "ifdef DA_CORE=0";
$label = set_patch_label();
my $dimspec_new =
  "${label}dimspec cspylay 2 namelist=max_cspy_layers z cspy_layers\n";
add_line_to_file( $dimspec_file, $dimspec_match, $dimspec_new, "p" );

my $em_common_file  = "${input_dir}Registry/Registry.EM_COMMON";
my $em_common_patch = "${patch_dir}Registry.EM_COMMON";
my $em_common_match = "# DFI variables";
$label = set_patch_label();
patch_from_file_array( $em_common_file, $em_common_patch, $em_common_match, 1,
    "p" );
$em_common_match =
"rconfig   integer     sf_surface_physics  namelist,physics	max_domains    -1      rh       \"sf_surface_physics\"            \"\"      \"\"";
patch_from_file_array( $em_common_file, $em_common_patch, $em_common_match, 1,
    "p" );

# PATCH DYN_EM - module_first_rk_step_part1
$patch_path = "${patch_dir}module_first_rk_step_part1.F";
my $first_rk_part1_file = "${input_dir}dyn_em/module_first_rk_step_part1.F";
my $first_rk_part1_match =
  "    USE module_configure, ONLY : grid_config_rec_type, model_config_rec";
$label      = set_patch_label("!");
$whitespace = " " x 4;
my $first_rk_part1_new =
  "\n${label}${whitespace}USE module_domain_type, ONLY : HISTORY_ALARM\n";
add_line_to_file( $first_rk_part1_file, $first_rk_part1_match,
    $first_rk_part1_new, "p" );
$first_rk_part1_match =
  "    INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde,     &";
patch_from_file_array( $first_rk_part1_file, $patch_path,
    $first_rk_part1_match, 1, "p" );
$first_rk_part1_match = "      CALL surface_driver";
patch_from_file_array( $first_rk_part1_file, $patch_path,
    $first_rk_part1_match, 2, "p" );
$first_rk_part1_match = "     &        ,fbur=grid%fbur,fgsn=grid%fgsn";
patch_from_file_array( $first_rk_part1_file, $patch_path,
    $first_rk_part1_match, 3, "a" );

# COPY COSIPY MODULE
$patch_path = "${patch_dir}module_sf_COSIPY.F";
my $target_path = "${input_dir}phys/module_sf_COSIPY.F";
copy( "${patch_path}", "${target_path}" ) or die "Copy failed: $!";

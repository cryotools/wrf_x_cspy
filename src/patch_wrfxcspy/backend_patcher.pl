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
my $patch_file;
my $check_file;
my $string_match;
my $string_new;

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
        $label = "${label} ${year}\n";
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

=head2 I<File Handling>

=over

=item get_file_is_safe($input_file, $patch_label)

Check if a file exists and has been patched.

Parameters:

=over

=item path to file

=item patch label. If this is found anywhere in the file, it is considered patched.

=back

=item get_file_exists($input_file)

Check a file exists.

=item get_file_is_patched($input_file, $patch_label)

Check if a file has been patched.

Parameters:

=over

=item path to file

=item patch label. If this is found anywhere in the file, it is considered patched.

=back

=back

=cut

sub get_file_is_safe {
    my ( $input_file, $patch_label ) = @_;
    $patch_label ||= "\\!WRF_X_CSPY";
    my $file_exists = get_file_exists($input_file);
    if ($file_exists) {
        my $file_patched = get_file_is_patched( $input_file, $patch_label );
        if ($file_patched) {
            return 1;
        }
    }
    return;
}

sub get_file_exists {
    my $file_path = $_[0];
    if ( -e $file_path ) {
        return 1;
    }
    else {
        print "File does not exist at: $file_path\nSkipping...\n";
        return;
    }
}

sub get_file_is_patched {
    my ( $input_file, $label ) = @_;
    $label ||= "\\!WRF_X_CSPY";
    open my $in, '<', "$input_file"
      or die "Can't read file at: $input_file\n$!";
    if ( grep { /$label/ } <$in> ) {
        print "File already patched: $input_file\n";
    }
    close $in;
}

sub copy_file {
    my ( $input_file, $output_file ) = @_;
    $output_file ||= "${input_file}.bak";
    copy( "${input_file}", "${output_file}" ) or die "Copy failed: $!";
}

sub open_file {
    my $input_file = $_[0];
    rename( $input_file, $input_file . '.tmp' );
    open my $in, '+<', "$input_file.tmp"
      or die "Can't read file at: $input_file\n$!";
    open my $out, '+>', "$input_file"
      or die "Can't read file at: $input_file\n$!";

    return ( $in, $out );
}

=head2 I<Regex Functions>

=over

=item regex_prepend_to_line($match_string, $new_text)

Prepends new text before matching string.

=item regex_append_to_line($match_string, $new_text)

Appends new text after matching string.

=item regex_replace_line($match_string, $new_text)

Replaces matching string with new text.

=back

=cut

sub get_clean_string {
    my ($input_string) = @_;
    $input_string =~ s{\\\(}{\(}g;
    $input_string =~ s{\\\)}{\)}g;

    return ($input_string);
}

sub regex_prepend_to_line {
    my ( $match_string, $new_text ) = @_;
    my $clean_string = get_clean_string($match_string);
    s/$match_string(?!.*$match_string)/$new_text\n$&/s;
}

sub regex_append_to_line {
    my ( $match_string, $new_text ) = @_;
    my $clean_string = get_clean_string($match_string);
    s/($match_string.*)/$clean_string\n$new_text/g;
}

sub regex_replace_line {
    my ( $match_string, $new_text ) = @_;
    my $clean_string = get_clean_string($match_string);
    s/($match_string.*)/$new_text/g;

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

=item new text

=item edit mode: 'p' -> prepend, 'a' -> append, 'r' -> replace line.

=back

=item patch_from_file_array($file_path, $patch_path, $match_string, $mode)

Patches text from another file.
Hunks in the patch file must be separated with "===N===", where N is an index.

Parameters:

=over

=item path to file

=item path to patch file (automatically generated)

=item string to match

=item edit mode: 'p' -> prepend, 'a' -> append, 'r' -> replace line.

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
        elsif ( $mode eq "a" ) {
            regex_append_to_line( $match_string, $new_text );
        }
        elsif ( $mode eq "r" ) {
            regex_replace_line( $match_string, $new_text );
        }
        else {
            die "Mode $! not supported.";
        }

        print $out $_;
    }
    close $in;
    close $out;
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
        elsif ( $mode eq "r" ) {
            regex_replace_line( $match_string, $new_text );
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
# Use "a" for append, "p" for prepend, or "r" to replace.

# my $file  = "${input_dir}foo/bar";
# my $string_match = "happy birthday";
# my $new_text   = "to you!";
# add_line_to_file( $file, $string_match, $new_text, 'a' );

# You can patch hunks from files in the patch_files directory.
# Hunks must be separated with "===N===", where N is an index.
# Use "a" for append, "p" for prepend, or "r" to replace.

# my $file  = "${input_dir}foo/bar";
# my $patch_file = "${patch_dir}bar";
# my $string_match = "happy birthday";
# patch_from_file_array( $file, $patch_file, $string_match, 1, "p" );

# NOTE: Do not forget to escape characters with a double backslash in
# $string_match, e.g. "\\( \\)".

# PHYS/MAKEFILE - add coupler
my $coupler_file = "${input_dir}phys/Makefile";
$check_file = get_file_is_safe($coupler_file);
if ($check_file) {
    $string_match = "module_sf_noahmp_glacier.o \\";
    $string_new   = "\tmodule_sf_COSIPY.o ";
    add_line_to_file( $coupler_file, $string_match, $string_new, 'a' );
}

# PATCH MODULE SURFACE DRIVER
my $driver_file = "${input_dir}phys/module_surface_driver.F";
$check_file = get_file_is_safe($driver_file);
if ($check_file) {
    copy_file($driver_file);
    $string_match =
      "\\!  This driver calls subroutines for the surface parameterizations\.";

    # my $driver_match = "parameterizations.";
    $patch_file = "${patch_dir}module_surface_driver.F";
    $label      = set_patch_label( "!", 1 );

    $whitespace = " " x 3;
    $string_new =
      "${label}${whitespace}USE cosipy_wrf, ONLY: glacier_mass_balance\n";
    add_line_to_file( $driver_file, $string_match, $string_new, "p" );

    $string_match = "! variables below are optional";
    $whitespace   = " " x 15;
    $string_new   = "${label}${whitespace}run_cspy, &";
    add_line_to_file( $driver_file, $string_match, $string_new, "p" );

    $string_match =
"     &          ,ua_phys,flx4,fvb,fbur,fgsn                                  &
";
    patch_from_file_array( $driver_file, $patch_file, $string_match, 1, "a" );
    $string_match = "! Variables for multi-layer UCM";
    patch_from_file_array( $driver_file, $patch_file, $string_match, 2, "p" );
    $string_match = "!jref: sfc diagnostics\n";
    patch_from_file_array( $driver_file, $patch_file, $string_match, 3, "p" );
}

# PATCH NOAHMP DRIVER
$driver_file = "${input_dir}phys/noahmp/drivers/wrf/module_sf_noahmpdrv.F";
$check_file  = get_file_is_safe($driver_file);
if ($check_file) {
    copy_file($driver_file);
    $string_match = "!Optional Detailed Precipitation Partitioning Inputs";
    $label        = set_patch_label( "!", 1 );
    $string_new =
"${label}    INTEGER, INTENT(IN) :: run_cspy\t\t!COSIPY call: 0=not called; 1=offline; 2=interactive/bypass NOAHMP_GLACIER\n";
    add_line_to_file( $driver_file, $string_match, $string_new, "p" );

    $string_match = "its,ite,  jts,jte,  kts,kte,                    &";
    $whitespace   = " " x 15;
    $string_new   = "${whitespace}${label}${whitespace}run_cspy, &";
    add_line_to_file( $driver_file, $string_match, $string_new, "a" );

    $string_match =
"CALL NOAHMP_OPTIONS_GLACIER\\(IOPT_ALB  ,IOPT_SNF  ,IOPT_TBOT, IOPT_STC, IOPT_GLA \\)";
    $whitespace = " " x 5;
    $string_new = "${whitespace}${label}${whitespace}IF(run_cspy .LT. 2)THEN";
    add_line_to_file( $driver_file, $string_match, $string_new, "a" );

    $patch_file   = "${patch_dir}module_sf_noahmpdrv.F";
    $whitespace   = " " x 9;
    $string_match = "${whitespace}FSNO   = 1.0";
    patch_from_file_array( $driver_file, $patch_file, $string_match, 1, "p" );

    $string_match = "${whitespace}FSNO   = 1.0";
    $string_new   = "${whitespace}IF(run_cspy .LT. 2) FSNO   = 1.0	${label}";
    add_line_to_file( $driver_file, $string_match, $string_new, "r" );

    $string_match = "${whitespace}Z0WRF  = 0.002";
    $string_new   = "${whitespace}IF(run_cspy .LT. 2) Z0WRF  = 0.002	${label}";
    add_line_to_file( $driver_file, $string_match, $string_new, "r" );
}
else {
    print
"Missing NoahMP submodule. Reinstall using the --install-wrf flag or download the drivers.";
}

# PATCH RUN
my $namelist_file = "${input_dir}run/namelist.input";
$check_file = get_file_is_safe( $namelist_file, " \&cosipy" );
if ($check_file) {
    copy_file($namelist_file);
    $string_match = " &dynamics";
    $string_new = " &cosipy\n max_cspy_layers = 200,\n run_cspy = 0, 1,\n \/\n";
    add_line_to_file( $namelist_file, $string_match, $string_new, "p" );
}

# PATCH REGISTRY
my $dimspec_file = "${input_dir}Registry/registry.dimspec";
$check_file = get_file_is_safe($dimspec_file);
if ($check_file) {
    copy_file($dimspec_file);
    $string_match = "ifdef DA_CORE=0";
    $label        = set_patch_label();
    $string_new =
      "${label}dimspec cspylay 2 namelist=max_cspy_layers z cspy_layers\n";
    add_line_to_file( $dimspec_file, $string_match, $string_new, "p" );
}

my $em_common_file = "${input_dir}Registry/Registry.EM_COMMON";
$check_file = get_file_is_safe($em_common_file);
if ($check_file) {
    copy_file($em_common_file);
    $patch_file   = "${patch_dir}Registry.EM_COMMON";
    $string_match = "# DFI variables";
    $label        = set_patch_label();
    patch_from_file_array( $em_common_file, $patch_file, $string_match, 1,
        "p" );
    $string_match =
"rconfig   integer     sf_surface_physics  namelist,physics	max_domains    -1      rh       \"sf_surface_physics\"            \"\"      \"\"";
    patch_from_file_array( $em_common_file, $patch_file, $string_match, 2,
        "p" );
}

# PATCH DYN_EM - module_first_rk_step_part1
my $first_rk_part1_file = "${input_dir}dyn_em/module_first_rk_step_part1.F";
$check_file = get_file_is_safe($first_rk_part1_file);
if ($check_file) {
    copy_file($first_rk_part1_file);
    $patch_file = "${patch_dir}module_first_rk_step_part1.F";
    $string_match =
      "    USE module_configure, ONLY : grid_config_rec_type, model_config_rec";
    $label      = set_patch_label("!");
    $whitespace = " " x 4;
    $string_new =
      "\n${label}${whitespace}USE module_domain_type, ONLY : HISTORY_ALARM\n";
    add_line_to_file( $first_rk_part1_file, $string_match, $string_new, "p" );
    $string_match = "    TYPE\\(WRFU_Time\\)                :: currentTime";
    patch_from_file_array( $first_rk_part1_file, $patch_file,
        $string_match, 1, "a" );
    $string_match = "      CALL surface_driver";
    patch_from_file_array( $first_rk_part1_file, $patch_file,
        $string_match, 2, "p" );
    $string_match =
"     &        ,fbur=grid%fbur,fgsn=grid%fgsn                                  &";
    patch_from_file_array( $first_rk_part1_file, $patch_file,
        $string_match, 3, "a" );
}

# COPY COSIPY MODULE
$patch_file = "${patch_dir}module_sf_COSIPY.F";
my $target_path = "${input_dir}phys/module_sf_COSIPY.F";
copy_file( $patch_file, $target_path );

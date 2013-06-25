#!/usr/bin/env perl 
#===============================================================================
#
#         FILE: pp.pl
#
#        USAGE: ./pp.pl  
#
#  DESCRIPTION: 
#
#      OPTIONS: ---
# REQUIREMENTS: ---
#         BUGS: ---
#        NOTES: ---
#       AUTHOR: Andrew Goldstone (agoldst), andrew.goldstone@gmail.com
# ORGANIZATION: Rutgers University, New Brunswick
#      VERSION: 1.0
#      CREATED: 06/24/2013 19:56:36
#     REVISION: ---
#===============================================================================

use v5.14;                                  # entails strict, unicode_strings 
use autodie;
use utf8;                                   # source code itself is in utf-8
use warnings;
use warnings FATAL => "utf8";               # Unicode encode errors are fatal
use open qw( :std :utf8 );                  # default utf8 layer




my %tally;

my $glitch_count = 0;
while(<>) {
    chomp;
    my @fields = split /,/;
    my $date = substr $fields[7], 0, 4;
    my $pagerange = $fields[8];
    $pagerange =~ s/^pp?\.\s*//;
    my @pps = split /\+/, $pagerange;
    foreach my $pp (@pps) {
        my ($start,$end) = $pp =~ /(\d+)(?:-(\d+))?\s*$/;
        unless($start) {
            $glitch_count++;
            say $_;
        }
        else {
            $tally{$date} += ($end) ? $end - $start + 1
                                    : 1;
        }
    }
}

open OUTFILE, ">", "tally.csv";

foreach my $year (sort keys %tally) {
    print OUTFILE "$year,$tally{$year}\n";
}

say "$glitch_count glitches";

#!/usr/bin/env perl

use strict;

my $line;
my $states = {};

while ($line = <STDIN>) {
    chomp $line;
    if ($line =~ /^(.*?),\s*(.*?)$/) {
        my $city = $1;
        my $state = $2;
        $states->{$state} = [] if not defined $states->{$state};
        push @{$states->{$state}}, $city;
    } else {
        die "Badly formed line";
    }
}

foreach my $state (sort keys %{$states}) {
    print "$state:\n";
    foreach my $city (@{$states->{$state}}) {
        print "  $city\n";
    }
}

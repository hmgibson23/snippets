#!/usr/bin/perl

do "credentials";
my $cmd = "mail-check imaps://imap.gmail.com $USER $PASS";

my $output = `$cmd`;
if ($output =~ /UNSEEN\s([0-9])/) {
    my $used = $1;
    print "$used";
}

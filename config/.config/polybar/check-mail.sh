#!/usr/bin/perl

do "credentials";
my $cmd = "mail-check imaps://imap.gmail.com $USER $PASS";

my $output = exec($cmd);
print $output;

if ($output =~ /UNSEEN (\d+)/) {
    my $used = $1;
    print $used;
}

#!/usr/bin/perl

my $output = system("mail-check imaps://imap.gmail.com gibsonhugo@gmail.com yzwqncqeopqgedra");

if ( $output =~ /UNSEEN (\d+)/) {
   my $used = $1;
   print $used;
}

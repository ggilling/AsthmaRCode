#!/usr/bin/perl

use strict;

while (<>) {
   my $t = $_;
   if ($t =~ /lang\":\"en\"/g) {
       $t =~ s/^.*\"text\":\"(.*?)\".*$/$1/g;
       print $t;
   }
}

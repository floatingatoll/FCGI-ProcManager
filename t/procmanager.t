# -*- perl -*-
# Copyright (c) 2000, FundsXpress Financial Network, Inc.
# This library is free software released "AS IS WITH ALL FAULTS"
# and WITHOUT ANY WARRANTIES under the terms of the GNU Lesser
# General Public License, Version 2.1, a copy of which can be
# found in the "COPYING" file of this distribution.

# $Id: procmanager.t,v 1.3 2000/12/10 01:48:58 muaddib Exp $

use strict;
use Test;

BEGIN { plan tests => 8; }

use FCGI::ProcManager;

my $m;

ok $m = FCGI::ProcManager->new();
ok $m->state() eq "idle";

ok $m->n_processes(100) == 100;
ok $m->n_processes(2) == 2;
ok $m->n_processes(0) == 0;

ok $m->pm_manage();
ok $m->want_to_die(1);

# i'm not sure how to test these
#eval { $m->pm_manage(); };
#ok $@ =~ /dying from death request/;
#undef $@;

ok $m->want_to_die(0) == 0;

#ok $m->n_processes(-3);
#eval { $m->pm_manage(); };
#ok $@ =~ /dying from number of processes exception: -3/;
#undef $@;

$m->n_processes(1);

#$m->pm_manage();
#sample_handler($m);

exit 0;

sub sample_handler {
  my ($m) = @_;

  while (1) {
    $m->state("handling");

    # Simulate a request dispatch.
    my $t = int(rand(6)+10);
    print "$$ sleeping $t..\n";
    while (my $nslept = sleep $t) {
      $t -= $nslept;
      last unless $t;
    }

    $m->want_to_die() 
      and $m->exit("Process $$ dying from SIGTERM after cleanup.\n");
    $m->state("idle");

    # Simulate blocking for a request.
    my $t1 = int(rand(5)+3);
    print "$$ waiting for $t1..\n";
    sleep $t1;
  }
}

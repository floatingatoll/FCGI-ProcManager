package FCGI::ProcManager;

# Copyright (c) 2000, FundsXpress Financial Network, Inc.
# This library is free software released under the GNU Lesser General
# Public License, Version 2.1.  Please read the important licensing and
# disclaimer information included below.

# $Id: ProcManager.pm,v 1.15 2001/01/31 07:00:55 muaddib Exp $

use strict;
use Exporter;

use vars qw($VERSION @ISA @EXPORT_OK %EXPORT_TAGS $Q);
BEGIN {
  $VERSION = '0.15';
  @ISA = qw(Exporter);
  @EXPORT_OK = qw(pm_manage pm_die pm_reap_server
		  pm_write_pid_file pm_remove_pid_file
		  pm_pre_dispatch pm_post_dispatch
  		  pm_change_process_name pm_received_signal pm_parameter 
		  pm_warn pm_notify pm_abort pm_exit);
  $EXPORT_TAGS{all} = \@EXPORT_OK;
  $FCGI::ProcManager::Default = 'FCGI::ProcManager';
}

=head1 NAME

 FCGI::ProcManager - functions for managing FastCGI applications.

=head1 SYNOPSIS

{
 # In Object-oriented style.
 use CGI::Fast;
 use FCGI::ProcManager;
 my $proc_manager = FCGI::ProcManager->new({ n_processes => 10 });
 $proc_manager->pm_manage();
 while (my $cgi = CGI::Fast->new()) {
   $proc_manager->pm_pre_dispatch();
   # ... handle the request here ...
   $proc_manager->pm_post_dispatch();
 }

 # This style is also supported:
 use CGI::Fast;
 use FCGI::ProcManager qw(pm_manage pm_pre_dispatch pm_post_dispatch);
 pm_manage( n_processes => 10 );
 while (my $cgi = CGI::Fast->new()) {
   pm_pre_dispatch();
   #...
   pm_post_dispatch();
 }

=head1 DESCRIPTION

FCGI::ProcManager is used to serve as a FastCGI process manager.  By
re-implementing it in perl, developers can more finely tune performance in
their web applications, and can take advantage of copy-on-write semantics
prevalent in UNIX kernel process management.  The process manager should
be invoked before the caller''s request loop

The primary routine, C<pm_manage>, enters a loop in which it maintains a
number of FastCGI servers (via fork(2)), and which reaps those servers
when they die (via wait(2)).

C<pm_manage> provides too hooks:

 C<managing_init> - called just before the manager enters the manager loop.
 C<handling_init> - called just before a server is returns from C<pm_manage>

It is necessary for the caller, when implementing its request loop, to
insert a call to C<pm_pre_dispatch> at the top of the loop, and then
7C<pm_post_dispatch> at the end of the loop.


=head1 METHODS

=head2 new

=cut

sub new {
  my ($proto,$init) = @_;

  my $this = { 
	      role => "manager",
	      start_delay => 0,
	      die_timeout => 60
	     };
  $init and %$this = %$init;

  bless $this, ref($proto)||$proto;

  $this->{PIDS} = {};

  return $this;
}

=head1 Manager methods

=head2 pm_manage

 global
 () pm_manage(int processes_to_spawn)

DESCRIPTION:

When this is called by a FastCGI script to manage application servers.

=cut

sub pm_manage {
  my ($this,%values) = self_or_default(@_);
  map { $this->pm_parameter($_,$values{$_}) } keys %values;

  # skip to handling now if we won't be managing any processes.
  $this->n_processes() or goto HANDLING;

  # call the (possibly overloaded) management initialization hook.
  $this->role("manager");
  $this->managing_init();
  $this->pm_notify("initialized");

  my $manager_pid = $$;

 MANAGING_LOOP: while (1) {

    # if the calling process goes away, perform cleanup.
    getppid() == 1 and
      return $this->pm_die("calling process has died");

    $this->n_processes() > 0 or
      return $this->pm_die();

    # while we have fewer servers than we want.
  PIDS: while (keys(%{$this->{PIDS}}) < $this->n_processes()) {

      if (my $pid = fork()) {
	# the manager remembers the server.
	$this->{PIDS}->{$pid} = { pid=>$pid };
        $this->pm_notify("server (pid $pid) started");

      } elsif (! defined $pid) {
	return $this->pm_abort("fork: $!");

      } else {
	$this->role("server");
	$this->{MANAGER_PID} = $manager_pid;
	# the server exits the managing loop.
	last MANAGING_LOOP;
      }

      for (my $s = $this->start_delay(); $s; $s = sleep $s) {};
    }

    # this should block until the next server dies.
    $this->pm_reap_server();

  }# while 1

HANDLING:

  # call the (possibly overloaded) handling init hook
  $this->role("server");
  $this->handling_init();
  $this->pm_notify("initialized");

  # server returns 
  return 1;
}

=head2 managing_init

=cut

sub managing_init {
  my ($this) = self_or_default(@_);

  # begin to handle signals.
  $SIG{TERM} = sub { $this->sig_manager(@_) };
  $SIG{HUP}  = sub { $this->sig_manager(@_) };

  # change the name of this process as it appears in ps(1) output.
  $this->pm_change_process_name("perl-fcgi-pm");

  $this->pm_write_pid_file();
}


=head2 pm_die

=cut

sub pm_die {
  my ($this,$msg,$n) = self_or_default(@_);

  # stop handling signals.
  $SIG{HUP}  = 'DEFAULT';
  $SIG{TERM} = 'DEFAULT';

  $this->pm_remove_pid_file();

  # prepare to die no matter what.
  if (defined $this->die_timeout()) {
    $SIG{ARLM} = sub { $this->pm_abort("reap timeout") };
    alarm $this->die_timeout();
  }

  # send a TERM to each of the servers.
  kill "TERM", keys %{$this->{PIDS}};

  # wait for the servers to die.
  while (%{$this->{PIDS}}) {
    $this->pm_reap_server();
  }

  # die already.
  $this->pm_exit("dying: ".$msg,$n);
}

=head2 pm_reap_server

=cut

sub pm_reap_server {
  my ($this) = self_or_default(@_);

  # wait for the next server to die.
  next if (my $pid = wait()) < 0;

  # notify when one of our servers have died.
  delete $this->{PIDS}->{$pid} and
    $this->pm_notify("server (pid $pid) exited with status $?");
}

=head2 pm_write_pid_file

=cut

sub pm_write_pid_file {
  my ($this,$fname) = self_or_default(@_);
  $fname ||= $this->pid_fname() or return;
  if (!open PIDFILE, ">$fname") {
    $this->pm_warn("open: $fname: $!");
    return;
  }
  print PIDFILE "$$\n";
  close PIDFILE;
}

=head2 pm_remove_pid_file

=cut

sub pm_remove_pid_file {
  my ($this,$fname) = self_or_default(@_);
  $fname ||= $this->pid_fname() or return;
  my $ret = unlink($fname) or $this->pm_warn("unlink: $fname: $!");
  return $ret;
}

=head2 sig_manager

=cut

sub sig_manager {
  my ($this,$name) = @_;
  if ($name eq "TERM" or $name eq "HUP") {
    $this->pm_die("received signal $name");
  } else {
    $this->pm_notify("ignoring signal $name");
  }
}

=head1 Handler methods

=head2 handling_init

=cut

sub handling_init {
  my ($this) = self_or_default(@_);

  # begin to handle signals.
  $SIG{TERM} = sub { $this->sig_handler(@_) };
  $SIG{HUP}  = sub { $this->sig_handler(@_) };

  # change the name of this process as it appears in ps(1) output.
  $this->pm_change_process_name("perl-fcgi");
}

=head2 pm_pre_dispatch

=cut

sub pm_pre_dispatch {
  my ($this) = self_or_default(@_);
}

=head2 pm_post_dispatch

=cut

sub pm_post_dispatch {
  my ($this) = self_or_default(@_);
  if ($this->pm_received_signal("TERM")) {
    $this->pm_exit("safe exit after SIGTERM");
  }
  if ($this->pm_received_signal("HUP")) {
    $this->pm_exit("safe exit after SIGHUP");
  }
  if (getppid() != $this->{MANAGER_PID}) {
    $this->pm_exit("safe exit: manager has died");
  }
}

=head2 sig_handler

=cut

sub sig_handler {
  my ($this,$name) = @_;
  $this->pm_received_signal($name,1);
}

=head1 Common methods and routines

=head2 self_or_default

 private global
 (ProcManager, @args) self_or_default([ ProcManager, ] @args);

DESCRIPTION:

This is a helper subroutine to acquire or otherwise create a singleton
default object if one is not passed in, e.g., a method call.

=cut

sub self_or_default {
  return @_ if defined $_[0] and !ref $_[0] and $_[0] eq 'FCGI::ProcManager';
  if (!defined $_[0] or (ref($_[0]) ne 'FCGI::ProcManager' and
			 !UNIVERSAL::isa($_[0],'FCGI::ProcManager'))) {
    $Q or $Q = $FCGI::ProcManager::Default->new;
    unshift @_, $Q;
  }
  return wantarray ? @_ : $Q;
}

=head2 pm_change_process_name

=cut

sub pm_change_process_name {
  my ($this,$name) = self_or_default(@_);
  $0 = $name;
}

=head2 pm_received_signal

=cut

sub pm_received_signal {
  my ($this,$sig,$received) = self_or_default(@_);
  $sig or return $this->{SIG_RECEIVED};
  $received and $this->{SIG_RECEIVED}->{$sig}++;
  return $this->{SIG_RECEIVED}->{$sig};
}

=head2 pm_parameter

=cut

sub pm_parameter {
  my ($this,$key,$value) = self_or_default(@_);
  defined $value and $this->{$key} = $value;
  return $this->{$key};
}

=head2 n_processes

=head2 no_signals

=head2 pid_fname

=head2 die_timeout

=head2 role

=head2 start_delay

=cut

sub n_processes     { shift->pm_parameter("n_processes",     @_); }
sub pid_fname       { shift->pm_parameter("pid_fname",       @_); }
sub no_signals      { shift->pm_parameter("no_signals",      @_); }
sub die_timeout     { shift->pm_parameter("die_timeout",     @_); }
sub role            { shift->pm_parameter("role",            @_); }
sub start_delay     { shift->pm_parameter("start_delay",     @_); }

=head2 pm_warn

=cut

sub pm_warn {
  my ($this,$msg) = self_or_default(@_);
  $this->pm_notify($msg);
}

=head2 pm_notify

=cut

sub pm_notify {
  my ($this,$msg) = self_or_default(@_);
  $msg =~ s/\s*$/\n/;
  print STDERR "FastCGI: ".$this->role()." (pid $$): ".$msg;
}

=head2 exit

=cut

sub pm_exit {
  my ($this,$msg,$n) = self_or_default(@_);
  $n ||= 0;
  $this->pm_warn($msg);
  $@ = $msg;
  exit $n;
}

=head2 pm_abort

=cut

sub pm_abort {
  my ($this,$msg,$n) = self_or_default(@_);
  $n ||= 1;
  $this->pm_exit($msg,1);
}

1;
__END__

=head1 BUGS

No known bugs, but this does not mean no bugs exist.

=head1 SEE ALSO

L<FCGI>.

=head1 COPYRIGHT

 FCGI-ProcManager - A Perl FCGI Process Manager
 Copyright (c) 2000, FundsXpress Financial Network, Inc.

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2 of the License, or (at your option) any later version.

 BECAUSE THIS LIBRARY IS LICENSED FREE OF CHARGE, THIS LIBRARY IS
 BEING PROVIDED "AS IS WITH ALL FAULTS," WITHOUT ANY WARRANTIES
 OF ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING, WITHOUT
 LIMITATION, ANY IMPLIED WARRANTIES OF TITLE, NONINFRINGEMENT,
 MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE, AND THE
 ENTIRE RISK AS TO SATISFACTORY QUALITY, PERFORMANCE, ACCURACY,
 AND EFFORT IS WITH THE YOU.  See the GNU Lesser General Public
 License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA

=cut

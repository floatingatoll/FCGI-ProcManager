package FCGI::ProcManager;

# Copyright (c) 2000, FundsXpress Financial Network, Inc.
# This library is free software released under the GNU Lesser General
# Public License, Version 2.1.  Please read the important licensing and
# disclaimer information included below.

# $Id: ProcManager.pm,v 1.17 2001/02/09 16:15:47 muaddie Exp $

use strict;
use Exporter;

use vars qw($VERSION @ISA @EXPORT_OK %EXPORT_TAGS $Q);
BEGIN {
  $VERSION = '0.16';
  @ISA = qw(Exporter);
  @EXPORT_OK = qw(pm_manage pm_die pm_wait
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
 my $proc_manager = FCGI::ProcManager->new({
	n_processes => 10 
 });
 $proc_manager->pm_manage();
 while (my $cgi = CGI::Fast->new()) {
   $proc_manager->pm_pre_dispatch();
   # ... handle the request here ...
   $proc_manager->pm_post_dispatch();
 }

 # This style is also supported:
 use CGI::Fast;
 use FCGI::ProcManager qw(pm_manage pm_pre_dispatch 
			  pm_post_dispatch);
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

 class or instance
 (ProcManager) new([hash parameters])

Constructs a new process manager.  Takes an option has of initial parameter
values, and assigns these to the constructed object HASH, overriding any
default values.  The default parameter values currently are:

 role         => manager
 start_delay  => 0
 die_timeout  => 60

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

 instance or export
 (int) pm_manage([hash parameters])

DESCRIPTION:

When this is called by a FastCGI script to manage application servers.  It
defines a sequence of instructions for a process to enter this method and
begin forking off and managing those handlers, and it defines a sequence of
instructions to intialize those handlers.

If n_processes < 1, the managing section is subverted, and only the
handling sequence is executed.

Either returns the return value of pm_die() and/or pm_abort() (which will
not ever return in general), or returns 1 to the calling script to begin
handling requests.

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
    $this->pm_wait();

  }# while 1

HANDLING:

  # forget any children we had been collecting.
  delete $this->{PIDS};

  # call the (possibly overloaded) handling init hook
  $this->role("server");
  $this->handling_init();
  $this->pm_notify("initialized");

  # server returns 
  return 1;
}

=head2 managing_init

 instance
 () managing_init()

DESCRIPTION:

Overrideable method which initializes a process manager.  In order to
handle signals, manage the PID file, and change the process name properly,
any method which overrides this should call SUPER::managing_init().

=cut

sub managing_init {
  my ($this) = @_;

  # begin to handle signals.
  $SIG{TERM} = sub { $this->sig_manager(@_) };
  $SIG{HUP}  = sub { $this->sig_manager(@_) };

  # change the name of this process as it appears in ps(1) output.
  $this->pm_change_process_name("perl-fcgi-pm");

  $this->pm_write_pid_file();
}


=head2 pm_die

 instance or export
 () pm_die(string msg[, int exit_status])

DESCRIPTION:

This method is called when a process manager receives a notification to
shut itself down.  pm_die() attempts to shutdown the process manager
gently, sending a SIGTERM to each managed process, waiting die_timeout()
seconds to reap each process, and then exit gracefully once all children
are reaped, or to abort if all children are not reaped.

=cut

sub pm_die {
  my ($this,$msg,$n) = self_or_default(@_);

  # stop handling signals.
  $SIG{HUP}  = 'DEFAULT';
  $SIG{TERM} = 'DEFAULT';

  $this->pm_remove_pid_file();

  # prepare to die no matter what.
  if (defined $this->die_timeout()) {
    $SIG{ARLM} = sub { $this->pm_abort("wait timeout") };
    alarm $this->die_timeout();
  }

  # send a TERM to each of the servers.
  kill "TERM", keys %{$this->{PIDS}};

  # wait for the servers to die.
  while (%{$this->{PIDS}}) {
    $this->pm_wait();
  }

  # die already.
  $this->pm_exit("dying: ".$msg,$n);
}

=head2 pm_wait

 instance or export
 (int pid) pm_wait()

DESCRIPTION:

This calls wait() which suspends execution until a child has exited.
If the process ID returned by wait corresponds to a managed process,
pm_notify() is called with the exit status of that process.
pm_wait() returns with the return value of wait().

=cut

sub pm_wait {
  my ($this) = self_or_default(@_);

  # wait for the next server to die.
  next if (my $pid = wait()) < 0;

  # notify when one of our servers have died.
  delete $this->{PIDS}->{$pid} and
    $this->pm_notify("server (pid $pid) exited with status $?");

  return $pid;
}

=head2 pm_write_pid_file

 instance or export
 () pm_write_pid_file([string filename])

DESCRIPTION:

Writes current process ID to optionally specified file.  If no filename is
specified, it uses the value of the C<pid_fname> parameter.

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

 instance or export
 () pm_remove_pid_file()

DESCRIPTION:

Removes optionally specified file.  If no filename is specified, it uses
the value of the C<pid_fname> parameter.

=cut

sub pm_remove_pid_file {
  my ($this,$fname) = self_or_default(@_);
  $fname ||= $this->pid_fname() or return;
  my $ret = unlink($fname) or $this->pm_warn("unlink: $fname: $!");
  return $ret;
}

=head2 sig_manager

 instance
 () sig_manager(string name)

DESCRIPTION:

Handles signals of the process manager.  Takes as input the name of signal
being handled.

=cut

sub sig_manager {
  my ($this,$name) = @_;
  if ($name eq "TERM" or $name eq "HUP") {
    $this->pm_notify("received signal $name");
    $this->pm_die("safe exit from signal $name");
  } else {
    $this->pm_notify("ignoring signal $name");
  }
}

=head1 Handler methods

=head2 handling_init

 instance or export
 () handling_init()

DESCRIPTION:

=cut

sub handling_init {
  my ($this) = @_;

  # begin to handle signals.
  $SIG{TERM} = sub { $this->sig_handler(@_) };
  $SIG{HUP}  = sub { $this->sig_handler(@_) };

  # change the name of this process as it appears in ps(1) output.
  $this->pm_change_process_name("perl-fcgi");
}

=head2 pm_pre_dispatch

 instance or export
 () pm_pre_dispatch()

DESCRIPTION:

=cut

sub pm_pre_dispatch {
  my ($this) = self_or_default(@_);
}

=head2 pm_post_dispatch

 instance or export
 () pm_post_dispatch()

DESCRIPTION:

=cut

sub pm_post_dispatch {
  my ($this) = self_or_default(@_);
  if ($this->pm_received_signal("TERM")) {
    $this->pm_exit("safe exit after SIGTERM");
  }
  if ($this->pm_received_signal("HUP")) {
    $this->pm_exit("safe exit after SIGHUP");
  }
  if ($this->{MANAGER_PID} and getppid() != $this->{MANAGER_PID}) {
    $this->pm_exit("safe exit: manager has died");
  }
}

=head2 sig_handler

 instance or export
 () sig_handler()

DESCRIPTION:

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

 instance or export
 () pm_change_process_name()

DESCRIPTION:

=cut

sub pm_change_process_name {
  my ($this,$name) = self_or_default(@_);
  $0 = $name;
}

=head2 pm_received_signal

 instance or export
 () pm_received signal()

DESCRIPTION:

=cut

sub pm_received_signal {
  my ($this,$sig,$received) = self_or_default(@_);
  $sig or return $this->{SIG_RECEIVED};
  $received and $this->{SIG_RECEIVED}->{$sig}++;
  return $this->{SIG_RECEIVED}->{$sig};
}

=head1 parameters

=head2 pm_parameter

 instance or export
 () pm_parameter()

DESCRIPTION:

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

DESCRIPTION:

=cut

sub n_processes     { shift->pm_parameter("n_processes",     @_); }
sub pid_fname       { shift->pm_parameter("pid_fname",       @_); }
sub no_signals      { shift->pm_parameter("no_signals",      @_); }
sub die_timeout     { shift->pm_parameter("die_timeout",     @_); }
sub role            { shift->pm_parameter("role",            @_); }
sub start_delay     { shift->pm_parameter("start_delay",     @_); }

=head1 notification and death

=head2 pm_warn

 instance or export
 () pm_warn()

DESCRIPTION:

=cut

sub pm_warn {
  my ($this,$msg) = self_or_default(@_);
  $this->pm_notify($msg);
}

=head2 pm_notify

 instance or export
 () pm_notify()

DESCRIPTION:

=cut

sub pm_notify {
  my ($this,$msg) = self_or_default(@_);
  $msg =~ s/\s*$/\n/;
  print STDERR "FastCGI: ".$this->role()." (pid $$): ".$msg;
}

=head2 pm_exit

 instance or export
 () pm_exit(string msg[, int exit_status])

DESCRIPTION:

=cut

sub pm_exit {
  my ($this,$msg,$n) = self_or_default(@_);
  $n ||= 0;

  # if we still have children at this point, something went wrong.
  # SIGKILL them now.
  kill "KILL", keys %{$this->{PIDS}} if $this->{PIDS};

  $this->pm_warn($msg);
  $@ = $msg;
  exit $n;
}

=head2 pm_abort

 instance or export
 () pm_abort(string msg[, int exit_status])

DESCRIPTION:

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

package FCGI::ProcManager;

# Copyright (c) 2000, FundsXpress Financial Network, Inc.
# This library is free software released under the GNU Lesser General
# Public License, Version 2.1.  Please read the important licensing and
# disclaimer information included below.

# $Id: ProcManager.pm,v 1.12 2001/01/13 06:44:34 muaddib Exp $

use strict;
use Exporter;

use vars qw($VERSION @ISA @EXPORT_OK %EXPORT_TAGS $Q @valid_states);
BEGIN {
  $VERSION = '0.14';
  @ISA = qw(Exporter);
  @EXPORT_OK = qw(pm_manage pm_parameter pm_state pm_warn pm_abort pm_exit
		  pm_write_pid_file pm_remove_pid_file
		  pm_pre_dispatch pm_post_dispatch
		  pm_register_sig_handler pm_unregister_sig_handler);
  $EXPORT_TAGS{all} = \@EXPORT_OK;
  $FCGI::ProcManager::Default = 'FCGI::ProcManager';

  @valid_states = qw(managing handling idle);
}

=head1 NAME

 FCGI::ProcManager - functions for managing FastCGI applications.

=head1 SYNOPSIS

 # In its simplest form.
 use CGI::Fast;
 use FCGI::ProcManager;
 my $proc_manager = FCGI::ProcManager->new({n_processes=>10});
 $proc_manager->manage();
 while (my $cgi = CGI::Fast->new()) {
   #...

=head1 DESCRIPTION

FCGI::ProcManager is used to serve as a FastCGI process manager.
The parent uses fork(2) and wait(2) to manage a set of FastCGI application
servers.  more later.

=head1 METHODS

=head2 new

=cut

sub new {
  my ($proto,$init) = @_;

  my $this = {};
  $init and %$this = %$init;

  bless $this, ref($proto)||$proto;

  $this->{PIDS} = {};

  return $this;
}

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

=head2 pm_manage

 global
 () pm_manage(int processes_to_spawn)

DESCRIPTION:

When this is called by a FastCGI script to manage application servers.

=cut

sub pm_manage {
  my ($this) = self_or_default(@_);

  # initialize state and begin to handle signals.
  $this->pm_register_sig_handler();

  # switch to handling state right away if we are not managing any processes.
  $this->n_processes() or goto HANDLING;

  # begin the managing sequence.
  $this->pm_state("managing");

  # call the (possibly overloaded) managing initialization.
  $this->managing_init();

  # write out the pid file.
  $this->pm_write_pid_file();

  my ($pid);
  MANAGE: while (1) {

    # do things that we only do when we're not already managing processes..
    if (! %{$this->{PIDS}}) {
      if ($this->received_signal()) {
	$this->pm_remove_pid_file();
	$this->pm_exit("Manager $$ dying from death request.\n");
      } elsif ($this->n_processes() < 0) {
	$this->pm_remove_pid_file();
	$this->pm_abort("Manager $$ dying from processes number exception: ".
			$this->n_processes(), -( 1 + $this->n_processes()));
      }
    }

    # if we have fewer children than we want..
    PIDS: while (keys(%{$this->{PIDS}}) < $this->n_processes()) {

      # fork.
      if ($pid = fork()) {
	# the parent notes the child.
	$this->pm_warn("started process $pid\n");
	$this->{PIDS}->{$pid} = { pid=>$pid };

      } elsif (! defined $pid) {
	# handle errors um  gracefully.
	$this->pm_abort("fork: $!\n");

      } else {
	# the child returns to the calling application.
	last MANAGE;
      }
    }

    # wait on the next child to die.
    $this->pm_abort("wait: $!\n") if ($pid = wait()) < 0;

    # notify when one of our children have died.
    delete $this->{PIDS}->{$pid} and
      $this->pm_warn("Child process $pid died with exit status $?\n");

  }# while 1

  HANDLING:
  $this->pm_state("handling");

  # call the (possibly overloaded) handling initialization.
  $this->handling_init();

  # children and parent with n_processes == 0 return to calling app.
  return 1;
}

=head2 managing_init

=cut

sub managing_init {
  my ($this) = self_or_default(@_);
}

=head2 handling_init

=cut

sub handling_init {
  my ($this) = self_or_default(@_);
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
  if (my $name = $this->received_signal()) {
    if ($name eq "HUP" or $name eq "TERM") {
      $this->pm_exit("Process $$ responding to $name death request.\n");
    }
  }
}

=head2 pm_write_pid_file

=cut

sub pm_write_pid_file {
  my ($this,$fname) = self_or_default(@_);
  $fname ||= $this->pid_fname() or return;
  if (!open PIDFILE, ">$fname") {
    $this->pm_warn("open: $fname: $!\n");
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
  my $ret = unlink($fname) or $this->pm_warn("unlink: $fname: $!\n");
  return $ret;
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

=cut

sub n_processes     { shift->pm_parameter("n_processes",     @_); }
sub pid_fname       { shift->pm_parameter("pid_fname",       @_); }
sub received_signal { shift->pm_parameter("received_signal", @_); }
sub no_signals      { shift->pm_parameter("no_signals",      @_); }

=head2 pm_state

=cut

sub pm_state {
  my ($this,$new_state) = self_or_default(@_);
  if (defined $new_state) {
    if (!grep {$new_state eq $_} @valid_states) {
      $this->pm_abort("Invalid state: $new_state\n");
    }
    $this->{state} = $new_state;
  }
  defined $this->{state} or $this->{state} = "idle";
  return $this->{state};
}

=head2 pm_register_sig_handler

=cut

sub pm_register_sig_handler {
  my ($this) = self_or_default(@_);
  return if $this->no_signals();
  $SIG{TERM} = sub { $this->sig_method(@_) };
  $SIG{HUP}  = sub { $this->sig_method(@_) };
}

=head2 pm_unregister_sig_handler

=cut

sub pm_unregister_sig_handler {
  my ($this) = self_or_default(@_);
  return if $this->no_signals();
  undef $SIG{TERM};
  undef $SIG{HUP};
}

=head2 sig_method

=cut

sub sig_method {
  my ($this,$name) = @_;
  # note which signal we've received.
  $this->{received_signal} = $name;
  $this->n_processes(0);
  # propagate this signal to children.  (is this necessary?)
  if (%{$this->{PIDS}}) {
    kill $name, keys %{$this->{PIDS}};
  }
}

=head2 pm_warn

=cut

sub pm_warn {
  my ($this,$msg) = self_or_default(@_);
  print STDERR $msg;
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

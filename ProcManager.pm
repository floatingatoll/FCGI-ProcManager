package FCGI::ProcManager;

# Copyright (c) 2000, FundsXpress Financial Network, Inc.
# This library is free software released under the GNU Lesser General
# Public License, Version 2.1.  Please read the important licensing and
# disclaimer information included below.

# $Id: ProcManager.pm,v 1.5 2000/11/18 07:06:09 muaddib Exp $

use strict;
use vars qw(@valid_states);
BEGIN {
  $FCGI::ProcManager::VERSION = '0.10';
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
  bless $this, ref($proto)||$proto;

  $init and %$this = %$init;
  defined $this->n_processes() or
	$this->n_processes($ENV{PROCMANAGER_PROCESSES});

  $this->{PIDS} = {};

  return $this;
}

=head2 manage

 global
 () manage(int processes_to_spawn)

DESCRIPTION:

When this is called by a FastCGI script to manage application servers.

=cut

sub manage {
  my ($this) = @_;

  # initialize state and begin to handle signals.
  $this->register_sig_handler();

  # return right away if we are not managing any processes.
  $this->n_processes() or return 1;

  # call the (possibly overloaded) pre-manage initialization.
  $this->state("managing");
  $this->pre_manage_init();

  # write out the pid file.
  $this->write_pid_file();

  my ($pid);
  MANAGE: while (1) {

    # do things that we only do when we're not already managing processes..
    if (! %{$this->{PIDS}}) {
      if (!$this->n_processes()) {
	$this->remove_pid_file();
	last;
      } elsif ($this->want_to_die()) {
	$this->remove_pid_file();
	$this->exit("Manager $$ dying from death request.\n");
      } elsif ($this->n_processes() < 0) {
	$this->remove_pid_file();
	$this->abort("Manager $$ dying from number of processes exception: ".
		     $this->n_processes(), -( 1 + $this->n_processes()));
      }
    }

    # if we have fewer children than we want..
    PIDS: while (keys(%{$this->{PIDS}}) < $this->n_processes()) {

      # fork.
      if ($pid = fork()) {
	# the parent notes the child.
	$this->warn("started process $pid\n");
	$this->{PIDS}->{$pid} = { pid=>$pid };

      } elsif (! defined $pid) {
	# handle errors um  gracefully.
	$this->abort("fork: $!\n");

      } else {
	# the child returns to the calling application.
	print "$$ lasting..\n";
	last MANAGE;
      }
    }

    # wait on the next child to die.
    $this->abort("wait: $!\n") if ($pid = wait()) < 0;
    $this->warn("Child process $pid died with exit status $?\n");
    delete $this->{PIDS}->{$pid}
	or $this->abort("Internal error: ".
			"wait() returned non-existent pid=$pid??\n");

  }# while 1

  # call the (possibly overloaded) post-manage initialization.
  $this->post_manage_init();

  $this->state("idle");

  print "$$ returning..\n";
  # children and parent with n_processes == 0 return to calling app.
  return 1;
}

=head2 pre_manage_init

=cut

sub pre_manage_init {
  my ($this) = @_;
}

=head2 post_manage_init

=cut

sub post_manage_init {
  my ($this) = @_;
}

=head2 pre_dispatch

=cut

sub pre_dispatch {
  my ($this) = @_;
  $this->state("handling");
}

=head2 post_dispatch

=cut

sub post_dispatch {
  my ($this) = @_;
  $this->want_to_die() and 
    $this->exit("Process $$ responding to death request.");
  $this->state("idle");
}

=head2 write_pid_file

=cut

sub write_pid_file {
  my ($this,$fname) = @_;
  $fname ||= $this->pid_fname() or return;
  if (!open PIDFILE, ">$fname") {
    $this->warn("open: $fname: $!\n");
    return;
  }
  print PIDFILE "$$\n";
  close PIDFILE;
}

=head2 remove_pid_file

=cut

sub remove_pid_file {
  my ($this,$fname) = @_;
  $fname ||= $this->pid_fname() or return;
  my $ret = unlink($fname) or $this->warn("unlink: $fname: $!\n");
  return $ret;
}

=head2 gen_mutator

=cut

sub gen_mutator {
  my ($this,$key,$value) = @_;
  defined $value and $this->{$key} = $value;
  return $this->{$key};
}

=head2 n_processes

=head2 want_to_die

=head2 no_signals

=head2 pid_fname

=cut

sub n_processes { shift->gen_mutator("n_processes",@_); }
sub want_to_die { shift->gen_mutator("want_to_die",@_); }
sub no_signals  { shift->gen_mutator("no_signals",@_);  }
sub pid_fname   { shift->gen_mutator("pid_fname",@_);   }

=head2 state

=cut

sub state {
  my ($this,$new_state) = @_;
  if (defined $new_state) {
    if (!grep {$new_state eq $_} @valid_states) {
      $this->abort("Invalid state: $new_state\n");
    }
    $this->{state} = $new_state;
  }
  defined $this->{state} or $this->{state} = "idle";
  return $this->{state};
}

=head2 register_sig_handler

=cut

sub register_sig_handler {
  my ($this) = @_;
  return if $this->no_signals();
  $SIG{TERM} = sub { $this->sig_method(@_) };
  $SIG{HUP} = sub { $this->sig_method(@_) };
}

=head2 unregister_sig_handler

=cut

sub unregister_sig_handler {
  my ($this) = @_;
  return if $this->no_signals();
  undef $SIG{TERM};
  undef $SIG{HUP};
}

=head2 sig_method

=cut

sub sig_method {
  my ($this,$name) = @_;
  if ($name eq "TERM") {
    if ($this->state() eq "idle") {
      $this->exit("Process $$ dying after receiving SIG$name.\n");
    } else {
      $this->warn("Process $$ received SIG$name.  Cleaning up.\n");
      $this->want_to_die(1);
      $this->n_processes(-1);
      # is the following necessary?
      kill $name, keys %{$this->{PIDS}};
    }
  } else {
    $this->warn("I don't know what to do with $name yet.. ignoring?\n");
  }
}

=head2 warn

=cut

sub warn {
  my ($this,$msg) = @_;
  print STDERR $msg;
}

=head2 exit

=cut

sub exit {
  my ($this,$msg,$n) = @_;
  $n ||= 0;
  $this->warn($msg);
  $@ = $msg;
  exit $n;
}

=head2 abort

=cut

sub abort {
  my ($this,$msg,$n) = @_;
  $n ||= 1;
  $this->exit($msg,1);
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

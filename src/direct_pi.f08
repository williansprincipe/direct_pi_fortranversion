! direct_pi_fortranversion.f08
! Calculates an estimate of pi by direct sampling -- SMAC from Coursera
! wpfernandes 2016-07-21 13h22min

! -----------------------------------------------------------------------------
subroutine init_random_seed()
  use iso_fortran_env, only: int64
  implicit none
  integer, allocatable :: seed(:)
  integer :: i, n, un, istat, dt(8), pid
  integer(int64) :: t

  call random_seed(size = n)
  allocate(seed(n))
  ! First try if the OS provides a random number generator
  open(newunit=un, file="/dev/urandom", access="stream", &
       form="unformatted", action="read", status="old", iostat=istat)
  if (istat == 0) then
     read(un) seed
     close(un)
  else
     ! Fallback to XOR:ing the current time and pid. The PID is
     ! useful in case one launches multiple instances of the same
     ! program in parallel.
     call system_clock(t)
     if (t == 0) then
        call date_and_time(values=dt)
        t = (dt(1) - 1970) * 365_int64 * 24 * 60 * 60 * 1000 &
             + dt(2) * 31_int64 * 24 * 60 * 60 * 1000 &
             + dt(3) * 24_int64 * 60 * 60 * 1000 &
             + dt(5) * 60 * 60 * 1000 &
             + dt(6) * 60 * 1000 + dt(7) * 1000 &
             + dt(8)
     end if
     pid = getpid()
     t = ieor(t, int(pid, kind(t)))
     do i = 1, n
        seed(i) = lcg(t)
     end do
  end if
  call random_seed(put=seed)
contains
  ! This simple PRNG might not be good enough for real work, but is
  ! sufficient for seeding a better PRNG.
  function lcg(s)
    integer :: lcg
    integer(int64) :: s
    if (s == 0) then
       s = 104729
    else
       s = mod(s, 4294967296_int64)
    end if
    s = mod(s * 279470273_int64, 4294967291_int64)
    lcg = int(mod(s, int(huge(0), int64)), kind(0))
  end function lcg
end subroutine init_random_seed

! -----------------------------------------------------------------------------
function function_direct_sample(n_trials) result(a_pi_estimate)
  integer, intent(in) :: n_trials
  real                :: a_pi_estimate
  real                :: r ! random number
  integer             :: n_hits
  do i_trials = 1, n_trials
    call random_number(r)
    write (*, *) 'i_trials = ', i_trials, ', random number r = ', r
  end do
  a_pi_estimate = 3.77
end function function_direct_sample

! =============================================================================
program direct_pi
! =============================================================================
implicit none

real            :: function_direct_sample
integer         :: n_trials
real, parameter :: pi = 4.0 * atan(1.0)
real            :: pi_estimate

write (*, *) 'direct_pi (written in fortran) -- version 0.01 (2016-07-21 15h36min)'
write (*, *) 'Estimative for pi by Direct Sampling Method.'
write (*, *) ''

call init_random_seed()

write (*, *) 'Enter number of trials (n_trials): '
read (*, *) n_trials

pi_estimate = 2.99
pi_estimate = function_direct_sample(n_trials)
write (*, *) pi_estimate

end program direct_pi

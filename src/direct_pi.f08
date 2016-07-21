! direct_pi_fortranversion.f08
! Calculates an estimate of pi by direct sampling -- SMAC from Coursera
! wpfernandes 2016-07-21 13h22min

! -----------------------------------------------------------------------------
function pi_by_direct_sample(n_trials) result(a_pi_estimate)
integer, intent(in) :: n_trials
real                :: a_pi_estimate
! integer :: i, n_trials, n_hits

a_pi_estimate = 3.77
end function pi_by_direct_sample

! =============================================================================
program direct_pi
! =============================================================================
implicit none

real            :: pi_by_direct_sample
integer         :: n_trials
real, parameter :: pi = 4.0 * atan(1.0)
real            :: pi_estimate

write (*, *) 'direct_pi (written in fortran) -- version 0.01 (2016-07-21 15h36min)'
write (*, *) 'Estimative for pi by Direct Sampling Method.'
write (*, *) ''
write (*, *) 'Enter number of trials (n_trials): '
read (*, *) n_trials

pi_estimate = 2.99
pi_estimate = pi_by_direct_sample(n_trials)
write (*, *) pi_estimate

end program direct_pi

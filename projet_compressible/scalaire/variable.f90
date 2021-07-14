module variable
	implicit none
	integer :: i, flag, lg = 1, nx = 1000 	
	integer, parameter :: ifile = 30
	integer, parameter :: rp = 8 
	real(rp) :: dx, cfl,cfl1, x_debut, x_fin
	real(rp)	:: dt, tf, date, ul, ulr, ur
	real(rp), allocatable, dimension(:) :: x, u, u_n, F, sigma
end module variable

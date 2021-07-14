program burger
use variable
use mod_func
use mod_flux
	implicit none
	allocate(x(1:nx), u(1:nx), u_n(1:nx), F(1:nx-1), sigma(1:nx)) 
	 ul = -0.2_rp
	 ulr = 0.2_rp
	 ur = 0.7_rp
	 x_debut = 0._rp
	 x_fin = 1._rp	
	 dx = (x_fin-x_debut)/nx
	 cfl = 0.5_rp
	 tf = 0.5_rp
	 
	x = 0.0_rp
	do i = 1, nx
		x(i) = i*dx
		u(i) = u_init(x(i),1)
	end do
	
	open(ifile, file = "init.dat", status = "unknown")
	write(ifile, '(ES22.6, ES22.6)') (x(i), u(i), i = 1, nx)
	close(ifile)
 	
!--------------------------------------------------------------!
! Lax-Friedrichs & Lax-wendorff
!--------------------------------------------------------------!
	date = 0._rp
	u_n = u 
	cfl1 = 0.25_rp
	do
		dt = dx*cfl1/maxval(abs(3._rp*u**2-1),dim = 1)
		call flux(nx, dx, dt, F, u, 1)
		do i = 2, nx - 1
			u_n(i) = u_n(i) -dt/dx*(F(i) - F(i-1))
		end do
		! condition de newman
		u_n(1) = u_n(2)
		u_n(nx) = u_n(nx-1)
		! mise à jour
		u = u_n
		date = date + dt
		if(date > tf) then 
			exit
		end if
	end do
!--------------------------------------------------------------!
! Enregistrement
!--------------------------------------------------------------!	
	open(ifile, file = "laxfriedrichs.dat", status = "unknown")
	write(ifile, '(ES22.6, ES22.6)') (x(i), u(i), i = 1, nx)
	close(ifile)
!--------------------------------------------------------------!
! monter en ordre : MUSCL
!--------------------------------------------------------------!
	
	do i = 1, nx
		sigma(i) = min(abs(u(i+1) - u(i))/dx, abs(u(i) - u(i-1))/dx)
		u(i) = u(i) + 0.5*dx*sigma(i)
	end do
	date = 0._rp
	u_n = u 
	
	do
		dt = dx*cfl/maxval(abs(3._rp*u**2-1),dim = 1)**2
		call flux(nx, dx, dt, F, u, 1)
		do i = 2, nx - 1
			u_n(i) = u_n(i) -dt/dx*(F(i) - F(i-1))
		end do
		! condition de newman
		u_n(1) = u_n(2)
		u_n(nx) = u_n(nx-1)
		! mise à jour
		u = u_n
		date = date + dt
		if(date > tf) then 
			exit
		end if
	end do
!--------------------------------------------------------------!
! Enregistrement
!--------------------------------------------------------------!	
	open(ifile, file = "muscl.dat", status = "unknown")
	write(ifile, '(ES22.6, ES22.6)') (x(i), u(i), i = 1, nx)
	close(ifile)
 
	
	call system("gnuplot")
	deallocate(x, u, u_n, F) 
end program burger

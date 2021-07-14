module mod_flux
use variable
contains
	subroutine flux(nx, dx, dt, F, u, flag)
		implicit none
		integer :: nx, flag
		real(rp) :: dt, dx
		real(rp), allocatable, dimension(:) :: F, u
		F = 0.0_rp
		select case(flag)
			case(1)
	 			do i = 1, nx -1
					F(i) = 0.25_rp*((u(i+1)**3-u(i+1))+(u(i)**3-u(i)))-0.5_rp*dx/dt*(u(i+1)-u(i))
				end do
			case(2)
				do i = 1, nx - 1
				F(i) = 0.5_rp*((u(i+1)**3-u(i+1))+(u(i)**3-u(i)))-0.25_rp*dt/dx*(3*(u(i+1)+u(i))**2-1)*((u(i+1)**3-u(i+1))-(u(i)**3-u(i)))
				end do
			case default
				write(6,*)' 1 = lax-riedrichs, 2 = lax-wendorff'
		end select
	end subroutine flux
	 
end module mod_flux

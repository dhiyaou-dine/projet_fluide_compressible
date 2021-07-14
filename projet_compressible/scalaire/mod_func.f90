module mod_func
use variable
contains
		real function u_init(x, flag) 
		integer :: flag
		real(rp), intent(in) :: x
		select case(flag)
			case(1)
 				if (0.5_rp > x) then
					u_init = ul
				else
					u_init = ur
				end if
			case(2)
				if (0.5_rp > x) then
					u_init = ur
				else
					u_init = ul
				end if
			case(3)
				if(x < 0.5_rp ) then
					u_init = ul
				else if(0.25_rp < x + 0.25_rp .and. x < 0.5_rp) then
					u_init = ulr
				else 
					u_init = ul
				end if 
			case default
				write(6,*) " 1 = créneau, 2 = detente, 3 = escalier" 
		end select	
	end function
end module mod_func

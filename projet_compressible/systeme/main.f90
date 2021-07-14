! dans  le module mod_var, on a déclaré les parametres necessaire
! gnuplot plot_h.gnu
! gnuplot plot_u.gnu
program main
use mod_systeme
implicit none
		            hg  		= 1.0       
		            ug  		= 1.0
		            hd  		= 2.0       
		            ud  		= 0.0 
	call systeme
	
end program main

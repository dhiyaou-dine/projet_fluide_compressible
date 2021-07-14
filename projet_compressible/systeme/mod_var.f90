module mod_var
implicit none
	integer, 	 parameter :: Nx   		= 500 , rp = 8   	! nombre de mailles
	real(rp),    parameter :: g=9.81
	real(rp),	 parameter :: L    		= 10.0	    ! longueur du domaine a gauche et a droite
	real(rp),	 parameter :: Tmax 		= 1.0		! borne en temps
	integer,	 parameter :: Smax 		= 30		! nombre de sauvegardes
	real(rp),	 parameter :: cfl  		= 0.5       ! coefficient CFL
	
	! CI
	real(rp)	           :: hg  		      
	real(rp)	           :: ug  		
	real(rp)	           :: hd  		      
	real(rp)	           :: ud  		     
	
	
	integer 						:: Nt, i, j, sauv
	real(rp)				:: dx, dt, t, alpha
	real(rp), dimension(Nx)	:: x
	
	real(rp), dimension(Nx)	:: h ! exacte
	real(rp), dimension(Nx)	:: u ! exacte
	
	real(rp), dimension(Nx)	:: hLF ! approchee par Lax-Friedrichs 
	real(rp), dimension(Nx)	:: uLF ! approchee par Lax-Friedrichs
	
	real(rp), dimension(Nx)	:: hRu ! approchee par Rusanov 
	real(rp), dimension(Nx)	:: uRu ! approchee par Rusanov
end module mod_var

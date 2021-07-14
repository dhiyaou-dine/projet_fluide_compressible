!
! Resolution des equations de Saint Venant
! d_t h(x,t) + d_x (hu)(x,t) = 0
! d_t (hu)(x,t) + d_x (hu**2+gh**2/2)(x,t) = 0
! pour le probleme de Riemann
! (h,u)(x<0,0) = (h,u)_L
! (h,u)(x>0,0) = (h,u)_R
! avec conditions au bord de Newmann
!
! Creer une directory data dans laquelle seront sauvegarde les sorties
!
!
! --- Pour voir les ``films'' avec gnuplot
! cd data

! 


module mod_systeme
use mod_var
use mod_flux
use mod_calcul
use mod_save
use mod_gnu
contains
	subroutine systeme
	
	
	Nt = 0						! numero de la sauvegarde
	dx = 2*L/Nx					! pas d'espace
	t  = 0.0 					! instant
	
	x = (/(-L+j*dx, j=1,Nx)/)	! position
	h = (/(0., j=1,Nx)/)		! h exacte
	u = (/(0., j=1,Nx)/)		! u exacte
	
	! Initialisations
	x = 0._rp	
	do i = 1, Nx
  		x(i) = x(i) + i*dx  
  		if (x(i) < L/2) then
	     h(i) = hg      
	     u(i) = hg*ug 
	  else
	     h(i) = hd      
	     u(i) = hd*ud 
	  end if
	end do
	hLF = h
	uLF = u
	hRu = h
	uRu = u
	
    ! Sauvegarde de la CI
    sauv = 0
    call sauvegarde(sauv,x,h,u,hLF,uLF,hRu,uRu)
    
!-----------------------------------------------------------------------------!
! 			Boucle de temps(marche en temps)
!-----------------------------------------------------------------------------!
	do while (t<Tmax)

		dt    = cfl*dx/max(abs(maxval(u-sqrt(g*h))),abs(maxval(u+sqrt(g*h))))	! pas de temps
		alpha = dt/dx	
		Nt    = Nt+1 
		t     = t+dt
		
		call Calcul_num(alpha,hLF,uLF,hRu,uRu)
 
		! sauvegarde dans un fichier de toutes les solutions
		if ( floor(t*Smax/Tmax)>floor((t-dt)*Smax/Tmax) ) then
			sauv=sauv+1
	        call sauvegarde(sauv,x,h,u,hLF,uLF,hRu,uRu)
        end if
	end do
		
	call scriptgnuplot(sauv)
	end subroutine systeme
end module mod_systeme

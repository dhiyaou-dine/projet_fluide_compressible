module mod_flux
use mod_var
contains
!---------------------------------------------------------------------------------------!
! 							LES FLUX
!---------------------------------------------------------------------------------------!
 ! Flux de Lax-Friedrichs
		subroutine Flux_LF(alpha,hg,ug,hd,ud,hflux,huflux)
			real(rp), 				 intent(in) :: alpha
            real(rp), dimension(Nx), intent(in) :: hg,ug,hd,ud
            real(rp), dimension(Nx) 		    :: hflux, huflux
			hflux = 0.5*( hg*ug                 + hd*ud                 -(1/alpha)*(hd-hg) )
			huflux= 0.5*( hg*ug**2+(g/2.0)*hg**2+ hd*ud**2+(g/2.)*hd**2 -(1/alpha)*(hd*ud-hg*ug))
		end subroutine Flux_LF
		
		! Flux de Rusanov
		subroutine Flux_Rusanov(hg,ug,hd,ud,hflux,huflux)
            real(rp), dimension(Nx), intent(in) :: hg,ug,hd,ud
            real(rp), dimension(Nx) 		    :: hflux, huflux
            real(rp), dimension(Nx) 		    :: Lambda
            Lambda = max(abs(ug)+sqrt(g*hg),abs(ud)+sqrt(g*hd))
			hflux =0.5*(hd*ud                +hg*ug                 -Lambda*(hd-hg))
			huflux=0.5*(hd*ud**2+(g/2.)*hd**2+hg*ug**2+(g/2.0)*hg**2-Lambda*(hd*ud-hg*ug))
		end subroutine Flux_Rusanov 
end module mod_flux

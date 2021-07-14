module mod_calcul
use mod_var
use mod_flux
contains
!---------------------------------------------------------------------------------------!
!						LES SCHEMAS
!---------------------------------------------------------------------------------------!
	 	subroutine Calcul_num(alpha,hLF,uLF,hRu,uRu)
			real(rp), 				 intent(in) :: alpha
            real(rp), dimension(Nx) 		    :: hLF, uLF, huLF						! Variable pour Lax
            real(rp), dimension(Nx) 		    :: hLF_M, hLF_P, hfluxLF_M, hfluxLF_P   ! Décallage pour h et flux de h de Lax
            real(rp), dimension(Nx) 		    :: uLF_M, uLF_P, hufluxLF_M, hufluxLF_P ! Décallage pour u et flux de hu de Lax
            real(rp), dimension(Nx) 		    :: hRu, uRu, huRu                       ! Variable pour Rusanov
            real(rp), dimension(Nx) 		    :: hRu_M, hRu_P, hfluxRu_M, hfluxRu_P   ! Décallage pour h et flux de h de Rusanov
            real(rp), dimension(Nx) 		    :: uRu_M, uRu_P, hufluxRu_M, hufluxRu_P ! Décallage pour u et flux de hu de Rusanov
			
			!-------- Lax-Friedrichs ---------!
			huLF=hLF*uLF
            ! conditions de Neumann
			hLF_P(1:Nx-1) = hLF(2:Nx)
			uLF_P(1:Nx-1) = uLF(2:Nx)
			hLF_P(Nx)     = hLF(Nx)
			uLF_P(Nx)     = uLF(Nx)
			hLF_M(1)      = hLF(1)
			uLF_M(1)      = uLF(1)
			hLF_M(2:Nx)   = hLF(1:Nx-1)
			uLF_M(2:Nx)   = uLF(1:Nx-1)
            ! schema
            call Flux_LF(alpha,hLF,uLF,hLF_P,uLF_P,hfluxLF_P,hufluxLF_P) ! decallage à gauche pour Lax
            call Flux_LF(alpha,hLF_M,uLF_M,hLF,uLF,hfluxLF_M,hufluxLF_M) ! decallage à droite pour Lax
			hLF  = hLF  - alpha*(hfluxLF_P -hfluxLF_M)	                 		
			huLF = huLF - alpha*(hufluxLF_P-hufluxLF_M)			
			uLF = huLF/hLF

			!-------- Rusanov ---------!
			huRu=hRu*uRu
            ! conditions de Neumann
			hRu_P(1:Nx-1) = hRu(2:Nx)
			uRu_P(1:Nx-1) = uRu(2:Nx)
			hRu_P(Nx)     = hRu(Nx)
			uRu_P(Nx)     = uRu(Nx)
			hRu_M(1)      = hRu(1)
			uRu_M(1)      = uRu(1)
			hRu_M(2:Nx)   = hRu(1:Nx-1)
			uRu_M(2:Nx)   = uRu(1:Nx-1)
            ! schema
            call Flux_Rusanov(hRu,uRu,hRu_P,uRu_P,hfluxRu_P,hufluxRu_P) ! decallage à droite pour Rusanov
            call Flux_Rusanov(hRu_M,uRu_M,hRu,uRu,hfluxRu_M,hufluxRu_M) ! decallage à gauche pour Rusanov
			hRu  = hRu  - alpha*(hfluxRu_P -hfluxRu_M)			
			huRu = huRu - alpha*(hufluxRu_P-hufluxRu_M)			
			uRu = huRu/hRu
			
		end subroutine Calcul_num
end module mod_calcul

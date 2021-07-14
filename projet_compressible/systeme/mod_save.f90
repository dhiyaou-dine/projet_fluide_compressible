module mod_save
use mod_var
contains
!---------------------------------------------------------------------------------------!
!									SAUVEGARDE
!---------------------------------------------------------------------------------------!
subroutine sauvegarde(sauv,x,h,u,hLF,uLF,hRu,uRu)

            integer                                     :: j
            integer,						 intent(in)	:: sauv
            real(rp), dimension(Nx), intent(in)         :: x, h, u, hLF,uLF,hRu,uRu
	        character(len=30)	 		                :: file_name ! nome du fichier a sauvegarder

            write(file_name,'("./systeme",i3.3,".dat")')  sauv
            open(unit=11,file=file_name)
            do j=1,Nx
                write(11,*) x(j), h(j), u(j), hLF(j), uLF(j), hRu(j), uRu(j)
            end do
            close(11)
            print "(A,I3,A)", " ===== Sauvegarde num.", sauv, " ====="
            
        end subroutine sauvegarde
end module mod_save

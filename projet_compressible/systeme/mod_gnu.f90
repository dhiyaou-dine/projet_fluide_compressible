module mod_gnu
use mod_var
contains
!---------------------------------------------------------------------------------------!
!							SCRIPTE GNUPLOT POUR LA TRACEE
!---------------------------------------------------------------------------------------!
	 subroutine scriptgnuplot(sauv)

		integer, intent(in) :: sauv
		integer				:: i, j, l1,l2

		
		open(unit=15,file="./plot_h.gnu")
		open(unit=16,file="./plot_u.gnu")
		do i=15,16
			write(i,'("set grid")')
			do j=0,sauv
				l1=i-11
				l2=l1+2
				write(i,'(a17,i3.3,a29,i3.3,a10,i1.1,a19,i3.3,a10,i1.1,a6)') "plot 'systeme",j,".dat' u 1:2 w l, 'systeme",j,&
				                                           ".dat' u 1:",l1," w lp, 'systeme",j,".dat' u 1:",l2," w lp;"
				write(i,'("pause 0.05;")')
			end do
			write(i,'("pause -1;")')
			close(i)
		end do		

	end subroutine scriptgnuplot
end module mod_gnu

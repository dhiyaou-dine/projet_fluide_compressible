FC = gfortran

FFLAGS = -g -O3 -fbacktrace

LD = $(FC)

LDGLAGS = -O3

OBGS = mod_var.o mod_flux.o mod_calcul.o mod_save.o mod_gnu.o mod_systeme.o main.o
MODS = mod_var.mod mod_flux.mod mod_calcul.mod mod_save.mod mod_gnu.mod mod_systeme.mod



EXEC = fic.out


all : $(EXEC)

$(EXEC) : $(OBGS)
	$(LD) $(LDGLAGS) $(OBGS) -o $(EXEC)
clean :
	\rm -f90 $(OBGS) $(MODS) 
mod_calcul.o : mod_calcul.f90
	$(FC) -c $(FFLAGS) mod_calcul.f90 -o mod_calcul.o
mod_save.o : mod_save.f90
	$(FC) -c $(FFLAGS) mod_save.f90 -o mod_save.o
mod_gnu.o : mod_gnu.f90
	$(FC) -c $(FFLAGS) mod_gnu.f90 -o mod_gnu.o
mod_flux.o : mod_flux.f90
	$(FC) -c $(FFLAGS) mod_flux.f90 -o mod_flux.o
mod_systeme.o : mod_systeme.f90
	$(FC) -c $(FFLAGS) mod_systeme.f90 -o mod_systeme.o
main.o : main.f90 
	$(FC) -c $(FFLAGS) main.f90 -o main.o
mod_var.o : mod_var.f90
	$(FC) -c $(FFLAGS) mod_var.f90 -o mod_var.o



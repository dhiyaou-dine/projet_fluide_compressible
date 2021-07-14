FC = gfortran

FFLAGS = -g -O3 -fbacktrace

LD = $(FC)

LDGLAGS = -O3

OBGS = variable.o mod_func.o mod_flux.o scalaire.o
MODS = variable.mod mod_func.mod mod_flux.mod



EXEC = fic.out


all : $(EXEC)

$(EXEC) : $(OBGS)
	$(LD) $(LDGLAGS) $(OBGS) -o $(EXEC)
clean :
	\rm -f90 $(OBGS) $(MODS) 
mod_func.o : mod_func.f90
	$(FC) -c $(FFLAGS) mod_func.f90 -o mod_func.o
mod_flux.o : mod_flux.f90
	$(FC) -c $(FFLAGS) mod_flux.f90 -o mod_flux.o
scalaire.o : scalaire.f90 
	$(FC) -c $(FFLAGS) scalaire.f90 -o scalaire.o
variable.o : variable.f90
	$(FC) -c $(FFLAGS) variable.f90 -o variable.o



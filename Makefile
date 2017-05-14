
#F95=ifort
F90=gfortran

EXE=sing_spo


SRC = $(wildcard *.f90)
OBJ=$(patsubst %.f90,%.o,$(SRC))



$(EXE): $(OBJ)
	$(F90) -g $^ -o $@

%.o:%.f90 
	$(F90) -g -c $< -o $@

clean:
	rm -f *.o $(EXE)


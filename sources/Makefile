
all: lib solver

solver:
	${MAKE} -C electrostatics2d
	${MAKE} -C electrostatics3d
	${MAKE} -C direct_current_2d
	${MAKE} -C direct_current_3d
	${MAKE} -C eddy_currents_2d
	${MAKE} -C eddy_currents_axi
	${MAKE} -C magnetostatics2d
	${MAKE} -C transient_magnetics2d
	${MAKE} -C eddy_currents_3d

lib:
	${MAKE} -C libs/mumps-4.9.2

clean: cleanlib cleansolver

cleansolver:
	${MAKE} -C electrostatics2d clean
	${MAKE} -C electrostatics3d clean
	${MAKE} -C direct_current_2d clean
	${MAKE} -C direct_current_3d clean
	${MAKE} -C eddy_currents_2d clean
	${MAKE} -C eddy_currents_axi clean
	${MAKE} -C magnetostatics2d clean
	${MAKE} -C transient_magnetics2d clean
	${MAKE} -C eddy_currents_3d clean

cleanlib:
	${MAKE} -C libs/mumps-4.9.2 clean

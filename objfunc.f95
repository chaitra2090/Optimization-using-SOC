! The following function objfuncS calls the corresponding glue code in C which in turn
! allows access to the objective function in Java via the invocation interface.
FUNCTION objfuncS(c, iflag) RESULT(f)
! On input:
! c - Point coordinates.
!
! On output:
! iflag - A flag that is used to indicate the status of the
! function evaluation. It is 0 for normal status.
! f - Function value at ‘c’.
!

USE REAL_PRECISION, ONLY : R8
USE, INTRINSIC :: ISO_C_BINDING
IMPLICIT NONE
! Dummy variables.
REAL(KIND = R8), DIMENSION(:), INTENT(IN):: c
INTEGER, INTENT(OUT):: iflag
REAL(KIND = R8) :: f
! Local variables.
REAL(KIND = C_DOUBLE), TARGET :: designPoints(1:SIZE(c))
TYPE(C_PTR) :: designPointsPtr
INTEGER(C_INT) :: N

! objS is the name of the real function procedure in C defining the
! objective function f(x) to be minimized. objS(c, c_size)
! returns the value f(c). c_size represents the size of ‘c’.
!
INTERFACE
  FUNCTION objS(c, c_size) RESULT(f) BIND(C, NAME=‘objS’)
	USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_DOUBLE, C_PTR, C_INT
	TYPE(C_PTR), INTENT(IN), VALUE :: c
	INTEGER(C_INT), INTENT(IN) :: c_size
	REAL(KIND = C_DOUBLE) :: f
  END FUNCTION objS
END INTERFACE

! Conversion of assumed-shape array to static array.
designPointsPtr = c_loc(designPoints(1))
designPoints = c
N = SIZE(c)

! Call to glue-code in C.
f = objS(designPointsPtr, N)
iflag = 0
RETURN
END FUNCTION objfuncS

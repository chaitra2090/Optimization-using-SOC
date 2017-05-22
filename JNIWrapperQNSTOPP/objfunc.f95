! The following function objfuncP calls the corresponding glue code in C which in turn
! allows access to the objective function in Java via the invocation interface.

FUNCTION objfuncP(c, p, resp) RESULT(iflag)
! On input:
! c - Chunked point coordinates.
! p - Problem dimension.
! resp - Dummy array.
!
! On output:
! resp - Array containing the function values.
! iflag - A flag that is used to indicate the status of the
! function evaluation values returned from Java. It is
! 0 for normal status.
!
USE REAL_PRECISION, ONLY : R8
USE, INTRINSIC :: ISO_C_BINDING
IMPLICIT NONE
! Dummy variables.
REAL(KIND = R8), DIMENSION(:), INTENT(IN) :: c
INTEGER, INTENT(IN) :: p
REAL(KIND = R8), DIMENSION(:), INTENT(INOUT) :: resp
INTEGER :: iflag
! Local variables.
REAL(KIND = C_DOUBLE), TARGET :: designPoints(1:SIZE(c))
TYPE(C_PTR) :: designPointsPtr
REAL(KIND = C_DOUBLE), TARGET :: respVars(1:SIZE(resp))
TYPE(C_PTR) :: respVarsPtr

! objP is the name of the procedure in C defining the objective
! function f(x) for each ‘x’ in ‘c’ to be minimized.
! objP(c, c_size, p, resp) sets the function values in the
! array ‘resp’. ‘c’ contains multiple design points chunked
! together. ‘c_size’ represents the size of ‘c’. ‘p’ is the
! problem dimension.
!
INTERFACE
  SUBROUTINE objP(c, c_size, p, resp) bind (C, name = "objP")
	USE, INTRINSIC :: ISO_C_BINDING
	IMPLICIT NONE
	TYPE(C_PTR), INTENT(IN), VALUE :: c
	INTEGER(C_INT), INTENT(IN):: c_size
	INTEGER(C_INT), INTENT(IN) :: p
	TYPE(C_PTR), INTENT(IN), VALUE :: resp
  END SUBROUTINE objP
END INTERFACE

! Conversion of assumed-shape arrays to static arrays.
designPointsPtr = c_loc(designPoints(1))
designPoints = c
respVarsPtr = c_loc(respVars(1))

! Call to glue-code in C
CALL objP(designPointsPtr, SIZE(c), p, respVarsPtr)

! Copy contents of array returned by C to array ‘resp’.
resp = respVars
iflag = 0
RETURN
END FUNCTION objfuncP

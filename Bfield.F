#include "symbol.inc"
      MODULE Bfield_modular

      USE prec
      USE constant

      REAL(q),PRIVATE,SAVE :: Bfield(3)
      CONTAINS
!*****************************************************************************
!
! initialise the magnetic field Bx, By, Bz
!
!*****************************************************************************


      SUBROUTINE Bfield_READER(IU5)
      USE base
      USE vaspxml


      INTEGER IU5, IU0
      LOGICAL :: LOPEN,LDUM      
      COMPLEX(q) :: CDUM
      CHARACTER (1) :: CHARAC

      LOPEN=.FALSE.
      OPEN(UNIT=IU5,FILE=INCAR,STATUS='OLD')
      
      Bfield(1)=0.d0
      Bfield(2)=0.d0
      Bfield(3)=0.d0

      CALL RDATAB(LOPEN,INCAR,IU5,'Bfield','=','#',';','F', &
                  IDUM,Bfield,CDUM,LDUM,CHARAC,N,3,IERR)
    
      IF (((IERR/=0).AND.(IERR/=3)).OR. &
                          ((IERR==0).AND.(N<3))) THEN
         IF (IU0>=0) &
         WRITE(IU0,*)'Error reading item ''BCONST'' from file INCAR.'
         BCONST=0
      ENDIF




      END SUBROUTINE Bfield_READER


!**********************SUBROUTINE ADD_Bfield_POT*****************************
!
! expects CVTOT in (charge,magnetization) convention in real space 
!
!****************************************************************************

      SUBROUTINE ADD_Bfield_POT(CVTOT,GRIDC,WDES)

      USE mgrid
      USE wave
      
      IMPLICIT NONE
      INTEGER :: ISP ! spin index

      TYPE (wavedes) WDES
      TYPE (grid_3d) GRIDC
      RGRID CVTOT(DIMREAL(GRIDC%MPLWV),WDES%NCDIJ)


      DO ISP=2, WDES%NCDIJ
         CVTOT(:,ISP)=CVTOT(:,ISP)-Bfield(ISP-1)*MUB
      ENDDO

      END SUBROUTINE ADD_Bfield_POT



     END MODULE Bfield_modular

      SUBROUTINE JPLVER( JPVER )
C
C     Set a version number for the SCHED main code.  
C     Note that a coarser, date based version is set in sched.f
C
      REAL JPVER
Cf2py intent(out) JPVER
C ----------------------------------------------------------------------
      JPVER = 1.01
C
      RETURN
      END

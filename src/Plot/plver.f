      SUBROUTINE PLVER( PLTVER )
C
C     Set a version number for the SCHED main code.  
C     Note that a coarser, date based version is set in sched.f
C
      REAL PLTVER
Cf2py intent(out) PLTVER
C ----------------------------------------------------------------------
C      PLTVER = 1.02   ! Went with 16 Oct 2001 version.
C      PLTVER = 1.03   ! RCW configuration search stuff.
C      PLTVER = 1.04   ! RCW.  Used in 15 May 2002 release.
C      PLTVER = 1.05   ! A few enhancements by FT over 1.04.
      PLTVER = 1.06   ! A few bug fixes by FT over 1.05.  Feb 2006.
C
      RETURN
      END


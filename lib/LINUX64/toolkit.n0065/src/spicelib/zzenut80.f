C$Procedure ZZENUT80 ( Earth nutation transformation, IAU 1980 model )

      SUBROUTINE ZZENUT80 ( ET, NUTXF )
      IMPLICIT NONE 
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Compute the state transformation matrix implementing the IAU 1980
C     nutation model.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     FRAMES
C     MATRIX
C     PRIVATE
C     TRANSFORMATION
C     UTILITY
C
C$ Declarations

      DOUBLE PRECISION      ET
      DOUBLE PRECISION      NUTXF  ( 6, 6 )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  -------------------------------------------------
C     ET         I   Ephemeris time, seconds past J2000.
C     NUTXF      O   Nutation transformation matrix.
C
C$ Detailed_Input
C
C     ET             is an epoch, expressed as seconds past J2000 TDB.
C
C$ Detailed_Output
C
C     NUTXF          is a state transformation matrix that maps states
C                    from the earth mean equator and equinox of date
C                    frame (based on the 1976 IAU precession model) to
C                    the earth true equator and equinox frame of date
C                    (based on the 1980 IAU nutation model).
C                                       
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     See the private SPICELIB routine ZZWAHR for a discussion
C     of the implementation of the 1980 IAU nutation model.
C
C     See the private SPICELIB routine ZZMOBLIQ for a discussion
C     of the implementation of the 1980 IAU earth mean obliquity 
C     of date model.
C     
C$ Examples
C
C     See ZZDYNFRM.
C
C$ Restrictions
C
C     1) This is a SPICE private routine; the routine is subject
C        to change without notice.  User applications should not
C        call this routine.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C
C$ Literature_References
C
C     [1] "Explanatory Supplement to the Astronomical Almanac"
C          edited by P. Kenneth Seidelmann. University Science
C          Books, 20 Edgehill Road, Mill Valley, CA 94941 (1992)
C
C     [2] "Section 5, Geocentric Space-Fixed Position, Velocity, and
C         Acceleration Vectors of Tracking Station" by T. D. Moyer.
C         Draft of JPL Publication documenting the JPL navigation
C         program "Regres."
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 18-DEC-2004 (NJB) 
C
C-&
      
C
C     SPICELIB functions
C
      LOGICAL               RETURN
      
C
C     Local variables
C
      DOUBLE PRECISION      DMOB
      DOUBLE PRECISION      DVNUT  ( 4 )
      DOUBLE PRECISION      EULANG ( 6 )
      DOUBLE PRECISION      MOB


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZENUT80' )

C
C      Get nutation angles and their rates.  We're expecting
C
C         DVNUT(1) = Psi------nutation in longitude (radians)
C         DVNUT(2) = Epsilon--nutation in obliquity (radians)
C         DVNUT(3) = dPsi/dt     (radians/second)
C         DVNUT(4) = dEpsilon/dt (radians/second)
C
      CALL ZZWAHR ( ET, DVNUT )

C
C     Get the mean obliquity of date.
C
C     We're expecting the outputs to be as follows:
C
C         MOB      is the mean obliquity of the ecliptic at epoch
C                  ET. The mean obliquity of the ecliptic is the
C                  inclination of the ecliptic of date to the
C                  mean Earth equator of date.  Output units are
C                  radians.
C
C         DMOB     is the time derivative of MOB at ET, expressed
C                        in radians per second.

      CALL ZZMOBLIQ ( ET, MOB, DMOB )

C
C     The nutation rotation N is defined by
C
C
C         N = [ -MOB - NUOBL ]  [ -NULON ]   [ MOB ]
C                             1           3         1
C
C     where MOBLIQ is the mean obliquity of the earth's ecliptic
C     at epoch, NUOB is nutation in obliquity at epoch, and
C     NULONG is nutation in longitude at epoch.  Using our
C     variable names, the Euler angle sequence is
C
C        [ -MOB - DVNUT(2) ]  [ -DVNUT(1) ]  [ MOB ]
C                           1              3        1
C
C     The rates corresponding to these angles are:
C
C        -DMOB - DVNUT(4),  -DVNUT(3),  DMOB
C
C     We can use EUL2XF to form the state transformation from
C     the nutation base frame to the nutation frame.
C

      EULANG(1) =  -MOB  - DVNUT(2)
      EULANG(2) =        - DVNUT(1)
      EULANG(3) =   MOB
      EULANG(4) =  -DMOB - DVNUT(4)
      EULANG(5) =        - DVNUT(3)
      EULANG(6) =   DMOB

      CALL EUL2XF ( EULANG, 1, 3, 1, NUTXF )

      CALL CHKOUT ( 'ZZENUT80' )
      RETURN
      END

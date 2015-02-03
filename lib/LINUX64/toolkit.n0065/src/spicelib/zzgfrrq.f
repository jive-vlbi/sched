C$Procedure ZZGFRRQ ( Private - GF, range rate between objects )
 
      SUBROUTINE ZZGFRRQ ( ET, TARG, OBS, ABCORR, VALUE )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Compute the apparent range rate between two ephemeris objects.
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
C     RANGE RATE
C     EPHEMERIS
C     GEOMETRY
C     SEARCH
C
C$ Declarations
 
      IMPLICIT NONE

      DOUBLE PRECISION      ET
      INTEGER               TARG
      INTEGER               OBS
      CHARACTER*(*)         ABCORR
      DOUBLE PRECISION      VALUE
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ET         I   Ephemeris seconds past J2000 TDB
C     TARG       I   Target body ID
C     OBS        I   Observer body ID
C     ABCORR     I   Aberration correction flag
C     REF        I   Reference frame of the range rate
C     VALUE      O   Value of range rate between objects
C
C$ Detailed_Input
C
C     ET       is the time in ephemeris seconds past J2000 TDB at
C              which the range rate is to be measured.
C
C     TARG     the SPICE integer ID for the target body.
C
C     OBS      the SPICE integer ID for the observer.
C
C     ABCORR   the string description of the aberration corrections to 
C              apply to the state evaluations to account for one-way 
C              light time and stellar aberration.
C
C              Any aberration correction accepted by the SPICE 
C              routine SPKEZR is accepted here. See the header
C              of SPKEZR for a detailed description of the 
C              aberration correction options. For convenience,
C              the options are listed below:
C
C                 'NONE'     Apply no correction. Returns the "true"
C                            geometric state.
C
C                 'LT'       "Reception" case:  correct for
C                            one-way light time using a Newtonian
C                            formulation.
C
C                 'LT+S'     "Reception" case:  correct for
C                            one-way light time and stellar
C                            aberration using a Newtonian
C                            formulation.
C
C                 'CN'       "Reception" case:  converged
C                            Newtonian light time correction.
C
C                 'CN+S'     "Reception" case:  converged
C                            Newtonian light time and stellar
C                            aberration corrections.
C
C                 'XLT'      "Transmission" case:  correct for
C                            one-way light time using a Newtonian
C                            formulation.
C
C                 'XLT+S'    "Transmission" case:  correct for
C                            one-way light time and stellar
C                            aberration using a Newtonian
C                            formulation.
C
C                 'XCN'      "Transmission" case:  converged
C                            Newtonian light time correction.
C
C                 'XCN+S'    "Transmission" case:  converged
C                            Newtonian light time and stellar
C                            aberration corrections.
C
C              The ABCORR string lacks sensitivity to case, leading 
C              and trailing blanks.
C
C$ Detailed_Output
C
C     VALUE    is the optionally light-time corrected range
C              rate of TARG observed from OBS.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine determines the apparent range rate of a target,
C     TARG, as seen from an observer, OBS, at epoch ET.  
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber     (JPL)
C     L.S. Elson     (JPL)
C
C$ Version
C
C-    SPICELIB version 1.0.1 08-JUL-2010 (EDW)
C
C        Minor typo correction to comments.
C
C-    SPICELIB version 1.0.0 09-JUN-2009 (NJB)(EDW)
C
C-&

C$ Index_Entries
C
C   compute the range rate between two objects.
C
C-&

C
C     SPICELIB functions.
C
      DOUBLE PRECISION      DVNORM

      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local Variables.
C
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      STATE    ( 6 )

      CHARACTER*(5)         REF

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN  ( 'ZZGFRRQ' ) 
      END IF

C
C     We just want the range rate of TARG relative to OBS.
C     This calculation is invariant with respect to reference
C     frame; we use 'J2000'.
C
      REF = 'J2000'
      CALL SPKEZ ( TARG, ET, REF, ABCORR, OBS, STATE, LT )
 
      IF (  FAILED() ) THEN
          CALL CHKOUT (  'ZZGFRRQ' )
          RETURN
      END IF 

C
C     Calculate the derivative from the STATE vector.
C
      VALUE = DVNORM( STATE )

C
C     All done.
C
      CALL CHKOUT (  'ZZGFRRQ' )
      RETURN

      END


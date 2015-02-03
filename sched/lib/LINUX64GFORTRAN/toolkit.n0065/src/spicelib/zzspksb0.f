 
C$Procedure ZZSPKSB0 ( S/P Kernel, solar system barycenter )
 
      SUBROUTINE ZZSPKSB0 ( TARG, ET, REF, STARG )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Return the state (position and velocity) of a target body
C     relative to the solar system barycenter.
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
C     SPK
C
C$ Keywords
C
C     EPHEMERIS
C
C$ Declarations
 
      IMPLICIT NONE 
      INTEGER               TARG
      DOUBLE PRECISION      ET
      CHARACTER*(*)         REF
      DOUBLE PRECISION      STARG    ( 6 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     TARG       I   Target body.
C     ET         I   Target epoch.
C     REF        I   Target reference frame.
C     STARG      O   State of target.
C
C$ Detailed_Input
C
C     TARG        is the standard NAIF ID code for a target body.
C
C     ET          is the epoch (ephemeris time) at which the state
C                 of the target body is to be computed.
C
C     REF         is the name of the  reference frame to which the
C                 vectors returned by the routine should be rotated.
C                 This may be any frame supported by the SPICELIB frame
C                 system, including dynamic and other non-inertial
C                 frames.
C
C$ Detailed_Output
C
C     STARG       contains the position and velocity of the target
C                 body, relative to the solar system barycenter,
C                 at epoch ET. These vectors are rotated into the
C                 specified  reference frame. Units are always
C                 km and km/sec.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If insufficient information has not bee "loaded" via the
C        routine SPKLEF or the PCK kernel loaders, the problem will
C        be diagnosed by a routine in the call tree of this routine.
C
C$ Files
C
C     See: $Restrictions.
C
C$ Particulars
C
C     In order to compute the state of one body relative to another,
C     the states of the two bodies must be known relative to a third
C     body. One simple solution is to use the solar system barycenter
C     as the third body.
C
C     Ephemeris data from more than one segment may be required
C     to determine the state of a body relative to the barycenter.
C     ZZSPKSB0 reads as many segments as necessary, from as many
C     files as necessary, using files that have been loaded by
C     previous calls to SPKLEF (load ephemeris file).
C
C$ Examples
C
C     In the following code fragment, ZZSPKSB0 is used to display
C     the distance from Earth (Body 399) to Mars (body 499) at
C     a series of epochs.
C
C        CALL SPKLEF ( 'DE125.SPK', HANDLE )
C         .
C         .
C
C        EARTH = 399
C        MARS  = 499
C
C        DO WHILE ( EPOCH .LE. END )
C           CALL ZZSPKSB0 ( EARTH, EPOCH, 'J2000', SEARTH )
C           CALL ZZSPKSB0 ( MARS,  EPOCH, 'J2000', SMARS  )
C
C           CALL VSUB ( SMARS, SEARTH, SMARS )
C           WRITE (*,*) EPOCH, VNORM ( SMARS )
C
C           EPOCH = EPOCH + DELTA
C        END DO
C
C$ Restrictions
C
C     1) SPICE Private routine.
C
C     2) The ephemeris files to be used by ZZSPKSB0 must be loaded
C        by SPKLEF before ZZSPKSB0 is called.
C
C$ Literature_References
C
C     NAIF Document 168.0, "S- and P- Kernel (SPK) Specification and
C     User's Guide"
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 12-DEC-2004 (NJB)
C
C        Based on SPICELIB Version 2.0.2, 20-NOV-2004  (NJB)
C
C-&
 
C$ Index_Entries
C
C     state relative to solar system barycenter
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 2.0.0, 19-SEP-1995  (WLT)
C
C        The routine was simplified by replacing all of the
C        main body of code with a call to SPKGEO.  By making
C        this change the routine now supports non-inertial frames.
C
C-&
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local variables
C
      DOUBLE PRECISION      LT
      INTEGER               BARY
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZSPKSB0' )
      END IF
 
      BARY = 0
 
      CALL ZZSPKGO0 ( TARG, ET, REF, BARY, STARG, LT )
 
      CALL CHKOUT ( 'ZZSPKSB0' )
      RETURN
      END

C$Procedure SPKPV ( S/P Kernel, position and velocity )
 
      SUBROUTINE SPKPV ( HANDLE, DESCR, ET, REF, STATE, CENTER )
 
C$ Abstract
C
C     Return the state (position and velocity) of a target body
C     relative to some center of motion in a specified frame.
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

      INCLUDE               'zzctr.inc'

      INTEGER               HANDLE
      DOUBLE PRECISION      DESCR    ( 5 )
      DOUBLE PRECISION      ET
      CHARACTER*(*)         REF
      DOUBLE PRECISION      STATE    ( 6 )
      INTEGER               CENTER
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     DESCR      I   Segment descriptor.
C     ET         I   Target epoch.
C     REF        I   Target reference frame.
C     STATE      O   Position, velocity.
C     CENTER     O   Center of state.
C
C$ Detailed_Input
C
C     HANDLE,
C     DESCR       are the file handle assigned to a SPK file, and the
C                 descriptor for a segment within the file. Together
C                 they determine the ephemeris data from which the
C                 state of the body is to be computed.
C
C     ET          is the epoch (ephemeris time) at which the state
C                 is to be computed.
C
C     REF         is the name of the reference frame to
C                 which the vectors returned by the routine should
C                 be rotated. This may be any frame supported by
C                 the SPICELIB subroutine FRMCHG.
C
C$ Detailed_Output
C
C     STATE       contains the position and velocity, at epoch ET,
C                 for whatever body is covered by the specified segment.
C                 STATE has six elements:  the first three contain the
C                 body's position; the last three contain the body's
C                 velocity.  These vectors are rotated into the
C                 specified reference frame, the origin of
C                 which is located at the center of motion for the
C                 body (see CENTER, below).  Units are always km and
C                 km/sec.
C
C     CENTER      is the integer ID code of the center of motion for
C                 the state.
C
C$ Parameters
C
C     NONE.
C
C$ Exceptions
C
C     1) If the requested reference frame is not supported by the
C        current version of CHGIRF, the error 'SPICE(SPKREFNOTSUPP)'
C        is signalled.
C
C$ Files
C
C     See argument HANDLE.
C
C$ Particulars
C
C     Once SPKPV was the most basic of the SPK readers, the reader upon
C     which SPKSSB, SPKAPP, and SPKEZ were built. However, its function
C     has now largely been replaced by SPKPVN. SPKPV should not normally
C     be called except by old software written before the release of
C     SPKPVN. This routine should be considered obsolete.
C
C
C$ Examples
C
C     In the following code fragment, an entire SPK file is searched
C     for segments containing a particular epoch. For each one found,
C     the body, center, segment identifier, and range at the epoch
C     are printed out.
C
C        CALL DAFOPR ( 'TEST.SPK', HANDLE )
C        CALL DAFBFS (             HANDLE )
C
C        CALL DAFFNA ( FOUND  )
C
C        DO WHILE ( FOUND )
C           CALL DAFGS ( DESCR )
C           CALL DAFUS ( DESCR, 2, 6, DC, IC )
C
C           IF ( DC(1) .LE. ET  .AND.  ET .LE. DC(2) ) THEN
C              CALL SPKPV ( HANDLE, DESCR, ET, 'J2000', STATE, CENTER )
C              CALL DAFGN ( IDENT )
C
C              WRITE (*,*)
C              WRITE (*,*) 'Body   = ', IC(1)
C              WRITE (*,*) 'Center = ', CENTER,
C              WRITE (*,*) 'ID     = ', IDENT
C              WRITE (*,*) 'Range  = ', VNORM ( STATE )
C           END IF
C
C           CALL DAFFNA ( FOUND )
C        END DO
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     NAIF Document 168.0, "S- and P- Kernel (SPK) Specification and
C     User's Guide"
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     K.R. Gehringer  (JPL)
C     W.L. Taber      (JPL)
C     J.M. Lynch      (JPL)
C     B.V. Semenov    (KPL)
C     R.E. Thurman    (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 6.1.0, 06-DEC-2013 (BVS) (NJB)
C
C        Updated to save the input frame name and POOL state counter
C        and to do frame name-ID conversion only if the counter has
C        changed. In-line comment regarding frame change was edited.
C
C-    SPICELIB Version 6.0.0, 19-SEP-1995 (WLT)
C
C        The routine was updated to handle non-inertial frames.
C
C-    SPICELIB Version 5.0.0, 13-MAR-1995 (KRG)
C
C        The routine was updated to handle type 14.
C
C        A new exception, 3, was also added.
C
C-    SPICELIB Version 4.0.0, 04-NOV-1994 (WLT)
C
C        The routine was updated to handle type 15.
C
C-    SPICELIB Version 3.0.0, 04-AUG-1993 (NJB)
C
C        The routine was updated to handle types 08 and 09.
C
C-    SPICELIB Version 2.0.0, 01-APR-1992 (JML)
C
C        The routine was updated to handle type 05.
C
C-    SPICELIB Version 1.0.2, 18-JUL-1991 (NJB)
C
C        The description of the output STATE was expanded slightly.
C
C-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
C
C        Literature references added to the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (RET)
C
C-&
 
C$ Index_Entries
C
C     position and velocity from ephemeris
C     spk file position and velocity
C
C-&
 
 
 
C$ Revisions
C
C-    SPICELIB Version 6.0.0, 6-OCT-1994 (WLT)
C
C        The routine was updated to handle non-inertial frames.
C
C-    SPICELIB Version 5.0.0, 13-MAR-1995 (KRG)
C
C        The routine was updated to handle type 14.
C
C        A new exception, 3, was also added.
C
C-    SPICELIB Version 4.0.0, 04-NOV-1994 (WLT)
C
C        The routine was updated to handle type 15.
C
C-    SPICELIB Version 3.0.0, 04-AUG-1993 (NJB)
C
C        The routine was updated to handle types 08 and 09.
C
C-    SPICELIB Version 2.0.0, 01-APR-1992 (JML)
C
C        The routine was updated to handle type 05.
C
C-&
 
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local parameters.
C

C
C     Saved frame name length.
C
      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

C
C     Some local space is needed in which to return records, and
C     into which to unpack the segment descriptor.
C
      INTEGER               IRF
      INTEGER               IRFREQ
      INTEGER               IC         (    6 )
      DOUBLE PRECISION      DC         (    2 )
      DOUBLE PRECISION      XFORM      ( 6, 6 )
      DOUBLE PRECISION      TSTATE     ( 6    )

C
C     Saved frame name/ID item declarations.
C
      INTEGER               SVCTR1 ( CTRSIZ )
      CHARACTER*(FRNMLN)    SVREF
      INTEGER               SVIRFR

      LOGICAL               FIRST

C
C     Saved frame name/ID items.
C
      SAVE                  SVCTR1
      SAVE                  SVREF
      SAVE                  SVIRFR

      SAVE                  FIRST

C
C     Initial values.
C
      DATA                  FIRST   / .TRUE. /


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPKPV' )
      END IF
 
C
C     Initialization.
C
      IF ( FIRST ) THEN

C
C        Initialize counter.
C
         CALL ZZCTRUIN( SVCTR1 )

         FIRST = .FALSE.

      END IF
 

      CALL DAFUS ( DESCR, 2, 6, DC, IC )
 
      CENTER = IC(2)
      IRF    = IC(3)
 
C
C     Rotate the raw state from its native frame to the one requested
C     by the user only if the two frames differ.
C
      CALL ZZNAMFRM ( SVCTR1, SVREF, SVIRFR, REF, IRFREQ )
 
      IF ( IRFREQ .EQ. 0 ) THEN
 
         CALL SETMSG ( 'No support for frame #.' )
         CALL ERRCH  ( '#', REF                  )
         CALL SIGERR ( 'SPICE(SPKREFNOTSUPP)'    )
 
      ELSE IF ( IRFREQ .NE. IRF ) THEN
 
         CALL SPKPVN ( HANDLE, DESCR,  ET, IRF, TSTATE, CENTER )
         CALL FRMCHG ( IRF,    IRFREQ, ET,              XFORM  )
         CALL MXVG   ( XFORM,  TSTATE,  6, 6,           STATE  )
 
      ELSE
 
         CALL SPKPVN ( HANDLE, DESCR, ET, IRF, STATE, CENTER )
 
      END IF
 
      CALL CHKOUT ( 'SPKPV' )
      RETURN
      END

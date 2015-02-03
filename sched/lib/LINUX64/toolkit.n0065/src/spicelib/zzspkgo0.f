C$Procedure ZZSPKGO0 ( S/P Kernel, geometric state )
 
      SUBROUTINE ZZSPKGO0 ( TARG, ET, REF, OBS, STATE, LT )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Compute the geometric state (position and velocity) of a target
C     body relative to an observing body.
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

      INCLUDE               'ninert.inc'
      INCLUDE               'zzctr.inc'

      INTEGER               TARG
      DOUBLE PRECISION      ET
      CHARACTER*(*)         REF
      INTEGER               OBS
      DOUBLE PRECISION      STATE    ( 6 )
      DOUBLE PRECISION      LT
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     TARG       I   Target body.
C     ET         I   Target epoch.
C     REF        I   Target reference frame.
C     OBS        I   Observing body.
C     STATE      O   State of target.
C     LT         O   Light time.
C
C$ Detailed_Input
C
C     TARG        is the standard NAIF ID code for a target body.
C
C     ET          is the epoch (ephemeris time) at which the state
C                 of the target body is to be computed.
C
C     REF         is the name of the reference frame to
C                 which the vectors returned by the routine should
C                 be rotated. This may be any frame supported by
C                 the SPICELIB subroutine ZZFRMCH0.
C
C     OBS         is the standard NAIF ID code for an observing body.
C
C$ Detailed_Output
C
C     STATE       contains the geometric position and velocity of the
C                 target body, relative to the observing body, at epoch
C                 ET. STATE has six elements: the first three contain
C                 the target's position; the last three contain the
C                 target's velocity. These vectors are transformed into
C                 the specified reference frame. Units are always km
C                 and km/sec.
C
C     LT          is the one-way light time in seconds from the 
C                 observing body to the geometric position of the
C                 target body at the specified epoch.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If insufficient ephemeris data has been loaded to compute
C        the necessary states, the error SPICE(SPKINSUFFDATA) is
C        signaled.
C
C$ Files
C
C     See: $Restrictions.
C
C$ Particulars
C
C     ZZSPKGO0 computes the geometric state, T(t), of the target
C     body and the geometric state, O(t), of the observing body
C     relative to the first common center of motion.  Subtracting
C     O(t) from T(t) gives the geometric state of the target
C     body relative to the observer.
C
C
C        CENTER ----- O(t)
C            |      /
C            |     /
C            |    /
C            |   /  T(t) - O(t)
C            |  /
C           T(t)
C
C
C     The one-way light time, tau, is given by
C
C
C               | T(t) - O(t) |
C        tau = -----------------
C                      c
C
C
C     For example, if the observing body is -94, the Mars Observer
C     spacecraft, and the target body is 401, Phobos, then the
C     first common center is probably 4, the Mars Barycenter.
C     O(t) is the state of -94 relative to 4 and T(t) is the
C     state of 401 relative to 4.
C
C     The center could also be the Solar System Barycenter, body 0.
C     For example, if the observer is 399, Earth, and the target
C     is 299, Venus, then O(t) would be the state of 399 relative
C     to 0 and T(t) would be the state of 299 relative to 0.
C
C     Ephemeris data from more than one segment may be required
C     to determine the states of the target body and observer
C     relative to a common center.  ZZSPKGO0 reads as many segments
C     as necessary, from as many files as necessary, using files
C     that have been loaded by previous calls to SPKLEF (load
C     ephemeris file).
C
C     ZZSPKGO0 is similar to SPKEZ but returns geometric states
C     only, with no option to make planetary (light-time) nor
C     stellar aberration corrections.  The geometric states
C     returned by SPKEZ and ZZSPKGO0 are the same.
C
C$ Examples
C
C     The following code example computes the geometric
C     state of the moon with respect to the earth and
C     then prints the distance of the moon from the
C     the earth at a number of epochs.
C
C     Assume the SPK file SAMPLE.BSP contains ephemeris data
C     for the moon relative to earth over the time interval
C     from BEGIN to END.
C
C            INTEGER               EARTH
C            PARAMETER           ( EARTH = 399 )
C
C            INTEGER               MOON
C            PARAMETER           ( MOON  = 301 )
C
C            INTEGER               N
C            PARAMETER           ( N     = 100 )
C
C            INTEGER               I
C            CHARACTER*(20)        UTC
C            DOUBLE PRECISION      BEGIN
C            DOUBLE PRECISION      DELTA
C            DOUBLE PRECISION      END
C            DOUBLE PRECISION      ET
C            DOUBLE PRECISION      LT
C            DOUBLE PRECISION      STATE ( 6 )
C
C            DOUBLE PRECISION      VNORM
C
C     C
C     C      Load the binary SPK ephemeris file.
C     C
C            CALL FURNSH ( 'SAMPLE.BSP' )
C
C            .
C            .
C            .
C
C     C
C     C      Divide the interval of coverage [BEGIN,END] into
C     C      N steps.  At each step, compute the state, and
C     C      print out the epoch in UTC time and position norm.
C     C
C            DELTA = ( END - BEGIN ) / N
C
C            DO I = 0, N
C
C               ET = BEGIN + I*DELTA
C
C               CALL ZZSPKGO0 ( MOON, ET, 'J2000', EARTH, STATE, LT )
C
C               CALL ET2UTC ( ET, 'C', 0, UTC )
C
C               WRITE (*,*) UTC, VNORM ( STATE )
C
C            END DO
C
C$ Restrictions
C
C     1) SPICE Private routine.
C
C     2) The ephemeris files to be used by ZZSPKGO0 must be loaded
C        by SPKLEF before ZZSPKGO0 is called.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman  (JPL)
C     J.E. McLean   (JPL)
C     B.V. Semenov  (JPL)
C     W.L. Taber    (JPL)
C     W.D. Wright   (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 08-JAN-2014 (BVS)
C
C        Updated to save the input frame name and POOL state counter
C        and to do frame name-ID conversion only if the counter has
C        changed.
C
C        Updated to map the input frame name to its ID by first calling
C        ZZNAMFRM, and then calling IRFNUM. The side effect of this
C        change is that now the frame with the fixed name 'DEFAULT'
C        that can be associated with any code via CHGIRF's entry point
C        IRFDEF will be fully masked by a frame with indentical name
C        defined via a text kernel. Previously the CHGIRF's 'DEFAULT'
C        frame masked the text kernel frame with the same name.
C
C        Fixed description of STATE in Detailed Output. Replaced 
C        SPKLEF with FURNSH and fixed errors in Examples.
C
C-    SPICELIB Version 1.1.0, 06-SEP-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in VADDG calls.
C
C-    SPICELIB Version 1.0.0, 05-JAN-2005 (NJB)  
C
C        Based on SPICELIB Version 2.3.0, 05-JAN-2005 (NJB)
C
C-&
 
C$ Index_Entries
C
C     geometric state of one body relative to another
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.1.0, 06-SEP-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in VADDG calls.
C
C-& 
 
 
C
C     This is the idea:
C
C     Every body moves with respect to some center. The center
C     is itself a body, which in turn moves about some other
C     center.  If we begin at the target body (T), follow
C     the chain,
C
C                                   T
C                                     \
C           SSB                        \
C               \                     C[1]
C                \                     /
C                 \                   /
C                  \                 /
C                   \               /
C                  C[3]-----------C[2]
C
C     and avoid circular definitions (A moves about B, and B moves
C     about A), eventually we get the state relative to the solar
C     system barycenter (which, for our purposes, doesn't move).
C     Thus,
C
C        T    = T     + C[1]     + C[2]     + ... + C[n]
C         SSB    C[1]       C[2]       [C3]             SSB
C
C     where
C
C        X
C         Y
C
C     is the state of body X relative to body Y.
C
C     However, we don't want to follow each chain back to the SSB
C     if it isn't necessary.  Instead we will just follow the chain
C     of the target body and follow the chain of the observing body
C     until we find a common node in the tree.
C
C     In the example below, C is the first common node.  We compute
C     the state of TARG relative to C and the state of OBS relative
C     to C, then subtract the two states.
C
C                                   TARG
C                                     \
C           SSB                        \
C               \                       A
C                \                     /            OBS
C                 \                   /              |
C                  \                 /               |
C                   \               /                |
C                    B-------------C-----------------D
C
C
 
C
C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN
 
      DOUBLE PRECISION      CLIGHT
      DOUBLE PRECISION      VNORM
 
      INTEGER               FRSTNP
      INTEGER               ISRCHI
 
C
C     Local parameters
C
      CHARACTER*(*)         RNAME
      PARAMETER           ( RNAME  =  'ZZSPKGO0' )

C
C     CHLEN is the maximum length of a chain.  That is,
C     it is the maximum number of bodies in the chain from
C     the target or observer to the SSB.
C
      INTEGER               CHLEN
      PARAMETER           ( CHLEN = 20 )
 
      INTEGER               SSB
      PARAMETER           ( SSB   =  0 )

C
C     Saved frame name length.
C
      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

C
C     Local variables
C
      CHARACTER*(40)        IDENT
      CHARACTER*(40)        TNAME
      CHARACTER*(40)        ONAME
      CHARACTER*(80)        TSTRING
 
      DOUBLE PRECISION      DESCR  ( 5        )
      DOUBLE PRECISION      SOBS   ( 6        )
      DOUBLE PRECISION      STARG  ( 6, CHLEN )
      DOUBLE PRECISION      STEMP  ( 6        )
 
      DOUBLE PRECISION      STXFRM ( 6, 6     )
      DOUBLE PRECISION      ROT    ( 3, 3     )
      DOUBLE PRECISION      VTEMP  ( 6        )
 
      INTEGER               CFRAME
      INTEGER               COBS
      INTEGER               CTARG  (    CHLEN )
      INTEGER               TFRAME (    CHLEN )
      INTEGER               CTPOS
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               LEGS
      INTEGER               NCT
      INTEGER               REFID
      INTEGER               TMPFRM
 
      LOGICAL               FOUND
      LOGICAL               NOFRM
      LOGICAL               ISINRT

C
C     Saved frame name/ID item declarations.
C
      INTEGER               SVCTR1 ( CTRSIZ )
      CHARACTER*(FRNMLN)    SVREF
      INTEGER               SVREFI

      LOGICAL               FIRST

C
C     Saved frame name/ID items.
C
      SAVE                  SVCTR1
      SAVE                  SVREF
      SAVE                  SVREFI

      SAVE                  FIRST

C
C     Initial values.
C
      DATA                  FIRST   / .TRUE. /


C
C     In-line Function Definitions
C
      ISINRT ( TMPFRM, CFRAME ) =       CFRAME .GT. 0
     .                            .AND. CFRAME .LE. NINERT
     .                            .AND. TMPFRM .GT. 0
     .                            .AND. TMPFRM .LE. NINERT
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( RNAME )
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
 
C
C     We take care of the obvious case first.  It TARG and OBS are the
C     same we can just fill in zero.
C
      IF ( TARG .EQ. OBS ) THEN
 
         LT = 0.0D0
 
         CALL CLEARD ( 6, STATE )
         CALL CHKOUT ( RNAME )
         RETURN
 
      END IF
 
 
C
C     CTARG contains the integer codes of the bodies in the
C     target body chain, beginning with TARG itself and then
C     the successive centers of motion.
C
C     STARG(1,I) is the state of the target body relative
C     to CTARG(I).  The id-code of the frame of this state is
C     stored in TFRAME(I).
C
C     COBS and SOBS will contain the centers and states of the
C     observing body.  (They are single elements instead of arrays
C     because we only need the current center and state of the
C     observer relative to it.)
C
C     First, we construct CTARG and STARG.  CTARG(1) is
C     just the target itself, and STARG(1,1) is just a zero
C     vector, that is, the state of the target relative
C     to itself.
C
C     Then we follow the chain, filling up CTARG and STARG
C     as we go.  We use SPKSFS to search through loaded
C     files to find the first segment applicable to CTARG(1)
C     and time ET.  Then we use SPKPVN to compute the state
C     of the body CTARG(1) at ET in the segment that was found
C     and get its center and frame of motion (CTARG(2) and TFRAME(2).
C
C     We repeat the process for CTARG(2) and so on, until
C     there is no data found for some CTARG(I) or until we
C     reach the SSB.
C
C     Next, we find centers and states in a similar manner
C     for the observer.  It's a similar construction as
C     described above, but I is always 1.  COBS and SOBS
C     are overwritten with each new center and state,
C     beginning at OBS.  However, we stop when we encounter
C     a common center of motion, that is when COBS is equal
C     to CTARG(I) for some I.
C
C     Finally, we compute the desired state of the target
C     relative to the observer by subtracting the state of
C     the observing body relative to the common node from
C     the state of the target body relative to the common
C     node.
C
C     CTPOS is the position in CTARG of the common node.
C
C     Since the upgrade to use hashes and counter bypass ZZNAMFRM
C     became more efficient in looking up frame IDs than IRFNUM. So the
C     original order of calls "IRFNUM first, NAMFRM second" was
C     switched to "ZZNAMFRM first, IRFNUM second". 
C
C     The call to IRFNUM, now redundant for built-in inertial frames,
C     was preserved to for a sole reason -- to still support the
C     ancient and barely documented ability for the users to associate
C     a frame with the fixed name 'DEFAULT' with any CHGIRF inertial
C     frame code via CHGIRF's entry point IRFDEF.
C
C     Note that in the case of ZZNAMFRM's failure to resolve name and
C     IRFNUM's success to do so, the code returned by IRFNUM for
C     'DEFAULT' frame is *not* copied to the saved code SVREFI (which
C     would be set to 0 by ZZNAMFRM) to make sure that on subsequent
C     calls ZZNAMFRM does not do a bypass (as SVREFI always forced look
C     up) and calls IRFNUM again to reset the 'DEFAULT's frame ID
C     should it change between the calls.
C
      CALL ZZNAMFRM ( SVCTR1, SVREF, SVREFI, REF, REFID )
 
      IF ( REFID .EQ. 0 ) THEN
         CALL IRFNUM ( REF, REFID )
      END IF
 
      IF ( REFID .EQ. 0 ) THEN
 
         IF (  FRSTNP(REF) .GT. 0 ) THEN
 
            CALL SETMSG ( 'The string supplied to specify the '
     .      //            'reference frame, (''#'') contains '
     .      //            'non-printing characters.  The two '
     .      //            'most common causes for this kind '
     .      //            'of error are: 1. an error in the '
     .      //            'call to ZZSPKGO0; 2. an '
     .      //            'uninitialized variable. ' )
            CALL ERRCH  ( '#', REF )
 
 
         ELSE IF ( REF .EQ. ' ' ) THEN
 
            CALL SETMSG ( 'The string supplied to specify the '
     .      //            'reference frame is blank.  The most '
     .      //            'common cause for this kind of error '
     .      //            'is an uninitialized variable. ' )
 
 
         ELSE
 
            CALL SETMSG ( 'The string supplied to specify the '
     .      //            'reference frame was ''#''.  This '
     .      //            'frame is not recognized. Possible '
     .      //            'causes for this error are: 1. failure '
     .      //            'to load the frame definition into the '
     .      //            'kernel pool; 2. An out-of-date '
     .      //            'edition of the toolkit. ' )
            CALL ERRCH  ( '#', REF )
 
         END IF

         CALL SIGERR ( 'SPICE(UNKNOWNFRAME)'  )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( RNAME )
            RETURN
         END IF

      END IF
 
 
 
C
C     Fill in CTARG and STARG until no more data is found
C     or until we reach the SSB.  If the chain gets too
C     long to fit in CTARG, that is if I equals CHLEN,
C     then overwrite the last elements of CTARG and STARG.
C
C     Note the check for FAILED in the loop.  If SPKSFS
C     or SPKPVN happens to fail during execution, and the
C     current error handling action is to NOT abort, then
C     FOUND may be stuck at TRUE, CTARG(I) will never
C     become zero, and the loop will execute indefinitely.
C
C
C     Construct CTARG and STARG.  Begin by assigning the
C     first elements:  TARG and the state of TARG relative
C     to itself.
C
      I        =  1
      CTARG(I) =  TARG
      FOUND    = .TRUE.
 
      CALL CLEARD ( 6, STARG(1,I) )
 
      DO WHILE (           FOUND
     .            .AND.  ( I        .LT. CHLEN )
     .            .AND.  ( CTARG(I) .NE. OBS   )
     .            .AND.  ( CTARG(I) .NE. SSB   )  )
 
C
C        Find a file and segment that has state
C        data for CTARG(I).
C
         CALL SPKSFS ( CTARG(I), ET, HANDLE, DESCR, IDENT, FOUND )
 
         IF ( FOUND ) THEN
C
C           Get the state of CTARG(I) relative to some
C           center of motion.  This new center goes in
C           CTARG(I+1) and the state is called STEMP.
C
            I = I + 1
 
            CALL SPKPVN ( HANDLE,    DESCR,      ET,
     .                    TFRAME(I), STARG(1,I), CTARG(I) )
C
C           Here's what we have.  STARG is the state of CTARG(I-1)
C           relative to CTARG(I) in reference frame TFRAME(I)
C
C           If one of the routines above failed during
C           execution, we just give up and check out.
C
            IF ( FAILED() ) THEN
               CALL CHKOUT ( RNAME )
               RETURN
            END IF
 
         END IF
 
      END DO
 
      TFRAME(1) = TFRAME(2)
C
C     If the loop above ended because we ran out of
C     room in the arrays CTARG and STARG, then we
C     continue finding states but we overwrite the
C     last elements of CTARG and STARG.
C
C     If, as a result, the first common node is
C     overwritten, we'll just have to settle for
C     the last common node.  This will cause a small
C     loss of precision, but it's better than other
C     alternatives.
C
      IF ( I .EQ. CHLEN ) THEN
 
         DO WHILE (      FOUND
     .             .AND. CTARG(CHLEN) .NE. SSB
     .             .AND. CTARG(CHLEN) .NE. OBS )
 
 
C
C           Find a file and segment that has state
C           data for CTARG(CHLEN).
C
            CALL SPKSFS ( CTARG(CHLEN), ET, HANDLE, DESCR, IDENT, FOUND)
 
            IF ( FOUND ) THEN
C
C              Get the state of CTARG(CHLEN) relative to
C              some center of motion.  The new center
C              overwrites the old.  The state is called
C              STEMP.
C
               CALL SPKPVN ( HANDLE, DESCR, ET, TMPFRM, STEMP,
     .                       CTARG(CHLEN))
 
C
C              Add STEMP to the state of TARG relative to
C              the old center to get the state of TARG
C              relative to the new center.  Overwrite
C              the last element of STARG.
C
               IF ( TFRAME(CHLEN) .EQ. TMPFRM ) THEN
 
                  CALL MOVED ( STARG(1,CHLEN), 6, VTEMP )
 
               ELSE IF ( ISINRT ( TFRAME(CHLEN), TMPFRM ) ) THEN
 
                  CALL IRFROT( TFRAME(CHLEN), TMPFRM,         ROT      )
                  CALL MXV   ( ROT,           STARG(1,CHLEN), VTEMP(1) )
                  CALL MXV   ( ROT,           STARG(4,CHLEN), VTEMP(4) )
 
               ELSE
 
                  CALL ZZFRMCH0( TFRAME(CHLEN), TMPFRM,   ET,   STXFRM )

                  IF ( FAILED() ) THEN
                     CALL CHKOUT ( RNAME )
                     RETURN
                  END IF

                  CALL MXVG  ( STXFRM, STARG (1,CHLEN), 6, 6, VTEMP  )
 
               END IF
 
               CALL VADDG ( VTEMP,  STEMP,        6, STARG(1,CHLEN) )
 
               TFRAME(CHLEN) = TMPFRM
 
C
C              If one of the routines above failed during
C              execution, we just give up and check out.
C
               IF ( FAILED() ) THEN
                  CALL CHKOUT ( RNAME )
                  RETURN
               END IF
 
            END IF
 
         END DO
 
      END IF
 
      NCT = I
C
C     NCT is the number of elements in CTARG,
C     the chain length.  We have in hand the following information
C
C        STARG(1...6,K)  state of body
C        CTARG(K-1)      relative to body CTARG(K) in the frame
C        TFRAME(K)
C
C
C     For K = 2,..., NCT.
C
C     CTARG(1) = TARG
C     STARG(1...6,1) = ( 0, 0, 0, 0, 0, 0 )
C     TFRAME(1)      = TFRAME(2)
C
 
 
C
C     Now follow the observer's chain.  Assign
C     the first values for COBS and SOBS.
C
      COBS = OBS
      CALL CLEARD ( 6, SOBS )
 
C
C     Perhaps we have a common node already.
C     If so it will be the last node on the
C     list CTARG.
C
C     We let CTPOS will be the position of the common
C     node in CTARG if one is found.  It will
C     be zero if COBS is not found in CTARG.
C
      IF ( CTARG(NCT) .EQ. COBS ) THEN
         CTPOS  = NCT
         CFRAME = TFRAME(CTPOS)
      ELSE
         CTPOS  = 0
      END IF
 
C
C     Repeat the same loop as above, but each time
C     we encounter a new center of motion, check to
C     see if it is a common node.  (When CTPOS is
C     not zero, CTARG(CTPOS) is the first common node.)
C
C     Note that we don't need a centers array nor a
C     states array, just a single center and state
C     is sufficient --- we just keep overwriting them.
C     When the common node is found, we have everything
C     we need in that one center (COBS) and state
C     (SOBS-state of the target relative to COBS).
C
      FOUND  = .TRUE.
      NOFRM  = .TRUE.
      LEGS   =  0
 
      DO WHILE (           FOUND
     .            .AND.  ( COBS  .NE. SSB )
     .            .AND.  ( CTPOS .EQ. 0   )  )
 
C
C        Find a file and segment that has state
C        data for COBS.
C
         CALL SPKSFS ( COBS, ET, HANDLE, DESCR, IDENT, FOUND )
 
         IF ( FOUND ) THEN
C
C           Get the state of COBS; call it STEMP.
C           The center of motion of COBS becomes the
C           new COBS.
C
            IF ( LEGS .EQ. 0 ) THEN
               CALL SPKPVN ( HANDLE, DESCR, ET, TMPFRM, SOBS,  COBS )
            ELSE
               CALL SPKPVN ( HANDLE, DESCR, ET, TMPFRM, STEMP, COBS )
            END IF
 
            IF ( NOFRM ) THEN
               NOFRM  = .FALSE.
               CFRAME = TMPFRM
            END IF
C
C           Add STEMP to the state of OBS relative to
C           the old COBS to get the state of OBS
C           relative to the new COBS.
C
            IF ( CFRAME .EQ. TMPFRM ) THEN
C
C              On the first leg of the state of the observer, we
C              don't have to add anything, the state of the observer
C              is already in SOBS.  We only have to add when the
C              number of legs in the observer state is one or greater.
C
               IF ( LEGS .GT. 0 ) THEN
                  CALL VADDG ( SOBS,  STEMP, 6, VTEMP )
                  CALL MOVED ( VTEMP,        6, SOBS  )
               END IF
 
            ELSE IF ( ISINRT( CFRAME, TMPFRM ) ) THEN
 
               CALL IRFROT ( CFRAME, TMPFRM, ROT )
               CALL MXV    ( ROT,    SOBS(1),   VTEMP(1)  )
               CALL MXV    ( ROT,    SOBS(4),   VTEMP(4)  )
               CALL VADDG  ( VTEMP,  STEMP,     6, SOBS   )
               CFRAME = TMPFRM
 
            ELSE
 
               CALL ZZFRMCH0 ( CFRAME, TMPFRM, ET,   STXFRM )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( RNAME )
                  RETURN
               END IF

               CALL MXVG   ( STXFRM, SOBS,   6, 6, VTEMP  )
               CALL VADDG  ( VTEMP,  STEMP,     6, SOBS   )
               CFRAME = TMPFRM
 
            END IF
 
C
C           Check failed.  We don't want to loop
C           indefinitely.
C
            IF ( FAILED() ) THEN
               CALL CHKOUT ( RNAME )
               RETURN
            END IF
 
C
C           We now have one more leg of the path for OBS.  Set
C           LEGS to reflect this.  Then see if the new center
C           is a common node. If not, repeat the loop.
C
            LEGS  = LEGS + 1
            CTPOS = ISRCHI ( COBS, NCT, CTARG )
 
         END IF
 
      END DO
 
 
C
C     If CTPOS is zero at this point, it means we
C     have not found a common node though we have
C     searched through all the available data.
C
      IF ( CTPOS .EQ. 0 ) THEN
 
         CALL BODC2N ( TARG, TNAME,   FOUND )
 
         IF ( FOUND ) THEN
            CALL PREFIX ( '# (',  0,        TNAME )
            CALL SUFFIX ( ')',    0,        TNAME )
            CALL REPMI  ( TNAME, '#', TARG, TNAME )
         ELSE
            CALL INTSTR ( TARG, TNAME )
         END IF
 
 
         CALL BODC2N ( OBS,  ONAME,   FOUND )
 
         IF ( FOUND ) THEN
            CALL PREFIX ( '# (',  0,       ONAME )
            CALL SUFFIX ( ')',    0,       ONAME )
            CALL REPMI  ( ONAME, '#', OBS, ONAME )
         ELSE
            CALL INTSTR ( OBS, ONAME )
         END IF
 
         CALL SETMSG ( 'Insufficient ephemeris data has been '
     .   //            'loaded to compute the state of TARG '
     .   //            'relative to OBS at the ephemeris epoch '
     .   //            '#. ' )
 
         CALL ETCAL  (  ET,    TSTRING                                 )
         CALL ERRCH  ( 'TARG', TNAME                                   )
         CALL ERRCH  ( 'OBS',  ONAME                                   )
         CALL ERRCH  ( '#',    TSTRING                                 )
         CALL SIGERR ( 'SPICE(SPKINSUFFDATA)'                          )
         CALL CHKOUT ( RNAME                                           )
         RETURN
 
      END IF
 
C
C     If CTPOS is not zero, then we have reached a
C     common node, specifically,
C
C        CTARG(CTPOS) = COBS = CENTER
C
C     (in diagram below).  The STATE of the target
C     (TARG) relative to the observer (OBS) is just
C
C        STARG(1,CTPOS) - SOBS.
C
C
C
C                     SOBS
C         CENTER ---------------->OBS
C            |                  .
C            |                .
C         S  |              .   E
C         T  |            .   T
C         A  |          .   A
C         R  |        .   T
C         G  |      .   S
C            |    .
C            |  .
C            V L
C           TARG
C
C
C     And the light-time between them is just
C
C               | STATE |
C          LT = ---------
C                   c
C
C
C     Compute the state of the target relative to CTARG(CTPOS)
C
      IF ( CTPOS .EQ. 1 ) THEN
         TFRAME(1) = CFRAME
      END IF
 
      DO I = 2, CTPOS-1
 
         IF ( TFRAME(I) .EQ. TFRAME(I+1) ) THEN
 
            CALL VADDG  ( STARG(1,I), STARG(1,I+1), 6, VTEMP        )
            CALL MOVED  ( VTEMP,                    6, STARG(1,I+1) )
 
         ELSE IF ( ISINRT ( TFRAME(I), TFRAME(I+1) ) ) THEN
 
            CALL IRFROT ( TFRAME(I),  TFRAME(I+1),     ROT          )
            CALL MXV    ( ROT,        STARG(1,I),      STEMP(1)     )
            CALL MXV    ( ROT,        STARG(4,I),      STEMP(4)     )
            CALL VADDG  ( STEMP,      STARG(1,I+1), 6, VTEMP        )
            CALL MOVED  ( VTEMP,                    6, STARG(1,I+1) )

         ELSE
 
            CALL ZZFRMCH0 ( TFRAME(I),  TFRAME(I+1), ET,  STXFRM       )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( RNAME )
               RETURN
            END IF

            CALL MXVG   ( STXFRM,     STARG(1,I),  6,6, STEMP        )
            CALL VADDG  ( STEMP,      STARG(1,I+1),  6, VTEMP        )
            CALL MOVED  ( VTEMP,                     6, STARG(1,I+1) )
 
         END IF
 
      END DO
 
 
C
C     To avoid unnecessary frame transformations we'll do
C     a bit of extra decision making here.  It's a lot
C     faster to make logical checks than it is to compute
C     frame transformations.
C
      IF ( TFRAME(CTPOS) .EQ. CFRAME ) THEN
 
         CALL VSUBG  ( STARG(1,CTPOS), SOBS, 6,   STATE  )
 
      ELSE IF ( TFRAME(CTPOS) .EQ. REFID ) THEN
 
C
C        If the last frame associated with the target is already
C        in the requested output frame, we convert the state of
C        the observer to that frame and then subtract the state
C        of the observer from the state of the target.
C
         IF ( ISINRT( CFRAME, REFID ) ) THEN
 
            CALL IRFROT ( CFRAME, REFID,   ROT      )
            CALL MXV    ( ROT,    SOBS(1), STEMP(1) )
            CALL MXV    ( ROT,    SOBS(4), STEMP(4) )
 
         ELSE
 
            CALL ZZFRMCH0 ( CFRAME, REFID, ET,  STXFRM )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( RNAME )
               RETURN
            END IF

            CALL MXVG   ( STXFRM, SOBS,  6,6, STEMP  )
 
         END IF
 
C
C        We've now transformed SOBS into the requested reference frame.
C        Set CFRAME to reflect this.
C
         CFRAME = REFID
         CALL VSUBG ( STARG(1,CTPOS),  STEMP, 6, STATE )
 
 
      ELSE IF ( ISINRT( TFRAME(CTPOS), CFRAME ) ) THEN
 
C
C        If both frames are inertial we use IRFROT instead of
C        ZZFRMCH0 to get things into a common frame.
C
         CALL IRFROT ( TFRAME(CTPOS),  CFRAME,               ROT      )
         CALL MXV    ( ROT,            STARG(1,CTPOS),       STEMP(1) )
         CALL MXV    ( ROT,            STARG(4,CTPOS),       STEMP(4) )
         CALL VSUBG  ( STEMP,          SOBS,            6,   STATE    )
 
      ELSE
C
C        Use the more general routine ZZFRMCH0 to make the
C        transformation.
C
         CALL ZZFRMCH0 ( TFRAME(CTPOS),  CFRAME,          ET,  STXFRM )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( RNAME )
            RETURN
         END IF

         CALL MXVG   ( STXFRM,         STARG(1,CTPOS),  6,6, STEMP  )
         CALL VSUBG  ( STEMP,          SOBS,            6,   STATE  )
 
      END IF
 
C
C     Finally, rotate as needed into the requested frame.
C
      IF ( CFRAME .EQ. REFID ) THEN
 
C
C        We don't have to do anything in this case.
C
 
      ELSE IF ( ISINRT ( CFRAME, REFID ) ) THEN
C
C        Since both frames are inertial, we use the more direct
C        routine IRFROT to get the transformation to REFID.
C
         CALL IRFROT ( CFRAME,  REFID,      ROT      )
         CALL MXV    ( ROT,     STATE(1),   STEMP(1) )
         CALL MXV    ( ROT,     STATE(4),   STEMP(4) )
         CALL MOVED  ( STEMP,   6,          STATE    )
 
      ELSE
 
         CALL ZZFRMCH0 ( CFRAME,  REFID, ET,  STXFRM )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( RNAME )
            RETURN
         END IF
 
         CALL MXVG   ( STXFRM,  STATE, 6, 6, STEMP )
         CALL MOVED  ( STEMP,          6,    STATE )
 
      END IF
 
      LT = VNORM  ( STATE ) / CLIGHT()
 
      CALL CHKOUT ( RNAME )
      RETURN
      END

C$Procedure      SPKW15 ( SPK, write a type 15 segment )
 
      SUBROUTINE SPKW15 ( HANDLE, BODY,  CENTER, FRAME,  FIRST, LAST,
     .                    SEGID,  EPOCH, TP,     PA,     P,     ECC,
     .                    J2FLG,  PV,    GM,     J2,     RADIUS      )
 
C$ Abstract
C
C     Write an SPK segment of type 15 given a type 15 data record.
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
 
      INTEGER               HANDLE
      INTEGER               BODY
      INTEGER               CENTER
      CHARACTER*(*)         FRAME
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      CHARACTER*(*)         SEGID
      DOUBLE PRECISION      EPOCH
      DOUBLE PRECISION      TP     ( 3 )
      DOUBLE PRECISION      PA     ( 3 )
      DOUBLE PRECISION      P
      DOUBLE PRECISION      ECC
      DOUBLE PRECISION      J2FLG
      DOUBLE PRECISION      PV     ( 3 )
      DOUBLE PRECISION      GM
      DOUBLE PRECISION      J2
      DOUBLE PRECISION      RADIUS
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of an SPK file open for writing.
C     BODY       I   Body code for ephemeris object.
C     CENTER     I   Body code for the center of motion of the body.
C     FRAME      I   The reference frame of the states.
C     FIRST      I   First valid time for which states can be computed.
C     LAST       I   Last valid time for which states can be computed.
C     SEGID      I   Segment identifier.
C     EPOCH      I   Epoch of the periapse.
C     TP         I   Trajectory pole vector.
C     PA         I   Periapsis vector.
C     P          I   Semi-latus rectum.
C     ECC        I   Eccentricity.
C     J2FLG      I   J2 processing flag.
C     PV         I   Central body pole vector.
C     GM         I   Central body GM.
C     J2         I   Central body J2.
C     RADIUS     I   Equatorial radius of central body.
C
C$ Detailed_Input
C
C     HANDLE      is the file handle of an SPK file that has been
C                 opened for writing.
C
C     BODY        is the NAIF ID for the body whose states are
C                 to be recorded in an SPK file.
C
C     CENTER      is the NAIF ID for the center of motion associated
C                 with BODY.
C
C     FRAME       is the reference frame that states are referenced to,
C                 for example 'J2000'.
C
C     FIRST       are the bounds on the ephemeris times, expressed as
C     LAST        seconds past J2000.
C
C     SEGID       is the segment identifier. An SPK segment identifier
C                 may contain up to 40 characters.
C
C     EPOCH       is the epoch of the orbit elements at periapse
C                 in ephemeris seconds past J2000.
C
C     TP          is a vector parallel to the angular momentum vector
C                 of the orbit at epoch expressed relative to FRAME. A
C                 unit vector parallel to TP will be stored in the
C                 output segment.
C
C     PA          is a vector parallel to the position vector of the
C                 trajectory at periapsis of EPOCH expressed relative
C                 to FRAME. A unit vector parallel to PA will be
C                 stored in the output segment.
C
C     P           is the semi-latus rectum--- p in the equation:
C
C                    r = p/(1 + ECC*COS(Nu))
C
C     ECC          is the eccentricity.
C
C     J2FLG        is the J2 processing flag describing what J2
C                  corrections are to be applied when the orbit is
C                  propagated.
C
C                  All J2 corrections are applied if the value of J2FLG
C                  is not 1, 2 or 3.
C
C                  If the value of the flag is 3 no corrections are
C                  done.
C
C                  If the value of the flag is 1 no corrections are
C                  computed for the precession of the line of apsides.
C                  However, regression of the line of nodes is
C                  performed.
C
C                  If the value of the flag is 2 no corrections are
C                  done for the regression of the line of nodes.
C                  However, precession of the line of apsides is
C                  performed.
C
C                  Note that J2 effects are computed only if the orbit
C                  is elliptic and does not intersect the central body.
C
C     PV           is a vector parallel to the north pole vector of the
C                  central body expressed relative to FRAME. A unit
C                  vector parallel to PV will be stored in the output
C                  segment.
C
C     GM           is the central body GM.
C
C     J2           is the central body J2 (dimensionless).
C
C     RADIUS       is the equatorial radius of the central body.
C
C     Units are radians, km, seconds.
C
C$ Detailed_Output
C
C     None.  A type 15 segment is written to the file attached
C     to HANDLE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the eccentricity is less than zero, the error
C        'SPICE(BADECCENTRICITY)' will be signaled.
C
C     2) If the semi-latus rectum is 0, the error
C        'SPICE(BADLATUSRECTUM)' is signaled.
C
C     3) If the pole vector, trajectory pole vector or periapsis vector
C        have zero length, the error 'SPICE(BADVECTOR)' is signaled.
C
C     4) If the trajectory pole vector and the periapsis vector are
C        not orthogonal, the error 'SPICE(BADINITSTATE)' is signaled.
C        The test for orthogonality is very crude.  The routine simply
C        checks that the dot product of the unit vectors parallel
C        to the trajectory pole and periapse vectors is less than
C        0.00001.  This check is intended to catch blunders, not to
C        enforce orthogonality to double precision capacity.
C
C     5) If the mass of the central body is non-positive, the error
C       'SPICE(NONPOSITIVEMASS)' is signaled.
C
C     6) If the radius of the central body is negative, the error
C       'SPICE(BADRADIUS)' is signaled.
C
C     7) If the segment identifier has more than 40 non-blank characters
C        the error 'SPICE(SEGIDTOOLONG)' is signaled.
C
C     8) If the segment identifier contains non-printing characters
C        the error 'SPICE(NONPRINTABLECHARS)' is signaled.
C
C     9) If there are inconsistencies in the BODY, CENTER, FRAME or
C        FIRST and LAST times, the problem will be diagnosed by
C        a routine in the call tree of this routine.
C
C$ Files
C
C     A new type 15 SPK segment is written to the SPK file attached
C     to HANDLE.
C
C$ Particulars
C
C     This routine writes an SPK type 15 data segment to the open SPK
C     file according to the format described in the type 15 section of
C     the SPK Required Reading. The SPK file must have been opened with
C     write access.
C
C     This routine is provided to provide direct support for the MASL
C     precessing orbit formulation.
C
C$ Examples
C
C     Suppose that at time EPOCH you have the J2000 periapsis
C     state of some object relative to some central body and would
C     like to create a type 15 SPK segment to model the motion of
C     the object using simple regression and precession of the
C     line of nodes and apsides. The following code fragment
C     illustrates how you can prepare such a segment.  We shall
C     assume that you have in hand the J2000 direction of the
C     central body's pole vector, its GM, J2 and equatorial
C     radius.  In addition we assume that you have opened an SPK
C     file for write access and that it is attached to HANDLE.
C
C    (If your state is at an epoch other than periapse the
C     fragment below will NOT produce a "correct" type 15 segment
C     for modeling the motion of your object.)
C
C     C
C     C     First we get the osculating elements.
C     C
C           CALL OSCELT ( STATE, EPOCH, GM, ELTS )
C
C     C
C     C     From these collect the eccentricity and semi-latus rectum.
C     C
C           ECC = ELTS ( 2 )
C           P   = ELTS ( 1 ) * ( 1.0D0 + ECC )
C     C
C     C     Next get the trajectory pole vector and the
C     C     periapsis vector.
C     C
C           CALL UCRSS ( STATE(1), STATE(4), TP )
C           CALL VHAT  ( STATE(1),           PA )
C
C     C
C     C     Enable both J2 corrections.
C     C
C
C          J2FLG = 0.0D0
C
C     C
C     C     Now add the segment.
C     C
C
C           CALL SPKW15 ( HANDLE, BODY,  CENTER, FRAME,  FIRST, LAST,
C           .              SEGID,  EPOCH, TP,     PA,    P,     ECC,
C           .              J2FLG,  PV,    GM,     J2,    RADIUS      )
C
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
C     N.J. Bachman    (JPL)
C     W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 29-MAY-2012 (NJB)
C
C        Input vectors that nominally have unit length
C        are mapped to local copies that actually do
C        have unit length. The applicable inputs are TP, PA,
C        and PV. The Detailed Input header section was updated
C        to reflect the change.
C
C        Some typos in error messages were corrected.
C
C-    SPICELIB Version 1.0.0, 28-NOV-1994 (WLT)
C
C-&
 
C$ Index_Entries
C
C     Write a type 15 spk segment
C
C-&
 
C
C     SPICELIB Functions
C
      DOUBLE PRECISION      DPR
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VSEP
 
      INTEGER               LASTNB
 
      LOGICAL               FAILED
      LOGICAL               RETURN
      LOGICAL               VZERO
 
C
C     Local Variables
C
C
C     Segment descriptor size
C
      INTEGER               NS
      PARAMETER           ( NS      =   5 )
 
C
C     Segment identifier size
C
      INTEGER               SIDLEN
      PARAMETER           ( SIDLEN  =  40 )
 
C
C     SPK data type
C
      INTEGER               TYPE
      PARAMETER           ( TYPE    = 15 )
 
C
C     Range of printing characters
C
      INTEGER               FPRINT
      PARAMETER           ( FPRINT  =  32 )
 
      INTEGER               LPRINT
      PARAMETER           ( LPRINT  = 126 )
 
C
C     Number of items in a segment
C
      INTEGER               DATSIZ
      PARAMETER           ( DATSIZ  = 16 )
 
 
      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      DOT
      DOUBLE PRECISION      DESCR ( NS )
      DOUBLE PRECISION      MYTP  ( 3 )
      DOUBLE PRECISION      MYPA  ( 3 )
 
      DOUBLE PRECISION      RECORD ( DATSIZ )
 
      INTEGER               I
      INTEGER               VALUE
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'SPKW15')
 
C
C     Fetch the various entities from the inputs and put them into
C     the data record, first the epoch.
C
      RECORD(1) = EPOCH
 
 
C
C     Convert TP and PA to unit vectors.
C
      CALL VHAT ( PA, MYPA )
      CALL VHAT ( TP, MYTP )

C
C     The trajectory pole vector.
C
      CALL VEQU ( MYTP, RECORD(2) )
 
C
C     The periapsis vector.
C
      CALL VEQU ( MYPA, RECORD(5) )
 
C
C     Semi-latus rectum ( P in the P/(1 + ECC*COS(Nu)  ),
C     and eccentricity.
C
      RECORD(8) = P
      RECORD(9) = ECC
 
C
C     J2 processing flag.
C
      RECORD(10) = J2FLG
 
C
C     Central body pole vector.
C
      CALL VHAT ( PV, RECORD(11) )
 
C
C     The central mass, J2 and radius of the central body.
C
      RECORD(14) = GM
      RECORD(15) = J2
      RECORD(16) = RADIUS
 
C
C     Check all the inputs here for obvious failures.  It's much
C     better to check them now and quit than it is to get a bogus
C     segment into an SPK file and diagnose it later.
C
 
      IF ( P .LE. 0 ) THEN
 
         CALL SETMSG ( 'The semi-latus rectum supplied to the '
     .   //            'SPK type 15 evaluator was non-positive.  This '
     .   //            'value must be positive. The value supplied was '
     .   //            '#.'                    )
         CALL ERRDP  ( '#', P                  )
         CALL SIGERR ( 'SPICE(BADLATUSRECTUM)' )
         CALL CHKOUT ( 'SPKW15'                )
         RETURN
 
      ELSE IF ( ECC .LT. 0.0D0 ) THEN
 
         CALL SETMSG ( 'The eccentricity supplied for a type 15 '
     .   //            'segment is negative.  It must be non-negative. '
     .   //            'The value supplied '
     .   //            'to the type 15 evaluator was #. ' )
         CALL ERRDP  ( '#',   ECC                )
         CALL SIGERR ( 'SPICE(BADECCENTRICITY)'  )
         CALL CHKOUT ( 'SPKW15'                  )
         RETURN
 
      ELSE IF ( GM  .LE. 0.0D0 ) THEN
 
         CALL SETMSG ( 'The mass supplied for the central body '
     .   //            'of a type 15 segment was non-positive. '
     .   //            'Masses must be positive.  The value '
     .   //            'supplied was #. '        )
         CALL ERRDP  ( '#', GM                   )
         CALL SIGERR ( 'SPICE(NONPOSITIVEMASS)'  )
         CALL CHKOUT ( 'SPKW15'                  )
         RETURN
 
      ELSE IF ( VZERO(TP) ) THEN
 
         CALL SETMSG ( 'The trajectory pole vector supplied to '
     .   //            'SPKW15 had length zero. The most likely '
     .   //            'cause of this problem is an uninitialized '
     .   //            'vector.'            )
         CALL SIGERR ( 'SPICE(BADVECTOR)'   )
         CALL CHKOUT ( 'SPKW15'             )
         RETURN
 
      ELSE IF ( VZERO(PA) ) THEN
 
         CALL SETMSG ( 'The periapse vector supplied to SPKW15 '
     .   //            'had length zero. The most likely cause '
     .   //            'of this problem is an uninitialized vector.' )
         CALL SIGERR ( 'SPICE(BADVECTOR)'   )
         CALL CHKOUT ( 'SPKW15'             )
         RETURN
 
      ELSE IF ( VZERO(PV) ) THEN
 
         CALL SETMSG ( 'The central pole vector supplied to '
     .   //            'SPKW15 had length zero. The most likely '
     .   //            'cause of this problem is an uninitialized '
     .   //            'vector. '           )
         CALL SIGERR ( 'SPICE(BADVECTOR)'   )
         CALL CHKOUT ( 'SPKW15'             )
         RETURN
 
 
      ELSE IF ( RADIUS .LT. 0.0D0 ) THEN
 
         CALL SETMSG ( 'The central body radius was negative. '
     .   //            'It must be zero or positive.  The value '
     .   //            'supplied was #. '  )
         CALL ERRDP  ( '#', RADIUS         )
         CALL SIGERR ( 'SPICE(BADRADIUS)'  )
         CALL CHKOUT ( 'SPKW15'            )
         RETURN
 
      END IF
 
C
C     One final check.  Make sure the pole and periapse vectors are
C     orthogonal. (We will use a very crude check but this should
C     rule out any obvious errors.)
C
      DOT = VDOT ( MYPA, MYTP )
 
      IF ( ABS(DOT) .GT. 1.0D-5 ) THEN
 
         ANGLE = VSEP ( PA, TP ) * DPR()
 
         CALL SETMSG ( 'The periapsis and trajectory pole '
     .   //            'vectors are not orthogonal. The angle '
     .   //            'between them is # degrees. '         )
         CALL ERRDP  ( '#',     ANGLE        )
         CALL SIGERR ( 'SPICE(BADINITSTATE)' )
         CALL CHKOUT ( 'SPKW15'              )
         RETURN
 
      END IF
C
C     Make sure the segment identifier is not too long.
C
      IF ( LASTNB(SEGID) .GT. SIDLEN ) THEN
 
         CALL SETMSG ( 'Segment identifier contains more than '
     .   //            '40 characters.'                          )
         CALL SIGERR ( 'SPICE(SEGIDTOOLONG)'                     )
         CALL CHKOUT ( 'SPKW15'                                  )
         RETURN
 
      END IF
C
C     Make sure it has only printing characters.
C
      DO I = 1, LASTNB(SEGID)
 
         VALUE = ICHAR( SEGID(I:I) )
 
         IF ( ( VALUE .LT. FPRINT ) .OR. ( VALUE .GT. LPRINT ) ) THEN
 
            CALL SETMSG ( 'The segment identifier contains '
     .      //            'the nonprintable character having ascii '
     .      //            'code #.'                               )
            CALL ERRINT ( '#',   VALUE                            )
            CALL SIGERR ( 'SPICE(NONPRINTABLECHARS)'              )
            CALL CHKOUT ( 'SPKW15'                                )
            RETURN
 
         END IF
 
      END DO
C
C     All of the obvious checks have been performed on the input
C     record.  Create the segment descriptor. (FIRST and LAST are
C     checked by SPKPDS as well as consistency between BODY and CENTER).
C
      CALL SPKPDS ( BODY,  CENTER, FRAME, TYPE, FIRST, LAST,
     .              DESCR        )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SPKW15' )
         RETURN
      END IF
 
C
C     Begin a new segment.
C
      CALL DAFBNA ( HANDLE, DESCR, SEGID )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SPKW15' )
         RETURN
      END IF
 
      CALL DAFADA ( RECORD, DATSIZ )
 
      IF ( .NOT. FAILED() ) THEN
         CALL DAFENA
      END IF
 
      CALL CHKOUT ( 'SPKW15' )
      RETURN
      END
 
 
 

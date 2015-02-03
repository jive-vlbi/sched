C$Procedure      SPKW17 ( SPK, write a type 17 segment )
 
      SUBROUTINE SPKW17 ( HANDLE, BODY,  CENTER, FRAME, FIRST, LAST,
     .                    SEGID,  EPOCH, EQEL,   RAPOL, DECPOL  )
 
C$ Abstract
C
C     Write an SPK segment of type 17 given a type 17 data record.
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
      DOUBLE PRECISION      EQEL  ( 9 )
      DOUBLE PRECISION      RAPOL
      DOUBLE PRECISION      DECPOL
 
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
C     EPOCH      I   Epoch of elements in seconds past J2000
C     EQEL       I   Array of equinoctial elements
C     RAPOL      I   Right Ascension of the pole of the reference plane
C     DECPOL     I   Declination of the pole of the reference plane
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
C     EPOCH      is the epoch of equinoctial elements in seconds
C                past the J2000 epoch.
C
C     EQEL       is an array of 9 double precision numbers that
C                are the equinoctial elements for some orbit relative
C                to the equatorial frame of a central body.
C
C                ( The z-axis of the equatorial frame is the direction
C                  of the pole of the central body relative to FRAME.
C                  The x-axis is given by the cross product of the
C                  Z-axis of FRAME with the direction of the pole of
C                  the central body.  The Y-axis completes a right
C                  handed frame. )
C
C                The specific arrangement of the elements is spelled
C                out below.  The following terms are used in the
C                discussion of elements of EQEL
C
C                    INC  --- inclination of the orbit
C                    ARGP --- argument of periapse
C                    NODE --- longitude of the ascending node
C                    E    --- eccentricity of the orbit
C
C                EQEL(1) is the semi-major axis (A) of the orbit in km.
C
C                EQEL(2) is the value of H at the specified epoch.
C                        ( E*SIN(ARGP+NODE) ).
C
C                EQEL(3) is the value of K at the specified epoch
C                        ( E*COS(ARGP+NODE) ).
C
C                EQEL(4) is the mean longitude (MEAN0+ARGP+NODE)at
C                        the epoch of the elements measured in radians.
C
C                EQEL(5) is the value of P (TAN(INC/2)*SIN(NODE))at
C                        the specified epoch.
C
C                EQEL(6) is the value of Q (TAN(INC/2)*COS(NODE))at
C                        the specified epoch.
C
C                EQEL(7) is the rate of the longitude of periapse
C                        (dARGP/dt + dNODE/dt ) at the epoch of
C                        the elements.  This rate is assumed to hold
C                        for all time. The rate is measured in
C                        radians per second.
C
C                EQEL(8) is the derivative of the mean longitude
C                        ( dM/dt + dARGP/dt + dNODE/dt ).  This
C                        rate is assumed to be constant and is
C                        measured in radians/second.
C
C                EQEL(9) is the rate of the longitude of the ascending
C                        node ( dNODE/dt).  This rate is measured
C                        in radians per second.
C
C     RAPOL      Right Ascension of the pole of the reference plane
C                relative to FRAME measured in radians.
C
C     DECPOL     Declination of the pole of the reference plane
C                relative to FRAME measured in radians.
C
C$ Detailed_Output
C
C     None.  A type 17 segment is written to the file attached
C     to HANDLE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the semi-major axis is less than or equal to zero, the error
C        'SPICE(BADSEMIAXIS)' is signalled.
C
C     2) If the eccentricity of the orbit corresponding to the values
C        of H and K ( EQEL(2) and EQEL(3) ) is greater than 0.9 the
C        error 'SPICE(ECCOUTOFRANGE)' is signalled.
C
C     3) If the segment identifier has more than 40 non-blank characters
C        the error 'SPICE(SEGIDTOOLONG)' is signalled.
C
C     4) If the segment identifier contains non-printing characters
C        the error 'SPICE(NONPRINTABLECHARS)' is signalled.
C
C     5) If there are inconsistencies in the BODY, CENTER, FRAME or
C        FIRST and LAST times, the problem will be diagnosed by
C        a routine in the call tree of this routine.
C
C$ Files
C
C     A new type 17 SPK segment is written to the SPK file attached
C     to HANDLE.
C
C$ Particulars
C
C     This routine writes an SPK type 17 data segment to the open SPK
C     file according to the format described in the type 17 section of
C     the SPK Required Reading. The SPK file must have been opened with
C     write access.
C
C$ Examples
C
C     Suppose that at time EPOCH you have the classical elements
C     of some BODY relative to the equatorial frame of some central
C     body CENTER. These can be converted to equinoctial elements
C     and stored in an SPK file as a type 17 segment so that this
C     body can be used within the SPK subsystem of the SPICE system.
C
C     Below is a list of the variables used to represent the
C     classical elements
C
C           Variable     Meaning
C           --------     ----------------------------------
C           A            Semi-major axis in km
C           ECC          Eccentricity of orbit
C           INC          Inclination of orbit
C           NODE         Longitude of the ascending node at epoch
C           OMEGA        Argument of periapse at epoch
C           M            Mean anomaly at epoch
C           DMDT         Mean anomaly rate in radians/second
C           DNODE        Rate of change of longitude of ascending node
C                        in radians/second
C           DOMEGA       Rate of change of argument of periapse in
C                        radians/second
C           EPOCH        is the epoch of the elements in seconds past
C                        the J2000 epoch.
C
C
C        These elements are converted to equinoctial elements (in
C        the order compatible with type 17) as shown below.
C
C           EQEL(1) = A
C           EQEL(2) = ECC * DSIN ( OMEGA + NODE )
C           EQEL(3) = ECC * DCOS ( OMEGA + NODE )
C
C           EQEL(4) = M + OMEGA + NODE
C
C           EQEL(5) = TAN(INC/2.0D0) * DSIN(NODE)
C           EQEL(6) = TAN(INC/2.0D0) * DCOS(NODE)
C
C           EQEL(7) = DOMEGA
C           EQEL(8) = DOMEGA + DMDT + DNODE
C           EQEL(9) = DNODE
C
C
C     C
C     C     Now add the segment.
C     C
C
C           CALL SPKW17 ( HANDLE, BODY,  CENTER, FRAME,  FIRST, LAST,
C          .              SEGID,  EPOCH, EQEL,   RAPOL,  DECPOL )
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
C     W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 24-Jun-1999 (WLT)
C
C        Corrected typographical errors in the header.
C
C-    SPICELIB Version 1.0.0, 8-Jan-1997 (WLT)
C
C-&
 
C$ Index_Entries
C
C     Write a type 17 spk segment
C
C-&
 
C
C     SPICELIB Functions
C
 
      INTEGER               LASTNB
 
      LOGICAL               FAILED
      LOGICAL               RETURN
 
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
      PARAMETER           ( TYPE    = 17 )
 
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
      PARAMETER           ( DATSIZ  = 12 )
 
 
 
      DOUBLE PRECISION      A
      DOUBLE PRECISION      DESCR  ( NS )
      DOUBLE PRECISION      H
      DOUBLE PRECISION      K
      DOUBLE PRECISION      ECC
      DOUBLE PRECISION      RECORD ( DATSIZ )
 
      INTEGER               I
      INTEGER               VALUE
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'SPKW17')
 
C
C     Fetch the various entities from the inputs and put them into
C     the data record, first the epoch.
C
      RECORD(1) = EPOCH
 
C
C     The trajectory pole vector.
C
      CALL MOVED ( EQEL, 9, RECORD(2) )
 
 
      RECORD(11) = RAPOL
      RECORD(12) = DECPOL
 
 
      A      = RECORD(2)
      H      = RECORD(3)
      K      = RECORD(4)
      ECC    = DSQRT( H*H + K*K )
 
C
C     Check all the inputs here for obvious failures.  It's much
C     better to check them now and quit than it is to get a bogus
C     segment into an SPK file and diagnose it later.
C
 
      IF ( A .LE. 0 ) THEN
 
         CALL SETMSG ( 'The semimajor axis supplied to the '
     .   //            'SPK type 17 evaluator was non-positive.  This '
     .   //            'value must be positive. The value supplied was '
     .   //            '#.' )
         CALL ERRDP  ( '#', A                  )
         CALL SIGERR ( 'SPICE(BADSEMIAXIS)' )
         CALL CHKOUT ( 'SPKW17'                )
         RETURN
 
      ELSE IF ( ECC .GT. 0.9D0 ) THEN
 
         CALL SETMSG ( 'The eccentricity supplied for a type 17 '
     .   //            'segment is greater than 0.9.  It must be '
     .   //            'less than 0.9.'
     .   //            'The value supplied '
     .   //            'to the type 17 evaluator was #. ' )
         CALL ERRDP  ( '#',   ECC                )
         CALL SIGERR ( 'SPICE(BADECCENTRICITY)'  )
         CALL CHKOUT ( 'SPKW17'                  )
         RETURN
 
      END IF
 
C
C     Make sure the segment identifier is not too long.
C
      IF ( LASTNB(SEGID) .GT. SIDLEN ) THEN
 
         CALL SETMSG ( 'Segment identifier contains more than '
     .   //            '40 characters.'                          )
         CALL SIGERR ( 'SPICE(SEGIDTOOLONG)'                     )
         CALL CHKOUT ( 'SPKW17'                                  )
         RETURN
 
      END IF
C
C     Make sure the segment identifier has only printing characters.
C
      DO I = 1, LASTNB(SEGID)
 
         VALUE = ICHAR( SEGID(I:I) )
 
         IF ( ( VALUE .LT. FPRINT ) .OR. ( VALUE .GT. LPRINT ) ) THEN
 
            CALL SETMSG ( 'The segment identifier contains '
     .      //            'the nonprintable character having ascii '
     .      //            'code #.'                               )
            CALL ERRINT ( '#',   VALUE                            )
            CALL SIGERR ( 'SPICE(NONPRINTABLECHARS)'              )
            CALL CHKOUT ( 'SPKW17'                                )
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
         CALL CHKOUT ( 'SPKW17' )
         RETURN
      END IF
 
C
C     Begin a new segment.
C
      CALL DAFBNA ( HANDLE, DESCR, SEGID )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SPKW17' )
         RETURN
      END IF
 
      CALL DAFADA ( RECORD, DATSIZ )
 
      IF ( .NOT. FAILED() ) THEN
         CALL DAFENA
      END IF
 
      CALL CHKOUT ( 'SPKW17' )
      RETURN
      END

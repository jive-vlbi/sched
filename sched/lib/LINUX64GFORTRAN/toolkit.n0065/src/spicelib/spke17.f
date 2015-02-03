C$Procedure      SPKE17 ( Evaluate a type 17 SPK data record)
 
      SUBROUTINE SPKE17 ( ET, RECIN, STATE )
 
C$ Abstract
C
C     Evaluates a single SPK data record from a segment of type 17
C    (Equinoctial Elements).
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
 
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      RECIN  ( * )
      DOUBLE PRECISION      STATE  ( 6 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ET         I   Target epoch.
C     RECIN      I   Data record.
C     STATE      O   State (position and velocity).
C
C$ Detailed_Input
C
C     ET          is a target epoch, specified as ephemeris seconds past
C                 J2000, at which a state vector is to be computed.
C
C     RECIN       is a data record which, when evaluated at epoch ET,
C                 will give the state (position and velocity) of some
C                 body, relative to some center, in some inertial
C                 reference frame.
C
C                 The structure of RECIN is:
C
C                 RECIN (1)  epoch of the elements in ephemeris seconds
C                            past J2000.
C
C                 RECIN (2)-RECIN (10) Equinoctial Elements:
C
C
C                 RECIN (2)  is the semi-major axis (A) of the orbit.
C
C                 RECIN (3)  is the value of H at the specified epoch.
C                            ( E*SIN(ARGP+NODE) ).
C
C                 RECIN (4)  is the value of K at the specified epoch
C                            ( E*COS(ARGP+NODE) ).
C
C                 RECIN (5)  is the mean longitude (MEAN0+ARGP+NODE)at
C                            the epoch of the elements.
C
C                 RECIN (6)  is the value of P (TAN(INC/2)*SIN(NODE))at
C                            the specified epoch.
C
C                 RECIN (7)  is the value of Q (TAN(INC/2)*COS(NODE))at
C                            the specified epoch.
C
C                 RECIN (8)  is the rate of the longitude of periapse
C                            (dARGP/dt + dNODE/dt ) at the epoch of
C                            the elements.  This rate is assumed to hold
C                            for all time.
C
C                 RECIN (9)  is the derivative of the mean longitude
C                            ( dM/dt + dARGP/dt + dNODE/dt ).  This
C                            rate is assumed to be constant.
C
C                 RECIN (10)  is the rate of the longitude of the
C                             ascending node ( dNODE/dt).
C
C                 RECIN (11) Right Ascension of the pole of the
C                            orbital reference system relative to the
C                            reference frame of the associated SPK
C                            segment.
C
C                 RECIN (12) Declination of the pole of the
C                            orbital reference system relative to
C                            the reference frame of the associated
C                            SPK segment.
C
C$ Detailed_Output
C
C     STATE       is the state produced by evaluating RECIN at ET.
C                 Units are km and km/sec.
C
C$ Parameters
C
C      None.
C
C$ Files
C
C      None.
C
C$ Exceptions
C
C     1) If the eccentricity is greater than 0.9, the error
C        'SPICE(BADECCENTRICITY)' will be signalled.
C
C     2) If the semi-major axis is non-positive, the error
C        'SPICE(BADSEMIAXIS)' is signalled.
C
C
C$ Particulars
C
C     This routine performs a cursory examination of the elements
C     of a type 17 SPK data record and then passes the equinoctial
C     elements contained in that record on to the SPICE routine
C     EQNCPV for evaluation.
C
C$ Examples
C
C     The SPKEnn routines are almost always used in conjunction with
C     the corresponding SPKRnn routines, which read the records from
C     SPK files.
C
C     The data returned by the SPKRnn routine is in its rawest form,
C     taken directly from the segment.  As such, it will be meaningless
C     to a user unless he/she understands the structure of the data type
C     completely.  Given that understanding, however, the SPKRnn
C     routines might be used to examine raw segment data before
C     evaluating it with the SPKEnn routines.
C
C
C     C
C     C     Get a segment applicable to a specified body and epoch.
C     C
C           CALL SPKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND )
C
C     C
C     C     Look at parts of the descriptor.
C     C
C           CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
C           CENTER = ICD( 2 )
C           REF    = ICD( 3 )
C           TYPE   = ICD( 4 )
C
C           IF ( TYPE .EQ. 17 ) THEN
C
C              CALL SPKR17 ( HANDLE, DESCR, ET, RECORD )
C                  .
C                  .  Look at the RECORD data.
C                  .
C              CALL SPKE17 ( ET, RECORD, STATE )
C                  .
C                  .  Check out the evaluated state.
C                  .
C           END IF
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C      W.L. Taber      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 8-JAN-1997 (WLT)
C
C-&
 
C$ Index_Entries
C
C     evaluate type_17 spk segment
C
C-&
 
 
C
C     SPICELIB Functions
C
      LOGICAL               RETURN
 
C
C     Local Variables
C
      DOUBLE PRECISION      A
      DOUBLE PRECISION      RAPOLE
      DOUBLE PRECISION      DECPOL
      DOUBLE PRECISION      H
      DOUBLE PRECISION      K
      DOUBLE PRECISION      ECC
      DOUBLE PRECISION      EPOCH
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'SPKE17')
 
C
C     Fetch the various entities from the input record, first the epoch.
C
      EPOCH  = RECIN(1)
      A      = RECIN(2)
      H      = RECIN(3)
      K      = RECIN(4)
      ECC    = DSQRT( H*H + K*K )
 
      RAPOLE = RECIN(11)
      DECPOL = RECIN(12)
 
 
C
C     Check all the inputs here for obvious failures.  Yes, perhaps
C     this is overkill.  However, there is a lot more computation
C     going on in this routine so that the small amount of overhead
C     here should not be significant.
C
      IF ( A .LE. 0 ) THEN
 
         CALL SETMSG ( 'The semi-major axis supplied to the '
     .   //            'SPK type 17 evaluator was non-positive.  This '
     .   //            'value must be positive. The value supplied was '
     .   //            '#.'                    )
         CALL ERRDP  ( '#', A                  )
         CALL SIGERR ( 'SPICE(BADSEMIAXIS)'    )
         CALL CHKOUT ( 'SPKE17'                )
         RETURN
 
      ELSE IF ( ECC .GT. 0.9D0 ) THEN
 
         CALL SETMSG ( 'The eccentricity supplied for a type 17 '
     .   //            'segment is greater than 0.9.  It must be '
     .   //            'less than 0.9.'
     .   //            'The value supplied '
     .   //            'to the type 17 evaluator was #. ' )
         CALL ERRDP  ( '#',   ECC                )
         CALL SIGERR ( 'SPICE(BADECCENTRICITY)'  )
         CALL CHKOUT ( 'SPKE17'                  )
         RETURN
 
      END IF
 
C
C     That's all for here, just plug the elements into the routine
C     knows how to evaluate the equinoctial elements.
C
      CALL EQNCPV ( ET, EPOCH, RECIN(2), RAPOLE, DECPOL, STATE )
 
C
C     That's all folks.  Check out and return.
C
      CALL CHKOUT('SPKE17')
      RETURN
      END

C$Procedure      CKR04 ( C-kernel, read pointing record, data type 4 )

      SUBROUTINE CKR04 ( HANDLE, DESCR, SCLKDP, TOL, NEEDAV,
     .                   RECORD, FOUND )

C$ Abstract
C
C     Read a single data record from a type 4 CK segment.
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
C     CK
C     DAF
C
C$ Keywords
C
C     POINTING
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE               'ckparam.inc'

      INTEGER               HANDLE
      DOUBLE PRECISION      DESCR  ( * )
      DOUBLE PRECISION      SCLKDP
      DOUBLE PRECISION      TOL
      LOGICAL               NEEDAV 
      DOUBLE PRECISION      RECORD ( * )
      LOGICAL               FOUND

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     DESCR      I   Segment descriptor.
C     SCLKDP     I   Pointing request time.
C     TOL        I   Time tolerance.
C     NEEDAV     I   Angular velocity request flag.
C     RECORD     O   Pointing data record.
C     FOUND      O   True when a record covering SCLKDP is found.
C
C$ Detailed_Input
C
C     HANDLE     is the integer handle of the CK file containing the
C                segment.
C
C     DESCR      is the descriptor of the segment.
C
C     SCLKDP     is the encoded spacecraft clock time for which
C                pointing is being requested.
C
C     TOL        is a time tolerance, measured in the same units as
C                encoded spacecraft clock.
C
C                When SCLKDP falls within the bounds of one of the
C                interpolation intervals then the tolerance has no
C                effect because pointing will be returned at the
C                request time.
C
C                However, if the request time is not in one of the
C                intervals, then the tolerance is used to determine
C                if pointing at one of the interval endpoints should
C                be returned.
C
C     NEEDAV     is true if angular velocity is requested.
C
C$ Detailed_Output
C
C     RECORD     is the record that CKE04 will evaluate to determine
C                the pointing and it includes parameters:
C                
C                ---------------------------------------------------
C                |    Encoded onboard time which is the closest    |
C                |  to SCLKDP and belongs to one of approximation  |
C                |                   intervals                     |
C                ---------------------------------------------------
C                |       encoded SCLK time of the midpoint of      |
C                |             interpolation interval              |
C                ---------------------------------------------------
C                |          radii of interpolation interval        |
C                |    expressed as double precision SCLK ticks     |
C                ---------------------------------------------------
C                |         Number of coefficients for q0           |
C                ---------------------------------------------------
C                |         Number of coefficients for q1           |
C                ---------------------------------------------------
C                |         Number of coefficients for q2           |
C                ---------------------------------------------------
C                |         Number of coefficients for q3           |
C                ---------------------------------------------------
C                |         Number of coefficients for AV1          |
C                ---------------------------------------------------
C                |         Number of coefficients for AV2          |
C                ---------------------------------------------------
C                |         Number of coefficients for AV3          |
C                ---------------------------------------------------
C                |               q0 Cheby coefficients             |
C                ---------------------------------------------------
C                |               q1 Cheby coefficients             |
C                ---------------------------------------------------
C                |               q2 Cheby coefficients             |
C                ---------------------------------------------------
C                |               q3 Cheby coefficients             |
C                ---------------------------------------------------
C                |         AV1 Cheby coefficients (optional)       |
C                ---------------------------------------------------
C                |         AV2 Cheby coefficients (optional)       |
C                ---------------------------------------------------
C                |         AV3 Cheby coefficients (optional)       |
C                ---------------------------------------------------
C
C     FOUND    is true if a record was found to satisfy the pointing
C              request. This occurs when the time for which pointing
C              is requested falls inside one of the interpolation
C              intervals, or when the request time is within the
C              tolerance of an interval endpoint.
C
C$ Parameters
C
C     See 'ckparam.inc'.
C
C$ Exceptions
C
C     1)  If the specified handle does not belong to an open DAF file,
C         an error is diagnosed by a routine that this routine calls.
C
C     2)  If the specified descriptor does not belong a segment 
C         data in which are organized in accordance with generic 
C         segment architecture, an error is diagnosed by DAF generic 
C         segment routines that this routine calls.
C
C     3)  If DESCR is not a valid descriptor of a segment in the CK
C         file specified by HANDLE, the results of this routine are
C         unpredictable.
C
C     4)  If the segment is not of data type 4, as specified in the
C         third integer component of the segment descriptor, then
C         the error SPICE(WRONGDATATYPE) is signalled.
C
C     5)  If angular velocity data was requested but the segment
C         contains no such data, the error SPICE(NOAVDATA) is 
C         signalled.
C
C$ Files
C
C     See argument HANDLE.
C
C$ Particulars
C
C     See the CK Required Reading file for a detailed description of
C     the structure of a type 4 pointing segment.
C
C     When the time for which pointing was requested falls within an
C     interpolation interval, then FOUND will be true and RECORD will
C     contain the set of Chebychev polynomial coefficients for the 
C     time interval that brackets the request time. CKE04 will 
C     evaluate RECORD to give pointing at the request time.
C
C     However, when the request time is not within any of the
C     interpolation intervals, then FOUND will be true only if the
C     interval endpoint closest to the request time is within the
C     tolerance specified by the user. In this case RECORD will
C     contain the set of Chebychev polynomial coefficients for the 
C     time interval one of the ends of which was within tolerance 
C     from the request time, and CKE04 will evaluate RECORD to give 
C     pointing at the time associated with that interval end time.
C
C
C$ Examples
C
C     The CKRnn routines are usually used in tandem with the CKEnn
C     routines, which evaluate the record returned by CKRnn to give
C     the pointing information and output time.
C
C     The following code fragment searches through all of the segments
C     in a file applicable to the Mars Global Surveyor spacecraft bus
C     that are of data type 4, for a particular spacecraft clock time.
C     It then evaluates the pointing for that epoch and prints the
C     result.
C
C     C
C     C     CK parameters include file.
C     C
C           INCLUDE               'ckparam.inc'
C     C
C     C     Declarations
C     C
C           CHARACTER*(20)        SCLKCH
C           CHARACTER*(20)        SCTIME
C           CHARACTER*(40)        IDENT
C
C           DOUBLE PRECISION      AV     ( 3 )
C           DOUBLE PRECISION      CLKOUT
C           DOUBLE PRECISION      CMAT   ( 3, 3 )
C           DOUBLE PRECISION      DCD    ( 2 )
C           DOUBLE PRECISION      DESCR  ( 5 )
C           DOUBLE PRECISION      RECORD ( CK4RSZ )
C           DOUBLE PRECISION      SCLKDP
C           DOUBLE PRECISION      TOL
C
C           INTEGER               HANDLE
C           INTEGER               I
C           INTEGER               ICD    ( 6 )
C           INTEGER               INST
C           INTEGER               SC
C
C           LOGICAL               FND
C           LOGICAL               NEEDAV
C           LOGICAL               SFND
C     C
C     C     Initial values.
C     C
C           SC     = -94
C           INST   = -94000
C           NEEDAV = .FALSE.
C     C
C     C     Load the MGS SCLK kernel and the C-kernel.
C     C
C           CALL FURNSH( 'MGS_SCLK.TSC' )
C           CALL DAFOPR( 'MGS_CK4.BC', HANDLE )
C     C
C     C     Get the spacecraft clock time. Then encode it for use
C     C     in the C-kernel.
C     C
C           CALL PROMPT( 'Enter SCLK string: ', SCLKCH )
C           CALL SCENCD( SC, SCLKCH, SCLKDP )
C     C
C     C     Use a tolerance of 2 seconds (half of the nominal
C     C     separation between MGS pointing instances ).
C     C
C           CALL SCTIKS ( SC, '0000000002:000', TOL )
C     C
C     C     Search from the beginning of the CK file through all
C     C     of the segments.
C     C
C           CALL DAFBFS( HANDLE )
C           CALL DAFFNA( SFND   )
C
C           FND = .FALSE.
C
C           DO WHILE ( ( SFND ) .AND. ( .NOT. FND ) )
C     C
C     C        Get the segment identifier and descriptor.
C     C
C              CALL DAFGN( IDENT )
C              CALL DAFGS( DESCR )
C     C
C     C        Unpack the segment descriptor into its integer and
C     C        double precision components.
C     C
C              CALL DAFUS( DESCR, 2, 6, DCD, ICD )
C     C
C     C        Determine if this segment should be processed.
C     C
C              IF ( ( INST          .EQ. ICD( 1 ) ) .AND.
C          .        ( SCLKDP + TOL  .GE. DCD( 1 ) ) .AND.
C          .        ( SCLKDP - TOL  .LE. DCD( 2 ) ) .AND.
C          .        ( CK4DTP        .EQ. ICD( 3 ) )      ) THEN
C     C
C     C           Find CK 4 record covering requested time.
C     C
C                 CALL CKR04( HANDLE, DESCR, SCLKDP, TOL, NEEDAV,
C          .                  RECORD, FND )
C
C                 IF ( FND ) THEN
C     C
C     C              Compute pointing using found CK 4 record.
C     C
C                    CALL CKE04( NEEDAV, RECORD, CMAT, AV, CLKOUT)
C
C                    CALL SCDECD( SC, CLKOUT, SCTIME )
C
C                    WRITE (*,*)
C                    WRITE (*,*) 'Segment identifier: ', IDENT
C                    WRITE (*,*)
C                    WRITE (*,*) 'Pointing returned for time: ',
C          .                      SCTIME
C                    WRITE (*,*)
C                    WRITE (*,*) 'C-matrix:'
C                    WRITE (*,*)
C                    WRITE (*,*) ( CMAT(1,I), I = 1, 3 )
C                    WRITE (*,*) ( CMAT(2,I), I = 1, 3 )
C                    WRITE (*,*) ( CMAT(3,I), I = 1, 3 )
C                    WRITE (*,*)
C
C                 END IF
C
C              END IF
C
C              CALL DAFFNA ( SFND )
C
C           END DO
C
C$ Restrictions
C
C     1) The file containing the segment should be opened for read
C        or write access either by CKLPF, DAFOPR, or DAFOPW.
C
C     2) The record returned by this routine is intended to be
C        evaluated by CKE04.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     Y.K. Zaiko     (JPL)
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.2, 18-APR-2014 (BVS)
C
C        Minor header edits.
C
C-    SPICELIB Version 1.0.1, 22-AUG-2006 (EDW)
C
C        Replaced references to LDPOOL with references 
C        to FURNSH.
C
C-    SPICELIB Version 1.0.0, 05-MAY-1999 (YKZ) (BVS)
C
C-&

C$ Index_Entries
C
C     read record from type_4 CK segment
C
C-&

C
C     SPICELIB functions
C
      LOGICAL               RETURN
      LOGICAL               FAILED

C
C     Local variables
C
      DOUBLE PRECISION      VALUE
      DOUBLE PRECISION      MIDPT1
      DOUBLE PRECISION      MIDPT2
      DOUBLE PRECISION      RAD1
      DOUBLE PRECISION      RAD2
      DOUBLE PRECISION      LBND1
      DOUBLE PRECISION      LBND2
      DOUBLE PRECISION      RBND1
      DOUBLE PRECISION      DCD    ( 2 )
      DOUBLE PRECISION      CLKOUT

      INTEGER               NUMALL
      INTEGER               NUMCFT ( QAVSIZ )
      INTEGER               ICD    ( 6 )
      INTEGER               ENDS
      INTEGER               INDX
      INTEGER               K
      INTEGER               NREC

      LOGICAL               EXIST
      
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CKR04' )
      END IF

C
C     Set initial value of the found flag to "NOT FOUND".
C
      FOUND = .FALSE.

C
C     We need to unpack and analyze descriptor components. The 
C     unpacked descriptor contains the following information
C     about the segment:
C
C        DCD(1)  Initial encoded SCLK
C        DCD(2)  Final encoded SCLK
C        ICD(1)  Instrument
C        ICD(2)  Inertial reference frame
C        ICD(3)  Data type
C        ICD(4)  Angular velocity flag
C        ICD(5)  Initial address of segment data
C        ICD(6)  Final address of segment data
C
      CALL DAFUS  ( DESCR, 2, 6, DCD, ICD )

C
C     Check if the segment is type 4. Signal an error if it's not.
C
      IF ( ICD( 3 ) .NE. CK4DTP ) THEN

         CALL SETMSG ( 'The segment is not a type 4 segment.  '   //
     .                 'Type is #'                                 )
         CALL ERRINT ( '#', ICD(3)                                 )
         CALL SIGERR ( 'SPICE(WRONGDATATYPE)'                      )
         CALL CHKOUT ( 'CKR04'                                     )
         RETURN

      END IF

      IF ( NEEDAV ) THEN

C
C        Signal an error if angular velocities are required but 
C        they are not present in the segment.
C
         IF ( ICD( 4 ) .NE. 1 ) THEN

            CALL SETMSG ( 'Segment does not contain angular '     //
     .                    'velocity data.'                        )
            CALL SIGERR ( 'SPICE(NOAVDATA)'                       )
            CALL CHKOUT ( 'CKR04'                                 )
            RETURN

         END IF

      END IF
      
C
C     Get number of records (packets) in the segment.
C
      CALL CKNR04 ( HANDLE, DESCR, NREC )

C
C     Locate the last time in the set of reference epochs less than or
C     equal to the input SCLKDP.
C
      CALL SGFRVI ( HANDLE, DESCR, SCLKDP, VALUE, INDX, EXIST )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'CKR04' )
         RETURN
      END IF

      IF ( .NOT. EXIST ) THEN

C
C        We didn't find reference value with means that SCLKDP is 
C        less than the left bound of the first interpolation interval. 
C        Fetch the first record.
C
         INDX = 1
         CALL SGFPKT ( HANDLE, DESCR, INDX, INDX, RECORD, ENDS )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'CKR04' )
            RETURN
         END IF

         MIDPT1 = RECORD( 1 )
         RAD1   = RECORD( 2 )

C
C        Check whether SCLKDP is within TOL of the left bound of the 
C        first interval.
C
         LBND1 = MIDPT1 - RAD1 - TOL

         IF ( SCLKDP .GE. LBND1  ) THEN

            FOUND = .TRUE.
            CLKOUT = MIDPT1 - RAD1

         END IF

      ELSE
      
C
C        We found reference value.
C
         IF ( INDX .GE. NREC ) THEN
      
C
C           The SCLKDP is greater than the left bound of the last 
C           interpolation interval. Fetch the last record.
C
            INDX = NREC
            
            CALL SGFPKT ( HANDLE, DESCR, INDX, INDX, RECORD, ENDS )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'CKR04' )
               RETURN
            END IF

            MIDPT1 = RECORD( 1 )
            RAD1   = RECORD( 2 )

C
C           Check whether SCLKDP is within TOL of the right bound of 
C           the last interval.
C
            RBND1 = MIDPT1 + RAD1 + TOL

            IF ( SCLKDP .LE. RBND1 ) THEN

               FOUND = .TRUE.

C
C              Check whether SCLKDP falls between right bound of the 
C              last interval and right bound + TOL.
C
               RBND1 = MIDPT1 + RAD1

               IF ( SCLKDP .GE. RBND1 ) THEN

                  CLKOUT = MIDPT1 + RAD1

               ELSE

C
C                 SCLKDP belongs to the last interval
C
                  CLKOUT = SCLKDP

               END IF

            END IF

         ELSE IF ( ( INDX .GE. 1 ) .AND. ( INDX .LT. NREC ) ) THEN

C
C           The SCLKDP lies between left bound of the first interval
C           and the right bound of the interval before the last 
C           interval. Fetch the found record.
C
            CALL SGFPKT ( HANDLE, DESCR, INDX, INDX, RECORD, ENDS )
            
            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'CKR04' )
               RETURN
            END IF

            MIDPT1 = RECORD( 1 )
            RAD1   = RECORD( 2 )

C
C           Check whether SCLKDP belongs to current interval.
C
            RBND1 = MIDPT1 + RAD1

            IF ( SCLKDP .LE. RBND1 ) THEN

               FOUND = .TRUE.
               CLKOUT = SCLKDP

            ELSE

C
C              SCLKDP doesn't belong to current interval. Fetch the 
C              next packet.
C
               CALL SGFPKT ( HANDLE, DESCR, INDX + 1, INDX + 1, RECORD,
     .                                                         ENDS )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'CKR04' )
                  RETURN
               END IF

               MIDPT2 = RECORD( 1 )
               RAD2   = RECORD( 2 )

C
C              Find the closest interval bound for SCLKDP.
C
               RBND1 = MIDPT1 + RAD1
               LBND2 = MIDPT2 - RAD2

               IF ( ( SCLKDP - RBND1 ) .LE. ( LBND2 - SCLKDP ) ) THEN

C
C                 SCLKDP is closer to the right bound of current  
C                 interval. Check whether it's within TOL of it.
C
                  RBND1 = MIDPT1 + RAD1 + TOL

                  IF ( SCLKDP .LE. RBND1 ) THEN

                     FOUND = .TRUE.
                     CLKOUT = MIDPT1 + RAD1

C
C                    At this point we need to re-read our current
C                    record because it was overwritten by the next 
C                    record. No FAILED() check here -- we already 
C                    fetched this packet successfully one call to 
C                    SGFPKT ago.
C
                     CALL SGFPKT( HANDLE, DESCR, INDX, INDX, RECORD, 
     .                                                          ENDS )

                  END IF

               ELSE

C
C                 SCLKDP is closer to the left bound of the next 
C                 interval. Check whether it's within TOL of it.
C
                  LBND2 = MIDPT2 - RAD2 - TOL

                  IF ( SCLKDP .GE. LBND2 ) THEN

                     FOUND = .TRUE.
                     INDX = INDX + 1
                     CLKOUT = MIDPT2 - RAD2

                  END IF

               END IF

            END IF

         END IF
         
      END IF

C
C     If we found the interval on segment the SCLKDP belongs to, then
C
      IF ( FOUND ) THEN

C
C        Decode numbers of polynomial coefficients.
C
         CALL ZZCK4D2I ( RECORD( 3 ), QAVSIZ, CK4PCD, NUMCFT )

C
C        Count total number of coefficients.
C        
         NUMALL = 0
         
         DO K = 1, QAVSIZ                
            NUMALL = NUMALL + NUMCFT ( K )                     
         END DO

C
C        Move coefficients to the right and insert numbers of 
C        coefficients into output RECORD.
C
         DO K = NUMALL, 1, -1
            RECORD ( K + CK4SFT ) = RECORD( K + 3 )
         END DO

         DO K = 1, QAVSIZ
            RECORD( K + 3 ) = NUMCFT( K )
         END DO

         RECORD( 3 ) = RECORD( 2 )
         RECORD( 2 ) = RECORD( 1 )

C
C        Insert CLKOUT into output RECORD
C
         RECORD( 1 ) = CLKOUT

      END IF

C
C     All done.
C
      CALL CHKOUT ( 'CKR04' )
      
      RETURN

      END

C$Procedure PCKW02 ( PCK, write type 2 segment )

      SUBROUTINE PCKW02 (  HANDLE,  BODY,    FRAME,   FIRST,
     .                     LAST,    SEGID,   INTLEN,  N,
     .                     POLYDG,  CDATA,   BTIME          )

C$ Abstract
C
C    Write a type 2 segment to a PCK binary file given
C    the file handle, body, frame, time range covered by the
C    segment, and the Chebyshev polynomial coefficeients.
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
C     NAIF_IDS
C     SPC
C     PCK
C
C$ Keywords
C
C     PCK
C
C$ Declarations

      INTEGER               HANDLE
      INTEGER               BODY
      CHARACTER*(*)         FRAME
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      CHARACTER*(*)         SEGID
      DOUBLE PRECISION      INTLEN
      INTEGER               N
      INTEGER               POLYDG
      DOUBLE PRECISION      CDATA (*)
      DOUBLE PRECISION      BTIME

C$ Brief_I/O
C
C   Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of binary PCK file open for writing.
C     BODY       I   NAIF code for ephemeris object.
C     FRAME      I   Reference frame name.
C     FIRST      I   Start time of interval covered by segment.
C     LAST       I   End time of interval covered by segment.
C     SEGID      I   Segment identifier.
C     INTLEN     I   Length of time covered by logical record.
C     N          I   Number of logical records in segment.
C     POLYDG     I   Chebyshev polynomial degree.
C     CDATA      I   Array of Chebyshev coefficients.
C     BTIME      I   Begin time of first logical record.
C
C$ Detailed_Input
C
C     HANDLE         is the DAF handle of an PCK file to which a type 2
C                    segment is to be added.  The PCK file must be open
C                    for writing.
C
C     BODY           is the NAIF integer code for an ephemeris object
C                    whose orientation is described by the segment to
C                    be created.
C
C     FRAME          is the NAIF name for a reference frame relative to
C                    which the orientation information for BODY is
C                    specified.
C
C     FIRST,
C     LAST           are, respectively, the start and stop times of
C                    the time interval over which the segment defines
C                    the orientation of body.
C
C     SEGID          is the segment identifier.  A PCK segment
C                    identifier may contain up to 40 characters.
C
C     INTLEN         Length of time, in seconds, covered by each set of
C                    Chebyshev polynomial coefficients (each logical
C                    record).  Each set of Chebyshev coefficents must
C                    cover this fixed time interval, INTLEN.
C
C     N              is the number of sets of Chebyshev polynomial
C                    coefficents (number of logical records)
C                    to be stored in the segment.  There is one set
C                    of Chebyshev coefficients for each time period.
C
C     POLYDG         Degree of each set of Chebyshev polynomials.
C
C     CDATA          Array containing all the sets of Chebyshev
C                    polynomial coefficients to be contained in the
C                    segment of the PCK file.  The coefficients are
C                    stored in CDATA in order as follows:
C
C                       the (degree + 1) coefficients for the first
C                       Euler angle of the first logical record
C
C                       the coefficients for the second Euler angle
C
C                       the coefficients for the third Euler angle
C
C                       the coefficients for the first Euler angle for
C                       the second logical record, ...
C
C                       and so on.
C
C     BTIME          Begin time (seconds past J2000 TDB) of first set
C                    of Chebyshev polynomial coefficients (first
C                    logical record).
C
C$ Detailed_Output
C
C      None.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     1) If the number of sets of coefficients is not positive
C        'SPICE(NUMCOEFFSNOTPOS)' is signalled.
C
C     2) If the interval length is not positive, 'SPICE(INTLENNOTPOS)'
C        is signalled.
C
C     3) If the integer code for the reference frame is not recognized,
C        'SPICE(INVALIDREFFRAME)' is signalled.
C
C     4) If segment stop time is not greater then the begin time,
C         'SPICE(BADDESCRTIMES)' is signalled.
C
C     5) If the time of the first record is not greater than
C        or equal to the descriptor begin time, 'SPICE(BADDESCRTIMES)'
C        is signalled.
C
C     6) If the end time of the last record is not greater than
C        or equal to the descriptor end time, 'SPICE(BADDESCRTIMES)' is
C        signalled.
C
C$ Files
C
C     A new type 2 PCK segment is written to the PCK file attached
C     to HANDLE.
C
C$ Particulars
C
C     This routine writes an PCK type 2 data segment to the designated
C     PCK file, according to the format described in the PCK Required
C     Reading.
C
C     Each segment can contain data for only one body and reference
C     frame.  The Chebyshev polynomial degree and length of time covered
C     by each logical record are also fixed.  However, an arbitrary
C     number of logical records of Chebyshev polynomial coefficients can
C     be written in each segment.  Minimizing the number of segments in
C     a PCK file will help optimize how the SPICE system accesses the
C     file.
C
C
C$ Examples
C
C
C     Suppose that you have sets of Chebyshev polynomial coefficients
C     in an array CDATA pertaining to the position of the moon (NAIF ID
C     = 301) in the J2000 reference frame, and want to put these into a
C     type 2 segment in an existing PCK file.  The following code could
C     be used to add one new type 2 segment.  To add multiple segments,
C     put the call to PCKW02 in a loop.
C
C     C
C     C      First open the PCK file and get a handle for it.
C     C
C            CALL DAFOPW ( PCKNAM, HANDLE )
C
C     C
C     C      Create a segment identifier.
C     C
C            SEGID = 'MY_SAMPLE_PCK_TYPE_2_SEGMENT'
C
C     C
C     C      Write the segment.
C
C            CALL PCKW02 (  HANDLE, 301,    'J2000',
C     .                     FIRST,  LAST,   SEGID,   INTLEN,
C     .                     N,      POLYDG, CDATA,   BTIME)
C
C     C
C     C      Close the file.
C     C
C            CALL DAFCLS ( HANDLE )
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
C     K.S. Zukor (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.1, 03-JAN-2014 (EDW)
C
C        Minor edits to Procedure; clean trailing whitespace.
C        Removed unneeded Revisions section.
C
C-    SPICELIB Version 2.0.0, 01-AUG-1995 (KSZ)
C
C        The calling sequence was corrected so that REF is
C        a character string and BTIME contains only the start
C        time of the first record.  Comments updated, and new
C        routine CHCKID is called to check segment identifier.
C
C-    SPICELIB Version 1.0.0, 11-MAR-1994 (KSZ)
C
C-&

C$ Index_Entries
C
C     write pck type_2 data segment
C
C-&

C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local Parameters
C
C     DTYPE is the PCK data type.
C
      INTEGER               DTYPE
      PARAMETER           ( DTYPE   =   2 )
C
C     NS is the size of a packed PCK segment descriptor.
C
      INTEGER               NS
      PARAMETER           ( NS      =   5 )
C
C     ND is the number of double precision components in an PCK
C     segment descriptor. PCK uses ND = 2.
C
      INTEGER               ND
      PARAMETER           ( ND      =   2 )
C
C     NI is the number of integer components in an PCK segment
C     descriptor. PCK uses NI = 5.
C
      INTEGER               NI
      PARAMETER           ( NI      =   5 )
C
C     SIDLEN is the maximum number of characters allowed in an
C     PCK segment identifier.
C
      INTEGER               SIDLEN
      PARAMETER           ( SIDLEN  =  40 )

C
C     Local variables
C
      CHARACTER*(SIDLEN)    ETSTR
      CHARACTER*(SIDLEN)    NETSTR

      DOUBLE PRECISION      DCD   ( ND )
      DOUBLE PRECISION      DESCR ( NS )
      DOUBLE PRECISION      LTIME
      DOUBLE PRECISION      MID
      DOUBLE PRECISION      NUMREC
      DOUBLE PRECISION      RADIUS
      DOUBLE PRECISION      RSIZE

      INTEGER               I
      INTEGER               ICD   ( NI )
      INTEGER               K
      INTEGER               NINREC
      INTEGER               REFCOD


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'PCKW02' )
      END IF

C
C     The number of sets of coefficients must be positive.
C
      IF ( N .LE. 0 ) THEN

         CALL SETMSG ( 'The number of sets of Euler angle'  //
     .                 'coefficients is not positive. N = #'       )
         CALL ERRINT ( '#', N                                      )
         CALL SIGERR ( 'SPICE(NUMCOEFFSNOTPOS)'                    )
         CALL CHKOUT ( 'PCKW02'                                    )
         RETURN

      END IF
C
C     The interval length must be positive.
C
      IF ( INTLEN .LE. 0 ) THEN

         CALL SETMSG ( 'The interval length is not positive.'  //
     .                 'N = #'                                     )
         CALL ERRDP  ( '#', INTLEN                                 )
         CALL SIGERR ( 'SPICE(INTLENNOTPOS)'                       )
         CALL CHKOUT ( 'PCKW02'                                    )
         RETURN

      END IF
C
C     Get the NAIF integer code for the reference frame.
C
      CALL IRFNUM ( FRAME, REFCOD )

      IF ( REFCOD .EQ. 0 ) THEN

         CALL SETMSG ( 'The reference frame # is not supported.'   )
         CALL ERRCH  ( '#', FRAME                                  )
         CALL SIGERR ( 'SPICE(INVALIDREFFRAME)'                    )
         CALL CHKOUT ( 'PCKW02'                                    )
         RETURN

      END IF

C
C     The segment stop time must be greater than the begin time.
C
      IF ( FIRST .GT. LAST ) THEN

         CALL SETMSG ( 'The segment start time: # is greater than ' //
     .                 'the segment end time: #'                   )
         CALL ETCAL  ( FIRST, ETSTR                                )
         CALL ERRCH  ( '#',   ETSTR                                )
         CALL ETCAL  ( LAST,  NETSTR                               )
         CALL ERRCH  ( '#',   NETSTR                               )
         CALL SIGERR ( 'SPICE(BADDESCRTIMES)'                      )
         CALL CHKOUT ( 'PCKW02'                                    )
         RETURN

      END IF

C
C     The begin time of the first record must be less than or equal
C     to the begin time of the segment.
C
      IF ( FIRST .LT. BTIME ) THEN

         CALL SETMSG ( 'The segment descriptor start time: # is '//
     .                 'less than the beginning time of the '//
     .                 'segment data: #'                           )
         CALL ETCAL  ( FIRST, ETSTR                                )
         CALL ERRCH  ( '#',   ETSTR                                )
         CALL ETCAL  ( BTIME, ETSTR                                )
         CALL ERRCH  ( '#',   ETSTR                                )
         CALL SIGERR ( 'SPICE(BADDESCRTIMES)'                      )
         CALL CHKOUT ( 'PCKW02'                                    )
         RETURN

      END IF
C
C     The end time of the final record must be greater than or
C     equal to the end time of the segment.
C
      LTIME = BTIME + N * INTLEN
      IF ( LAST .GT. LTIME ) THEN

         CALL SETMSG ( 'The segment descriptor end time: # is '//
     .                 'greater than the end time of the segment '//
     .                 'data: #'  )
         CALL ETCAL  ( LAST,  ETSTR                                )
         CALL ERRCH  ( '#',   ETSTR                                )
         CALL ETCAL  ( LTIME, ETSTR                                )
         CALL ERRCH  ( '#',   ETSTR                                )
         CALL SIGERR ( 'SPICE(BADDESCRTIMES)'                      )
         CALL CHKOUT ( 'PCKW02'                                    )
         RETURN

      END IF

C
C     Now check the validity of the segment identifier.
C
      CALL CHCKID ( 'PCK segment identifier', SIDLEN, SEGID )
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'PCKW02' )
         RETURN
      END IF

C
C     Store the start and end times to be associated
C     with this segment.
C
      DCD(1) = FIRST
      DCD(2) = LAST

C
C     Create the integer portion of the descriptor.
C
      ICD(1) = BODY
      ICD(2) = REFCOD
      ICD(3) = DTYPE

C
C     Pack the segment descriptor.
C
      CALL DAFPS ( ND, NI, DCD, ICD, DESCR )

C
C     Begin a new segment of PCK type 2 form:
C
C        Record 1
C        Record 2
C        ...
C        Record N
C        INIT       ( initial epoch of first record )
C        INTLEN     ( length of interval covered by each record )
C        RSIZE      ( number of data elements in each record )
C        N          ( number of records in segment )
C
C     Each record will have the form:
C
C        MID        ( midpoint of time interval )
C        RADIUS     ( radius of time interval )
C        X coefficients, Y coefficients, Z coefficients
C
      CALL DAFBNA ( HANDLE, DESCR, SEGID )

C
C     Calculate the number of entries in a record.
C
      NINREC = ( POLYDG + 1 ) * 3

C
C     Fill segment with N records of data.
C
      DO I = 1, N

C
C        Calculate the midpoint and radius of the time of each
C        record, and put that at the beginning of each record.
C
         RADIUS = INTLEN / 2
         MID    = BTIME + RADIUS + ( I - 1 ) * INTLEN

         CALL DAFADA ( MID,    1)
         CALL DAFADA ( RADIUS, 1)

C
C        Put one set of coefficients into the segment.
C
         K = 1 + (I - 1) * NINREC

         CALL DAFADA ( CDATA(K), NINREC )

      END DO
C
C     Store the initial epoch of the first record.
C
      CALL DAFADA ( BTIME, 1 )

C
C     Store the length of interval covered by each record.
C
      CALL DAFADA ( INTLEN, 1 )

C
C     Store the size of each record (total number of array elements).
C
      RSIZE = 2 + NINREC
      CALL DAFADA ( RSIZE, 1 )

C
C     Store the number of records contained in the segment.
C
      NUMREC = N
      CALL DAFADA ( NUMREC, 1 )

C
C     End this segment.
C
      CALL DAFENA

      CALL CHKOUT ( 'PCKW02' )
      RETURN
      END

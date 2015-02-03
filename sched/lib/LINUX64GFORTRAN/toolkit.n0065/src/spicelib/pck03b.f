C$Procedure PCK03B ( PCK, begin a type 3 segment )

      SUBROUTINE PCK03B ( HANDLE,
     .                    SEGID,  BODY,  FRAME,
     .                    FIRST,  LAST,  CHBDEG  )

C$ Abstract
C
C     Begin a type 03 PCK segment in the binary PCK file associated with
C     HANDLE. See also PCK03A and PCK03E.
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
C     PCK
C
C$ Keywords
C
C     PCK
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'sgparam.inc'

      INTEGER               HANDLE
      CHARACTER*(*)         SEGID
      INTEGER               BODY
      CHARACTER*(*)         FRAME
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      INTEGER               CHBDEG

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   The handle of a DAF file open for writing.
C     SEGID      I   The string to use for segment identifier.
C     BODY       I   The NAIF ID code for the body of the segment.
C     FRAME      I   The inertial frame for this segment.
C     FIRST      I   The first epoch for which the segment is valid.
C     LAST       I   The last epoch for which the segment is valid.
C     CHBDEG     I   The degree of the Chebyshev Polynomial used.
C
C$ Detailed_Input
C
C     HANDLE   is the file handle of a PCK file that has been
C              opened for writing.
C
C     SEGID    is the segment identifier. A PCK segment identifier
C              may contain up to 40 printing characters. It may also be
C              blank.
C
C     BODY     is the SPICE ID code for the body whose orientation
C              information is to be stored in the PCK segment being
C              created.
C
C     FRAME    is the inertial reference frame to which the orientation
C              data for BODY is relative.
C
C     FIRST    are the bounds on the ephemeris times, expressed as
C     LAST     seconds past J2000, for which the states can be used
C              to interpolate a state for BODY.
C
C     CHBDEG   is the degree of the Chebyshev Polynomial used for
C              each set of Chebyshev coefficients that are to be stored
C              in the segment.
C
C$ Detailed_Output
C
C     None.    The data are stored in the PCK segment in the DAF
C              attached to HANDLE.
C
C              See the $ Particulars section for details about the
C              structure of a type 03 PCK segment.
C
C$ Parameters
C
C     This subroutine makes use of parameters defined in the file
C     'sgparam.inc'.
C
C$ Exceptions
C
C     1) If the degree of the Chebyshev Polynomial to be used for this
C        segment is negative, the error SPICE(INVALIDARGUMENT) will
C        be signalled.
C
C     2) Errors in the structure or content of the inputs other than the
C        degree of the Chebyshev Polynomial are diagnosed by routines
C        called by this one.
C
C     3) File access errors are diagnosed by routines in the call tree
C        of this routine.
C
C$ Files
C
C      None.
C
C$ Particulars
C
C     This routine begins a type 03 segment in the binary PCK file that
C     is associated with HANDLE. The file must have been opened with
C     write access.
C
C     This routine is one of a set of three routines for creating and
C     adding data to type 03 PCK segments. These routines are:
C
C        PCK03B: Begin a type 03 PCK segment. This routine must be
C                called before any data may be added to a type 03
C                segment.
C
C        PCK03A: Add data to a type 03 PCK segment. This routine may be
C                called any number of times after a call to PCK03B to
C                add type 03 records to the PCK segment that was
C                started.
C
C        PCK03E: End a type 03 PCK segment. This routine is called to
C                make the type 03 segment a permanent addition to the
C                PCK file. Once this routine is called, no further type
C                03 records may be added to the segment. A new segment
C                must be started.
C
C     A type 03 PCK segment consists of coefficient sets for fixed order
C     Chebyshev polynomials over consecutive time intervals, where the
C     time intervals need not all be of the same length. The Chebyshev
C     polynomials represent the orientation of a body specified relative
C     to an inertial frame by the angles RA, DEC, W and body fixed
C     angular rates for each axis of the body fixed coordinate system
C     defined by RA, DEC, and W. All of the angles and the angular rates
C     of the axes are given in degrees.
C
C     The orientation data supplied to the type 03 PCK writer is packed
C     into an array as a sequence of logical records,
C
C        -----------------------------------------------------
C        | Record 1 | Record 2 | ... | Record N-1 | Record N |
C        -----------------------------------------------------
C
C     with each record has the following format.
C
C           ------------------------------------------------
C           |  The midpoint of the approximation interval  |
C           ------------------------------------------------
C           |  The radius of the approximation interval    |
C           ------------------------------------------------
C           |  CHBDEG+1 coefficients for RA                |
C           ------------------------------------------------
C           |  CHBDEG+1 coefficients for DEC               |
C           ------------------------------------------------
C           |  CHBDEG+1 coefficients for W                 |
C           ------------------------------------------------
C           |  CHBDEG+1 coefficients for the X-axis rate   |
C           ------------------------------------------------
C           |  CHBDEG+1 coefficients for the Y-axis rate   |
C           ------------------------------------------------
C           |  CHBDEG+1 coefficients for the Z-axis rate   |
C           ------------------------------------------------
C
C$ Examples
C
C     Assume we have the following for each of the examples that
C     follow.
C
C        HANDLE   is the handle of a PCK file opened with write
C                 access.
C
C        SEGID    is a character string of no more than 40 characters
C                 which provides a pedigree for the data in the PCK
C                 segment.
C
C        BODY     is the SPICE ID code for the body whose orientation
C                 data is to be placed into the file.
C
C        REFFRM   is the name of the SPICE inertial reference frame
C                 the orientation data is relative to.
C
C        FIRST    is the starting epoch, in seconds past J2000, for
C                 the orientation data to be placed into the segment.
C
C        LAST     is the ending epoch, in seconds past J2000, for
C                 the orientation data to be placed into the segment.
C
C     Example 1:
C
C        For this example, we also assume that:
C
C           N        is the number of type 03 records that we want to
C                    put into a segment in PCK file.
C
C           RECRDS   contains N type 03 records packaged for the PCK
C                    file.
C
C           ETSTRT   contains the initial epochs for each of the
C                    records contained in RECRDS, where
C
C                       ETSTRT(I) < ETSTRT(I+1), I = 1, N-1
C
C                       ETSTRT(1) <= FIRST, ETSTRT(N) < LAST
C
C                       ETSTRT(I+1), I = 1, N-1, is the ending epoch for
C                       record I as well as the initial epoch for record
C                       I+1.
C
C        Then the following code fragment demonstrates how to create a
C        type 03 PCK segment if all of the data for the segment is
C        available at one time.
C
C        C
C        C     Begin the segment.
C        C
C              CALL PCK03B ( HANDLE, SEGID, BODY, REFFRM,
C             .              FIRST,  LAST,  CHBDEG        )
C        C
C        C     Add the data to the segment all at once.
C        C
C              CALL PCK03A ( HANDLE, N, RECRDS, ETSTRT )
C        C
C        C     End the segment, making the segment a permanent addition
C        C     to the PCK file.
C        C
C              CALL PCK03E ( HANDLE )
C
C     Example 2:
C
C        In this example we want to add type O3 PCK records, as
C        described above in the $ Particulars section, to the segment
C        being written as they are generated. The ability to write the
C        records in this way is useful if computer memory is limited. It
C        may also be convenient from a programming perspective to write
C        the records one at a time.
C
C        For this example, assume that we want to generate N type 03 PCK
C        records, one for each of N time intervals, writing them all to
C        the same segment in a PCK file. Let
C
C           N        be the number of type 03 records that we want to
C                    generate and put into a segment in an PCK file.
C
C           RECORD   be an array with enough room to hold a single type
C                    03 record, i.e. RECORD should have dimension at
C                    least 6 * (CHBDEG + 1 ) + 2.
C
C           START    be an array of N times that are the beginning
C                    epochs for each of the intervals of interest. The
C                    times should be in increasing order and the start
C                    time for the first interval should equal the
C                    starting time for the segment.
C
C                       START(I) < START(I+1), I = 1, N-1
C
C                       START(1) = FIRST
C
C           STOP     be an array of N times that are the ending epochs
C                    for each of the intervals of interest. The times
C                    should be in increasing order and the stop time for
C                    interval I should equal the start time for interval
C                    I+1, i.e., we want to have continuous coverage in
C                    time across all of the records. Also, the stop time
C                    for the last interval should equal the ending time
C                    for the segment.
C
C                       STOP(I) < STOP(I+1), I = 1, N-1
C
C                       STOP(I) = START(I+1), I = 1, N-1
C
C                       STOP(N) = LAST
C
C           GENREC( TIME1, TIME2, RECORD )
C
C                    be a subroutine that generates a type 03 PCK record
C                    for a time interval specified by TIME1 and TIME2.
C
C        Then the following code fragment demonstrates how to create a
C        type 03 PCK segment if all of the data for the segment is not
C        available at one time.
C
C        C
C        C     Begin the segment.
C        C
C              CALL PCK03B ( HANDLE, SEGID, BODY, REFFRM,
C             .              FIRST,  LAST,  CHBDEG        )
C
C        C
C        C     Generate the records and write them to the segment in the
C        C     PCK file one at at time.
C        C
C              DO I = 1, N
C
C                 CALL GENREC ( START(I), STOP(I), RECORD   )
C                 CALL PCK03A ( HANDLE, 1, RECORD, START(I) )
C
C              END DO
C
C        C
C        C     End the segment, making the segment a permanent addition
C        C     to the PCK file.
C        C
C              CALL PCK03E ( HANDLE )
C
C$ Restrictions
C
C     The binary PCK file must be open with write access.
C
C     Only one segment may be written to a particular PCK file at a
C     time. All of the data for the segment must be written and the
C     segment must be ended before another segment may be started in
C     the file.
C
C$ Literature_References
C
C      None.
C
C$ Author_and_Institution
C
C      K.R. Gehringer      (JPL)
C      E.D. Wright         (JPL)
C      B.V. Semenov        (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 10-FEB-2014 (EDW) (BVS)
C
C        Minor edits to Procedure; clean trailing whitespace.
C        Corrected order of header sections to conform to NAIF 
C        standard.
C
C        Removed comments from the Declarations section.
C
C-    SPICELIB Version 1.0.0, 06-MAR-1995 (KRG)
C
C-&

C$ Index_Entries
C
C     begin writing a type_03 pck segment
C
C-&

C
C     Spicelib functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN
C
C     Local Parameters
C
C     DAF ND and NI values for PCK files.
C
      INTEGER               ND
      PARAMETER           ( ND = 2 )

      INTEGER               NI
      PARAMETER           ( NI = 5 )
C
C     Length of an PCK descriptor.
C
      INTEGER               NDESCR
      PARAMETER           ( NDESCR =  ND + (NI+1)/2 )
C
C     Number of Euler angles.
C
      INTEGER               NEANGS
      PARAMETER           ( NEANGS = 6 )
C
C     The type of this segment.
C
      INTEGER               TYPE
      PARAMETER           ( TYPE = 03 )
C
C     The number of constants.
C
      INTEGER               NCONST
      PARAMETER           ( NCONST = 1 )
C
C     Local variables
C
      DOUBLE PRECISION      DESCR ( NDESCR )
      DOUBLE PRECISION      DCOEFF

      INTEGER               NCOEFF
      INTEGER               PKTSIZ
C
C     Standard SPICELIB error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'PCK03B' )
      END IF
C
C     First, check the degree of the polynomial to be sure that it is
C     not negative.
C
      IF ( CHBDEG .LT. 0 ) THEN

         CALL SETMSG ( 'The degree of the Chebyshev Polynomial'  //
     .                 ' was negative, #. The degree of the'     //
     .                 ' polynomial must be greater than or'     //
     .                 ' equal to zero.'                          )
         CALL ERRINT ( '#', CHBDEG                                )
         CALL SIGERR ( 'SPICE(INVALIDARGUMENT)'                   )
         CALL CHKOUT ( 'PCK03B'                                   )
         RETURN

      END IF
C
C     Create a descriptor for the segment we are about to write.
C
      CALL PCKPDS ( BODY, FRAME, TYPE, FIRST, LAST, DESCR )

      IF ( FAILED() ) THEN

         CALL CHKOUT ( 'PCK03B' )
         RETURN

      END IF

C
C     We've got a valid descriptor, so compute a few things and begin
C     the segment.
C
      NCOEFF = CHBDEG + 1
      PKTSIZ = NEANGS * NCOEFF + 2
      DCOEFF = DBLE ( NCOEFF )
C
C     For this data type, we want to use an explicit reference value
C     index where the reference epochs are in increasing order. We also
C     want to have as the index for a particular request epoch the index
C     of the greatest reference epoch less than or equal to the request
C     epoch. These characteristics are prescribed by the mnemonic EXPLE.
C     See the include file 'sgparam.inc' for more details.
C
      CALL SGBWFS ( HANDLE, DESCR,  SEGID,  NCONST,
     .                      DCOEFF, PKTSIZ, EXPLE   )

      CALL CHKOUT ( 'PCK03B' )
      RETURN

      END

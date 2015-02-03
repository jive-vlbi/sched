C$Procedure PCK03E ( PCK, end a type 3 segment )

      SUBROUTINE PCK03E ( HANDLE )

C$ Abstract
C
C     End the type 03 PCK segment currently being written to the binary
C     PCK file associated with HANDLE. See also PCK03B and PCK03A.
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

      INTEGER               HANDLE

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   The handle of a binary PCK file open for writing.
C
C$ Detailed_Input
C
C     HANDLE   is the file handle of a binary PCK file that has been
C              opened for writing and to which a type 03 PCK segment is
C              being written.
C
C$ Detailed_Output
C
C     None.    The segment in the PCK file associated with HANDLE will
C              be ended, making the addition of the data permanent.
C
C              See the $ Particulars section for details about the
C              structure of a type 03 PCK segment.
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
C     See the argument HANDLE.
C
C$ Particulars
C
C     This routine ends a type 03 PCK segment that is being written to
C     the binary PCK file associated with HANDLE. Ending the PCK segment
C     is a necessary step in the process of making the data a permanent
C     part of the binary PCK file.
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
C              CALL PCK03B ( HANDLE, SEGID, DESCR, CHBDEG )
C
C        C
C        C     Generate the records and write them to the segment in the
C        C     PCK file one at at time.
C        C
C              DO I = 1, N
C
C                 CALL GENREC ( START(I), STOP(I), RECORD )
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
C     1) The type 03 binary PCK segment being closed must have been
C        started by the routine PCK03B, the routine which begins a type
C        03 PCK segment.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     K.R. Gehringer      (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 03-JAN-2014 (EDW)
C
C        Minor edits to Procedure; clean trailing whitespace.
C        Corrected order of header sections to conform to NAIF 
C        standard.
C
C-    SPICELIB Version 1.0.0, 06-MAR-1995 (KRG)
C
C-&

C$ Index_Entries
C
C     end a type_03 pck segment
C
C-&

C
C     Spicelib functions
C
      LOGICAL               RETURN

C
C     Standard SPICELIB error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'PCK03E' )
      END IF

C
C     This is simple, just call the routine which ends a generic
C     segment.
C
      CALL SGWES ( HANDLE )

C
C     No need to check FAILED() since all we do is leave. The caller can
C     check it.
C
      CALL CHKOUT ( 'PCK03E' )
      RETURN

      END

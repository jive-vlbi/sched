 
C$Procedure      SPK14E ( SPK type 14: End a segment. )
 
      SUBROUTINE SPK14E ( HANDLE )
 
C$ Abstract
C
C     End the type 14 SPK segment currently being written to the SPK
C     file associated with HANDLE. See also SPK14B and SPK14E.
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
C     None.
C
C$ Keywords
C
C     SPK
C
C$ Declarations
 
      IMPLICIT NONE
 
      INTEGER               HANDLE
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   The handle of an SPK file open for writing.
C
C$ Detailed_Input
C
C     HANDLE   is the file handle of an SPK file that has been
C              opened for writing, and to which a type 14 segment is
C              being written.
C
C$ Detailed_Output
C
C     None.    The type 14 segment in the SPK file associated with
C              HANDLE will be ended, making the addition of the data
C              to the file permanent.
C
C              See the $ Particulars section for details about the
C              structure of a type 14 SPK segment.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C     This routine ends a type 14 SPK segment which is being written to
C     the SPK file associated with HANDLE. Ending the SPK segment is a
C     necessary step in the process of making the data a permanent part
C     of the SPK file.
C
C     This routine is one of a set of three routines for creating and
C     adding data to type 14 SPK segments. These routines are:
C
C        SPK14B: Begin a type 14 SPK segment. This routine must be
C                called before any data may be added to a type 14
C                segment.
C
C        SPK14A: Add data to a type 14 SPK segment. This routine may be
C                called any number of times after a call to SPK14B to
C                add type 14 records to the SPK segment that was
C                started.
C
C        SPK14E: End a type 14 SPK segment. This routine is called to
C                make the type 14 segment a permanent addition to the
C                SPK file. Once this routine is called, no further type
C                14 records may be added to the segment. A new segment
C                must be started.
C
C     A type 14 SPK segment consists of coefficient sets for fixed order
C     Chebyshev polynomials over consecutive time intervals, where the
C     time intervals need not all be of the same length. The Chebyshev
C     polynomials represent the position, X, Y, and Z coordinates, and
C     the velocities, dX/dt, dY/dt, and dZ/dt, of BODY relative to
C     CENTER.
C
C     The ephemeris data supplied to the type 14 SPK writer is packed
C     into an array as a sequence of logical records,
C
C        -----------------------------------------------------
C        | Record 1 | Record 2 | ... | Record N-1 | Record N |
C        -----------------------------------------------------
C
C     with each record has the following format.
C
C           ------------------------------------------------
C           |  the midpoint of the approximation interval  |
C           ------------------------------------------------
C           |   the radius of the approximation interval   |
C           ------------------------------------------------
C           |  CHBDEG+1 coefficients for the X coordinate  |
C           ------------------------------------------------
C           |  CHBDEG+1 coefficients for the Y coordinate  |
C           ------------------------------------------------
C           |  CHBDEG+1 coefficients for the Z coordinate  |
C           ------------------------------------------------
C           |   CHBDEG+1 coefficients for the X velocity   |
C           ------------------------------------------------
C           |   CHBDEG+1 coefficients for the Y velocity   |
C           ------------------------------------------------
C           |   CHBDEG+1 coefficients for the Z velocity   |
C           ------------------------------------------------
C
C$ Examples
C
C     Assume we have the following for each of the examples that
C     follow.
C
C        HANDLE   is the handle of an SPK file opened with write
C                 access.
C
C        SEGID    is a character string of no more than 40 characters
C                 which provides a pedigree for the data in the SPK
C                 segment we will create.
C
C        BODY     is the SPICE ID code for the body whose ephemeris
C                 is to be placed into the file.
C
C        CENTER   is the center of motion for the ephemeris of BODY.
C
C        REFFRM   is the name of the SPICE inertial reference frame
C                 for the ephemeris.
C
C        FIRST    is the starting epoch, in seconds past J2000, for
C                 the ephemeris data to be placed into the segment.
C
C        LAST     is the ending epoch, in seconds past J2000, for
C                 the ephemeris data to be placed into the segment.
C
C     Example 1:
C
C        For this example, we also assume that:
C
C           N        is the number of type 14 records that we want to
C                    put into a segment in an SPK file.
C
C           RECRDS   contains N type 14 records packaged for the SPK
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
C        type 14 SPK segment if all of the data for the segment is
C        available at one time.
C
C        C
C        C     Begin the segment.
C        C
C              CALL SPK14B ( HANDLE, SEGID, BODY, CENTER, REFFRM,
C             .              FIRST,  LAST,  CHBDEG                )
C        C
C        C     Add the data to the segment all at once.
C        C
C              CALL SPK14A ( HANDLE, N, RECRDS, ETSTRT )
C        C
C        C     End the segment, making the segment a permanent addition
C        C     to the SPK file.
C        C
C              CALL SPK14E ( HANDLE )
C
C     Example 2:
C
C        In this example we want to add type 14 SPK records, as
C        described above in the $ Particulars section, to the segment
C        being written as they are generated.  The ability to write the
C        records in this way is useful if computer memory is limited. It
C        may also be convenient from a programming perspective to write
C        the records one at a time.
C
C        For this example, assume that we want to generate N type 14 SPK
C        records, one for each of N time intervals, writing them all to
C        the same segment in the SPK file. Let
C
C           N        be the number of type 14 records that we want to
C                    generate and put into a segment in an SPK file.
C
C           RECORD   be an array with enough room to hold a single type
C                    14 record, i.e. RECORD should have dimension at
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
C                    be a subroutine that generates a type 14 SPK record
C                    for a time interval specified by TIME1 and TIME2.
C
C        Then the following code fragment demonstrates how to create a
C        type 14 SPK segment if all of the data for the segment is not
C        available at one time.
C
C        C
C        C     Begin the segment.
C        C
C              CALL SPK14B ( HANDLE, SEGID, BODY, CENTER, REFFRM,
C             .              FIRST, LAST, CHBDEG                  )
C
C        C
C        C     Generate the records and write them to the segment in the
C        C     SPK file one at at time.
C        C
C              DO I = 1, N
C
C                 CALL GENREC ( START(I), STOP(I), RECORD )
C                 CALL SPK14A ( HANDLE, 1, RECORD, START(I) )
C
C              END DO
C
C        C
C        C     End the segment, making the segment a permanent addition
C        C     to the SPK file.
C        C
C              CALL SPK14E ( HANDLE )
C
C$ Restrictions
C
C     1) The type 14 SPK segment being closed must have been started by
C        the routine SPK14B, the routine which begins a type 14 SPK
C        segment.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C     See the argument HANDLE.
C
C$ Author_and_Institution
C
C     K.R. Gehringer      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 06-MAR-1995 (KRG)
C
C-&
 
C$ Index_Entries
C
C     end a type_14 spk segment
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
         CALL CHKIN ( 'SPK14E' )
      END IF
 
C
C     This is simple, just call the routine which ends a generic
C     segment.
C
      CALL SGWES ( HANDLE )
 
C
C     No need to check FAILED() here, since all we do is check out.
C     Leave it up to the caller.
 
      CALL CHKOUT ( 'SPK14E' )
      RETURN
 
      END

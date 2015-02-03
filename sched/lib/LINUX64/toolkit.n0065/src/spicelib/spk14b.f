C$Procedure      SPK14B ( SPK type 14: Begin a segment.)
 
      SUBROUTINE SPK14B ( HANDLE, SEGID,
     .                    BODY,   CENTER, FRAME,
     .                    FIRST,  LAST,   CHBDEG  )
 
C$ Abstract
C
C     Begin a type 14 SPK segment in the SPK file associated with
C     HANDLE. See also SPK14A and SPK14E.
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
C     SPK
C
C$ Declarations
 
      IMPLICIT NONE

      INCLUDE 'sgparam.inc'
 
      INTEGER               HANDLE
      CHARACTER*(*)         SEGID
      INTEGER               BODY
      INTEGER               CENTER
      CHARACTER*(*)         FRAME
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      INTEGER               CHBDEG
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   The handle of an SPK file open for writing.
C     SEGID      I   The string to use for segment identifier.
C     BODY       I   The NAIF ID code for the body of the segment.
C     CENTER     I   The center of motion for BODY.
C     FRAME      I   The reference frame for this segment.
C     FIRST      I   The first epoch for which the segment is valid.
C     LAST       I   The last epoch for which the segment is valid.
C     CHBDEG     I   The degree of the Chebyshev Polynomial used.
C
C$ Detailed_Input
C
C     HANDLE   is the file handle of an SPK file that has been
C              opened for writing.
C
C     SEGID    is the segment identifier. An SPK segment identifier
C              may contain up to 40 printing ASCII characters.
C
C     BODY     is the SPICE ID for the body whose states are
C              to be recorded in an SPK file.
C
C     CENTER   is the SPICE ID for the center of motion associated
C              with BODY.
C
C     FRAME    is the reference frame that states are referenced to,
C              for example 'J2000'.
C
C     FIRST    is the starting epoch, in seconds past J2000, for
C              the ephemeris data to be placed into the segment.
C
C     LAST     is the ending epoch, in seconds past J2000, for
C              the ephemeris data to be placed into the segment.
C
C     CHBDEG   is the degree of the Chebyshev Polynomials used to
C              represent the ephemeris information stored in the
C              segment.
C
C$ Detailed_Output
C
C     None.    The input data is used to create the segment summary for
C              the segment being started in the SPK file associated with
C              HANDLE.
C
C              See the $ Particulars section for details about the
C              structure of a type 14 SPK segment.
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
C        be signaled.
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
C      See HANDLE in the $ Detailed_Input section.
C
C$ Particulars
C
C     This routine begins writing a type 14 SPK segment to the open SPK
C     file that is associated with HANDLE. The file must have been
C     opened with write access.
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
C     into an array as a sequence of records,
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
C           |  CHBDEG+1 coefficients for the X coordinate  |
C           ------------------------------------------------
C           |  CHBDEG+1 coefficients for the Y coordinate  |
C           ------------------------------------------------
C           |  CHBDEG+1 coefficients for the Z coordinate  |
C           ------------------------------------------------
C           |  CHBDEG+1 coefficients for the X velocity    |
C           ------------------------------------------------
C           |  CHBDEG+1 coefficients for the Y velocity    |
C           ------------------------------------------------
C           |  CHBDEG+1 coefficients for the Z velocity    |
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
C        REFFRM   is the name of the SPICE reference frame for the
C                 ephemeris.
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
C             .              FIRST,  LAST,  CHBDEG                )
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
C     The SPK file must be open with write access.
C
C     Only one segment may be written to a particular SPK file at a
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
C      B.V. Semenov        (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.2, 10-FEB-2014 (BVS)
C
C        Removed comments from the Declarations section.
C
C-    SPICELIB Version 1.0.1, 30-OCT-2006 (BVS)
C
C        Deleted "inertial" from the FRAME description in the Brief_I/O
C        section of the header.
C
C-    SPICELIB Version 1.0.0, 06-MAR-1995 (KRG)
C
C-&
 
C$ Index_Entries
C
C     begin writing a type_14 spk segment
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
C     DAF ND and NI values for SPK files.
C
      INTEGER               ND
      PARAMETER           ( ND = 2 )
 
      INTEGER               NI
      PARAMETER           ( NI = 6 )
C
C     Length of an SPK descriptor.
C
      INTEGER               NDESCR
      PARAMETER           ( NDESCR =  ND + (NI+1)/2 )
C
C     Length of a state.
C
      INTEGER               NSTATE
      PARAMETER           ( NSTATE = 6 )
C
C     The type of this segment
C
      INTEGER               TYPE
      PARAMETER           ( TYPE = 14 )
C
C     The number of constants:
C
      INTEGER               NCONST
      PARAMETER           ( NCONST = 1 )
C
C     Local variables
C
      DOUBLE PRECISION      DCOEFF
      DOUBLE PRECISION      DESCR(NDESCR)
 
      INTEGER               NCOEFF
      INTEGER               PKTSIZ
C
C     Standard SPICELIB error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPK14B' )
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
         CALL CHKOUT ( 'SPK14B'                                   )
         RETURN
 
      END IF
C
C     Create a descriptor for the segment we are about to write.
C
      CALL SPKPDS ( BODY,  CENTER, FRAME, TYPE, FIRST, LAST, DESCR )
 
      IF ( FAILED() ) THEN
 
         CALL CHKOUT ( 'SPK14B' )
         RETURN
 
      END IF
C
C     We've got a valid descriptor, so compute a few things and begin
C     the segment.
C
      NCOEFF = CHBDEG + 1
      PKTSIZ = NSTATE * NCOEFF + 2
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
 
C
C     No need to check FAILED() here, since all we do is check out.
C     Leave it up to the caller.
C
 
      CALL CHKOUT ( 'SPK14B' )
      RETURN
 
      END

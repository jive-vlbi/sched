C$Procedure      SPKS21 ( S/P Kernel, subset, type 21 )
 
      SUBROUTINE SPKS21 ( HANDLE, BADDR, EADDR, BEGIN, END )
      IMPLICIT NONE
 
C$ Abstract
C
C     Extract a subset of the data in a SPK segment of type 21
C     into a new segment.
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
C     DAF
C     SPK
C     TIME
C
C$ Keywords
C
C     EPHEMERIS
C
C$ Declarations
 
      INCLUDE 'spk21.inc'

      INTEGER               HANDLE
      INTEGER               BADDR
      INTEGER               EADDR
      DOUBLE PRECISION      BEGIN
      DOUBLE PRECISION      END
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of source segment.
C     BADDR      I   Beginning address of source segment.
C     EADDR      I   Ending address of source segment.
C     BEGIN      I   Beginning (initial epoch) of subset.
C     END        I   End (final epoch) of subset.
C
C$ Detailed_Input
C
C     HANDLE,
C     BADDR,
C     EADDR       are the file handle assigned to a SPK file, and the
C                 beginning and ending addresses of a segment within
C                 the file. Together they determine a complete set of
C                 ephemeris data, from which a subset is to be
C                 extracted.
C
C     BEGIN,
C     END         are the initial and final epochs (ephemeris time)
C                 of the subset to be extracted.
C
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  Any errors that occur while reading data from the source SPK
C         file will be diagnosed by routines in the call tree of this
C         routine.
C
C     2)  Any errors that occur while writing data to the output SPK
C         file will be diagnosed by routines in the call tree of this
C         routine.
C
C$ Files
C
C     See argument HANDLE.
C
C$ Particulars
C
C     The exact structure of a segment of data type 21 is detailed in
C     the SPK Required Reading file.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     NAIF Document 168.0, "S- and P- Kernel (SPK) Specification and
C     User's Guide"
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     I.M. Underwood  (JPL)
C     E.D. Wright     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 16-JAN-2014 (NJB) (FTK) (WLT) (IMU)
C
C-&
 
C$ Index_Entries
C
C     subset type_21 spk segment
C
C-&
 
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local parameters
C
      INTEGER               DIRSIZ
      PARAMETER           ( DIRSIZ = 100 )

      INTEGER               MAXDSZ
      PARAMETER           ( MAXDSZ =  (4 * MAXTRM) + 11 )

C
C     Local variables
C
      DOUBLE PRECISION      DATA     ( MAXDSZ )
 
      INTEGER               DLSIZE
      INTEGER               MAXDIM
      INTEGER               NREC
      INTEGER               NDIR
      INTEGER               OFFE
      INTEGER               FIRST
      INTEGER               LAST
      INTEGER               OFFSET
      INTEGER               I
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'SPKS01' )
 
C
C     Get the number of records in the segment. From that, we can
C     compute
C
C        NDIR      The number of directory epochs.
C
C        OFFE      The offset of the first epoch.
C
C
C     the number of directory epochs.
C
      CALL DAFGDA ( HANDLE, EADDR-1, EADDR, DATA )
      
      MAXDIM = NINT( DATA(1) )
      NREC   = NINT( DATA(2) )
 
      NDIR   = NREC / DIRSIZ
      OFFE   = EADDR - NDIR - NREC - 2
 
C
C     Well, the new segment has already been begun. We just have to
C     decide what to move, and move it (using DAFADA).
C
C     Let's agree right now that speed is not of the greatest
C     importance here. We can probably do this with two passes
C     through the record epochs, and one pass through the records.
C
C        1) Determine the first and last records to be included
C           in the subset.
C
C        2) Move the records.
C
C        3) Write the epochs.
C
C     We can leap through the epochs one last time to get the
C     directory epochs.
C
C     First pass: which records are to be moved?
C
      FIRST = 0
      LAST  = 0
 
      DO I = 1, NREC

         CALL DAFGDA ( HANDLE, OFFE+I, OFFE+I, DATA )
 
         IF (       (  FIRST   .EQ. 0     )
     .        .AND. (  DATA(1) .GE. BEGIN ) ) THEN

             FIRST = I

         END IF

         IF (        ( FIRST   .NE. 0   )
     .        .AND.  ( LAST    .EQ. 0   )
     .        .AND.  ( DATA(1) .GE. END )  ) THEN

            LAST = I

         END IF
 
      END DO
 
C
C     Second pass. Move the records.
C
      DLSIZE = ( 4 * MAXDIM ) + 11

      OFFSET = (BADDR - 1) + (FIRST - 1) * DLSIZE
 
      DO I = FIRST, LAST

         CALL DAFGDA ( HANDLE, OFFSET+1, OFFSET+DLSIZE, DATA         )
         CALL DAFADA (                                  DATA, DLSIZE )
 
         OFFSET = OFFSET + DLSIZE

      END DO
 
C
C     Third pass. Move the epochs.
C
      DO I = FIRST, LAST

         CALL DAFGDA ( HANDLE, OFFE+I, OFFE+I, DATA    )
         CALL DAFADA (                         DATA, 1 )

      END DO
 
C
C     Get every DIRSIZ'th epoch for the directory.
C
      DO I = (FIRST + DIRSIZ - 1), LAST, DIRSIZ

         CALL DAFGDA ( HANDLE, OFFE+I, OFFE+I, DATA    )
         CALL DAFADA (                         DATA, 1 )

      END DO
 
C
C     Add the maximum difference line dimension and the 
C     number of records, and we're done.
C
      CALL DAFADA ( DBLE( MAXDIM ), 1 )

      DATA(1) = DBLE ( LAST - FIRST + 1 )

      CALL DAFADA ( DATA, 1 )
 
      CALL CHKOUT ( 'SPKS01' )
      RETURN
      END

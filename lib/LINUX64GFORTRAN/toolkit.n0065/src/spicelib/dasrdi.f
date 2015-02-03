C$Procedure      DASRDI ( DAS, read data, integer )
 
      SUBROUTINE DASRDI ( HANDLE, FIRST, LAST, DATA )
 
C$ Abstract
C
C     Read integer data from a range of DAS logical addresses.
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
C     DAS
C
C$ Keywords
C
C     ARRAY
C     ASSIGNMENT
C     DAS
C     FILES
C
C$ Declarations
 
      INTEGER               HANDLE
      INTEGER               FIRST
      INTEGER               LAST
      INTEGER               DATA   ( * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   DAS file handle.
C     FIRST,
C     LAST       I   Range of DAS integer logical addresses.
C     DATA       O   Data having addresses FIRST through LAST.
C
C$ Detailed_Input
C
C     HANDLE         is a file handle for an open DAS file.
C
C     FIRST,
C     LAST           are a range of DAS integer logical addresses.
C                    FIRST and LAST must be greater than or equal to
C                    1 and less than or equal to the highest integer
C                    logical address in the DAS file designated by
C                    HANDLE.
C
C$ Detailed_Output
C
C     DATA           is an array of integers.  DATA should have length
C                    at least LAST - FIRST + 1.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input file handle is invalid, the error will be
C         diagnosed by routines called by this routine.  DATA will
C         not be modified.
C
C     2)  If FIRST or LAST are out of range, the error will be diagnosed
C         by routines called by this routine.
C
C     3)  If FIRST is greater than LAST, DATA is left unchanged.
C
C     4)  If DATA is declared with length less than FIRST - LAST + 1,
C         the error cannot be diagnosed by this routine.
C
C$ Files
C
C     See the description of the argument HANDLE in $Detailed_Input.
C
C$ Particulars
C
C     This routine provides random read access to the integer data in
C     a DAS file.  This data is logically structured as a
C     one-dimensional array of integers.
C
C$ Examples
C
C
C     1)  Create the new DAS file TEST.DAS and add 200 integers to it.
C         Close the file, then re-open it and read the data back out.
C
C
C                  PROGRAM TEST_READ
C
C                  CHARACTER*(4)         TYPE
C
C                  INTEGER               DATA   ( 200 )
C
C                  INTEGER               FIRST
C                  INTEGER               HANDLE
C                  INTEGER               I
C                  INTEGER               LAST
C            C
C            C     Open a new DAS file.  Use the file name as
C            C     the internal file name.
C            C
C                  TYPE = 'TEST'
C                  CALL DASONW ( 'TEST.DAS', TYPE, 'TEST.DAS', HANDLE )
C
C            C
C            C     Fill the array DATA with the integers 1 through
C            C     100, and add this array to the file.
C            C
C                  DO I = 1, 100
C                     DATA(I) = I
C                  END DO
C
C                  CALL DASADI ( HANDLE, 100, DATA, FIRST, LAST )
C
C            C
C            C     Now append the array DATA to the file again.
C            C
C                  CALL DASADI ( HANDLE, 100, DATA, FIRST, LAST )
C
C            C
C            C     Close the file.
C            C
C                  CALL DASCLS ( HANDLE )
C
C            C
C            C     Now verify the addition of data by opening the
C            C     file for read access and retrieving the data.
C            C
C                  CALL DASRDI ( HANDLE, 1, 200, DATA )
C
C            C
C            C     Dump the data to the screen.  We should see the
C            C     sequence  1, 2, ..., 100, 1, 2, ... , 100.
C            C
C                  WRITE (*,*) ' '
C                  WRITE (*,*) 'Data from TEST.DAS: '
C                  WRITE (*,*) ' '
C                  WRITE (*,*) DATA
C
C                  END
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
C     K.R. Gehringer (JPL)
C     N.J. Bachman   (JPL)
C     W.L. Taber     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.2.1 19-DEC-1995 (NJB)
C
C        Corrected title of permuted index entry section.
C
C-    SPICELIB Version 1.2.0, 30-OCT-1995 (NJB)
C
C        Routine now uses discovery check-in.  FAILED test moved inside
C        loop.
C
C-    SPICELIB Version 1.1.0, 12-MAY-1994 (KRG) (NJB)
C
C        Test of FAILED() added to loop termination condition.
C
C        Removed references to specific DAS file open routines in the
C        $ Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if the DAS open routines ever
C        change.
C
C        Modified the $ Examples section to demonstrate the new ID word
C        format which includes a file type and to include a call to the
C        new routine DASONW, open new for write, which makes use of the
C        file type. Also,  a variable for the type of the file to be
C        created was added.
C
C-    SPICELIB Version 1.0.0, 13-JUN-1992 (NJB) (WLT)
C
C-&
 
 
C$ Index_Entries
C
C     read integer data from a DAS file
C
C-&
 
 
C$ Revisions
C
C-    SPICELIB Version 1.2.0, 30-OCT-1995 (NJB)
C
C        Routine now uses discovery check-in.  FAILED test moved inside
C        loop.
C
C-    SPICELIB Version 1.1.0, 12-MAY-1994 (KRG) (NJB)
C
C        Test of FAILED() added to loop termination condition.  Without
C        this test, an infinite loop could result if DASA2L or DASRRI
C        signaled an error inside the loop.
C
C        Removed references to specific DAS file open routines in the
C        $ Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if the DAS open routines ever
C        change.
C
C        Modified the $ Examples section to demonstrate the new ID word
C        format which includes a file type and to include a call to the
C        new routine DASONW, open new for write, which makes use of the
C        file type. Also,  a variable for the type of the file to be
C        created was added.
C
C-    SPICELIB Version 1.0.0, 13-JUN-1992 (NJB) (WLT)
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               FAILED
 
C
C     Local parameters
C
      INTEGER               NWI
      PARAMETER           ( NWI    =  256 )
 
      INTEGER               INT
      PARAMETER           ( INT    =  3  )
 
C
C     Local variables
C
      INTEGER               CLBASE
      INTEGER               CLSIZE
      INTEGER               N
      INTEGER               NREAD
      INTEGER               NUMINT
      INTEGER               RECNO
      INTEGER               WORDNO
 
 
 
C
C     Find out the physical location of the first integer.  If FIRST
C     is invalid, DASA2L will take care of the problem.
C
      CALL DASA2L ( HANDLE, INT, FIRST, CLBASE, CLSIZE, RECNO, WORDNO )
 
C
C     Decide how many integers to read.
C
      NUMINT = LAST - FIRST + 1
      NREAD  = 0
 
C
C     Read as much data from record RECNO as necessary.
C
      N  =  MIN ( NUMINT,  NWI - WORDNO + 1 )
 
      CALL DASRRI ( HANDLE, RECNO, WORDNO, WORDNO + N-1, DATA )
 
      NREAD  =  N
      RECNO  =  RECNO + 1
 
C
C     Read from as many additional records as necessary.
C
      DO WHILE ( NREAD .LT. NUMINT )
 
         IF ( FAILED() ) THEN
            RETURN
         END IF
 
C
C        At this point, RECNO is the correct number of the
C        record to read from next.  CLBASE is the number
C        of the first record of the cluster we're about
C        to read from.
C
         IF (  RECNO  .LT.  ( CLBASE + CLSIZE )  ) THEN
C
C           We can continue reading from the current
C           cluster.
C
            N  =  MIN ( NUMINT - NREAD,  NWI )
 
            CALL DASRRI ( HANDLE, RECNO, 1, N, DATA(NREAD + 1) )
 
            NREAD   =   NREAD + N
            RECNO   =   RECNO + 1
 
         ELSE
C
C           We must find the next integer cluster to
C           read from.  The first integer in this
C           cluster has address FIRST + NREAD.
C
            CALL DASA2L ( HANDLE,
     .                    INT,
     .                    FIRST + NREAD,
     .                    CLBASE,
     .                    CLSIZE,
     .                    RECNO,
     .                    WORDNO  )
 
         END IF
 
      END DO
 
      RETURN
      END

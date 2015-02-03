C$Procedure      DASRDD ( DAS, read data, double precision )
 
      SUBROUTINE DASRDD ( HANDLE, FIRST, LAST, DATA )
 
C$ Abstract
C
C     Read double precision data from a range of DAS logical addresses.
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
      DOUBLE PRECISION      DATA   ( * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   DAS file handle.
C     FIRST,
C     LAST       I   Range of DAS double precision logical addresses.
C     DATA       O   Data having addresses FIRST through LAST.
C
C$ Detailed_Input
C
C     HANDLE         is a file handle for an open DAS file.
C
C     FIRST,
C     LAST           are a range of DAS double precision logical
C                    addresses.  FIRST and LAST must be greater than or
C                    equal to 1 and less than or equal to the highest
C                    double precision logical address in the DAS file
C                    designated by HANDLE.
C
C$ Detailed_Output
C
C     DATA           is an array of double precision numbers.  DATA
C                    should have length at least LAST - FIRST + 1.
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
C     This routine provides random read access to the double precision
C     data in a DAS file.  This data is logically structured as a
C     one-dimensional array of double precision numbers.
C
C$ Examples
C
C     1)  Create the new DAS file TEST.DAS and add 200 double
C         precision numbers to it.  Close the file, then re-open
C         it and read the data back out.
C
C                  PROGRAM TEST_READ
C
C                  CHARACTER*(4)         TYPE
C
C                  DOUBLE PRECISION      DATA   ( 200 )
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
C            C     Fill the array DATA with the double precision
C            C     numbers 1.D0 through 100.D0, and add this array
C            C     to the file.
C            C
C                  DO I = 1, 100
C                     DATA(I) = DBLE(I)
C                  END DO
C
C                  CALL DASADD ( HANDLE, 100, DATA, FIRST, LAST )
C
C            C
C            C     Now append the array DATA to the file again.
C            C
C                  CALL DASADD ( HANDLE, 100, DATA, FIRST, LAST )
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
C                  CALL DASRDD ( HANDLE, 1, 200, DATA )
C
C            C
C            C     Dump the data to the screen.  We should see the
C            C     sequence  1, 2, ..., 100, 1, 2, ... , 100.  The
C            C     numbers will be represented as double precision
C            C     numbers in the output.
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
C-    SPICELIB Version 1.1.1 19-DEC-1995 (NJB)
C
C        Corrected title of permuted index entry section.
C
C-    SPICELIB Version 1.2.0, 01-NOV-1995 (NJB)
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
C     read double precision data from a DAS file
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
C        this test, an infinite loop could result if DASA2L or DASRRD
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
      INTEGER               NWD
      PARAMETER           ( NWD   =  128 )
 
      INTEGER               DP
      PARAMETER           ( DP     =  2  )
 
 
C
C     Local variables
C
      INTEGER               CLBASE
      INTEGER               CLSIZE
      INTEGER               N
      INTEGER               NREAD
      INTEGER               NUMDP
      INTEGER               RECNO
      INTEGER               WORDNO
 
 
 
C
C     Find out the physical location of the first double precision
C     number.  If FIRST is invalid, DASA2L will take care of the
C     problem.
C
      CALL DASA2L ( HANDLE, DP, FIRST, CLBASE, CLSIZE, RECNO, WORDNO )
 
C
C     Decide how many double precision numbers to read.
C
      NUMDP = LAST - FIRST + 1
      NREAD = 0
 
C
C     Read as much data from record RECNO as necessary.
C
      N   =   MIN ( NUMDP,  NWD - WORDNO + 1 )
 
      CALL DASRRD ( HANDLE, RECNO, WORDNO, WORDNO + N-1, DATA )
 
      NREAD  =  N
      RECNO  =  RECNO + 1
 
C
C     Read from as many additional records as necessary.
C
      DO WHILE ( NREAD .LT. NUMDP )
 
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
            N  =  MIN ( NUMDP - NREAD,  NWD )
 
            CALL DASRRD ( HANDLE, RECNO, 1, N, DATA(NREAD + 1) )
 
            NREAD   =   NREAD + N
            RECNO   =   RECNO + 1
 
         ELSE
C
C           We must find the next double precision cluster to
C           read from.  The first double precision number in this
C           cluster has address FIRST + NREAD.
C
            CALL DASA2L ( HANDLE,
     .                    DP,
     .                    FIRST + NREAD,
     .                    CLBASE,
     .                    CLSIZE,
     .                    RECNO,
     .                    WORDNO  )
 
         END IF
 
      END DO
 
      RETURN
      END

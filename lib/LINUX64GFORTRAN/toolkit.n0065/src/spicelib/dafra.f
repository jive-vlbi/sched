C$Procedure DAFRA ( DAF, Re-order arrays )
 
      SUBROUTINE DAFRA ( HANDLE, IORDER, N )
 
C$ Abstract
C
C     Re-order the arrays in a DAF according to a given order
C     vector.
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
C
C$ Keywords
C
C     FILES
C     SORT
C
C$ Declarations
 
      INTEGER               HANDLE
      INTEGER               IORDER ( * )
      INTEGER               N
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of DAF.
C     IORDER     I   Order vector.
C     N          I   Dimension of IORDER.
C
C$ Detailed_Input
C
C     HANDLE      is the handle of a DAF that has been opened for
C                 write access.  Use DAFOPW, for example, to open
C                 an existing file and get its handle.
C
C     IORDER      is the order vector to be used to re-order the
C                 arrays stored in the DAF specified by HANDLE.
C
C                 An integer order vector is an array of length
C                 N whose elements are the integers 1 through N.
C
C                 The first element of IORDER is the index of the
C                 first array in the re-ordered file, and so on.
C
C     N           is the number of elements in the order vector.
C                 This may be less than the number of arrays in
C                 the file.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C     DAFRA does not actually move the elements of the double
C     precision arrays; it works by rearranging the contents
C     of the summary and name records in the file. The result
C     is that the search routines (BFS, FNA, BBS, FPA) will
C     return the arrays in the indicated order.
C
C     After re-ordering, array IORDER(1) of the input file is the
C     first array of the output file, array IORDER(2) of the input
C     file is the second array of the output file, and so on.
C
C     The order vector used by DAFRA is typically created for
C     a related array by one of the ORDER routines, as shown in
C     the example below.
C
C$ Examples
C
C     The following code fragment sorts the arrays in a DAF by name.
C
C        C
C        C     Collect the names of the arrays in the file.
C        C
C              CALL DAFOPW ( FILE, HANDLE )
C
C              N = 0
C              CALL DAFBFS ( HANDLE )
C              CALL DAFFNA ( FOUND  )
C
C              DO WHILE ( FOUND )
C                 N = N + 1
C                 CALL DAFGN  ( NAMES(I) )
C                 CALL DAFFNA ( FOUND    )
C              END DO
C
C        C
C        C     Sort the names.
C        C
C              CALL ORDERC ( NAMES, N, IORDER )
C
C        C
C        C     Re-order the arrays.
C        C
C              CALL DARFA  ( HANDLE, IORDER, N )
C              CALL DAFCLS ( HANDLE            )
C
C     Afterward, a forward search like the one shown below
C
C        CALL DAFBFS ( HANDLE )
C        CALL DAFFNA ( FOUND  )
C
C        DO WHILE ( FOUND )
C           CALL DAFGN ( NAME )
C           WRITE (*,*) NAME
C
C           CALL DAFFNA ( FOUND )
C        END DO
C
C     produces an ordered list of the names in the sorted file.
C
C$ Restrictions
C
C     None.
C
C$ Exceptions
C
C     1) If IORDER is not an order vector (that is, if it does
C        not contain every integer between 1 and N), the error
C        SPICE(DISORDER) is signalled.
C
C     2) If N is greater than the number of arrays in the file,
C        the error SPICE(DISARRAY) is signalled.
C
C$ Files
C
C     See argument HANDLE.
C
C$ Author_and_Institution
C
C     I.M. Underwood  (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 28-MAR-1991 (IMU)
C
C-&
 
C$ Index_Entries
C
C     reorder daf arrays
C
C-&
 
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               ISORDV
      LOGICAL               RETURN
 
C
C     Local parameters
C
      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 1000 )
 
      INTEGER               SUMLEN
      PARAMETER           ( SUMLEN =  128 )
 
C
C     Local variables
C
      CHARACTER*(NAMLEN)    HOLDN
      CHARACTER*(NAMLEN)    TEMPN
 
      DOUBLE PRECISION      HOLDS    ( SUMLEN )
      DOUBLE PRECISION      TEMPS    ( SUMLEN )
 
      INTEGER               HOLD
      INTEGER               I
      INTEGER               INDEX
      INTEGER               START
      INTEGER               TOTAL
 
      LOGICAL               FOUND
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFRA' )
      END IF
 
C
C     If the order vector has fewer than two elements, don't bother.
C
      IF ( N .LT. 2 ) THEN
        CALL CHKOUT ( 'DAFRA' )
        RETURN
      END IF
 
C
C     If IORDER is not an order vector, complain.
C
      IF ( .NOT. ISORDV ( IORDER, N ) ) THEN
         CALL SETMSG ( 'Sorry, IORDER is not an order vector.' )
         CALL SIGERR ( 'SPICE(DISORDER)' )
         CALL CHKOUT ( 'DAFRA' )
         RETURN
      END IF
 
C
C     If the number of arrays to be moved exceeds the number of
C     arrays in the file, complain.
C
      TOTAL = 0
 
      CALL DAFBFS ( HANDLE )
      CALL DAFFNA ( FOUND  )
 
      DO WHILE ( FOUND  .AND.  ( .NOT. FAILED() ) )
         TOTAL = TOTAL + 1
         CALL DAFFNA ( FOUND )
      END DO
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DAFRA' )
         RETURN
 
      ELSE IF ( TOTAL .LT. N ) THEN
         CALL SETMSG ( 'N (#) exceeds number of arrays (#).' )
         CALL ERRINT ( '#', N )
         CALL ERRINT ( '#', TOTAL )
         CALL SIGERR ( 'SPICE(DISARRAY)' )
         CALL CHKOUT ( 'DAFRA' )
         RETURN
      END IF
 
 
 
C
C     Not surprisingly, this routine is patterned closely after the
C     (original) REORDx routines in SPICELIB. The only differences
C     are that
C
C        1) This routine is not error free---it checks to make
C           sure that IORDER is in fact an order vector, and that
C           every element in IORDER refers to an existing array.
C
C        2) Instead of moving elements of an array in and out of
C           a temporary location, it moves summaries and names.
C           This means that two sets of temporary storage locations
C           are needed: one to hold the summary and name of the
C           guy who began the current cycle; and one to hold the guy
C           being moved from location HOLD to location INDEX.
C
      START = 1
 
      DO WHILE ( ( START .LT. N )  .AND.  ( .NOT. FAILED() ) )
 
C
C        Start the cycle. One guy (pair of summary and name record)
C        has to sit out (in HOLDS and HOLDN) until the end of the cycle
C        is reached.
C
         INDEX = START
         HOLD  = IORDER(INDEX)
 
         CALL DAFBFS ( HANDLE )
 
         DO I = 1, INDEX
            CALL DAFFNA ( FOUND )
         END DO
 
         CALL DAFGS ( HOLDS )
         CALL DAFGN ( HOLDN )
 
C
C        Move guys from HOLD to INDEX; then update HOLD (to point
C        to the next guy to be moved) and INDEX (to point at the
C        space just vacated).
C
C        Keep going until HOLD points to the first guy moved during
C        the current cycle. This ends the cycle.
C
         DO WHILE ( HOLD .NE. START )
 
C
C           Get the guy in position HOLD.
C
            CALL DAFBFS ( HANDLE )
 
            DO I = 1, HOLD
               CALL DAFFNA ( FOUND )
            END DO
 
            CALL DAFGS ( TEMPS )
            CALL DAFGN ( TEMPN )
 
C
C           Move him to position INDEX. (Note that DAFWS is used to
C           update the summary instead of DAFRS, because the addresses
C           are actually being changed.)
C
            CALL DAFBFS ( HANDLE )
 
            DO I = 1, INDEX
               CALL DAFFNA ( FOUND )
            END DO
 
            CALL DAFWS ( TEMPS )
            CALL DAFRN ( TEMPN )
 
C
C           Update HOLD and INDEX.
C
            INDEX              =  HOLD
            HOLD               =  IORDER(HOLD)
            IORDER(INDEX)      = -IORDER(INDEX)
 
         END DO
 
C
C        The last element in the cycle is restored from TEMP.
C
         CALL DAFBFS ( HANDLE )
 
         DO I = 1, INDEX
            CALL DAFFNA ( FOUND )
         END DO
 
         CALL DAFWS ( HOLDS )
         CALL DAFRN ( HOLDN )
 
         IORDER(HOLD) = -IORDER(HOLD)
 
C
C        Begin the next cycle at the next element in the order
C        vector with a positive sign. (That is, the next one
C        that hasn't been moved.)
C
         DO WHILE ( IORDER(START) .LT. 0  .AND.  START .LT. N )
            START = START + 1
         END DO
 
      END DO
 
C
C     Restore the original signs of the elements of the order
C     vector, for the next go around.
C
      DO INDEX = 1, N
        IORDER(INDEX) = IABS ( IORDER(INDEX) )
      END DO
 
      CALL CHKOUT ( 'DAFRA' )
      RETURN
      END
 

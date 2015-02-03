 
C$Procedure DAFWDA ( DAF, write data to address )
 
      SUBROUTINE DAFWDA ( HANDLE, BEGIN, END, DATA )
 
C$ Abstract
C
C     Write or rewrite the double precision data bounded by two
C     addresses within a DAF.
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
C
C$ Declarations
 
      INTEGER               HANDLE
      INTEGER               BEGIN
      INTEGER               END
      DOUBLE PRECISION      DATA     ( * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of a DAF.
C     BEGIN,
C     END        I   Initial, final address within file.
C     DATA       I   Data to be stored between BEGIN and END.
C
C$ Detailed_Input
C
C     HANDLE      is the handle of a DAF.
C
C     BEGIN,
C     END         are the initial and final addresses of a contiguous
C                 set of double precision numbers within a DAF.
C                 Presumably, these make up all or part of a
C                 particular array.
C
C     DATA        are the double precision data to be stored between
C                 the specified addresses within the specified file.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C      None.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     1) If BEGIN is zero or negative, the error SPICE(DAFNEGADDR)
C        is signalled.
C
C     1) If the BEGIN > END, the error SPICE(DAFBEGGTEND)
C        is signalled.
C
C$ Particulars
C
C     The principal reason that DAFs are so easy to use is that
C     the data in each DAF are considered to be one long contiguous
C     set of double precision numbers. You can store data anywhere
C     within a DAF without knowing (or caring) about the physical
C     records in which they are stored.
C
C     Of course, if you are merely adding arrays to a DAF,
C     you should not use DAFWDA directly, but should use DAFANA
C     (add new array) and its entry points, since these update
C     the appropriate bookkeeping records automatically.
C
C$ Examples
C
C     The following code fragment illustrates the use of DAFWDA
C     to update an imaginary array. The array begins with a directory
C     containing 11 epochs. Each pair of epochs bounds an
C     interval, and each interval is covered by a set of eight
C     osculating elements.
C
C     By accident, the elements were written with the wrong value for
C     the GM of the central body (the last element in each set). Each
C     set must be retrieved, updated,and rewritten.
C
C        CALL DAFUS ( SUM, ND, NI, DC, IC )
C        BEGIN = IC(5)
C
C        DO I = 1, 10
C           OFFSET = BEGIN + 11 + (I - 1) * 8
C
C           CALL DAFRDA ( HANDLE, OFFSET+1, OFFSET+8, ELEMENTS )
C           ELEMENTS(8) = NEW_GM
C
C           CALL DAFWDA ( HANDLE, OFFSET+1, OFFSET+8, ELEMENTS )
C        END DO
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     NAIF Document 167.0, "Double Precision Array Files (DAF)
C     Specification and User's Guide"
C
C$ Author_and_Institution
C
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
C
C        Literature references added to the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&
 
C$ Index_Entries
C
C     write data to daf address
C
C-&
 
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local variables
C
      INTEGER               BSIZE
      PARAMETER           ( BSIZE = 128 )
 
      INTEGER               BEGR
      INTEGER               BEGW
      INTEGER               ENDR
      INTEGER               ENDW
 
      INTEGER               RECNO
      DOUBLE PRECISION      BUFFER   ( BSIZE )
      INTEGER               FIRST
      INTEGER               N
      INTEGER               NEXT
      LOGICAL               FOUND
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFWDA' )
      END IF
 
C
C     Bad addresses?
C
      IF ( BEGIN .LE. 0 ) THEN
         CALL SETMSG ( 'Negative beginning address: #' )
         CALL ERRINT ( '#', BEGIN )
         CALL SIGERR ( 'SPICE(DAFNEGADDR)' )
         CALL CHKOUT ( 'DAFWDA' )
         RETURN
 
      ELSE IF ( BEGIN .GT. END ) THEN
         CALL SETMSG ( 'Beginning address (#) greater than ending '   //
     .                 'address (#)' )
         CALL ERRINT ( '#', BEGIN )
         CALL ERRINT ( '#', END   )
         CALL SIGERR ( 'SPICE(DAFBEGGTEND)' )
         CALL CHKOUT ( 'DAFWDA' )
         RETURN
      END IF
 
C
C     Convert raw addresses to record/word representations.
C
      CALL DAFARW ( BEGIN, BEGR, BEGW )
      CALL DAFARW ( END,   ENDR, ENDW )
 
C
C     The first and last records may have to be read, updated, and
C     rewritten. Any records in between may be written directly.
C
      NEXT = 1
 
      DO RECNO = BEGR, ENDR
 
         IF ( RECNO .EQ. BEGR  .OR.  RECNO .EQ. ENDR ) THEN
            CALL DAFRDR ( HANDLE, RECNO, 1, BSIZE, BUFFER, FOUND )
 
            IF ( .NOT. FOUND ) THEN
               CALL CLEARD ( BSIZE, BUFFER )
            END IF
         END IF
 
         IF ( BEGR .EQ. ENDR ) THEN
            FIRST = BEGW
            N     = ENDW - BEGW + 1
 
         ELSE IF ( RECNO .EQ. BEGR ) THEN
            FIRST = BEGW
            N     = BSIZE - BEGW + 1
 
         ELSE IF ( RECNO .EQ. ENDR ) THEN
            FIRST = 1
            N     = ENDW
 
         ELSE
            FIRST = 1
            N     = BSIZE
         END IF
 
         CALL MOVED ( DATA(NEXT), N, BUFFER(FIRST) )
         NEXT = NEXT + N
 
         CALL DAFWDR ( HANDLE, RECNO, BUFFER )
 
      END DO
 
      CALL CHKOUT ( 'DAFWDA' )
      RETURN
      END
 

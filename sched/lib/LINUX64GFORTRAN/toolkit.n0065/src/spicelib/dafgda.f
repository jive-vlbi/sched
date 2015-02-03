C$Procedure DAFGDA ( DAF, read data from address )
 
      SUBROUTINE DAFGDA ( HANDLE, BEGIN, END, DATA )
 
C$ Abstract
C
C     Read the double precision data bounded by two addresses within
C     a DAF.
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
C     DATA       O   Data contained between BEGIN and END.
C
C$ Detailed_Input
C
C     HANDLE      is the handle of a DAF.
C
C     BEGIN,
C     END         are the initial and final addresses of a contiguous
C                 set of double precision numbers within a DAF.
C                 Presumably, these make up all or part of a particular
C                 array.
C
C$ Detailed_Output
C
C     DATA        are the double precision data contained between
C                 the specified addresses within the specified file.
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
C     2) If the BEGIN > END, the error SPICE(DAFBEGGTEND)
C        is signalled.
C
C     3) If HANDLE is invalid, routines in the call tree of DAFGDA
C        signal an appropriate error.
C
C     4) If the range of addresses covered between BEGIN and END
C        includes records that do not contain strictly double
C        precision data, then the values returned in DATA are
C        undefined.  See the Restrictions section below for details.
C
C$ Particulars
C
C     The principal reason that DAFs are so easy to use is that
C     the data in each DAF are considered to be one long contiguous
C     set of double precision numbers. You can grab data from anywhere
C     within a DAF without knowing (or caring) about the physical
C     records in which they are stored.
C
C     This routine replaces DAFRDA as the principle mechanism for
C     reading the contents of DAF arrays.
C
C$ Examples
C
C     The following code fragment illustrates the use of DAFGDA
C     to read data from an imaginary array. The array begins with a
C     directory containing 11 epochs. Each pair of epochs bounds
C     an interval, and each interval is covered by a set of eight
C     osculating elements.
C
C        CALL DAFUS ( SUM, ND, NI, DC, IC )
C        BEGIN = IC(5)
C        END   = IC(6)
C
C        CALL DAFGDA ( HANDLE, BEGIN, BEGIN+10, EPOCHS )
C
C        DO I = 1, 10
C           IF ( ET .GE. EPOCHS(I)  .AND.  ET .LE. EPOCHS(I+1) ) THEN
C              OFFSET = 11 + (I - 1) * 8
C
C              CALL DAFGDA ( HANDLE, OFFSET+1, OFFSET+8, ELEMENTS )
C              RETURN
C           END IF
C        END DO
C
C
C$ Restrictions
C
C     1) There are several types of records in a DAF.  This routine
C        is only to be used to read double precision data bounded
C        between two DAF addresses.  The range of addresses input
C        may not cross data and summary record boundaries.
C
C$ Literature_References
C
C     NAIF Document 167.0, "Double Precision Array Files (DAF)
C     Specification and User's Guide"
C
C$ Author_and_Institution
C
C     F.S. Turner     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 16-NOV-2001 (FST)
C
C-&
 
C$ Index_Entries
C
C     read data from daf address
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
      INTEGER               FIRST
      INTEGER               LAST
      INTEGER               NEXT
 
      LOGICAL               FOUND
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF
 
C
C     Bad addresses?
C
      IF ( BEGIN .LE. 0 ) THEN
         CALL CHKIN  ( 'DAFGDA' )
         CALL SETMSG ( 'Negative value for BEGIN address: #' )
         CALL ERRINT ( '#', BEGIN )
         CALL SIGERR ( 'SPICE(DAFNEGADDR)' )
         CALL CHKOUT ( 'DAFGDA' )
         RETURN
 
      ELSE IF ( BEGIN .GT. END ) THEN
         CALL CHKIN  ( 'DAFGDA' )
         CALL SETMSG ( 'Beginning address (#) greater than ending '   //
     .                 'address (#).' )
         CALL ERRINT ( '#', BEGIN )
         CALL ERRINT ( '#', END   )
         CALL SIGERR ( 'SPICE(DAFBEGGTEND)' )
         CALL CHKOUT ( 'DAFGDA' )
         RETURN
      END IF
 
C
C     Convert raw addresses to record/word representations.
C
      CALL DAFARW ( BEGIN, BEGR, BEGW )
      CALL DAFARW ( END,   ENDR, ENDW )
 
C
C     Get as many records as needed. Return the last part of the
C     first record, the first part of the last record, and all of
C     every record in between. Any record not found is assumed to
C     be filled with zeros.
C
      NEXT = 1
 
      DO RECNO = BEGR, ENDR
 
         IF ( BEGR .EQ. ENDR ) THEN
            FIRST = BEGW
            LAST  = ENDW
 
         ELSE IF ( RECNO .EQ. BEGR ) THEN
            FIRST = BEGW
            LAST  = BSIZE
 
         ELSE IF ( RECNO .EQ. ENDR ) THEN
            FIRST = 1
            LAST  = ENDW
 
         ELSE
            FIRST = 1
            LAST  = BSIZE
         END IF
 
         CALL DAFGDR ( HANDLE, RECNO, FIRST, LAST, DATA( NEXT ), FOUND )
 
         IF ( .NOT. FOUND ) THEN
            CALL CLEARD ( LAST-FIRST+1, DATA( NEXT ) )
         END IF
 
         NEXT = NEXT + ( LAST-FIRST+1 )
 
      END DO
 
      RETURN
      END

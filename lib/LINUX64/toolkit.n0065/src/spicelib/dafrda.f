C$Procedure DAFRDA ( DAF, read data from address )
 
      SUBROUTINE DAFRDA ( HANDLE, BEGIN, END, DATA ) 
 
C$ Abstract
C
C     Read the double precision data bounded by two addresses within
C     a DAF.
C
C     Deprecated: This routine has been superseded by DAFGDA and
C     DAFGSR.  This routine is supported for purposes of backward
C     compatibility only.
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
C$ Exceptions
C
C     1) If BEGIN is zero or negative, the error SPICE(DAFNEGADDR)
C        is signalled.
C
C     2) If the BEGIN > END, the error SPICE(DAFBEGGTEND)
C        is signalled.
C
C     3) If the file associated with HANDLE is not of the native
C        binary file format this routine signals the error
C        SPICE(UNSUPPORTEDBFF).
C
C     4) If HANDLE is invalid, routines in the call tree of DAFRDA
C        signal an appropriate error.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The principal reason that DAFs are so easy to use is that
C     the data in each DAF are considered to be one long contiguous
C     set of double precision numbers. You can grab data from anywhere
C     within a DAF without knowing (or caring) about the physical
C     records in which they are stored.
C
C     This routine has been made obsolete by the routines DAFGDA and
C     DAFGSR.  This routine is supported for reasons of backward
C     compatibility only.  New software development should utilize
C     DAFGDA or DAFGSR.
C
C$ Examples
C
C     The following code fragment illustrates the use of DAFRDA
C     to read data from an imaginary array. The array begins with a
C     directory containing 11 epochs. Each pair of epochs bounds
C     an interval, and each interval is covered by a set of eight
C     osculating elements.
C
C        CALL DAFUS ( SUM, ND, NI, DC, IC )
C        BEGIN = IC(5)
C        END   = IC(6)
C
C        CALL DAFRDA ( HANDLE, BEGIN, BEGIN+10, EPOCHS )
C
C        DO I = 1, 10
C           IF ( ET .GE. EPOCHS(I)  .AND.  ET .LE. EPOCHS(I+1) ) THEN
C              OFFSET = IC(5) + 11 + (I - 1) * 8
C
C              CALL DAFRDA ( HANDLE, OFFSET+1, OFFSET+8, ELEMENTS )
C              RETURN
C           END IF
C        END DO
C
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
C     R.E. Thurman    (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.2, 18-MAY-2010 (BVS) 
C
C        Index line now states that this routine is deprecated.
C
C-    SPICELIB Version 2.0.1, 27-OCT-2003 (NJB)
C
C        The header now states that this routine is deprecated.
C        The Exceptions header section has been extended.
C        Minor additional header updates were made.
C
C-    SPICELIB Version 2.0.0, 16-NOV-2001 (FST)
C
C        Added SPICE(UNSUPPORTEDBFF) exception to the routine.
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
C     DEPRECATED read data from daf address
C
C-&
 
 
C$ Revisions
C
C-    SPICELIB Version 2.0.0, 16-NOV-2001 (FST)
C
C        The exception SPICE(UNSUPPORTEDBFF) was added to guarantee
C        this routine's functionality remains unchanged as a result
C        of the updates to the underlying DAF software's utilization of
C        the handle manager.  In versions of the Toolkit prior to this,
C        all DAFs loaded were of the native binary file format.
C        While rather unlikely, this routine could be used to read
C        the contents of summary records in addition to the usual
C        data records.  The non-native to native translation process
C        for these two different types of records in general are not
C        the same.  Rather than attempt to interpret the caller's
C        intent, this routine is deprecated and restricted to
C        functioning only on DAFs of the native binary file format.
C
C-    Beta Version 1.1.0, 1-NOV-1989 (RET)
C
C        DAFRDA now only checks in and checks out if one of the two
C        possible exceptions occurs. The purpose of this change was to
C        help speed up a routine that gets called constantly by higher
C        level DAF routines.
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
      LOGICAL               NATIVE
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF
 
C
C     Check to see if HANDLE is associated with a DAF of the native
C     binary file format.
C
      CALL ZZDDHISN ( HANDLE, NATIVE, FOUND )
 
C
C     If the HANDLE was located, then check whether the binary file
C     format is native.  Otherwise, defer diagnosing the missing
C     handle to DAFRDR.
C
      IF ( ( FOUND ) .AND. ( .NOT. NATIVE ) ) THEN
         CALL CHKIN  ( 'DAFRDA'                                 )
         CALL SETMSG ( 'The binary file format for file ''#'' '
     .   //            'is not native. This routine operates '
     .   //            'only on files of the native format.'    )
         CALL ERRHAN ( '#', HANDLE                              )
         CALL SIGERR ( 'SPICE(UNSUPPORTEDBFF)'                  )
         CALL CHKOUT ( 'DAFRDA'                                 )
         RETURN
      END IF
 
C
C     Bad addresses?
C
      IF ( BEGIN .LE. 0 ) THEN
         CALL CHKIN  ( 'DAFRDA' )
         CALL SETMSG ( 'Negative value for BEGIN address: #' )
         CALL ERRINT ( '#', BEGIN )
         CALL SIGERR ( 'SPICE(DAFNEGADDR)' )
         CALL CHKOUT ( 'DAFRDA' )
         RETURN
 
      ELSE IF ( BEGIN .GT. END ) THEN
         CALL CHKIN  ( 'DAFRDA' )
         CALL SETMSG ( 'Beginning address (#) greater than ending '   //
     .                 'address (#).' )
         CALL ERRINT ( '#', BEGIN )
         CALL ERRINT ( '#', END   )
         CALL SIGERR ( 'SPICE(DAFBEGGTEND)' )
         CALL CHKOUT ( 'DAFRDA' )
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
 
         CALL DAFRDR ( HANDLE, RECNO, FIRST, LAST, DATA( NEXT ), FOUND )
 
         IF ( .NOT. FOUND ) THEN
            CALL CLEARD ( LAST-FIRST+1, DATA( NEXT ) )
         END IF
 
         NEXT = NEXT + ( LAST-FIRST+1 )
 
      END DO
 
      RETURN
      END

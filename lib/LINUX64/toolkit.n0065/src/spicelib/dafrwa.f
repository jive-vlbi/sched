 
C$Procedure DAFRWA ( DAF, record/word to address )
 
      SUBROUTINE DAFRWA ( RECNO, WORDNO, ADDR )
 
C$ Abstract
C
C     Convert a record/word pair to its equivalent address within
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
C     CONVERSION
C     FILES
C
C$ Declarations
 
      INTEGER               RECNO
      INTEGER               WORDNO
      INTEGER               ADDR
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     RECNO,
C     WORDNO     I   Record, word numbers of a location within DAF.
C     ADDR       O   Corresponding address.
C
C$ Detailed_Input
C
C     RECNO,
C     WORDNO      are the record and word numbers of an arbitrary
C                 location within a DAF.
C
C$ Detailed_Output
C
C     ADDR        is the corresponding address within the DAF.
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
C     1) If either RECNO or WORDNO is zero or negative, the error
C        SPICE(DAFNOSUCHADDR) is signalled.
C
C$ Particulars
C
C     To the user, the data in a DAF appear to be a contiguous
C     collection of double precision numbers, each of which has an
C     address. To the DAF software, however, the data appear to be
C     a collection of records, each containing 128 double precision
C     words. The routines DAFARW and DAFRWA translate between these
C     two representations.
C
C$ Examples
C
C     Routines DAFRDA and DAFWDA illustrate the use of DAFARW and
C     DAFRWA.
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
C     record/word to daf address
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
 
      ELSE IF ( RECNO .LE. 0  .OR.  WORDNO .LE. 0 ) THEN
         CALL CHKIN  ( 'DAFRWA'                           )
         CALL SETMSG ( 'No address for record #, word #.' )
         CALL ERRINT ( '#', RECNO                         )
         CALL ERRINT ( '#', WORDNO                        )
         CALL SIGERR ( 'SPICE(DAFNOSUCHADDR)'             )
         CALL CHKOUT ( 'DAFRWA'                           )
         RETURN
      END IF
 
C
C     If the record and word numbers are legal, the computation is
C     straightforward.
C
      ADDR = WORDNO + ( RECNO - 1 ) * 128
 
      RETURN
 
 
 
C$Procedure DAFARW ( DAF, address to record/word )
 
      ENTRY DAFARW ( ADDR, RECNO, WORDNO )
 
C$ Abstract
C
C     Convert an address within a DAF to its equivalent
C     record/word representation.
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
C     CONVERSION
C     FILES
C
C$ Declarations
C
C     INTEGER               ADDR
C     INTEGER               RECNO
C     INTEGER               WORDNO
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ADDR       I   Address within DAF.
C     RECNO,
C     WORDNO     O   Corresponding record, word numbers.
C
C$ Detailed_Input
C
C     ADDR        is an arbitrary address within a DAF.
C
C$ Detailed_Output
C
C     RECNO,
C     WORDNO      are the corresponding record and word numbers
C                 within the DAF.
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
C     1) If ADDR is zero or negative, the error SPICE(DAFNOSUCHADDR)
C        is signalled.
C
C$ Particulars
C
C     To the user, the data in a DAF appear to be a contiguous
C     collection of double precision numbers, each of which has an
C     address. To the DAF software, however, the data appear to be
C     a collection of records, each containing 128 double precision
C     words. The routines DAFARW and DAFRWA translate between these
C     two representations.
C
C$ Examples
C
C     Routines DAFRDA and DAFWDA illustrate the use of DAFARW and
C     DAFRWA.
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
C     daf address to record/word
C
C-&
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
 
      ELSE IF ( ADDR .LE. 0 ) THEN
         CALL CHKIN  ( 'DAFARW'                         )
         CALL SETMSG ( 'No record, word for address #.' )
         CALL ERRINT ( '#', ADDR                        )
         CALL SIGERR ( 'SPICE(DAFNOSUCHADDR)'           )
         CALL CHKOUT ( 'DAFARW'                         )
         RETURN
      END IF
 
C
C     If the address is legal, the computation is straightforward.
C
      RECNO  =        ( ADDR  - 1 ) / 128 + 1
      WORDNO = ADDR - ( RECNO - 1 ) * 128
 
      RETURN
      END

C$Procedure      DASRDC ( DAS, read data, character )
 
      SUBROUTINE DASRDC ( HANDLE, FIRST, LAST, BPOS, EPOS, DATA )
 
C$ Abstract
C
C     Read character data from a range of DAS logical addresses.
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
      INTEGER               BPOS
      INTEGER               EPOS
      CHARACTER*(*)         DATA   ( * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   DAS file handle.
C     FIRST,
C     LAST       I   Range of DAS character logical addresses.
C     BPOS,
C     EPOS       I   Begin and end positions of substrings.
C     DATA       O   Data having addresses FIRST through LAST.
C
C$ Detailed_Input
C
C     HANDLE         is a file handle for an open DAS file.
C
C     FIRST,
C     LAST           are a range of DAS character logical addresses.
C                    FIRST and LAST must be greater than or equal to
C                    1 and less than or equal to the highest character
C                    logical address in the DAS file designated by
C                    HANDLE.
C
C     BPOS,
C     EPOS           are begin and end character positions that define
C                    the substrings of the elements of the output array
C                    DATA into which character data is to be read.
C
C$ Detailed_Output
C
C     DATA           is an array of character strings.  On output, the
C                    character words in the logical address range
C                    FIRST through LAST are copied into the characters
C
C                       DATA(1)(BPOS:BPOS),
C                       DATA(1)(BPOS+1:BPOS+1),
C                                   .
C                                   .
C                                   .
C                       DATA(1)(EPOS:EPOS),
C                       DATA(2)(BPOS:BPOS),
C                       DATA(2)(BPOS+1:BPOS+1),
C                                   .
C                                   .
C                                   .
C
C                     in that order.
C
C                    DATA must have dimension at least
C
C                       ( LAST - FIRST + L ) / L
C
C                    where
C
C                       L  =  EPOS - BPOS + 1
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
C     2)  If EPOS or BPOS are outside of the range
C         [  1,  LEN( DATA(1) )  ],  or if EPOS < BPOS, the error
C         SPICE(BADSUBSTRINGBOUNDS) will be signalled.
C
C     3)  If FIRST or LAST are out of range, the error will be diagnosed
C         by routines called by this routine.  DATA will not be
C         modified.
C
C     4)  If FIRST is greater than LAST, DATA is left unchanged.
C
C     5)  If DATA is declared with length less than
C
C            ( LAST - FIRST + ( EPOS-BPOS+1 )  ) / ( EPOS-BPOS+1 )
C
C         the error cannot be diagnosed by this routine.
C
C$ Files
C
C     See the description of the argument HANDLE in $Detailed_Input.
C
C$ Particulars
C
C     This routine provides random read access to the character data in
C     a DAS file.  This data is logically structured as a
C     one-dimensional array of characters.
C
C     However, since Fortran programs usually use strings rather
C     than arrays of individual characters, the interface of this
C     routine provides for extraction of data from a DAS file into
C     an array of strings.
C
C     DASRDC allows the caller to control the amount of character data
C     read into each array element.  This feature allows a program to
C     read character data into an array that has a different string
C     length from the one used to write the character data, without
C     losing the correspondence between input and output array elements.
C     For example, an array of strings of 32 characters can be written
C     to a DAS file and read back by DASRDC into a buffer of strings
C     having length 80 characters, mapping each 32-character string to
C     characters 1--32 of the output buffer.
C
C
C$ Examples
C
C     1)  Create the new DAS file TEST.DAS and add 240 characters to it.
C         Close the file, then re-open it and read the data back out.
C
C
C                  PROGRAM TEST_ADD
C
C                  CHARACTER*(40)        LINES  ( 3 )
C                  CHARACTER*(80)        BUFFER ( 3 )
C                  CHARACTER*(4)         TYPE
C
C                  INTEGER               FIRST
C                  INTEGER               HANDLE
C                  INTEGER               I
C                  INTEGER               LAST
C
C                  DATA LINES  / 'Here is the first line.',
C                 .              'Here is the second line.',
C                 .              'Here is the third line.'    /
C
C            C
C            C     Open a new DAS file.  Use the file name as
C            C     the internal file name.
C            C
C                  TYPE = 'TEST'
C                  CALL DASONW ( 'TEST.DAS', TYPE, 'TEST.DAS', HANDLE )
C
C            C
C            C     Add the contents of the array LINES to the file.
C            C
C                  CALL DASADC ( HANDLE, 120, 1, 40, LINES )
C
C            C
C            C     Close the file.
C            C
C                  CALL DASCLS ( HANDLE )
C
C            C
C            C     Now verify the addition of data by opening the
C            C     file for read access and retrieving the data.  This
C            C     time, use a buffer of 80-character strings to read
C            C     the data.  Use only the first 40 characters of each
C            C     buffer element.
C            C
C                  DO I = 1, 3
C                     BUFFER(I) = ' '
C                  END DO
C
C                  CALL DASRDC ( HANDLE, 1, 120, 1, 40, BUFFER )
C
C            C
C            C     Dump the data to the screen.  We should see the
C            C     sequence
C            C
C            C        Here is the first line.
C            C        Here is the second line.
C            C        Here is the third line.
C            C
C                  WRITE (*,*) ' '
C                  WRITE (*,*) 'Data from TEST.DAS: '
C                  WRITE (*,*) ' '
C                  WRITE (*,*) BUFFER
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
C-    SPICELIB Version 1.2.2 03-JUL-1996 (NJB)
C
C        Various errors in the header comments were fixed.
C
C-    SPICELIB Version 1.2.1 19-DEC-1995 (NJB)
C
C        Corrected title of permuted index entry section.
C
C-    SPICELIB Version 1.2.0, 03-NOV-1995 (NJB)
C
C        Routine now uses discovery check-in.  FAILED test moved inside
C        loops.
C
C-    SPICELIB Version 1.2.0, 14-SEP-1995 (NJB)
C
C        Bug fix:  reference to DASADS in CHKOUT calls corrected.
C
C-    SPICELIB Version 1.1.0, 12-MAY-1994 (KRG) (NJB)
C
C        Test of FAILED() added to loop termination conditions.
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
C-    SPICELIB Version 1.0.0, 12-NOV-1992 (NJB) (WLT)
C
C-&
 
C$ Index_Entries
C
C     read character data from a DAS file
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.2.0, 03-NOV-1995 (NJB)
C
C        Routine now uses discovery check-in.  FAILED test moved inside
C        loops.
C
C-    SPICELIB Version 1.2.0, 14-SEP-1995 (NJB)
C
C        Bug fix:  reference to DASADS in CHKOUT calls corrected.
C        These references have been changed to 'DASRDC'.
C
C
C-    SPICELIB Version 1.1.0, 12-MAY-1994 (KRG) (NJB)
C
C        Test of FAILED() added to loop termination conditions.  Without
C        this test, an infinite loop could result if DASA2L or DASRRC
C        signaled an error inside the loops.
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
C-    SPICELIB Version 1.0.0, 12-NOV-1992 (NJB) (WLT)
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               FAILED
 
C
C     Local parameters
C
      INTEGER               NWC
      PARAMETER           ( NWC =  1024 )
 
      INTEGER               CHAR
      PARAMETER           ( CHAR   =  1  )
 
 
 
C
C     Local variables
C
      INTEGER               CHR
      INTEGER               CLBASE
      INTEGER               CLSIZE
      INTEGER               ELT
      INTEGER               L
      INTEGER               N
      INTEGER               NMOVE
      INTEGER               NMOVED
      INTEGER               NREAD
      INTEGER               NUMCHR
      INTEGER               RECNO
      INTEGER               RCPOS
      INTEGER               WORDNO
 
 
 
C
C     Make sure BPOS and EPOS are ok; stop here if not.
C
      IF (      ( BPOS .LT. 1              )
     .     .OR. ( EPOS .LT. 1              )
     .     .OR. ( BPOS .GT. LEN( DATA(1) ) )
     .     .OR. ( EPOS .GT. LEN( DATA(1) ) )  )  THEN
 
         CALL CHKIN  ( 'DASRDC'                                        )
         CALL SETMSG ( 'Substring bounds must be in range [1,#]. '    //
     .                 'Actual range [BPOS,EPOS] was [#,#].'           )
         CALL ERRINT ( '#',  LEN(DATA(1))                              )
         CALL ERRINT ( '#',  BPOS                                      )
         CALL ERRINT ( '#',  EPOS                                      )
         CALL SIGERR ( 'SPICE(BADSUBSTRINGBOUNDS)'                     )
         CALL CHKOUT ( 'DASRDC'                                        )
         RETURN
 
 
      ELSE IF ( EPOS .LT. BPOS ) THEN
 
         CALL CHKIN  ( 'DASRDC'                                        )
         CALL SETMSG ( 'Substring upper bound must not be less than ' //
     .                 'lower bound.  Actual range [BPOS,EPOS] was '  //
     .                 '[#,#].'                                        )
         CALL ERRINT ( '#',  BPOS                                      )
         CALL ERRINT ( '#',  EPOS                                      )
         CALL SIGERR ( 'SPICE(BADSUBSTRINGBOUNDS)'                     )
         CALL CHKOUT ( 'DASRDC'                                        )
         RETURN
 
      END IF
 
C
C     Find out the physical location of the first character to read.  If
C     FIRST is out of range, DASA2L will cause an error to be signalled.
C
      CALL DASA2L ( HANDLE, CHAR, FIRST, CLBASE, CLSIZE, RECNO, WORDNO )
 
C
C     Get the length of the elements of DATA.  Count the total number
C     of characters to read.
C
      L       =  EPOS - BPOS  + 1
      N       =  LAST - FIRST + 1
      NREAD   =  0
 
C
C     Read as much data from record RECNO as is necessary and possible.
C
      NUMCHR  =  MIN ( N,  NWC - WORDNO + 1 )
 
      ELT     =  1
      CHR     =  BPOS
      NMOVED  =  0
      RCPOS   =  WORDNO
 
 
      DO WHILE ( NMOVED .LT. NUMCHR )
 
         IF ( FAILED() ) THEN
            RETURN
         END IF
 
         IF ( CHR .GT. EPOS ) THEN
            ELT = ELT + 1
            CHR = BPOS
         END IF
C
C        Find out how many characters to move from the current record
C        to the current array element.
C
         NMOVE   =  MIN (  NUMCHR - NMOVED,   EPOS - CHR + 1  )
 
         CALL DASRRC (  HANDLE,
     .                  RECNO,
     .                  RCPOS,
     .                  RCPOS + NMOVE - 1,
     .                  DATA(ELT) ( CHR  :  CHR + NMOVE - 1 )   )
 
         NMOVED  =  NMOVED + NMOVE
         RCPOS   =  RCPOS  + NMOVE
         CHR     =  CHR    + NMOVE
 
      END DO
 
      NREAD  =  NUMCHR
      RECNO  =  RECNO + 1
 
C
C     Read from as many additional records as necessary.
C
 
      DO WHILE ( NREAD .LT. N )
 
         IF ( FAILED() ) THEN
            RETURN
         END IF
 
C
C        At this point, RECNO is the correct number of the
C        record to read from next.  CLBASE is the number
C        of the first record of the cluster we're about
C        to read from.
C
C
         IF (  RECNO  .LT.  ( CLBASE + CLSIZE )  ) THEN
C
C           We can continue reading from the current cluster.  Find
C           out how many elements to read from the current record,
C           and read them.
C
            NUMCHR  =  MIN ( N - NREAD,  NWC )
            NMOVED  =  0
            RCPOS   =  1
 
            DO WHILE ( ( NMOVED .LT. NUMCHR ) .AND. ( .NOT. FAILED() ) )
 
               IF ( CHR .GT. EPOS ) THEN
                  ELT = ELT + 1
                  CHR = BPOS
               END IF
C
C              Find out how many characters to move from the current
C              record to the current array element.
C
               NMOVE   =  MIN (  NUMCHR - NMOVED,   EPOS - CHR + 1  )
 
               CALL DASRRC (  HANDLE,
     .                        RECNO,
     .                        RCPOS,
     .                        RCPOS + NMOVE - 1,
     .                        DATA(ELT) ( CHR  :  CHR + NMOVE - 1 )   )
 
               NMOVED  =  NMOVED + NMOVE
               RCPOS   =  RCPOS  + NMOVE
               CHR     =  CHR    + NMOVE
 
            END DO
 
            NREAD   =   NREAD + NUMCHR
            RECNO   =   RECNO + 1
 
 
         ELSE
C
C           We must find the next character cluster to
C           read from.  The first character in this
C           cluster has address FIRST + NREAD.
C
            CALL DASA2L ( HANDLE,
     .                    CHAR,
     .                    FIRST + NREAD,
     .                    CLBASE,
     .                    CLSIZE,
     .                    RECNO,
     .                    WORDNO  )
 
         END IF
 
      END DO
 
      RETURN
      END

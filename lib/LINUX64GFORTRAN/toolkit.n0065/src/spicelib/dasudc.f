C$Procedure      DASUDC ( DAS, update data, character )
 
      SUBROUTINE DASUDC ( HANDLE, FIRST, LAST, BPOS, EPOS, DATA )
 
C$ Abstract
C
C     Update character data in a specified range of DAS logical
C     addresses with substrings of a character array.
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
C     DATA       I   Data having addresses FIRST through LAST.
C
C$ Detailed_Input
C
C     HANDLE         is a file handle of a DAS file opened for writing.
C
C     FIRST,
C     LAST           are the first and last of a range of DAS logical
C                    addresses of characters.  These addresses satisfy
C                    the inequality
C
C                       1  <   FIRST   <   LAST   <   LASTC
C                          _           -          -
C
C                    where LASTC is the last character logical address
C                    in use in the DAS file designated by HANDLE.
C
C     BPOS,
C     EPOS           are begin and end character positions that define
C                    the substrings of the input array that are to be
C                    added to the DAS file.
C
C     DATA           is an array of character strings.  The contents of
C                    the specified substrings of the elements of the
C                    array DATA will be written to the indicated DAS
C                    file in order:  DATA(1)(BPOS:BPOS) will be written
C                    to character logical address FIRST;
C                    DATA(1)(BPOS+1:BPOS+1) will be written to
C                    the character logical address FIRST+1, and so on;
C                    in this ordering scheme, character (BPOS:BPOS) of
C                    DATA(I+1) is the successor of character (EPOS:EPOS)
C                    of DATA(I).
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
C     1)  If the input file handle is invalid, the error will be
C         diagnosed by routines called by this routine.
C
C     2)  Only logical addresses that already contain data may be
C         updated:  if either FIRST or LAST are outside the range
C
C           [ 1,  LASTC ]
C
C         where LASTC is the last character logical address that
C         currently contains data in the indicated DAS file, the error
C         SPICE(INVALIDADDRESS) is signalled.  The DAS file will not be
C         modified.
C
C     3)  If FIRST > LAST but both addresses are valid, this routine
C         will not modify the indicated DAS file.  No error will be
C         signalled.
C
C     4)  If an I/O error occurs during the data update attempted
C         by this routine, the error will be diagnosed by routines
C         called by this routine.  FIRST and LAST will not be modified.
C
C$ Files
C
C     See the description of the argument HANDLE in $Detailed_Input.
C
C$ Particulars
C
C     This routine replaces the character data in the specified range
C     of logical addresses within a DAS file with the contents of the
C     specified substrings of the input array DATA.
C
C     The actual physical write operations that update the indicated
C     DAS file with the contents of the input array DATA may not take
C     place before this routine returns, since the DAS system buffers
C     data that is written as well as data that is read.  In any case,
C     the data will be flushed to the file at the time the file is
C     closed, if not earlier.  A physical write of all buffered
C     records can be forced by calling the SPICELIB routine DASWBR
C     (DAS, write buffered records).
C
C     In order to append character data to a DAS file, filling in a
C     range of character logical addresses that starts immediately
C     after the last character logical address currently in use, the
C     SPICELIB routines DASADS ( DAS add data, substring ) or DASADC
C     ( DAS add data, character ) should be used.
C
C$ Examples
C
C     1)  Write to addresses 1 through 320 in a DAS file in
C         random-access fashion by updating the file.  Recall
C         that data must be present in the file before it can
C         be updated.
C
C
C                  PROGRAM UP
C
C                  CHARACTER*(80)        BUFFER ( 10 )
C                  CHARACTER*(80)        LINE
C                  CHARACTER*(4)         TYPE
C
C                  INTEGER               FIRST
C                  INTEGER               HANDLE
C                  INTEGER               I
C                  INTEGER               LAST
C
C            C
C            C     Open the new DAS file RAND.DAS.  Use the file name
C            C     as the internal file name.
C            C
C                  TYPE = 'TEST'
C                  CALL DASONW ( 'TEST.DAS', TYPE, 'TEST.DAS', HANDLE )
C
C            C
C            C     Append 320 characters to the file, thereby reserving
C            C     enough room for 10 strings of 32 characters.  After
C            C     the data is present, we're free to update it in any
C            C     order we please.
C            C
C                  LINE = ' '
C
C                  DO I = 1, 10
C                    CALL DASADC ( HANDLE, 32, 1, 32, LINE )
C                  END DO
C
C            C
C            C     Now the character logical addresses 1:320 can be
C            C     written to in random-access fashion.  We'll fill
C            C     them in by writing 32 characters at a time, starting
C            C     with addresses 289:320 and working backwards.
C            C
C                  FIRST = 321
C
C                  DO I = 10, 1, -1
C
C                     LAST  = FIRST - 1
C                     FIRST = LAST  - 32
C
C                     LINE = 'This is the # line.'
C                     CALL REPMOT ( LINE,  '#',    I,   'L',    LINE )
C                     CALL DASUDC ( HANDLE, FIRST, LAST, 1, 32, LINE )
C
C                  END DO
C
C            C
C            C     Close the file.
C            C
C                  CALL DASCLS ( HANDLE )
C
C            C
C            C     Now make sure that we updated the file properly.
C            C     Open the file for reading and dump the contents
C            C     of the character logical addresses 1:320.
C            C
C                  CALL DASOPR ( 'RAND.DAS',  HANDLE       )
C
C                  CALL DASRDC (  HANDLE,  1, 320, 1, 32, BUFFER )
C
C                  WRITE (*,*) 'Contents of RAND.DAS:'
C                  WRITE (*,*) ' '
C                  WRITE (*,*) BUFFER(1:32)
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
C-    SPICELIB Version 1.3.0 10-APR-2014 (NJB)
C
C        Deleted declarations of unused parameters.
C
C        Corrected header comments: routine that flushes
C        written, buffered records is DASWBR, not DASWUR.
C
C-    SPICELIB Version 1.2.1 19-DEC-1995 (NJB)
C
C        Corrected title of permuted index entry section.
C
C-    SPICELIB Version 1.2.0, 12-MAY-1995 (NJB)
C
C        Bug fix:  routine handled values of BPOS incorrectly when
C        BPOS > 1.
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
C     update a range of DAS logical addresses using substrings
C     write substrings to a range of DAS logical addresses
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.2.0, 12-MAY-1995 (NJB)
C
C        Bug fix:  routine handled values of BPOS incorrectly when
C        BPOS > 1.  This was due to the incorrect initialization
C        of the internal variables CHR and ELT.  The initialization
C        was corrected.
C
C-    SPICELIB Version 1.1.0, 12-MAY-1994 (KRG) (NJB)
C
C        Tests of FAILED() added to loop termination conditions.
C        Without these tests, infinite loops could result if DASA2L or
C        DASURC signaled an error inside the loops.
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
      LOGICAL               RETURN
 
C
C     Local parameters
C
      INTEGER               NWC
      PARAMETER           ( NWC  =  1024 )
 
      INTEGER               CHAR
      PARAMETER           ( CHAR =  1  )
 
C
C     Local variables
C
      INTEGER               CHR
      INTEGER               CLBASE
      INTEGER               CLSIZE
      INTEGER               ELT
      INTEGER               L
      INTEGER               LASTC
      INTEGER               LASTD
      INTEGER               LASTI
      INTEGER               N
      INTEGER               NMOVE
      INTEGER               NMOVED
      INTEGER               NUMCHR
      INTEGER               NWRITN
      INTEGER               RECNO
      INTEGER               RCPOS
      INTEGER               WORDNO
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DASUDC' )
      END IF
 
C
C     Get the last logical addresses in use in this DAS file.
C
      CALL DASLLA ( HANDLE, LASTC, LASTD, LASTI )
 
C
C     Validate the input addresses.
C
      IF (      ( FIRST .LT. 1     )
     .     .OR. ( FIRST .GT. LASTC )
     .     .OR. ( LAST  .LT. 1     )
     .     .OR. ( LAST  .GT. LASTC )  ) THEN
 
         CALL SETMSG ( 'FIRST was #. LAST was #. Valid range is [1,#].')
         CALL ERRINT ( '#',   FIRST                                    )
         CALL ERRINT ( '#',   LAST                                     )
         CALL ERRINT ( '#',   LASTC                                    )
         CALL SIGERR ( 'SPICE(INVALIDADDRESS)'                         )
         CALL CHKOUT ( 'DASUDC'                                        )
         RETURN
 
      END IF
 
C
C     Get the length of the substrings of DATA.  Count the total number
C     of characters to write.
C
      L       =  EPOS - BPOS  + 1
      N       =  LAST - FIRST + 1
      NWRITN  =  0
 
C
C     Find out the physical location of the first character to update.
C
      CALL DASA2L ( HANDLE, CHAR, FIRST, CLBASE, CLSIZE, RECNO, WORDNO )
 
C
C     Write as much data into record RECNO as is necessary and possible.
C
C     NUMCHR is the number of characters to write to the current record.
C
C     ELT is the index of the element of the input array that we're
C     taking data from.  CHR is the position in that array element of
C     the next character to move to the file.
C
C     NMOVED is the number of characters we've moved into the current
C     record so far.
C
C     RCPOS is the character position we'll write to next in the current
C     record.
C
      NUMCHR  =  MIN ( N,  NWC - WORDNO + 1 )
      ELT     =  1
      CHR     =  BPOS
      NMOVED  =  0
      RCPOS   =  WORDNO
 
 
      DO WHILE (  ( NMOVED .LT. NUMCHR ) .AND. ( .NOT. FAILED() )  )
 
         IF ( CHR .GT. EPOS ) THEN
            ELT = ELT + 1
            CHR = BPOS
         END IF
C
C        Find out how many characters to move from the current array
C        element to the current record.
C
         NMOVE   =  MIN (  NUMCHR - NMOVED,   EPOS - CHR + 1  )
 
C
C        Update the current record.
C
         CALL DASURC (  HANDLE,
     .                  RECNO,
     .                  RCPOS,
     .                  RCPOS + NMOVE - 1,
     .                  DATA(ELT) ( CHR  :  CHR + NMOVE - 1 )   )
 
         NMOVED  =  NMOVED + NMOVE
         RCPOS   =  RCPOS  + NMOVE
         CHR     =  CHR    + NMOVE
 
      END DO
 
      NWRITN =  NUMCHR
      RECNO  =  RECNO + 1
 
C
C     Update as many additional records as necessary.
C
 
      DO WHILE (  ( NWRITN .LT. N ) .AND. ( .NOT. FAILED() )  )
C
C        At this point, RECNO is the correct number of the record to
C        write to next.  CLBASE is the number of the first record of
C        the cluster we're about to write to.
C
         IF (  RECNO  .LT.  ( CLBASE + CLSIZE )  ) THEN
C
C           We can continue writing the current cluster.  Find
C           out how many elements to write to the current record,
C           and write them.
C
            NUMCHR  =  MIN ( N - NWRITN,  NWC )
            NMOVED  =  0
            RCPOS   =  1
 
            DO WHILE ( ( NMOVED .LT. NUMCHR ) .AND. ( .NOT. FAILED() ) )
 
               IF ( CHR .GT. L ) THEN
                  ELT = ELT + 1
                  CHR = BPOS
               END IF
C
C              Find out how many characters to move from the array
C              element to the current record.
C
               NMOVE   =  MIN (  NUMCHR - NMOVED,   EPOS - CHR + 1  )
 
               CALL DASURC (  HANDLE,
     .                        RECNO,
     .                        RCPOS,
     .                        RCPOS + NMOVE - 1,
     .                        DATA(ELT) ( CHR  :  CHR + NMOVE - 1 )  )
 
               NMOVED  =  NMOVED + NMOVE
               RCPOS   =  RCPOS  + NMOVE
               CHR     =  CHR    + NMOVE
 
            END DO
 
            NWRITN  =   NWRITN + NUMCHR
            RECNO   =   RECNO  + 1
 
 
         ELSE
C
C           We must find the next character cluster to write to.
C           The first character in this cluster has address FIRST +
C           NWRITN.
C
            CALL DASA2L ( HANDLE,
     .                    CHAR,
     .                    FIRST + NWRITN,
     .                    CLBASE,
     .                    CLSIZE,
     .                    RECNO,
     .                    WORDNO  )
 
         END IF
 
      END DO
 
 
      CALL CHKOUT ( 'DASUDC' )
      RETURN
      END

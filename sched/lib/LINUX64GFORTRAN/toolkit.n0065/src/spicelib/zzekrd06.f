C$Procedure   ZZEKRD06 ( EK, read class 6 column entry elements )
 
      SUBROUTINE ZZEKRD06 ( HANDLE,  SEGDSC,  COLDSC,  RECPTR,
     .                      BEG,     END,     CVALS,   ISNULL,  FOUND )
 
      IMPLICIT NONE
      
C$ Abstract
C
C     Read a specified element range from a column entry in a specified
C     record in a class 6 column.  Class 6 columns have character arrays
C     as column entries.
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
C     EK
C
C$ Keywords
C
C     EK
C     PRIVATE
C
C$ Declarations
 
      INCLUDE 'ekbool.inc'
      INCLUDE 'ekcnamsz.inc'
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ekdatpag.inc'
      INCLUDE 'ekrecptr.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'
 
      INTEGER               HANDLE
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               COLDSC ( CDSCSZ )
      INTEGER               RECPTR
      INTEGER               BEG
      INTEGER               END
      CHARACTER*(*)         CVALS  ( * )
      LOGICAL               ISNULL
      LOGICAL               FOUND
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle attached to EK file.
C     SEGDSC     I   Segment descriptor.
C     COLDSC     I   Column descriptor.
C     RECPTR     I   Record pointer.
C     BEG        I   Start element index.
C     END        I   End element index.
C     CVALS      O   Character values in column entry.
C     ISNULL     O   Flag indicating whether column entry is null.
C     FOUND      O   Flag indicating whether elements were found.
C
C$ Detailed_Input
C
C     HANDLE         is an EK file handle.
C
C     SEGDSC         is the descriptor of the segment from which data is
C                    to be read.
C
C     COLDSC         is the descriptor of the column from which data is
C                    to be read.
C
C     RECPTR         is a pointer to the record containing the column
C                    entry to be written.
C
C     BEG,
C     END            are, respectively, the start and end indices of
C                    the contiguous range of elements to be read from
C                    the specified column entry.
C
C$ Detailed_Output
C
C     CVALS          are the values read from the specified column
C                    entry.  The mapping of elements of the column entry
C                    to elements of CVALS is as shown below:
C
C                       Column entry element       CVALS element
C                       --------------------       -------------
C                       BEG                        1
C                       BEG+1                      2
C                       .                          .
C                       .                          .
C                       .                          .
C                       END                        END-BEG+1
C
C                    CVALS must have sufficient string length to hold 
C                    the longest returned string value.  Entries that
C                    are shorter than the string length of CVALS are 
C                    padded with trailing blanks.
C
C                    CVALS is valid only if the output argument
C                    FOUND is returned .TRUE.
C
C     ISNULL         is a logical flag indicating whether the entry is
C                    null.  ISNULL is set on output whether or not
C                    the range of elements designated by BEG and END
C                    exists.
C
C     FOUND          is a logical flag indicating whether the range
C                    of elements designated by BEG and END exists.
C                    If the number of elements in the specified column
C                    entry is not at least END, FOUND will be returned
C                    .FALSE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If HANDLE is invalid, the error will be diagnosed by routines
C         called by this routine.
C
C     2)  If the specified column entry has not been initialized, the
C         error SPICE(UNINITIALIZED) is signalled.
C
C     3)  If the ordinal position of the column specified by COLDSC
C         is out of range, the error SPICE(INVALIDINDEX) is signalled.
C
C     4)  If the string length of CVALS is shorter than the declared
C         string length of the specified column, the error 
C         SPICE(STRINGTRUNCATED) is signalled.
C
C     5)  If an I/O error occurs while reading the indicated file,
C         the error will be diagnosed by routines called by this
C         routine.
C
C$ Files
C
C     See the EK Required Reading for a discussion of the EK file
C     format.
C
C$ Particulars
C
C     This routine is a utility for reading data from class 6 columns.
C
C$ Examples
C
C     See EKRCEC.
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
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 28-JUL-1997 (NJB)
C
C        Error check for string truncation on output was added.
C        SHORT error message SPICE(UNINITIALIZEDVALUE) was shortened
C        to SPICE(UNINITIALIZED).  Error messages were enhanced so
C        as to use column names rather than indices.
C        
C-    SPICELIB Version 1.0.0, 18-OCT-1995 (NJB)
C
C-&


C$ Revisions
C
C-    SPICELIB Version 1.1.0, 28-JUL-1997 (NJB)
C
C        Error check for string truncation on output was added.
C        SHORT error message SPICE(UNINITIALIZEDVALUE) was shortened
C        to SPICE(UNINITIALIZED).  Error messages were enhanced so
C        as to use column names rather than indices.
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               FAILED
 
C
C     Non-SPICELIB functions
C
      INTEGER               ZZEKRP2N
 
C
C     Local variables
C
      CHARACTER*(CNAMSZ)    COLUMN
      
      INTEGER               AVAIL
      INTEGER               BASE
      INTEGER               COLIDX
      INTEGER               CVLEN
      INTEGER               D
      INTEGER               DATPTR
      INTEGER               DELTA
      INTEGER               ELTIDX
      INTEGER               ELTOFF
      INTEGER               MAXELT
      INTEGER               NCOLS
      INTEGER               NELT
      INTEGER               NREAD
      INTEGER               NREC
      INTEGER               NSKIP
      INTEGER               OFFSET
      INTEGER               P
      INTEGER               PAGNUM
      INTEGER               PG
      INTEGER               PTRLOC
      INTEGER               PTROFF
      INTEGER               RECNO
      INTEGER               REMAIN
      INTEGER               START
      INTEGER               STRLEN
      INTEGER               UNIT
 
C
C     Use discovery check-in.
C
      NREC  =  SEGDSC ( NRIDX )
 
C
C     Make sure the column exists.
C
      NCOLS  =  SEGDSC ( NCIDX  )
      COLIDX =  COLDSC ( ORDIDX )
 
      IF (  ( COLIDX .LT. 1 ) .OR. ( COLIDX .GT. NCOLS )  ) THEN
 
         CALL CHKIN  ( 'ZZEKRD06'                              )
         CALL SETMSG ( 'Column index = #; valid range is 1:#.' )
         CALL ERRINT ( '#',  COLIDX                            )
         CALL ERRINT ( '#',  NREC                              )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                   )
         CALL CHKOUT ( 'ZZEKRD06'                              )
         RETURN
 
      END IF

C
C     Make sure the output buffer is wide enough to hold the returned
C     strings.
C     
      CVLEN   =  LEN    ( CVALS(1) )
      STRLEN  =  COLDSC ( LENIDX   )

      IF ( STRLEN .GT. CVLEN ) THEN
C              
C        We have a string truncation error.  Look up the column
C        name, record number, and file name before signalling an
C        error.
C        
         CALL DASHLU   ( HANDLE, UNIT )
         CALL ZZEKCNAM ( HANDLE, COLDSC, COLUMN )  
         
         RECNO  =  ZZEKRP2N ( HANDLE, SEGDSC(SNOIDX), RECPTR )
            
         CALL CHKIN  ( 'ZZEKRD06'                                  )
         CALL SETMSG ( 'String value has length #; output string '//
     .                 'can hold only # characters.  COLUMN = #; '// 
     .                 'SEGNO = #; RECNO = #; EK = #'              )
         CALL ERRINT ( '#',  STRLEN                                )
         CALL ERRINT ( '#',  CVLEN                                 )
         CALL ERRCH  ( '#',  COLUMN                                )
         CALL ERRINT ( '#',  SEGDSC(SNOIDX)                        )
         CALL ERRINT ( '#',  RECNO                                 )
         CALL ERRFNM ( '#',  UNIT                                  )
         CALL SIGERR ( 'SPICE(STRINGTRUNCATED)'                    )
         CALL CHKOUT ( 'ZZEKRD06'                                  )
         RETURN
            
      END IF

 
C
C     Compute the data pointer location, and read the pointer.
C
      PTRLOC  =  RECPTR + DPTBAS + COLIDX
 
      CALL DASRDI ( HANDLE, PTRLOC, PTRLOC, DATPTR )
 
 
      IF ( DATPTR .GT. 0 ) THEN
C
C        The entry is non-null.
C
         ISNULL  =  .FALSE.
 
C
C        Get the element count.  Check for range specifications that
C        can't be met.
C
         CALL ZZEKGEI ( HANDLE, DATPTR, NELT )
 
         IF (  ( BEG .LT. 1 ) .OR. ( BEG .GT. NELT )  )  THEN
 
            FOUND  =  .FALSE.
            RETURN
 
         ELSE IF (  ( END .LT. 1 ) .OR. ( END .GT. NELT )  )  THEN
 
            FOUND  =  .FALSE.
            RETURN
 
         ELSE IF ( END .LT. BEG ) THEN
 
            FOUND  =  .FALSE.
            RETURN
 
         END IF
 
C
C        The request is valid, so read the data.  The first step is to
C        locate the element at index BEG.  We'll first decide on which
C        page the desired element starts.  The first page holds up to
C        CPSIZE - ENCSIZ characters; the rest hold CPSIZE characters.
C        While we're at it, we'll compute the offset ELTOFF of the
C        element from the base of the page on which the element starts.
C        We'll use the name OFFSET to represent the character offset
C        of the element from the base of the page on which the column
C        entry starts.
C
         CALL ZZEKPGPG ( CHR, DATPTR, P, BASE )
 
         PTROFF  =  DATPTR  -  BASE
         OFFSET  =  PTROFF  +  ENCSIZ   +   STRLEN * ( BEG - 1 )
 
         IF ( OFFSET .LE. CPSIZE ) THEN
 
            PAGNUM   =    1
            ELTOFF   =    OFFSET
 
         ELSE
 
            PAGNUM   =  ( OFFSET  +  CPSIZE   -   1 ) / CPSIZE
            ELTOFF   =    OFFSET  -    ( PAGNUM - 1 ) * CPSIZE
 
         END IF
 
C
C        Get the absolute page number and base address of the page
C        on which the element starts.  If this is not the page on
C        which the column entry starts, we'll chain along using
C        the page's forward links until we arrive at the correct page.
C
         PG  =  1
 
         DO WHILE ( PG .LT. PAGNUM )
C
C           Get the link to the next page, then look up the base
C           address of that page.
C
            CALL ZZEKGEI  ( HANDLE, BASE+CFPIDX, P    )
            CALL ZZEKPGBS ( CHR,    P,           BASE )
 
            PG  =  PG + 1
 
         END DO
 
C
C        The desired element starts at address BASE + ELTOFF.
C
         DATPTR  =  BASE + ELTOFF
 
C
C        At this point, P is the page on which the element having index
C        BEG is located.  BASE is the base address of this page.
C
C        Read the strings one at a time.  
C
         ELTIDX   =  1
         MAXELT   =  END - BEG + 1
 
         DO WHILE (  ( ELTIDX .LE. MAXELT ) .AND. ( .NOT. FAILED() )  )
C
C           Read the current string.  The string may be continued over
C           multiple pages.  Read only as many characters as will fit
C           in the output buffer element CVALS(ELTIDX).
C
            REMAIN  =  MIN ( CVLEN, STRLEN )
            START   =  1
 
            DO WHILE (  ( REMAIN .GT. 0 ) .AND. ( .NOT. FAILED() )  )
 
               AVAIL  =  BASE + CPSIZE - DATPTR + 1
               NREAD  =  MIN  ( REMAIN,  AVAIL )
 
               IF ( NREAD .GT. 0 ) THEN
 
                  CALL DASRDC ( HANDLE,
     .                          DATPTR,
     .                          DATPTR + NREAD - 1,
     .                          START,
     .                          START + NREAD - 1,
     .                          CVALS ( ELTIDX )    )
 
                  START  =  START  + NREAD
                  REMAIN =  REMAIN - NREAD
                  DATPTR =  DATPTR + NREAD
 
               ELSE
C
C                 Go to the next page for the continuation of the
C                 current string.
C
                  CALL ZZEKGEI  ( HANDLE, BASE+CFPIDX, P    )
                  CALL ZZEKPGBS ( CHR,    P,           BASE )
 
                  DATPTR  =  BASE   +  1
 
               END IF
 
            END DO
 
C
C           If we did not read all of the current array element,
C           we'll need to advance DATPTR past the end of the element.
C           If this advance moved DATPTR beyond the last character
C           of the current page, the logic above will set DATPTR to
C           indicate the first character of the next continuation page.
C
            DELTA  =  STRLEN - CVLEN
 
            IF ( DELTA .GT. 0 ) THEN
 
               D  =  DELTA
 
               DO WHILE ( D .GT. 0 )
 
                  AVAIL  =  BASE + CPSIZE - DATPTR + 1
                  NSKIP  =  MIN  ( D,  AVAIL )
 
                  IF ( NSKIP .GT. 0 ) THEN
 
                     D      =  D      - NSKIP
                     DATPTR =  DATPTR + NSKIP
 
                  ELSE
C
C                    Go to the next page for the continuation of the
C                    current string.
C
                     CALL ZZEKGEI  ( HANDLE, BASE+CFPIDX, P    )
                     CALL ZZEKPGBS ( CHR,    P,           BASE )
 
                     DATPTR  =  BASE   +  1
 
                  END IF
 
               END DO
 
            END IF
 
C
C           Blank-pad the output string if necessary.
C
            IF ( CVLEN .GT. STRLEN ) THEN
               CVALS ( ELTIDX ) ( STRLEN+1: ) = ' '
            END IF
 
 
            ELTIDX  =  ELTIDX + 1
 
         END DO
 
 
         FOUND =  .NOT. FAILED()
 
 
      ELSE IF ( DATPTR .EQ. NULL ) THEN
C
C        The value is null.
C
         ISNULL =  .TRUE.
         FOUND  =  .TRUE.
 
 
      ELSE IF ( DATPTR .EQ. UNINIT ) THEN
C
C        The data value is absent.  This is an error.
C
         RECNO  =  ZZEKRP2N ( HANDLE, SEGDSC(SNOIDX), RECPTR )
         CALL DASHLU   (  HANDLE,  UNIT           )
         CALL ZZEKCNAM ( HANDLE,   COLDSC, COLUMN )  
 
         CALL CHKIN  ( 'ZZEKRD06'                                    )
         CALL SETMSG ( 'Attempted to read uninitialized column '    //
     .                 'entry.  SEGNO = #; COLUMN = #; RECNO = #; ' //
     .                 'EK = #'                                      )
         CALL ERRINT ( '#',  SEGDSC(SNOIDX)                          )
         CALL ERRCH  ( '#',  COLUMN                                  )
         CALL ERRINT ( '#',  RECNO                                   )
         CALL ERRFNM ( '#',  UNIT                                    )
         CALL SIGERR ( 'SPICE(UNINITIALIZED)'                        )
         CALL CHKOUT ( 'ZZEKRD06'                                    )
         RETURN
 
 
      ELSE
C
C        The data pointer is corrupted.
C
         RECNO  =  ZZEKRP2N ( HANDLE, SEGDSC(SNOIDX), RECPTR )
         CALL DASHLU   (  HANDLE,  UNIT           )
         CALL ZZEKCNAM ( HANDLE,   COLDSC, COLUMN )  
 
         CALL CHKIN  ( 'ZZEKRD06'                                )
         CALL SETMSG ( 'Data pointer is corrupted. SEGNO = #; '  //
     .                 'COLUMN =  #; RECNO = #; EK = #'          )
         CALL ERRINT ( '#',  SEGDSC(SNOIDX)                      )
         CALL ERRCH  ( '#',  COLUMN                              )
         CALL ERRINT ( '#',  RECNO                               )
         CALL ERRFNM ( '#',  UNIT                                )
         CALL SIGERR ( 'SPICE(BUG)'                              )
         CALL CHKOUT ( 'ZZEKRD06'                                )
         RETURN
 
      END IF
 
 
      RETURN
      END

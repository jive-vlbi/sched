 
C$Procedure BBOARD ( Bulletin board )
 
      SUBROUTINE BBOARD ( ACTION, ITEM, N, IVALS,
     .                                     DVALS,
     .                                     CVALS,
     .                                     SVAL   )
      IMPLICIT NONE
 
C$ Abstract
C
C     Maintain a global bulletin board for use by application
C     programs.
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
C     BBOARD
C
C$ Keywords
C
C     UTILITY
C
C$ Declarations
 
      CHARACTER*(*)         ACTION
      CHARACTER*(*)         ITEM
      INTEGER               N
      INTEGER               IVALS    ( * )
      DOUBLE PRECISION      DVALS    ( * )
      CHARACTER*(*)         CVALS    ( * )
      CHARACTER*(*)         SVAL
 
      INTEGER               MAXNL
      PARAMETER           ( MAXNL = 32 )
 
      INTEGER               MAXCL
      PARAMETER           ( MAXCL = 255 )
 
      INTEGER               MAXI
      PARAMETER           ( MAXI = 100 )
 
      INTEGER               MAXD
      PARAMETER           ( MAXD = 100 )
 
      INTEGER               MAXC
      PARAMETER           ( MAXC = 100 )
 
      INTEGER               MAXS
      PARAMETER           ( MAXS = 100 )
 
      INTEGER               MAXIV
      PARAMETER           ( MAXIV = 5000 )
 
      INTEGER               MAXDV
      PARAMETER           ( MAXDV = 5000 )
 
      INTEGER               MAXCV
      PARAMETER           ( MAXCV = 300 )
 
      INTEGER               MAXCHR
      PARAMETER           ( MAXCHR = 5000 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ACTION     I   Action to be taken.
C     ITEM       I   Item to be posted or retrieved.
C     N         I,O  Number of values posted or retrieved.
C     IVALS     I,O  Integer values.
C     DVALS     I,O  Double precision values
C     CVALS     I,O  Character values.
C     SVAL      I,O  String value.
C     MAXNL      P   Maximum name length.
C     MAXCL      P   Maximum character length.
C     MAXI       P   Maximum number of integer items.
C     MAXD       P   Maximum number of double precision items.
C     MAXC       P   Maximum number of character items.
C     MAXS       P   Maximum number of string items.
C     MAXIV      P   Maximum number of integer values.
C     MAXDV      P   Maximum number of double precision values.
C     MAXCV      P   Maximum number of character values.
C     MAXCHR     P   Maximum number of string characters.
C
C$ Detailed_Input
C
C     ACTION     is used by entry points BBPUT and BBGET to indicate
C                a specific action to be taken. Possible actions
C                are 'POST', 'COPY', 'TAKE', 'PUSH', 'APPEND', and
C                'POP'.
C
C     ITEM       is the name of an item to be posted, retrieved,
C                removed, and so on. Names are case-sensitive, but
C                leading and embedded blanks are ignored.
C
C     N          on input is the number of values to be posted.
C
C     IVALS,
C     DVALS,
C     CVALS,     on input are values to be associated with a specific
C                integer, DP, or character item on the board.
C
C     SVAL       on input is a string value to be associated with a
C                specific string item on the board.
C
C$ Detailed_Output
C
C     N          on output is the number of values being returned,
C                or the number of values associated with an item.
C
C     IVALS,
C     DVALS,
C     CVALS,     on output are values associated with a specific
C                integer, DP, or character item on the board.
C
C     SVAL       on output is a string value associated with a
C                specific string item on the board.
C
C$ Parameters
C
C     MAXNL      is the maximum number of characters that can make
C                up an item name.
C
C     MAXCL      is the declared length of the individual values
C                of character items. That is, each multi-valued
C                character item is equivalent to a CHARACTER*(MAXCL)
C                array.
C
C     MAXI,
C     MAXD,
C     MAXC,
C     MAXS,      are the maximum numbers of items of each type
C                (integer, DP, character, and string) that can be
C                stored simultaneously.
C
C     MAXIV,
C     MAXDV,
C     MAXCV      are the maximum numbers of values of each type
C                (integer, DP, and character) that can be stored
C                simultaneously. MAXIV, MAXDV, and MAXCV must be
C                at least as large as MAXI, MAXD, and MAXC,
C                respectively. (Note that string items are are
C                not multi-valued.)
C
C     MAXCHR     is the maximum number characters that can be used
C                to store string items at any one time. MAXCHR must
C                be an integer multiple of 100.
C
C$ Exceptions
C
C     1) If BBOARD is called directly, the error 'SPICE(BOGUSENTRY)'
C        is signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C           BBOARD implements a global storage area, which may be
C           used by the individual modules of an application program
C           communicate with each other. The metaphor for this area
C           is a bulletin board: modules may leave messages (called
C           `items') on this board, to be copied, modified, or removed
C           by other modules.
C
C     Types
C
C           The board can contain four types of items: integer, double
C           precision (DP), character, and string. The first three types
C           may be multi-valued: for example, a single integer item may
C           be associated with more than one integer value. Thus, the
C           board may be used to store arrays as well as scalar values.
C
C           Each string item may contain only one value: however,
C           the number of characters in this value may be anywhere
C           between one and the total size of the string buffer
C           (parameter MAXCHR).
C
C     Posting items
C
C           An item may be posted on the board by calling BBPUTx
C           (where x indicates the type of the item: I, D, C, or S).
C           For example, the call
C
C              IMAGES(1) = '22421.36'
C              IMAGES(2) = '22421.39'
C              IMAGES(3) = '22421.45'
C
C              CALL BBPUTC ( 'POST', 'IMAGE QUEUE', 3, IMAGES )
C
C           creates an item with the name 'IMAGE QUEUE', which contains
C           the three character values in the array IMAGES. If an item
C           with that name already exists, it is replaced.
C
C     Item names
C
C           Item names are case-sensitive, but blanks are ignored.
C           The item 'IMAGE QUEUE' may be accessed under any of the
C           following names.
C
C              'IMAGE QUEUE'
C              'IMAGEQUEUE'
C              '  IMAGE   QUEUE  '
C
C           On the other hand, the names
C
C              'Image queue'
C              'image queue'
C              'Image Queue'
C
C           all refer to distinct items.
C
C           The same item name may be applied to one item of each
C           type. This allows you to associate values of different
C           types under a single name, as in the following example.
C
C              IMAGES(  1) = '22421.36'
C              BODIES(  1) =  801
C              RADII (1,1) =  1600.D0
C              RADII (2,1) =  1600.D0
C              RADII (3,1) =  1600.D0
C
C              IMAGES(  2) = '22427.19'
C              BODIES(  2) =  899
C              RADII (1,2) =  25295.D0
C              RADII (2,2) =  25295.D0
C              RADII (3,2) =  24738.D0
C
C              DESCR       = 'Preliminary NINA testing, 4/12/89'
C
C              CALL BBPUTC ( 'POST', 'IMAGE QUEUE', 2, IMAGES )
C              CALL BBPUTI ( 'POST', 'IMAGE QUEUE', 2, BODIES )
C              CALL BBPUTD ( 'POST', 'IMAGE QUEUE', 6, RADII  )
C              CALL BBPUTS ( 'POST', 'IMAGE QUEUE',    DESCR  )
C
C     Copying items
C
C           Once an item has been posted, its values may be copied
C           by calling BBGETx. For example, the call
C
C              CALL BBGETC ( 'COPY', 'IMAGE QUEUE', N, IMAGES )
C
C           copies the values associated with the character item
C           'IMAGE QUEUE' into the character array IMAGES. All of
C           the values associated with the item are returned.
C
C     Taking items
C
C           When an item is copied, its values remain intact, ready
C           to be copied by other modules. Posted items may also be
C           taken by calling BBGETx. For example, the call
C
C              CALL BBGETD ( 'TAKE', 'IMAGE QUEUE', N, IMAGES )
C
C           returns the values just as the previous call did; however,
C           following this call, the item is no longer on the board.
C
C     Removing items
C
C           It is possible to remove an item without copying its values,
C           by calling BBREMx. For example, the calls
C
C              CALL BBREMC ( 'IMAGE QUEUE' )
C              CALL BBREMI ( 'IMAGE QUEUE' )
C              CALL BBREMD ( 'IMAGE QUEUE' )
C              CALL BBREMS ( 'IMAGE QUEUE' )
C
C           removes these items from the board without allocating space
C           for the return of any values. Removing an item that is not
C           on the board does not cause an error.
C
C     Stacks and Queues
C
C           The list of values associated with a multi-valued item
C           may be thought of as a stack or queue. The values can be
C           popped (copied and removed) from this list in pieces,
C           instead of all at once. Thus, the images in 'IMAGE QUEUE'
C           can be processed as shown below.
C
C              DO I = 1, 2
C                 CALL BBGETC ( 'POP', 'IMAGE QUEUE', 1, IMAGE )
C                 CALL BBGETI ( 'POP', 'IMAGE QUEUE', 1, BODY  )
C                 CALL BBGETD ( 'POP', 'IMAGE QUEUE', 3, RADII )
C                  .
C                  .
C
C              END DO
C
C           Values may be added to the beginning of the value list
C           (treating it as a push-down stack), or to the end of the
C           list (treating it as a queue). The following sequence
C
C              CALL BBPUTI ( 'POST', 'SEQUENCE', 1, 5 )
C
C              DATA(1) = 1
C              DATA(2) = 2
C              DATA(3) = 3
C              DATA(4) = 4
C              CALL BBPUTI ( 'PUSH', 'SEQUENCE', 4, DATA )
C
C              DATA(1) = 6
C              DATA(2) = 7
C              DATA(3) = 8
C              DATA(4) = 9
C              CALL BBPUTI ( 'APPEND', 'SEQUENCE', 4, DATA )
C
C           creates an integer item 'SEQUENCE' containing the numbers
C           1-9 in order.
C
C           Pushing or appending values onto an item that doesn't exist
C           causes the item to be created.
C
C     Finding items
C
C           Attempting to copy, take, or pop values from an item not on
C           the board results in an error (which is reported through the
C           normal SPICELIB error handling mechanism). The presence of
C           an item may be confirmed by calling BBFNDx. For example,
C           the call
C
C              CALL BBFNDI ( 'SEQUENCE', N )
C
C           returns a value of 9 in N, because 'SEQUENCE' contains nine
C           values. Items not on the board contain zero values. (Note
C           that BBFNDS, which finds string items, can only return one
C           or zero.)
C
C     Clearing the board
C
C           The entire board may be cleared at any time by calling
C           BBCLR,
C
C              CALL BBCLR
C
C           The board MUST be cleared at least once (usually by the
C           main module of the calling program) before any items can
C           be posted.
C
C     Storage
C
C           Because standard Fortran-77 does not allow storage to be
C           allocated dynamically, the storage used by the bulletin
C           board must be allocated when BBOARD is compiled, by
C           setting the values of the parameters MAXNL, MAXCL, MAXI,
C           MAXD, MAXS, MAXIV, MAXDV, MAXCV, and MAXCHR.
C
C$ Examples
C
C     Consider the following program,
C
C        PROGRAM SIMPLE
C
C        CALL BBCLR
C
C        CALL READ
C        CALL BBFNDS ( 'NAME', N )
C
C        DO WHILE ( N .GT. 0 )
C           CALL LOOK_UP
C           CALL COMPUTE
C           CALL PRINT
C
C           CALL READ
C           CALL BBFNDS ( 'NAME', N )
C        END DO
C
C        END
C
C     which calls four modules:
C
C        READ      which reads the name of a picture file from the
C                  standard input file, and places it on the bulletin
C                  board as string item 'NAME'.
C
C        LOOK_UP   which looks up the spacecraft event time, filter
C                  number, exposure time, and pointing angles for the
C                  picture in the picture file.
C
C        COMPUTE   which computes the equivalent pointing in two
C                  auxiliary coordinate systems.
C
C        PRINT     which prints everything to the standard output file.
C
C     The program begins by clearing the bulletin board. This prepares
C     the board for use by the rest of the program.
C
C     READ begins by removing item NAME from the board. It then attempts
C     to read the name of the next picture file. If successful, it posts
C     the name. (If not the board will not contain the item, and the
C     program will terminate.)
C
C        SUBROUTINE READ
C
C        CHARACTER*128   FILE
C        INTEGER         IOSTAT
C
C        CALL BBREMS ( 'NAME' )
C        READ (*,*,IOSTAT=IOSTAT) FILE
C
C        IF ( IOSTAT .EQ. 0 ) THEN
C           CALL BBPUTS ( 'POST', 'NAME', FILE )
C        END IF
C
C        RETURN
C        END
C
C     LOOK_UP uses the name of the file as an index into a database
C     (the details of which are not important). It retrieves the items
C     of interest from the database, and posts them on the board.
C     (Note that the spacecraft event time is posted in UTC and ET.)
C
C
C        SUBROUTINE LOOK_UP
C
C        CALL BBGETS ( 'COPY', 'NAME', INDEX )
C         .
C         .
C
C        CALL BBPUTS ( 'POST', 'S/C EVENT (UTC)',    UTC   )
C        CALL BBPUTD ( 'POST', 'S/C EVENT (ET)',  1, ET    )
C        CALL BBPUTI ( 'POST', 'FILTER NUMBER',   1, FNUM  )
C        CALL BBPUTD ( 'POST', 'EXPOSURE',        1, EXP   )
C        CALL BBPUTD ( 'POST', 'POINTING (CCT)',  3, CCT   )
C
C        RETURN
C        END
C
C     COMPUTE begins with the nominal (Clock, Cone, Twist) pointing
C     and the spacecraft event time, and computes the equivalent
C     pointing in two other systems: Azimuth, Elevation, Twist; and
C     Right ascension, Declination, Twist. (Again, the details are not
C     important.) These are stored on the board.
C
C     The conversion depends on an optional bias angle, which may
C     or may not be posted. If not found, it defaults to zero.
C
C        SUBROUTINE COMPUTE
C         .
C         .
C
C        CALL BBGETD ( 'COPY', 'POINTING (CCT)', N, CCT )
C        CALL BBGETD ( 'COPY', 'S/C EVENT (ET)', N, ET  )
C
C        CALL BBFNDD ( 'BIAS', N )
C        IF ( N .EQ. 1 ) THEN
C           CALL BBGETD ( 'COPY', 'BIAS', N, BIAS )
C        ELSE
C           BIAS = 0.D0
C        END IF
C         .
C         .
C
C        CALL BBPUTD ( 'POST', 'POINTING (AET)', 3, AET )
C        CALL BBPUTD ( 'POST', 'POINTING (RDT)', 3, RDT )
C
C        RETURN
C        END
C
C     PRINT simply retrieves the items from the board and writes
C     them to the standard output file. The items are removed from
C     the board as their values are printed, freeing space for use
C     by other parts of the program. (This is largely a preventative
C     measure: it is not necessary for the program as it stands,
C     but it could become important as the program undergoes further
C     development.)
C
C        SUBROUTINE PRINT
C         .
C         .
C
C        CALL BBGETS ( 'TAKE', 'NAME', STRING )
C        WRITE (*,*)
C        WRITE (*,*) STRING
C
C         .
C         .
C
C        CALL BBGETS ( 'TAKE', 'POINTING (RDT)', N, NUMBERS )
C        WRITE (*,*) ( NUMBERS(I), I = 1, N )
C
C        RETURN
C        END
C
C$ Restrictions
C
C     1) The values of parameters MAXNL and MAXCL must not be smaller
C        than the value of parameter MINLEN in subroutine ENCHAR.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     Dagny Taggart  (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 5-APR-1989 (DMT)
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
      INTEGER               SYDIMC
      INTEGER               SYDIMD
      INTEGER               SYDIMI
 
C
C     Local variables
C
C     Integer, DP, and character items are stored in symbol tables.
C     Later, they should be stored in card catalogs, when the necessary
C     routines have been completed.
C
C     Strings are stored in a string buffer.
C
C     Actions, where input, are compressed and converted to uppercase
C     (WHAT). Item names are compressed (WHICH).
C
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      INTEGER               LBCBUF
      PARAMETER           ( LBCBUF =  0 )
 
      CHARACTER*(MAXNL)     INTAB     ( LBCELL:MAXI  )
      INTEGER               IPTAB     ( LBCELL:MAXI  )
      INTEGER               IVTAB     ( LBCELL:MAXIV )
 
      CHARACTER*(MAXNL)     DNTAB     ( LBCELL:MAXD  )
      INTEGER               DPTAB     ( LBCELL:MAXD  )
      DOUBLE PRECISION      DVTAB     ( LBCELL:MAXDV )
 
      CHARACTER*(MAXNL)     CNTAB     ( LBCELL:MAXC  )
      INTEGER               CPTAB     ( LBCELL:MAXC  )
      CHARACTER*(MAXCL)     CVTAB     ( LBCELL:MAXCV )
 
      CHARACTER*(MAXNL)     NBUF      ( LBCELL:MAXS         )
      INTEGER               PBUF      ( LBCELL:MAXS * 4 + 4 )
      CHARACTER*(100)       VBUF      ( LBCBUF:MAXCHR / 100 )
 
      CHARACTER*(MAXNL)     WHAT
      CHARACTER*(MAXNL)     WHICH
 
      INTEGER               POS
      INTEGER               I
      LOGICAL               FND
 
C
C     Save everything between calls.
C
      SAVE
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'BBOARD' )
      END IF
 
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
 
      CALL CHKOUT ( 'BBOARD' )
      RETURN
 
 
 
C$Procedure BBPUTI ( Bulletin board, put, integer )
 
      ENTRY BBPUTI_1 ( ACTION, ITEM, N, IVALS )
 
C$ Abstract
C
C     Put one or more values on the board, associated with
C     an integer item.
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
C     BBOARD
C
C$ Keywords
C
C     UTILITY
C
C$ Declarations
C
C     CHARACTER*(*)         ACTION
C     CHARACTER*(*)         ITEM
C     INTEGER               N
C     INTEGER               IVALS
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ACTION     I   Action: 'POST', 'PUSH', or 'APPEND'.
C     ITEM       I   Item name.
C     N          I   Number of values to be posted.
C     IVALS      I   Values to be posted.
C
C$ Detailed_Input
C
C     ACTION      specifies an action to be taken. Possible actions
C                 are 'POST', 'PUSH', and 'APPEND'.
C
C     ITEM        is the name of an integer item, which may or
C                 may not be on the board already.
C
C     N           is the number of values to be associated with the
C                 specified item.
C
C     IVALS       are the values.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     See BBOARD.
C
C$ Exceptions
C
C     1) If ACTION is not recognized, the error 'SPICE(UNNATURALACT)'
C        is signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     'POST' creates a new item, containing the specified values.
C     (If an item of the same type with the same name already exists,
C     it is replaced.)
C
C     'PUSH' modifies the list of values associated with an existing
C     item by placing items at the beginning of the list (treating the
C     list as a push-down stack).
C
C     'APPEND' modifies the list of values associated with an existing
C     item by placing items at the end of the list (treating the list
C     as a queue).
C
C     Both 'PUSH' and 'APPEND' will create a new item if the specified
C     item does not exist.
C
C$ Examples
C
C     See BBOARD.
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
C     Dagny Taggart  (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 5-APR-1989 (DMT)
C
C-&
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'BBPUTI_1' )
      END IF
 
C
C     Compress spaces, change cases, as needed.
C
      CALL CMPRSS ( ' ', 0, ACTION, WHAT       )
      CALL UCASE  (                 WHAT, WHAT )
 
      CALL CMPRSS ( ' ', 0, ITEM, WHICH )
 
C
C     The real work is done by the symbol table routines. (Later,
C     it will be done by the card catalog routines.) Note that
C     items must be pushed and appended one at a time.
C
      IF ( WHAT .EQ. 'POST' ) THEN
         CALL SYPUTI ( WHICH, IVALS, N, INTAB, IPTAB, IVTAB )
 
      ELSE IF ( WHAT .EQ. 'PUSH' ) THEN
         DO I = N, 1, -1
            CALL SYPSHI ( WHICH, IVALS(I), INTAB, IPTAB, IVTAB )
         END DO
 
      ELSE IF ( WHAT .EQ. 'APPEND' ) THEN
         DO I = 1, N
            CALL SYENQI ( WHICH, IVALS(I), INTAB, IPTAB, IVTAB )
         END DO
 
      ELSE
         CALL SETMSG ( 'Sorry, # is not a legal action.' )
         CALL ERRCH  ( '#', WHAT                         )
         CALL SIGERR ( 'SPICE(UNNATURALACT)' )
      END IF
 
      CALL CHKOUT ( 'BBPUTI_1' )
      RETURN
 
 
C$Procedure BBPUTD ( Bulletin board, put, DP )
 
      ENTRY BBPUTD_1 ( ACTION, ITEM, N, DVALS )
 
C$ Abstract
C
C     Put one or more values on the board, associated with
C     a DP item.
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
C     BBOARD
C
C$ Keywords
C
C     UTILITY
C
C$ Declarations
C
C     CHARACTER*(*)         ACTION
C     CHARACTER*(*)         ITEM
C     INTEGER               N
C     DOUBLE PRECISION      DVALS
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ACTION     I   Action: 'POST', 'PUSH', or 'APPEND'.
C     ITEM       I   Item name.
C     N          I   Number of values to be posted.
C     DVALS      I   Values to be posted.
C
C$ Detailed_Input
C
C     ACTION      specifies an action to be taken. Possible actions
C                 are 'POST', 'PUSH', and 'APPEND'.
C
C     ITEM        is the name of a DP item, which may or
C                 may not be on the board already.
C
C     N           is the number of values to be associated with the
C                 specified item.
C
C     DVALS       are the values.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     See BBOARD.
C
C$ Exceptions
C
C     1) If ACTION is not recognized, the error 'SPICE(UNNATURALACT)'
C        is signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     'POST' creates a new item, containing the specified values.
C     (If an item of the same type with the same name already exists,
C     it is replaced.)
C
C     'PUSH' modifies the list of values associated with an existing
C     item by placing items at the beginning of the list (treating the
C     list as a push-down stack).
C
C     'APPEND' modifies the list of values associated with an existing
C     item by placing items at the end of the list (treating the list
C     as a queue).
C
C     Both 'PUSH' and 'APPEND' will create a new item if the specified
C     item does not exist.
C
C$ Examples
C
C     See BBOARD.
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
C     Dagny Taggart  (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 5-APR-1989 (DMT)
C
C-&
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'BBPUTD_1' )
      END IF
 
C
C     Compress spaces, change cases, as needed.
C
      CALL CMPRSS ( ' ', 0, ACTION, WHAT       )
      CALL UCASE  (                 WHAT, WHAT )
 
      CALL CMPRSS ( ' ', 0, ITEM, WHICH )
 
C
C     The real work is done by the symbol table routines. (Later,
C     it will be done by the card catalog routines.) Note that
C     items must be pushed and appended one at a time.
C
      IF ( WHAT .EQ. 'POST' ) THEN
         CALL SYPUTD ( WHICH, DVALS, N, DNTAB, DPTAB, DVTAB )
 
      ELSE IF ( WHAT .EQ. 'PUSH' ) THEN
         DO I = N, 1, -1
            CALL SYPSHD ( WHICH, DVALS(I), DNTAB, DPTAB, DVTAB )
         END DO
 
      ELSE IF ( WHAT .EQ. 'APPEND' ) THEN
         DO I = 1, N
            CALL SYENQD ( WHICH, DVALS(I), DNTAB, DPTAB, DVTAB )
         END DO
 
      ELSE
         CALL SETMSG ( 'Sorry, # is not a legal action.' )
         CALL ERRCH  ( '#', WHAT                         )
         CALL SIGERR ( 'SPICE(UNNATURALACT)' )
      END IF
 
      CALL CHKOUT ( 'BBPUTD_1' )
      RETURN
 
 
C$Procedure BBPUTC ( Bulletin board, put, character )
 
      ENTRY BBPUTC_1 ( ACTION, ITEM, N, CVALS )
 
C$ Abstract
C
C     Put one or more values on the board, associated with
C     a character item.
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
C     BBOARD
C
C$ Keywords
C
C     UTILITY
C
C$ Declarations
C
C     CHARACTER*(*)         ACTION
C     CHARACTER*(*)         ITEM
C     INTEGER               N
C     CHARACTER*(*)         CVALS
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ACTION     I   Action: 'POST', 'PUSH', or 'APPEND'.
C     ITEM       I   Item name.
C     N          I   Number of values to be posted.
C     CVALS      I   Values to be posted.
C
C$ Detailed_Input
C
C     ACTION      specifies an action to be taken. Possible actions
C                 are 'POST', 'PUSH', and 'APPEND'.
C
C     ITEM        is the name of a character item, which may or
C                 may not be on the board already.
C
C     N           is the number of values to be associated with the
C                 specified item.
C
C     CVALS       are the values.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     See BBOARD.
C
C$ Exceptions
C
C     1) If ACTION is not recognized, the error 'SPICE(UNNATURALACT)'
C        is signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     'POST' creates a new item, containing the specified values.
C     (If an item of the same type with the same name already exists,
C     it is replaced.)
C
C     'PUSH' modifies the list of values associated with an existing
C     item by placing items at the beginning of the list (treating the
C     list as a push-down stack).
C
C     'APPEND' modifies the list of values associated with an existing
C     item by placing items at the end of the list (treating the list
C     as a queue).
C
C     Both 'PUSH' and 'APPEND' will create a new item if the specified
C     item does not exist.
C
C$ Examples
C
C     See BBOARD.
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
C     Dagny Taggart  (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 5-APR-1989 (DMT)
C
C-&
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'BBPUTC_1' )
      END IF
 
C
C     Compress spaces, change cases, as needed.
C
      CALL CMPRSS ( ' ', 0, ACTION, WHAT       )
      CALL UCASE  (                 WHAT, WHAT )
 
      CALL CMPRSS ( ' ', 0, ITEM, WHICH )
 
C
C     The real work is done by the symbol table routines. (Later,
C     it will be done by the card catalog routines.) Note that
C     items must be pushed and appended one at a time.
C
      IF ( WHAT .EQ. 'POST' ) THEN
         CALL SYPUTC ( WHICH, CVALS, N, CNTAB, CPTAB, CVTAB )
 
      ELSE IF ( WHAT .EQ. 'PUSH' ) THEN
         DO I = N, 1, -1
            CALL SYPSHC ( WHICH, CVALS(I), CNTAB, CPTAB, CVTAB )
         END DO
 
      ELSE IF ( WHAT .EQ. 'APPEND' ) THEN
         DO I = 1, N
            CALL SYENQC ( WHICH, CVALS(I), CNTAB, CPTAB, CVTAB )
         END DO
 
      ELSE
         CALL SETMSG ( 'Sorry, # is not a legal action.' )
         CALL ERRCH  ( '#', WHAT                         )
         CALL SIGERR ( 'SPICE(UNNATURALACT)' )
      END IF
 
      CALL CHKOUT ( 'BBPUTC_1' )
      RETURN
 
 
C$Procedure BBPUTS ( Bulletin board, put, string )
 
      ENTRY BBPUTS_1 ( ACTION, ITEM, SVAL )
 
C$ Abstract
C
C     Put a value on the board, associated with a string item.
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
C     BBOARD
C
C$ Keywords
C
C     UTILITY
C
C$ Declarations
C
C     CHARACTER*(*)         ACTION
C     CHARACTER*(*)         ITEM
C     INTEGER               N
C     CHARACTER*(*)         SVAL
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ACTION     I   Action: 'POST'.
C     ITEM       I   Item name.
C     SVAL       I   Value to be posted.
C
C$ Detailed_Input
C
C     ACTION      specifies an action to be taken. Currently, the
C                 only possible action is 'POST'.
C
C     ITEM        is the name of a string item, which may or
C                 may not be on the board already.
C
C     SVAL        is the value to be associated with the specified
C                 item. Trailing blanks are ignored.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     See BBOARD.
C
C$ Exceptions
C
C     1) If ACTION is not recognized, the error 'SPICE(UNNATURALACT)'
C        is signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     'POST' creates a new item, containing the specified value.
C     (If an item of the same type with the same name already exists,
C     it is replaced.)
C
C$ Examples
C
C     See BBOARD.
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
C     Dagny Taggart  (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 5-APR-1989 (DMT)
C
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'BBPUTS_1' )
      END IF
 
      CALL CMPRSS ( ' ', 0, ACTION, WHAT       )
      CALL UCASE  (                 WHAT, WHAT )
 
      CALL CMPRSS ( ' ', 0, ITEM, WHICH )
 
      IF ( WHAT .EQ. 'POST' ) THEN
         CALL SBSET_1 ( WHICH, SVAL, NBUF, PBUF, VBUF )
 
      ELSE
         CALL SETMSG ( 'Sorry, # is not a legal action.' )
         CALL ERRCH  ( '#', WHAT                         )
         CALL SIGERR ( 'SPICE(UNNATURALACT)' )
      END IF
 
      CALL CHKOUT ( 'BBPUTS_1' )
      RETURN
 
 
 
C$Procedure BBGETI ( Bulletin board, get, integer )
 
      ENTRY BBGETI_1 ( ACTION, ITEM, N, IVALS )
 
C$ Abstract
C
C     Get one or more values from the board, associated with
C     an integer item.
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
C     BBOARD
C
C$ Keywords
C
C     UTILITY
C
C$ Declarations
C
C     CHARACTER*(*)         ACTION
C     CHARACTER*(*)         ITEM
C     INTEGER               N
C     INTEGER               IVALS
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ACTION     I   Action: 'COPY', 'TAKE', or 'POP'.
C     ITEM       I   Item name.
C     N         I,O  Number of values returned.
C     IVALS      O   Values.
C
C$ Detailed_Input
C
C     ACTION      specifies an action to be taken. Possible actions
C                 are 'COPY', 'TAKE', and 'POP'.
C
C     ITEM        is the name of an integer item, which must be
C                 on the board.
C
C     N           on input is the number of values to be popped.
C
C
C$ Detailed_Output
C
C     N           on output is the number of values returned.
C
C     IVALS       are some or all of the values associated with the
C                 specified item.
C
C$ Parameters
C
C     See BBOARD.
C
C$ Exceptions
C
C     1) If ACTION is not recognized, the error 'SPICE(UNNATURALACT)'
C        is signalled.
C
C     2) If ITEM is not found, or if the number of values to be popped
C        is smaller than the number of values associated with the item,
C        the error 'SPICE(ALLGONE)' is signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     'COPY' returns all of the values associated with the specified
C     item. The number of values is returned in N. Copying an item
C     leaves the item intact.
C
C     'TAKE' returns all of the values associated with the specified
C     item. The number of values is returned in N. Unlike copying,
C     taking an item removes the item from the board.
C
C     'POP' takes some of the values associated with the specified
C     item. Items are taken from the front of the list; the remaining
C     values are left intact. The number of values to be popped is
C     specified in N. Popping the final value of an item removes the
C     item from the board.
C
C$ Examples
C
C     See BBOARD.
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
C     Dagny Taggart  (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 5-APR-1989 (DMT)
C
C-&
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'BBGETI_1' )
      END IF
 
C
C     Compress spaces, change cases, as needed.
C
      CALL CMPRSS ( ' ', 0, ACTION, WHAT       )
      CALL UCASE  (                 WHAT, WHAT )
 
      CALL CMPRSS ( ' ', 0, ITEM, WHICH )
 
C
C     The real work is done by the symbol table routines. (Later,
C     it will be done by the card catalog routines.) Note that
C     items must be popped one at a time.
C
      IF ( WHAT .EQ. 'COPY'   .OR.  WHAT .EQ. 'TAKE' ) THEN
         CALL SYGETI ( WHICH, INTAB, IPTAB, IVTAB, N, IVALS, FND )
 
         IF ( .NOT. FND ) THEN
            CALL SETMSG ( 'Could not find item #.' )
            CALL ERRCH  ( '#', WHICH               )
            CALL SIGERR ( 'SPICE(ALLGONE)'         )
 
         ELSE IF ( WHAT .EQ. 'TAKE' ) THEN
            CALL SYDELI ( WHICH, INTAB, IPTAB, IVTAB )
         END IF
 
      ELSE IF ( WHAT .EQ. 'POP' ) THEN
         DO I = 1, N
            CALL SYPOPI ( WHICH, INTAB, IPTAB, IVTAB, IVALS(I), FND )
         END DO
 
         IF ( .NOT. FND ) THEN
            CALL SETMSG ( 'Could not find item #.' )
            CALL ERRCH  ( '#', WHICH               )
            CALL SIGERR ( 'SPICE(ALLGONE)'         )
         END IF
 
      ELSE
         CALL SETMSG ( 'Sorry, # is not a legal action.' )
         CALL ERRCH  ( '#', WHAT                         )
         CALL SIGERR ( 'SPICE(UNNATURALACT)' )
      END IF
 
      CALL CHKOUT ( 'BBGETI_1' )
      RETURN
 
 
C$Procedure BBGETD ( Bulletin board, get, DP )
 
      ENTRY BBGETD_1 ( ACTION, ITEM, N, DVALS )
 
C$ Abstract
C
C     Get one or more values from the board, associated with
C     a DP item.
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
C     BBOARD
C
C$ Keywords
C
C     UTILITY
C
C$ Declarations
C
C     CHARACTER*(*)         ACTION
C     CHARACTER*(*)         ITEM
C     INTEGER               N
C     DOUBLE PRECISION      DVALS
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ACTION     I   Action: 'COPY', 'TAKE', or 'POP'.
C     ITEM       I   Item name.
C     N         I,O  Number of values returned.
C     DVALS      O   Values.
C
C$ Detailed_Input
C
C     ACTION      specifies an action to be taken. Possible actions
C                 are 'COPY', 'TAKE', and 'POP'.
C
C     ITEM        is the name of a DP item, which must be
C                 on the board.
C
C     N           on input is the number of values to be popped.
C
C
C$ Detailed_Output
C
C     N           on output is the number of values returned.
C
C     DVALS       are some or all of the values associated with the
C                 specified item.
C
C$ Parameters
C
C     See BBOARD.
C
C$ Exceptions
C
C     1) If ACTION is not recognized, the error 'SPICE(UNNATURALACT)'
C        is signalled.
C
C     2) If ITEM is not found, or if the number of values to be popped
C        is smaller than the number of values associated with the item,
C        the error 'SPICE(ALLGONE)' is signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     'COPY' returns all of the values associated with the specified
C     item. The number of values is returned in N. Copying an item
C     leaves the item intact.
C
C     'TAKE' returns all of the values associated with the specified
C     item. The number of values is returned in N. Unlike copying,
C     taking an item removes the item from the board.
C
C     'POP' takes some of the values associated with the specified
C     item. Items are taken from the front of the list; the remaining
C     values are left intact. The number of values to be popped is
C     specified in N. Popping the final value of an item removes the
C     item from the board.
C
C$ Examples
C
C     See BBOARD.
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
C     Dagny Taggart  (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 5-APR-1989 (DMT)
C
C-&
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'BBGETD_1' )
      END IF
 
C
C     Compress spaces, change cases, as needed.
C
      CALL CMPRSS ( ' ', 0, ACTION, WHAT       )
      CALL UCASE  (                 WHAT, WHAT )
 
      CALL CMPRSS ( ' ', 0, ITEM, WHICH )
 
C
C     The real work is done by the symbol table routines. (Later,
C     it will be done by the card catalog routines.) Note that
C     items must be popped one at a time.
C
      IF ( WHAT .EQ. 'COPY'   .OR.  WHAT .EQ. 'TAKE' ) THEN
         CALL SYGETD ( WHICH, DNTAB, DPTAB, DVTAB, N, DVALS, FND )
 
         IF ( .NOT. FND ) THEN
            CALL SETMSG ( 'Could not find item #.' )
            CALL ERRCH  ( '#', WHICH               )
            CALL SIGERR ( 'SPICE(ALLGONE)'         )
 
         ELSE IF ( WHAT .EQ. 'TAKE' ) THEN
            CALL SYDELD ( WHICH, DNTAB, DPTAB, DVTAB )
         END IF
 
      ELSE IF ( WHAT .EQ. 'POP' ) THEN
         DO I = 1, N
            CALL SYPOPD ( WHICH, DNTAB, DPTAB, DVTAB, DVALS(I), FND )
         END DO
 
         IF ( .NOT. FND ) THEN
            CALL SETMSG ( 'Could not find item #.' )
            CALL ERRCH  ( '#', WHICH               )
            CALL SIGERR ( 'SPICE(ALLGONE)'         )
         END IF
 
      ELSE
         CALL SETMSG ( 'Sorry, # is not a legal action.' )
         CALL ERRCH  ( '#', WHAT                         )
         CALL SIGERR ( 'SPICE(UNNATURALACT)' )
      END IF
 
      CALL CHKOUT ( 'BBGETD_1' )
      RETURN
 
 
C$Procedure BBGETC ( Bulletin board, get, character )
 
      ENTRY BBGETC_1 ( ACTION, ITEM, N, CVALS )
 
C$ Abstract
C
C     Get one or more values from the board, associated with
C     a character item.
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
C     BBOARD
C
C$ Keywords
C
C     UTILITY
C
C$ Declarations
C
C     CHARACTER*(*)         ACTION
C     CHARACTER*(*)         ITEM
C     INTEGER               N
C     CHARACTER*(*)         CVALS
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ACTION     I   Action: 'COPY', 'TAKE', or 'POP'.
C     ITEM       I   Item name.
C     N         I,O  Number of values returned.
C     CVALS      O   Values.
C
C$ Detailed_Input
C
C     ACTION      specifies an action to be taken. Possible actions
C                 are 'COPY', 'TAKE', and 'POP'.
C
C     ITEM        is the name of a character item, which must be
C                 on the board.
C
C     N           on input is the number of values to be popped.
C
C
C$ Detailed_Output
C
C     N           on output is the number of values returned.
C
C     CVALS       are some or all of the values associated with the
C                 specified item.
C
C$ Parameters
C
C     See BBOARD.
C
C$ Exceptions
C
C     1) If ACTION is not recognized, the error 'SPICE(UNNATURALACT)'
C        is signalled.
C
C     2) If ITEM is not found, or if the number of values to be popped
C        is smaller than the number of values associated with the item,
C        the error 'SPICE(ALLGONE)' is signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     'COPY' returns all of the values associated with the specified
C     item. The number of values is returned in N. Copying an item
C     leaves the item intact.
C
C     'TAKE' returns all of the values associated with the specified
C     item. The number of values is returned in N. Unlike copying,
C     taking an item removes the item from the board.
C
C     'POP' takes some of the values associated with the specified
C     item. Items are taken from the front of the list; the remaining
C     values are left intact. The number of values to be popped is
C     specified in N. Popping the final value of an item removes the
C     item from the board.
C
C$ Examples
C
C     See BBOARD.
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
C     Dagny Taggart  (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 5-APR-1989 (DMT)
C
C-&
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'BBGETC_1' )
      END IF
 
C
C     Compress spaces, change cases, as needed.
C
      CALL CMPRSS ( ' ', 0, ACTION, WHAT       )
      CALL UCASE  (                 WHAT, WHAT )
 
      CALL CMPRSS ( ' ', 0, ITEM, WHICH )
 
C
C     The real work is done by the symbol table routines. (Later,
C     it will be done by the card catalog routines.) Note that
C     items must be popped one at a time.
C
      IF ( WHAT .EQ. 'COPY'   .OR.  WHAT .EQ. 'TAKE' ) THEN
         CALL SYGETC ( WHICH, CNTAB, CPTAB, CVTAB, N, CVALS, FND )
 
         IF ( .NOT. FND ) THEN
            CALL SETMSG ( 'Could not find item #.' )
            CALL ERRCH  ( '#', WHICH               )
            CALL SIGERR ( 'SPICE(ALLGONE)'         )
 
         ELSE IF ( WHAT .EQ. 'TAKE' ) THEN
            CALL SYDELC ( WHICH, CNTAB, CPTAB, CVTAB )
         END IF
 
      ELSE IF ( WHAT .EQ. 'POP' ) THEN
         DO I = 1, N
            CALL SYPOPC ( WHICH, CNTAB, CPTAB, CVTAB, CVALS(I), FND )
         END DO
 
         IF ( .NOT. FND ) THEN
            CALL SETMSG ( 'Could not find item #.' )
            CALL ERRCH  ( '#', WHICH               )
            CALL SIGERR ( 'SPICE(ALLGONE)'         )
         END IF
 
      ELSE
         CALL SETMSG ( 'Sorry, # is not a legal action.' )
         CALL ERRCH  ( '#', WHAT                         )
         CALL SIGERR ( 'SPICE(UNNATURALACT)' )
      END IF
 
      CALL CHKOUT ( 'BBGETC_1' )
      RETURN
 
 
C$Procedure BBGETS ( Bulletin board, get, string )
 
      ENTRY BBGETS_1 ( ACTION, ITEM, SVAL )
 
C$ Abstract
C
C     Get a value from the board, associated with a string item.
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
C     BBOARD
C
C$ Keywords
C
C     UTILITY
C
C$ Declarations
C
C     CHARACTER*(*)         ACTION
C     CHARACTER*(*)         ITEM
C     CHARACTER*(*)         SVAL
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ACTION     I   Action: 'COPY' or 'TAKE'.
C     ITEM       I   Item name.
C     SVAL       O   Value.
C
C$ Detailed_Input
C
C     ACTION      specifies an action to be taken. Possible actions
C                 are 'COPY' and 'TAKE'.
C
C     ITEM        is the name of a string item, which must be
C                 on the board.
C
C$ Detailed_Output
C
C     SVAL        is the value associated with the specified item.
C
C$ Parameters
C
C     See BBOARD.
C
C$ Exceptions
C
C     1) If ACTION is not recognized, the error 'SPICE(UNNATURALACT)'
C        is signalled.
C
C     2) If ITEM is not found, the error 'SPICE(ALLGONE)' is signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     'COPY' returns the value associated with the specified item.
C     Copying an item leaves the item intact.
C
C     'TAKE' returns the value associated with the specified item.
C     Unlike copying, taking an item removes the item from the board.
C
C$ Examples
C
C     See BBOARD.
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
C     Dagny Taggart  (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 5-APR-1989 (DMT)
C
C-&
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'BBGETS_1' )
      END IF
 
      CALL CMPRSS ( ' ', 0, ACTION, WHAT       )
      CALL UCASE  (                 WHAT, WHAT )
 
      CALL CMPRSS ( ' ', 0, ITEM, WHICH )
 
      IF ( WHAT .EQ. 'COPY'   .OR.  WHAT .EQ. 'TAKE' ) THEN
         CALL SBGET_1 ( WHICH, NBUF, PBUF, VBUF, SVAL, POS )
 
         IF ( POS .EQ. 0 ) THEN
            CALL SETMSG ( 'Could not find item #.' )
            CALL ERRCH  ( '#', WHICH               )
            CALL SIGERR ( 'SPICE(ALLGONE)'         )
 
         ELSE IF ( WHAT .EQ. 'TAKE' ) THEN
            CALL SBREM_1 ( WHICH, NBUF, PBUF, VBUF )
         END IF
 
      ELSE
         CALL SETMSG ( 'Sorry, # is not a legal action.' )
         CALL ERRCH  ( '#', WHAT                         )
         CALL SIGERR ( 'SPICE(UNNATURALACT)' )
      END IF
 
      CALL CHKOUT ( 'BBGETS_1' )
      RETURN
 
 
 
C$Procedure BBREMI ( Bulletin board, remove, integer )
 
      ENTRY BBREMI_1 ( ITEM )
 
C$ Abstract
C
C     Remove an integer item from the board.
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
C     BBOARD
C
C$ Keywords
C
C     UTILITY
C
C$ Declarations
C
C     CHARACTER*(*)         ITEM
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ITEM       I   Item name.
C
C$ Detailed_Input
C
C     ITEM        is the name of an integer item, which may or
C                 may not be on the board.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     See BBOARD.
C
C$ Exceptions
C
C     1) If ITEM is not recognized, the board is not changed.
C        No error occurs.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Items may also be removed by calling BBGETI, using 'TAKE'
C     or 'POP'. However, BBREMI allows you to remove an item without
C     providing space for its values. Also, it does not cause an
C     error if the item is not on the board.
C
C$ Examples
C
C     See BBOARD.
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
C     Dagny Taggart  (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 5-APR-1989 (DMT)
C
C-&
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'BBREMI_1' )
      END IF
 
C
C     Compress spaces as needed.
C
      CALL CMPRSS ( ' ', 0, ITEM, WHICH )
 
C
C     The real work is done by a symbol table routine. (Later,
C     it will be done by a card catalog routine.)
C
      CALL SYDELI ( WHICH, INTAB, IPTAB, IVTAB )
 
      CALL CHKOUT ( 'BBREMI_1' )
      RETURN
 
 
C$Procedure BBREMD ( Bulletin board, remove, DP )
 
      ENTRY BBREMD_1 ( ITEM )
 
C$ Abstract
C
C     Remove a DP item from the board.
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
C     BBOARD
C
C$ Keywords
C
C     UTILITY
C
C$ Declarations
C
C     CHARACTER*(*)         ITEM
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ITEM       I   Item name.
C
C$ Detailed_Input
C
C     ITEM        is the name of a DP item, which may or
C                 may not be on the board.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     See BBOARD.
C
C$ Exceptions
C
C     1) If ITEM is not recognized, the board is not changed.
C        No error occurs.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Items may also be removed by calling BBGETD, using 'TAKE'
C     or 'POP'. However, BBREMD allows you to remove an item without
C     providing space for its values. Also, it does not cause an
C     error if the item is not on the board.
C
C$ Examples
C
C     See BBOARD.
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
C     Dagny Taggart  (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 5-APR-1989 (DMT)
C
C-&
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'BBREMD_1' )
      END IF
 
C
C     Compress spaces as needed.
C
      CALL CMPRSS ( ' ', 0, ITEM, WHICH )
 
C
C     The real work is done by a symbol table routine. (Later,
C     it will be done by a card catalog routine.)
C
      CALL SYDELD ( WHICH, DNTAB, DPTAB, DVTAB )
 
      CALL CHKOUT ( 'BBREMD_1' )
      RETURN
 
 
C$Procedure BBREMC ( Bulletin board, remove, character )
 
      ENTRY BBREMC_1 ( ITEM )
 
C$ Abstract
C
C     Remove a character item from the board.
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
C     BBOARD
C
C$ Keywords
C
C     UTILITY
C
C$ Declarations
C
C     CHARACTER*(*)         ITEM
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ITEM       I   Item name.
C
C$ Detailed_Input
C
C     ITEM        is the name of a character item, which may or
C                 may not be on the board.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     See BBOARD.
C
C$ Exceptions
C
C     1) If ITEM is not recognized, the board is not changed.
C        No error occurs.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Items may also be removed by calling BBGETC, using 'TAKE'
C     or 'POP'. However, BBREMC allows you to remove an item without
C     providing space for its values. Also, it does not cause an
C     error if the item is not on the board.
C
C$ Examples
C
C     See BBOARD.
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
C     Dagny Taggart  (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 5-APR-1989 (DMT)
C
C-&
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'BBREMC_1' )
      END IF
 
C
C     Compress spaces as needed.
C
      CALL CMPRSS ( ' ', 0, ITEM, WHICH )
 
C
C     The real work is done by a symbol table routine. (Later,
C     it will be done by a card catalog routine.)
C
      CALL SYDELC ( WHICH, CNTAB, CPTAB, CVTAB )
 
      CALL CHKOUT ( 'BBREMC_1' )
      RETURN
 
 
C$Procedure BBREMS ( Bulletin board, remove, string )
 
      ENTRY BBREMS_1 ( ITEM )
 
C$ Abstract
C
C     Remove a string item from the board.
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
C     BBOARD
C
C$ Keywords
C
C     UTILITY
C
C$ Declarations
C
C     CHARACTER*(*)         ITEM
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ITEM       I   Item name.
C
C$ Detailed_Input
C
C     ITEM        is the name of a string item, which may or
C                 may not be on the board.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     See BBOARD.
C
C$ Exceptions
C
C     1) If ITEM is not recognized, the board is not changed.
C        No error occurs.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Items may also be removed by calling BBGETS, using 'TAKE'.
C     However, BBREMS allows you to remove an item without
C     providing space for its value. Also, it does not cause an
C     error if the item is not on the board.
C
C$ Examples
C
C     See BBOARD.
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
C     Dagny Taggart  (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 5-APR-1989 (DMT)
C
C-&
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'BBREMS_1' )
      END IF
 
      CALL CMPRSS ( ' ', 0, ITEM, WHICH )
      CALL SBREM_1  ( WHICH, NBUF, PBUF, VBUF )
 
      CALL CHKOUT ( 'BBREMS_1' )
      RETURN
 
 
 
 
C$Procedure BBFNDI ( Bulletin board, find, integer )
 
      ENTRY BBFNDI_1 ( ITEM, N )
 
C$ Abstract
C
C     Find an integer item on the board.
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
C     BBOARD
C
C$ Keywords
C
C     UTILITY
C
C$ Declarations
C
C     CHARACTER*(*)         ITEM
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ITEM       I   Item name.
C     N          O   Number of values.
C
C$ Detailed_Input
C
C     ITEM        is the name of an integer item, which may or
C                 may not be on the board.
C
C$ Detailed_Output
C
C     N           is the number of values associated with the item.
C                 If the item is not on the board, N is zero.
C
C$ Parameters
C
C     See BBOARD.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     BBFNDI has two main uses:
C
C        1) To confirm that an item exists before attempting to
C           copy or take its values (anticipating a possible error).
C
C        2) To determine the number of values associated with an
C           item, so that the right number of values can be popped
C           from the value list.
C
C$ Examples
C
C     See BBOARD.
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
C     Dagny Taggart  (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 5-APR-1989 (DMT)
C
C-&
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'BBFNDI_1' )
      END IF
 
C
C     Compress spaces as needed.
C
      CALL CMPRSS ( ' ', 0, ITEM, WHICH )
 
C
C     The real work is done by a symbol table routine. (Later,
C     it will be done by a card catalog routine.)
C
      N = SYDIMI ( WHICH, INTAB, IPTAB, IVTAB )
 
      CALL CHKOUT ( 'BBFNDI_1' )
      RETURN
 
 
C$Procedure BBFNDD ( Bulletin board, find, DP )
 
      ENTRY BBFNDD_1 ( ITEM, N )
 
C$ Abstract
C
C     Find a DP item on the board.
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
C     BBOARD
C
C$ Keywords
C
C     UTILITY
C
C$ Declarations
C
C     CHARACTER*(*)         ITEM
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ITEM       I   Item name.
C     N          O   Number of values.
C
C$ Detailed_Input
C
C     ITEM        is the name of a DP item, which may or
C                 may not be on the board.
C
C$ Detailed_Output
C
C     N           is the number of values associated with the item.
C                 If the item is not on the board, N is zero.
C
C$ Parameters
C
C     See BBOARD.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     BBFNDD has two main uses:
C
C        1) To confirm that an item exists before attempting to
C           copy or take its values (anticipating a possible error).
C
C        2) To determine the number of values associated with an
C           item, so that the right number of values can be popped
C           from the value list.
C
C$ Examples
C
C     See BBOARD.
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
C     Dagny Taggart  (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 5-APR-1989 (DMT)
C
C-&
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'BBFNDD_1' )
      END IF
 
C
C     Compress spaces as needed.
C
      CALL CMPRSS ( ' ', 0, ITEM, WHICH )
 
C
C     The real work is done by a symbol table routine. (Later,
C     it will be done by a card catalog routine.)
C
      N = SYDIMD ( WHICH, DNTAB, DPTAB, DVTAB )
 
      CALL CHKOUT ( 'BBFNDD_1' )
      RETURN
 
 
C$Procedure BBFNDC ( Bulletin board, find, character )
 
      ENTRY BBFNDC_1 ( ITEM, N )
 
C$ Abstract
C
C     Find a character item on the board.
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
C     BBOARD
C
C$ Keywords
C
C     UTILITY
C
C$ Declarations
C
C     CHARACTER*(*)         ITEM
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ITEM       I   Item name.
C     N          O   Number of values.
C
C$ Detailed_Input
C
C     ITEM        is the name of a character item, which may or
C                 may not be on the board.
C
C$ Detailed_Output
C
C     N           is the number of values associated with the item.
C                 If the item is not on the board, N is zero.
C
C$ Parameters
C
C     See BBOARD.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     BBFNDC has two main uses:
C
C        1) To confirm that an item exists before attempting to
C           copy or take its values (anticipating a possible error).
C
C        2) To determine the number of values associated with an
C           item, so that the right number of values can be popped
C           from the value list.
C
C$ Examples
C
C     See BBOARD.
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
C     Dagny Taggart  (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 5-APR-1989 (DMT)
C
C-&
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'BBFNDC_1' )
      END IF
 
C
C     Compress spaces as needed.
C
      CALL CMPRSS ( ' ', 0, ITEM, WHICH )
 
C
C     The real work is done by a symbol table routine. (Later,
C     it will be done by a card catalog routine.)
C
      N = SYDIMC ( WHICH, CNTAB, CPTAB, CVTAB )
 
      CALL CHKOUT ( 'BBFNDC_1' )
      RETURN
 
 
C$Procedure BBFNDS ( Bulletin board, find, string )
 
      ENTRY BBFNDS_1 ( ITEM, N )
 
C$ Abstract
C
C     Find a string item on the board.
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
C     BBOARD
C
C$ Keywords
C
C     UTILITY
C
C$ Declarations
C
C     CHARACTER*(*)         ITEM
C     INTEGER               N
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ITEM       I   Item name.
C     N          O   Number of values.
C
C$ Detailed_Input
C
C     ITEM        is the name of a string item, which may or
C                 may not be on the board.
C
C$ Detailed_Output
C
C     N           is the number of values associated with the item.
C                 If the item is on the board, N is one. Otherwise
C                 N is zero.
C
C$ Parameters
C
C     See BBOARD.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     BBFNDS is used mainly to confirm that an item exists before
C     attempting to copy or take its value (anticipating a possible
C     error).
C
C$ Examples
C
C     See BBOARD.
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
C     Dagny Taggart  (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 5-APR-1989 (DMT)
C
C-&
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'BBFNDS_1' )
      END IF
 
      CALL CMPRSS ( ' ', 0, ITEM, WHICH )
      CALL SBGET_1  ( WHICH, NBUF, PBUF, VBUF, WHAT, POS )
 
      IF ( POS .GT. 0 ) THEN
         N = 1
      ELSE
         N = 0
      END IF
 
      CALL CHKOUT ( 'BBFNDS_1' )
      RETURN
 
 
 
 
C$Procedure BBCLR ( Bulletin board, clear )
 
      ENTRY BBCLR_1
 
C$ Abstract
C
C     Clear the entire board.
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
C     BBOARD
C
C$ Keywords
C
C     UTILITY
C
C$ Declarations
C
C     None.
C
C$ Brief_I/O
C
C     None.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     See BBOARD.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     BBCLR clears the entire bulletin board. The board MUST be
C     cleared at least once before any items can be posted.
C     This is usually done by the main module of the calling
C     program, during program initialization.
C
C$ Examples
C
C     See BBOARD.
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
C     Dagny Taggart  (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 5-APR-1989 (DMT)
C
C-&
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'BBCLR_1' )
      END IF
 
C
C     Wipe out all three symbol tables.
C
      CALL SSIZEC ( MAXI,  INTAB )
      CALL SSIZEI ( MAXI,  IPTAB )
      CALL SSIZEI ( MAXIV, IVTAB )
 
      CALL SSIZEC ( MAXD,  DNTAB )
      CALL SSIZEI ( MAXD,  DPTAB )
      CALL SSIZED ( MAXDV, DVTAB )
 
      CALL SSIZEC ( MAXC,  CNTAB )
      CALL SSIZEI ( MAXC,  CPTAB )
      CALL SSIZEC ( MAXCV, CVTAB )
 
C
C     Re-initialize the string buffer.
C
      CALL SBINIT_1 ( MAXS,
     .              MAXS * 4 + 4,
     .              MAXCHR / 100,
     .              NBUF,
     .              PBUF,
     .              VBUF          )
 
      CALL CHKOUT ( 'BBCLR_1' )
      RETURN
 
 
      END

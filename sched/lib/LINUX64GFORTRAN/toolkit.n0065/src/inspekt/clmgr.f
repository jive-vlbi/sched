      SUBROUTINE CLMGR ( NAME,  ID,    NUM,   HANDLE,
     .                   NULL,  INDX,  CTYPE, CSIZE,  ATTR,
     .                   CLEN,  IATTR, CATTR, FOUND,  COLS, QUERY,
     .                   ERROR, WDTH,  CORCTN,  WIDEST  )
C
C     This routine manages the attributes and evaluation of columns.
C     It acts as an umbrella over a collection of entry points that
C     allow programs to create, remove, modify attributes of and
C     fetch the value of a column associated with a particular
C     index associated with a query.  Some assumptions are implicit
C     in the design of this routine.  For example it is assumed
C     that only legitimate values are PUT to the "setting"
C     entry points of this routine.
C
C-    Version 7.1  10-APR-2000
C
C       Declared SCAN to be external
C
C-    Version 7.0  16-NOV-1995
C
C       We replaced the definition of IMIN to point to the
C       value EQIMIN that is included via the file ekquery.inc
C
C-    Version 6.0    16-OCT-1995
C
C     When a new column is supplied (via CLNEW) but the column already
C     exists, we now change the width only if the column is of type
C     character.
C
C-    Version 5.0    7-SEP-1995
C
C     A 'default' Integer and D.P. format were added to the formats
C
C     The behaviour is the same as the behaviour for TIME columns.
C
C-    Version 4.0   21-AUG-1995  Editorial Comment.
C
C     The concept that setting time format only affected new columns
C     as they were loaded proved to be too difficult for the poor
C     computer challenged user.
C
C     Now, when a new column is loaded and is of type TIME, the value
C     of the format set is DEFAULT.  The width is set to -1.  The actual
C     width is determined by calling NSPCHT to construct the width
C     and truncates this to 40 at max.
C
C     If specific calls are made to set the format to something other
C     than DEFAULT, the width is set at that time to a positive
C     value and that width shall be used until the user sets the
C     width to some other value or changes the format again.
C
C     If the value of FMTS(ID) = 'DEFAULT' for a time column, the
C     program looks up the actual format when it is called for
C     the called for locations are CLGAC and CLPVAL.  Also when
C     width is requested in CLGAC, CLGAI and CLPVAL.
C
C
C-    Version 3.0   3-Aug-1995  The interface to the EK library
C     changed and NJB modified this routine to reflect that
C     change.
C
C-    Version 2.0.  Fixed the problem with an unitialized variable
C     D.  D is no longer used.
C
      IMPLICIT NONE
 
 
C
C     Include files for parameters used in the declarations below:
C
      INCLUDE 'ekquery.inc'
      INCLUDE 'ekqlimit.inc'
 
 
 
      CHARACTER*(*)         NAME
      INTEGER               ID
      INTEGER               NUM
      INTEGER               HANDLE
      LOGICAL               NULL
      LOGICAL               INDX
      INTEGER               CTYPE
      INTEGER               CLEN
      INTEGER               CSIZE
      INTEGER               IATTR  ( * )
      CHARACTER*(*)         CATTR
      CHARACTER*(*)         ATTR
      LOGICAL               FOUND
      CHARACTER*(*)         COLS
      CHARACTER*(*)         QUERY
      CHARACTER*(*)         ERROR
      INTEGER               WDTH
      CHARACTER*(*)         CORCTN
      INTEGER               WIDEST
 
 
C
C     Spicelib functions
C
      INTEGER               BRCKTI
      INTEGER               CARDI
      INTEGER               ISRCHI
      INTEGER               RTRIM
      EXTERNAL              SCAN
 
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Routine name
C
      CHARACTER*(6)         RNAME
 
C
C     Parameters
C
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
C
C     Parameters that are used to define the sizes of the attributes
C     and number of columns.
C
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE  = 64 )
 
      INTEGER               SWDSIZ
      PARAMETER           ( SWDSIZ = 8 )
 
      INTEGER               MAXNAM
      PARAMETER           ( MAXNAM = 400  )
 
      INTEGER               MAXHDL
      PARAMETER           ( MAXHDL = 20*MAXNAM )
 
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 128 )
C
C     Attributes that are associated with each column
C
 
 
      CHARACTER*(LNSIZE)    FMTS  ( MAXNAM )
 
      CHARACTER*(SWDSIZ)    JUSTS ( MAXNAM )
 
      CHARACTER*(WDSIZE)    ALIASS( MAXNAM )
      CHARACTER*(WDSIZE)    NAMES ( MAXNAM )
      CHARACTER*(WDSIZE)    TABLES( MAXNAM )
      CHARACTER*(WDSIZE)    CNAMES( MAXNAM )
 
      INTEGER               NACTIV
      INTEGER               LENTHS( MAXNAM )
      INTEGER               SIZES ( MAXNAM )
      INTEGER               TYPES ( MAXNAM )
      INTEGER               WIDTHS( MAXNAM )
 
      LOGICAL               ACTIVE( MAXNAM )
      LOGICAL               AVAIL ( MAXNAM )
      LOGICAL               INDEXD( MAXNAM )
      LOGICAL               NULLOK( MAXNAM )
 
      CHARACTER*(WDSIZE)    NAMS  (LBCELL : MAXNAM  )
      INTEGER               PTRS  (LBCELL : MAXNAM  )
      INTEGER               HNDLS (LBCELL : MAXHDL  )
 
C
C     The variable AMOUNT contains the number of components associated
C     with a column at the "current" row of a query.
C
      INTEGER               AMOUNT( MAXNAM )
C
C     The variable AQUERY contains a logical that contains the truth
C     value of the assertion "there is an active query".
C
      LOGICAL               AQUERY
C
C     We shall need local storage to hold the fetch from a string
C     data type.  The maximum such string that can be in an
C     E-kernel is 1024 characters long.
C
      INTEGER               MAXLEN
      PARAMETER           ( MAXLEN = 1024 )
 
      CHARACTER*(MAXLEN)    MYSTR
      CHARACTER*(MAXLEN)    ERRMSG
      INTEGER               ERRPTR
C
C     These are the names of attributes that can be set or
C     fetched.
C
      CHARACTER*(*)         FMT
      PARAMETER           ( FMT    = 'FORMAT' )
 
      CHARACTER*(*)         TYPE
      PARAMETER           ( TYPE   = 'TYPE'   )
 
      CHARACTER*(*)         CID
      PARAMETER           ( CID    = 'ID'     )
 
      CHARACTER*(*)         ALIAS
      PARAMETER           ( ALIAS  = 'ALIAS'  )
 
      CHARACTER*(*)         JUST
      PARAMETER           ( JUST   = 'JUSTIFICATION' )
 
      CHARACTER*(*)         WIDTH
      PARAMETER           ( WIDTH  = 'WIDTH'    )
 
      CHARACTER*(*)         INDXD
      PARAMETER           ( INDXD  = 'INDEXED'  )
 
      CHARACTER*(*)         NOK
      PARAMETER           ( NOK    = 'NULLOK'   )
 
      CHARACTER*(*)         SIZE
      PARAMETER           ( SIZE   = 'SIZE'     )
 
      CHARACTER*(*)         HNDL
      PARAMETER           ( HNDL   = 'HANDLES'  )
 
      CHARACTER*(*)         NAMVAL
      PARAMETER           ( NAMVAL = 'NAME' )
 
      CHARACTER*(*)         TABVAL
      PARAMETER           ( TABVAL = 'TABLE' )
 
      CHARACTER*(*)         CNAME
      PARAMETER           ( CNAME = 'COLNAM' )
C
C     If a column has a variable number of components the size
C     stored for it will be -1.  We use a parameter to keep
C     track of this.
C
 
      INTEGER               VAR
      PARAMETER           ( VAR    = -1 )
C
C     Impossible handles and segment numbers are 0.
C
      INTEGER               BOGUS
      PARAMETER           ( BOGUS  = 0 )
 
C
C     The following are the allowed types for the columns
C
      INTEGER               CH
      PARAMETER           ( CH   = 1 )
 
      INTEGER               DP
      PARAMETER           ( DP   = 2 )
 
      INTEGER               INT
      PARAMETER           ( INT  = 3 )
 
      INTEGER               TIME
      PARAMETER           ( TIME = 4 )
C
C     Print values for null and unavailable columns
C
      CHARACTER*(*)         NOCOLM
      PARAMETER           ( NOCOLM = '<absent>' )
 
      CHARACTER*(*)         NULCOL
      PARAMETER           ( NULCOL = '<null>' )
 
C
C     The next set of parameters are the default formats and widths
C     for columns.
C
      CHARACTER*(*)         IFMT
      PARAMETER           ( IFMT   = 'IIIIIIIIIIIII' )
 
      CHARACTER*(*)         DFMT
      PARAMETER           ( DFMT   = 'XXXXXXXX.XXX' )
 
      CHARACTER*(*)         CFMT
      PARAMETER           ( CFMT   = ' ' )
 
      INTEGER               IWDTH
      PARAMETER           ( IWDTH  = 12 )
 
      INTEGER               TWDTH
      PARAMETER           ( TWDTH  = 20 )
 
      INTEGER               DWDTH
      PARAMETER           ( DWDTH  = 22 )
C
C     Next we have the print values that shall be used for the
C     various types of columns.
C
      CHARACTER*(*)         PVALCH
      PARAMETER           ( PVALCH = 'CHARACTER*(#)' )
 
      CHARACTER*(*)         PVALI
      PARAMETER           ( PVALI  = 'INTEGER' )
 
      CHARACTER*(*)         PVALDP
      PARAMETER           ( PVALDP = 'DOUBLE PRECISION' )
 
      CHARACTER*(*)         PVALT
      PARAMETER           ( PVALT  = 'TIME' )
 
      CHARACTER*(WDSIZE)    PVALUE ( CH:TIME )
 
C
C     The variables below are shared between the entry points
C     CLSCOP and CLPVAL to keep track of the number of rows that
C     have been examined as the result of a query.
C
C     CURRNT is the number of the matching row that is currently
C            being examined
C
C     LASTRD is the row number where the EK-reader begins pointing as
C            the result of a processed valid query (this could be either
C            zero or one depending upon how the routine EKFIND is
C            implemented.  Right now its zero,  the EK-reader
C            isn't pointing at any matching row as the result of
C            processing a valid query.
C
C
C     SCOPE  is the total number of rows that fit a query condition.
C
      INTEGER               SCOPE
      INTEGER               CURRNT
 
      INTEGER               LASTRD
      PARAMETER           ( LASTRD = 0 )
C
C     The following variable is used to look up the default time
C     format stored in the bulletin board.
C
 
      CHARACTER*(LNSIZE)    TFMT
 
C
C     The variables below allow CLSPEL to diagnose correct spellings
C     for mispelled column names.
C
      CHARACTER*(WDSIZE)    KNOWN (LBCELL : MAXNAM )
 
      INTEGER               CUTOFF
      INTEGER               BEST    ( LBCELL : 10 )
      INTEGER               SCORES  ( LBCELL : 10 )
 
      INTEGER               HIGH
      INTEGER               HIGHST
C
C     Declarations for interfacing with the query parser and
C     EKSRCH   and EKG* are given below.
C
C
C     This parameter is the minimum size of the integer portion of
C     an encoded query:
C
      INTEGER               IMIN
      PARAMETER           ( IMIN = EQIMIN )
 
C
C     The following cell, string and array make comprise the data
C     structure used to represent encoded queries:
C
      INTEGER               EQRYI  ( LBCELL : IMIN )
      CHARACTER*(MAXCLN)    EQRYC
      DOUBLE PRECISION      EQRYD  ( MAXQNM )
 
C
C     The items below are used for taking apart the list of columns
C     supplied to CLSCOP.
C
      INTEGER               MRKSIZ
      PARAMETER           ( MRKSIZ = 4 )
 
      INTEGER               MAXMRK
      PARAMETER           ( MAXMRK = 1 )
 
      INTEGER               FCHAR
      PARAMETER           ( FCHAR =  44  )
 
      INTEGER               LCHAR
      PARAMETER           ( LCHAR =  44  )
 
      INTEGER               TCHAR
      PARAMETER           ( TCHAR = LCHAR - FCHAR + 5 )
 
      INTEGER               NKEEP
      PARAMETER           ( NKEEP = 1 )
 
      CHARACTER*(MRKSIZ)    MARKS  ( MAXMRK )
      INTEGER               NMARKS
      INTEGER               MRKLEN ( MAXMRK )
      INTEGER               PNTERS ( TCHAR  )
 
      INTEGER               QBEG   ( MAXNAM )
      INTEGER               QEND   ( MAXNAM )
      INTEGER               IDENT  ( MAXNAM )
      INTEGER               NTOKNS
 
      INTEGER               KEEP   ( NKEEP )
 
      CHARACTER*(LNSIZE)    MYCOL
      CHARACTER*(WDSIZE)    MYALIS
      CHARACTER*(WDSIZE)    QCOL
C
C     Storage for query relative items.
C
 
      INTEGER               MAXCOL
      PARAMETER           ( MAXCOL = MAXNAM )
 
 
      CHARACTER*(WDSIZE)    LNAME
      CHARACTER*(WDSIZE)    QALIAS ( MAXCOL )
      CHARACTER*(WDSIZE)    QCOLMN ( MAXCOL )
      CHARACTER*(WDSIZE)    TABLE
      CHARACTER*(WDSIZE)    TALIAS
 
      INTEGER               COLIDX
      INTEGER               NQIDS
      INTEGER               QCIDS  ( MAXCOL )
      INTEGER               QTINDX ( MAXCOL )
      INTEGER               TABIDX
 
 
C
C     Initialization control variables.
C
      LOGICAL               FIRST
      LOGICAL               UNPREP
 
C
C     Local Variables
C
      CHARACTER*(WDSIZE)    ITEM
 
      DOUBLE PRECISION      X
C
C     qqqqqqqq
C
      INTEGER               COUNT
      INTEGER               HAN
      INTEGER               B
      INTEGER               E
      INTEGER               I
      INTEGER               J
      INTEGER               L
      INTEGER               MYID
      INTEGER               N
      INTEGER               TE
      INTEGER               NB
C     INTEGER               D
 
      INTEGER               START
      INTEGER               W
 
      LOGICAL               BADQRY
      LOGICAL               GOTIT
      LOGICAL               GOTONE
      LOGICAL               GOTFRE
      LOGICAL               NULVAL
      LOGICAL               OK
 
      SAVE
C
C     Initial Values
C
 
      DATA AQUERY       / .FALSE. /
      DATA FIRST        / .TRUE.  /
      DATA UNPREP       / .TRUE.  /
      DATA PVALUE(CH)   /  PVALCH /
      DATA PVALUE(INT)  /  PVALI  /
      DATA PVALUE(DP)   /  PVALDP /
      DATA PVALUE(TIME) /  PVALT  /
 
      RETURN
      
C$Procedure CLGAC ( Column, Get attribute character. )
 
 
      ENTRY CLGAC ( ID, ATTR, CATTR )
 
C$ Abstract
C
C     Fetch the character attributes of the column with the specified
C     ID code.
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
C-&
 
      IF ( RETURN()  ) THEN
         RETURN
      END IF
 
      RNAME = 'CLGAC'
 
      CALL CHKIN ( RNAME )
 
      IF ( FIRST ) THEN
 
         FIRST = .FALSE.
 
         CALL SSIZEC ( MAXNAM, NAMS  )
         CALL SSIZEI ( MAXNAM, PTRS  )
         CALL SSIZEI ( MAXHDL, HNDLS )
 
         NACTIV = 0
 
         DO   I = 1, MAXNAM
            ACTIVE(I) = .FALSE.
         END DO
 
      END IF
 
 
      IF (       ( ID .LT. 1      )
     .     .OR.  ( ID .GT. MAXNAM ) ) THEN
 
         CALL SETMSG ( 'The ID, #, used to specified a column '
     .   //            'is out of bounds. To be considered as '
     .   //            'an active column, ID must be between 1 '
     .   //            'and # inclusive.'       )
         CALL ERRINT( '#', ID                   )
         CALL ERRINT( '#', MAXNAM               )
         CALL SIGERR( 'INSPEKT(BADCOLUMNID)'    )
         CALL CHKOUT ( RNAME )
         RETURN
 
      END IF
 
      IF ( .NOT. ACTIVE(ID) ) THEN
 
         CALL SETMSG ( 'The column specified by the ID, #, '
     .   //            'is not active now.'   )
         CALL ERRINT ( '#', ID                )
         CALL SIGERR( 'INSPEKT(INACTIVEID)'   )
         CALL CHKOUT ( RNAME                  )
         RETURN
 
      END IF
 
C
C     See which attribute is requested and load the value into
C     the output string.
C
      IF      ( ATTR .EQ. FMT   ) THEN
 
         IF ( FMTS(ID) .EQ. 'DEFAULT' ) THEN
 
            IF   ( TYPES(ID) .EQ. TIME ) THEN
               ITEM = 'TIMEFMT'
            ELSE IF ( TYPES(ID) .EQ. INT ) THEN
               ITEM = 'INTFMT'
            ELSE IF ( TYPES(ID) .EQ. DP  ) THEN
               ITEM = 'DPFMT'
            ELSE IF ( TYPES(ID) .EQ. CH )  THEN
               ITEM = 'CHFMT'
            END IF
 
            CALL BBGETC_1 ( 'COPY', ITEM, N, CATTR )
 
         ELSE
            CATTR = FMTS(ID)
         END IF
 
      ELSE IF ( ATTR .EQ. TYPE  ) THEN
 
         IF ( LENTHS(ID) .GT. 0 ) THEN
            CALL REPMI ( PVALUE( TYPES(ID) ), '#', LENTHS(ID), CATTR )
         ELSE
            CALL REPMC ( PVALUE( TYPES(ID) ), '#', '*',        CATTR )
         END IF
 
      ELSE IF ( ATTR .EQ. CID   ) THEN
 
         CALL INTSTR( ID, CATTR )
 
      ELSE IF ( ATTR .EQ. ALIAS ) THEN
 
         CATTR = ALIASS ( ID )
 
      ELSE IF ( ATTR .EQ. TABVAL ) THEN
 
         CATTR = TABLES ( ID )
 
      ELSE IF ( ATTR .EQ. JUST  ) THEN
 
         CATTR = JUSTS ( ID )
 
      ELSE IF ( ATTR .EQ. CNAME ) THEN
 
         CATTR = CNAMES( ID )
 
      ELSE IF ( ATTR .EQ. WIDTH ) THEN
 
         IF (       TYPES(ID)  .EQ.  TIME
     .        .AND. FMTS (ID)  .EQ. 'DEFAULT'
     .        .AND. WIDTHS(ID) .LT.  0        ) THEN
 
            W = 32
            CALL BBGETC_1 ( 'COPY', 'TIMEFMT', N, TFMT )
            CALL NSPCHT   ( TFMT,    W                 )
            CALL INTSTR   ( W,       CATTR             )
 
         ELSE IF ( FMTS(ID) .EQ. 'DEFAULT' ) THEN
 
            IF ( TYPES(ID) .EQ. INT ) THEN
               ITEM = 'INTFMT'
            ELSE IF ( TYPES(ID) .EQ. DP  ) THEN
               ITEM = 'DPFMT'
            ELSE IF ( TYPES(ID) .EQ. CH )  THEN
               ITEM = 'CHFMT'
            END IF
 
            CALL BBGETC_1 ( 'COPY', ITEM, N, TFMT )
 
            W = MAX( 8, RTRIM(TFMT) )
 
            CALL INTSTR ( W, CATTR  )
 
         ELSE
            CALL INTSTR( WIDTHS(ID), CATTR )
         END IF
 
      ELSE IF ( ATTR .EQ. INDXD ) THEN
 
         IF ( INDEXD (ID) ) THEN
            CATTR = 'YES'
         ELSE
            CATTR = 'NO'
         END IF
 
      ELSE IF ( ATTR .EQ. NOK   ) THEN
 
         IF ( NULLOK ( ID )  ) THEN
            CATTR = 'YES'
         ELSE
            CATTR = 'NO'
         END IF
 
      ELSE IF ( ATTR .EQ. SIZE  ) THEN
 
         IF ( SIZES(ID) .EQ. VAR ) THEN
            CATTR = 'VARIABLE'
         ELSE
            CALL INTSTR ( SIZES(ID), CATTR )
         END IF
 
      ELSE IF ( ATTR .EQ. HNDL  ) THEN
 
         I     = 1
         CATTR = ' '
C
C        Fetch the first handle associated with this ID.
C
         CALL SYPTRI ( NAMES(ID), NAMS, PTRS,  HNDLS,
     .                 START,     N,    GOTONE         )
 
 
         DO I = START, START + N - 1
C
C           Now as long as we get another handle, append it
C           as a string to the list of handles associated with
C           this ID.
C
            J = HNDLS(I)
 
            CALL REPMC  ( CATTR, '#', ',',  CATTR )
            CALL SUFFIX (        '#',  1,   CATTR )
            CALL REPMI  ( CATTR, '#',  J,   CATTR )
            CALL SUFFIX (        '#',  0,   CATTR )
 
         END DO
 
         CALL REPMC ( CATTR, '#', ' ', CATTR )
 
      ELSE IF ( ATTR .EQ. NAMVAL) THEN
 
         CATTR = NAMES ( ID )
 
      ELSE
 
         CALL SETMSG ( 'The attribute requested, #, is not a '
     .   //            'recognized column attribute. '  )
         CALL ERRCH  ( '#', ATTR                        )
         CALL SIGERR ( 'INSPEKT(UNKNOWNCOLATTRIBUTE)'   )
         CALL CHKOUT ( RNAME )
         RETURN
 
      END IF
 
      CALL CHKOUT ( RNAME )
      RETURN
 
 
 
 
 
C$Procedure CLGAI ( Column, get attribute integer )
 
 
      ENTRY CLGAI ( ID, ATTR, NUM, IATTR )
 
C$ Abstract
C
C     This entry point fetches an integer attribute associated with
C     a column.
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
C-&
 
      IF ( RETURN()  ) THEN
         RETURN
      END IF
 
      RNAME = 'CLGAI'
 
      CALL CHKIN ( RNAME )
 
      IF ( FIRST ) THEN
 
         FIRST = .FALSE.
 
         CALL SSIZEC ( MAXNAM, NAMS  )
         CALL SSIZEI ( MAXNAM, PTRS  )
         CALL SSIZEI ( MAXHDL, HNDLS )
 
         NACTIV = 0
 
         DO   I = 1, MAXNAM
            ACTIVE(I) = .FALSE.
         END DO
 
      END IF
 
 
      IF (       ( ID .LT. 1      )
     .     .OR.  ( ID .GT. MAXNAM ) ) THEN
 
         CALL SETMSG ( 'The ID, #, used to specify a column '
     .   //            'is out of bounds. To be considered as '
     .   //            'an active column, ID must be between 1 '
     .   //            'and # inclusive.'       )
         CALL ERRINT( '#', ID                   )
         CALL ERRINT( '#', MAXNAM               )
         CALL SIGERR( 'INSPEKT(BADCOLUMNID)'    )
         CALL CHKOUT ( RNAME )
         RETURN
 
      END IF
 
      IF ( .NOT. ACTIVE(ID) ) THEN
 
         CALL SETMSG ( 'The column specified by the ID, #, '
     .   //            'is not active now.'   )
         CALL ERRINT ( '#', ID                )
         CALL SIGERR( 'INSPEKT(INACTIVEID)'   )
         CALL CHKOUT ( RNAME                  )
         RETURN
 
      END IF
 
C
C     See which attribute is requested and load the value into
C     the output string.  Normally, the number of attributes
C     returned will be one.  If this isn't true. We adjust the count
C     when we find out that it is not.
C
      NUM = 1
 
      IF ( ATTR .EQ. CID   ) THEN
 
         IATTR(1) = ID
 
      ELSE IF ( ATTR .EQ. WIDTH ) THEN
 
         IF (       TYPES(ID)  .EQ.  TIME
     .        .AND. FMTS (ID)  .EQ. 'DEFAULT'
     .        .AND. WIDTHS(ID) .LT.  0        ) THEN
            W = 32
            ITEM = 'TIMEFMT'
            CALL BBGETC_1 ( 'COPY', ITEM, N, TFMT )
            CALL NSPCHT   ( TFMT,    W                 )
            IATTR(1) = W
 
         ELSE IF ( FMTS(ID) .EQ. 'DEFAULT' ) THEN
 
            IF ( TYPES(ID) .EQ. INT ) THEN
               ITEM = 'INTFMT'
            ELSE IF ( TYPES(ID) .EQ. DP  ) THEN
               ITEM = 'DPFMT'
            ELSE IF ( TYPES(ID) .EQ. CH )  THEN
               ITEM = 'CHFMT'
            END IF
 
            CALL BBGETC_1 ( 'COPY', ITEM, N, TFMT )
 
            W        = MAX( 8, RTRIM(TFMT) )
            IATTR(1) =  W
 
         ELSE
            IATTR(1) =  WIDTHS(ID)
         END IF
 
      ELSE IF ( ATTR .EQ. SIZE  ) THEN
 
         IATTR(1) = SIZES(ID)
 
      ELSE IF ( ATTR .EQ. HNDL  ) THEN
 
         CALL SYGETI ( NAMES(ID),
     .                 NAMS,      PTRS,  HNDLS,
     .                 NUM,       IATTR, GOTONE )
 
      ELSE IF ( ATTR .EQ. TYPE ) THEN
 
         IATTR(1) = TYPES(ID)
 
      ELSE
 
         CALL SETMSG ( 'The attribute requested, #, is not a '
     .   //            'recognized integer column attribute. '  )
         CALL ERRCH  ( '#', ATTR                        )
         CALL SIGERR ( 'INSPEKT(UNKNOWNCOLATTRIBUTE)'   )
         CALL CHKOUT ( RNAME )
         RETURN
 
      END IF
 
      CALL CHKOUT ( RNAME )
      RETURN
 
C$Procedure      CLNEW ( Allocate a new column )
 
 
      ENTRY CLNEW ( NAME,   HANDLE,  CTYPE,  CLEN,
     .              WIDEST, CSIZE,  INDX,    NULL,   ID )
 
C$ Abstract
C
C     Given the basic column attributes, this routine enters this
C     column in the column database and returns the ID assigned to
C     this column.
C
      IF ( RETURN()  ) THEN
         RETURN
      END IF
 
      RNAME = 'CLNEW'
 
      CALL CHKIN ( RNAME )
 
      IF ( FIRST ) THEN
 
         FIRST = .FALSE.
 
         CALL SSIZEC ( MAXNAM, NAMS  )
         CALL SSIZEI ( MAXNAM, PTRS  )
         CALL SSIZEI ( MAXHDL, HNDLS )
 
         NACTIV = 0
 
         DO   I = 1, MAXNAM
            ACTIVE(I) = .FALSE.
         END DO
 
      END IF
C
C     Loading a new column deactivates any existing query.
C
      AQUERY = .FALSE.
      NQIDS  =  0
 
 
C
C     First see if this column already exists.  If it does we
C     shall simply update it's values (with a bit of checking
C     thrown in for good measure).
C
      GOTONE = .FALSE.
      GOTFRE = .FALSE.
      I      =  1
 
      DO WHILE (            .NOT. GOTONE
     .          .AND. I     .LE.  MAXNAM  )
 
         IF ( ACTIVE(I) ) THEN
 
            IF ( NAME .EQ. NAMES(I) ) THEN
               GOTONE = .TRUE.
               ID     =  I
            END IF
 
         ELSE IF ( .NOT. GOTFRE ) THEN
 
            ID     =  I
            GOTFRE = .TRUE.
 
         END IF
 
         I = I + 1
 
      END DO
 
C
C     OK. At this point we've either located the input name
C     or a slot to put it (if one exists).  Now simply update
C     the values (or set them for the first time) for this
C     column.
C
      IF      ( GOTONE ) THEN
C
C        This guy is a repeat, we only need to update values.
C
         TYPES (ID) = CTYPE
         SIZES (ID) = CSIZE
         INDEXD(ID) = INDX
         NULLOK(ID) = NULL
         AMOUNT(ID) = CSIZE
         LENTHS(ID) = CLEN
C
C        Note we only want to update the width in the case of
C        character columns.  For everything else there is nothing
C        to do.
C
         IF ( CTYPE .EQ. CH ) THEN
            WIDTHS(ID) = BRCKTI( MAX(WIDTHS(ID),WIDEST), 8, 40 )
         END IF
 
C
C        Set the collection of handle associated with this
C        column name (but only if this handle is not already
C        attached to this name).
C
         CALL SYPTRI ( NAME, NAMS, PTRS, HNDLS, START, N, OK )
 
         IF ( ISRCHI ( HANDLE, N, HNDLS(START) ) .EQ. 0 ) THEN
            CALL SYPSHI ( NAME, HANDLE, NAMS, PTRS, HNDLS )
         END IF
 
 
      ELSE IF ( GOTFRE ) THEN
C
C        This is a new column.  Activivate this id and fill in
C        all of the appropriate values.
C
         NACTIV     =  NACTIV + 1
         ACTIVE(ID) = .TRUE.
         TYPES (ID) =  CTYPE
         SIZES (ID) =  CSIZE
         INDEXD(ID) =  INDX
         NULLOK(ID) =  NULL
         AMOUNT(ID) =  CSIZE
         NAMES (ID) =  NAME
         LENTHS(ID) =  CLEN
C
C        We need to parse the table name from the from the column name.
C
         TE         = INDEX ( NAME, '.' ) - 1
         NB         = TE + 2
 
         IF ( TE .GT. 1 ) THEN
            TABLES(ID) = NAME(1:TE)
         ELSE
            TABLES(ID) = ' '
         END IF
 
         CALL SYPUTI ( NAME, HANDLE, 1, NAMS, PTRS, HNDLS )
C
C        Finally, set the default values for this column
C
         IF ( NB .LT. LEN(NAME) ) THEN
            ALIASS(ID) =  NAME(NB:)
            CNAMES(ID) =  NAME(NB:)
         ELSE
            ALIASS(ID) =  NAME
            CNAMES(ID) =  NAME
         END IF
 
         IF      ( CTYPE .EQ. INT )  THEN
 
            FMTS  (ID) =  'DEFAULT'
            WIDTHS(ID) =  MAX( 8, RTRIM(FMTS(ID)) )
            JUSTS (ID) = 'RIGHT'
 
         ELSE IF ( CTYPE .EQ. TIME ) THEN
 
 
            FMTS  (ID) = 'DEFAULT'
            WIDTHS(ID) =  -1
            JUSTS (ID) = 'LEFT'
 
         ELSE IF ( CTYPE .EQ. DP  ) THEN
 
            FMTS  (ID) =  'DEFAULT'
            WIDTHS(ID) =  DWDTH
            JUSTS (ID) = 'RIGHT'
 
         ELSE IF ( CTYPE .EQ. CH  ) THEN
C
C           This must be a character type of column.
            WIDTHS(ID) =  BRCKTI( WIDEST, 8, 40 )
            FMTS  (ID) = ' '
            JUSTS (ID) = 'LEFT'
 
         ELSE
 
            CALL SETMSG ( 'The column type supplied is not one of '
     .      //            'the recognized column types. The value '
     .      //            'supplied was #. '       )
            CALL ERRINT ( '#', CTYPE               )
            CALL SIGERR ( 'INSPEKT(BADCOLUMNTYPE)' )
            CALL CHKOUT (  RNAME                   )
            RETURN
 
         END IF
 
 
      ELSE
C
C        The tables must be full.  There's nothing we can do but
C        signal an error.
C
         CALL SETMSG ( 'There is no room left for creating another '
     .   //            'column in the column manager.  To remedy '
     .   //            'this situation, you might try unloading '
     .   //            'some of the loaded E-kernels.  If this is '
     .   //            'not a viable option, the parameter, MAXNAM '
     .   //            ', (it controls the maximum number of'
     .   //            'columns that can be maintained by the '
     .   //            'column manager) will need to be increased. '
     .   //            'Unfortunately, this means editing source '
     .   //            'code and rebuilding INSPEKT.  We at NAIF '
     .   //            'apologize for this inconvenience and ask '
     .   //            'that you notify us of this problem. '       )
         CALL SIGERR ( 'INSPEKT(COLUMNBUFFERFULL)'                  )
         CALL CHKOUT ( RNAME )
         RETURN
 
      END IF
 
      CALL CHKOUT ( RNAME )
      RETURN
 
 
 
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
C$Procedure CLNID ( Column, Get N'th ID )
 
      ENTRY CLNID ( NUM, ID, FOUND )
 
C$ Abstract
C
C     This routine returns the ID of the NUM'th column stored in the
C     column manager.
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
C-&
 
      IF ( RETURN()  ) THEN
         RETURN
      END IF
 
      RNAME = 'CLNID'
 
      CALL CHKIN ( RNAME )
 
      IF ( FIRST ) THEN
 
         FIRST = .FALSE.
 
         CALL SSIZEC ( MAXNAM, NAMS  )
         CALL SSIZEI ( MAXNAM, PTRS  )
         CALL SSIZEI ( MAXHDL, HNDLS )
 
         NACTIV = 0
 
         DO   I = 1, MAXNAM
            ACTIVE(I) = .FALSE.
         END DO
 
      END IF
 
 
      FOUND = .FALSE.
C
C     Nothing very smart here, simply locate the NUM'th
C     array element of ACTIVE that has a value of .TRUE.
C
      COUNT = 0
 
      DO  I = 1, MAXNAM
 
         IF ( ACTIVE(I) ) THEN
            COUNT = COUNT + 1
            IF ( COUNT .EQ. NUM ) THEN
               ID    =  I
               FOUND = .TRUE.
            END IF
         END IF
 
      END DO
 
      CALL CHKOUT ( RNAME )
      RETURN
 
C$Procedure CLNUM ( Column, number active )
 
 
      ENTRY CLNUM ( NUM )
 
C$ Abstract
C
C     Return the number of active columns
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
C-&
 
      IF ( RETURN()  ) THEN
         RETURN
      END IF
 
      RNAME = 'CLNUM'
 
      CALL CHKIN ( RNAME )
 
      IF ( FIRST ) THEN
 
         FIRST = .FALSE.
 
         CALL SSIZEC ( MAXNAM, NAMS  )
         CALL SSIZEI ( MAXNAM, PTRS  )
         CALL SSIZEI ( MAXHDL, HNDLS )
 
         NACTIV = 0
 
         DO   I = 1, MAXNAM
            ACTIVE(I) = .FALSE.
         END DO
 
      END IF
 
 
      NUM = NACTIV
 
      CALL CHKOUT ( RNAME )
      RETURN
 
 
 
C$Procedure CLN2ID ( Column, translate name to ID )
 
      ENTRY CLN2ID ( NAME, ID, FOUND )
 
C$ Abstract
C
C     Locate the ID associated with this name (provided there is
C     such an ID).
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
C-&
 
      IF ( RETURN()  ) THEN
         RETURN
      END IF
 
      RNAME = 'CLN2ID'
 
      CALL CHKIN ( RNAME )
 
      IF ( FIRST ) THEN
 
         FIRST = .FALSE.
 
         CALL SSIZEC ( MAXNAM, NAMS  )
         CALL SSIZEI ( MAXNAM, PTRS  )
         CALL SSIZEI ( MAXHDL, HNDLS )
 
         NACTIV = 0
 
         DO   I = 1, MAXNAM
            ACTIVE(I) = .FALSE.
         END DO
 
      END IF
C
C     We don't do anything very sophisticated here. We simply
C     look for the name of interest among the those names
C     that are active.
C
      DO I = 1, MAXNAM
 
         IF ( ACTIVE(I) .AND. NAME .EQ. NAMES(I) ) THEN
            ID    =  I
            FOUND = .TRUE.
            CALL CHKOUT ( RNAME )
            RETURN
         END IF
 
 
 
      END DO
C
C     If we get to this point, we didn't find an ID for this
C     column name.  Set ID to -1.  Any attempts to use this
C     ID for something will then result in an error.
C
      FOUND = .FALSE.
      ID    = -1
 
      CALL CHKOUT ( RNAME )
      RETURN
 
 
 
 
C$Procedure CLPAC ( Column, put attribute---character)
 
 
      ENTRY CLPAC ( ID, ATTR, CATTR )
 
C$ Abstract
C
C     Set one of the settable character column attributes (there are
C     only three right now---alias, format and justification).
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
C-&
 
      IF ( RETURN()  ) THEN
         RETURN
      END IF
 
      RNAME = 'CLPAC'
 
      CALL CHKIN ( RNAME )
 
      IF ( FIRST ) THEN
 
         FIRST = .FALSE.
 
         CALL SSIZEC ( MAXNAM, NAMS  )
         CALL SSIZEI ( MAXNAM, PTRS  )
         CALL SSIZEI ( MAXHDL, HNDLS )
 
         NACTIV = 0
 
         DO   I = 1, MAXNAM
            ACTIVE(I) = .FALSE.
         END DO
 
      END IF
C
C     Check to make sure that the ID is a recognized one.
C
      IF (       ( ID .LT. 1      )
     .     .OR.  ( ID .GT. MAXNAM ) ) THEN
 
         CALL SETMSG ( 'The ID, #, used to specified a column '
     .   //            'is out of bounds. To be considered as '
     .   //            'an active column, ID must be between 1 '
     .   //            'and # inclusive.'       )
         CALL ERRINT( '#', ID                   )
         CALL ERRINT( '#', MAXNAM               )
         CALL SIGERR( 'INSPEKT(BADCOLUMNID)'    )
         CALL CHKOUT ( RNAME )
         RETURN
 
      END IF
 
      IF ( .NOT. ACTIVE(ID) ) THEN
 
         CALL SETMSG ( 'The column specified by the ID, #, '
     .   //            'is not active now.'   )
         CALL ERRINT ( '#', ID                )
         CALL SIGERR( 'INSPEKT(INACTIVEID)'   )
         CALL CHKOUT ( RNAME                  )
         RETURN
 
      END IF
 
 
      IF      ( ATTR .EQ. FMT   ) THEN
 
C
C        We shall adjust the widths of this column if the format
C        type is TIME or INT or DP.
C
         IF ( TYPES(ID) .EQ. TIME ) THEN
C
C           Check to make sure that the format is do-able.
C
            CALL LJUST ( CATTR, TFMT )
            CALL UCASE ( TFMT,  TFMT )
 
            IF ( TFMT .EQ. 'DEFAULT' ) THEN
 
               FMTS  (ID) = 'DEFAULT'
               WIDTHS(ID) = -1
 
            ELSE
               CALL NSPCHT ( CATTR, W )
 
               IF ( FAILED() ) THEN
                  CALL CHKOUT ( RNAME )
                  RETURN
               END IF
 
               FMTS  (ID) =  CATTR
               WIDTHS(ID) =  MAX( 8, W )
 
            END IF
 
 
 
         ELSE IF ( TYPES(ID) .EQ. INT .OR. TYPES(ID) .EQ. DP ) THEN
 
            FMTS  (ID) = CATTR
 
            CALL LJUST ( CATTR, TFMT )
            CALL UCASE ( TFMT,  TFMT )
 
            IF ( TFMT .EQ. 'DEFAULT' ) THEN
 
               FMTS  (ID) = 'DEFAULT'
               WIDTHS(ID) = -1
            ELSE
               FMTS  (ID) = CATTR
               WIDTHS(ID) = MAX( 8, RTRIM(CATTR) )
            END IF
 
         ELSE
 
            FMTS  (ID) = CATTR
 
         END IF
 
      ELSE IF ( ATTR .EQ. ALIAS ) THEN
 
         ALIASS ( ID ) = CATTR
 
      ELSE IF ( ATTR .EQ. JUST  ) THEN
 
         JUSTS ( ID )  = CATTR
 
      ELSE
 
         CALL SETMSG ( 'The attribute specified, #, cannot'
     .   //            'be set as a character column attribute. '  )
         CALL ERRCH  ( '#', ATTR                        )
         CALL SIGERR ( 'INSPEKT(UNSETABLECHARATTR)'     )
         CALL CHKOUT ( RNAME )
         RETURN
 
      END IF
 
      CALL CHKOUT ( RNAME )
      RETURN
 
 
 
 
 
C$Procedure CLPAI ( Column, put attribute---integer)
 
 
      ENTRY CLPAI ( ID, ATTR, IATTR )
 
C$ Abstract
C
C     Set one of the integer attributes associated with the input
C     ID.  (There is only one right now --- width).
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
C-&
 
      IF ( RETURN()  ) THEN
         RETURN
      END IF
 
      RNAME = 'CLPAI'
 
      CALL CHKIN ( RNAME )
 
      IF ( FIRST ) THEN
 
         FIRST = .FALSE.
 
         CALL SSIZEC ( MAXNAM, NAMS  )
         CALL SSIZEI ( MAXNAM, PTRS  )
         CALL SSIZEI ( MAXHDL, HNDLS )
 
         NACTIV = 0
 
         DO   I = 1, MAXNAM
            ACTIVE(I) = .FALSE.
         END DO
 
      END IF
 
 
      IF (       ( ID .LT. 1      )
     .     .OR.  ( ID .GT. MAXNAM ) ) THEN
 
         CALL SETMSG ( 'The ID, #, used to specified a column '
     .   //            'is out of bounds. To be considered as '
     .   //            'an active column, ID must be between 1 '
     .   //            'and # inclusive.'       )
         CALL ERRINT( '#', ID                   )
         CALL ERRINT( '#', MAXNAM               )
         CALL SIGERR( 'INSPEKT(BADCOLUMNID)'    )
         CALL CHKOUT ( RNAME )
         RETURN
 
      END IF
 
      IF ( .NOT. ACTIVE(ID) ) THEN
 
         CALL SETMSG ( 'The column specified by the ID, #, '
     .   //            'is not active now.'   )
         CALL ERRINT ( '#', ID                )
         CALL SIGERR( 'INSPEKT(INACTIVEID)'   )
         CALL CHKOUT ( RNAME                  )
         RETURN
 
      END IF
 
 
      IF ( ATTR .EQ. WIDTH ) THEN
 
         WIDTHS ( ID ) = IATTR(1)
 
      ELSE
 
         CALL SETMSG ( 'The attribute specified, #, cannot be'
     .   //            'set as an integer column attribute. '  )
         CALL ERRCH  ( '#', ATTR                        )
         CALL SIGERR ( 'INSPEKT(UNKNOWNCOLATTRIBUTE)'   )
         CALL CHKOUT ( RNAME )
         RETURN
 
      END IF
 
      CALL CHKOUT ( RNAME )
      RETURN
 
 
 
 
C$Procedure CLUNLD ( Column, unload )
 
      ENTRY CLUNLD ( HANDLE )
 
C$ Abstract
C
C     Disassociate a handle from all columns
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
C-&
      IF ( RETURN()  ) THEN
         RETURN
      END IF
 
      RNAME = 'CLUNLD'
 
      CALL CHKIN ( RNAME )
 
      IF ( FIRST ) THEN
 
         FIRST = .FALSE.
 
         CALL SSIZEC ( MAXNAM, NAMS  )
         CALL SSIZEI ( MAXNAM, PTRS  )
         CALL SSIZEI ( MAXHDL, HNDLS )
 
         NACTIV = 0
 
         DO   I = 1, MAXNAM
            ACTIVE(I) = .FALSE.
         END DO
 
      END IF
C
C     Look at every id.  If it is active, remove this handle
C     from the list associated with the id.
C
      DO I = 1, MAXNAM
 
         IF ( ACTIVE(I) ) THEN
C
C           Look up the handle pointer and count associated
C           with this name.
C
            CALL SYPTRI( NAMES(I), NAMS,
     .                             PTRS,
     .                             HNDLS,  START, N, GOTONE )
C
C           If our handle is among those attached to this column
C           we remove it from the list of attached handles.
C           If it was the only handle attached to this column
C           we deactivate this id.
C
            J = ISRCHI ( HANDLE, N, HNDLS(START) )
 
            IF ( J .GT. 0 ) THEN
C
C              Swap the first handle with this one and then
C              pop it from the collection of values associated
C              with this ID.
C
               CALL SYTRNI ( NAMES(I), 1, J, NAMS,
     .                                       PTRS,
     .                                       HNDLS )
 
               CALL SYPOPI ( NAMES(I),       NAMS,
     .                                       PTRS,
     .                                       HNDLS, HAN, GOTONE )
C
C              If there was only one handle, deactivate this ID.
C
               IF ( N .EQ. 1 ) THEN
                  ACTIVE(I) = .FALSE.
                  NACTIV    =  NACTIV - 1
               END IF
 
            END IF
 
         END IF
 
      END DO
 
      CALL CHKOUT ( RNAME )
      RETURN
 
 
 
C$Procedure CLSCOP ( Column set scope )
 
      ENTRY CLSCOP ( COLS, QUERY, CSIZE, IATTR, NUM, ERROR )
 
C$ Abstract
C
C     Set the scope of rows for which data can be returned.
C
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
C$ Brief I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      COLS       I   A string giving all columns to be fetched.
C      QUERY      I   A query that sets the rows in the EK to fetch
C      CSIZE      O   The number of columns present in COLS
C      IATTR      O   The query-relative idcodes of the columns.
C      NUM        O   The number of rows that match the query.
C      ERROR      O   Diagnoistic if something is bad.
C
C-&
C
C     Take care of the usual setup stuff.
C
      IF ( RETURN()  ) THEN
         RETURN
      END IF
 
      RNAME = 'CLSCOP'
 
      CALL CHKIN ( RNAME )
 
      IF ( FIRST ) THEN
 
         FIRST = .FALSE.
 
         CALL SSIZEC ( MAXNAM, NAMS  )
         CALL SSIZEI ( MAXNAM, PTRS  )
         CALL SSIZEI ( MAXHDL, HNDLS )
 
         NACTIV = 0
 
         DO   I = 1, MAXNAM
            ACTIVE(I) = .FALSE.
         END DO
 
      END IF
 
C
C     Any calls to CLSCOP wipe out any previous query relative
C     information.
C
      NQIDS = 0
 
      DO I = 1, MAXCOL
         QCIDS(I)   = -1
         QTINDX(I)  =  0
         QALIAS(I)  = '<bad alias>'
         QCOLMN(I)  = ' '
      END DO
 
C
C     First thing to do is to encode the query.
C
      CALL ZZEKENCD ( QUERY,
     .                EQRYI,  EQRYC,  EQRYD,
     .                BADQRY, ERRMSG, ERRPTR  )
 
      IF ( BADQRY ) THEN
 
         ERROR   =  ERRMSG
         NUM     =  0
         AQUERY  = .FALSE.
         CSIZE   =  0
         NQIDS   =  0
 
         CALL PREFIX ( 'CLSCOP:', 1, ERROR )
         CALL CHKOUT ( RNAME )
         RETURN
      END IF
 
C
C     Next we need to resolve the columns
C     that are requested.  There are two cases.  Wildcard requests
C     and explicit lists.
C
      IF ( COLS .EQ. '*' ) THEN
 
         ERROR = 'CLSCOP: Sorry but we can''t handle SELECT * yet.'
 
 
      ELSE
C
C        First thing we need to do is take apart the list
C        of columns.  We use SCANIT to accomplish this task.
C        Here are the delimiters we allow.  ' ' and ','
C
         IF ( UNPREP ) THEN
 
            UNPREP = .FALSE.
 
            MARKS(1) = ','
            NMARKS   =  1
C
C           We will select from the identified tokens in the
C           columns only the stuff that isn't a recognized
C           delimiter.
C
            KEEP(1)  =  0
 
            CALL SCANPR ( NMARKS, MARKS, MRKLEN, PNTERS )
 
         END IF
 
         IF ( COLS(1:1) .EQ. ',' ) THEN
            ERROR    = 'There is a column name missing in '
     .      //         'the set of columns to fetch: '
     .      //         '"<column missing here>'
     .      //          COLS
 
            CALL SUFFIX ( '"', 1, ERROR )
            NUM     =  0
            AQUERY  = .FALSE.
            CSIZE   =  0
            NQIDS   =  0
 
            CALL CHKOUT ( RNAME )
            RETURN
 
         END IF
 
 
         START = 1
         CALL SCAN   ( COLS,
     .                 MARKS,  MRKLEN, PNTERS, MAXCOL, START,
     .                 NTOKNS, IDENT,  QBEG,   QEND  )
         CALL SCANSL ( KEEP,   NKEEP,
     .                 NTOKNS, IDENT,  QBEG,   QEND  )
 
C
C        Now for each of the remaining tokens we should have
C        columns (possibly with aliases).  We need to check
C        each one for its alias and then resolve the column
C        for table index, and name.
C
 
         DO L = 1, NTOKNS
C
C           Take apart this token to get the qualified column
C           name and any alias that might be attached to it.
C
            B = QBEG(L)
            E = QEND(L)
 
            MYCOL  = COLS(B:E)
            MYALIS = ' '
 
            IF ( MYCOL .EQ. ' ' ) THEN
 
               IF ( B .GT. 1 ) THEN
                  ERROR    = 'There is a column name missing in '
     .            //         'the set of columns to fetch: "'
     .            //          COLS(1:B-1)
     .            //         '<column missing here> '
     .            //          COLS(B:)
               ELSE
                  ERROR    = 'There is a column name missing in '
     .            //         'the set of columns to fetch: '
     .            //         '"<column missing here>'
     .            //          COLS(B:)
               END IF
 
               CALL SUFFIX ( '"', 1, ERROR )
               NUM     =  0
               AQUERY  = .FALSE.
               CSIZE   =  0
               NQIDS   =  0
 
               CALL CHKOUT ( RNAME )
               RETURN
 
            END IF
 
            START = 1
            CALL FNDNWD ( MYCOL, START, B, E )
 
            CALL LJUST ( MYCOL(E+1:), MYALIS )
            QCOL =       MYCOL(B:E)
 
C
C           Find out all the interesting attributes of this column.
C
            CALL ZZEKPCOL ( QCOL,    EQRYI,   EQRYC,  TABLE,  TALIAS,
     .                      TABIDX,  LNAME,  COLIDX,  BADQRY, ERRMSG )
 
 
            IF ( BADQRY ) THEN
 
               ERROR   =  ERRMSG
               NUM     =  0
               AQUERY  = .FALSE.
               CSIZE   =  0
               NQIDS   =  0
 
               CALL PREFIX ( 'CLSCOP:', 1, ERROR )
               CALL CHKOUT ( RNAME )
               RETURN
            END IF
C
C           Construct the name that will be recognized by the
C           stuff in this routine.  Save the column name. We'll need
C           it later when we start fetching data.
C
            QCOLMN(L) = LNAME
            CALL PREFIX ( '.',   0, LNAME )
            CALL PREFIX ( TABLE, 0, LNAME )
C
C           Search for this guy in the list of loaded columns.
C
            J     = 1
            GOTIT = .FALSE.
 
            DO WHILE ( J .LT. MAXNAM .AND. .NOT. GOTIT )
 
               IF ( ACTIVE(J) .AND. LNAME .EQ. NAMES(J) ) THEN
                  MYID  =  J
                  GOTIT = .TRUE.
               END IF
 
               J = J + 1
 
            END DO
 
C
C           We should never get the error below, but just in case...
C
            IF ( .NOT. GOTIT ) THEN
 
               ERROR = 'A serious problem has occurred.  We got '
     .         //      'back the qualified name of a column from '
     .         //      'the EK library, but this column is not '
     .         //      'listed in the Column Manager''s list of '
     .         //      'columns. This should never happen. '
     .         //      'The name of the column was: ''#'', the '
     .         //      'fully qualified name was: ''#''. '
     .         //      'Please save your log file '
 
               CALL REPMC ( ERROR, '#', QCOLMN(L), ERROR )
               CALL REPMC ( ERROR, '#', LNAME,     ERROR )
               
               NUM    =  0
               AQUERY = .FALSE.
               CSIZE  =  0
               NQIDS  =  0
 
               CALL CHKOUT ( RNAME )
               RETURN
 
            END IF
C
C           If we get here we have identified this column.
C           We need to store its ID, table index, column name,
C           and SCOPE alias.
C
            NQIDS        = NQIDS + 1
            CSIZE        = NQIDS
            QCIDS  ( L ) = MYID
            QTINDX ( L ) = TABIDX
            IATTR  ( L ) = L
 
            IF ( MYALIS .NE. ' ' ) THEN
C
C              If the user supplied an alias in the query we use
C              that.
C
               QALIAS(L) = MYALIS
 
            ELSE
C
C              Otherwise we just use a provided alias.
C
               QALIAS(L) = ALIASS(MYID)
 
            END IF
 
         END DO
 
      END IF
 
C
C     All of the columns have now been resolved. Issue the search
C
 
      CALL EKSRCH   ( EQRYI, EQRYC, EQRYD, NUM, BADQRY, ERRMSG )
 
      IF ( BADQRY ) THEN
 
         ERROR   =  ERRMSG
         NUM     =  0
         AQUERY  = .FALSE.
         CSIZE   =  0
         NQIDS   =  0
         CSIZE   =  0
 
         CALL PREFIX ( 'CLSCOP:', 1, ERROR )
         CALL CHKOUT ( RNAME )
         RETURN
 
      END IF
 
C
C     At this point we have an active query.
C
      AQUERY = .TRUE.
 
C
C     Until the row advance routine is called there are
C     no rows available for examination.
C
      DO I = 1, MAXNAM
         AVAIL(I) = .FALSE.
      END DO
C
C     We need to save the number of rows found and the
C     number of rows that have been read so far.
C
      SCOPE  = NUM
      CURRNT = LASTRD
 
 
 
      CALL CHKOUT ( RNAME )
      RETURN
 
 
 
C
C     Entry for advancing the row from which columns can be fetched
C
 
      ENTRY CLADV ( FOUND )
C
C     Advance the row pointed to by the currently active query.
C
 
      IF ( RETURN()  ) THEN
         RETURN
      END IF
 
      RNAME = 'CLADV'
 
      CALL CHKIN ( RNAME )
 
      IF ( FIRST ) THEN
 
         FIRST = .FALSE.
 
         CALL SSIZEC ( MAXNAM, NAMS  )
         CALL SSIZEI ( MAXNAM, PTRS  )
         CALL SSIZEI ( MAXHDL, HNDLS )
 
         NACTIV = 0
 
         DO   I = 1, MAXNAM
            ACTIVE(I) = .FALSE.
         END DO
 
      END IF
 
 
      IF ( AQUERY ) THEN
         CURRNT = CURRNT  +   1
         FOUND  = CURRNT .LE. SCOPE
 
      ELSE
 
         FOUND = .FALSE.
         CALL CHKOUT ( RNAME )
         RETURN
 
      END IF
 
 
 
C
C     Determine which columns are available  (Yes, we could check the
C     flag GOTONE outside the loop, but it's just not worth
C     the extra code.)
C
      DO I = 1, NQIDS
 
         MYID         = QCIDS(I)
         AMOUNT(MYID) = SIZES(MYID)
         AVAIL (MYID) = .TRUE.
 
      END DO
C
C     That's all folks.
C
      CALL CHKOUT ( RNAME )
      RETURN
 
 
 
      ENTRY CLSROW ( NUM, FOUND )
C
C     Set the current row of an active query to a specific value.
C     This is only valid if there has been a successful call to
C     the routine CLSCOP.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      RNAME = 'CLSROW'
      CALL CHKIN ( RNAME )
C
C     If we don't have an active query we can just bag this and
C     return
C
      IF ( .NOT. AQUERY ) THEN
         FOUND = .FALSE.
         CALL CHKOUT ( RNAME )
         RETURN
      END IF
 
C
C     If the row requested doesn't exist for this query we can
C     bail out now as well.
C
      IF ( NUM .GT. SCOPE .OR. NUM .LT. 0 ) THEN
         FOUND = .FALSE.
         CALL CHKOUT ( RNAME )
         RETURN
      END IF
 
 
      FOUND  = .TRUE.
      CURRNT = NUM
C
C     Set the availability and amounts for each of the columns.
C
      DO I = 1, NQIDS
         MYID         = QCIDS (I)
         AVAIL (MYID) = ACTIVE(MYID)
         AMOUNT(MYID) = SIZES (MYID)
      END DO
 
      CALL CHKOUT ( RNAME )
      RETURN
 
C$Procedure CLNCMP ( Column, number of components )
 
      ENTRY CLNCMP ( ID,  NUM )
 
C     Get the number of number of components associated with the
C     specified column for the row number indicated for an active
C     query. This should always be called after CLADV
C     and before calling CLPVAL for the column specified by ID.
 
C     Note that ID is the Query Relative ID.
 
 
      IF ( RETURN()  ) THEN
         RETURN
      END IF
 
      RNAME = 'CLNCMP'
 
 
      CALL CHKIN ( RNAME )
 
      IF ( FIRST ) THEN
 
         FIRST = .FALSE.
 
         CALL SSIZEC ( MAXNAM, NAMS  )
         CALL SSIZEI ( MAXNAM, PTRS  )
         CALL SSIZEI ( MAXHDL, HNDLS )
 
         NACTIV = 0
 
         DO   I = 1, MAXNAM
            ACTIVE(I) = .FALSE.
         END DO
 
      END IF
 
C     Until we know otherwise the number of components available
C     is zero.
 
      NUM = 0
 
C     Make sure the ID is a legitimate one at this point of time.
 
      IF (       ( ID .LT. 1      )
     .     .OR.  ( ID .GT. NQIDS  ) ) THEN
 
         CALL SETMSG ( 'The ID, #, used to specified a column '
     .   //            'is out of bounds. To be considered as '
     .   //            'an active column, ID must be between 1 '
     .   //            'and # inclusive.'       )
         CALL ERRINT( '#', ID                   )
         CALL ERRINT( '#', MAXNAM               )
         CALL SIGERR( 'INSPEKT(BADCOLUMNID)'    )
         CALL CHKOUT ( RNAME )
         RETURN
 
      END IF
 
      MYID = QCIDS(ID)
 
      IF ( .NOT. ACTIVE(MYID) ) THEN
 
         CALL SETMSG ( 'The column specified by the ID, #, '
     .   //            'is not active now.'   )
         CALL ERRINT ( '#', ID                )
         CALL SIGERR( 'INSPEKT(INACTIVEID)'   )
         CALL CHKOUT ( RNAME                  )
         RETURN
 
      END IF
 
C     If we do not have an active query, then NUM = 0 (we have
C     already set it) and we can quit now.
 
      IF (      .NOT. AQUERY
     .     .OR. .NOT. AVAIL(MYID) ) THEN
         CALL CHKOUT ( RNAME )
         RETURN
      END IF
 
 
C     If this item is of variable size we will need to look up it's
C     current size.
 
      IF ( AMOUNT(MYID) .EQ. VAR ) THEN
         CALL EKNELT   ( ID, CURRNT, AMOUNT(MYID) )
      END IF
 
      IF ( AMOUNT(MYID) .EQ. 0 ) THEN
         AVAIL (MYID) = .FALSE.
      END IF
 
      NUM = AMOUNT(MYID)
 
 
      CALL CHKOUT ( RNAME )
      RETURN
 
 
 
 
C     entry CLPVAL  get the print value for a column
 
      ENTRY CLPVAL ( ID, NUM, CATTR, WDTH )
 
C     This routine returns the print value for the column specified
C     by ID as well as the width associated with this column.  If
C     there is no value for ID it is returned as a blank.  If it has
C     a NULL value the string <null> is returned.
 
      IF ( RETURN()  ) THEN
         RETURN
      END IF
 
      RNAME = 'CLPVAL'
 
      CALL CHKIN ( RNAME )
 
C     Just in case we bail out early, set the values associated
C     with CATTR and WDTH.
 
      CATTR = ' '
      WDTH =  0
 
      IF ( FIRST ) THEN
 
         FIRST = .FALSE.
 
         CALL SSIZEC ( MAXNAM, NAMS  )
         CALL SSIZEI ( MAXNAM, PTRS  )
         CALL SSIZEI ( MAXHDL, HNDLS )
 
         NACTIV = 0
 
         DO   I = 1, MAXNAM
            ACTIVE(I) = .FALSE.
         END DO
 
      END IF
 
 
      IF (       ( ID .LT. 1      )
     .     .OR.  ( ID .GT. NQIDS  ) ) THEN
 
         CALL SETMSG ( 'The ID, #, used to specified a column '
     .   //            'is out of bounds. To be considered as '
     .   //            'an active column, ID must be between 1 '
     .   //            'and # inclusive.'       )
         CALL ERRINT( '#', ID                   )
         CALL ERRINT( '#', MAXNAM               )
         CALL SIGERR( 'INSPEKT(BADCOLUMNID)'    )
         CALL CHKOUT ( RNAME )
         RETURN
 
      END IF
 
C     Look up the global column ID value.
 
      MYID = QCIDS(ID)
 
C     This had better be active, but it doesn't hurt to check.
 
      IF ( .NOT. ACTIVE(MYID) ) THEN
 
         CALL SETMSG ( 'The column specified by the ID, #, '
     .   //            'is not active now.'   )
         CALL ERRINT ( '#', ID                )
         CALL SIGERR( 'INSPEKT(INACTIVEID)'   )
         CALL CHKOUT ( RNAME                  )
         RETURN
 
      END IF
 
 
 
C     OK. We are ready to rock 'n roll.
 
      WDTH = WIDTHS(MYID)
 
      IF ( WDTH .LT. 0 .AND. TYPES(MYID) .EQ. TIME ) THEN
         CALL BBGETC_1 ( 'COPY', 'TIMEFMT', N, TFMT )
         CALL NSPCHT   ( TFMT,    WDTH              )
      END IF
 
C     If the column was unavailable, say so.
 
      IF ( .NOT. AVAIL(MYID) ) THEN
         IF ( NUM .LE. 1 ) THEN
            CATTR = NOCOLM
         ELSE
            CATTR = ' '
         END IF
 
         CALL CHKOUT ( RNAME )
         RETURN
      END IF
 
C     If we go beyond the number of components available
C     return a blank.
 
      IF ( NUM .GT. AMOUNT(MYID) ) THEN
         CATTR = ' '
         CALL CHKOUT ( RNAME )
         RETURN
      END IF
 
 
 
C     Ok.  We should be in range and actually have something to
C     fetch.
 
      IF      ( TYPES(MYID) .EQ. CH )   THEN
 
         MYSTR = ' '
         CALL EKGC   ( ID, CURRNT, NUM, MYSTR, NULVAL, GOTONE )
 
      ELSE IF ( TYPES(MYID) .EQ. DP )   THEN
 
         CALL EKGD   ( ID, CURRNT, NUM, X,     NULVAL, GOTONE )
 
      ELSE IF ( TYPES(MYID) .EQ. INT ) THEN
 
         CALL EKGI   ( ID, CURRNT, NUM, I,     NULVAL, GOTONE )
 
      ELSE IF ( TYPES(MYID) .EQ. TIME ) THEN
 
         CALL EKGD   ( ID, CURRNT, NUM, X,     NULVAL, GOTONE )
 
      END IF
 
 
      IF ( .NOT. GOTONE ) THEN
 
         AVAIL(MYID) = .FALSE.
 
         IF ( NUM .LE. 1 ) THEN
            CATTR = NOCOLM
         ELSE
            CATTR = ' '
         END IF
 
         CALL CHKOUT ( RNAME )
         RETURN
      END IF
 
 
C     We have to treat columns with null values too.
 
      IF ( NULVAL ) THEN
         IF ( NUM .EQ. 1 ) THEN
            CATTR = NULCOL
         ELSE
            CATTR = ' '
         END IF
 
         CALL CHKOUT ( RNAME )
         RETURN
      END IF
 
C     Finally, it is time to produce a print string for this
C     value.
 
      IF ( TYPES(MYID) .EQ. CH ) THEN
 
         CATTR = MYSTR
 
      ELSE IF ( TYPES(MYID) .EQ. INT ) THEN
 
         IF ( FMTS(MYID) .EQ. 'DEFAULT' ) THEN
            CALL BBGETC_1 ( 'COPY', 'INTFMT', N, TFMT )
         ELSE
            TFMT = FMTS(MYID)
         END IF
 
         CALL FMTINT ( I, TFMT, CATTR )
 
      ELSE IF ( TYPES(MYID) .EQ. DP ) THEN
 
         IF ( FMTS(MYID) .EQ. 'DEFAULT' ) THEN
            CALL BBGETC_1 ( 'COPY', 'DPFMT', N, TFMT )
         ELSE
            TFMT = FMTS(MYID)
         END IF
 
         CALL FMTDP  ( X, TFMT, CATTR )
 
      ELSE IF ( TYPES(MYID) .EQ. TIME ) THEN
 
         IF ( FMTS(MYID) .EQ. 'DEFAULT' ) THEN
            CALL BBGETC_1 ( 'COPY', 'TIMEFMT', N, TFMT )
         ELSE
            TFMT = FMTS(MYID)
         END IF
 
         CALL FMTTIM ( X, TFMT, CATTR )
 
      END IF
 
      CALL CHKOUT ( RNAME )
      RETURN
 
 
 
 
C     The following entry point finds the column name that most
C     closely spells the input name.
 
      ENTRY CLSPEL ( NAME, FOUND, CORCTN, ERROR )
 
      CALL SSIZEC ( MAXNAM, KNOWN  )
      CALL SSIZEI ( 10,     BEST   )
      CALL SSIZEI ( 10,     SCORES )
 
      DO I = 1, MAXNAM
 
         IF ( ACTIVE(I) ) THEN
            CALL APPNDC ( NAMES(I), KNOWN )
         END IF
 
      END DO
 
      CUTOFF = 70
 
      CALL BESTWD ( NAME, KNOWN, CUTOFF, BEST, SCORES, ERROR )
 
      HIGH = -1
 
      DO I = 1, CARDI ( BEST )
 
         IF ( BEST(I) .GT. HIGH ) THEN
            HIGHST = I
            HIGH   = BEST(I)
         END IF
 
      END DO
 
      IF ( HIGH .GE. CUTOFF ) THEN
         FOUND  = .TRUE.
         CORCTN = KNOWN(HIGHST)
      ELSE
         FOUND  = .FALSE.
         CORCTN = ' '
      END IF
 
      RETURN
 
 
C$Procedure CLGQAL ( Column --- get query relative alias )
 
 
      ENTRY CLGQAL ( ID, CATTR )
 
C$ Abstract
C
C     This entry point returns the query alias for a column if there
C     is an active query.
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
C-&
 
      RNAME = 'CLGQAL'
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( RNAME )
C
C     A column ID has a query alias only if there is an active
C     query.
C
      IF ( .NOT. AQUERY ) THEN
 
         CATTR = ' '
         CALL CHKOUT ( RNAME )
         RETURN
 
      END IF
 
C
C     Check to make sure the ID is in range.
C
      IF ( ID .LT. 0 .OR. ID .GT. NQIDS ) THEN
 
         CATTR = ' '
         CALL CHKOUT ( RNAME )
         RETURN
 
      END IF
 
      CATTR = QALIAS( ID )
      CALL CHKOUT ( RNAME )
      RETURN
 
 
 
C$Procedure CLQ2ID (Column, query id to global column id.)
 
      ENTRY CLQ2ID ( CSIZE, ID )
 
C$ Abstract
C
C     Map a query relative ID to the corresponding global
C     column ID.
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
C$ Brief I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      CSIZE      I   The query relative ID of some column
C      ID         O   The corresponding global column ID.
C-&
 
      IF (       AQUERY
     .    .AND. CSIZE .GT. 0
     .    .AND. CSIZE .LE. NQIDS ) THEN
 
         ID = QCIDS(CSIZE)
 
      ELSE
 
         ID = -1
 
      END IF
      RETURN
 
 
 
 
 
      END

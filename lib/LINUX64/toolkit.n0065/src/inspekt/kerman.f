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
 
      SUBROUTINE KERMAN ( COMMND, INFILE, ERROR )
 
C
C     Version 2.4.0, 26-SEP-2005
C
C        Minor bug fix: replaced FILE with INFILE in the RTRIM call
C        constructing "The file # is not listed ..." error message.
C
C     Version 2.3.0, 21-JUN-1999
C
C        Added RETURN before first entry points.
C
C     Version 2.2.0, 22-APR-1997
C
C        Declared PAGPUT external
C
C     Version 2.1.0  14-SEP-1995
C
C        Variable INDEX removed.
C
C     Version 2.0.0  23-AUG-1995
C
C        The widest string in a string column is no longer supplied
C        by the EK summary stuff.  We just set the value WIDEST
C        to 24.
C
 
       IMPLICIT NONE
C
C     This routine handles the loading of E-kernels, leapsecond and
C     SCLK kernels.
C
      CHARACTER*(*)         COMMND
      CHARACTER*(*)         INFILE
      CHARACTER*(*)         ERROR   ( 2 )
C
C     Passable routines
C
      EXTERNAL              NSPWLN
      EXTERNAL              GCOLMN
      EXTERNAL              PAGPUT
 
C
C     Parameters that contain the routine name for use in check-in,
C     check-out, and error messages.
C
      CHARACTER*(6)         RNAME
      CHARACTER*(7)         RNAMEC
 
C
C     SPICELIB functions
C
      INTEGER               ISRCHC
      INTEGER               ISRCHI
      INTEGER               POS
      INTEGER               CARDC
      INTEGER               RTRIM
      INTEGER               LTRIM
 
      LOGICAL               MATCH
      LOGICAL               RETURN
 
C
C     E-kernel functions
C
      INTEGER               EKNSEG
 
C
C     Meta/2 Functions
C
      LOGICAL               M2XIST
 
C
C     Interface to the SPICELIB error handling.
C
      LOGICAL               HAVE
 
C
C     Ek include files.
C
C+==============================================================
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ekbool.inc'
      INCLUDE 'ektype.inc'
 
C+==============================================================
C
C     Meta/2 syntax definition variables.
C
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )
 
      INTEGER               NUPPER
      PARAMETER           ( NUPPER = 3 )
 
      INTEGER               NSYN
      PARAMETER           ( NSYN   = NUPPER )
 
      INTEGER               SYNLEN
      PARAMETER           ( SYNLEN = 80 )
 
      CHARACTER*(1)         BS
      CHARACTER*(WDSIZE)    SYNKEY ( LBCELL : NSYN )
      INTEGER               SYNPTR ( LBCELL : NSYN )
      CHARACTER*(SYNLEN)    SYNVAL ( LBCELL : NSYN )
 
C
C     E-kernel column type definitions
C
C
C     INTEGER               CH
C     PARAMETER           ( CH   = 1 )
C
C     INTEGER               DP
C     PARAMETER           ( DP   = 2 )
C
C     INTEGER               INT
C     PARAMETER           ( INT  = 3 )
C
C     INTEGER               TIME
C     PARAMETER           ( TIME = 4 )
C
C     Local Parameters
C
C     FILSIZ   is the maximum number of characters allowed for a
C              filename
C
C     LNGSIZ   is the maximum number of characters allowed for
C              use in reporting the columns associated with a given
C              file.
C
C     MAXFIL   is the maximum number of E-kernels that can be loaded
C              at any one time.
C
C     NNAMES   is the maximum number of names/headings that can appear
C              in a report of loaded files and columns.
C
C     MAXCOL   is the maximum number of columns that may be present
C              in any segment of an E-kernel
C
C     LNSIZE   is the standard text line length.
C
      INTEGER               FILSIZ
      PARAMETER           ( FILSIZ = 127 )
 
      INTEGER               MAXFIL
      PARAMETER           ( MAXFIL = 20 )
 
      INTEGER               NNAMES
      PARAMETER           ( NNAMES = 4 )
 
      INTEGER               MAXCOL
      PARAMETER           ( MAXCOL = 100 )
 
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE  = 80 )
 
      INTEGER               CLSIZE
      PARAMETER           ( CLSIZE =  64 )
 
      INTEGER               IDSIZE
      PARAMETER           ( IDSIZE = 8 )
 
      INTEGER               INFSIZ
      PARAMETER           ( INFSIZ = 60 )
 
      INTEGER               LONGSZ
      PARAMETER           ( LONGSZ = 300 )
 
C
C     Initialization logical
C
      LOGICAL               FIRST
 
C
C     Loaded file database (shared between entry points)
C
      INTEGER               NFILES
      CHARACTER*(FILSIZ)    EKFILS ( MAXFIL )
 
C
C     Local Variables
C
      CHARACTER*(FILSIZ)    FILE
      CHARACTER*(FILSIZ)    THISFL
 
      CHARACTER*(LONGSZ)    MESSGE
 
 
      CHARACTER*(CLSIZE)    CNAMES ( MAXCOL )
      CHARACTER*(CLSIZE)    TABNAM
      CHARACTER*(WDSIZE)    NAME
 
      CHARACTER*(IDSIZE)    IDWORD
      CHARACTER*(INFSIZ)    IFNAME
 
 
      INTEGER               CDSCRS ( CDSCSZ, MAXCOL )
      INTEGER               COL
      INTEGER               CLEN
      INTEGER               COUNT
      INTEGER               CSIZE
      INTEGER               CTYPE
      INTEGER               HANDLE
      INTEGER               HANDLS (         MAXFIL )
      INTEGER               ID
      INTEGER               I
      INTEGER               J
      INTEGER               NCOLS
      INTEGER               NH
      INTEGER               NID
      INTEGER               NSEG
      INTEGER               SEG
      INTEGER               SEGDSC ( SDSCSZ         )
      INTEGER               WIDEST
      INTEGER               TOTALC
      INTEGER               R
      INTEGER               L
      INTEGER               NRESVR
      INTEGER               NRESVC
      INTEGER               NCOMR
      INTEGER               NCOMC
      INTEGER               HITS
 
      LOGICAL               CINDXD
      LOGICAL               CNULL
      LOGICAL               FOUND
      LOGICAL               QUIT
 
 
      LOGICAL               TRUTH
 
C
C     INTEGER               IFALSE
C     PARAMETER           ( IFALSE = -1 )
C
C
C     Variables needed by NSPEKS
C
 
      INTEGER               MAXRM
      PARAMETER           ( MAXRM = 500 )
 
      INTEGER               SHSIZE
      PARAMETER           ( SHSIZE = 4 )
 
 
      CHARACTER*(LNSIZE)    BREAK
      CHARACTER*(LNSIZE)    CNAME
      CHARACTER*(LNSIZE)    STYLE
      CHARACTER*(LNSIZE)    TABCOL ( LBCELL : MAXRM )
 
      CHARACTER*(SHSIZE)    SPCIAL ( 5 )
      CHARACTER*(SHSIZE)    INDX
 
      CHARACTER*(WDSIZE)    LSTTAB
      CHARACTER*(WDSIZE)    PVAL  ( 4 )
      CHARACTER*(WDSIZE)    SIZE
      CHARACTER*(WDSIZE)    TNAME
      CHARACTER*(WDSIZE)    TYPE
 
      INTEGER               COLIDS ( LBCELL : MAXRM )
      INTEGER               HEADR  ( 5 )
      INTEGER               IDS    ( 5 )
      INTEGER               K
      INTEGER               LEFT
      INTEGER               LMARGE
      INTEGER               N
      INTEGER               NB
      INTEGER               NEED
      INTEGER               ORDVEC (          MAXRM )
      INTEGER               REQD
      INTEGER               RIGHT
      INTEGER               SIZES  ( 5 )
      INTEGER               SB
      INTEGER               SPACE
      INTEGER               TCODE
      INTEGER               WIDTH  ( 5 )
 
      LOGICAL               JUSTR  ( 5 )
      LOGICAL               PRESRV ( 5 )
 
 
 
C
C     Save everything.
C
      SAVE
 
C
C     Initial Values
C
      DATA     NFILES /  0     /
      DATA     FIRST  / .TRUE. /
      DATA     ( SYNVAL(I), I=LBCELL,NUPPER )
     .                / ' ', ' ', ' ', ' ', ' ', ' ',
     .                  'EK #word[ekfile]',
     .                  'LEAPSECONDS #word[leapfile] ',
     .                  'SCLK KERNEL #word[sclkfile]' /
 
 
 
      TRUTH ( I ) = I .NE. IFALSE
      RETURN
 
C
C  Load an E-, leapsecond, or sclk kernel.
C
      ENTRY NSPLD ( COMMND, ERROR )
C
C     Standard Spicelib error handling.
C
      RNAME  = 'NSPLD'
      RNAMEC = 'NSPLD:'
 
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( RNAME )
C
C     On the first pass establish the syntax that this routine
C     is responsible for recognizing.
C
      IF ( FIRST ) THEN
 
         FIRST = .FALSE.
         BS    =  '@'
 
         DO I = 1, MAXCOL
            CNAMES(I) = ' '
         END DO
 
 
         DO I = 1, NUPPER
            CALL REPLCH ( SYNVAL(I), '#', BS, SYNVAL(I)        )
         END DO
 
         CALL M2INTS ( NSYN, SYNKEY, SYNPTR, SYNVAL )
 
      END IF
 
C
C     See if this command matches a known syntax.  If it doesn't
C     there is no point in hanging around.
C
      CALL M2CHCK ( COMMND, SYNKEY, SYNPTR, SYNVAL, ERROR )
 
      IF ( HAVE(ERROR) ) THEN
         CALL PREFIX ( RNAMEC, 1, ERROR )
         CALL CHKOUT ( RNAME )
         RETURN
      END IF
 
 
 
      IF      ( M2XIST('ekfile')   ) THEN
C
C        We need to have a leapseconds kernel loaded before
C        we can load an E-kernel.
C
         CALL EXPOOL ( 'DELTET/DELTA_AT', FOUND )
 
         IF ( .NOT. FOUND ) THEN
 
            ERROR(1) = 'Before an E-kernel can be loaded, you '
     .      //         'must load a leapseconds kernel.  '
            CALL CHKOUT ( RNAME )
            RETURN
 
         END IF
 
         CALL M2GETC ( 'ekfile', COMMND, FOUND, FILE  )
C
C        See if we already have this file.
C
         IF ( ISRCHC ( FILE, NFILES, EKFILS ) .GT. 0 ) THEN
            CALL CHKOUT ( RNAME )
            RETURN
         END IF
C
C        Make sure there is room for this file.
C
         IF ( NFILES .EQ. MAXFIL ) THEN
            ERROR(1) = 'The maximum number of E-kernels that can '
     .      //         'loaded at open by INSPEKT at one time is '
     .      //         '#.  That number has already been reached.'
     .      //         ' You will need to unload one of the '
     .      //         'files that have already been loaded '
     .      //         'before you will be able to load any other '
     .      //         'files. '
 
            CALL REPMCT ( ERROR(1), '#', MAXFIL, 'L', ERROR(1) )
            CALL PREFIX ( RNAMEC,        1,           ERROR(1) )
            CALL CHKOUT ( RNAME )
            RETURN
         END IF
C
C        Load the file as an e-kernel.
C
         CALL EKLEF   ( FILE(1:RTRIM(FILE)), HANDLE )
 
         IF ( HAVE(ERROR) ) THEN
            CALL PREFIX ( RNAMEC, 1, ERROR )
            CALL CHKOUT ( RNAME )
            RETURN
         END IF
C
C        Store the name of this file.
C
         NFILES = NFILES + 1
         EKFILS ( NFILES ) = FILE
 
 
C
C        Determine how many segments are in the file we just loaded.
C
         NSEG = EKNSEG   ( HANDLE )
C
C        For each segment in the newly loaded file ...
C
         DO SEG = 1, NSEG
 
            TABNAM = ' '
 
            DO I = 1, MAXCOL
               CNAMES(I) = ' '
            END DO
 
            CALL ZZEKSINF ( HANDLE, SEG,    TABNAM,
     .                      SEGDSC, CNAMES, CDSCRS )
 
 
C
C           Add each column name to the list of columns held by the
C           column manager.
C
            NCOLS = SEGDSC(NCIDX)
 
            DO COL = 1, NCOLS
C
C              We need to make the column name include table it
C              belongs to (a fully qualified column name).
C
               CALL PREFIX ( '.',    0,      CNAMES(COL) )
               CALL PREFIX ( TABNAM, 0,      CNAMES(COL) )
 
               CINDXD    = TRUTH  ( CDSCRS(IXTIDX, COL )  )
               CNULL     = TRUTH  ( CDSCRS(NFLIDX, COL )  )
 
               CTYPE     = CDSCRS ( TYPIDX, COL )
               CLEN      = CDSCRS ( LENIDX, COL )
               CSIZE     = CDSCRS ( SIZIDX, COL )
C
C              This is what used to be here, but the item NBLIDX
C              vanished by design.  We now just set this so something
C              reasonable.  24 seemed like the reasonable thing at
C              the time.  (See the column manager and do a bit of
C              code diving to see what this is used for.)
C
C              WIDEST    = CDSCRS ( NBLIDX, COL )
C
 
               WIDEST = 24
 
               CALL CLNEW ( CNAMES(COL), HANDLE,
     .                      CTYPE,
     .                      CLEN,        WIDEST,
     .                      CSIZE,       CINDXD, CNULL, ID )
 
 
            END DO
 
         END DO
C
C        If anything went wrong, unload the file.
C
         IF ( HAVE(ERROR) ) THEN
            CALL PREFIX ( RNAMEC, 1, ERROR )
            CALL EKUEF   ( HANDLE )
            CALL CLUNLD  ( HANDLE )
            NFILES = NFILES - 1
            CALL CHKOUT  ( RNAME )
            RETURN
         END IF
 
      ELSE IF ( M2XIST('leapfile') ) THEN
 
         CALL M2GETC ( 'leapfile', COMMND, FOUND, FILE  )
         CALL LDPOOL ( FILE )
         CALL BBPUTC_1 ( 'POST', 'LEAPSECONDS', 1, FILE )
 
      ELSE IF ( M2XIST('sclkfile') ) THEN
 
         CALL M2GETC ( 'sclkfile', COMMND, FOUND, FILE  )
         CALL LDPOOL ( FILE )
         CALL BBPUTC_1 ( 'APPEND', 'SCLK', 1, FILE )
      ELSE
 
         ERROR(1) = 'The input command was unrecognized and '
     .   //         'somehow got to an "impossible" place '
     .   //         'in KERMAN.FOR'
 
      END IF
 
      IF ( HAVE(ERROR) ) THEN
         CALL PREFIX ( RNAMEC, 1, ERROR )
         CALL CHKOUT ( RNAME )
         RETURN
      END IF
 
      CALL CHKOUT ( RNAME )
      RETURN
 
 
 
C
C  Unload an E-kernel from the list of known files.
C
      ENTRY NSPULD ( INFILE, ERROR )
 
      RNAME  = 'NSPULD'
      RNAMEC = 'NSPULD:'
 
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( RNAME )
 
      J = ISRCHC ( INFILE, NFILES, EKFILS )
 
      IF ( J .EQ. 0 ) THEN
         ERROR(1) = 'The file # is not listed among those files '
     .   //         'that have been loaded. '
         CALL REPMC ( ERROR(1), '#', INFILE(1:RTRIM(INFILE)), ERROR(1) )
         CALL CHKOUT( RNAME )
         RETURN
      END IF
 
C
C     Get the handle associated with this file.
C
      CALL DASFNH ( INFILE(1:RTRIM(INFILE)), HANDLE )
 
      IF ( HAVE(ERROR) ) THEN
         CALL CHKOUT ( RNAME )
         RETURN
      END IF
C
C     Now unload the file, and detach its handle from any columns to
C     which it might be attached.
C
      CALL EKUEF   ( HANDLE )
      CALL CLUNLD  ( HANDLE )
C
C     Finally remove this file from our internal list of files.
C
      CALL REMLAC  ( 1, J, EKFILS, NFILES )
 
 
      CALL CHKOUT ( RNAME )
      RETURN
 
 
C
C  Create a report regarding currently loaded kernels/columns.
C
      ENTRY NSPEKS
C
C     Version 2.0  Aug 3, 1995
C
C        This routine was rewritten to provide a more friendly
C        kernel summary.
C
C     ---B. Taber
C
C     This routine displays the currently loaded E-kernels.
C
      RNAME  = 'NSPEKS'
      RNAMEC = 'NSPEKS:'
 
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
C
C     write (*,*) 'Checking in:'
C
      CALL CHKIN ( RNAME )
 
      IF ( NFILES .LE. 0 ) THEN
 
         CALL NSPWLN ( ' ' )
         CALL NSPWLN ( 'There are no E-kernels loaded now.' )
         CALL NSPWLN ( ' ' )
         CALL CHKOUT ( RNAME )
         RETURN
 
      END IF
C
C     First thing we do is set up the NICEPR_1 style string
C     to be used in creation of summary headers.
C
C     write (*,*) 'Fetching margins: '
      CALL NSPGLR (  LEFT,          RIGHT )
      CALL NSPMRG (                 STYLE )
 
      CALL SUFFIX ( 'FLAG',      1, STYLE )
      CALL SUFFIX ( 'E-kernel:', 1, STYLE )
C
C     Reset the output page, title frequency and header frequency
C     values.
C
C     write (*,*) 'Resetting page and setting up page attributes:'
C
      CALL PAGRST
      CALL PAGSET ( 'TITLEFREQUENCY',   0 )
      CALL PAGSET ( 'HEADERFREQUENCY',  0 )
      CALL PAGSET ( 'NOSPACEFOOTER',    1 )
      CALL PAGSET ( 'FOOTERFREQUENCY', -1 )
 
 
      PVAL(1) = 'CH'
      PVAL(2) = 'D.P.'
      PVAL(3) = 'INTEGER'
      PVAL(4) = 'TIME'
 
 
      LMARGE  = 1
      SPACE   = 1
C
C     Next we set up the the column id codes, sizes,
C     default widths, justifications, component preservation,
C     and special marker attributes for each column.
C
 
 
      HEADR(1)  = 1
      HEADR(2)  = 2
      HEADR(3)  = 3
      HEADR(4)  = 4
      HEADR(5)  = 5
 
      SIZES(1)  = 1
      SIZES(2)  = 1
      SIZES(3)  = 1
      SIZES(4)  = 1
      SIZES(5)  = 1
 
      WIDTH(1)  = 16
      WIDTH(2)  = 16
      WIDTH(3)  = 8
      WIDTH(4)  = 8
      WIDTH(5)  = 6
 
      NEED      = WIDTH(1)
     .          + WIDTH(2)
     .          + WIDTH(3)
     .          + WIDTH(4)
     .          + WIDTH(5)
     .          + 4
 
      RIGHT = MIN ( RIGHT, NEED )
      CALL PAGSET ( 'PAGEWIDTH', RIGHT   )
 
      REQD      = WIDTH(3)
     .          + WIDTH(4)
     .          + WIDTH(5)
     .          + 4
C
C     If the page width is less than default needed, we reset the
C     widths of the first two columns so they will fit in available
C     space.
C
      IF ( RIGHT .LT. NEED ) THEN
         WIDTH(1) = (RIGHT - REQD)/2
         WIDTH(2) =  WIDTH(1)
      END IF
 
      JUSTR(1)  = .FALSE.
      JUSTR(2)  = .FALSE.
      JUSTR(3)  = .FALSE.
      JUSTR(4)  = .TRUE.
      JUSTR(5)  = .TRUE.
 
      PRESRV(1) = .TRUE.
      PRESRV(2) = .TRUE.
      PRESRV(3) = .TRUE.
      PRESRV(4) = .TRUE.
      PRESRV(5) = .TRUE.
 
      SPCIAL(1) = ' '
      SPCIAL(2) = ' '
      SPCIAL(3) = ' '
      SPCIAL(4) = ' '
      SPCIAL(5) = ' '
 
C
C     write (*,*) 'Starting file loop:'
C
      DO I = 1, NFILES
C
C        Get the handle associated with this file, and get the
C        number of ID's currently known.
C
 
         CALL DASFNH ( EKFILS(I), HANDLE )
         CALL CLNUM  ( NID               )
C        write (*,*) 'File: ', I, 'Handle: ', HANDLE
C
C        Now empty out the table/column data for this file.
C
C        write (*,*) 'Empty out the column collector.'
         CALL SSIZEC ( MAXRM, TABCOL )
         CALL SSIZEI ( MAXRM, COLIDS )
C
C        Cycle over all column id's to determine if they
C        are attached to this particular file.
C
C        write (*,*) 'Beginning Column search:  ', NID, ' Columns'
         DO J = 1, NID
 
            CALL CLNID ( J,   ID,            FOUND  )
            CALL CLGAI ( ID, 'HANDLES',  NH, HANDLS )
 
            IF ( ISRCHI ( HANDLE, NH, HANDLS ) .GT. 0 ) THEN
C
C              This column is associated with this file.  Store
C              its name and id-code for the next section of code.
C
C              write (*,*) 'Column id and associated handle match.'
C
               CALL CLGAC  ( ID,   'NAME', CNAME )
               CALL APPNDC ( CNAME, TABCOL      )
               CALL APPNDI ( ID,    COLIDS      )
 
            END IF
 
         END DO
 
C
C        Layout the pages.  We perform a soft page reset
C        so that the various sections will be empty.
C        Note this doesn't affect frequency parameter
C        or other geometry attributes of pages.
C
C        write (*,*) 'Creating page: Title:'
C
         CALL PAGSCN ( 'TITLE' )
         CALL PAGPUT ( ' ' )
         CALL PAGPUT ( 'Summary of Loaded E-kernels' )
         CALL PAGPUT ( ' ' )
C
C        write (*,*) 'Creating page: Header'
C
C        Set up the various items needed for the report header.
C
         CALL PAGSCN   ( 'HEADER' )
         CALL PAGPUT   ( ' '      )
         CALL NICEPR_1 ( EKFILS(I), STYLE, PAGPUT )
         CALL PAGPUT   ( ' ' )
 
         CALL SCOLMN   ( 1, 1, 'Table Name'  )
         CALL SCOLMN   ( 2, 1, 'Column Name' )
         CALL SCOLMN   ( 3, 1, 'Type'        )
         CALL SCOLMN   ( 4, 1, 'Size'        )
         CALL SCOLMN   ( 5, 1, 'Index'       )
C
C        write (*,*) 'Creating page: Column headings'
C
         CALL TABRPT   ( 5,
     .                   HEADR,  SIZES, WIDTH, JUSTR,  PRESRV, SPCIAL,
     .                   LMARGE, SPACE,
     .                   GCOLMN )
 
         BREAK = '========================================'
     .   //      '========================================'
         CALL PAGPUT   ( BREAK(1:RIGHT) )
 
C
C        Now set the page section to the body portion for
C        preparing to fill in the e-kernel summary.
C
C        write (*,*) 'Creating page: Body of report:'
         CALL PAGSCN   ( 'BODY'        )
 
         N =  CARDC  ( TABCOL )
         CALL ORDERC ( TABCOL(1), N, ORDVEC )
 
         LSTTAB = ' '
 
 
         DO J = 1, N
 
            K = ORDVEC(J)
 
            CALL CLGAC ( COLIDS(K), 'TABLE',   TNAME )
            CALL CLGAC ( COLIDS(K), 'NAME',    CNAME )
            CALL CLGAC ( COLIDS(K), 'SIZE',    SIZE  )
            CALL CLGAC ( COLIDS(K), 'INDEXED', INDX  )
C
C           Note:  There is only one type associated with each
C           handle.  Thus TCODE does not need to be an array.
C
            CALL CLGAI ( COLIDS(K), 'TYPE', COUNT, TCODE )
 
 
            IF ( TNAME .EQ. LSTTAB ) THEN
               TNAME = ' '
            ELSE IF ( LSTTAB .NE. ' ' ) THEN
               CALL PAGPUT ( ' ' )
               LSTTAB = TNAME
            ELSE
               LSTTAB = TNAME
            END IF
 
            NB   = POS( CNAME, '.', 1 ) + 1
            NAME = CNAME(NB:)
 
            IF ( TCODE .EQ. 1 ) THEN
               CALL CLGAC ( COLIDS(K), 'TYPE', TYPE )
               SB      = POS( TYPE, '*', 1 )
               PVAL(1) = 'CH'
               CALL SUFFIX ( TYPE(SB:), 0, PVAL(1) )
            END IF
 
            CALL SCOLMN (  6, 1, TNAME       )
            CALL SCOLMN (  7, 1, NAME        )
            CALL SCOLMN (  8, 1, PVAL(TCODE) )
            CALL SCOLMN (  9, 1, SIZE        )
            CALL SCOLMN ( 10, 1, INDX        )
 
 
            IDS(1) = 6
            IDS(2) = 7
            IDS(3) = 8
            IDS(4) = 9
            IDS(5) = 10
 
C
C           write (*,*) 'Creating next row:'
C           write (*,*) TNAME
C           write (*,*) NAME
C           write (*,*) PVAL(TCODE)
C           write (*,*) SIZE
C           write (*,*) INDX
C
            CALL TABRPT( 5,
     .                   IDS,    SIZES, WIDTH, JUSTR,  PRESRV, SPCIAL,
     .                   LMARGE, SPACE,
     .                   GCOLMN )
C           write (*,*) 'Row created.'
C
 
         END DO
C
C        Do a soft page reset so for the next file to be displayed
C
C        write (*,*) 'Performing soft page reset.'
         CALL PAGSFT
         CALL PAGRST
         CALL PAGSET ( 'TITLEFREQUENCY',   -1 )
         CALL PAGSET ( 'HEADERFREQUENCY',  0 )
         CALL PAGSET ( 'NOSPACEFOOTER',    1 )
         CALL PAGSET ( 'FOOTERFREQUENCY', -1 )
 
      END DO
 
      CALL CHKOUT ( RNAME )
      RETURN
 
 
C$Procedure      NSPEKC ( Inspekt the comments from EK files )
 
      ENTRY NSPEKC ( INFILE )
 
C     This entry point examines each file that matches the
C     template given by INFILE and if comments exist for the
C     file, they are displayed.
 
C     Version 1.0.0 25-AUG-1995 (WLT)
 
 
      CALL CHKIN ( 'NSPEKC' )
      TOTALC = 0
      THISFL = ' '
 
 
C     We might not need the style string, but it doesn't hurt to
C     get it.
 
      CALL NSPMRG ( STYLE )
 
 
C     If there are no loaded E-kernels say so and return.
 
      IF ( NFILES .EQ. 0 ) THEN
         MESSGE = 'There are no E-kernels loaded now. '
         CALL NICEPR_1 ( MESSGE, STYLE, NSPWLN )
         CALL CHKOUT ( 'NSPEKC' )
         RETURN
      END IF
 
 
C     Count the number of characters present in the files
C     that match the template.
 
      R = RTRIM(INFILE)
      L = LTRIM(INFILE)
 
      DO I = 1, NFILES
 
 
         IF ( MATCH ( EKFILS(I), INFILE(L:R) ) ) THEN
 
            CALL DASFNH ( EKFILS(I), HANDLE )
            CALL DASRFR (  HANDLE,
     .                     IDWORD, IFNAME,
     .                     NRESVR, NRESVC,
     .                     NCOMR,  NCOMC   )
 
            TOTALC = TOTALC + NCOMC
            HITS   = HITS   + 1
            THISFL = EKFILS(I)
 
         END IF
 
      END DO
 
C     If we didn't get any characters there several possible
C     reasons.  We can look at HITS to see why and form a
C     grammatically reasonable message.
 
      IF ( TOTALC .EQ. 0 ) THEN
 
         IF ( HITS .EQ. 0 ) THEN
 
            MESSGE = 'There are no E-kernels loaded whose file '
     .      //       'name matches the supplied template ''#''.'
            CALL REPMC ( MESSGE, '#', INFILE(L:R), MESSGE )
 
         ELSE IF ( HITS .EQ. 1 ) THEN
 
            MESSGE = 'There are no comments present in the file '
     .      //       '''#''. '
            CALL REPMC ( MESSGE, '#', THISFL, MESSGE )
 
         ELSE IF ( HITS .EQ. 2 ) THEN
 
            MESSGE = 'There are no comments present in either of '
     .      //       'the # files that match the supplied '
     .      //       'template. '
 
            CALL REPMCT ( MESSGE, '#', HITS, 'L', MESSGE )
 
         ELSE
 
            MESSGE = 'There are no comments present in any of '
     .      //       'the # files that match the supplied '
     .      //       'template. '
 
            CALL REPMCT ( MESSGE, '#', HITS, 'L', MESSGE )
 
         END IF
 
         CALL NICEPR_1 ( MESSGE, STYLE, NSPWLN )
         CALL CHKOUT ( 'NSPEKC' )
         RETURN
      END IF
 
 
C     Ok. We've got something.  Set up the output page to receive
C     the comments a file at a time.
 
      CALL SUFFIX ( 'FLAG E-kernel:', 1, STYLE )
 
 
      DO I = 1, NFILES
 
 
         IF ( MATCH ( EKFILS(I), INFILE(L:R) ) ) THEN
 
            CALL DASFNH ( EKFILS(I), HANDLE )
            CALL DASRFR (  HANDLE,
     .                     IDWORD, IFNAME,
     .                     NRESVR, NRESVC,
     .                     NCOMR,  NCOMC   )
 
            IF ( NCOMC .EQ. 0 ) THEN
 
               MESSGE = '# contains no comments.'
               CALL REPMC ( MESSGE, '#', EKFILS(I), MESSGE )
               CALL NSPWLN   ( ' ' )
               CALL NICEPR_1 ( MESSGE, STYLE, NSPWLN )
 
            ELSE
 
               CALL PAGRST
 
               CALL PAGSCN ( 'HEADER' )
               CALL PAGSET ( 'TITLEFREQUENCY',   0 )
               CALL PAGSET ( 'HEADERFREQUENCY',  0 )
               CALL PAGSET ( 'NOSPACEFOOTER',    1 )
               CALL PAGSET ( 'FOOTERFREQUENCY', -1 )
 
               CALL PAGPUT   ( ' ' )
               CALL NICEPR_1 ( EKFILS(I), STYLE, PAGPUT )
               CALL PAGPUT   ( ' ' )
 
               CALL NSPSHC   ( HANDLE, QUIT )
 
               IF ( QUIT ) THEN
                  CALL NSPWLN ( ' ' )
                  CALL CHKOUT ( 'NSPEKC' )
                  RETURN
               END IF
 
            END IF
 
 
         END IF
 
 
      END DO
 
      CALL NSPWLN ( ' ' )
      CALL CHKOUT ( 'NSPEKC' )
      RETURN
 
 
 
      END

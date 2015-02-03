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
 
      SUBROUTINE NSPFLG ( IDLIST, N, FROM, TO, EVERY, FORMAT )
 
      IMPLICIT NONE
 
      INTEGER               IDLIST ( 0 : * )
      INTEGER               N
      INTEGER               FROM
      INTEGER               TO
      INTEGER               EVERY
      CHARACTER*(*)         FORMAT
 
 
      CHARACTER*(*)         RNAME
      PARAMETER           ( RNAME = 'NSPFLG' )
 
 
C
C     Functions to pass on to others.
C
      EXTERNAL              NSPFRP
 
C
C     Spicelib Functions
C
      LOGICAL               RETURN
      INTEGER               RTRIM
 
C
C     Local Parameters
C
      INTEGER               MOST
      PARAMETER           ( MOST   = 3  )
 
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )
 
      INTEGER               MAXLEN
      PARAMETER           ( MAXLEN = 132 )
 
C
C     Local Variables
C
      CHARACTER*(MAXLEN)    TITLE
      CHARACTER*(WDSIZE)    LR
      CHARACTER*(WDSIZE)    NAME
      CHARACTER*(WDSIZE)    SPCIAL( MOST )
 
      INTEGER               COUNT
      INTEGER               FREQF
      INTEGER               FREQH
      INTEGER               FREQT
      INTEGER               I
      INTEGER               ID    ( MOST )
      INTEGER               K
      INTEGER               LMARGE
      INTEGER               NITEMS
      INTEGER               PAGEHT
      INTEGER               PAGEWD
      INTEGER               R
      INTEGER               SHIFT
      INTEGER               SKIP
      INTEGER               SIZE  ( MOST )
      INTEGER               SPACE
      INTEGER               WIDTH ( MOST )
      INTEGER               WDTH
 
      LOGICAL               DOTITL
      LOGICAL               FOUND
      LOGICAL               JUSTR ( MOST )
      LOGICAL               PRESRV( MOST )
      LOGICAL               PRSERV
 
 
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( RNAME )
 
C
C     Determine whether we are in FLAGGED or FLAGGED PRESERVED format
C
      IF ( FORMAT .EQ. 'FLAGGED' ) THEN
         PRSERV  = .FALSE.
      ELSE
         PRSERV = .TRUE.
      END IF
 
      WDTH  = 0
 
C
C     First  set up the page parameters.
C
C     If still, here, the page is wide enough to hold everything.
C     It is time to fetch and set all of the page attributes.
C
      CALL BBGETI_1 ( 'COPY', 'PAGEWIDTH',       K, PAGEWD )
      CALL BBGETI_1 ( 'COPY', 'PAGEHEIGHT',      K, PAGEHT )
      CALL BBGETI_1 ( 'COPY', 'TITLEFREQUENCY',  K, FREQT  )
      CALL BBGETI_1 ( 'COPY', 'HEADERFREQUENCY', K, FREQH  )
 
      FREQF = -1
 
      CALL BBGETC_1 ( 'COPY', 'PAGETITLE',          K, TITLE )
      CALL BBGETC_1 ( 'COPY', 'TITLEJUSTIFICATION', K, LR    )
 
      R      = RTRIM(TITLE)
      DOTITL = TITLE .NE. ' '
 
      IF      ( LR .EQ. 'LEFT'   ) THEN
         SHIFT = 0
      ELSE IF ( LR .EQ. 'RIGHT'  ) THEN
         SHIFT = PAGEWD - R
      ELSE IF ( LR .EQ. 'CENTER' ) THEN
         SHIFT = (PAGEWD - R)/2
      END IF
 
      CALL SHIFTR ( TITLE, SHIFT, ' ', TITLE )
 
 
      CALL PAGRST
 
      CALL PAGSET ( 'TITLEFREQUENCY',  FREQT  )
      CALL PAGSET ( 'HEADERFREQUENCY', FREQH  )
      CALL PAGSET ( 'FOOTERFREQUENCY', FREQT  )
      CALL PAGSET ( 'PAGEWIDTH',       PAGEWD )
      CALL PAGSET ( 'PAGEHEIGHT',      PAGEHT )
 
      IF ( DOTITL ) THEN
 
         CALL PAGSCN ( 'TITLE'        )
         CALL PAGPUT ( ' '   )
         CALL PAGPUT ( ' '   )
         CALL PAGPUT ( TITLE )
         CALL PAGPUT ( ' '   )
 
      END IF
C
C     Now set the output section to be the body of the report.
C
      CALL PAGSCN ( 'BODY' )
 
C
C     Next set up the non-volatile portions of the report.
C
C     We shall leave no spaces between adjacent columns in the
C     table.
C
      SPACE = 0
 
      WDTH  = 0
      DO I  = 1, N
         CALL   CLGQAL ( IDLIST(I)    ,        NAME  )
         WDTH = MAX    ( WDTH,           RTRIM(NAME) )
      END DO
 
      WDTH = MIN( 32, WDTH )
C
C     Pass the width of the widest column alias onto the
C     routine NSPFRP which when given an ID and component
C
      CALL NSPFRW ( WDTH )
 
      NITEMS    = 3
 
      WIDTH(1)  =  WDTH
      WIDTH(2)  =  2
      WIDTH(3)  =  PAGEWD - WIDTH(1) - WIDTH(2) - 2*SPACE
 
      JUSTR(1)  = .FALSE.
      JUSTR(2)  = .FALSE.
      JUSTR(3)  = .FALSE.
 
      PRESRV(1) =  PRSERV
      PRESRV(2) =  PRSERV
      PRESRV(3) =  PRSERV
 
      SPCIAL(1) =  ' '
      SPCIAL(2) =  ' '
      SPCIAL(3) =  ' '
 
      SIZE  (1) =  1
      SIZE  (2) =  1
 
      LMARGE    =  1
 
C
C     Advance to the first row of the current scope of the query.
C
      DO I = 1, FROM
         CALL CLADV ( FOUND )
      END DO
 
      COUNT = FROM
 
      DO WHILE ( FOUND )
C
C        For each column, we pass the opposite of its ID,
C        zero and the ID. This will cause NSPFRP to fetch
C        the column alias, a separator and the column value.
C
         DO I = 1, N
 
            ID(1) = -IDLIST(I)
            ID(2) =  0
            ID(3) =  IDLIST(I)
 
            CALL CLNCMP ( IDLIST(I), SIZE(3) )
 
            CALL TABRPT ( NITEMS, ID,     SIZE,
     .                            WIDTH,  JUSTR,
     .                            PRESRV, SPCIAL,
     .                            LMARGE, SPACE,
     .                            NSPFRP )
 
         END DO
 
C
C        Separate lines of the report by blank lines.
C
         CALL PAGPUT ( ' ' )
 
C
C        Now fetch the next row within the scope of the current
C        query.
C
         SKIP = 0
         DO WHILE ( FOUND .AND. SKIP .LT. EVERY )
            CALL CLADV ( FOUND )
            SKIP  = SKIP  + 1
            COUNT = COUNT + 1
         END DO
 
         IF ( COUNT .GT. TO ) THEN
            FOUND = .FALSE.
         END IF
 
      END DO
 
C
C     Finally fill out the rest of the page.
C
      CALL PAGSET ( 'TITLEFREQUENCY',  0  )
      CALL PAGSET ( 'HEADERFREQUENCY', 0  )
 
      CALL PAGPUT ( ' ' )
      CALL PAGPUT ( ' ' )
 
      CALL CHKOUT ( RNAME )
      RETURN
      END
 
 
 
 
 
 

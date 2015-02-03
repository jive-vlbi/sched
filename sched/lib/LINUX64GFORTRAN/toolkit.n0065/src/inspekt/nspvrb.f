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
      SUBROUTINE NSPVRB ( IDLIST,  N, FROM, TO, EVERY )
 
      IMPLICIT NONE
      INTEGER               IDLIST ( 0 : * )
      INTEGER               N
      INTEGER               FROM
      INTEGER               TO
      INTEGER               EVERY
 
      CHARACTER*(*)         RNAME
      PARAMETER           ( RNAME = 'NSPVRB' )

C
C     Version 1.1  27-MAR-1998  W.L. Taber
C
C        Modified the original version to remove an unneeded
C        EXTERNAL declaration of NSPWLN.
C
 
C
C     Spicelib Functions
C
      LOGICAL               RETURN
      INTEGER               RTRIM
C
C     Local Parameters
C
      INTEGER               MAXLEN
      PARAMETER           ( MAXLEN = 132 )
 
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE  = 32 )
 
      CHARACTER*(*)         LINE
      PARAMETER           ( LINE = '========================'
     .                      //     '========================'
     .                      //     '========================'
     .                      //     '========================'
     .                      //     '========================'
     .                      //     '========================'
     .                      //     '========================' )
 
      CHARACTER*(*)         DASH
      PARAMETER           ( DASH = '------------------------'
     .                      //     '------------------------'
     .                      //     '------------------------'
     .                      //     '------------------------'
     .                      //     '------------------------'
     .                      //     '------------------------'
     .                      //     '------------------------' )
C     Local Variables
C
      CHARACTER*(MAXLEN)    TITLE
      CHARACTER*(MAXLEN)    MYLINE
 
      CHARACTER*(WDSIZE)    LR
      CHARACTER*(WDSIZE)    NAME
 
      INTEGER               COUNT
      INTEGER               FREQF
      INTEGER               FREQH
      INTEGER               FREQT
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               NUM
      INTEGER               PAGEHT
      INTEGER               PAGEWD
      INTEGER               R
      INTEGER               SHIFT
      INTEGER               SKIP
      INTEGER               WIDTH
      INTEGER               WDTH
 
      LOGICAL               DOTITL
      LOGICAL               FOUND
 
 
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( RNAME )
 
 
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
C
C     Set up the title section of the report.
C
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
         CALL PAGPUT ( LINE  )
         CALL PAGPUT ( ' '   )
 
      END IF
 
      CALL PAGSCN ( 'BODY' )
C
C     Find the width of the widest column name.
C
      WDTH  = 0
      DO I  = 1, N
         CALL  CLGQAL ( IDLIST(I),       NAME    )
         WDTH = MAX ( WDTH,        RTRIM(NAME)   )
      END DO
 
C
C     Advance to the first row of the current scope of the query.
C
      DO I = 1, FROM
         CALL CLADV ( FOUND )
      END DO
 
      COUNT = FROM
 
      DO WHILE ( FOUND )
 
         DO I = 1, N
C
C           Get the name of this column, spruce it up a bit
C           and send it to the page manager.
C
            CALL  CLGQAL ( IDLIST(I),       NAME    )
            MYLINE = '--- ' // NAME(1:WDTH) // ' --- '
 
            CALL PAGPUT ( MYLINE )
C
C           Now send every component of this column directly to
C           output.  No formatting.
C
            CALL CLNCMP ( IDLIST(I), NUM )
 
            DO J = 1, NUM
               CALL CLPVAL ( IDLIST(I), J,  MYLINE, WIDTH  )
               CALL PAGPUT ( MYLINE                        )
            END DO
 
         END DO
C
C        We always put a blank line after completing a row.
C
         CALL PAGPUT ( ' ' )
 
C
C        Now fetch the next row within the scope of the current
C        query.  If we find something, we output a marker to
C        indicate the beginning of another row of the E-kernel
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
 
         IF ( FOUND ) THEN
            MYLINE = DASH
            CALL PAGPUT ( MYLINE )
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
 
 
 
 
 

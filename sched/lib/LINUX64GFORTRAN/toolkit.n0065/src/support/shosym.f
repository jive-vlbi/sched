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
      SUBROUTINE SHOSYM ( TEMPLT )
 
      IMPLICIT NONE
      CHARACTER*(*)         TEMPLT
 
 
      EXTERNAL              NSPWLN
      EXTERNAL              RETSYM
 
      INTEGER               RTRIM
 
      INTEGER               MAXLEN
      PARAMETER           ( MAXLEN = 132 )
 
      CHARACTER*(MAXLEN)    MYLINE
      CHARACTER*(MAXLEN)    LINE
      CHARACTER*(MAXLEN)    MESSGE
      CHARACTER*(MAXLEN)    REST
 
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )
 
      CHARACTER*(WDSIZE)    NAME
      CHARACTER*(WDSIZE)    FRSTWD
 
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 2000 )
 
      CHARACTER*(LNSIZE)    DEF
      CHARACTER*(LNSIZE)    VALUE
 
      LOGICAL               PRESRV ( 3 )
      INTEGER               LMARGE
      INTEGER               SPACE  ( 3 )
      CHARACTER*(1)         SPCIAL ( 3 )
 
      LOGICAL               JUSTR  ( 3 )
 
      INTEGER               WIDTH  ( 3 )
      INTEGER               SIZE   ( 3 )
      INTEGER               ITEM   ( 3 )
 
      INTEGER               NCOL
      INTEGER               I
      INTEGER               N
      INTEGER               R
      INTEGER               PAGEWD
 
      CHARACTER*(WDSIZE)    MARGIN
      LOGICAL               TRAN
 
 
      R = RTRIM(TEMPLT)
 
      CALL SYMPAT ( TEMPLT(1:R) )
      CALL SYMGET ( NAME, DEF )
 
 
 
 
 
 
 
 
      CALL NSPMRG ( MARGIN )
 
      IF ( NAME .EQ. ' ' ) THEN
 
         MESSGE = 'There are no symbols that match the template "#".'
         CALL REPMC   ( MESSGE, '#', TEMPLT(1:R), MESSGE )
         CALL NICEPR_1( MESSGE, MARGIN, NSPWLN )
         RETURN
 
      END IF
 
C
C     If still here there are some matching symbols.  Set up the
C     standard defaults.
C
       LINE = '========================'
     .//      '========================'
     .//      '========================'
     .//      '========================'
     .//      '========================'
     .//      '========================'
     .//      '========================'
 
      PRESRV(1) = .TRUE.
      PRESRV(2) = .TRUE.
      PRESRV(3) = .TRUE.
 
      LMARGE    =  1
 
      SPACE (1) =  2
      SPACE (2) =  2
      SPACE (3) =  2
 
      SPCIAL(1) = ' '
      SPCIAL(2) = ' '
      SPCIAL(3) = ' '
 
      JUSTR (1) = .FALSE.
      JUSTR (2) = .FALSE.
      JUSTR (3) = .FALSE.
C
C     Get the width of the page and based upon that determine
C     the basic table style that will be used to display the
C     symbol definition.
C
      CALL NSPGLR   ( N, PAGEWD )
 
 
      WIDTH(1) = 14
      WIDTH(2) = 30
      WIDTH(3) = 30
 
      SIZE (1) = 1
      SIZE (2) = 1
      SIZE (3) = 1
 
      ITEM (1) = 1
      ITEM (2) = 2
      ITEM (3) = 3
 
 
      NCOL    = 3
C
C     Adjust all of the columns
C
      DO I = 1, NCOL
         WIDTH(I) = (WIDTH(I)*PAGEWD)/80
      END DO
 
      PAGEWD = 0
 
      DO I = 1, NCOL
         PAGEWD = WIDTH(I) + SPACE(I) + PAGEWD
      END DO
 
      PAGEWD = PAGEWD - SPACE(NCOL)
 
 
 
      CALL NSPWLN ( ' ' )
      CALL NSPWLN ( 'Symbols Matching Request: ' )
      CALL NSPWLN ( ' ' )
 
      CALL PAGRST
      CALL PAGSET ( 'PAGEWIDTH', PAGEWD )
      CALL PAGSCN ( 'BODY' )
 
      CALL SETSYM ( 'Symbol Name', 'Definition', 'Expanded Value' )
      CALL TABRPT ( NCOL, ITEM,   SIZE,
     .                    WIDTH,  JUSTR,
     .                    PRESRV, SPCIAL,
     .                    LMARGE, SPACE,
     .                    RETSYM        )
 
      MYLINE = LINE(1:PAGEWD)
 
      CALL NSPWLN ( MYLINE )
 
 
      DO WHILE ( NAME .NE. ' ' )
C
C        Expand this symbol until there's nothing left to do.
C
         VALUE =  DEF
         TRAN  = .TRUE.
 
         DO WHILE ( TRAN )
            CALL NEXTWD ( DEF,    FRSTWD, REST )
            CALL UCASE  ( FRSTWD, FRSTWD       )
 
            IF (        FRSTWD .NE. 'DEFINE'
     .            .AND. FRSTWD .NE. 'UNDEFINE' ) THEN
               CALL STRAN ( VALUE, VALUE, TRAN )
            ELSE
               TRAN = .FALSE.
            END IF
         END DO
 
         CALL SETSYM ( NAME, DEF,    VALUE )
         CALL TABRPT ( NCOL, ITEM,   SIZE,
     .                        WIDTH,  JUSTR,
     .                        PRESRV, SPCIAL,
     .                        LMARGE, SPACE,
     .                        RETSYM        )
 
         CALL SYMGET ( NAME, DEF )
 
      END DO
 
      CALL NSPWLN ( ' ' )
      RETURN
      END

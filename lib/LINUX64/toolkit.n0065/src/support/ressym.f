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
      SUBROUTINE RESSYM ( INPUT, OUTPUT )
 
      IMPLICIT NONE
 
      LOGICAL               FAILED
      INTEGER               LASTNB
 
      CHARACTER*(*)         INPUT
      CHARACTER*(*)         OUTPUT
 
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 255 )
 
      CHARACTER*(LNSIZE)    SUBSTR
 
 
      CHARACTER*(1)         TAB
      CHARACTER*(1)         SPACE
      CHARACTER*(1)         EQUOTE
 
      INTEGER               I
      INTEGER               LOC
      INTEGER               R
      INTEGER               E
 
      LOGICAL               CHANGE
      LOGICAL               TRAN1
      LOGICAL               TRAN2
 
 
 
      CALL CHKIN ( 'RESSYM' )
 
      TAB    = CHAR(9)
      SPACE  = ' '
 
      CALL GETEQ ( EQUOTE )
 
      CALL REPLCH ( INPUT, TAB, SPACE, OUTPUT )
 
      CALL PRTRAP ( OUTPUT, CHANGE )
 
C
C     Now we just loop until all translations have
C     been performed.  We do:
C
C        1) symbol resolution
C        2) query resolution
C        3) tab removal
C
      DO WHILE ( CHANGE )
 
         CHANGE = .FALSE.
         TRAN1  = .TRUE.
         TRAN2  = .TRUE.
C
C        First we resolve all symbols.  After each pass we check
C        that we have not created a command that must be trapped.
C
         DO WHILE ( TRAN1 .AND. TRAN2 )
 
            CALL STRAN  ( OUTPUT, OUTPUT, TRAN1 )
            CALL PRTRAP ( OUTPUT,         TRAN2        )
C
C           Determine whether or not more changes are possible
C           at this point.
C
            CHANGE =          ( CHANGE .OR. TRAN1 )
     .               .AND. TRAN2
     .               .AND. .NOT. FAILED()
 
         END DO
C
C        If we don't have any errors we take a stab at replacing
C        all queries.  Note that queries can not result in changing
C        anything that isn't a query so we don't have to trap
C        inside the loop.  Note that this means you can't have
C        a command like DEFINE? SYMBOL? VALUE? and just replace
C        the first two queries.  You've got to do them all.  If
C        you want a symbol to have a query you must do it this
C        way:  DEFINE SYMBOL  QUERY?  That way the queries won't
C        get resolve too soon.
C
C        Note:  This can easily be changed so that if a query
C        introduces a symbol, we immediately loop back to the
C        symbol resolution branch.  Simply change the DO WHILE
C        loop below to an IF.  The "loop" will then terminate
C        after one execution leaving any remaining queries
C        untouched until the next pass through the loop.
C
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'RESSYM' )
            RETURN
         END IF
 
         TRAN1 = .NOT. FAILED()
 
 
         DO WHILE ( TRAN1 )
 
            CALL QTRAN  ( OUTPUT, OUTPUT, TRAN1 )
            CALL REPLCH ( OUTPUT, TAB, SPACE, OUTPUT )
 
            CHANGE = CHANGE .OR. TRAN1
 
         END DO
 
         CALL PRTRAP ( OUTPUT, TRAN2 )
         CHANGE = CHANGE .AND. TRAN2
 
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'RESSYM' )
            RETURN
         END IF
 
      END DO
 
      IF ( TRAN2 ) THEN
C
C        We remove the special markers that may have been present to
C        protect symbol or query resolution.
C
         I = 1
         CALL NTHUQW ( OUTPUT, I, ' ', SUBSTR, LOC )
 
         DO WHILE ( LOC .GT. 0 )
 
            R = LASTNB ( SUBSTR ) - 1
            E = LOC + R
 
            CALL REPLCH( OUTPUT(LOC:E), EQUOTE, SPACE, OUTPUT(LOC:E) )
 
            I = I+1
            CALL NTHUQW ( OUTPUT, I, ' ', SUBSTR, LOC )
 
         END DO
 
      END IF
C
C     Finally, left justify the commmand.
C
      CALL LJUST ( OUTPUT, OUTPUT )
 
      CALL CHKOUT ( 'RESSYM' )
      RETURN
      END

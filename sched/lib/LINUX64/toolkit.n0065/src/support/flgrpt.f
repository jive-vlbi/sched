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
      SUBROUTINE FLGRPT ( NITEMS, NAMES, VALUES, MYIO )
C
C     This routine takes an array of names and an array of associated
C     value strings and produces a flagged set of outputs.  This
C     routine signals no errors.
C
      IMPLICIT NONE
      INTEGER               NITEMS
      CHARACTER*(*)         NAMES ( * )
      CHARACTER*(*)         VALUES( * )
C
C     The routine MYIO is a routine that is supplied by the user
C     that can handle io of text lines without any action by the
C     routine that calls it.
C
C$ Version
C
C     Inspekt Routine version 2.0.0, 7-APR-1995 (WLT)
C
C        Unused variables LEFT and RIGHT were removed.
C
      EXTERNAL              MYIO
 
C
C     Spicelib functions
C
      INTEGER               RTRIM
      LOGICAL               RETURN
 
 
      INTEGER               STYLEN
      PARAMETER           ( STYLEN = 200 )
 
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               L
      INTEGER               WIDTH
 
      LOGICAL               FREE ( 0 : 128 )
 
      CHARACTER*(1)         HARD
      CHARACTER*(1)         LETTER
      CHARACTER*(STYLEN)    STYLE
 
 
 
 
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'FLGRPT' )
C
C     First find the widest of the names:
C
      WIDTH = 0
 
      DO I = 1, NITEMS
 
         IF ( RTRIM(NAMES(I)) .GT. WIDTH ) THEN
            WIDTH = RTRIM(NAMES(I))
         END IF
 
      END DO
C
C     Now for each of the NAME/VALUE pairs construct a style
C     string using NAMES and run the VALUES through NICEPR_1.
C
      DO I = 1, NITEMS
C
C        First we need to find a character that is not used
C        in the NAMES(I)/VALUES(I) pair.  We will use this as
C        a hardspace in our style string.
C
         DO J = 33, 127
            FREE(J) = .TRUE.
         END DO
 
         DO J = 1, WIDTH
            FREE( ICHAR(NAMES(I)(J:J))  ) = .FALSE.
         END DO
 
         DO J = 1, LEN(VALUES(1))
            FREE( ICHAR(VALUES(I)(J:J)) ) = .FALSE.
         END DO
 
         J = 33
         DO WHILE ( .NOT. FREE(J) .AND. J .LT. 127 )
            J = J + 1
         END DO
 
         HARD = CHAR(J)
 
 
C
C        Set up the style we are going to use for this
C        value
C
         CALL NSPMRG ( STYLE )
 
         CALL SUFFIX ( 'HARDSPACE', 1, STYLE )
         CALL SUFFIX (  HARD,       1, STYLE )
         CALL SUFFIX ( 'FLAG',      1, STYLE )
 
         L = RTRIM(STYLE) + 2
 
         DO K = 1, WIDTH
 
            LETTER = NAMES(I)(K:K)
 
            IF (LETTER .EQ. ' ' ) THEN
               STYLE(L:L) = HARD
            ELSE
               STYLE(L:L) = LETTER
            END IF
 
            L = L + 1
 
         END DO
 
         STYLE(L:L) = ':'
         L          =  L + 1
         STYLE(L:L) =  HARD
 
C
C        Ok.  Now just ship the stuff to the output routines.
C
         IF (       ( NAMES (I) .EQ. ' ' )
     .        .AND. ( VALUES(I) .EQ. ' ' ) ) THEN
 
             STYLE(L-1:L-1) = HARD
             CALL NICEPR_1  ( HARD,    STYLE(1:L), MYIO )
 
         ELSE IF ( VALUES(I) .EQ. ' ' ) THEN
            STYLE(L-1:L-1) = HARD
            CALL NICEPR_1  ( HARD,     STYLE(1:L), MYIO )
         ELSE
            CALL NICEPR_1 ( VALUES(I), STYLE(1:L), MYIO )
         END IF
 
 
      END DO
 
      CALL CHKOUT ( 'FLGRPT' )
      RETURN
      END

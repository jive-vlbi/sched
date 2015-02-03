C$Procedure      CMREDO ( COMMND loop trap )
 
      SUBROUTINE CMREDO ( COMMND, FROM, TRAP )
 
C$ Abstract
C
C    This routine examines COMMND and checks to see if it
C    should be sent to the COMMND loop stuff so that it
C    can be re-evaluated.
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
C     None.
C
C$ Keywords
C
C     COMMAND LOOP
C
C$ Declarations
 
      IMPLICIT NONE
      CHARACTER*(*)         COMMND
      INTEGER               FROM
      LOGICAL               TRAP
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     COMMND    I   A COMMND string to be checked for special syntax
C     TRAP       O   Indicates whether the string has special form
C
C$ Detailed_Input
C
C     COMMND    is a string that represents some COMMND.
C
C
C$ Detailed_Output
C
C     TRAP       is a logical idicating whether the string was special
C                and was put on the COMMND buffer.
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Particulars
C
C     This routine examines the input COMMND to see if it is one
C     of the following.
C
C     EDIT number
C     RECALL ALL
C     RECALL number
C     START  word
C     STOP
C     EXIT
C
C
C$ Examples
C
C     Later,
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C       W.L. Taber      (JPL)
C
C$ Literature_References
C
C       None.
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 23-AUG-1995 (WLT)
C
C        Updated the routine so that EDIT *, DO * and RECALL *
C        are trapped.
C
C
C-&
 
C$ Index_Entries
C
C     «We need a permuted index entry»
C
C-&
      INTEGER               RTRIM
      LOGICAL               M2WMCH
 
 
      INTEGER               NONE
      PARAMETER           ( NONE   = 0 )
 
      INTEGER               COMBUF
      PARAMETER           ( COMBUF = NONE   + 1 )
 
      INTEGER               KEYBRD
      PARAMETER           ( KEYBRD = COMBUF + 1 )
 
      INTEGER               INPFIL
      PARAMETER           ( INPFIL = KEYBRD + 1 )
 
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )
 
      CHARACTER*(WDSIZE)    START
      CHARACTER*(WDSIZE)    EXIT
      CHARACTER*(WDSIZE)    STOP
      CHARACTER*(WDSIZE)    FRSTWD
      CHARACTER*(WDSIZE)    SCNDWD
 
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 300 )
 
      CHARACTER*(LNSIZE)    REST
 
      INTEGER               B1
      INTEGER               B2
      INTEGER               E1
      INTEGER               E2
 
      LOGICAL               FIRST
      SAVE
 
      DATA                  FIRST /.TRUE./
C
 
      IF ( FIRST ) THEN
         CALL TRNLAT ( 'STOP',      STOP   )
         CALL TRNLAT ( 'EXIT',      EXIT   )
         CALL TRNLAT ( 'START',     START  )
 
         FIRST = .FALSE.
      END IF
 
 
 
      CALL NEXTWD ( COMMND, FRSTWD, REST )
      CALL NEXTWD ( REST,   SCNDWD, REST )
 
      CALL UCASE  ( FRSTWD, FRSTWD )
      CALL UCASE  ( SCNDWD, SCNDWD )
 
      B1 = 1
      B2 = 1
      E1 = RTRIM  ( FRSTWD )
      E2 = RTRIM  ( SCNDWD )
 
 
      IF ( REST .NE. ' ' ) THEN
         TRAP = .FALSE.
         RETURN
      END IF
 
      IF ( FRSTWD .EQ. ' ' ) THEN
         TRAP = .FALSE.
         RETURN
      END IF
 
 
      IF ( FRSTWD .EQ. START ) THEN
 
         TRAP = .TRUE.
         CALL PUTCOM ( COMMND, FROM )
         RETURN
 
      ELSE IF (       FRSTWD .EQ. EXIT
     .          .AND. SCNDWD .EQ. ' '
     .          .AND. FROM   .NE. KEYBRD ) THEN
 
         TRAP = .TRUE.
         CALL PUTCOM ( COMMND, FROM )
         RETURN
 
      ELSE IF (       FRSTWD .EQ. STOP
     .          .AND. SCNDWD .EQ. ' '
     .          .AND. FROM   .NE. KEYBRD ) THEN
 
         TRAP = .TRUE.
         CALL PUTCOM ( COMMND, FROM )
         RETURN
 
 
      ELSE IF ( FROM .NE. KEYBRD ) THEN
 
         TRAP = .FALSE.
         RETURN
 
      ELSE IF (       SCNDWD .EQ. ' '
     .          .AND. .NOT. M2WMCH ( FRSTWD, B1, E1, 'RECALL' ) ) THEN
 
         TRAP = .FALSE.
         RETURN
 
      ELSE IF (      M2WMCH ( FRSTWD, B1, E1, 'RECALL'     )
     .         .AND. M2WMCH ( SCNDWD, B2, E2, '@int(1:20)' ) ) THEN
 
         TRAP = .TRUE.
         CALL PUTCOM ( COMMND, FROM )
 
      ELSE IF (      M2WMCH ( FRSTWD, B1, E1, 'RECALL' )
     .         .AND. M2WMCH ( SCNDWD, B2, E2, 'ALL'    ) ) THEN
 
         TRAP = .TRUE.
         CALL PUTCOM ( COMMND, FROM )
 
      ELSE IF (      M2WMCH ( FRSTWD, B1, E1, 'EDIT'     )
     .         .AND. M2WMCH ( SCNDWD, B2, E2, '@int(1:20)' ) ) THEN
 
         TRAP = .TRUE.
         CALL PUTCOM ( COMMND, FROM )
 
      ELSE IF (      M2WMCH ( FRSTWD, B1, E1, 'DO'     )
     .         .AND. M2WMCH ( SCNDWD, B2, E2, '@int(1:20)' ) ) THEN
 
         TRAP = .TRUE.
         CALL PUTCOM ( COMMND, FROM )
 
      ELSE IF (      M2WMCH ( FRSTWD, B1, E1, 'RECALL' )
     .         .AND. SCNDWD .NE. ' '
     .         .AND. REST   .EQ. ' ' ) THEN
 
         TRAP = .TRUE.
         CALL PUTCOM ( COMMND, FROM )
 
      ELSE IF (      M2WMCH ( FRSTWD, B1, E1, 'EDIT'     )
     .         .AND. SCNDWD .NE. ' '
     .         .AND. REST   .EQ. ' ' ) THEN
 
         TRAP = .TRUE.
         CALL PUTCOM ( COMMND, FROM )
 
      ELSE IF (      M2WMCH ( FRSTWD, B1, E1, 'DO'     )
     .         .AND. SCNDWD .NE. ' '
     .         .AND. REST   .EQ. ' ' ) THEN
 
         TRAP = .TRUE.
         CALL PUTCOM ( COMMND, FROM )
 
      ELSE
 
         TRAP = .FALSE.
 
      END IF
 
 
      RETURN
      END

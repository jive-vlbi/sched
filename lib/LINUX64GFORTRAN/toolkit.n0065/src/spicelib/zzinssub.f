C$Procedure      ZZINSSUB ( Insert a substring )
 
      SUBROUTINE ZZINSSUB ( IN, SUB, LOC, OUT )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Insert a substring into a character string at a specified
C     location.
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
C     ASSIGNMENT
C     CHARACTER
C     STRING
C
C$ Declarations
 
      CHARACTER*(*)    IN
      CHARACTER*(*)    SUB
      INTEGER          LOC
      CHARACTER*(*)    OUT
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     IN         I   Input string.
C     SUB        I   Substring to be inserted.
C     LOC        I   Position at which substring is to be inserted.
C     OUT        O   Output string.
C
C$ Detailed_Input
C
C     IN          is an input character string, into which a substring
C                 is to be inserted.
C
C     SUB         is the substring to be inserted. Leading and trailing
C                 blanks are significant.
C
C     LOC         is the position in the input string at which the
C                 substring is to be inserted. To append to the
C                 string, set LOC equal to LEN(IN) + 1.
C
C$ Detailed_Output
C
C     OUT         is the output string. This is equivalent to the
C                 string that would be created by the concatenation
C
C                    OUT = IN(1:LOC-1) // SUB // IN(LOC: )
C
C                 If the output string is too long, it is truncated
C                 on the right.
C
C                 OUT may overwrite IN.  OUT may NOT overwrite SUB.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error Free.
C
C     1) If LOC is less than 1 it is treateds as having value 1.
C
C     2) If LOC is greater than LEN(IN) + 1, it is treated as if
C        it had value LEN(IN) + 1.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Shift the end of the input string, beginning with LOC, to the
C     right, leaving space for the substring. Then insert the substring
C     into the vacated space in the middle of the string. This has
C     the same effect as the concatenation
C
C        OUT = IN(1:LOC-1) // SUB // IN(LOC: )
C
C     Because this operation is not standard for strings of length (*),
C     this routine does not use concatenation.
C
C     This private routine is just a copy of the SPICE routine INSSUB
C     with "reasonable" choices made for out of bounds errors.
C
C$ Examples
C
C     The following examples illustrate the use of ZZINSSUB.
C
C        IN                SUB      LOC    OUT
C        ----------------- -------  ---    ------------------------
C        'ABCDEFGHIJ'      ' YXZ '    3    'AB XYZ CDEFGHIJ'
C        'The rabbit'      'best '    5    'The best rabbit'
C        ' other woman'    'The'      1    'The other woman'
C        'An Apple a day'  ' keeps'  15    'An Apple a day keeps'
C        'Apple a day'     'An '      0    'An Apple a day'
C        'Apple a day'     'An '     -3    'An Apple a day'
C        'An Apple a day'  ' keeps'  16    'An Apple a day keeps'
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     H.A. Neilan     (JPL)
C     I.M. Underwood  (JPL)
C     W.L. Taber      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 12-AUG-1996 (WLT)
C
C        Adapted from the SPICELIB routine INSSUB to be error free.
C
C
C-&
 
C$ Index_Entries
C
C     insert a substring
C
C-&
 
 
 
C$ Revisions
C
C-    SPICELIB Version 1.1.0, 24-OCT-1994 (NJB)
C
C        Bug fix:  case where insertion location follows end of
C        input string is now handled correctly.  Formerly, an
C        out-of-range substring bound violation was incurred in this
C        case.
C
C        Bug fix:  use of SHIFTC routine in old implementation
C        resulted in output string being truncated at length
C        LEN(IN), which is not consistent with the routine's
C        specification.
C
C        Now does discovery check-in.  Header sections re-arranged.
C        Some clean-up of header format done.
C
C-    Beta Version 2.0.0, 4-JAN-1989 (HAN)
C
C        If the location at which the substring is to be inserted is
C        not in the interval [1, LEN(IN)+1], an error is signalled.
C        Locations not within that interval refer to non-existent
C        characters positions. (To append to the string, set the
C        location equal to LEN(IN)+1.)
C
C-&
 
 
C
C     Local Variables
C
      CHARACTER*(1)         CHR
 
      INTEGER               FROM
      INTEGER               I
      INTEGER               INLEN
      INTEGER               MYLOC
      INTEGER               NMOVE
      INTEGER               OUTLEN
      INTEGER               SUBEND
      INTEGER               SUBLEN
      INTEGER               TO
 
      LOGICAL               SAME
 
C
C     Note to the careful reader:  in order to scrupulously avoid
C     non-standard assignments of characters from a substring of IN to
C     an overlapping substring of OUT, in the case where IN and OUT
C     refer to the same memory, we'll test whether the output and
C     input strings are the same.  If they're the same, we can avoid
C     various assignments that could cause trouble if IN and OUT
C     actually refer to the same memory.  This test has little effect on
C     performance, and allows the author to sleep more soundly at night.
C
C     Capture the lengths of the input, output, and substitution
C     strings.
C
      INLEN   =  LEN ( IN  )
      OUTLEN  =  LEN ( OUT )
      SUBLEN  =  LEN ( SUB )
      MYLOC   =  MIN ( INLEN + 1, MAX ( 1, LOC ) )
 
 
C
C     If the insertion occurs after the end of the output string,
C     just return the original string.  Don't do the assignment if
C     the output and input strings have equal values; the assignment
C     is not needed in this case and could cause a run-time error if
C     OUT and IN refer to the same memory.
C
      SAME  =  OUT .EQ. IN
 
      IF ( MYLOC .GT. OUTLEN ) THEN
 
         IF ( .NOT. SAME ) THEN
            OUT = IN
         END IF
 
         RETURN
 
      END IF
 
 
C
C     At this point, we're guaranteed that
C
C        MYLOC  <  OUTLEN
C             -
C
C        MYLOC  <  INLEN + 1
C             -
C
C        MYLOC  >  0
C
C
C     The first part of the input string is copied without change
C     to the output string, if this first part is non-empty.
C
      IF ( MYLOC .GT. 1 ) THEN
C
C        Again, do the assignment only if it's required.
C
         IF ( .NOT. SAME ) THEN
            OUT(1 : MYLOC-1) = IN
         END IF
 
      END IF
 
C
C     The part following the new substring is shifted into place, if
C     there's both something to move and a place to put it.  Move the
C     rightmost characters first.
C
      SUBEND  =  MYLOC - 1 + SUBLEN
 
 
      IF (        ( MYLOC  .LE. INLEN  )
     .      .AND. ( SUBEND .LT. OUTLEN )  ) THEN
 
 
         NMOVE  =  MIN (  OUTLEN-SUBEND, INLEN-MYLOC+1  )
 
         DO I   =  NMOVE,  1,  -1
 
            FROM         =  MYLOC  + I - 1
            TO           =  SUBEND + I
            CHR          =  IN(FROM:FROM)
            OUT (TO:TO)  =  CHR
 
         END DO
 
      END IF
 
C
C     And the new word is dropped into the middle.
C
      OUT( MYLOC : MIN(SUBEND,OUTLEN) )  =  SUB
 
C
C     Blank-pad the output string if necessary.
C
      IF ( OUTLEN .GT. INLEN+SUBLEN ) THEN
         OUT( INLEN+SUBLEN+1 : ) = ' '
      END IF
 
      RETURN
      END

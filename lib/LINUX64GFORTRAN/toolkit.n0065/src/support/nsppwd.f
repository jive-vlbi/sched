C$Procedure      NSPPWD ( NSP --- Page width)
 
 
      SUBROUTINE NSPPWD ( MARGIN, LEFT, RIGHT )
 
C$ Abstract
C
C    This routine is an umbrella routine used to cover the
C    three entry points used for setting and retrieving
C    page width settings.
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
C     PAGE
C
C$ Declarations
 
      IMPLICIT NONE
      CHARACTER*(*)         MARGIN
      INTEGER               LEFT
      INTEGER               RIGHT
 
      INTEGER               MXPGWD
      PARAMETER           ( MXPGWD = 131 )
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     MARGIN     O   A "NICEIO" style string for left and right margins
C     LEFT      I/O  The column to be used for the left margin.
C     RIGHT     I/O  The column to be used for the right margin.
C     MXPGWD     P   Maximum allowed page width.
C
C     The function returns
C
C$ Detailed_Input
C
C     LEFT      is an integer that sets the left margin.
C
C     RIGHT     is an integer that sets the right margin.
C
C
C$ Detailed_Output
C
C     LEFT      is the current left margin.
C
C     RIGHT     is the current right margin.
C
C$ Parameters
C
C     MXPGWD    is the maximum allowed page width.  This is here
C               so that this routine can be error free.  It is
C               possible that on some systems this could be made
C               substantially larger.
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
C     This routine is an umbrella for the three entry points
C
C     NSPMRG   ---  returns a NICEIO/NICEBT/NICEPR style string
C                   of the form 'LEFT number1 RIGHT number2 '
C                   where number1 and number2 give the left and
C                   right margins to use when creating NICEIO
C                   style output.
C
C                   Other style items may be added to this string
C                   for use in creating output.
C
C     NSPSLR   ---  sets the left and right margins to be used
C                   when creating a style string.  Note there are
C                   no erroneous inputs.  Values are forced into
C                   a "reasonable" range.
C
C     NSPGLR   ---  get the current left and right margins.
C
C$ Examples
C
C     To set the margins to 1 to 72 make the following call:
C
C        CALL NSPSLR ( 1, 72 )
C
C     To get back a NICEPR string that will be used for setting
C     the style of page output.
C
C        CALL NSPMRG ( MARGIN )
C
C     To get the numeric values (so you don't have to parse MARGIN)
C     make the following call:
C
C        CALL NSPGLR ( LEFT, RIGHT )
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
C-    Command Loop Version 1.0.0, 1-AUG-1995 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Set or get command loop page margins.
C
C-&
 
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 16 )
 
      CHARACTER*(WDSIZE)    STRLFT
      CHARACTER*(WDSIZE)    STRRHT
 
      INTEGER               MYLEFT
      INTEGER               MYRGHT
 
 
      SAVE
 
      DATA                  MYLEFT / 1  /
      DATA                  MYRGHT / 80 /
 
      RETURN
 
      ENTRY NSPMRG ( MARGIN )
C
C        Return the current margins to be used by the NICEIO and NICEPR
C        routines.
C
 
         CALL INTSTR (  MYLEFT,    STRLFT )
         CALL INTSTR (  MYRGHT,    STRRHT )
 
         MARGIN      = 'LEFT'
 
         CALL SUFFIX (  STRLFT, 1, MARGIN )
         CALL SUFFIX ( 'RIGHT', 1, MARGIN )
         CALL SUFFIX (  STRRHT, 1, MARGIN )
 
         RETURN
 
 
      ENTRY NSPSLR ( LEFT, RIGHT )
 
C
C        Set the left and right margins to be used when creating
C        margin style strings in the entry point above.  Note
C        we force these to be reasonable.  No error checking is
C        done.
C
         MYLEFT = MAX( 1,      MIN( LEFT, RIGHT, MXPGWD - 2 ) )
         MYRGHT = MIN( MXPGWD, MAX( LEFT, RIGHT, MYLEFT + 2 ) )
 
         RETURN
 
 
      ENTRY NSPGLR ( LEFT, RIGHT )
C
C        Get the left and right margins that are currently
C        being used.
C
 
         LEFT  = MYLEFT
         RIGHT = MYRGHT
 
         RETURN
 
      END

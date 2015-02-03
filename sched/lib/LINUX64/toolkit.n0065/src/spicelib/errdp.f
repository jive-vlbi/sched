C$Procedure      ERRDP  ( Insert D.P. Number into Error Message Text )
 
      SUBROUTINE ERRDP ( MARKER, DPNUM )
 
C$ Abstract
C
C     Substitute a double precision number for the first occurrence of
C     a marker found in the current long error message.
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
C     ERROR
C
C$ Keywords
C
C     ERROR, CONVERSION
C
C$ Declarations
 
      INCLUDE 'errhnd.inc'
 
      CHARACTER*(*)        MARKER
      DOUBLE PRECISION     DPNUM
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     MARKER     I   A substring of the error message to be replaced.
C     DPNUM      I   The d.p. number to substitute for MARKER.
C
C$ Detailed_Input
C
C
C     MARKER     is a character string which marks a position in
C                the long error message where a character string
C                representing an double precision number is to be
C                substituted.  Leading and trailing blanks in MARKER
C                are not significant.
C
C                Case IS significant;  'XX' is considered to be
C                a different marker from 'xx'.
C
C     DPNUM      is an double precision number whose character
C                representation will be substituted for the first
C                occurrence of MARKER in the long error message.
C                This occurrence of the substring indicated by MARKER
C                will be removed, and replaced by a character string,
C                with no leading or trailing blanks, representing
C                DPNUM.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     LMSGLN  is the maximum length of the long error message.  See
C             the include file errhnd.inc for the value of LMSGLN.
C
C$ Exceptions
C
C     This routine does not detect any errors.
C
C     However, this routine is part of the SPICELIB error
C     handling mechanism.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The effect of this routine is to update the current long
C     error message.  If no marker is found, (e.g., in the
C     case that the long error message is blank), the routine
C     has no effect.  If multiple instances of the marker
C     designated by MARKER are found, only the first one is
C     replaced.
C
C     If the character string resulting from the substitution
C     exceeds the maximum length of the long error message, the
C     characters on the right are lost.  No error is signalled.
C
C     This routine has no effect if changes to the long message
C     are not allowed.
C
C$ Examples
C
C
C      1.   In this example, the marker is:   #
C
C
C           The current long error message is:
C
C              'Invalid operation value.  The value was #'.
C
C
C           After the call,
C
C
C              CALL ERRDP ( '#',  5.D0  )
C
C           The long error message becomes:
C
C           'Invalid operation value.  The value was 5.0'.
C
C
C
C
C      2.   In this example, the marker is:   XX
C
C
C           The current long error message is:
C
C              'Left endpoint exceeded right endpoint.  The left'//
C              'endpoint was:  XX.  The right endpoint was:  XX.'
C
C
C           After the call,
C
C              CALL ERRDP ( 'XX',  5.D0  )
C
C           The long error message becomes:
C
C              'Left endpoint exceeded right endpoint.  The left'//
C              'endpoint was:  5.0.  The right endpoint was:  XX.'
C
C
C$ Restrictions
C
C     The caller must ensure that the message length, after sub-
C     stitution is performed, doesn't exceed LMSGLN characters.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.2.1, 08-JAN-2014 (BVS)
C
C        Fixed header example (5.0 -> 5.D0).
C
C-    SPICELIB Version 2.2.0, 29-JUL-2005 (NJB)
C
C        Bug fix:  increased length of internal string DPSTRG to 
C        handle 3-digit exponents.
C
C-    SPICELIB Version 2.1.0, 29-JUL-1997 (NJB)
C
C        Bug fix:  extraneous leading blank has been removed from 
C        numeric string substituted for marker.
C
C        Maximum length of the long error message is now represented
C        by the parameter LMSGLN.  Miscellaneous format changes to the
C        header, code and in-line comments were made.  
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990  (NJB)
C
C-&
 
C$ Index_Entries
C
C     insert d.p. number into error message text
C
C-&
 
 
C$ Revisions
C
C-    SPICELIB Version 2.1.0, 29-JUL-1997 (NJB)
C
C        Bug fix:  extraneous leading blank has been removed from 
C        numeric string substituted for marker.
C
C        Maximum length of the long error message is now represented
C        by the parameter LMSGLN.  Miscellaneous format changes to the
C        header, code and in-line comments were made.  
C
C-&
 
 
C
C     SPICELIB functions
C
      INTEGER               LASTNB
      INTEGER               FRSTNB
      LOGICAL               ALLOWD
 
C
C     Local Variables:
C
      CHARACTER*(LMSGLN)    LNGMSG
      CHARACTER*(LMSGLN)    TMPMSG
 
C
C     Length of DPSTRG is number of significant digits plus 7
C     (see DPSTR header)
C
      CHARACTER*(21)        DPSTRG
 
      INTEGER               STRPOS
 
 
C
C     Executable Code:
C
C
C     Changes to the long error message have to be allowed, or we
C     do nothing.
C
      IF ( .NOT. ALLOWD() ) THEN
         RETURN
      END IF
 
C
C     MARKER has to have some non-blank characters, or we do nothing.
C
      IF (  LASTNB (MARKER)  .EQ.  0  )   THEN
         RETURN
      END IF
 
C
C     Get a copy of the current long error message.  Convert DPNUM
C     to a character string.  Ask for 14 significant digits in
C     string.
C
      CALL GETLMS ( LNGMSG )
      CALL DPSTR  ( DPNUM, 14, DPSTRG )
      CALL LJUST  ( DPSTRG,    DPSTRG )
C
C     Locate the leftmost occurrence of MARKER, if there is one
C     (ignoring leading and trailing blanks):
C
      STRPOS = INDEX (  LNGMSG,
     .                  MARKER ( FRSTNB(MARKER):LASTNB(MARKER) )  )
 
      IF  ( STRPOS .EQ. 0 ) THEN
      
         RETURN
         
      ELSE
C
C        We put together TMPMSG, a copy of LNGMSG with MARKER
C        replaced by the character representation of DPNUM:
C
         IF ( STRPOS .GT. 1 ) THEN
 
            IF (    ( STRPOS + LASTNB(MARKER) - FRSTNB(MARKER) ) 
     .          .LT.  LASTNB ( LNGMSG )                          ) THEN
C
C              There's more of the long message after the marker...
C
               TMPMSG =
     .         LNGMSG ( :STRPOS - 1     )                             //
     .         DPSTRG ( :LASTNB(DPSTRG) )                             //
     .         LNGMSG ( STRPOS + LASTNB(MARKER) - FRSTNB(MARKER) + 1: )
 
            ELSE
 
               TMPMSG =  LNGMSG ( :STRPOS - 1     )                   //
     .                   DPSTRG ( :LASTNB(DPSTRG) )
 
            END IF
 
 
         ELSE
C
C           We're starting with the d.p. number, so we know it fits...
C
            IF (      ( LASTNB(MARKER) - FRSTNB(MARKER) )
     .           .LT.   LASTNB(LNGMSG)                    ) THEN
C
C              There's more of the long message after the marker...
C
               TMPMSG = DPSTRG ( :LASTNB(DPSTRG) )                    //
     .                  LNGMSG ( STRPOS + LASTNB(MARKER)
     .                                  - FRSTNB(MARKER)  + 1 :     )
            ELSE
C
C              The marker's the whole string:
C
               TMPMSG = DPSTRG
 
            END IF
 
         END IF
 
C
C        Update the long message:
C
         CALL PUTLMS ( TMPMSG )
 
      END IF
 
      END

C$Procedure      ZZMKPC ( Make a time format picture mark )
 
      SUBROUTINE ZZMKPC ( PICTUR, B, E, MARK, PATTRN )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Given a numeric pattern, construct the appropriate time format
C     picture component.
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
C      None.
C
C$ Keywords
C
C      Time --- PRIVATE
C
C$ Declarations
 
      IMPLICIT NONE
      CHARACTER*(*)         PICTUR
      INTEGER               B
      INTEGER               E
      CHARACTER*(*)         MARK
      CHARACTER*(*)         PATTRN
 
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     PICTUR    I/O  A partially constructed time format picture
C     B          I   Beginning of substring to place a mark
C     E          I   End of substring to place a mark
C     MARK       I   Initial portion of a mark
C     PATTRN     I   Decimal pattern
C
C$ Detailed_Input
C
C     PICTUR     is a "TIMOUT" format picture that is under construction
C                The substring PICTUR(B:E) is supposed to be a sequence
C                of digits with possibly a decimal point in it.  The
C                digits before the decimal will be replaced by MARK.
C                The decimal point will be copied and digits after
C                the decimal point (up to 14 of them) will be replaced
C                by a the octothorpe character '#'.
C
C     B          are the beginning and ends of the substring mentioned
C     E          in PICTUR.
C
C     MARK       is a numeric time format component (DD, DOY, JULIAND,
C                HR, MN, SC )
C
C     PATTRN     a sequence of digits, possibly a leading minus sign
C                and possibly an embedded decimal point.
C
C$ Detailed_Output
C
C     PICTUR     is the input string with the appropriate time format
C                picture component inserted.
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
C     None.
C
C$ Particulars
C
C     This is a utility routine that assists in the construction of
C     a format picture that corresponds to a particular instance
C     of a time string.
C
C$ Examples
C
C     See ZZTIME.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 25-APR-1996 (WLT)
C
C
C-&
      INTEGER               LASTNB
 
      INTEGER               MAXPLC
      PARAMETER           ( MAXPLC = 14 )
 
      INTEGER               MAXSIZ
      PARAMETER           ( MAXSIZ = MAXPLC + 12 )
 
      CHARACTER*(MAXPLC)    PLACES
      CHARACTER*(MAXSIZ)    MYMARK
 
      INTEGER               LPAT
      INTEGER               LMRK
      INTEGER               POINT
      INTEGER               USE
      INTEGER               LAST
 
 
      PLACES = '##############'
C
C     Construct the replacement marker.  First the unmodified
C     portion of the marker.  (We use LAST as the pointer to the
C     last valid character of the marker).
C
      LMRK   = LASTNB(MARK)
      LPAT   = LEN   (PATTRN)
      MYMARK = MARK
      LAST   = LMRK
C
C     Is there a decimal point in the pattern?
C
      POINT  = INDEX(PATTRN, '.')
 
      IF ( POINT .GT. 0 ) THEN
C
C        We've got a decimal point.  We have to at least put this
C        into the marker.
C
         LAST              = LAST + 1
         MYMARK(LAST:LAST) = '.'
C
C        If the decimal point is not at the end of the pattern, we
C        will need to add some #'s to the marker (but not more than
C        MAXPLC of them).
C
         IF ( POINT .LT. LPAT ) THEN
 
            USE             = MIN ( MAXPLC, LPAT - POINT )
            MYMARK(LAST+1:) = PLACES(1:USE)
            LAST            = LAST + USE
 
         END IF
 
      END IF
C
C     We now let REPSUB do the work of replacing the substring
C     PICTUR(B:E) with the marker we've constructed.
C
      CALL ZZREPSUB ( PICTUR, B, E, MYMARK(1:LAST), PICTUR )
 
 
      RETURN
 
      END

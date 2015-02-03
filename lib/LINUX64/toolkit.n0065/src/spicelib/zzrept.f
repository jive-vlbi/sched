C$Procedure      ZZREPT ( Private --- replace tokens )
 
      LOGICAL FUNCTION ZZREPT ( SUB, REPLAC, L2R )
 
C$ Abstract
C
C    SPICE Private routine intended solely for the support of SPICE
C    routines.  Users should not call this routine directly due
C    to the volatile nature of this routine.
C
C    Replace matching tokens and remove the character "*"
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
C     TIME --- PRIVATE
C
C$ Declarations
 
      IMPLICIT NONE
      CHARACTER*(*)         SUB
      CHARACTER*(*)         REPLAC
      LOGICAL               L2R
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     SUB        I   is the substring to perform replacements on
C     REPLAC     I   is the replacement string
C     L2R        I   use left to right scanning if L2R is TRUE.
C
C     The function returns TRUE if a replacement is performed
C
C$ Detailed_Input
C
C     SUB        is a substring of characters to located in the
C                current internal tokenized representation of a
C                time string that is maintained by ZZTIME.
C
C     REPLAC     is a string of characters that will replace 1 for 1
C                the characters in SUB.  Note that character * is
C                a special character in this substitution as it
C                will be removed (via ZZREMT) after substitution.
C
C     L2R        is a logical flag.  If L2R is TRUE, the search
C                for a substring matching SUB will be performed
C                in left to right order. If L2R is FALSE the
C                search for substring matching SUB will be performed
C                from right to left.
C
C$ Detailed_Output
C
C     The function returns TRUE if a replacement is performed. Otherwise
C     it returns FALSE.
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
C     This routine is simply a macro for the combination of the
C     ZZTIME entry points ZZSUBT and ZZREMT
C
C$ Examples
C
C     See TPARTV.
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
C-    SPICELIB Version 1.0.0, 8-APR-1996 (WLT)
C
C
C-&
 
 
      LOGICAL               ZZREMT
      LOGICAL               ZZSUBT
 
      LOGICAL               OK
 
      ZZREPT = ZZSUBT ( SUB, REPLAC, L2R )
      OK     = ZZREMT ( '*' )
 
      RETURN
      END

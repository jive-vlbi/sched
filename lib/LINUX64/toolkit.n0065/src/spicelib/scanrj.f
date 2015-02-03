C$Procedure      SCANRJ ( Scan --- reject tokens )
 
      SUBROUTINE SCANRJ ( IDS, N, NTOKNS, IDENT, BEG, END )
      IMPLICIT NONE
 
 
C$ Abstract
C
C     Reject those tokens descriptors whose identities are among those
C     of a specific collection.
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
C     SCANNING
C
C$ Keywords
C
C     SEARCH
C     UTILITY
C
C$ Declarations
 
      INTEGER               IDS  ( * )
      INTEGER               N
      INTEGER               NTOKNS
      INTEGER               IDENT  ( * )
      INTEGER               BEG    ( * )
      INTEGER               END    ( * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     IDS        I   value of id's of tokens that should be dumped.
C     N          I   number of id's.
C     NTOKNS    I/O  number of tokens input.  The number kept.
C     IDENT     I/O  identity of each of the tokens.
C     BEG       I/O  indices of beginning of tokens.
C     END       I/O  indices of endings of tokens.
C
C$ Detailed_Input
C
C     IDS       is a list of the identity codes that we will want to
C               reject.
C
C     N         is the number of different cases.
C
C     NTOKNS    is the number of tokens to consider.
C
C     IDENT     holds the identities of each token that is up for
C               consideration.
C
C     BEG       holds the beginning indices of each token being
C               considered.
C
C     END       holds the ending indices of each token being
C               considered.
C
C$ Detailed_Output
C
C     NTOKNS    is the number of tokens remaining after the rejection
C               process has been completed.
C
C     IDENT     holds the identities of each token remaining.
C
C     BEG       holds the beginning indices of each token remaining.
C
C     END       holds the ending indices of each token remaining.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine serves as a macro for the rejection process that
C     is typically performed to remove tokens whose ID's fall into
C     some set.
C
C$ Examples
C
C     Suppose you wished to scan a string to locate the beginning and
C     endings of words together with punctuation, but that you did not
C     want to keep white space. The following code fragment illustrates
C     how you could use this routine to accomplish this task.
C
C     Words will be delimited by spaces, periods, commas, colons,
C     question marks, exclamation marks, semicolons, parentheses,
C     m-dashes, and quotes.
C
C     MARKS(1)  = ' '
C     MARKS(2)  = '.'
C     MARKS(3)  = ','
C     MARKS(4)  = '?'
C     MARKS(5)  = '!'
C     MARKS(6)  = '---'
C     MARKS(7)  = ':'
C     MARKS(8)  = ';'
C     MARKS(9)  = '('
C     MARKS(10) = ')'
C     MARKS(11) = '"'
C
C     NMARKS    = 11
C
C     IDS(1)    = 0
C     N         = 1
C
C
C     CALL SCANPR ( NMARKS, MARKS,  MRKLEN, MRKPTR )
C
C     IDS(1)    = BSRCHC ( ' ', NMARKS, MARKS )
C     N         = 1
C
C     CALL SCAN   ( STRING, MARKS,  MRKLEN, MRKPTR,
C    .              ROOM,   NTOKNS, IDENT,  BEG,    END )
C
C     CALL SCANRJ ( IDS, N, NTOKNS, IDENT,  BEG,    END )
C
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber     (JPL)
C
C$ Version
C
C-    SPICELIB Version  1.0.0, 26-JUL-1996 (WLT)
C
C-&
 
C$ Index_Entries
C
C     Remove tokens from a scanned list of tokens
C
C-&
 
 
 
C
C     SPICELIB functions
C
      INTEGER               ISRCHI
 
C
C     Local variables
C
      INTEGER               I
      INTEGER               J
 
 
C
C     There's not much to do, shift forward the token attributes for
C     tokens whose identities don't belong to the rejection list.
C
      J = 0
 
      DO I = 1, NTOKNS
 
         IF ( ISRCHI(IDENT(I), N, IDS) .EQ. 0 ) THEN
 
            J        = J + 1
            IDENT(J) = IDENT(I)
            BEG  (J) = BEG  (I)
            END  (J) = END  (I)
 
         END IF
 
      END DO
 
      NTOKNS = J
 
      RETURN
      END

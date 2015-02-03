C$Procedure ZZFTPSTR ( Private --- Fetch FTP Validation String )
 
      SUBROUTINE ZZFTPSTR ( TSTCOM, LEND, REND, DELIM )
 
C$ Abstract
C
C    Retrieve the components of the FTP validation string.
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
C     UTILITY
C
C$ Declarations
 
      IMPLICIT NONE
 
      INCLUDE              'zzftprms.inc'
 
      CHARACTER*(*)         TSTCOM
      CHARACTER*(*)         LEND
      CHARACTER*(*)         REND
      CHARACTER*(*)         DELIM
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     TSTCOM     O   The FTP test component string.
C     LEND       O   String that brackets TSTCOM on the left in a file.
C     REND       O   String that brackets TSTCOM on the right in a file.
C     DELIM      O   Delimiter that separates the pieces of TSTCOM.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     TSTCOM     is a string composed of clusters of characters that
C                are susceptible to FTP ASCII mode transfer corruption,
C                separated by the DELIM character.  For example:
C
C                    :<CLUSTR(1)>:<CLUSTR(2):...
C                                            ...<CLUSTR(N)>:
C
C                where <CLUSTR(I)> is one cluster of characters that
C                is subject to improper FTP corruption. The string
C                that is to receive this value should be SIZSTR
C                characters in length.
C
C     LEND,      are the two sequences of printing characters that
C     REND       bracket TSTCOM in the binary file.  Their purpose is
C                to permit proper detection of TSTCOM in the event
C                of compression or expansion, due to improper FTP
C                transfer.  The variables which are to receive these
C                values should be SIZEND characters in length.
C
C     DELIM      is the printing character delimiter that separates the
C                test character clusters from one another, as well as
C                LEND and REND.  Since it is often the case that pairs
C                or triples of non-printing characters will trigger
C                FTP corruption, this delimiter blocks any unintended
C                interaction.
C
C$ Parameters
C
C     1) See include file zzftprms.inc
C
C     2) Since inserting non-printing characters into strings is a
C        somewhat arduous task requiring extensive use of the intrinsic
C        CHAR, integer parameters that map to the needed ASCII codes are
C        defined with variable names INT###, where ### is replaced with
C        the three digit ASCII integer code.  For each such integer 
C        code, there is a corresponding character parameter whose name
C        is of the form ASC###.  For example:
C
C           INT010 = 10  -> ASC010 = <10> or <LF>
C           INT206 = 206 -> ASC206 = <206>
C
C        where <#> refers to CHAR(#) or CHAR(ICHAR('#')) in the case of
C        LF(line feed).
C
C        These naming conventions should be preserved when the FTP
C        validation string is updated.
C
C$ Files
C
C     While this routine is designed to aid in the detection of
C     improper FTP transfers, it simply returns the candidate
C     string for validation and does not interact with any
C     files directly.
C
C$ Exceptions
C
C     Error Free.
C
C$ Particulars
C
C     To minimize code alterations in the event of a string update,
C     the calling routine that declares the variables to receive
C     the strings stored here should include zzftprms.inc and utilize
C     the size parameters defined there as recommended in the Detailed
C     I/O sections above.
C
C     This private SPICELIB routine is designed to centralize the
C     definition of the FTP validation string present in binary
C     SPICE kernels.  If in the process of FTP'ing a binary
C     file from one platform to another, the user neglects to
C     invoke the IMAGE (BINARY) transfer mode, an ASCII mode
C     transfer may occur.  As this at the very least may substitute
C     one set of line terminators for another, corruption of the
C     binary file is likely.  By placing a string that encapsulates
C     a representative set of these character sequences that are
C     susceptible to corruption in the file record, it is possible
C     to trap and report any problems to the user when corrupted 
C     kernels are loaded at run time.
C
C     To that end, analysis of evidence obtained by moving test binary 
C     files from one platform to another indicates the following 
C     clusters of ASCII codes are likely candidates for corruption:
C
C        Test Clusters:
C
C        <13>      - Text line terminator on Macintosh-based platforms.
C        <10>      - Text line terminator on UNIX-based platforms.
C        <13><10>  - Text line terminator on Microsoft platforms.
C        <13><0>   - Sequence of characters that maps into <13> on some
C                    UNIX-based systems. (HP, SGI, NEXT)
C        <129>     - Macintosh based systems permute ASCII values whose
C                    parity bit is set.  Codes in excess of ASCII
C                    127 are altered.
C        <16><206> - Some ancient FTP servers on PC's convert this
C                    sequence of ASCII characters to <16><16><206>.
C
C     The examples above show that substitution of one set of line 
C     terminators for another can result in expansion or compression of 
C     certain sequences of bytes.  If the clusters were juxtaposed, new
C     sequences of adjacent bytes, themselves subject to transformation,
C     might be formed.  So the FTP test string present in the binary 
C     file should have some mechanism for preventing interaction between
C     the clusters.  The test string should also be constructed so that
C     it can be easily located in the event compression or expansion,
C     either internally or elsewhere in the file record, shifts it away 
C     from its default location.
C
C     So by separating these clusters with a printable delimiter, then 
C     bracketing the entire test string with start and stop identifiers,
C     we have a reasonable mechanism for locating and analyzing any 
C     potential FTP corruption. Then the sequence of characters to be 
C     inserted into the file will appear as:
C
C        FTPSTR:<13>:<10>:<13><10>:<13><0>:<129>:<16><206>:ENDFTP
C
C     where 'FTPSTR' and 'ENDFTP' are the bracketing substrings and
C     ':' is the delimiting character.
C
C     By no means do we claim that these are the complete set of
C     clusters that are corruptible through an improper FTP transfer.
C     An update procedure is provided in the Revisions section just
C     after the routine header.  Following this procedure will require
C     the least amount of effort to prevent older files from falsely
C     indicating corruption under new Toolkits, as well as newer files
C     failing on old Toolkits.
C
C
C$ Examples
C
C     This routine just fetches the components of the FTP validation
C     string.
C
C$ Restrictions
C
C     1) TSTCOM, LEND, REND, and DELIM must be large enough to hold
C        the entire values returned by this routine, otherwise
C        truncation will occur.
C
C$ Author_and_Institution
C
C     F.S. Turner     (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 21-MAR-1999 (FST)
C
C
C-&
 
C$ Index_Entries
C
C     fetch the ftp validation string components
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.0.0, 21-MAR-1999 (FST)
C
C        FTP validation string update procedure:
C
C           (1) Leave 'FTPSTR', 'ENDFTP', and ':' alone, as
C               their alteration will require special
C               consideration for older files.
C
C           (2) Leave the existing test clusters in the
C               existing order, and place any new clusters
C               between the last ':' and the E in 'ENDFTP'.
C               Make certain these are ':' delimited as well.
C
C           (3) Modify the contents of zzftprms.inc to
C               indicate the new sizes of the various string
C               components. Routines that include this must
C               then be recompiled.
C
C-&
 
C
C     Local Parameters
C
C     Maximum size of an individual test cluster component
C     including the ':'.
C
      INTEGER               SIZTSQ
      PARAMETER           ( SIZTSQ = 5 )
 
C
C     Integer codes of characters appearing in test clusters.
C
      INTEGER               INT000
      PARAMETER           ( INT000 = 0 )
 
      INTEGER               INT010
      PARAMETER           ( INT010 = 10 )
 
      INTEGER               INT013
      PARAMETER           ( INT013 = 13 )
 
      INTEGER               INT016
      PARAMETER           ( INT016 = 16 )
 
      INTEGER               INT129
      PARAMETER           ( INT129 = 129 )
 
      INTEGER               INT206
      PARAMETER           ( INT206 = 206 )

C
C
C     Local Variables
C
      CHARACTER*(SIZDLM)    LOCDLM
      CHARACTER*(SIZEND)    LOCLND
      CHARACTER*(SIZEND)    LOCRND
      CHARACTER*(SIZSTR)    LOCSTR
      CHARACTER*(SIZTSQ)    TESTSQ ( NUMTST )
      
C
C     Non-printing character values.
C
      CHARACTER*(1)         ASC000
      CHARACTER*(1)         ASC010
      CHARACTER*(1)         ASC013
      CHARACTER*(1)         ASC016
      CHARACTER*(1)         ASC129
      CHARACTER*(1)         ASC206
 
      INTEGER               I
 
      LOGICAL               FIRST
 
C
C     Saved Variables
C
      SAVE                  FIRST
      SAVE                  LOCDLM
      SAVE                  LOCLND
      SAVE                  LOCRND
      SAVE                  LOCSTR
 
C
C     Data Statements
C
      DATA   FIRST       / .TRUE.   /
 
C
C     Set up the components of the FTP validation string that
C     are not supposed to change for forward and backward
C     compatibility.
C
      DATA   LOCDLM      / ':'      /
      DATA   LOCLND      / 'FTPSTR' /
      DATA   LOCRND      / 'ENDFTP' /
 
C
C     On the first invocation initialize the string values.
C
      IF ( FIRST ) THEN
C
C        Convert the integer parameters to their non-printing ASCII
C        equivalents.
C
         ASC000 = CHAR( INT000 )
         ASC010 = CHAR( INT010 )
         ASC013 = CHAR( INT013 )
         ASC016 = CHAR( INT016 )
         ASC129 = CHAR( INT129 )
         ASC206 = CHAR( INT206 )
 
C
C        Now build the individual components of the test clusters.
C        Make certain the first component begins and ends with a ':',
C        and that the remaining pieces end in ':'. If you intend to
C        add some clusters, then append them to the end of the
C        sequence so as not to break the existing detection code.
C

C
C        Cluster #1 : <CR> - <13> - Macintosh Line Terminator
C
         TESTSQ(1) = LOCDLM // ASC013 // LOCDLM

C
C        Cluster #2 : <LF> - <10> - Unix Line Terminator
C
         TESTSQ(2) = ASC010 // LOCDLM

C
C        Cluster #3 : <CR><LF> - <10><13> - Microsoft Line Terminator
C
         TESTSQ(3) = ASC013 // ASC010 // LOCDLM

C
C        Cluster #4 : <13><0>
C
         TESTSQ(4) = ASC013 // ASC000 // LOCDLM

C
C        Cluster #5 : <129> - Macintosh Permutation of Parity Codes
C
         TESTSQ(5) = ASC129 // LOCDLM

C
C        Cluster #6 : <16><206>
C
         TESTSQ(6) = ASC016 // ASC206 // LOCDLM 

C
C        Sample cluster addition code follows
C
C        Cluster #7 : <xxx> - Description
C
C        TESTSQ(7) = ASCxxx // ... // LOCDLM
C

C
C        Now build the local copy of TSTCOM, LOCSTR. First clear the
C        uninitialized contents.
C
         LOCSTR = ' '
 
         DO I = 1, NUMTST

C
C           Append TESTSQ(I) to LOCSTR to properly construct the
C           test component of the FTP validation string.
C
            CALL SUFFIX ( TESTSQ(I), 0, LOCSTR )

         END DO

C
C        Prevent execution of this initialization code after first pass.
C
         FIRST = .FALSE.
 
      END IF
C
C     Copy the local copies of the FTP string components to the
C     arguments passed in from the caller.
C
      TSTCOM = LOCSTR
      LEND   = LOCLND
      REND   = LOCRND
      DELIM  = LOCDLM
 
      RETURN
      END

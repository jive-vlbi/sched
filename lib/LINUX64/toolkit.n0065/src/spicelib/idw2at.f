C$Procedure      IDW2AT ( Get file architecture and type from ID word )
 
      SUBROUTINE IDW2AT ( IDWORD, ARCH, TYPE )
 
C$ Abstract
C
C     Extract the architecture and type of a SPICE binary kernel file
C     from a file ID word.
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
C     KERNEL
C     UTILITY
C
C$ Declarations
 
      CHARACTER*(*)         IDWORD
      CHARACTER*(*)         ARCH
      CHARACTER*(*)         TYPE
 
C
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      IDWORD     I   The IDWORD to be examined.
C      ARCH       O   The file architecture DAS or DAF.
C      TYPE       O   The type of the file.
C
C$ Detailed_Input
C
C     IDWORD      is the ID word from a SPICE binary kernel file or a
C                 text version of a binary kernel file whose
C                 architecture and type are to be extracted.
C
C$ Detailed_Output
C
C     ARCH        is the file architecture used to store the data in
C                 a SPICE binary kernel file. If the architecture cannot
C                 be extracted or is not recognized the value '?' is
C                 returned.
C
C                 The possible architectures are:
C
C                    ASC -- An ASCII text file.
C                    DAF -- A DAF based file.
C                    DAS -- A DAS based file.
C                    KPL -- Kernel Pool File (i.e., a text kernel)
C                    TXT -- An ASCII text file.
C                    TE1 -- Text E-Kernel type 1.

C
C     TYPE        is the type of the SPICE file. If the type can not be 
C                 extracted or if it is blank, the value '?' is 
C                 returned.
C
C                 The type can only be extracted by this routine if
C                 the ID word follows the convention
C
C                    <architecture>/<type>
C
C                 where <architecture> is one of the file architectures 
C                 specified above, and
C
C                    <type> = 'xxxx'
C
C                 where 'xxxx' represents a four character mnemonic or
C                 code for the file type.
C
C                 This subroutine does not do any checking of the file 
C                 types. If a valid architecture is found and the type 
C                 is non-blank, that is what will be returned. It is up 
C                 to a higher level athority to determine whether a type 
C                 is valid.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C      1) If the variable ID word is blank, both the architecture and
C         type will be unknown, specified by '?'.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This subroutine is a support utility routine that attempts
C     to extract the architecture and type of a file from its ID word.
C     It may not be possible to determine the type of the file from the
C     ID word alone. Older files which contain the ID words 'NAIF/NIP',
C     or 'NAIF/DAF' do not have sufficient information in the ID word to
C     determine the type of the file. A type for the ID word 'NAIF/DAS'
C     is always 'PRE ', since files with this ID word were pre-release 
C     DAS files.
C
C     A file architecture can always be extracted from a valid SPICE
C     ID word.
C
C     This subroutine and the subroutine GETFAT (get file architecture 
C     and type) are intimately related. Whenever one of them is modified 
C     the other should be checked to see if the modifications affect it. 
C     Whenever a new architecture is added, both of the subroutines are 
C     affected.
C
C$ Examples
C
C     Suppose you wish to write a single routine for converting files
C     between text and binary formats. You can use this routine to
C     determine the architecture and type of the file and then pass the
C     file to the appropriate low level file conversion routine to
C     handle the actual conversion.
C
C        CALL IDW2AT ( IDWORD, ARCH, TYPE )
C
C        IF ( ARCH .EQ. 'DAF' ) THEN
C
C           convert a DAF file
C
C        ELSE IF ( ARCH .EQ. 'DAS' ) THEN
C
C           convert a DAS file
C
C        ELSE
C
C           WRITE(*,*) 'File architecture not supported.'
C
C        END IF
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
C     K.R. Gehringer  (JPL)
C
C$ Version
C
C-     SPICELIB Version 2.0.0, 26-OCT-1995 (KRG)
C
C         Changed the Version line from "Beta" to "SPICELIB" for the 
C         current revisions. The subroutine was already in SPICELIB,
C         but the Version line said "Beta."
C
C         Added several new architectures:
C
C            KPL -- Kernel Pool File (i.e., a text kernel)
C            TXT -- An ASCII text file.
C            ASC -- An ASCII text file.
C            TE1 -- Text E-Kernel type 1.
C
C         Changed the response foe the ID word 'NAIF/DAS' to be 
C         consistent with GETFAT. It now sets the architecture to 'DAS' 
C         and the type to 'PRE', for pre-release version.
C
C-     Beta Version 1.0.0, 30-SEP-1993 (KRG)
C
C-&
 
C
C$ Index_Entries
C
C     extract architecture and type from an id word
C
C-&
 
C$ Revisions
C
C-     SPICELIB Version 2.0.0, 26-OCT-1995 (KRG)
C
C         Changed the Version line from "Beta" to "SPICELIB" for the 
C         current revisions. The subroutine was already in SPICELIB,
C         but the Version line said "Beta."
C
C         Added several new architectures:
C
C            KPL -- Kernel Pool File (i.e., a text kernel)
C            TXT -- An ASCII text file.
C            ASC -- An ASCII text file.
C            TE1 -- Text E-Kernel type 1.
C
C         Changed the response foe the ID word 'NAIF/DAS' to be 
C         consistent with GETFAT. It now sets the architecture to 'DAS' 
C         and the type to 'PRE', for pre-release version.
C
C-&

C
C     Spicelib Routines
C
      INTEGER               POS
 
      LOGICAL               RETURN
C
C     Set the length of a SPICE file ID word.
C
      INTEGER               IDWLEN
      PARAMETER           ( IDWLEN = 8 )
C
C     Local Variables
C
      CHARACTER*(IDWLEN)     PART1
      CHARACTER*(IDWLEN)     PART2
 
      INTEGER               SLASH
C
C     Standard obligatory error handling stuff.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN  ( 'IDW2AT' )
      END IF
C
C     Check to see if we got a blank string for the ID word. If we did,
C     set the architecture and type to unknown.
C

      IF ( IDWORD .EQ. ' ' ) THEN
 
         ARCH = '?'
         TYPE = '?'
 
         CALL CHKOUT ( 'IDW2AT' )
         RETURN
 
      END IF
C
C     Initialize the temporary storage variables that we use.
C
      PART1  = ' '
      PART2  = ' '
C
C     See if we can get the architecture and type from the ID word.
C
C     Look for a '/' in the string. If we can't find it, we don't
C     recognize the architecture or the type, so set the architecture
C     and type to unknown.
C
      SLASH = POS( IDWORD, '/', 1 )
 
      IF ( SLASH .EQ. 0 ) THEN
 
         ARCH = '?'
         TYPE = '?'
 
         CALL CHKOUT ( 'IDW2AT' )
         RETURN
 
      END IF
C
C     The part before the slash is the architecture or the word 'NAIF'
C     in older files and the part after the slash is the type of file or
C     the architecture in older files.
C
      PART1 = IDWORD(1:SLASH-1)
      PART2 = IDWORD(SLASH+1:)
C
C     Let's now do some testing to try and figure out what's going on.
C
C     First we look for the information in the ID word format:
C
C        <architecture>/<type>,
C
C    then we look for the things that begin with the word 'NAIF'
C
      IF ( PART1 .EQ. 'DAF' ) THEN
C
C        We have a DAF file, so set the architecture and type.
C
         ARCH = 'DAF'

         IF ( PART2 .NE. ' ' ) THEN
            TYPE = PART2
         ELSE
            TYPE = '?'
         END IF
 
      ELSE IF ( PART1 .EQ. 'DAS' ) THEN
C
C        We have a DAS file, so set the architecture and type.
C
         ARCH = 'DAS'

         IF ( PART2 .NE. ' ' ) THEN
            TYPE = PART2
         ELSE
            TYPE = '?'
         END IF
 
      ELSE IF ( PART1 .EQ. 'TXT' ) THEN
C
C        We have an ASCII text file, so set the architecture and type.
C
         ARCH = 'TXT'

         IF ( PART2 .NE. ' ' ) THEN         
            TYPE = PART2
         ELSE
            TYPE = '?'
         END IF
 
      ELSE IF ( PART1 .EQ. 'ASC' ) THEN
C
C        We have an ASCII text file, so set the architecture and type.
C
         ARCH = 'TXT'

         IF ( PART2 .NE. ' ' ) THEN         
            TYPE = PART2
         ELSE
            TYPE = '?'
         END IF
 
      ELSE IF ( PART1 .EQ. 'KPL' ) THEN
C
C        We have a kernel pool file, so set the architecture and type.
C
         ARCH = 'KPL'

         IF ( PART2 .NE. ' ' ) THEN
            TYPE = PART2
         ELSE
            TYPE = '?'
         END IF
 
      ELSE IF (  PART1 .EQ. 'NAIF' )  THEN
C
C        We have a DAF (or NIP, these are equivalent) or DAS file,
C        identified by the value of PART2, but we have no idea what the
C        type is, unless the file is a DAS file, in which case it is a
C        pre-release EK file, since these are the only DAS files which 
C        used the 'NAIF/DAS' ID word.
C
C        First, we determine the architecture from PART2, then if it is
C        DAF or NIP, we give up on the type. As mentioned above, if
C        PART2 contains DAS, we know a priori the type of the file.
C
         IF ( ( PART2 .EQ. 'DAF' ) .OR. ( PART2 .EQ. 'NIP' ) ) THEN
 
            ARCH = 'DAF'
            TYPE = '?'
 
         ELSE IF ( PART2 .EQ. 'DAS' ) THEN
 
            ARCH = 'DAS'
            TYPE = 'PRE'
 
         ELSE
 
            ARCH = '?'
            TYPE = '?'
 
         END IF
 
      ELSE
 
         ARCH = '?'
         TYPE = '?'
 
      END IF

      CALL CHKOUT ( 'IDW2AT' )
      RETURN
 
      END

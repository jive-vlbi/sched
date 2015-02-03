C$Procedure ZZDDHPPF ( Private --- DDH Prepare Preexisting File )
 
      SUBROUTINE ZZDDHPPF ( UNIT, ARCH, BFF )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Prepare preexisting binary file for entry into the handle
C     table.
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
C     PRIVATE
C
C$ Declarations
 
      IMPLICIT NONE
 
      INCLUDE              'zzddhman.inc'
      INCLUDE              'zzftprms.inc'
 
      INTEGER               UNIT
      INTEGER               ARCH
      INTEGER               BFF
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     UNIT       I   Logical unit attached to the binary file.
C     ARCH       I   Integer code indicating the file architecture.
C     BFF        O   Integer code indicating the binary file format.
C
C$ Detailed_Input
C
C     UNIT       is a logical unit attached to the binary file to be
C                prepared for inclusion into the handle table.
C
C     ARCH       is an integer that indicates the architecture of
C                the file attached to UNIT.  Acceptable values are
C                the parameters:
C
C                   DAF
C                   DAS
C
C                defined in ZZDDHMAN.INC.
C
C$ Detailed_Output
C
C     BFF        is an integer that indicates the binary file format
C                of the DAF attached to UNIT.  Possible values are
C                the parameters:
C
C                   BIGI3E
C                   LTLI3E
C                   VAXGFL
C                   VAXDFL
C
C                defined in ZZDDHMAN.INC.
C
C$ Parameters
C
C     See the include file ZZDDHMAN.INC.
C
C$ Exceptions
C
C     1) SPICE(UNKNOWNFILARC) is signaled when ARCH is not in the
C        range of codes for known file architectures or the binary
C        file's ID word is unknown to IDW2AT.  BFF is set to 0 when
C        this error is signaled.  UNIT is not closed.
C
C     2) SPICE(FILEREADFAILED) is signaled when either of the two
C        READ statements in the module returns non-zero IOSTAT, thus
C        indicating read failure.  BFF is set to 0 in this case.  Unit
C        is not closed.
C
C     3) SPICE(FILARCMISMATCH) is signaled when the file attached to
C        UNIT is determined to utilize an architecture that is
C        different from the one to which the input argument ARCH
C        refers.  Unit is not closed.
C
C     4) SPICE(UNKNOWNBFF) is signaled whenever the binary file
C        format detection algorithm reaches a state of uncertainty
C        for DAFs.  This can be the result of several conditions,
C        an empty pre-N0052 DAF, reading a DAF with an unknown BFF
C        from a future toolkit, etc.  In all cases, BFF is set to 0.
C        Unit is not closed.
C
C     5) If a pre-FTP string binary is loaded, no FTP based
C        diagnostics are performed, and the file is assumed to be
C        in proper, working order.
C
C$ Files
C
C     This routine reads at least one, and potentially, several records
C     from the file attached to UNIT.
C
C$ Particulars
C
C     This routine exists to prepare a binary file for inclusion
C     in the handle table in ZZDDHMAN.  This includes verifying
C     that the file is suitable to load and determining the binary
C     file format where possible.
C
C     For DAF files:
C
C        The binary file format of old (pre-N0050) binaries is
C        detectable if the file is non-empty and undamaged.
C        New files contain the binary file format identification
C        string in the file record along with the FTP error
C        detection string.  They are correctly identified in most
C        cases, including damaged.
C
C     For DAS files:
C
C        The binary file format of old (pre-N0052) binaries is
C        not detectable.  This this module will assume that any
C        old DAS binaries are of the native format.  New binaries
C        include the binary file format identification string as
C        well as the FTP error detection string.  They are
C        correctly identified in most cases as well.
C
C     FTP Error Detection:
C
C        FTP error detection occurs when at least part of the
C        detection string is detected in the file record.  When
C        absent, no errors are signaled and the file is then
C        assumed to be an old binary.  In the event that the FTP
C        detection string is present, and additional unknown
C        sequences are present, diagnostics are only performed on
C        sequences known to this version of the toolkit.
C
C$ Examples
C
C     See ZZDDHMAN for sample usage.
C
C$ Restrictions
C
C     1) The file attached to UNIT was written on a platform whose
C        characters are of a single byte in length.
C
C     2) Numeric data when read as characters from the UNIT
C        preserves the bit patterns present in the file in
C        memory.
C
C     3) The intrinsic ICHAR preserves the bit pattern of the
C        character byte read from a file.  Namely if one examines
C        the integer created the 8 least significant bits will be
C        precisely those found in the character.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     F.S. Turner     (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.25.0, 10-MAR-2014 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-INTEL.
C
C-    SPICELIB Version 2.24.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-LINUX-64BIT-IFORT.
C
C-    SPICELIB Version 2.23.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-GFORTRAN.
C
C-    SPICELIB Version 2.22.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GFORTRAN.
C
C-    SPICELIB Version 2.21.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GCC_C.
C
C-    SPICELIB Version 2.20.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL.
C
C-    SPICELIB Version 2.19.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-CC_C.
C
C-    SPICELIB Version 2.18.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
C
C-    SPICELIB Version 2.17.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
C
C-    SPICELIB Version 2.16.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-WINDOWS-64BIT-IFORT.
C
C-    SPICELIB Version 2.15.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-LINUX-64BIT-GFORTRAN.
C
C-    SPICELIB Version 2.14.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-64BIT-MS_C.
C
C-    SPICELIB Version 2.13.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-INTEL_C.
C
C-    SPICELIB Version 2.12.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-IFORT.
C
C-    SPICELIB Version 2.11.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-GFORTRAN.
C
C-    SPICELIB Version 2.10.0, 18-MAR-2009 (BVS)
C
C        Updated for PC-LINUX-GFORTRAN.
C
C-    SPICELIB Version 2.9.0, 18-MAR-2009 (BVS)
C
C        Updated for MAC-OSX-GFORTRAN.
C
C-    SPICELIB Version 2.8.0, 19-FEB-2008 (BVS)
C
C        Updated for PC-LINUX-IFORT.
C
C-    SPICELIB Version 2.7.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-LINUX-64BIT-GCC_C.
C
C-    SPICELIB Version 2.6.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-INTEL_C.
C
C-    SPICELIB Version 2.5.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-IFORT.
C
C-    SPICELIB Version 2.4.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-WINDOWS-IFORT.
C
C-    SPICELIB Version 2.3.0, 26-OCT-2005 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-GCC_C.
C
C-    SPICELIB Version 2.2.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN_C.
C
C-    SPICELIB Version 2.1.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN.
C
C-    SPICELIB Version 2.0.1, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    SPICELIB Version 2.0.0, 06-FEB-2002 (FST)
C
C        This routine was updated to load binaries created by
C        N0051 versions of Sun Solaris Native C Toolkits.  See
C        the Revisions section for details.
C
C-    SPICELIB Version 1.0.0, 04-OCT-2001 (FST)
C
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 2.0.0, 06-FEB-2002 (FST)
C
C        Shortly after releasing N0052, a few of our users
C        discovered that they were unable to load binary
C        DAFs created with the N0051 Sun Solaris Native C
C        (SUN-SOLARIS-NATIVE_C) Toolkits.  The reason for this
C        is the previous version of ZZDDHPPF released with N0052
C        assumed that if a DAF file record possessed a valid
C        FTP error detection string, then it must contain a
C        binary file format ID string as well.  Both were added
C        to the DAF file record in N0050.
C
C        However, a bug in the N0051 version of the ZZPLATFM
C        master file, the source of the binary file format ID
C        string for a given platform, neglected to assign a
C        value to the string.  Since it was a C environment
C        and the implementation of ZZPLATFM resulted in the
C        string being a static variable, it was initialized
C        to nulls and written into the file.
C
C        This version of ZZDDHPPF has been extended to recognize
C        a null binary file format ID, and apply the byte
C        examination algorithm used on pre-N0050 DAFs to determine
C        its format.
C
C-&
 
 
C
C     SPICELIB Functions
C
      INTEGER               ISRCHC
      INTEGER               POS
 
      LOGICAL               RETURN
 
C
C     Local Parameters
C
C     Number of characters to be read in from a record.
C
      INTEGER               NUMCHR
      PARAMETER           ( NUMCHR = 1000 )
 
C
C     Bounding indices for the window that brackets the FTP
C     error detection string in the file record.
C
      INTEGER               FTPSTR
      PARAMETER           ( FTPSTR = 500 )
 
      INTEGER               FTPEND
      PARAMETER           ( FTPEND = 1000 )
 
C
C     Index of the start of the binary file format identification
C     string in DAF binaries.
C
      INTEGER               DAFBFF
      PARAMETER           ( DAFBFF = 89 )
 
C
C     Index of the start of the binary file format identification
C     string in DAS binaries.
C
      INTEGER               DASBFF
      PARAMETER           ( DASBFF = 85 )
 
C
C     Size of the binary format identification string.
C
      INTEGER               BFFSIZ
      PARAMETER           ( BFFSIZ = 8 )
 
C
C     Index of the first byte of NI in the DAF file record.
C
      INTEGER               NIPOS
      PARAMETER           ( NIPOS = 13 )
 
C
C     Index of the first byte of NSUM in the DAF descriptor record.
C
      INTEGER               NSMPOS
      PARAMETER           ( NSMPOS = 17 )
 
C
C     Index of the first byte of FDREC in the DAF file record.
C
      INTEGER               FDRPOS
      PARAMETER           ( FDRPOS = 77 )
 
C
C     Integer code such that CHAR(INTNUL) produces the NULL character.
C
      INTEGER               INTNUL
      PARAMETER           ( INTNUL = 0 )
 
C
C     IDW2AT Output Argument Lengths.
C
      INTEGER               IDWLEN
      PARAMETER           ( IDWLEN = 4 )
 
C
C     NULLID is the index of the extended STRBFF "NULL" string ID.
C
      INTEGER               NULLID
      PARAMETER           ( NULLID = NUMBFF+1 )
 
C
C     Local Variables
C
      CHARACTER*(1)         CARG
      CHARACTER*(NUMCHR)    CHRREC
      CHARACTER*(BFFSIZ)    BFFIDW
      CHARACTER*(IDWLEN)    FILARC
      CHARACTER*(IDWLEN)    FILTYP
      CHARACTER*(SIZDLM)    FTPDLM
      CHARACTER*(SIZEND)    FTPLFT
      CHARACTER*(SIZSTR)    FTPMEM
      CHARACTER*(SIZEND)    FTPRGT
      CHARACTER*(1)         NULL
      CHARACTER*(STRSIZ)    STRARC ( NUMARC )
      CHARACTER*(STRSIZ)    STRBFF ( NULLID )
 
      INTEGER               FDREC
      INTEGER               FTPPOS
      INTEGER               I
      INTEGER               IOSTAT
      INTEGER               TSTARC
 
      LOGICAL               FIRST
      LOGICAL               FOUND
      LOGICAL               FTPERR
 
C
C     Statement Functions
C
      INTEGER               ZZICHR
 
C
C     Saved Variables
C
      SAVE                  FIRST
      SAVE                  FTPDLM
      SAVE                  FTPLFT
      SAVE                  FTPMEM
      SAVE                  FTPRGT
      SAVE                  NULL
      SAVE                  STRARC
      SAVE                  STRBFF
 
C
C     Data Statements
C
      DATA                  FIRST  / .TRUE. /
 
C
C     Statement Function Definitions
C
C     This function controls the conversion of characters to integers.
C     Some versions of the g77 implement ICHAR with a signed integer.
C     This function computes the value of ICHAR that this code requires
C     on any version of g77 for x86 Linux.
C
      ZZICHR(CARG) = ICHAR(CARG) - MAX( -1, MIN(0,ICHAR(CARG)) )*256
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZDDHPPF' )
      END IF
 
C
C     If this is the first time into the routine, populate local
C     copies of reference values.  This includes the names of the
C     BFF parameters, the names of the ARCH parameters, and the
C     local copy of the FTP string.
C
      IF ( FIRST ) THEN
 
C
C        Construct and store the NULL valued byte.
C
         NULL = CHAR(INTNUL)
 
C
C        Retrieve the BFF and ARCH names.
C
         DO I = 1, NUMBFF
            CALL ZZDDHGSD ( 'BFF', I, STRBFF(I) )
         END DO
 
         DO I = 1, NUMARC
            CALL ZZDDHGSD ( 'ARCH', I, STRARC(I) )
         END DO
 
C
C        Extend STRBFF to include the null BFFID.  This addresses
C        the N0051 Sun Solaris Native C toolkit binary files.
C
         DO I = 1, BFFSIZ
            STRBFF(NULLID) (I:I) = NULL
         END DO
 
C
C        Fetch the FTP string.
C
         CALL ZZFTPSTR ( FTPMEM, FTPLFT, FTPRGT, FTPDLM )
 
C
C        Set FIRST to FALSE so we will not reassign any of these values.
C
         FIRST = .FALSE.
 
      END IF
 
C
C     Get the simple consistency checks out of the way first.  Is
C     the input ARCH value valid?
C
      IF ( ( ARCH .LE. 0 ) .OR. ( ARCH .GT. NUMARC ) ) THEN
 
         BFF = 0
 
         CALL SETMSG ( 'The integer code, ''#'' indicating the file '
     .   //            'architecture to examine is out of range.'     )
         CALL ERRINT ( '#', ARCH                                      )
         CALL SIGERR ( 'SPICE(UNKNOWNFILARC)'                         )
         CALL CHKOUT ( 'ZZDDHPPF'                                     )
         RETURN
 
      END IF
 
C
C     Read the first record from the file as a string of NUMCHR
C     characters.
C
      READ ( UNIT, REC=1, IOSTAT=IOSTAT ) CHRREC
 
C
C     Check for read failure.
C
      IF ( IOSTAT .NE. 0 ) THEN
 
         BFF = 0
 
         CALL SETMSG ( 'Error reading the file record from the '
     .   //            'binary DAF file ''#''.  IOSTAT = #.'     )
         CALL ERRFNM ( '#', UNIT                                 )
         CALL ERRINT ( '#', IOSTAT                               )
         CALL SIGERR ( 'SPICE(FILEREADFAILED)'                   )
         CALL CHKOUT ( 'ZZDDHPPF'                                )
         RETURN
 
      END IF
 
C
C     First check the ID word from the input file.
C
      CALL IDW2AT ( CHRREC(1:8), FILARC, FILTYP )
 
C
C     Now locate FILARC in the STRARC array.
C
      TSTARC = ISRCHC ( FILARC, NUMARC, STRARC )
 
C
C     If FILARC was not found, signal an appropriate error.
C
      IF ( TSTARC .EQ. 0 ) THEN
 
         BFF = 0
 
         CALL SETMSG ( 'The file, #, has a unidentified file '
     .   //            'architecture.  Check that this file '
     .   //            'is a properly created binary SPICE '
     .   //            'kernel.'                               )
         CALL ERRFNM ( '#', UNIT                               )
         CALL SIGERR ( 'SPICE(UNKNOWNFILARC)'                  )
         CALL CHKOUT ( 'ZZDDHPPF'                              )
         RETURN
 
C
C     Otherwise we have an architecture mismatch error, if
C     FILARC does not agree with ARCH.
C
      ELSE IF ( TSTARC .NE. ARCH ) THEN
 
         BFF = 0
 
         CALL SETMSG ( 'A request to load the # file, $, has '
     .   //            'been made by the % system.  This operation '
     .   //            'is not permitted.'                           )
         CALL ERRCH  ( '#', STRARC(TSTARC)                           )
         CALL ERRFNM ( '$', UNIT                                     )
         CALL ERRCH  ( '%', STRARC(ARCH)                             )
         CALL SIGERR ( 'SPICE(FILARCHMISMATCH)'                      )
         CALL CHKOUT ( 'ZZDDHPPF'                                    )
         RETURN
 
      END IF
 
C
C     Now check for possible FTP transfer errors.
C
      CALL ZZFTPCHK ( CHRREC(FTPSTR:FTPEND), FTPERR )
 
      IF ( FTPERR ) THEN
 
         BFF = 0
 
         CALL SETMSG ( 'FTP transfer error detected.  This '
     .   //            'binary $, ''#'', has most likely been '
     .   //            'corrupted by an ASCII mode FTP transfer. '
     .   //            'Obtain the file using IMAGE or BINARY '
     .   //            'transfer mode from the source.'            )
         CALL ERRCH  ( '$', STRARC(TSTARC)                         )
         CALL ERRFNM ( '#', UNIT                                   )
         CALL SIGERR ( 'SPICE(FTPXFERERROR)'                       )
         CALL CHKOUT ( 'ZZDDHPPF'                                  )
         RETURN
 
      END IF
 
C
C     Now this search is redundant, but the presence of the
C     FTPLFT string in the latter half of the file record
C     is fairly conclusive evidence that this is a "new" binary,
C     and we can expect to locate the binary file format
C     identification string.
C
      FTPPOS = POS ( CHRREC(500:1000), FTPLFT, 1 )
 
C
C     Check to see if we found FTPLFT.  If so extract the binary
C     file format ID word from the file record.
C
      IF ( FTPPOS .NE. 0 ) THEN
 
C
C        Extract BFFIDW from CHRREC.
C
         IF ( ARCH .EQ. DAF ) THEN
            BFFIDW = CHRREC ( DAFBFF : (DAFBFF+BFFSIZ-1) )
         ELSE IF ( ARCH .EQ. DAS ) THEN
            BFFIDW = CHRREC ( DASBFF : (DASBFF+BFFSIZ-1) )
         END IF
 
C
C        See if we can find BFFIDW in the STRBFF list.
C
         BFF = ISRCHC ( BFFIDW, NUMBFF + 1, STRBFF )
 
C
C        Check to see if BFF is 0, if it is, signal an error since
C        this indicates an unrecognized BFF.
C
         IF ( BFF .EQ. 0 ) THEN
 
            CALL SETMSG ( 'The file ''#'' utilizes the binary file '
     .      //            'format ''#''.  This format is currently '
     .      //            'unknown to this toolkit.  A toolkit '
     .      //            'update may be in order.'                  )
            CALL ERRFNM ( '#', UNIT                                  )
            CALL ERRCH  ( '#', BFFIDW                                )
            CALL SIGERR ( 'SPICE(UNKNOWNBFF)'                        )
            CALL CHKOUT ( 'ZZDDHPPF'                                 )
            RETURN
 
         END IF
 
C
C        See if we have a NULLID situation, if not check out and
C        return as swe have identified the BFF.
C
         IF ( BFF .NE. NULLID ) THEN
            CALL CHKOUT ( 'ZZDDHPPF' )
            RETURN
         END IF
 
      END IF
 
C
C     There is no FTP string, if the file is a DAS, we have to
C     assume it is of the native architecture.
C
      IF ( ARCH .EQ. DAS ) THEN
 
         CALL ZZPLATFM ( 'FILE_FORMAT', BFFIDW )
         CALL UCASE    ( BFFIDW,        BFFIDW )
 
         BFF = ISRCHC ( BFFIDW, NUMBFF, STRBFF )
 
         IF ( BFF .EQ. 0 ) THEN
 
            CALL SETMSG ( 'The native architecture for this '
     .      //            'platform is unknown to this version of '
     .      //            'the toolkit. This is a severe problem '
     .      //            'that should never occur, please contact '
     .      //            'NAIF.'                                    )
            CALL SIGERR ( 'SPICE(BUG)'                               )
            CALL CHKOUT ( 'ZZDDHPPF'                                 )
            RETURN
 
         END IF
 
         CALL CHKOUT ( 'ZZDDHPPF' )
         RETURN
 
      END IF
 
C
C     If we reach this point, then we are either dealing with
C     an old DAF (created by a pre-N0050 toolkit) or one of the
C     DAFs created by the N0051 Sun Solaris Native C version of
C     the toolkit.  This requires an examination of the bits
C     and bytes in the file that works this way:
C
C        Since in a valid DAF, 2 <= NI <= 250, we can easily
C        determine whether the 4 bytes used to store NI in the
C        file record are little or big endian.  If we discover
C        that the integer is encoded as big-endian, then stop
C        as this file must be 'BIG-IEEE'.  If it is little
C        endian, then locate the first descriptor record
C        in the file.
C
C        Read the first descriptor record.  Extract NSUM, the
C        3rd DP from the record.  If it is 0.0D0, signal an error
C        as this is an empty DAF and we can not determine its
C        type.  If it's non-zero, then check to see if the first
C        4 bytes are "0s".  If they are it must be 'LTL-IEEE'.
C        Otherwise pass it off to ZZDDHIVF to discriminate between
C        'VAX-GFLT' and 'VAX-DFLT'.  We know the first 4 bytes must
C        be "0s" in the 'LTL-IEEE" case, since NSUM is subject to
C        the following inequality: 1 <= NSUM <= 125
C
C     Having laid out the scheme, let's get to it.  First take a
C     look at the four character bytes that hold NI.  These bytes
C     be one of the following:
C
C        Little Endian:  VAL, 0, 0, 0
C           Big Endian:    0, 0, 0, VAL
C
C     where VAL is some non-zero value.
C
      IF ( ( CHRREC(     NIPOS:NIPOS     ) .EQ. NULL ) .AND.
     .     ( CHRREC( (NIPOS+1):(NIPOS+1) ) .EQ. NULL ) .AND.
     .     ( CHRREC( (NIPOS+2):(NIPOS+2) ) .EQ. NULL ) .AND.
     .     ( CHRREC( (NIPOS+3):(NIPOS+3) ) .NE. NULL )       ) THEN
 
         BFF = BIGI3E
 
      ELSE IF ( ( CHRREC(     NIPOS:NIPOS     ) .NE. NULL ) .AND.
     .          ( CHRREC( (NIPOS+1):(NIPOS+1) ) .EQ. NULL ) .AND.
     .          ( CHRREC( (NIPOS+2):(NIPOS+2) ) .EQ. NULL ) .AND.
     .          ( CHRREC( (NIPOS+3):(NIPOS+3) ) .EQ. NULL )       ) THEN
 
C
C        At this point we know we are dealing with a little endian
C        file.  Locate the first descriptor record.
C
         FDREC = ZZICHR( CHRREC(     FDRPOS:FDRPOS     ) )
         FDREC = ZZICHR( CHRREC( (FDRPOS+1):(FDRPOS+1) ) )*16   + FDREC
         FDREC = ZZICHR( CHRREC( (FDRPOS+2):(FDRPOS+2) ) )*256  + FDREC
         FDREC = ZZICHR( CHRREC( (FDRPOS+3):(FDRPOS+3) ) )*4096 + FDREC
 
C
C        Read the record into CHRREC.
C
         READ ( UNIT, REC=FDREC, IOSTAT=IOSTAT ) CHRREC
 
C
C        Check for read failure.
C
         IF ( IOSTAT .NE. 0 ) THEN
 
            BFF = 0
 
            CALL SETMSG ( 'Error reading a descriptor record from '
     .      //            'the binary DAF file ''#''.  IOSTAT = #.' )
            CALL ERRFNM ( '#', UNIT                                 )
            CALL ERRINT ( '#', IOSTAT                               )
            CALL SIGERR ( 'SPICE(FILEREADFAILED)'                   )
            CALL CHKOUT ( 'ZZDDHPPF'                                )
            RETURN
 
         END IF
 
C
C        Now examine the NSUM DP in this record to determine the
C        architecture.
C
         IF ( ( CHRREC(     NSMPOS:NSMPOS )     .EQ. NULL ) .AND.
     .        ( CHRREC( (NSMPOS+1):(NSMPOS+1) ) .EQ. NULL ) .AND.
     .        ( CHRREC( (NSMPOS+2):(NSMPOS+2) ) .EQ. NULL ) .AND.
     .        ( CHRREC( (NSMPOS+3):(NSMPOS+3) ) .EQ. NULL ) .AND.
     .        ( CHRREC( (NSMPOS+4):(NSMPOS+4) ) .EQ. NULL ) .AND.
     .        ( CHRREC( (NSMPOS+5):(NSMPOS+5) ) .EQ. NULL ) .AND.
     .        ( CHRREC( (NSMPOS+6):(NSMPOS+6) ) .EQ. NULL ) .AND.
     .        ( CHRREC( (NSMPOS+7):(NSMPOS+7) ) .EQ. NULL )       ) THEN
 
C
C           In this case we have an empty DAF, and can not distinguish
C           between little endian formats.  Signal an error and return.
C
            BFF = 0
 
            CALL SETMSG ( 'The DAF, ''#'', appears to contain no '
     .      //            'data.  As such, its binary file format '
     .      //            'can not be determined which prevents '
     .      //            'it from being loaded.'                   )
            CALL ERRFNM ( '#', UNIT                                 )
            CALL SIGERR ( 'SPICE(UNKNOWNBFF)'                       )
            CALL CHKOUT ( 'ZZDDHPPF'                                )
            RETURN
 
         ELSE IF ( ( CHRREC(     NSMPOS:NSMPOS     ) .EQ. NULL ) .AND.
     .             ( CHRREC( (NSMPOS+1):(NSMPOS+1) ) .EQ. NULL ) .AND.
     .             ( CHRREC( (NSMPOS+2):(NSMPOS+2) ) .EQ. NULL ) .AND.
     .             ( CHRREC( (NSMPOS+3):(NSMPOS+3) ) .EQ. NULL )  ) THEN
 
C
C           In this case the file is little endian IEEE.  Set BFF.
C
            BFF = LTLI3E
 
         ELSE
 
C
C           We are probably looking at a VAX file.  Find out which
C           format.
C
            CALL ZZDDHIVF ( CHRREC(NSMPOS:(NSMPOS+7)), BFF, FOUND )
 
            IF ( .NOT. FOUND ) THEN
 
               BFF = 0
 
               CALL SETMSG ( 'Unable to determine the binary file '
     .         //            'format of DAF ''#''.'                 )
               CALL ERRFNM ( '#', UNIT                              )
               CALL SIGERR ( 'SPICE(UNKNOWNBFF)'                    )
               CALL CHKOUT ( 'ZZDDHPPF'                             )
               RETURN
 
            END IF
 
         END IF
 
      ELSE
 
         BFF = 0
 
      END IF
 
      CALL CHKOUT ( 'ZZDDHPPF' )
      RETURN
 
      END

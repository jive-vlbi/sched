C$Procedure DAFAH ( DAF, assign handles )

      SUBROUTINE DAFAH ( FNAME,
     .                   FTYPE,
     .                   ND,
     .                   NI,
     .                   IFNAME,
     .                   RESV,
     .                   HANDLE,
     .                   UNIT,
     .                   FHSET,
     .                   ACCESS  )

C$ Abstract
C
C     Assign handles to DAFs as they are opened.
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
C     DAF
C
C$ Keywords
C
C     DAF
C     FILES
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE              'zzddhman.inc'

      INTEGER               INTEOC
      PARAMETER           ( INTEOC = 4 )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL  = -5 )

      CHARACTER*(*)         FNAME
      CHARACTER*(*)         FTYPE
      INTEGER               ND
      INTEGER               NI
      CHARACTER*(*)         IFNAME
      INTEGER               RESV
      INTEGER               HANDLE
      INTEGER               UNIT
      INTEGER               FHSET ( LBCELL : * )
      CHARACTER*(*)         ACCESS

C$ Brief_I/O
C
C     Variable  I/O  Entry points
C     --------  ---  --------------------------------------------------
C     FNAME     I,O  OPR, OPW, ONW, OPN (Obsolete), HFN, FNH
C     FTYPE      I   ONW
C     ND        I,O  ONW, OPN (Obsolete), HSF
C     NI        I,O  ONW, OPN (Obsolete), HSF
C     IFNAME     I   ONW, OPN (Obsolete)
C     RESV       I   ONW, OPN (Obsolete)
C     HANDLE    I,O  OPR, OPW, ONW, OPN (Obsolete), CLS, HLU, LUH, HFN,
C                    FNH, SIH
C     UNIT      I,O  HLU, LUH
C     FHSET      O   HOF
C     ACCESS     I   SIH
C     RECL       P   OPR, OPW, ONW, OPN (Obsolete)
C     FTSIZE     P   OPR, OPW, ONW, OPN (Obsolete), CLS, HLU, LUH, HFN,
C                    FNH
C     FILEN      P   SIH
C
C$ Detailed_Input
C
C     FNAME       on input is the name of a DAF to be opened, or
C                 the name of a DAF about which some information
C                 (handle, logical unit) is requested.
C
C     FTYPE       on input is a code for the type of data that is
C                 contained in the DAF file. This code has no meaning or
C                 interpretation at the level of the DAF file
C                 architecture, but is provided as a convenience for
C                 higher level software. The maximum length for the file
C                 type is four (4) characters. If the input string is
C                 longer than four characters, the first nonblank
C                 character and its three, or fewer, immediate nonblank
C                 successors will be used as the file type. The file
C                 type may not contain nonprinting characters, and it IS
C                 case sensitive.
C
C                 NAIF has reserved for its own use file types
C                 consisting of the upper case letters (A-Z) and the
C                 digits 0-9. NAIF recommends lower case or mixed case
C                 file types be used by all others in order to avoid
C                 any conflicts with NAIF file types.
C
C     ND          on input is the number of double precision components
C                 in each array summary of a new file.
C
C     NI          on input is the number of integer components in each
C                 array summary in a new file.
C
C     IFNAME      is the internal file name for a DAF to be created.
C
C     RESV        is the number of records to be reserved in a DAF
C                 to be created.
C
C     HANDLE      on input is the handle of a DAF about which some
C                 information (file name, logical unit) is requested,
C                 or the handle of a DAF to be closed.
C
C     UNIT        on input is the logical unit connected to a DAF
C                 about which some information (file name, handle) is
C                 requested.
C
C     ACCESS      is the type of access a DAF is open for, that is,
C                 either reading or writing.  The values of ACCESS
C                 may be
C
C                    'READ'
C                    'WRITE'
C
C                 Leading and trailing blanks are ignored, and case
C                 is not significant.
C
C$ Detailed_Output
C
C     FNAME       on output is the name of a DAF for which
C                 the corresponding handle or logical unit has been
C                 supplied.
C
C     ND          on output is the number of double precision
C                 components in each array summary of an existing file.
C
C     NI          on output is the number of integer components in
C                 each array summary in an existing file.
C
C     HANDLE      on output is the handle of a DAF for which
C                 the corresponding file name or logical unit has been
C                 supplied.
C
C     UNIT        on output is the logical unit connected to a DAF
C                 for which the corresponding file name or handle has
C                 been supplied.
C
C     FHSET       is a SPICELIB set containing the handles of the
C                 currently open DAFs.
C
C$ Parameters
C
C     RECL        is the record length of a DAF. Each record
C                 must be large enough to hold 128 double
C                 precision numbers or 1000 characters, whichever
C                 is greater. The units in which the record length
C                 must be specified vary from environment to
C                 environment. For example, VAX Fortran requires
C                 record lengths to be specified in longwords,
C                 where two longwords equal one double precision
C                 number.  See the include file 'zzddhman.inc' for
C                 details.
C
C     FTSIZE      is the size of the file table maintained internally
C                 by DAFAH. In effect, FTSIZE is the maximum number
C                 of DAFs that the DAF routines allow to be open
C                 simultaneously.  See the include file 'zzddhman.inc'
C                 for details.
C
C     FILEN       is the maximum filename length.  See the include file
C                 'zzddhman.inc' for details.
C
C
C     INTEOC      is the ASCII decimal integer code of the character
C                 recognized by SPICE as representing the end of the
C                 comment data in the reserved record area.
C
C$ Exceptions
C
C     1) If DAFAH is called directly, the error SPICE(BOGUSENTRY)
C        is signalled.
C
C     2) See entry points DAFOPR, DAFOPW, DAFONW, DAFOPN, DAFCLS,
C        DAFHSF, DAFHLU, DAFLUH, DAFHFN, DAFNFH, DAFHOF, and DAFSIH for
C        exceptions specific to those entry points.
C
C$ Files
C
C     All DAFs opened by this routine are specified by name.
C
C$ Particulars
C
C     DAFAH serves as an umbrella, allowing data to be shared by its
C     entry points:
C
C        DAFOPR         Open for read.
C        DAFOPW         Open for write.
C        DAFONW         Open new.
C        DAFOPN         Open new. (Obsolete, use DAFONW )
C
C        DAFCLS         Close.
C
C        DAFHSF         Handle to summary format.
C
C        DAFHLU         Handle to logical unit.
C        DAFLUH         Logical to handle.
C
C        DAFHFN         Handle to name.
C        DAFFNH         File name to handle.
C
C        DAFHOF         Handles of open files.
C        DAFSIH         Signal invalid handles.
C
C     Before a DAF can be used, it must be opened. Entry points
C     DAFOPR and DAFOPW provide the only means for opening an
C     existing DAF.
C
C     Several files may be opened for use simultaneously. (This makes
C     it convenient to combine data from several files to produce a
C     single result.) As each DAF is opened, it is assigned a file
C     handle, which is used to keep track of the file internally, and
C     which is used by the calling program to refer to the file in all
C     subsequent calls to DAF routines.
C
C     DAFs may be opened for two kinds of access: read, and write.
C     Files opened for read access may not be changed in any way. Files
C     opened for write access may be both read and written.
C
C     DAFONW is used to open a new DAF file. This routine extends the
C     functionality of DAFOPN by providing a mechanism for associating a
C     type with the data in the DAF file. The use of this entry over
C     DAFOPN is highly recommended.
C
C     Since the only reason for creating a new file is to write
C     something in it, all new files are opened for write access.
C
C     Entry point DAFOPN, for opening a new DAF file, has been rendered
C     obsolete by the new entry point DAFONW. The entry point DAFOPN
C     will continue to be supported for purposes of backward
C     compatibility, but its use in new software development is
C     discouraged.
C
C     Entry point DAFCLS provides the only official means of closing
C     a DAF that is currently open. Closing a DAF any other way (for
C     example, by determining its logical unit and using the Fortran
C     CLOSE statement directly) may affect your calling program in
C     mysterious ways.
C
C     Entry point DAFHSF allows you to determine the summary format
C     of any DAF that is currently open, without calling DAFRFR to
C     re-read the file record.
C
C     Entry point DAFHOF allows you to determine which DAFs are open
C     at any time.  In particular, you can use DAFHOF to determine
C     whether any file handle points to an open DAF.
C
C     Entry point DAFSIH signals errors when it is supplied with invalid
C     handles, so it serves to centralize error handling associated
C     with invalid handles.
C
C     The remaining entry points exist mainly to translate between
C     alternative representations of DAFs. There are three ways to
C     identify any open DAF: by name, by handle, and by logical
C     unit. Given any one of these, you may use these entry points to
C     find the other two.
C
C$ Examples
C
C     See entry points DAFOPR, DAFOPW, DAFONW, DAFOPN, DAFCLS, DAFHSF,
C     DAFHLU, DAFLUH, DAFHFN, DAFNFH, DAFHOF, and DAFSIH for examples
C     specific to those entry points.
C
C$ Restrictions
C
C     1) The value of parameter RECL may need to be changed when DAFAH
C        and its entry points are ported to a new environment (CPU and
C        compiler).
C
C     2) An integer overflow may occur if the number of files opened
C        by a single program exceeds the maximum number that can be
C        stored in an integer variable.
C
C$ Literature_References
C
C     1) Sun Fortran Programmer's Guide
C
C     2) Microsoft Fortran Optimizing Compiler User's Guide
C
C     3) Lahey F77 EM/32 Language Reference Manual, page 144
C
C     4) Language Systems FORTRAN Reference Manual, Version 1.2,
C        page 12-7
C
C     5) "FORTRAN/9000 Reference HP 9000 Series 700 Computers",
C        First Edition, June 1991, Hewlett Packard Company, page 5-110.
C
C$ Author_and_Institution
C
C     K.R. Gehringer  (JPL)
C     N.J. Bachman    (JPL)
C     J.M. Lynch      (JPL)
C     J.E. McLean     (JPL)
C     H.A. Neilan     (JPL)
C     M.J. Spencer    (JPL)
C     W.L. Taber      (JPL)
C     F.S. Turner     (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 9.0.1, 10-OCT-2012 (EDW)
C
C        Edited DAFOPN Abstract section to use "Deprecated" keyword
C        and state replacement routine.
C
C        Corrected ordering of all header sections.
C
C        Added a functional code example to the Examples section
C        in DAFOPN and DAFCLS.
C
C        Removed the obsolete Reference citation to "NAIF
C        Document 167.0."
C
C-    SPICELIB Version 9.0.0, 09-NOV-2006 (NJB)
C
C        Updated the entry point DAFONW so that a non-empty reserved
C        record area will also be a valid empty comment area.  DAFONW
C        now writes a EOC character to the first byte of the second
C        record when the input number of reserved records NRESV is
C        greater than zero.
C
C-    SPICELIB Version 8.1.0, 02-APR-2002 (FST)
C
C        Updated the following entry points in response to changes
C        to the handle manager interfaces:
C
C           DAFCLS
C           DAFOPR
C           DAFOPW
C           DAFONW
C           DAFOPN
C
C        See the Revisions section for details.
C
C        Minor bug fix to DAFFNH.  An error was signaled but the
C        intended call to CHKOUT and RETURN statement were omitted.
C
C-    SPICELIB Version 8.0.0, 14-NOV-2000 (FST)
C
C        Cleaned up entry point headers by removing duplicate
C        entries from the Revisions section where appropriate.
C
C        Integrated the new handle manager code into this module.
C        The number of DAFs the system can load is now 1000,
C        and some supported environments can read non-native
C        binary DAFs.  See the Convert User's Guide for details.
C
C-    SPICELIB Version 7.0.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  New
C        environments are WIN-NT
C
C-    SPICELIB Version 7.0.3, 16-SEP-1999 (NJB)
C
C        CSPICE environments were added.  Some typos were corrected.
C
C-    SPICELIB Version 7.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 7.0.1, 22-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 7.0.0, 22-MAR-1999 (FST)
C
C        To accommodate the DAF FTP validation check, the following
C        entry points were modified:
C
C           DAFOPR, DAFOPW, DAFONW, DAFOPN.
C
C        See their headers and code for the details of the changes.
C
C-    SPICELIB Version 6.0.0, 05-APR-1998 (NJB)
C
C        Added references to the PC-LINUX environment.
C
C-    SPICELIB Version 5.1.0, 08-MAR-1996 (KRG)
C
C        The Following entry points have been modified: DAFONW and
C        DAFOPN.
C
C        The modifications support the notion of a DAF comment area,
C        and involve writing NULL filled reserved records when the
C        number of reserved records is greater than zero (0).
C
C        Some nested IF...THEN...ELSE IF...THEN...END IF constructs
C        were expanded to be independent IF...THEN...END IF tests.
C        The tests were for IOSTAT errors on cascading write statements
C        nested in the IF...ELSE IF... statements, and this was
C        confusing. These tests were restructured so that IOSTAT is
C        tested after each write statement which is equicalent to the
C        original intent and easier to read.
C
C-    SPICELIB Version 5.0.0, 27-SEP-1993 (KRG)
C
C        The following entry points have had code modifications:
C        DAFOPR, DAFOPW and DAFOPN.
C
C        A new entry point has been added: DAFONW.
C
C        The modifications are to allow a type to be associated with a
C        DAF file.
C
C        A new parameter has been added to this subroutine's parameter
C        list, FTYPE, so that type information may be passed to the
C        entry point DAFONW. Two new variables were added to the
C        routine as well, TARCH and TTYPE, which provide temporary
C        storage for the file architecture and type.
C
C        Several new parameters have been added to the declarations for
C        this routine:
C
C           ARCLEN   The length of a file architecture.
C
C           MAXPC    The maximum decimal value for the range of
C                    printable characters.
C
C           MINPC    The minimum decimal value for the range of
C                    printable characters.
C
C           TYPLEN   The length of a file type.
C
C        See the individual entry points for detailed descriptions of
C        their modifications.
C
C        Removed the variables MINHAN and NIL, as they were not used in
C        any of the entry points, yet they had values assigned to them
C        through DATA statements.
C
C        Made all occurrences of error message formatting of filenames
C        consistent. All filenames will be single quoted in the output
C        error message.
C
C-    SPICELIB Version 4.0.0, 25-FEB-1993 (JML)
C
C        In the entry points DAFOPR, DAFOPW, and DAFFNH, the INQUIRE
C        statement that checks if the file is already open now also
C        checks that the file exists.
C
C        IOSTAT is now checked after all INQUIRE statements.
C
C        A new variable LUN is used in DAFOPR, DAFOPW, and DAFOPN
C        for the logical unit number returned by GETLUN.
C
C        The IF-THEN statements in DAFOPR and DAFOPW were reorganized
C        to make the routines more readable.
C
C        In DAFOPR and DAFOPW, a long error message was added for the
C        case when the NAIF/DAF id word was not recognized. Also, the
C        file is closed when this error is signalled.
C
C        In DAFOPR and DAFOPW, IOSTAT is now checked after the file
C        record is read.
C
C        In DAFOPR, DAFOPW, DAFOPN, and DAFFNH, the file name is
C        checked to see if it is blank.
C
C        In DAFOPR, DAFOPW, DAFOPN, and DAFFNH, the file name passed
C        to the FORTRAN OPEN and INQUIRE statements has been chopped
C        at the last non-blank character.
C
C        A minor error in the particulars section of the header of
C        DAFCLS was corrected.  It formerly stated that a file could be
C        open more than once for read or write access instead of just
C        read access.
C
C-    SPICELIB Version 3.2.0, 6-OCT-1992 (HAN)
C
C        Module was updated to include the record length and source
C        for the Hewlett Packard UX 9000/750 environment. Moved FILEN
C        to the Declarations section, and corrected Revisions section
C        to include the last code change description, 3.1.0.
C
C-    SPICELIB Version 3.1.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 3.1.0, 13-NOV-1991 (MJS)
C
C        Module was updated to operate in the Lahey F77 EM/32
C        PC environment.
C
C-    SPICELIB Version 3.0.0, 03-SEP-1991 (NJB) (WLT)
C
C        DAFAH and its entry points were modified to permit multiple
C        DAFs to be open for writing at the same time.  Also, the
C        entry points DAFHOF and DAFSIH were added.
C
C-    SPICELIB Version 2.0.0, 25-MAR-1991 (JEM) (MJS)
C
C        The variable MINHAN was initialized to zero and the variable
C        NEXT was saved.  DAFOPW now accepts the ID word 'NAIF/NIP'
C        as well 'NAIF/DAF'.  Spelling mistakes were corrected.
C
C-    SPICELIB Version 1.1.0, 5-NOV-1990 (HAN)
C
C        The parameter FTSIZE was increased from 4 to 20.
C
C-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
C
C        Literature references added to the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&


C$ Index_Entries
C
C     assign daf handles
C
C-&

C$ Revisions
C
C-    SPICELIB Version 8.1.0, 02-APR-2002 (FST)
C
C        The entry point ZZDDHCLS in the handle manager (ZZDDHMAN)
C        had its argument list augmented to allow files to be
C        deleted on close.  This allows the removal of a series
C        of "raw" CLOSE statements in a few of the entry points
C        of this routine.
C
C-    SPICELIB Version 8.0.0, 14-NOV-2001 (FST)
C
C        The DAF system now utilizes the handle manager umbrella
C        (ZZDDHMAN) and its entry points to provide most of the
C        handle and logical unit based operations that DAFAH
C        previously managed.
C
C        FTSIZE Files with UTSIZE Units:
C
C        In previous versions of the DAF system all files opened
C        through the DAFAH entry points were connected to logical
C        units.  In contrast, the handle manager umbrella entry
C        points allow FTSIZE files to be loaded (opened), while
C        only utilizing UTSIZE (less than FTSIZE, see the include
C        file 'zzddhman.inc') logical units.  The entry points in
C        the handle manager automatically connect and disconnect
C        loaded files from their logical units as new files are
C        loaded and accessed.
C
C        Previously, one could buffer a logical unit associated
C        with a particular handle and access the file directly
C        with Fortran I/O statements.  To preserve this capability
C        invoking DAFHLU locks a handle to its assigned logical
C        unit, until that lock is removed (see ZZDDHUNL, an entry
C        point in ZZDDHMAN) or the file is closed.  See the
C        Revisions section in the DAFHLU entry point for details.
C
C        Another consequence of the utilization of the handle
C        manager code is that the process of connecting a file
C        name to a HANDLE may require performing up to FTSIZE
C        INQUIRE statements.  This is necessary to insure that
C        different names referring to the same file return the
C        same handle.  This was the case previously with the DAF
C        system since an INQUIRE on a different, but equivalent,
C        file name would produce the same logical unit.
C
C        FTP Error Detection:
C
C        The FTP error detection software is now integrated into
C        the handle manager umbrella entry points, and as such
C        is no longer present in DAFAH.
C
C        Non-Native Files:
C
C        In addition to expanding the number of loaded files the
C        DAF system supports, the handle manager also detects and
C        tracks binary file formats.  This allows a layer of
C        private code that has been inserted between DAF routines
C        and the Fortran I/O statements to provide translation
C        services for DAF.  Some environments are now endowed with
C        the ability to read files created with certain non-native
C        binary file formats.  See the Convert User's Guide for
C        details.
C
C-    SPICELIB Version 7.0.0, 22-MAR-1999 (FST)
C
C        Binary File Format Identification:
C
C        The file record now contains an 8 character string that
C        identifies the binary file format utilized by DAFs.
C        The purpose of this string's inclusion in the file record
C        is preparatory in nature, to accelerate the migration to
C        files that support the runtime translation update that
C        is scheduled.
C
C        FTP Validation:
C
C        The DAF system now employs a validation scheme to assist
C        users in detecting DAFs potentially corrupted via ASCII mode
C        FTP transfers.  A string that contains sequences of
C        characters commonly corrupted by improper FTP transfers is
C        inserted into the unused portion of the file record. When any
C        DAFAH entry point attempts to open a file, this string is
C        located and examined.  If the string indicates the file is
C        corrupted, the entry point signals an error.
C
C           Detection Scheme Implementation:
C
C           When a new DAF is created, the entry points DAFONW and
C           DAFOPN(obsolete) retrieve the FTP validation string from
C           the defining routine (ZZFTPSTR) and insert it into the
C           tail of the file record.  A diagram illustrating the new
C           file record for 32-bit environments with single byte
C           characters follows:
C
C              +=============+
C              | File Record |
C              |    Data     |
C              +=============+
C                     |
C               +=====|===+==========================+===+========+
C               |     |   |    603 bytes of nulls    | | | nulls  |
C               +=========+==========================+=|=+========+
C           Byte 1                                     |         1024
C                                                 +============+
C                                                 | FTP        |
C                                                 | Validation |
C                                                 | String     |
C                                                 +============+
C
C           As can be seen above, the file record is now null padded,
C           which was not the case previously.
C
C           When an existing DAF is opened, the entry points DAFOPR
C           and DAFOPW attempt to verify that the validation string is
C           intact.  This is accomplished by reading the file
C           record into a character string, and then passing the last
C           half of this string into the validation subroutine
C           ZZFTPCHK.  Only sending the latter half of the file record
C           into ZZFTPCHK is done to prevent other portions of the file
C           record from confusing the validation process.  The following
C           three abnormal situations may arise during validation:
C
C              (1) Older DAFs without the FTP validation string are
C                  not validated.  As far as the DAF open routines
C                  are concerned such files are valid by default. The
C                  only notable exception is that the garbage that
C                  resides in the unused portion of the file record may
C                  confuse ZZFTPCHK into thinking the validation
C                  string is present.  (The probability of this event
C                  is minimal and noted only for completeness.)
C
C              (2) Files with an older version of the validation
C                  string are examined for errors supported by the
C                  contemporaneous version of the Toolkit.
C
C              (3) Files with a newer version of the validation
C                  string are examined for errors supported by the
C                  current version of the Toolkit.
C
C           Updates to the FTP Validation String:
C
C           In the event that it becomes necessary to add additional
C           test characters to the validation string, refer to
C           ZZFTPSTR for the proper procedure.  The instructions
C           provided there ensure that the above behavior is properly
C           adhered to by the modifications.
C
C           FTP Validation Issues in Code Portability:
C
C           The scheme as currently implemented will function
C           properly in any computing environment whose character data
C           conforms to the single byte ASCII standards with a word
C           size that is between 32 and 64 bits inclusive.  Refer to
C           the above diagram that displays the new DAF file record
C           and the following discussion for details.
C
C           Since the DAF file record block contains integer data,
C           it may expand if the word size increases above the
C           currently supported 32 bits.  However, the FTP validation
C           string is extracted by reading in 1000 bytes of character
C           data and examining bytes 500-1000. (See the parameters
C           FTPBLK and FTPSTR if you need to alter these numbers).
C           So as long as the alteration in word size does not cause
C           the FTP string information to shift out of bytes 500-1000
C           in the file record, the existing code will function
C           properly.
C
C-    SPICELIB Version 3.2.0, 6-OCT-1992 (HAN)
C
C        The code was also reformatted so that a utility program can
C        create the source file for a specific environment given a
C        master source file.
C
C-    SPICELIB Version 3.0.0, 03-SEP-1991 (NJB) (WLT)
C
C        DAFAH and the entry point DAFOPW were modified to permit
C        multiple DAFs to be open for writing at the same time.
C        Also, the entry points DAFHOF and DAFSIH were added.  DAFHOF
C        returns a set containing the handles of currently open DAFs.
C        To accommodate the addition of DAFHOF, the argument FHSET
C        was added to DAFAH's argument list, and local declarations
C        for DAFHOF were added to DAFAH's declaration section.  DAFSIH
C        signals an error if the file indicated by the handle is not
C        open for the specified type of access.
C
C-    SPICELIB Version 2.0.0, 24-JAN-1991 (JEM) (MJS)
C
C        The entry point DAFOPW accepted only 'NAIF/DAF' as a valid
C        ID word.  It now accepts 'NAIF/NIP' as well for
C        backwards compatibility.  The entry point DAFOPR did not need
C        this fix because it already accepts both ID words.
C
C-    SPICELIB Version 1.1.0,  5-NOV-1990 (HAN)
C
C        The parameter FTSIZE was increased from 4 to 20. The number
C        4 was chosen for testing purposes and was not removed.
C
C-&

C
C     SPICELIB functions
C
      INTEGER               ISRCHI
      INTEGER               LTRIM
      INTEGER               RTRIM

      LOGICAL               ELEMI
      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local parameters
C
      INTEGER               ACCLEN
      PARAMETER           ( ACCLEN =   10 )

      INTEGER               ARCLEN
      PARAMETER           ( ARCLEN =    3 )

      INTEGER               CRLEN
      PARAMETER           ( CRLEN  = 1000 )

      INTEGER               DPRSIZ
      PARAMETER           ( DPRSIZ =  128 )

      INTEGER               FMTLEN
      PARAMETER           ( FMTLEN =    8 )

      INTEGER               IDLEN
      PARAMETER           ( IDLEN  =    8 )

      INTEGER               IFNLEN
      PARAMETER           ( IFNLEN =   60 )

      INTEGER               MAXND
      PARAMETER           ( MAXND  =  124 )

      INTEGER               MAXNI
      PARAMETER           ( MAXNI  =  250 )

      INTEGER               MAXSUM
      PARAMETER           ( MAXSUM =  125 )

      INTEGER               MAXPC
      PARAMETER           ( MAXPC  =  126 )

      INTEGER               MINPC
      PARAMETER           ( MINPC  =   32 )

      INTEGER               TYPLEN
      PARAMETER           ( TYPLEN =    4 )

C
C     Local variables
C

C
C     As each file is opened, it is assigned a handle, and the
C     internal file name is stored for comparison with other files.
C     All names in the file table begin with FT.
C
C        HAN      Handle
C        LNK      Number of links
C        ND,
C        NI       Summary format
C
C     The columns are stored in no particular order. New files are
C     added to the end of the list; the list is repacked whenever a
C     file is removed from the list.
C
C     NFT is the number of files currently opened: this may not be
C     greater than FTSIZE. FINDEX refers to a file of interest within
C     the table.
C
C     NEXT is incremented each time a file is opened to become the
C     next file handle assigned.
C
      INTEGER               NFT
      INTEGER               FTHAN  ( FTSIZE )
      INTEGER               FTLNK  ( FTSIZE )
      INTEGER               FTND   ( FTSIZE )
      INTEGER               FTNI   ( FTSIZE )
      INTEGER               FINDEX

C
C     Other local variables
C
      CHARACTER*(ACCLEN)    ACC
      CHARACTER*(CRLEN)     CREC
      CHARACTER*(FILEN)     DAFNAM
      CHARACTER*(FMTLEN)    FORMAT
      CHARACTER*(IDLEN)     IDWORD
      CHARACTER*(IFNLEN)    IFN
      CHARACTER*(TYPLEN)    TTYPE

      DOUBLE PRECISION      DREC   ( DPRSIZ )

      INTEGER               BWARD
      INTEGER               FHLIST ( LBCELL : FTSIZE )
      INTEGER               FND
      INTEGER               FNI
      INTEGER               FREE
      INTEGER               FNB
      INTEGER               FWARD
      INTEGER               I
      INTEGER               IARC
      INTEGER               IBFF
      INTEGER               IAMH
      INTEGER               IOSTAT
      INTEGER               LUN

      LOGICAL               FIRST
      LOGICAL               FOUND

C
C     Saved variables
C

C
C     Save everything between calls.
C
      SAVE

C
C     Initial values
C
      DATA                  FIRST    / .TRUE. /
      DATA                  NFT      / 0      /

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN  ( 'DAFAH' )
         CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
         CALL CHKOUT ( 'DAFAH' )
      END IF

      RETURN




C$Procedure DAFOPR ( DAF, open for read )

      ENTRY DAFOPR ( FNAME, HANDLE )

C$ Abstract
C
C     Open a DAF for subsequent read requests.
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
C     DAF
C
C$ Keywords
C
C     DAF
C     FILES
C
C$ Declarations
C
C     CHARACTER*(*)         FNAME
C     INTEGER               HANDLE
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     FNAME      I   Name of DAF to be opened.
C     HANDLE     O   Handle assigned to DAF.
C
C$ Detailed_Input
C
C     FNAME       is the file name of a DAF to be opened for read
C                 access.
C
C$ Detailed_Output
C
C     HANDLE      is the file handle associated with the file. This
C                 handle is used to identify the file in subsequent
C                 calls to other DAF routines.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     1) If the specified file has already been opened for read
C        access, the handle already associated with the file is
C        returned.
C
C     2) If the specified file has already been opened for write
C        access, an error is signaled by routines in the call
C        tree of this routine.
C
C     3) If the specified file has already been opened by a non-DAF
C        routine, an error is signaled by routines in the call
C        tree of this routine.
C
C     4) If the specified file cannot be opened without exceeding
C        the maximum number of files, the error SPICE(DAFFTFULL)
C        is signaled.
C
C     5) If the attempt to read the file's file record fails,
C        the error SPICE(FILEREADFAILED) is signaled.
C
C     6) If the specified file is not a DAF file, an error is
C        signaled by routines in the call tree of this routine.
C
C     7) If no logical units are available, an error is
C        signaled by routines called by this routine.
C
C     8) If the file does not exist, the error SPICE(FILENOTFOUND)
C        is signaled by routines in the call tree of this routine.
C
C     9) If an I/O error occurs in the process of opening the file,
C        routines in the call tree of this routine signal an error.
C
C    10) If the file name is blank or otherwise inappropriate
C        routines in the call tree of this routine signal an error.
C
C    11) If the file was transferred improperly via FTP, routines
C        in the call tree of this routine signal an error.
C
C    12) If the file utilizes a binary file format that is not
C        currently supported on this platform, an error is signaled
C        by routines in the call tree of this routine.
C
C$ Files
C
C     See argument FNAME.
C
C$ Particulars
C
C     Most DAFs require only read access. If you do not need to
C     change the contents of a file, you should open it with DAFOPR.
C
C$ Examples
C
C     Example (1):
C
C     In the following code fragment, DAFOPR is used to open a file,
C     which is then searched for DAFs containing data for a particular
C     object.
C
C        CALL DAFOPR ( FNAME, HANDLE )
C        CALL DAFBFS ( HANDLE )
C        CALL DAFFNA ( FOUND  )
C
C        DO WHILE ( FOUND )
C           CALL DAFGS ( SUM )
C           CALL DAFUS ( SUM, ND, NI, DC, IC )
C
C           IF ( IC(1) .EQ. TARGET_OBJECT ) THEN
C            .
C            .
C
C           END IF
C
C           CALL DAFFNA ( FOUND )
C        END DO
C
C
C     Example (2):
C
C     Use a simple routine to output the double precision and integer
C     values stored in an SPK's segments descriptors. This function
C     opens a DAF for read, performs a forwards search for the DAF
C     arrays, prints segments description for each array found, then
C     closes the DAF.
C
C           PROGRAM DAF_T
C
C           INTEGER             HANDLE
C
C     C
C     C     Define the summary parameters appropriate
C     C     for an SPK file.
C     C
C           INTEGER             ND
C           PARAMETER         ( ND = 2 )
C
C           INTEGER             NI
C           PARAMETER         ( NI = 6 )
C
C           INTEGER             IC( NI )
C
C           DOUBLE PRECISION    DC( ND )
C
C           CHARACTER*(32)      KERNEL
C
C           LOGICAL             FOUND
C
C
C     C
C     C     Open a DAF for read. Return a HANDLE referring to the file.
C     C
C           KERNEL = 'de421.bsp'
C           CALL DAFOPR ( KERNEL, HANDLE )
C
C     C
C     C     Begin a forward search on the file.
C     C
C           CALL DAFBFS ( HANDLE )
C
C     C
C     C     Search until a DAF array is found.
C     C
C           CALL DAFFNA ( FOUND )
C
C     C
C     C     Loop while the search finds subsequent DAF arrays.
C     C
C           DO WHILE ( FOUND )
C
C              CALL DAFGS ( SUM )
C              CALL DAFUS ( SUM, ND, NI, DC, IC )
C
C              WRITE(*,*)                'Doubles: ', DC(1:ND)
C              WRITE(*, FMT='(A,6I9)' ) 'Integers: ', IC(1:NI)
C
C     C
C     C        Check for another segment.
C     C
C              CALL DAFFNA ( FOUND )
C
C           END DO
C
C     C
C     C     Safely close the DAF.
C     C
C           CALL DAFCLS ( HANDLE )
C
C           END
C
C     The program outputs:
C
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         1        0        1        2      641   310404
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         2        0        1        2   310405   423048
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         3        0        1        2   423049   567372
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         4        0        1        2   567373   628976
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         5        0        1        2   628977   674740
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         6        0        1        2   674741   715224
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         7        0        1        2   715225   750428
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         8        0        1        2   750429   785632
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         9        0        1        2   785633   820836
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:        10        0        1        2   820837   944040
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:       301        3        1        2   944041  1521324
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:       399        3        1        2  1521325  2098608
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:       199        1        1        2  2098609  2098620
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:       299        2        1        2  2098621  2098632
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:       499        4        1        2  2098633  2098644
C
C      Note, the final entries in the integer array contains the segment
C      start/end indexes. The output indicates the search proceeded
C      from the start of the file (low value index) towards the end
C      (high value index).
C
C$ Restrictions
C
C     1) Files opened using this routine must be closed with DAFCLS.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     K.R. Gehringer  (JPL)
C     N.J. Bachman    (JPL)
C     J.M. Lynch      (JPL)
C     W.L. Taber      (JPL)
C     F.S. Turner     (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 8.1.1, 10-OCT-2012 (EDW)
C
C        Added a functional code example to the Examples section.
C
C        Removed the unneeded Revisions section.
C
C        Removed the obsolete Reference citation to "NAIF
C        Document 167.0."
C
C        Corrected ordering of header section.
C
C-    SPICELIB Version 8.1.0, 02-APR-2002 (FST)
C
C        This routine was updated to accomodate changes to the
C        handle manager interface.  See DAFAH's Revision section
C        for details.
C
C-    SPICELIB Version 8.0.0, 13-NOV-2001 (FST)
C
C        This routine was updated to utilize the new handle manager
C        software to manage binary file formats and consolidated
C        I/O code.
C
C-    SPICELIB Version 7.0.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  New
C        environments are WIN-NT
C
C-    SPICELIB Version 7.0.3, 16-SEP-1999 (NJB)
C
C        CSPICE environments were added.  Some typos were corrected.
C
C-    SPICELIB Version 7.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 7.0.1, 17-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 5.0.0, 03-MAR-1999 (FST)
C
C        This entry point now attempts to locate and validate the
C        FTP validation string contained in the file record.
C
C        See the Revisions section under DAFAH for a discussion
C        of the impact of the changes made for this version.
C
C-    SPICELIB Version 4.0.0, 27-SEP-1993 (KRG)
C
C        This routine was modified to use a subroutine to obtain the
C        architecture of the file rather than using hard coded values
C        for comparison with the file ID word. This was done in order to
C        isolate the code which checks to determine a file architecture
C        and to make the identification of file types easier through a
C        change to the file ID word.
C
C        In particular, the changes to this routine support the change
C        of the file ID word from 'NAIF/DAF' or 'NAIF/NIP' to 'DAF/xxxx'
C        where 'xxxx' represents a four character mnemonic code for the
C        type of data in the file.
C
C        Removed the error SPICE(DAFNOIDWORD) as it was no longer
C        relevant.
C
C        Added the error SPICE(NOTADAFFILE) if this routine is called
C        with a file that does not contain an ID word identifying the
C        file as a DAF file.
C
C        Changed the long error message when the error
C        SPICE(NOTADAFFILE) is signalled to suggest that a common error
C        is attempting to load a text version of the desired file rather
C        than the binary version.
C
C-    SPICELIB Version 3.0.0, 25-FEB-1993 (JML)
C
C        The INQUIRE statement that checks if the file is already open
C        now also checks that the file exists.
C
C        A new variable LUN is used for the logical unit number
C        returned by GETLUN.
C
C        The IF-THEN statements were reorganized to improve readability.
C
C        A long error message is now set when the DAF id word is not
C        recognized.  Also, the file is closed when this error is
C        signalled.
C
C        IOSTAT is checked after the file record is read.
C
C        The file name is checked to see if it is blank.
C
C        The file name string that is passed to the FORTRAN OPEN and
C        INQUIRE statements has been chopped at the last non-blank
C        character.
C
C-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 2.0.0, 03-SEP-1991 (NJB) (WLT)
C
C        This routine was updated so that it now keeps current the set
C        of DAF handles returned by DAFHOF.
C
C        Some error messages were changed so that they specify
C        names of relevant DAFs.
C
C-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
C
C        Literature references added to the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&

C$ Index_Entries
C
C     open daf for read
C
C-&


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFOPR' )
      END IF

C
C     Initialize the handle list, if necessary.
C
      IF ( FIRST ) THEN
         CALL SSIZEI ( FTSIZE, FHLIST )
         FIRST = .FALSE.
      END IF

C
C     Attempt to open the file; perform any appropriate checks.
C
      CALL ZZDDHOPN ( FNAME, 'READ', 'DAF', HANDLE )

C
C     Check FAILED(); return if an error has occurred.
C
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DAFOPR' )
         RETURN
      END IF


C
C     See if this file is already present in the file table.  If it
C     is simply increment its link count by one, check out and
C     return.
C
      FINDEX = ISRCHI ( HANDLE, NFT, FTHAN )

      IF ( FINDEX .NE. 0 ) THEN
         FTLNK(FINDEX) = FTLNK(FINDEX) + 1
         CALL CHKOUT ( 'DAFOPR' )
         RETURN
      END IF

C
C     Retrieve ND and NI from the file record.
C
      CALL ZZDAFGFR ( HANDLE,
     .                IDWORD,
     .                FND,
     .                FNI,
     .                IFN,
     .                FWARD,
     .                BWARD,
     .                FREE,
     .                FOUND   )

      IF ( .NOT. FOUND ) THEN

         CALL ZZDDHCLS ( HANDLE, 'DAF', .FALSE.                      )
         CALL SETMSG   ( 'Error reading the file record from the'  //
     .                   ' binary DAF file ''#''.'                   )
         CALL ERRCH    ( '#', FNAME                                  )
         CALL SIGERR   ( 'SPICE(FILEREADFAILED)'                     )
         CALL CHKOUT   ( 'DAFOPR'                                    )
         RETURN

      END IF

C
C     At this point, we know that we have a valid DAF file, and we're
C     set up to read from it, so ...
C
C     Update the file table to include information about our newly
C     opened DAF.
C
      NFT        = NFT  + 1
      FTHAN(NFT) = HANDLE
      FTND(NFT)  = FND
      FTNI(NFT)  = FNI
      FTLNK(NFT) = 1

C
C     Insert the new handle into our handle set.
C
      CALL INSRTI ( HANDLE, FHLIST )

      CALL CHKOUT ( 'DAFOPR' )
      RETURN




C$Procedure DAFOPW ( DAF, open for write )

      ENTRY DAFOPW ( FNAME, HANDLE )

C$ Abstract
C
C     Open a DAF for subsequent write requests.
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
C     DAF
C
C$ Keywords
C
C     DAF
C     FILES
C
C$ Declarations
C
C     CHARACTER*(*)         FNAME
C     INTEGER               HANDLE
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     FNAME      I   Name of DAF to be opened.
C     HANDLE     O   Handle assigned to DAF.
C
C$ Detailed_Input
C
C     FNAME       is the name of a DAF to be opened with write
C                 access.
C
C$ Detailed_Output
C
C     HANDLE      is the file handle associated with the file. This
C                 handle is used to identify the file in subsequent
C                 calls to other DAF routines.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     1) If the specified file has already been opened, either by
C        the DAF routines or by other code, an error is signaled by
C        routines in the call tree of this routine.  Note that this
C        response is not paralleled by DAFOPR, which allows you
C        to open a DAF for reading even if it is already open for
C        reading.
C
C     2) If the specified file cannot be opened without exceeding
C        the maximum number of files, the error SPICE(DAFFTFULL)
C        is signaled.
C
C     3) If the attempt to read the file's file record fails, the
C        error SPICE(FILEREADFAILED) will be signalled.
C
C     4) If the specified file is not a DAF file, an error is
C        signaled by routines in the call tree of this routine.
C
C     5) If no logical units are available, an error is
C        signaled by routines called by this routine.
C
C     6) If the file does not exist, the error SPICE(FILENOTFOUND)
C        is signaled by routines in the call tree of this routine.
C
C     7) If an I/O error occurs in the process of opening the file,
C        routines in the call tree of this routine signal an error.
C
C     8) If the file name is blank or otherwise inappropriate
C        routines in the call tree of this routine signal an error.
C
C     9) If the file was transferred improperly via FTP, routines
C        in the call tree of this routine signal an error.
C
C    10) If the file utilizes a non-native binary file format, an
C        error is signaled by routines in the call tree of this
C        routine.
C
C$ Files
C
C     See argument FNAME.
C
C$ Particulars
C
C     Most DAFs require only read access. If you do not need to
C     change the contents of a file, you should open it with DAFOPR.
C     Use DAFOPW when you need to
C
C        -- change (update) one or more summaries, names, or
C           arrays within a file; or
C
C        -- add new arrays to a file.
C
C$ Examples
C
C     In the following code fragment, DAFOPW is used to open a
C     file, which is then searched for arrays containing data for
C     a particular object. The code for the object is then changed
C     (perhaps to reflect some new convention).
C
C        CALL DAFOPW ( FNAME, HANDLE )
C        CALL DAFBFS ( HANDLE )
C        CALL DAFFNA ( FOUND  )
C
C        DO WHILE ( FOUND )
C           CALL DAFGS ( SUM )
C           CALL DAFUS ( SUM, ND, NI, DC, IC )
C
C           IF ( IC(1) .EQ. OLD_CODE ) THEN
C              IC(1) = NEW_CODE
C
C              CALL DAFPS ( ND, NI, DC, IC, SUM )
C              CALL DAFRS ( SUM )
C           END IF
C
C           CALL DAFFNA ( FOUND )
C        END DO
C
C$ Restrictions
C
C     1) Only file of the native binary file format may be opened
C        with this routine.
C
C     2) Files opened using this routine must be closed with DAFCLS.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     K.R. Gehringer  (JPL)
C     N.J. Bachman    (JPL)
C     J.M. Lynch      (JPL)
C     J.E. McLean     (JPL)
C     W.L. Taber      (JPL)
C     F.S. Turner     (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 8.1.1, 10-OCT-2012 (EDW)
C
C        Corrected ordering of header section.
C
C        Removed the obsolete Reference citation to "NAIF
C        Document 167.0."
C
C-    SPICELIB Version 8.1.0, 02-APR-2002 (FST)
C
C        This routine was updated to accomodate changes to the
C        handle manager interface.  See DAFAH's Revision section
C        for details.
C
C-    SPICELIB Version 8.0.0, 13-NOV-2001 (FST)
C
C        This routine was updated to utilize the new handle manager
C        software to manage binary file formats and consolidated
C        I/O code.
C
C-    SPICELIB Version 7.0.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  New
C        environments are WIN-NT
C
C-    SPICELIB Version 7.0.3, 16-SEP-1999 (NJB)
C
C        CSPICE environments were added.  Some typos were corrected.
C
C-    SPICELIB Version 7.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 7.0.1, 17-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 6.0.0, 03-MAR-1999 (FST)
C
C        This entry point now attempts to locate and validate the
C        FTP validation string contained in the file record.
C
C-    SPICELIB Version 5.0.0, 27-SEP-1993 (KRG)
C
C        This routine was modified to use a subroutine to obtain the
C        architecture of the file rather than using hard coded values
C        for comparing to the file ID word. This was done in order to
C        isolate the code which checks to determine a file architecture,
C        and to make the identification of file types easier through a
C        change to the file ID word.
C
C        In particular, the changes to this routine support the change
C        of the file ID word from 'NAIF/DAF' or 'NAIF/NIP' to 'DAF/xxxx'
C        where 'xxxx' represents a four character mnemonic code for the
C        type of data in the file.
C
C        Removed the error SPICE(DAFNOIDWORD) as it was no longer
C        relevant.
C
C        Added the error SPICE(NOTADAFFILE) if this routine is called
C        with a file that does not contain an ID word identifying the
C        file as a DAF file.
C
C        Changed the long error message when the error
C        SPICE(NOTADAFFILE) is signalled to suggest that a common error
C        is attempting to load a text version of the desired file rather
C        than the binary version.
C
C-    SPICELIB Version 4.0.0, 25-FEB-1993 (JML)
C
C        The INQUIRE statement that checks if the file is already open
C        now also checks that the file exists.
C
C        A new variable LUN is used for the logical unit number
C        returned by GETLUN.
C
C        The IF-THEN statements were reorganized to improve readability.
C
C        A long error message is now set when the DAF id word is not
C        recognized.  Also, the file is closed when this error is
C        signalled.
C
C        IOSTAT is now checked after the file record is read.
C
C        The file name is checked to see if it is blank.
C
C        The file name string that is passed to the FORTRAN OPEN and
C        INQUIRE statements has been chopped at the last non-blank
C        character.
C
C-    SPICELIB Version 3.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 3.0.0, 03-SEP-1991 (NJB) (WLT)
C
C        DAFOPW now allows multiple files to be open for writing.
C
C        This routine was updated so that it now keeps current the set
C        of DAF handles returned by DAFHOF.
C
C-    SPICELIB Version 2.0.0, 24-JAN-1991 (JEM)
C
C        DAFOPW now accepts the ID word 'NAIF/NIP' as well 'NAIF/DAF'.
C
C-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
C
C        Literature references added to the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&

C$ Index_Entries
C
C     open daf for write
C
C-&

C$ Revisions
C
C-    SPICELIB Version 6.0.0, 03-MAR-1999 (FST)
C
C        See the Revisions section under DAFAH for a discussion
C        of the impact of the changes made for this version.
C
C-    SPICELIB Version 2.0.0, 03-SEP-1991 (NJB) (WLT)
C
C        DAFOPW now allows multiple files to be open for writing.
C
C        This routine was updated so that it now keeps current the set
C        of DAF handles returned by DAFHOF.
C
C        Some error messages were changed so that they specify
C        names of relevant DAFs.
C-&

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFOPW' )
      END IF

C
C     Initialize the handle list, if necessary.
C
      IF ( FIRST ) THEN

         CALL SSIZEI ( FTSIZE, FHLIST )
         FIRST = .FALSE.

      END IF

C
C     Check to see if there is room in the file table.
C
      IF ( NFT .EQ. FTSIZE ) THEN

         CALL SETMSG ( 'The file table is full, with # entries. '     //
     .                 'Could not open ''#''.'                         )
         CALL ERRINT ( '#', FTSIZE                                     )
         CALL ERRCH  ( '#', FNAME                                      )
         CALL SIGERR ( 'SPICE(DAFFTFULL)'                              )
         CALL CHKOUT ( 'DAFOPW'                                        )
         RETURN

      END IF

C
C     Attempt to open the file; perform any appropriate checks.
C
      CALL ZZDDHOPN ( FNAME, 'WRITE', 'DAF', HANDLE )

C
C     Check FAILED(); return if an error has occurred.
C
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DAFOPW' )
         RETURN
      END IF

C
C     Retrieve ND and NI from the file record.
C
      CALL ZZDAFGFR ( HANDLE,
     .                IDWORD,
     .                FND,
     .                FNI,
     .                IFN,
     .                FWARD,
     .                BWARD,
     .                FREE,
     .                FOUND   )

      IF ( .NOT. FOUND ) THEN

         CALL ZZDDHCLS ( HANDLE, 'DAF', .FALSE.                      )
         CALL SETMSG   ( 'Error reading the file record from the'  //
     .                   ' binary DAF file ''#''.'                   )
         CALL ERRCH    ( '#', FNAME                                  )
         CALL ERRINT   ( '#', IOSTAT                                 )
         CALL SIGERR   ( 'SPICE(FILEREADFAILED)'                     )
         CALL CHKOUT   ( 'DAFOPW'                                    )
         RETURN

      END IF

C
C     At this point, we know that we have a valid DAF file, and we're
C     set up to write to it or read from it, so ...
C
C     Update the file table to include information about our
C     newly opened DAF.
C
      NFT        = NFT  + 1
      FTHAN(NFT) = HANDLE
      FTND(NFT)  = FND
      FTNI(NFT)  = FNI
      FTLNK(NFT) = 1

C
C     Insert the new handle into our handle set.
C
      CALL INSRTI ( HANDLE, FHLIST )

      CALL CHKOUT ( 'DAFOPW' )
      RETURN




C$Procedure DAFONW ( DAF, open new )

      ENTRY DAFONW ( FNAME, FTYPE, ND, NI, IFNAME, RESV, HANDLE )

C$ Abstract
C
C     Open a new DAF for subsequent write requests.
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
C     DAF
C
C$ Keywords
C
C     DAF
C     FILES
C
C$ Declarations
C
C     CHARACTER*(*)         FNAME
C     CHARACTER*(*)         FTYPE
C     INTEGER               ND
C     INTEGER               NI
C     CHARACTER*(*)         IFNAME
C     INTEGER               RESV
C     INTEGER               HANDLE
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     FNAME      I   Name of DAF to be opened.
C     FTYPE      I   Mnemonic code for type of data in the DAF file.
C     ND         I   Number of double precision components in summaries.
C     NI         I   Number of integer components in summaries.
C     IFNAME     I   Internal file name.
C     RESV       I   Number of records to reserve.
C     HANDLE     O   Handle assigned to DAF.
C
C$ Detailed_Input
C
C     FNAME       is the name of a new DAF to be created (and
C                 consequently opened for write access).
C
C     FTYPE       is a code for type of data placed into a DAF file.
C                 The first nonblank character and the three (3)
C                 characters immediately following it, giving four (4)
C                 characters, are used to represent the type of the data
C                 placed in the DAF file. This is provided as a
C                 convenience for higher level software. It is an error
C                 if this string is blank. When written to the DAF file,
C                 the value for the type IS case sensitive; what you put
C                 in is what you get out, so be careful.
C
C                 NAIF has reserved for its own use file types
C                 consisting of the upper case letters (A-Z) and the
C                 digits 0-9. NAIF recommends lower case or mixed case
C                 file types be used by all others in order to avoid
C                 any conflicts with NAIF file types.
C
C     ND          is the number of double precision components
C                 in each array summary of the new file.
C
C     NI          is the number of integer components in each
C                 array summary in the new file.
C
C     IFNAME      is the internal file name (containing as many as 60
C                 characters) for the new file. This should uniquely
C                 identify the file.
C
C     RESV        is the number of records in the new file to be
C                 reserved; these records will not be used to store any
C                 data belonging to DAF arrays subsequently written to
C                 the file. The user may reserve records 2 through (2 +
C                 RESV - 1) in the file. SPICE kernels based on the DAF
C                 format use the reserved record area to store optional
C                 textual information; for these kernels, the reserved
C                 records contain the file's "comment area."
C
C                 When RESV is non-zero, this routine writes an
C                 end-of-comments character into the first byte of
C                 record 2, and fills the rest of the allocated records
C                 will null (ASCII code 0) characters.
C
C$ Detailed_Output
C
C     HANDLE      is the file handle associated with the file. This
C                 handle is used to identify the file in subsequent
C                 calls to other DAF routines.
C
C$ Parameters
C
C     INTEOC      is the ASCII decimal integer code of the character
C                 recognized by SPICE as representing the end of the
C                 comment data in the reserved record area.
C
C$ Exceptions
C
C     1) If the specified file cannot be opened without exceeding
C        the maximum number of files, the error SPICE(DAFFTFULL)
C        is signalled.
C
C     2) If the input argument ND is out of the range [0, 124]
C        or if NI is out of the range [2, 250], the error
C        SPICE(DAFINVALIDPARAMS) is signalled.
C
C     3) If
C
C           ND + ( NI + 1 ) / 2   >  125
C
C        the error SPICE(DAFINVALIDPARAMS) is signalled.
C
C     4) If the number of records to be reserved is not zero or
C        positive, the error SPICE(DAFNORESV) is signalled.
C
C     5) If an I/O error occurs in the process of creating the file,
C        routines in the call tree of this routine signal an error.
C
C     6) If (for some reason) the initial records in the file cannot
C        be written, the error SPICE(DAFWRITEFAIL) is signalled.
C
C     7) If no logical units are available, the error is
C        signaled by routines called by this routine.
C
C     8) If the file name is blank or otherwise inappropriate
C        routines in the call tree of this routine signal an error.
C
C     9) If the file type is blank, the error SPICE(BLANKFILETYPE)
C        is signalled.
C
C     10) If the file type contains nonprinting characters, decimal
C         0-31 and 127-255, the error SPICE(ILLEGALCHARACTER) is
C         signalled.
C
C$ Files
C
C     See argument FNAME.
C
C$ Particulars
C
C     This routine supersedes DAFOPN as the method for opening a new DAF
C     file. It includes a data type identifier as part of the ID word of
C     a DAF file it creates.
C
C     The DAFs created by DAFONW have initialized file records but
C     do not yet contain any arrays.  See the DAF Required Reading
C     for a discussion of file records.
C
C$ Examples
C
C     In the following code fragment, DAFONW is used to open a file,
C     to which a new array is then added. This file will have the data
C     type 'TEST' which may be used to distinguish production data from
C     test data at a user subroutine level.
C
C        FNAME = 'test.bin'
C        FTYPE = 'TEST'
C
C        CALL DAFONW   ( FNAME, FTYPE,  ND,  NI,  IFNAME, 0, HANDLE )
C
C        CALL DAFBNA   ( HANDLE, SUM, NAME  )
C        CALL GET_DATA ( DATA,   N,   FOUND )
C
C        DO WHILE ( FOUND )
C           CALL DAFADA   ( DATA, N        )
C           CALL GET_DATA ( DATA, N, FOUND )
C        END DO
C
C        CALL DAFENA
C
C$ Restrictions
C
C     1) Files opened using this routine must be closed with DAFCLS.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     K.R. Gehringer  (JPL)
C     N.J. Bachman    (JPL)
C     J.M. Lynch      (JPL)
C     H.A. Neilan     (JPL)
C     W.L. Taber      (JPL)
C     F.S. Turner     (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 9.0.1, 10-OCT-2012 (EDW)
C
C        Corrected ordering of header section.
C
C        Removed the obsolete Reference citation to "NAIF
C        Document 167.0."
C
C-    SPICELIB Version 9.0.0, 09-NOV-2006 (NJB)
C
C        DAFONW now writes a EOC character to the first byte
C        of the second record when NRESV > 0.
C
C-    SPICELIB Version 8.1.0, 02-APR-2002 (FST)
C
C        This routine was updated to accomodate changes to the
C        handle manager interface.  See DAFAH's Revision section
C        for details.
C
C-    SPICELIB Version 8.0.0, 13-NOV-2001 (FST)
C
C        This routine was updated to utilize the new handle manager
C        software to manage binary file formats and consolidated
C        I/O code.
C
C-    SPICELIB Version 7.0.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  New
C        environments are WIN-NT
C
C-    SPICELIB Version 7.0.3, 16-SEP-1999 (NJB)
C
C        CSPICE environments were added.  Some typos were corrected.
C
C-    SPICELIB Version 7.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 7.0.1, 17-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 2.0.0, 03-MAR-1999 (FST)
C
C        The entry point was modified to insert the FTP validation
C        string, as well as the binary file format into the file record.
C
C-    SPICELIB Version 1.1.0, 08-MAR-1996 (KRG)
C
C        The modifications support the notion of a DAF comment area,
C        and involve writing NULL filled reserved records when the
C        number of reserved records is greater than zero (0).
C
C        Some nested IF...THEN...ELSE IF...THEN...END IF constructs
C        were expanded to be independent IF...THEN...END IF tests.
C        The tests were for IOSTAT errors on cascading write statements
C        nested in the IF...ELSE IF... statements, and this was
C        confusing. These tests were restructured so that IOSTAT is
C        tested after each write statement which is equicalent to the
C        original intent and easier to read.
C
C-    SPICELIB Version 1.0.0, 29-SEP-1993 (KRG)
C
C        This routine implements the notion of a file type for DAF
C        files. It allows type information to be added to the file ID
C        word.
C
C        This routine is a modified version of DAFOPN. See the revision
C        history of that entry point for details of changes before the
C        creation of this entry point.
C
C-&

C$ Index_Entries
C
C     open new daf with type
C
C-&

C$ Revisions
C
C-    SPICELIB Version 2.0.0, 03-MAR-1999 (FST)
C
C        See the Revisions section under DAFAH for a discussion
C        of the impact of the changes made for this version.
C
C-&

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFONW' )
      END IF

C
C     Initialize the handle list, if necessary.
C
      IF ( FIRST ) THEN
         CALL SSIZEI ( FTSIZE, FHLIST )
         FIRST = .FALSE.
      END IF

C
C     Check to see if there is room in the file table.
C
      IF ( NFT .EQ. FTSIZE ) THEN

         CALL SETMSG ( 'The file table is full, with # entries. '     //
     .                 'Could not open ''#''.'                         )
         CALL ERRINT ( '#', FTSIZE                                     )
         CALL ERRCH  ( '#', FNAME                                      )
         CALL SIGERR ( 'SPICE(DAFFTFULL)'                              )
         CALL CHKOUT ( 'DAFONW'                                        )
         RETURN

      END IF

C
C     Check if the file type is blank.
C
      IF ( FTYPE .EQ. ' ' ) THEN

         CALL SETMSG ( 'The file type is blank.' )
         CALL SIGERR ( 'SPICE(BLANKFILETYPE)'    )
         CALL CHKOUT ( 'DAFONW'                  )
         RETURN

      END IF

C
C     Check for nonprinting characters in the file type.
C
      FNB = LTRIM ( FTYPE )

      DO I = FNB, RTRIM ( FTYPE )

         IF ( ( ICHAR ( FTYPE(I:I) ) .GT. MAXPC )   .OR.
     .        ( ICHAR ( FTYPE(I:I) ) .LT. MINPC ) ) THEN

            CALL SETMSG ( 'The file type contains nonprinting' //
     .                    ' characters.'                        )
            CALL SIGERR ( 'SPICE(ILLEGALCHARACTER)'             )
            CALL CHKOUT ( 'DAFONW'                              )
            RETURN

         END IF

      END DO

C
C     Set the value the file type in a temporary variable to be sure of
C     its length and then set the value of the ID word. Only 4
C     characters are allowed for the file type, and they are the first
C     nonblank character and its three (3), or fewer, immediate
C     successors in the input string FTYPE.
C
      TTYPE  = FTYPE(FNB:)
      IDWORD = 'DAF/' // TTYPE

C
C     Make sure ND and NI are in range.
C
      IF (  ( ND .LT. 0 )  .OR.  ( ND .GT. MAXND )  ) THEN

         CALL SETMSG ( 'ND was #, should be in range [0,#].' )
         CALL ERRINT ( '#', ND                               )
         CALL ERRINT ( '#', MAXND                            )
         CALL SIGERR ( 'SPICE(DAFINVALIDPARAMS)'             )
         CALL CHKOUT ( 'DAFONW'                              )
         RETURN

      END IF

      IF (  ( NI .LT. 2 )  .OR.  ( NI .GT. MAXNI )  ) THEN

         CALL SETMSG ( 'NI was #, should be in range [2,#].' )
         CALL ERRINT ( '#', NI                               )
         CALL ERRINT ( '#', MAXNI                            )
         CALL SIGERR ( 'SPICE(DAFINVALIDPARAMS)'             )
         CALL CHKOUT ( 'DAFONW'                              )
         RETURN

      END IF

      IF (  ( ND + (NI+1)/2 )  .GT.  MAXSUM  ) THEN

         CALL SETMSG ( 'Summary size was #, should not exceed #.' )
         CALL ERRINT ( '#', ND + (NI+1)/2                         )
         CALL ERRINT ( '#', MAXSUM                                )
         CALL SIGERR ( 'SPICE(DAFINVALIDPARAMS)'                  )
         CALL CHKOUT ( 'DAFONW'                                   )
         RETURN

      END IF

C
C     The user must reserve some non-negative number of records.
C
      IF ( RESV .LT. 0 ) THEN

         CALL SETMSG ( 'An attempt was made to reserve a negative '   //
     .                 'number (#) of records.'                        )
         CALL ERRINT ( '#', RESV                                       )
         CALL SIGERR ( 'SPICE(DAFNORESV)'                              )
         CALL CHKOUT ( 'DAFONW'                                        )
         RETURN

      END IF

C
C     Attempt to create the file; perform any appropriate checks.
C
      CALL ZZDDHOPN ( FNAME, 'NEW', 'DAF', HANDLE )

C
C     Check FAILED(); return if an error has occurred.
C
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DAFONW' )
         RETURN
      END IF

      IFN    = IFNAME
      FND    = ND
      FNI    = NI
      FWARD  = RESV + 2
      BWARD  = FWARD
      CREC   = ' '

      CALL CLEARD ( 128, DREC )
      CALL DAFRWA ( FWARD+2, 1, FREE )

C
C     Fetch a logical unit for HANDLE.
C
      CALL ZZDDHHLU ( HANDLE, 'DAF', .FALSE., LUN )

C
C     Check FAILED(); return if an error has occurred.
C
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DAFONW' )
         RETURN
      END IF

C
C     Fetch the system file format.
C
      CALL ZZPLATFM ( 'FILE_FORMAT', FORMAT )

C
C     Write the new file record to the logical unit, LUN.
C
      CALL ZZDAFNFR ( LUN,
     .                IDWORD,
     .                FND,
     .                FNI,
     .                IFN,
     .                FWARD,
     .                BWARD,
     .                FREE,
     .                FORMAT  )

C
C     Check to see whether or not ZZDAFNFR generated an error writing
C     the file record to the logical unit.  In the event an error
C     occurs, checkout and return.
C
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DAFONW' )
         RETURN
      END IF

C
C     Write NULL filled reserved records.
C
      IF ( RESV .GT. 0 ) THEN

         DO I = 1, CRLEN
            CREC(I:I) = CHAR(0)
         END DO

         DO I = 2, RESV + 1
C
C            Place an end-of-comments marker in the first byte
C            of the first record.
C
            IF ( I .EQ. 2 ) THEN
               CREC(1:1) = CHAR(INTEOC)
            ELSE
               CREC(1:1) = CHAR(0)
            END IF

            WRITE ( LUN, REC=I, IOSTAT=IOSTAT ) CREC

            IF ( IOSTAT .NE. 0 ) THEN

               CALL ZZDDHCLS ( HANDLE, 'DAF', .TRUE. )
               CALL SETMSG ( 'Attempt to write file ''#'' failed. '   //
     .                       'Value of IOSTAT was #.'                  )
               CALL ERRCH  ( '#', FNAME                                )
               CALL ERRINT ( '#', IOSTAT                               )
               CALL SIGERR ( 'SPICE(DAFWRITEFAIL)'                     )
               CALL CHKOUT ( 'DAFONW'                                  )
               RETURN

            END IF

         END DO

      END IF

      WRITE ( LUN, REC=FWARD, IOSTAT=IOSTAT ) DREC

      IF ( IOSTAT .NE. 0 ) THEN

         CALL ZZDDHCLS ( HANDLE, 'DAF', .TRUE.                     )
         CALL SETMSG   ( 'Attempt to write file ''#'' failed. '   //
     .                   'Value of IOSTAT was #.'                  )
         CALL ERRCH    ( '#', FNAME                                )
         CALL ERRINT   ( '#', IOSTAT                               )
         CALL SIGERR   ( 'SPICE(DAFWRITEFAIL)'                     )
         CALL CHKOUT   ( 'DAFONW'                                  )
         RETURN

      END IF

      WRITE ( LUN, REC=FWARD+1, IOSTAT=IOSTAT ) CREC

      IF ( IOSTAT .NE. 0 ) THEN

         CALL ZZDDHCLS ( HANDLE, 'DAF', .TRUE.                     )
         CALL SETMSG   ( 'Attempt to write file ''#'' failed. '   //
     .                   'Value of IOSTAT was #.'                  )
         CALL ERRCH    ( '#', FNAME                                )
         CALL ERRINT   ( '#', IOSTAT                               )
         CALL SIGERR   ( 'SPICE(DAFWRITEFAIL)'                     )
         CALL CHKOUT   ( 'DAFONW'                                  )
         RETURN

      END IF

C
C     Update the file table to include information about our newly
C     opened DAF.
C
      NFT        = NFT + 1
      FTHAN(NFT) = HANDLE
      FTND(NFT)  = FND
      FTNI(NFT)  = FNI
      FTLNK(NFT) = 1

C
C     Insert the new handle into our handle set.
C
      CALL INSRTI ( HANDLE, FHLIST )

      CALL CHKOUT ( 'DAFONW' )
      RETURN




C$Procedure DAFOPN ( DAF, open new )

      ENTRY DAFOPN ( FNAME, ND, NI, IFNAME, RESV, HANDLE )

C$ Abstract
C
C     Deprecated. The routine DAFONW supersedes this routine.
C     NAIF supports this routine only to provide backward
C     compatibility.
C
C     Open a new DAF for subsequent write requests.
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
C     DAF
C
C$ Keywords
C
C     DAF
C     FILES
C
C$ Declarations
C
C     CHARACTER*(*)         FNAME
C     INTEGER               ND
C     INTEGER               NI
C     CHARACTER*(*)         IFNAME
C     INTEGER               RESV
C     INTEGER               HANDLE
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     FNAME      I   Name of DAF to be opened.
C     ND         I   Number of double precision components in summaries.
C     NI         I   Number of integer components in summaries.
C     IFNAME     I   Internal file name.
C     RESV       I   Number of records to reserve.
C     HANDLE     O   Handle assigned to DAF.
C
C$ Detailed_Input
C
C     FNAME       is the name of a new DAF to be created (and
C                 consequently open for write access).
C
C     ND          is the number of double precision components
C                 in each array summary of the new file.
C
C     NI          is the number of integer components in each
C                 array summary in the new file.
C
C     IFNAME      is the internal file name (containing as many as 60
C                 characters) for the new file. This should uniquely
C                 identify the file.
C
C     RESV        is the number of records in the new file to be
C                 reserved for non-DAF use. The user may reserve
C                 records 2 through (2 + RESV - 1) in the file.
C                 These records are not used to store DAF data,
C                 and are in fact invisible to all DAF routines.
C
C$ Detailed_Output
C
C     HANDLE      is the file handle associated with the file. This
C                 handle is used to identify the file in subsequent
C                 calls to other DAF routines.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     1) If the specified file cannot be opened without exceeding
C        the maximum number of files, the error SPICE(DAFFTFULL)
C        is signalled.
C
C     2) If the input argument ND is out of the range [0, 124]
C        or if NI is out of the range [2, 250], the error
C        SPICE(DAFINVALIDPARAMS) is signalled.
C
C     3) If
C
C           ND + ( NI + 1 ) / 2   >  125
C
C        the error SPICE(DAFINVALIDPARAMS) is signalled.
C
C     4) If the number of records to be reserved is not zero or
C        positive, the error SPICE(DAFNORESV) is signalled.
C
C     5) If an I/O error occurs in the process of creating the file,
C        routines in the call tree of this routine signal an error.
C
C     6) If (for some reason) the initial records in the file cannot
C        be written, the error SPICE(DAFWRITEFAIL) is signalled.
C
C     7) If no logical units are available, the error is
C        signaled by routines called by this routine.
C
C     8) If the file name is blank, or otherwise inappropriate
C        routines in the call tree of this routine signal an error.
C
C$ Files
C
C     See argument FNAME.
C
C$ Particulars
C
C     The DAFs created by DAFOPN have initialized file records but
C     do not yet contain any arrays.  See the DAF Required Reading
C     for a discussion of file records.
C
C     This entry point has been made obsolete by the entry point DAFONW.
C     It is supported for reasons of backward compatibility only. New
C     software development should use the entry point DAFONW.
C
C$ Examples
C
C     In the following code fragment, DAFOPN is used to open a file,
C     to which a new array is then added.
C
C        CALL DAFOPN   ( FNAME,  ND,  NI,  IFNAME, 0, HANDLE )
C
C        CALL DAFBNA   ( HANDLE, SUM, NAME  )
C        CALL GET_DATA ( DATA,   N,   FOUND )
C
C        DO WHILE ( FOUND )
C           CALL DAFADA   ( DATA, N        )
C           CALL GET_DATA ( DATA, N, FOUND )
C        END DO
C
C        CALL DAFENA
C
C$ Restrictions
C
C     1) Files opened using this routine must be closed with DAFCLS.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     K.R. Gehringer  (JPL)
C     N.J. Bachman    (JPL)
C     J.M. Lynch      (JPL)
C     H.A. Neilan     (JPL)
C     W.L. Taber      (JPL)
C     F.S. Turner     (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 8.1.1, 10-OCT-2012 (EDW)
C
C        Edited Abstract section to use "Deprecated" keyword
C        and state replacement routine.
C
C        Corrected ordering of header section.
C
C        Removed the obsolete Reference citation to "NAIF
C        Document 167.0."
C
C-    SPICELIB Version 8.1.0, 02-APR-2002 (FST)
C
C        This routine was updated to accomodate changes to the
C        handle manager interface.  See DAFAH's Revision section
C        for details.
C
C-    SPICELIB Version 8.0.0, 13-NOV-2001 (FST)
C
C        This routine was updated to utilize the new handle manager
C        software to manage binary file formats and consolidated
C        I/O code.
C
C-    SPICELIB Version 7.0.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  New
C        environments are WIN-NT
C
C-    SPICELIB Version 7.0.3, 16-SEP-1999 (NJB)
C
C        CSPICE environments were added.  Some typos were corrected.
C
C-    SPICELIB Version 7.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 7.0.1, 17-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 4.0.0, 03-MAR-1999 (FST)
C
C        The entry point was modified to insert the FTP validation
C        string, as well as the binary file format into the file record.
C
C-    SPICELIB Version 3.1.0, 08-MAR-1996 (KRG)
C
C        The modifications support the notion of a DAF comment area,
C        and involve writing NULL filled reserved records when the
C        number of reserved records is greater than zero (0).
C
C        Some nested IF...THEN...ELSE IF...THEN...END IF constructs
C        were expanded to be independent IF...THEN...END IF tests.
C        The tests were for IOSTAT errors on cascading write statements
C        nested in the IF...ELSE IF... statements, and this was
C        confusing. These tests were restructured so that IOSTAT is
C        tested after each write statement which is equicalent to the
C        original intent and easier to read.
C
C-    SPICELIB Version 3.0.0, 29-SEP-1993 (KRG)
C
C        Modified the logical structure of some
C           IF ... THEN ... ELSE IF... END IF
C        statements which were testing different items in each ELSE IF
C        clause for failure into separate IF ... END IF statements. This
C        improved the readability and supportability of the code.
C
C-    SPICELIB Version 2.1.0, 25-FEB-1993 (JML)
C
C        A new variable LUN is used for the logical unit number
C        returned by GETLUN.
C
C        The file name is checked to see if it is blank.
C
C        The file name string that is passed to the FORTRAN OPEN and
C        INQUIRE statements has been chopped at the last non-blank
C        character.
C
C-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 2.0.0, 03-SEP-1991 (NJB) (HAN) (WLT)
C
C        Updated to allow multiple DAFs to be open for write
C        access simultaneously.  An error in a calling sequence
C        shown in the Examples section was corrected.
C
C        This routine was updated so that it now keeps current the set
C        of DAF handles returned by DAFHOF.
C
C-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
C
C        Literature references added to the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&

C$ Index_Entries
C
C     open new daf
C
C-&

C$ Revisions
C
C-    SPICELIB Version 4.0.0, 03-MAR-1999 (FST)
C
C        See the Revisions section under DAFAH for a discussion
C        of the impact of the changes made for this version.
C
C-    SPICELIB Version 2.0.0, 03-SEP-1991 (NJB) (HAN) (WLT)
C
C        Updated to allow multiple DAFs to be open for write
C        access simultaneously.
C
C        This routine was updated so that it now keeps current the set
C        of DAF handles returned by DAFHOF.
C
C        Invalid values of ND and NI are now screened; two new
C        exceptions were added to the $Exceptions header section.
C
C        The calling sequence of DAFADA shown in the first example
C        in the Examples section was reversed; this was fixed.
C
C        Some error messages were changed so that they specify
C        names of relevant DAFs.
C-&

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFOPN' )
      END IF

C
C     Initialize the handle list, if necessary.
C
      IF ( FIRST ) THEN
         CALL SSIZEI ( FTSIZE, FHLIST )
         FIRST = .FALSE.
      END IF

C
C     Check to see if there is room in the file table.
C
      IF ( NFT .EQ. FTSIZE ) THEN

         CALL SETMSG ( 'The file table is full, with # entries. '     //
     .                 'Could not open ''#''.'                         )
         CALL ERRINT ( '#', FTSIZE                                     )
         CALL ERRCH  ( '#', FNAME                                      )
         CALL SIGERR ( 'SPICE(DAFFTFULL)'                              )
         CALL CHKOUT ( 'DAFOPN'                                        )
         RETURN

      END IF

C
C     Make sure ND and NI are in range.
C
      IF (  ( ND .LT. 0 )  .OR.  ( ND .GT. MAXND )  ) THEN

         CALL SETMSG ( 'ND was #, should be in range [0,#].' )
         CALL ERRINT ( '#', ND                               )
         CALL ERRINT ( '#', MAXND                            )
         CALL SIGERR ( 'SPICE(DAFINVALIDPARAMS)'             )
         CALL CHKOUT ( 'DAFOPN'                              )
         RETURN

      END IF

      IF (  ( NI .LT. 2 )  .OR.  ( NI .GT. MAXNI )  ) THEN

         CALL SETMSG ( 'NI was #, should be in range [2,#].' )
         CALL ERRINT ( '#', NI                               )
         CALL ERRINT ( '#', MAXNI                            )
         CALL SIGERR ( 'SPICE(DAFINVALIDPARAMS)'             )
         CALL CHKOUT ( 'DAFOPN'                              )
         RETURN

      END IF

      IF (  ( ND + (NI+1)/2 )  .GT.  MAXSUM  ) THEN

         CALL SETMSG ( 'Summary size was #, should not exceed #.' )
         CALL ERRINT ( '#', ND + (NI+1)/2                         )
         CALL ERRINT ( '#', MAXSUM                                )
         CALL SIGERR ( 'SPICE(DAFINVALIDPARAMS)'                  )
         CALL CHKOUT ( 'DAFOPN'                                   )
         RETURN

      END IF

C
C     The user must reserve some non-negative number of records.
C
      IF ( RESV .LT. 0 ) THEN

         CALL SETMSG ( 'An attempt was made to reserve a negative '   //
     .                 'number (#) of records.'                        )
         CALL ERRINT ( '#', RESV                                       )
         CALL SIGERR ( 'SPICE(DAFNORESV)'                              )
         CALL CHKOUT ( 'DAFOPN'                                        )
         RETURN

      END IF

C
C     Attempt to create the file; perform any appropriate checks.
C
      CALL ZZDDHOPN ( FNAME, 'NEW', 'DAF', HANDLE )

C
C     Check FAILED(); return if an error has occurred.
C
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DAFOPN' )
         RETURN
      END IF

      IFN    = IFNAME
      FND    = ND
      FNI    = NI
      FWARD  = RESV + 2
      BWARD  = FWARD
      CREC   = ' '

      CALL CLEARD ( 128, DREC )
      CALL DAFRWA ( FWARD+2, 1, FREE )

C
C     Fetch a logical unit for HANDLE.
C
      CALL ZZDDHHLU ( HANDLE, 'DAF', .FALSE., LUN )

C
C     Check FAILED(); return if an error has occurred.
C
      IF ( FAILED() ) THEN
         CALL CHKOUT( 'DAFOPN' )
         RETURN
      END IF

C
C     Fetch the system file format.
C
      CALL ZZPLATFM ( 'FILE_FORMAT', FORMAT )

C
C     Write the new file record to the logical unit, LUN.
C
      CALL ZZDAFNFR ( LUN,
     .                'NAIF/DAF',
     .                FND,
     .                FNI,
     .                IFN,
     .                FWARD,
     .                BWARD,
     .                FREE,
     .                FORMAT  )
C
C     Check to see whether or not ZZDAFNFR generated an error writing
C     the file record to the logical unit.  In the event an error
C     occurs, checkout and return.
C
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DAFOPN' )
         RETURN
      END IF

C
C     Write NULL filled reserved records.
C
      IF ( RESV .GT. 0 ) THEN

         DO I = 1, CRLEN
            CREC(I:I) = CHAR(0)
         END DO

         DO I = 2, RESV + 1

            WRITE ( LUN, REC=I, IOSTAT=IOSTAT ) CREC

            IF ( IOSTAT .NE. 0 ) THEN

               CALL ZZDDHCLS ( HANDLE, 'DAF', .TRUE. )

               CALL SETMSG ( 'Attempt to write file ''#'' failed. '   //
     .                       'Value of IOSTAT was #.'                  )
               CALL ERRCH  ( '#', FNAME                                )
               CALL ERRINT ( '#', IOSTAT                               )
               CALL SIGERR ( 'SPICE(DAFWRITEFAIL)'                     )
               CALL CHKOUT ( 'DAFOPN'                                  )
               RETURN

            END IF

         END DO

      END IF

      WRITE ( LUN, REC=FWARD, IOSTAT=IOSTAT ) DREC

      IF ( IOSTAT .NE. 0 ) THEN

         CALL ZZDDHCLS ( HANDLE, 'DAF', .TRUE. )

         CALL SETMSG ( 'Attempt to write file ''#'' failed. '   //
     .                 'Value of IOSTAT was #.'                  )
         CALL ERRCH  ( '#', FNAME                                )
         CALL ERRINT ( '#', IOSTAT                               )
         CALL SIGERR ( 'SPICE(DAFWRITEFAIL)'                     )
         CALL CHKOUT ( 'DAFOPN'                                  )
         RETURN

      END IF

      WRITE ( LUN, REC=FWARD+1, IOSTAT=IOSTAT ) CREC

      IF ( IOSTAT .NE. 0 ) THEN

         CALL ZZDDHCLS ( HANDLE, 'DAF', .TRUE. )

         CALL SETMSG ( 'Attempt to write file ''#'' failed. '   //
     .                 'Value of IOSTAT was #.'                  )
         CALL ERRCH  ( '#', FNAME                                )
         CALL ERRINT ( '#', IOSTAT                               )
         CALL SIGERR ( 'SPICE(DAFWRITEFAIL)'                     )
         CALL CHKOUT ( 'DAFOPN'                                  )
         RETURN

      END IF

C
C     Update the file table to include information about
C     our newly opened DAF.
C
      NFT        = NFT + 1
      FTHAN(NFT) = HANDLE
      FTND(NFT)  = FND
      FTNI(NFT)  = FNI
      FTLNK(NFT) = 1

C
C     Insert the new handle into our handle set.
C
      CALL INSRTI ( HANDLE, FHLIST )

      CALL CHKOUT ( 'DAFOPN' )
      RETURN




C$Procedure DAFCLS ( DAF, close )

      ENTRY DAFCLS ( HANDLE )

C$ Abstract
C
C     Close the DAF associated with a given handle.
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
C     DAF
C
C$ Keywords
C
C     DAF
C     FILES
C
C$ Declarations
C
C     INTEGER               HANDLE
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of DAF to be closed.
C
C$ Detailed_Input
C
C     HANDLE      is the file handle of a previously opened DAF file.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     1) If the specified handle does not belong to a DAF
C        that is currently open, nothing happens.
C
C     2) If this routine is used to close an HANDLE not associated
C        with a DAF, routines called by this routine signal an error.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Because DAFAH and its entry points must keep track of what
C     files are open at any given time, it is important that DAF
C     files be closed only with DAFCLS, to prevent the remaining
C     DAF routines from failing, sometimes mysteriously.
C
C     Note that when a file is opened more than once for read access,
C     DAFOPR returns the same handle each time it is re-opened.
C     Each time the file is closed, DAFCLS checks to see if any other
C     claims on the file are still active before physically closing
C     the file.
C
C$ Examples
C
C     Example(1):
C
C     In the following code fragment, the arrays in a file are
C     examined in order to determine whether the file contains
C     any arrays whose names begin with the word TEST.
C     The complete names for these arrays are printed to
C     the screen. The file is closed at the end of the search.
C
C        CALL DAFOPR ( FNAME, HANDLE )
C        CALL DAFBFS ( HANDLE )
C        CALL DAFFNA ( FOUND  )
C
C        DO WHILE ( FOUND )
C           CALL DAFGN ( NAME )
C
C           IF ( NAME(1:5) .EQ. 'TEST ' ) THEN
C              WRITE (*,*) NAME
C           END IF
C
C           CALL DAFFNA ( FOUND )
C        END DO
C
C        CALL DAFCLS ( HANDLE )
C
C     Note that if the file has been opened already by a DAF routine
C     at some other place in the calling program, it remains open.
C     This makes it possible to examine files that have been opened for
C     use by other modules without interfering with the operation of
C     those routines.
C
C
C     Example (2):
C
C     Use a simple routine to output the double precision and integer
C     values stored in an SPK's segments descriptors. This function
C     opens a DAF for read, performs a forwards search for the DAF
C     arrays, prints segments description for each array found, then
C     closes the DAF.
C
C           PROGRAM DAF_T
C
C           INTEGER             HANDLE
C
C     C
C     C     Define the summary parameters appropriate
C     C     for an SPK file.
C     C
C           INTEGER             ND
C           PARAMETER         ( ND = 2 )
C
C           INTEGER             NI
C           PARAMETER         ( NI = 6 )
C
C           INTEGER             IC( NI )
C
C           DOUBLE PRECISION    DC( ND )
C
C           CHARACTER*(32)      KERNEL
C
C           LOGICAL             FOUND
C
C
C     C
C     C     Open a DAF for read. Return a HANDLE referring to the file.
C     C
C           KERNEL = 'de421.bsp'
C           CALL DAFOPR ( KERNEL, HANDLE )
C
C     C
C     C     Begin a forward search on the file.
C     C
C           CALL DAFBFS ( HANDLE )
C
C     C
C     C     Search until a DAF array is found.
C     C
C           CALL DAFFNA ( FOUND )
C
C     C
C     C     Loop while the search finds subsequent DAF arrays.
C     C
C           DO WHILE ( FOUND )
C
C              CALL DAFGS ( SUM )
C              CALL DAFUS ( SUM, ND, NI, DC, IC )
C
C              WRITE(*,*)                'Doubles: ', DC(1:ND)
C              WRITE(*, FMT='(A,6I9)' ) 'Integers: ', IC(1:NI)
C
C     C
C     C        Check for another segment.
C     C
C              CALL DAFFNA ( FOUND )
C
C           END DO
C
C     C
C     C     Safely close the DAF.
C     C
C           CALL DAFCLS ( HANDLE )
C
C           END
C
C     The program outputs:
C
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         1        0        1        2      641   310404
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         2        0        1        2   310405   423048
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         3        0        1        2   423049   567372
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         4        0        1        2   567373   628976
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         5        0        1        2   628977   674740
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         6        0        1        2   674741   715224
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         7        0        1        2   715225   750428
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         8        0        1        2   750429   785632
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:         9        0        1        2   785633   820836
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:        10        0        1        2   820837   944040
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:       301        3        1        2   944041  1521324
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:       399        3        1        2  1521325  2098608
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:       199        1        1        2  2098609  2098620
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:       299        2        1        2  2098621  2098632
C      Doubles:   -3169195200.0000000        1696852800.0000000
C     Integers:       499        4        1        2  2098633  2098644
C
C      Note, the final entries in the integer array contains the segment
C      start/end indexes. The output indicates the search proceeded
C      from the start of the file (low value index) towards the end
C      (high value index).
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
C     N.J. Bachman    (JPL)
C     K.R. Gehringer  (JPL)
C     W.L. Taber      (JPL)
C     F.S. Turner     (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 8.1.1, 10-OCT-2012 (EDW)
C
C        Added a functional code example to the Examples section.
C
C        Removed the unneeded Revisions section.
C
C        Removed the obsolete Reference citation to "NAIF
C        Document 167.0."
C
C        Corrected ordering of header section.
C
C-    SPICELIB Version 8.1.0, 02-APR-2002 (FST)
C
C        This routine was updated to accomodate changes to the
C        handle manager interface.  See DAFAH's Revision section
C        for details.
C
C-    SPICELIB Version 8.0.0, 13-NOV-2001 (FST)
C
C        This routine was updated to utilize the new handle manager
C        software to manage binary file formats and consolidated
C        I/O code.
C
C-    SPICELIB Version 7.0.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  New
C        environments are WIN-NT
C
C-    SPICELIB Version 7.0.3, 16-SEP-1999 (NJB)
C
C        CSPICE environments were added.  Some typos were corrected.
C
C-    SPICELIB Version 7.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 7.0.1, 17-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 2.0.3, 29-SEP-1993 (KRG)
C
C        Removed references to specific DAF file open routines in the
C        $ Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if these open routines ever
C        change.
C
C-    SPICELIB Version 2.0.2, 25-FEB-1993 (JML)
C
C        A minor error in the particulars section of the header was
C        corrected.  It formerly stated that a file could be open more
C        than once for read or write access instead of just read access.
C
C-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 2.0.0, 03-SEP-1991 (NJB) (WLT)
C
C        This routine was updated so that it now keeps current the set
C        of DAF handles returned by DAFHOF.
C
C        Upgraded to support file handle checking routines
C        DAFHOF and DAFSIH.  DAFCLS now initializes the file
C        handle list if necessary, and removes from the list
C        the handles of files it closes.
C
C-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
C
C        Literature references added to the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&


C$ Index_Entries
C
C     close daf
C
C-&


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFCLS' )
      END IF

C
C     Initialize the handle list, if necessary.
C
      IF ( FIRST ) THEN
         CALL SSIZEI ( FTSIZE, FHLIST )
         FIRST = .FALSE.
      END IF

C
C     Is this file even open? If so, decrement the number of links
C     to the file. If the number of links drops to zero, physically
C     close the file and remove it from the file buffer.
C
C     If the file is not open: no harm, no foul.
C
      FINDEX = ISRCHI ( HANDLE, NFT, FTHAN )

      IF ( FINDEX .GT. 0 ) THEN

         FTLNK(FINDEX) = FTLNK(FINDEX) - 1

         IF ( FTLNK(FINDEX) .EQ. 0 ) THEN

            CALL ZZDDHCLS ( HANDLE, 'DAF', .FALSE. )

            DO I = FINDEX, NFT - 1
               FTHAN(I) = FTHAN(I+1)
               FTLNK(I) = FTLNK(I+1)
               FTND(I)  = FTND(I+1)
               FTNI(I)  = FTNI(I+1)
            END DO

            NFT = NFT - 1

C
C           Delete the handle from our handle set.
C
            CALL REMOVI ( HANDLE, FHLIST )

         END IF

      END IF

      CALL CHKOUT ( 'DAFCLS' )
      RETURN




C$Procedure DAFHSF ( DAF, handle to summary format )

      ENTRY DAFHSF ( HANDLE, ND, NI )

C$ Abstract
C
C     Return the summary format associated with a handle.
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
C     DAF
C
C$ Keywords
C
C     CONVERSION
C     DAF
C     FILES
C
C$ Declarations
C
C     INTEGER               HANDLE
C     INTEGER               ND
C     INTEGER               NI
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of a DAF file.
C     ND         O   Number of double precision components in summaries.
C     NI         O   Number of integer components in summaries.
C
C$ Detailed_Input
C
C     HANDLE      is the handle associated with a previously opened
C                 DAF file.
C
C$ Detailed_Output
C
C     ND,
C     NI          are the numbers of double precision and integer
C                 components, respectively, in each array summary
C                 in the specified file.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     1) If the specified handle does not belong to any file that is
C        currently known to be open, the error SPICE(DAFNOSUCHHANDLE)
C        is signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The summary format must be known in order to pack or unpack
C     an array summary.  See the DAF Required Reading for a discussion
C     of summary formats.
C
C$ Examples
C
C     1)  Find the number of d.p. `words' in a DAF having an
C         arbitrary summary format.
C
C
C                  PROGRAM NWORDS
C            C
C            C     Count the number of d.p. words of data in a
C            C     DAF.  Exclude array summaries, reserved records,
C            C     the file record, and character records.
C            C
C                  INTEGER               FILEN
C                  PARAMETER           ( FILEN  = 128 )
C
C                  INTEGER               MAXND
C                  PARAMETER           ( MAXND  = 124 )
C
C                  INTEGER               MAXNI
C                  PARAMETER           ( MAXNI  = 250 )
C
C                  INTEGER               MAXSUM
C                  PARAMETER           ( MAXSUM = 125 )
C
C                  CHARACTER*(FILEN)     DAF
C
C                  DOUBLE PRECISION      DC    ( MAXND  )
C                  DOUBLE PRECISION      SUM   ( MAXSUM )
C
C                  INTEGER               FA
C                  INTEGER               HANDLE
C                  INTEGER               IA
C                  INTEGER               IC    ( MAXNI )
C                  INTEGER               N
C                  INTEGER               ND
C                  INTEGER               NI
C
C                  LOGICAL               FOUND
C
C                  DATA                  N   /  0  /
C
C                  WRITE (*,*)          'Enter file name'
C                  READ  (*,FMT='(A)')   DAF
C
C            C
C            C     Open the DAF and find the summary format.
C            C
C                  CALL DAFOPR ( DAF,    HANDLE )
C                  CALL DAFHSF ( HANDLE, ND, NI )
C
C            C
C            C     Start a forward search and examine each array in
C            C     turn.
C            C
C                  CALL DAFBFS ( HANDLE )
C                  CALL DAFFNA ( FOUND  )
C
C                  DO WHILE ( FOUND )
C            C
C            C        Obtain the array summary, unpack it, and get
C            C        the initial and final array addresses from
C            C        the integer descriptor component.
C            C
C                     CALL DAFGS ( SUM )
C                     CALL DAFUS ( SUM, ND, NI, DC, IC )
C
C                     IA  =  IC ( NI - 1 )
C                     FA  =  IC ( NI     )
C
C                     N   =  FA - IA + 1 + N
C
C                     CALL DAFFNA ( FOUND )
C
C                  END DO
C
C                  WRITE (*,*) 'Number of d.p. words is ', N
C
C                  END
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     NONE.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     K.R. Gehringer  (JPL)
C     W.L. Taber      (JPL)
C     F.S. Turner     (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 8.0.1, 10-OCT-2012 (EDW)
C
C        Corrected ordering of header section.
C
C        Removed the obsolete Reference citation to "NAIF
C        Document 167.0."
C
C-    SPICELIB Version 8.0.0, 13-NOV-2001 (FST)
C
C        This routine was updated to utilize the new handle manager
C        software to manage binary file formats and consolidated
C        I/O code.
C
C-    SPICELIB Version 7.0.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  New
C        environments are WIN-NT
C
C-    SPICELIB Version 7.0.3, 16-SEP-1999 (NJB)
C
C        CSPICE environments were added.  Some typos were corrected.
C
C-    SPICELIB Version 7.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 7.0.1, 17-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 1.0.4, 29-SEP-1993 (KRG)
C
C        Removed references to specific DAF file open routines in the
C        $ Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if these open routines ever
C        change.
C
C-    SPICELIB Version 1.0.3, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.2, 03-SEP-1990 (NJB)
C
C        Example added to the $Examples section.
C
C-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
C
C        Literature references added to the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&

C$ Index_Entries
C
C     handle to daf summary format
C
C-&

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFHSF' )
      END IF

      FINDEX = ISRCHI ( HANDLE, NFT, FTHAN )

      IF ( FINDEX .GT. 0 ) THEN

         ND = FTND(FINDEX)
         NI = FTNI(FINDEX)

      ELSE

         CALL SETMSG ( 'There is no DAF open with handle = #' )
         CALL ERRINT ( '#', HANDLE                            )
         CALL SIGERR ( 'SPICE(DAFNOSUCHHANDLE)'               )

      END IF

      CALL CHKOUT ( 'DAFHSF' )
      RETURN




C$Procedure DAFHLU ( DAF, handle to logical unit )

      ENTRY DAFHLU ( HANDLE, UNIT )

C$ Abstract
C
C     Return the logical unit associated with a handle.
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
C     DAF
C
C$ Keywords
C
C     CONVERSION
C     DAF
C     FILES
C
C$ Declarations
C
C     INTEGER               HANDLE
C     INTEGER               UNIT
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of a DAF file.
C     UNIT       O   Corresponding logical unit.
C
C$ Detailed_Input
C
C     HANDLE      is the handle associated with a previously opened
C                 DAF file.
C
C$ Detailed_Output
C
C     UNIT        is the Fortran logical unit to which the file is
C                 connected.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     1) If an error occurs while attempting to fetch a logical
C        unit, routines in the call tree process and signal any
C        appropriate errors.  The value of UNIT in this case is
C        undefined.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The best reason for knowing the logical unit to which a DAF
C     is connected is to read or write from the records reserved in a
C     file. Since these records are by definition invisible to the DAF
C     routines, you must read and write them directly.
C
C$ Examples
C
C     In the following code fragment, the first reserved record in
C     a newly created DAF is used to store the name and address
C     of the person who created it.
C
C        FTYPE = 'TEST'
C        CALL DAFONW ( FNAME, FTYPE, 3, 6, IFNAME, 5, HANDLE )
C        CALL DAFHLU ( HANDLE, UNIT )
C
C        WRITE (UNIT,REC=2) 'Ellis Wyatt, JPL ',
C       .                   '4800 Oak Grove Drive ',
C       .                   'Room 301-125A ',
C       .                   'Pasadena, CA 91109'
C
C$ Restrictions
C
C     1) This routine may only be used to retrieve logical units
C        for DAFs loaded or created using the interfaces available
C        in this entry point umbrella.  Using this entry point to
C        retrieve units for files not loaded through these interfaces
C        may result in unexpected behavior.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     K.R. Gehringer  (JPL)
C     W.L. Taber      (JPL)
C     R.E. Thurman    (JPL)
C     F.S. Turner     (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 8.0.1, 10-OCT-2012 (EDW)
C
C        Corrected ordering of header section.
C
C        Removed the obsolete Reference citation to "NAIF
C        Document 167.0."
C
C-    SPICELIB Version 8.0.0, 13-NOV-2001 (FST)
C
C        This routine was updated to utilize the new handle manager
C        software to manage binary file formats and consolidated
C        I/O code.
C
C-    SPICELIB Version 7.0.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  New
C        environments are WIN-NT
C
C-    SPICELIB Version 7.0.3, 16-SEP-1999 (NJB)
C
C        CSPICE environments were added.  Some typos were corrected.
C
C-    SPICELIB Version 7.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 7.0.1, 17-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 1.0.3, 29-SEP-1993 (KRG)
C
C        Removed references to specific DAF file open routines in the
C        $ Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if these open routines ever
C        change.
C
C        Changed the example to use the new entry point DAFONW.
C
C-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
C
C        Literature references added to the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&

C$ Index_Entries
C
C     daf handle to logical unit
C
C-&

C$ Revisions
C
C-    SPICELIB Version 8.0.0, 15-NOV-2000 (FST)
C
C        Successfully invoking this module has the side effect of
C        locking UNIT to HANDLE.  This 'lock' guarentees until
C        HANDLE is closed (or unlocked) that the file associated
C        with HANDLE is always open and attached to logical unit
C        UNIT.  To unlock a handle without closing the file, use
C        ZZDDHUNL, an entry point in the handle manager umbrella,
C        ZZDDHMAN.
C
C        The system can lock at most UTSIZE-SCRUNT-RSVUNT
C        simultaneously (see the include file 'zzddhman.inc' for
C        specific values of these parameters), but unnecessarily
C        locking handles to their logical units may cause performance
C        degradation.  The handle manager will have less logical
C        units to utilize when disconnecting and reconnecting
C        loaded files.
C
C-    Beta Version 1.1.0, 1-NOV-1989 (RET)
C
C        DAFHLU now only checks in and checks out if the one exception
C        occurs. The purpose of this change was to help speed up a
C        routine that gets called constantly by higher level DAF
C        routines.
C
C-&

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFHLU' )
      END IF

      CALL ZZDDHHLU ( HANDLE, 'DAF', .TRUE., UNIT )

      CALL CHKOUT ( 'DAFHLU' )
      RETURN




C$Procedure DAFLUH ( DAF, logical unit to handle )

      ENTRY DAFLUH ( UNIT, HANDLE )

C$ Abstract
C
C     Return the handle associated with a logical unit.
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
C     DAF
C
C$ Keywords
C
C     CONVERSION
C     DAF
C     FILES
C
C$ Declarations
C
C     INTEGER               UNIT
C     INTEGER               HANDLE
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     UNIT       I   Logical unit connected to a DAF.
C     HANDLE     O   Corresponding DAF file handle.
C
C$ Detailed_Input
C
C     UNIT        is the logical unit to which a DAF has been
C                 connected after it has been opened.
C
C$ Detailed_Output
C
C     HANDLE      is the handle associated with the file.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     1) If the specified unit is not connected to any file that is
C        currently loaded as a DAF, the error SPICE(DAFNOSUCHUNIT)
C        is signaled.  The value of HANDLE returned is undefined in
C        this case.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     It is unlikely, but possible, that a calling program would know
C     the logical unit to which a file is connected without knowing the
C     handle associated with the file. DAFLUH is provided mostly for
C     completeness.
C
C$ Examples
C
C     In the following code fragment, the handle associated with
C     a DAF is retrieved using the logical unit to which the
C     file is connected. The handle is then used to determine the
C     name of the file.
C
C        CALL DAFLUH ( UNIT,   HANDLE )
C        CALL DAFHFN ( HANDLE, FNAME )
C
C$ Restrictions
C
C     1) This routine may only be used to retrieve handles for logical
C        units connected to DAFs loaded or created using the interfaces
C        available in this entry point umbrella.  Using this entry point
C        to retrieve handles for files not loaded through these
C        interfaces may result in unexpected behavior.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     K.R. Gehringer  (JPL)
C     W.L. Taber      (JPL)
C     F.S. Turner     (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 8.0.1, 10-OCT-2012 (EDW)
C
C        Corrected ordering of header section.
C
C        Removed the obsolete Reference citation to "NAIF
C        Document 167.0."
C
C-    SPICELIB Version 8.0.0, 13-NOV-2001 (FST)
C
C        This routine was updated to utilize the new handle manager
C        software to manage binary file formats and consolidated
C        I/O code.
C
C-    SPICELIB Version 7.0.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  New
C        environments are WIN-NT
C
C-    SPICELIB Version 7.0.3, 16-SEP-1999 (NJB)
C
C        CSPICE environments were added.  Some typos were corrected.
C
C-    SPICELIB Version 7.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 7.0.1, 17-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 1.0.3, 29-SEP-1993 (KRG)
C
C        Removed references to specific DAF file open routines in the
C        $ Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if these open routines ever
C        change.
C
C-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
C
C        Literature references added to the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&

C$ Index_Entries
C
C     logical unit to daf handle
C
C-&

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFLUH' )
      END IF

      CALL ZZDDHLUH ( UNIT, HANDLE, FOUND )

      IF ( .NOT. FOUND ) THEN
         HANDLE = 0
         CALL SETMSG ( 'There is no file open with unit = #' )
         CALL ERRINT ( '#', UNIT                             )
         CALL SIGERR ( 'SPICE(DAFNOSUCHUNIT)'                )
         CALL CHKOUT ( 'DAFLUH'                              )
         RETURN
      END IF

C
C     Now make certain that the HANDLE is associated with a DAF.
C
      CALL ZZDDHNFO ( HANDLE, DAFNAM, IARC, IBFF, IAMH, FOUND )

      IF ( IARC .NE. DAF ) THEN
         HANDLE = 0
         CALL SETMSG ( 'The file, ''#'', connected to unit # '
     .   //            'is not a DAF.'                         )
         CALL ERRFNM ( '#', UNIT                               )
         CALL ERRINT ( '#', UNIT                               )
         CALL SIGERR ( 'SPICE(DAFNOSUCHUNIT)'                  )
         CALL CHKOUT ( 'DAFLUH'                                )
         RETURN
      END IF

      CALL CHKOUT ( 'DAFLUH' )
      RETURN




C$Procedure DAFHFN ( DAF, handle to file name )

      ENTRY DAFHFN ( HANDLE, FNAME )

C$ Abstract
C
C     Return the name of the file associated with a handle.
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
C     DAF
C
C$ Keywords
C
C     CONVERSION
C     DAF
C     FILES
C
C$ Declarations
C
C     INTEGER               HANDLE
C     CHARACTER*(*)         FNAME
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of a DAF file.
C     FNAME      O   Corresponding file name.
C
C$ Detailed_Input
C
C     HANDLE      is the handle associated with a previously opened
C                 DAF file.
C
C$ Detailed_Output
C
C     UNIT        is the name of the file.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     1) If the specified handle does not belong to any file that is
C        currently known to be loaded as a DAF, the error
C        SPICE(DAFNOSUCHHANDLE) is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     It may be desirable to recover the names of one or more DAF
C     files in a different part of the program from the one in which
C     they were opened. Note that the names returned by DAFHFN may
C     not be identical to the names used to open the files. Under
C     most operating systems, a particular file can be accessed using
C     many different names. DAFHFN returns one of them.
C
C$ Examples
C
C     In the following code fragment, the name of a DAF is
C     recovered using the handle associated with the file.
C
C        CALL DAFOPR ( 'sample.DAF', HANDLE )
C         .
C         .
C
C        CALL DAFHFN ( HANDLE, FNAME )
C
C     Depending on the circumstances (operating system, compiler,
C     default directory) the value of FNAME might resemble any of
C     the following:
C
C        'USER$DISK:[WYATT.IMAGES]SAMPLE.DAF;4'
C
C        '/wyatt/images/sample.DAF'
C
C        'A:\IMAGES\SAMPLE.DAF'
C
C     On the other hand, it might not.
C
C$ Restrictions
C
C     1) This routine may only be used to retrieve the names of DAFs
C        loaded or created using the interfaces available in this entry
C        point umbrella.  Using this entry point to retrieve names for
C        files not loaded through these interfaces may result in
C        unexpected behavior.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     K.R. Gehringer  (JPL)
C     J.M. Lynch      (JPL)
C     W.L. Taber      (JPL)
C     F.S. Turner     (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 8.0.1, 10-OCT-2012 (EDW)
C
C        Corrected ordering of header section.
C
C        Removed the obsolete Reference citation to "NAIF
C        Document 167.0."
C
C-    SPICELIB Version 8.0.0, 13-NOV-2001 (FST)
C
C        This routine was updated to utilize the new handle manager
C        software to manage binary file formats and consolidated
C        I/O code.
C
C-    SPICELIB Version 7.0.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  New
C        environments are WIN-NT
C
C-    SPICELIB Version 7.0.3, 16-SEP-1999 (NJB)
C
C        CSPICE environments were added.  Some typos were corrected.
C
C-    SPICELIB Version 7.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 7.0.1, 17-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 1.1.1, 29-SEP-1993 (KRG)
C
C        Removed references to specific DAF file open routines in the
C        $ Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if these open routines ever
C        change.
C
C-    SPICELIB Version 1.1.0, 25-FEB-1993 (JML)
C
C        IOSTAT is checked after the INQUIRE statement.
C
C-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
C
C        Literature references added to the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&

C$ Index_Entries
C
C     daf handle to file name
C
C-&

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFHFN' )
      END IF

      CALL ZZDDHNFO ( HANDLE, DAFNAM, IARC, IBFF, IAMH, FOUND )

      IF ( ( .NOT. FOUND ) .OR. ( IARC .NE. DAF ) ) THEN
         CALL SETMSG ( 'There is no file open with handle = #' )
         CALL ERRINT ( '#', HANDLE                             )
         CALL SIGERR ( 'SPICE(DAFNOSUCHHANDLE)'                )
         CALL CHKOUT ( 'DAFHFN'                                )
         RETURN
      END IF

      FNAME = DAFNAM

      CALL CHKOUT ( 'DAFHFN' )
      RETURN




C$Procedure DAFFNH ( DAF, file name to handle )

      ENTRY DAFFNH ( FNAME, HANDLE )

C$ Abstract
C
C     Return handle associated with a file name.
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
C     DAF
C
C$ Keywords
C
C     CONVERSION
C     DAF
C     FILES
C
C$ Declarations
C
C     CHARACTER*(*)         FNAME
C     INTEGER               HANDLE
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     FNAME      I   Name of a DAF file.
C     HANDLE     O   Corresponding DAF file handle.
C
C$ Detailed_Input
C
C     FNAME       is the name of a previously opened DAF file.
C
C$ Detailed_Output
C
C     HANDLE      is the handle associated with the file.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     1) If the specified name does not specify any file currently known
C        to be loaded as a DAF the error SPICE(DAFNOSUCHFILE) is
C        signaled.  The value of HANDLE is undefined in this case.
C
C     2) If the file does not exist, an error is signaled by routines
C        in the call tree of this routine.  The value of HANDLE is
C        undefined in this case.
C
C     3) Any I/O errors generated in the process of connecting the
C        specified name with a handle cause errors to be signaled
C        by routines in the call tree of this routine.  The value of
C        HANDLE is undefined in this case.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     It is sometimes easier to work with file names (which are
C     meaningful, and often predictable) than with file handles
C     (which are neither), especially in interactive situations.
C     However, nearly every DAF routines requires that you use file
C     handles to refer to files. DAFFNH is provided to bridge the gap
C     between the two representations.
C
C$ Examples
C
C     In the following code fragment, the handle associated with a
C     DAF is recovered using the name of the file.
C
C        CALL DAFOPR ( 'sample.DAF', HANDLE )
C         .
C         .
C
C        CALL DAFFNH ( 'sample.DAF', HANDLE )
C
C$ Restrictions
C
C     1) Only file names of DAFs loaded with interfaces present in
C        this entry point umbrella should be passed into this routine.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     K.R. Gehringer  (JPL)
C     J.M. Lynch      (JPL)
C     H.A. Neilan     (JPL)
C     W.L. Taber      (JPL)
C     F.S. Turner     (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 8.1.1, 10-OCT-2012 (EDW)
C
C        Eliminated unneeded Revisions section.
C
C        Corrected ordering of header section.
C
C        Removed the obsolete Reference citation to "NAIF
C        Document 167.0."
C
C-    SPICELIB Version 8.1.0, 02-APR-2002 (FST)
C
C        Fixed a bug, where an error was signaled but the call to
C        CHKOUT and the RETURN statement were omitted.
C
C-    SPICELIB Version 8.0.0, 13-NOV-2001 (FST)
C
C        This routine was updated to utilize the new handle manager
C        software to manage binary file formats and consolidated
C        I/O code.
C
C        In previous version of DAFAH, this module simply
C        performed an INQUIRE on FNAME and looked in the
C        file table for the logical unit returned.
C
C        The integration of the new handle manager interfaces
C        into this entry point has the possibility of increasing
C        the complexity of this routine when more than UTSIZE
C        files are loaded.  Essentially, when given an arbitrary
C        name, a total of FTSIZE INQUIRE statements may be executed
C        to accurately connect FNAME with HANDLE.  See ZZDDHFNH and
C        ZZDDHF2H for details.
C
C-    SPICELIB Version 7.0.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  New
C        environments are WIN-NT
C
C-    SPICELIB Version 7.0.3, 16-SEP-1999 (NJB)
C
C        CSPICE environments were added.  Some typos were corrected.
C
C-    SPICELIB Version 7.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 7.0.1, 17-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 2.0.1, 29-SEP-1993 (KRG)
C
C        Removed references to specific DAF file open routines in the
C        $ Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if these open routines ever
C        change.
C
C-    SPICELIB Version 2.0.0, 25-FEB-1993 (JML)
C
C        The INQUIRE statement that checks if the file is open now also
C        checks that the file exists. Two new exceptions were added as
C        a result of this change.
C
C        A RETURN statement was added after the error signalled when
C        the file is not open.
C
C        The file name is checked to see if it is blank.
C
C        The file name string that is passed to the FORTRAN INQUIRE
C        statement has been chopped at the last non-blank character.
C
C-    SPICELIB Version 1.1.2, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.1.1,  18-SEP-1991 (HAN)
C
C        The Revisions section was incorrectly named Version. This has
C        been fixed.
C
C-    SPICELIB Version 1.1.0,  5-NOV-1990 (HAN)
C
C        Call to CHKIN was corrected. The module was checking in
C        as 'DAFFHN'.
C
C-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
C
C        Literature references added to the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&

C$ Index_Entries
C
C     file name to daf handle
C
C-&

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFFNH' )
      END IF

      CALL ZZDDHFNH ( FNAME, HANDLE, FOUND )

      IF ( ( .NOT. FOUND ) ) THEN
         HANDLE = 0
         CALL SETMSG ( 'There is no file in the DAF table with '  //
     .                 'file name = ''#'''                          )
         CALL ERRCH  ( '#', FNAME                                   )
         CALL SIGERR ( 'SPICE(DAFNOSUCHFILE)'                       )
         CALL CHKOUT ( 'DAFFNH'                                     )
         RETURN
      END IF

C
C     Now make certain that HANDLE is associated with a DAF.
C
      CALL ZZDDHNFO ( HANDLE, DAFNAM, IARC, IBFF, IAMH, FOUND )

      IF ( IARC .NE. DAF ) THEN
         HANDLE = 0
         CALL SETMSG ( 'The file, ''#'', is not a DAF.' )
         CALL ERRCH  ( '#', FNAME                       )
         CALL SIGERR ( 'SPICE(DAFNOSUCHFILE)'           )
         CALL CHKOUT ( 'DAFFNH'                         )
         RETURN
      END IF

      CALL CHKOUT ( 'DAFFNH' )
      RETURN




C$Procedure DAFHOF ( DAF, handles of open files )

      ENTRY DAFHOF ( FHSET )

C$ Abstract
C
C     Return a SPICELIB set containing the handles of all currently
C     open DAFS.
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
C     DAF
C     SETS
C
C$ Keywords
C
C     DAF
C     FILES
C
C$ Declarations
C
C     INTEGER               LBCELL
C     PARAMETER           ( LBCELL = -5 )
C
C     INTEGER               FHSET ( LBCELL : * )
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     FHSET      O   A set containing handles of currently open DAFS.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     FHSET          is a SPICELIB set containing the file handles of
C                    all currently open DAFs.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the set FHSET is not initialized, the error will be
C         diagnosed by routines called by this routine.
C
C     2)  If the set FHSET is too small to accommodate the set of
C         handles to be returned, the error will be diagnosed by
C         routines called by this routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine allows subroutines to test file handles for
C     validity before performing operations on them, such as
C     finding the name of the file designated by a handle.  Many
C     DAF operations on handles cause errors to be signalled if
C     the handles are invalid.
C
C$ Examples
C
C     1)  Find out how may DAFs are open for writing.
C
C            C
C            C    Find out which DAFs are open.
C            C
C                 CALL DAFHOF  ( FHSET )
C
C            C
C            C    Count the ones open for writing.  These have
C            C    negative file handles.
C            C
C                 COUNT = 0
C
C                 DO I = 1, CARDC(FHSET)
C
C                    IF ( FHSET(I) .LT. 0 ) THEN
C                       COUNT = COUNT + 1
C                    END IF
C
C                 END DO
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
C     N.J. Bachman   (JPL)
C     W.L. Taber     (JPL)
C     F.S. Turner    (JPL)
C
C$ Version
C
C-    SPICELIB Version 8.0.1, 10-OCT-2012 (EDW)
C
C        Corrected ordering of header section.
C
C-    SPICELIB Version 8.0.0, 13-NOV-2001 (FST)
C
C        This routine was updated to utilize the new handle manager
C        software to manage binary file formats and consolidated
C        I/O code.
C
C-    SPICELIB Version 7.0.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  New
C        environments are WIN-NT
C
C-    SPICELIB Version 7.0.3, 16-SEP-1999 (NJB)
C
C        CSPICE environments were added.  Some typos were corrected.
C
C-    SPICELIB Version 7.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 7.0.1, 17-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 03-SEP-1991 (NJB) (WLT)
C
C-&

C$ Index_Entries
C
C     return the set of handles for open daf files
C
C-&

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFHOF' )
      END IF

C
C     Initialize the handle list, if necessary.
C
      IF ( FIRST ) THEN

         CALL SSIZEI ( FTSIZE, FHLIST )
         FIRST = .FALSE.

      END IF

C
C     Just stuff our local list into the set.
C
      CALL COPYI ( FHLIST, FHSET )

      CALL CHKOUT ( 'DAFHOF' )
      RETURN




C$Procedure DAFSIH ( DAF, signal invalid handles )

      ENTRY DAFSIH ( HANDLE, ACCESS )

C$ Abstract
C
C     Signal an error if a DAF file handle does not designate a DAF
C     that is open for a specified type of access.
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
C     DAF
C     ERROR
C     SETS
C
C$ Keywords
C
C     DAF
C     FILES
C
C$ Declarations
C
C     INTEGER               HANDLE
C     CHARACTER*(*)         ACCESS
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   HANDLE to be validated.
C     ACCESS     I   String indicating access type.
C
C$ Detailed_Input
C
C     HANDLE         is a DAF handle to validate.  For HANDLE to be
C                    considered valid, it must specify a DAF that is
C                    open for the type of access specified by the input
C                    argument ACCESS.
C
C
C     ACCESS         is a string indicating the type of access that
C                    the DAF specified by the input argument HANDLE
C                    must be open for.  The values of ACCESS may be
C
C
C                       'READ'      File must be open for read access
C                                   by DAF routines.  All open DAFs
C                                   may be read.
C
C                       'WRITE'     File must be open for write access
C                                   by DAF routines.
C
C                                   Note that files open for write
C                                   access may be read as well as
C                                   written.
C
C
C                    Leading and trailing blanks in ACCESS are ignored,
C                    and case is not significant.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input argument ACCESS has an unrecognized value,
C         the error SPICE(INVALIDOPTION) is signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine signals the error SPICE(DAFINVALIDACCESS) if the
C     DAF designated by the input argument HANDLE is not open
C     for the specified type of access.  If HANDLE does not designate
C     an open DAF, the error SPICE(DAFNOSUCHHANDLE) is signalled.
C
C     This routine allows subroutines to test file handles for
C     validity before performing operations on them, such as
C     finding the name of the file designated by a handle.  Many
C     DAF operations on handles may cause unpredictable program
C     behavior if the handles are invalid.  This routine should
C     be used in situations where the appropriate action to take upon
C     determining that a handle is invalid is to signal an error.
C     DAFSIH centralizes the error response for this type of error in a
C     single routine.
C
C     In cases where it is necessary to determine the validity of a
C     file handle, but it is not an error for the handle to refer
C     to a closed file, the entry point DAFHOF should be used instead
C     of DAFSIH.
C
C$ Examples
C
C     1)  Add data to a DAF specified by a file handle.  Signal an
C         error if the file is not open for writing.  Check the
C         SPICELIB error status function FAILED after calling
C         DAFSIH, so that the routine will return if DAFSIH
C         signalled an error (we're presuming that this code
C         fragment would be used in a subroutine).
C
C            C
C            C     Check that HANDLE is valid, then add data to the
C            C     file specified by HANDLE.
C            C
C                  CALL DAFSIH  (  HANDLE, 'WRITE' )
C
C                  IF ( FAILED() ) THEN
C                     RETURN
C                  END IF
C
C                  CALL DAFBNA (  HANDLE,  SUM,    NAME )
C                  CALL DAFADA (  DATA,    N            )
C                  CALL DAFENA
C
C     2)  Find the size of an array in a DAF specified by a file
C         handle.  Signal an error if the file is not open for reading.
C
C            C
C            C     Check that HANDLE is valid, then obtain the
C            C     current array summary and compute the size of
C            C     the current array.
C            C
C                  CALL DAFSIH  ( HANDLE, 'READ' )
C
C                  IF ( FAILED() ) THEN
C                     RETURN
C                  END IF
C
C            C
C            C     Obtain the summary format, then the integer and d.p.
C            C     components of the summary.  Finally, compute the
C            C     array length.
C            C
C                  CALL DAFHSF (  HANDLE, ND, NI          )
C                  CALL DAFGS  (  SUMMRY                  )
C                  CALL DAFUS  (  SUMMRY, ND, NI, DC, IC  )
C
C                  IA      =  IC( NI - 1 )
C                  FA      =  IC( NI     )
C                  LENGTH  =  FA  -  IA  +  1
C
C     3)  Make sure that a file handle designates an open DAF.  Signal
C         an error if it does not.
C
C         Note that if a DAF is open at all, read access is allowed.
C
C                  CALL DAFSIH ( HANDLE, 'READ' )
C
C                  IF ( FAILED() ) THEN
C                     RETURN
C                  END IF
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
C     K.R. Gehringer (JPL)
C     N.J. Bachman   (JPL)
C     J.M. Lynch     (JPL)
C     W.L. Taber     (JPL)
C     F.S. Turner    (JPL)
C
C$ Version
C
C-    SPICELIB Version 8.0.1, 10-OCT-2012 (EDW)
C
C        Corrected ordering of header section.
C
C-    SPICELIB Version 8.0.0, 13-NOV-2001 (FST)
C
C        This routine was updated to utilize the new handle manager
C        software to manage binary file formats and consolidated
C        I/O code.
C
C-    SPICELIB Version 7.0.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  New
C        environments are WIN-NT
C
C-    SPICELIB Version 7.0.3, 16-SEP-1999 (NJB)
C
C        CSPICE environments were added.  Some typos were corrected.
C
C-    SPICELIB Version 7.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 7.0.1, 17-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given.  Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 1.2.1, 29-SEP-1993 (KRG)
C
C        Removed references to specific DAF file open routines in the
C        $ Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if these open routines ever
C        change.
C
C-    SPICELIB Version 1.2.0, 25-FEB-1993 (JML)
C
C        IOSTAT is now checked after the INQUIRE statement.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 03-SEP-1991 (NJB) (WLT)
C
C-&

C$ Index_Entries
C
C     signal an error for invalid daf handles
C
C-&

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFSIH' )
      END IF

C
C     Initialize the handle list, if necessary.
C
      IF ( FIRST ) THEN

         CALL SSIZEI ( FTSIZE, FHLIST )
         FIRST = .FALSE.

      END IF

C
C     Get an upper case, left-justified copy of ACCESS.
C
      CALL LJUST ( ACCESS, ACC )
      CALL UCASE ( ACC,    ACC )

C
C     Make sure we recognize the access type specified by the caller.
C
      IF (  ( ACC .NE. 'READ' ) .AND. ( ACC .NE. 'WRITE' )  ) THEN

         CALL SETMSG ( 'Unrecognized access type.  Type was #. ' )
         CALL ERRCH  ( '#', ACCESS                               )
         CALL SIGERR ( 'SPICE(INVALIDOPTION)'                    )
         CALL CHKOUT ( 'DAFSIH'                                  )
         RETURN

      END IF

C
C     Retrieve information about this HANDLE.
C
      CALL ZZDDHNFO ( HANDLE, DAFNAM, IARC, IBFF, IAMH, FOUND )

C
C     See whether the input handle is in our list at all.  It's
C     unlawful for the handle to be absent.  All open DAFs are
C     readable, so in the case that ACC is 'READ', we're done if
C     the DAF is open.
C
      IF ( ( .NOT. FOUND ) .OR. ( .NOT. ELEMI( HANDLE, FHLIST ) ) ) THEN

         CALL SETMSG ( 'There is no file open with handle = #' )
         CALL ERRINT ( '#', HANDLE                             )
         CALL SIGERR ( 'SPICE(DAFNOSUCHHANDLE)'                )
         CALL CHKOUT ( 'DAFSIH'                                )
         RETURN

C
C     If the access type is 'WRITE', the DAF must be open for writing.
C     This is not the case if the value of IAMH returned from the handle
C     manager is not READ.
C
      ELSE IF ( ( ACC .EQ. 'WRITE' ) .AND. ( IAMH .EQ. READ ) ) THEN

         CALL SETMSG ( 'DAF not open for write.  Handle = #, ' //
     .                    'file = ''#'''                         )
         CALL ERRINT ( '#', HANDLE                               )
         CALL ERRCH  ( '#', DAFNAM                               )
         CALL SIGERR ( 'SPICE(DAFINVALIDACCESS)'                 )
         CALL CHKOUT ( 'DAFSIH'                                  )
         RETURN

      END IF

C
C     The DAF's handle is o.k.
C
      CALL CHKOUT ( 'DAFSIH' )
      RETURN
      END

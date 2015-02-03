C$Procedure  ZZEKPAGE ( Private: Manage EK DAS paging system )
 
      SUBROUTINE ZZEKPAGE (  HANDLE,  TYPE,   ADDRSS,  STAT,  P,
     .                       PAGEC,   PAGED,  PAGEI,   BASE,  VALUE  )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Manage EK DAS paging system.
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
C     EK
C
C$ Keywords
C
C     EK
C     PRIVATE
C
C$ Declarations
 
      INCLUDE 'ekarch.inc'
      INCLUDE 'ekpage.inc'
      INCLUDE 'ektype.inc'
 
      INTEGER               HANDLE
      INTEGER               TYPE
      INTEGER               ADDRSS
      CHARACTER*(*)         STAT
      INTEGER               P
      CHARACTER*(*)         PAGEC
      DOUBLE PRECISION      PAGED ( * )
      INTEGER               PAGEI ( * )
      INTEGER               BASE
      INTEGER               VALUE
 
C$ Brief_I/O
C
C     Variable  I/O  Entries
C     --------  ---  --------------------------------------------------
C     HANDLE     I   PGIN, PGAN, PGAL, PGFR, PGRx, PGWx, PGST.
C     TYPE       I   PGBS, PGPG.
C     ADDRSS     I   PGPG.
C     STAT       I   PGST.
C     P         I-O  PGAN, PGAL, PGFR, PGRx, PGWx, PGBS, PGPG.
C     PAGEC     I-O  PGRC, PGWC.
C     PAGED     I-O  PGRD, PGWD.
C     PAGEI     I-O  PGRI, PGWI.
C     BASE       O   PGAN, PGAL, PGBS, PGPG.
C     VALUE      O   PGST.
C
C$ Detailed_Input
C
C     See the entry points for descriptions of their inputs.
C
C$ Detailed_Output
C
C     See the entry points for descriptions of their outputs.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If this routine is called directly, the error
C         SPICE(BOGUSENTRY) will be signalled.
C
C     See the entry points for discussions of errors particular to
C     those routines.
C
C$ Files
C
C     This suite of routines provides paged access to DAS files.  Only
C     DAS files initialized via a call to ZZEKPGIN may be written or
C     read by these routines.
C
C$ Particulars
C
C     The EK paging system provides a means for the rest of the EK
C     system to allocate and deallocate contiguous blocks of DAS
C     addresses of character, d.p. and integer type.  The rest of the EK
C     system never accesses EK files directly; it only reads and writes
C     pages allocated via this system.
C
C     Much of the page allocation and de-allocation performed by
C     higher-level routines is done via the routines ZZEKAPS and
C     ZZEKDPS; those routines should be called if applicable, rather
C     than ZZEKPGAL, ZZEKPGAN, or ZZEKPGFR.
C
C$ Examples
C
C     Initialization:               see EKOPN.
C     Page allocation:              see EKAPS.
C     Writing:                      see ZZEKAD01, ZZEKAD02, ZZEKAD03.
C     Reading:                      see ZZEKRD01, ZZEKRD02, ZZEKRD03.
C     Freeing pages:                see ZZEKDPS.
C     Address-to-page mapping:      see EKDELR.
C     Page number-to-base mapping:  see ZZEKAD0x
C
C$ Restrictions
C
C     1) Only `empty' DAS files may be initialized for paged access.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 20-OCT-1995 (NJB)
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               EQSTR
      LOGICAL               FAILED
 
C
C     Local variables
C
C
C     Note:  the integer fill buffer should be as large as the maximum
C     of the integer page size and the metadata area size.
C
      CHARACTER*(PGSIZC)    CFILL
      CHARACTER*(5)         ENCPAG
 
      DOUBLE PRECISION      DFILL  ( PGSIZD )
      DOUBLE PRECISION      DPPTR
 
      INTEGER               ADDR
      INTEGER               E
      INTEGER               FORWRD
      INTEGER               FREEC
      INTEGER               FREED
      INTEGER               FREEI
      INTEGER               IFILL  ( PGSIZI )
      INTEGER               L
      INTEGER               LASTC
      INTEGER               LASTD
      INTEGER               LASTI
      INTEGER               NFREEC
      INTEGER               NFREED
      INTEGER               NFREEI
      INTEGER               NPC
      INTEGER               NPD
      INTEGER               NPI
      INTEGER               UNIT
 
C
C     Saved variables
C
      SAVE
 
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
      RETURN
 
 
 
 
 
 
C$Procedure  ZZEKPGIN ( Private: Initialize DAS for paged access )
 
      ENTRY ZZEKPGIN ( HANDLE )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Initialize an open DAS file for paged access.
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
C     EK
C
C$ Keywords
C
C     EK
C     PRIVATE
C
C$ Declarations
C
C     INTEGER               HANDLE
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   DAS file handle.
C
C$ Detailed_Input
C
C     HANDLE         is a handle of a DAS file open for write access.
C                    The file must be empty:  the last address of
C                    each type (character, d.p. and integer) must be
C                    zero.
C
C$ Detailed_Output
C
C     None.  This routine operates by side effects; see $Particulars
C     for a description of the effect of this routine.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the DAS file designated by HANDLE is not empty, the error
C         SPICE(DASNOTEMPTY) is signalled.
C
C     2)  Any read or write errors detected during reading or writing
C         the DAS file will be diagnosed by routines called by this
C         routine.
C
C$ Files
C
C     This suite of routines provides paged access to DAS files.  Only
C     DAS files initialized via a call to ZZEKPGIN may be written or
C     read by these routines.
C
C$ Particulars
C
C     This routine initializes a DAS file for paged access.
C     Initialization consists of:
C
C        - Setting up the metadata area.  This structure is defined in
C          the include file ekpage.inc.  For each data type, there is
C          a free list pointer and an allocated page count.
C
C        - Writing the architecture code to the file.  This code is
C          defined in the include file ekarch.inc.
C
C$ Examples
C
C     See EKOPN.
C
C$ Restrictions
C
C     1) Only `empty' DAS files may be initialized for paged access.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 18-OCT-1995 (NJB)
C
C-&
 
      CALL CHKIN ( 'ZZEKPGIN' )
 
C
C     The file must be open for write access.
C
      CALL DASSIH ( HANDLE, 'WRITE' )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZEKPGIN' )
         RETURN
      END IF
 
C
C     Find out which addresses are already in use.  A file containing
C     data cannot be initialized.
C
      CALL DASLLA ( HANDLE, LASTC, LASTD, LASTI )
 
      IF (      ( LASTC .GT. 0 )
     .     .OR. ( LASTD .GT. 0 )
     .     .OR. ( LASTI .GT. 0 ) ) THEN
 
         CALL DASHLU ( HANDLE, UNIT )
         CALL SETMSG ( 'File # contains data; LASTC = #; LASTD = #; '//
     .                 'LASTI = #.'                                  )
         CALL ERRFNM ( '#',  UNIT                                    )
         CALL ERRINT ( '#',  LASTC                                   )
         CALL ERRINT ( '#',  LASTD                                   )
         CALL ERRINT ( '#',  LASTI                                   )
         CALL SIGERR ( 'SPICE(DASNOTEMPTY)'                          )
         CALL CHKOUT ( 'ZZEKPGIN'                                    )
         RETURN
 
      END IF
 
C
C     Initialize our fill buffers.
C
      CALL FILLC ( ' ',   1,       CFILL )
      CALL FILLD ( 0.D0,  PGSIZD,  DFILL )
      CALL FILLI ( 0,     PGSIZI,  IFILL )
 
C
C     Initialize enough integer addresses to hold the metadata area.
C
      CALL DASADI ( HANDLE, PGBASI, IFILL )
 
C
C     Set the architecture code.
C
      CALL DASUDI ( HANDLE, EPARCH, EPARCH, ARCHID )
 
C
C     Set the page sizes and base addresses.
C
      CALL DASUDI ( HANDLE, EPPSZC, EPPSZC, PGSIZC )
      CALL DASUDI ( HANDLE, EPPSZD, EPPSZD, PGSIZD )
      CALL DASUDI ( HANDLE, EPPSZI, EPPSZI, PGSIZI )
 
      CALL DASUDI ( HANDLE, EPBASC, EPBASC, PGBASC )
      CALL DASUDI ( HANDLE, EPBASD, EPBASD, PGBASD )
      CALL DASUDI ( HANDLE, EPBASI, EPBASI, PGBASI )
 
C
C     Since the integer fill value is zero, and since zero is
C     interpreted as null pointer, all pointers are initialized.
C
      CALL CHKOUT ( 'ZZEKPGIN' )
      RETURN
 
 
 
 
 
C$Procedure  ZZEKPGAN ( Private: EK, allocate new page )
 
      ENTRY ZZEKPGAN ( HANDLE, TYPE, P, BASE )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Allocate a new page of a specified data type.
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
C     EK
C
C$ Keywords
C
C     EK
C     PRIVATE
C
C$ Declarations
C
C     INTEGER               HANDLE
C     INTEGER               TYPE
C     INTEGER               P
C     INTEGER               BASE
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Paged EK file handle.
C     TYPE       I   Data type of page to allocate.
C     P          O   Page number.
C     BASE       O   DAS base address of page.
C
C$ Detailed_Input
C
C     HANDLE         is a handle of a paged EK file.  The file must
C                    be open for write access.
C
C     TYPE           is the data type of the page to allocate.  The
C                    type may be CHR, DP, or INT.  Values of these
C                    parameters are defined in ektype.inc.
C
C$ Detailed_Output
C
C     P              is the number of an allocated page.  The returned
C                    page is never taken from the free list; it is
C                    the lowest-addressed page of the specifed type
C                    that has never been allocated.
C
C     BASE           is the base DAS address of the page.  This address
C                    is the predecessor of the first DAS word of the
C                    page.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the DAS file designated by HANDLE is not open for paged
C         write access, the error will be diagnosed by routines called
C         by this routine.
C
C     2)  Any read or write errors detected during reading or writing
C         the DAS file will be diagnosed by routines called by this
C         routine.
C
C     3)  If the requested data type is not recognized, the error
C         SPICE(INVALIDTYPE) is signalled.
C
C$ Files
C
C     This suite of routines provides paged access to DAS files.  Only
C     DAS files initialized via a call to ZZEKPGIN may be written or
C     read by these routines.
C
C$ Particulars
C
C     The pages returned by this routine lie on DAS record boundaries.
C     Successive requests for pages of the same data type will return
C     pages that are adjacent in the DAS address space of that type.
C     In fact, the main reason to call this routine rather than
C     ZZEKPGAL is to allocate adjacent pages.
C
C     Use ZZEKPGAL for normal allocation.
C
C$ Examples
C
C     See EKAPS.
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
C     N.J. Bachman       (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 18-OCT-1995 (NJB)
C
C-&
 
      CALL CHKIN ( 'ZZEKPGAN' )
 
C
C     Validate the file.
C
      CALL ZZEKPGCH ( HANDLE, 'WRITE' )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZEKPGAN' )
         RETURN
      END IF
 
 
      IF ( TYPE .EQ. CHR ) THEN
C
C        The new page follows the last character address.
C
         CALL DASADC ( HANDLE, PGSIZC, 1, PGSIZC, CFILL )
 
C
C        Update the character page count.
C
         CALL DASRDI ( HANDLE, EPNPC, EPNPC, NPC   )
         CALL DASUDI ( HANDLE, EPNPC, EPNPC, NPC+1 )
 
C
C        Set the page number and base address.
C
         P     =  NPC + 1
         BASE  =  NPC * PGSIZC
 
 
      ELSE IF ( TYPE .EQ. DP ) THEN
 
         CALL DASADD ( HANDLE, PGSIZD, DFILL )
 
         CALL DASRDI ( HANDLE, EPNPD, EPNPD, NPD   )
         CALL DASUDI ( HANDLE, EPNPD, EPNPD, NPD+1 )
 
         P     =  NPD + 1
         BASE  =  NPD * PGSIZD
 
 
      ELSE IF ( TYPE .EQ. INT ) THEN
 
         CALL DASADI ( HANDLE, PGSIZI, IFILL )
 
         CALL DASRDI ( HANDLE, EPNPI, EPNPI, NPI   )
         CALL DASUDI ( HANDLE, EPNPI, EPNPI, NPI+1 )
 
         P     =  NPI + 1
         BASE  =  NPI * PGSIZI  +  PGBASI
 
 
      ELSE
 
         CALL SETMSG ( 'The data type code # was not recognized.' )
         CALL ERRINT ( '#', TYPE                                  )
         CALL SIGERR ( 'SPICE(INVALIDTYPE)'                       )
         CALL CHKOUT ( 'ZZEKPGAN'                                 )
         RETURN
 
      END IF
 
      CALL CHKOUT ( 'ZZEKPGAN' )
      RETURN
 
 
 
 
C$Procedure  ZZEKPGAL ( Private: EK, allocate page )
 
      ENTRY ZZEKPGAL ( HANDLE, TYPE, P, BASE )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Allocate a page of a specified data type.  The page need not
C     be new:  free pages are returned if possible.
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
C     EK
C
C$ Keywords
C
C     EK
C     PRIVATE
C
C$ Declarations
C
C     INTEGER               HANDLE
C     INTEGER               TYPE
C     INTEGER               P
C     INTEGER               BASE
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Paged EK file handle.
C     TYPE       I   Data type of page to allocate.
C     P          O   Page number.
C     BASE       O   DAS base address of page.
C
C$ Detailed_Input
C
C     HANDLE         is a handle of a paged EK file.  The file must
C                    be open for write access.
C
C     TYPE           is the data type of the page to allocate.  The
C                    type may be CHR, DP, or INT.  Values of these
C                    parameters are defined in ektype.inc.
C
C$ Detailed_Output
C
C     P              is the number of an allocated page.  The returned
C                    page is taken from the free list if the free list
C                    is non-empty; otherwise, a new page is returned.
C
C     BASE           is the base DAS address of the page.  This address
C                    is the predecessor of the first DAS word of the
C                    page.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the DAS file designated by HANDLE is not open for paged
C         write access, the error will be diagnosed by routines called
C         by this routine.
C
C     2)  Any read or write errors detected during reading or writing
C         the DAS file will be diagnosed by routines called by this
C         routine.
C
C     3)  If the requested data type is not recognized, the error
C         SPICE(INVALIDTYPE) is signalled.
C
C$ Files
C
C     This suite of routines provides paged access to DAS files.  Only
C     DAS files initialized via a call to ZZEKPGIN may be written or
C     read by these routines.
C
C$ Particulars
C
C     This routine should be used for page allocation, except for
C     applications requiring allocation of contiguous pages.  If
C     contiguous pages are required, use ZZEKPGAN.
C
C$ Examples
C
C     See EKAPS.
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
C     N.J. Bachman       (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 18-OCT-1995 (NJB)
C
C-&
      CALL CHKIN ( 'ZZEKPGAL' )
 
C
C     Validate the file.
C
      CALL ZZEKPGCH ( HANDLE, 'WRITE' )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZEKPGAL' )
         RETURN
      END IF
 
 
      IF ( TYPE .EQ. CHR ) THEN
C
C        If the character free list is non-empty, take a page from
C        that list.
C
         CALL DASRDI ( HANDLE, EPFPC, EPFPC, FREEC )
 
 
         IF ( FREEC .GT. 0 ) THEN
C
C           We'll return the first free page.
C
            P       =     FREEC
C
C           The new head of the list is the successor of FREEC, if
C           any.  Obtain the forward pointer from the page.
C
            ADDR  =   ( FREEC - 1 ) * PGSIZC  + 1
 
            CALL DASRDC ( HANDLE,  ADDR,  ADDR+4,  1,  5,  ENCPAG )
            CALL PRTDEC ( ENCPAG,  FORWRD  )
 
            FREEC =   FORWRD
 
C
C           Decrement the free page count, and write the free pointer
C           back to the file.
C
            CALL DASRDI ( HANDLE, EPNFPC, EPNFPC, NFREEC   )
            CALL DASUDI ( HANDLE, EPNFPC, EPNFPC, NFREEC-1 )
            CALL DASUDI ( HANDLE, EPFPC,  EPFPC,  FREEC    )
 
C
C           Set base address.
C
            BASE  =  (P-1) * PGSIZC
 
 
         ELSE
C
C           The new page follows the last character address.
C
            CALL DASADC ( HANDLE, PGSIZC, 1, PGSIZC, CFILL )
 
C
C           Update the character page count.
C
            CALL DASRDI ( HANDLE, EPNPC, EPNPC, NPC   )
            CALL DASUDI ( HANDLE, EPNPC, EPNPC, NPC+1 )
 
C
C           Set the page number and base address.
C
            P     =  NPC + 1
            BASE  =  NPC * PGSIZC
 
         END IF
 
 
 
      ELSE IF ( TYPE .EQ. DP ) THEN
C
C        If the d.p. free list is non-empty, take a page from
C        that list.
C
         CALL DASRDI ( HANDLE, EPFPD, EPFPD, FREED )
 
 
         IF ( FREED .GT. 0 ) THEN
C
C           We'll return the first free page.
C
            P       =     FREED
C
C           The new head of the list is the successor of FREED, if
C           any.  Obtain the forward pointer from the page.
C
            ADDR  =   ( FREED - 1 ) * PGSIZD   +  1
 
            CALL DASRDD ( HANDLE, ADDR, ADDR, DPPTR )
            FREED =   NINT(DPPTR)
 
C
C           Decrement the free page count, and write the free pointer
C           back to the file.
C
            CALL DASRDI ( HANDLE, EPNFPD, EPNFPD, NFREED   )
            CALL DASUDI ( HANDLE, EPNFPD, EPNFPD, NFREED-1 )
            CALL DASUDI ( HANDLE, EPFPD,  EPFPD,  FREED    )
 
C
C           Set base address.
C
            BASE  =  (P-1) * PGSIZD
 
 
         ELSE
C
C           The new page follows the last d.p. address.
C
            CALL DASADD ( HANDLE, PGSIZD, DFILL )
 
C
C           Update the d.p. page count.
C
            CALL DASRDI ( HANDLE, EPNPD, EPNPD, NPD   )
            CALL DASUDI ( HANDLE, EPNPD, EPNPD, NPD+1 )
 
C
C           Set the page number and base address.
C
            P     =  NPD + 1
            BASE  =  NPD * PGSIZD
 
         END IF
 
 
      ELSE IF ( TYPE .EQ. INT ) THEN
C
C        If the integer free list is non-empty, take a page from
C        that list.
C
         CALL DASRDI ( HANDLE, EPFPI, EPFPI, FREEI )
 
 
         IF ( FREEI .GT. 0 ) THEN
C
C           We'll return the first free page.
C
            P     =     FREEI
C
C           The new head of the list is the successor of FREEI, if
C           any.  Obtain the forward pointer from the page.
C
            ADDR  =   ( FREEI - 1 ) * PGSIZI   +   PGBASI  +  1
 
            CALL DASRDI ( HANDLE, ADDR, ADDR, FREEI )
 
C
C           Decrement the free page count, and write the free pointer
C           back to the file.
C
            CALL DASRDI ( HANDLE, EPNFPI, EPNFPI, NFREEI   )
            CALL DASUDI ( HANDLE, EPNFPI, EPNFPI, NFREEI-1 )
            CALL DASUDI ( HANDLE, EPFPI,  EPFPI,  FREEI    )
 
C
C           Set base address.
C
            BASE  =  (P-1) * PGSIZI  +  PGBASI
 
 
         ELSE
 
            CALL DASADI ( HANDLE, PGSIZI, IFILL )
 
            CALL DASRDI ( HANDLE, EPNPI, EPNPI, NPI   )
            CALL DASUDI ( HANDLE, EPNPI, EPNPI, NPI+1 )
 
            P     =  NPI + 1
            BASE  =  NPI * PGSIZI  +  PGBASI
 
         END IF
 
 
      ELSE
 
         CALL SETMSG ( 'The data type code # was not recognized.' )
         CALL ERRINT ( '#', TYPE                                  )
         CALL SIGERR ( 'SPICE(INVALIDTYPE)'                       )
         CALL CHKOUT ( 'ZZEKPGAL'                                 )
         RETURN
 
      END IF
 
      CALL CHKOUT ( 'ZZEKPGAL' )
      RETURN
 
 
 
 
 
 
C$Procedure  ZZEKPGFR ( Private: EK, free page )
 
      ENTRY ZZEKPGFR ( HANDLE, TYPE, P )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Free a specified page.
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
C     EK
C
C$ Keywords
C
C     EK
C     PRIVATE
C
C$ Declarations
C
C     INTEGER               HANDLE
C     INTEGER               TYPE
C     INTEGER               P
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Paged EK file handle.
C     TYPE       I   Data type of page to allocate.
C     P          I   Page number.
C
C$ Detailed_Input
C
C     HANDLE         is a handle of a paged EK file.  The file must
C                    be open for write access.
C
C     TYPE           is the data type of the page to allocate.  The
C                    type may be CHR, DP, or INT.  Values of these
C                    parameters are defined in ektype.inc.
C
C     P              is the number of an allocated page to be freed.
C
C$ Detailed_Output
C
C    None.  See $Particulars for a description of the effect of this
C    routine.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the DAS file designated by HANDLE is not open for paged
C         write access, the error will be diagnosed by routines called
C         by this routine.
C
C     2)  Any read or write errors detected during reading or writing
C         the DAS file will be diagnosed by routines called by this
C         routine.
C
C     3)  If the requested data type is not recognized, the error
C         SPICE(INVALIDTYPE) is signalled.
C
C     4)  If the number of the page to be freed is not that of an
C         allocated page of the specified type, the error
C         SPICE(INVALIDINDEX) is signalled.
C
C$ Files
C
C     This suite of routines provides paged access to DAS files.  Only
C     DAS files initialized via a call to ZZEKPGIN may be written or
C     read by these routines.
C
C$ Particulars
C
C     This routine should be used for page deallocation.  The input
C     page is placed at the head of the free list of the specified
C     data type.
C
C$ Examples
C
C     See EKDPS.
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
C     N.J. Bachman       (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 18-OCT-1995 (NJB)
C
C-&
      CALL CHKIN ( 'ZZEKPGFR' )
 
C
C     Check the file.
C
      CALL ZZEKPGCH ( HANDLE, 'WRITE' )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZEKPGFR' )
         RETURN
      END IF
 
 
      IF ( TYPE .EQ. CHR ) THEN
C
C        Validate the page number.  Find out how many pages are
C        out there.
C
         CALL DASRDI ( HANDLE, EPNPC, EPNPC, NPC )
 
         IF (  ( P .LT. 1 ) .OR. ( P .GT. NPC )  ) THEN
 
            CALL SETMSG ( 'Attempt to free non-existent CHR page. ' //
     .                    'Page number = #; valid range is 1:#'     )
            CALL ERRINT ( '#',  P                                   )
            CALL ERRINT ( '#',  NPC                                 )
            CALL SIGERR ( 'SPICE(INVALIDINDEX)'                     )
            CALL CHKOUT ( 'ZZEKPGFR'                                )
            RETURN
 
         END IF
 
C
C        Get the current character free pointer and free page count.
C
         CALL DASRDI ( HANDLE, EPFPC,  EPFPC,  FREEC  )
         CALL DASRDI ( HANDLE, EPNFPC, EPNFPC, NFREEC )
 
C
C        Insert into the freed page a pointer to the head of the
C        free list.
C
         CALL PRTENC ( FREEC, ENCPAG )
         ADDR  =  (P-1)*PGSIZC + 1
 
         CALL DASUDC ( HANDLE, ADDR, ADDR+4, 1, 5, ENCPAG )
 
C
C        Update the current character free pointer and free page count.
C
         CALL DASUDI ( HANDLE, EPFPC,  EPFPC,  P        )
         CALL DASUDI ( HANDLE, EPNFPC, EPNFPC, NFREEC+1 )
 
 
      ELSE IF ( TYPE .EQ. DP ) THEN
C
C        Validate the page number.  Find out how many pages are
C        out there.
C
         CALL DASRDI ( HANDLE, EPNPD, EPNPD, NPD )
 
         IF (  ( P .LT. 1 ) .OR. ( P .GT. NPD )  ) THEN
 
            CALL SETMSG ( 'Attempt to free non-existent DP page. ' //
     .                    'Page number = #; valid range is 1:#'     )
            CALL ERRINT ( '#',  P                                   )
            CALL ERRINT ( '#',  NPD                                 )
            CALL SIGERR ( 'SPICE(INVALIDINDEX)'                     )
            CALL CHKOUT ( 'ZZEKPGFR'                                )
            RETURN
 
         END IF
 
C
C        Get the current d.p. free pointer and free page count.
C
         CALL DASRDI ( HANDLE, EPFPD,  EPFPD,  FREED  )
         CALL DASRDI ( HANDLE, EPNFPD, EPNFPD, NFREED )
 
C
C        Insert into the freed page a pointer to the head of the
C        free list.
C
         ADDR  =  (P-1)*PGSIZD + 1
 
         CALL DASUDD ( HANDLE, ADDR, ADDR, DBLE(FREED) )
 
C
C        Update the current d.p. free pointer and free page count.
C
         CALL DASUDI ( HANDLE, EPFPD,  EPFPD,  P        )
         CALL DASUDI ( HANDLE, EPNFPD, EPNFPD, NFREED+1 )
 
 
 
 
      ELSE IF ( TYPE .EQ. INT ) THEN
C
C        Validate the page number.  Find out how many pages are
C        out there.
C
         CALL DASRDI ( HANDLE, EPNPI, EPNPI, NPI )
 
         IF (  ( P .LT. 1 ) .OR. ( P .GT. NPI )  ) THEN
 
            CALL SETMSG ( 'Attempt to free non-existent INT page. ' //
     .                    'Page number = #; valid range is 1:#'     )
            CALL ERRINT ( '#',  P                                   )
            CALL ERRINT ( '#',  NPI                                 )
            CALL SIGERR ( 'SPICE(INVALIDINDEX)'                     )
            CALL CHKOUT ( 'ZZEKPGFR'                                )
            RETURN
 
         END IF
 
C
C        Get the current integer free pointer and free page count.
C
         CALL DASRDI ( HANDLE, EPFPI,  EPFPI,  FREEI  )
         CALL DASRDI ( HANDLE, EPNFPI, EPNFPI, NFREEI )
 
C
C        Insert into the freed page a pointer to the head of the
C        free list.
C
         ADDR  =  (P-1)*PGSIZI  +  PGBASI  +  1
 
         CALL DASUDI ( HANDLE, ADDR, ADDR, FREEI )
 
C
C        Update the current integer free pointer and free page count.
C
         CALL DASUDI ( HANDLE, EPFPI,  EPFPI,  P        )
         CALL DASUDI ( HANDLE, EPNFPI, EPNFPI, NFREEI+1 )
 
 
      ELSE
 
         CALL SETMSG ( 'The data type code # was not recognized.' )
         CALL ERRINT ( '#', TYPE                                  )
         CALL SIGERR ( 'SPICE(INVALIDTYPE)'                       )
         CALL CHKOUT ( 'ZZEKPGFR'                                 )
         RETURN
 
      END IF
 
      CALL CHKOUT ( 'ZZEKPGFR' )
      RETURN
 
 
 
 
C$Procedure  ZZEKPGRC ( Private: EK, read character page )
 
      ENTRY ZZEKPGRC ( HANDLE, P, PAGEC )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Read a specified character page.
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
C     EK
C
C$ Keywords
C
C     EK
C     PRIVATE
C
C$ Declarations
C
C     INTEGER               HANDLE
C     INTEGER               P
C     CHARACTER*(*)         PAGEC
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Paged EK file handle.
C     P          I   Page number.
C     PAGEC      O   Character page.
C
C$ Detailed_Input
C
C     HANDLE         is a handle of a paged EK file.  The file may
C                    be open for read or write access.
C
C     P              is the number of a character page to read.
C
C$ Detailed_Output
C
C     PAGEC          is a string containing the contents of the
C                    specified page.  PAGEC should be declared with
C                    length at PGSIZC characters.  This parameter is
C                    declared in the include file ekpage.inc.
C
C                    If PAGEC has length less than PGSIZC characters,
C                    the output will be truncated on the right.  If
C                    PAGEC is longer than PGSIZC characters, the output
C                    will be padded with trailing blanks.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  Any errors detected during reading the DAS file will be
C         diagnosed by routines called by this routine.
C
C     2)  If the number of the page to read is not that of an
C         allocated character page, the error SPICE(INVALIDINDEX) is
C         signalled.
C
C$ Files
C
C     This suite of routines provides paged access to DAS files.  Only
C     DAS files initialized via a call to ZZEKPGIN may be written or
C     read by these routines.
C
C$ Particulars
C
C     This routine should be used to read character pages.
C
C$ Examples
C
C     See ZZEKRD03.
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
C     N.J. Bachman       (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 18-OCT-1995 (NJB)
C
C-&
 
C
C     Use discovery check-in.
C
C
C     Find out how many character pages are in use.
C
      CALL DASRDI ( HANDLE, EPNPC, EPNPC, NPC )
 
      IF (  ( P .LT. 1 )  .OR.  ( P .GT. NPC )  ) THEN
 
         CALL CHKIN  ( 'ZZEKPGRC'                           )
         CALL SETMSG ( 'CHR page = #; valid range is [1:#]' )
         CALL ERRINT ( '#', P                               )
         CALL ERRINT ( '#', NPC                             )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                )
         CALL CHKOUT ( 'ZZEKPGRC'                           )
         RETURN
 
      END IF
 
      L      =   LEN( PAGEC )
      E      =   MIN( L, PGSIZC )
 
      ADDR   =   (P-1)*PGSIZC + 1
 
      CALL DASRDC ( HANDLE, ADDR, ADDR+PGSIZC-1, 1, E, PAGEC )
 
      IF ( L .GT. E ) THEN
         PAGEC ( E+1: ) = ' '
      END IF
 
      RETURN
 
 
 
 
C$Procedure  ZZEKPGRD ( Private: EK, read d.p. page )
 
      ENTRY ZZEKPGRD ( HANDLE, P, PAGED )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Read a specified double precision page.
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
C     EK
C
C$ Keywords
C
C     EK
C     PRIVATE
C
C$ Declarations
C
C     INTEGER               HANDLE
C     INTEGER               P
C     DOUBLE PRECISION      PAGED ( * )
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Paged EK file handle.
C     P          I   Page number.
C     PAGED      O   Double precision page.
C
C$ Detailed_Input
C
C     HANDLE         is a handle of a paged EK file.  The file may
C                    be open for read or write access.
C
C     P              is the number of a double precision page to read.
C
C$ Detailed_Output
C
C     PAGED          is a double precision array containing the contents
C                    of the specified page.  PAGED should be declared
C                    with dimension PGSIZD.  This parameter is
C                    declared in the include file ekpage.inc.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  Any errors detected during reading the DAS file will be
C         diagnosed by routines called by this routine.
C
C     2)  If the number of the page to read is not that of an
C         allocated double precision page, the error SPICE(INVALIDINDEX)
C         is signalled.
C
C$ Files
C
C     This suite of routines provides paged access to DAS files.  Only
C     DAS files initialized via a call to ZZEKPGIN may be written or
C     read by these routines.
C
C$ Particulars
C
C     This routine should be used to read double precision pages.
C
C$ Examples
C
C     See ZZEKRD02.
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
C     N.J. Bachman       (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 18-OCT-1995 (NJB)
C
C-&
 
C
C     Use discovery check-in.
C
C
C     Find out how many d.p. pages are in use.
C
      CALL DASRDI ( HANDLE, EPNPD, EPNPD, NPD )
 
      IF (  ( P .LT. 1 )  .OR.  ( P .GT. NPD )  ) THEN
 
         CALL CHKIN  ( 'ZZEKPGRD'                           )
         CALL SETMSG ( 'DP page = #; valid range is [1:#]'  )
         CALL ERRINT ( '#', P                               )
         CALL ERRINT ( '#', NPD                             )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                )
         CALL CHKOUT ( 'ZZEKPGRD'                           )
         RETURN
 
      END IF
 
      ADDR   =   (P-1)*PGSIZD + 1
 
      CALL DASRDD ( HANDLE, ADDR, ADDR+PGSIZD-1, PAGED )
 
      RETURN
 
 
 
 
 
C$Procedure  ZZEKPGRI ( Private: EK, read integer page )
 
      ENTRY ZZEKPGRI ( HANDLE, P, PAGEI )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Read a specified integer page.
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
C     EK
C
C$ Keywords
C
C     EK
C     PRIVATE
C
C$ Declarations
C
C     INTEGER               HANDLE
C     INTEGER               P
C     INTEGER               PAGEI ( * )
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Paged EK file handle.
C     P          I   Page number.
C     PAGEI      O   Integer page.
C
C$ Detailed_Input
C
C     HANDLE         is a handle of a paged EK file.  The file may
C                    be open for read or write access.
C
C     P              is the number of an integer page to read.
C
C$ Detailed_Output
C
C     PAGEI          is an integer array containing the contents
C                    of the specified page.  PAGEI should be declared
C                    with dimension PGSIZI.  This parameter is
C                    declared in the include file ekpage.inc.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  Any errors detected during reading the DAS file will be
C         diagnosed by routines called by this routine.
C
C     2)  If the number of the page to read is not that of an
C         allocated double precision page, the error SPICE(INVALIDINDEX)
C         is signalled.
C
C$ Files
C
C     This suite of routines provides paged access to DAS files.  Only
C     DAS files initialized via a call to ZZEKPGIN may be written or
C     read by these routines.
C
C$ Particulars
C
C     This routine should be used to read integer pages.
C
C$ Examples
C
C     See ZZEKRD01.
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
C     N.J. Bachman       (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 18-OCT-1995 (NJB)
C
C-&
 
C
C     Use discovery check-in.
C
C
C     Find out how many integer pages are in use.
C
      CALL DASRDI ( HANDLE, EPNPI, EPNPI, NPI )
 
      IF (  ( P .LT. 1 )  .OR.  ( P .GT. NPI )  ) THEN
 
         CALL CHKIN  ( 'ZZEKPGRI'                           )
         CALL SETMSG ( 'INT page = #; valid range is [1:#]' )
         CALL ERRINT ( '#', P                               )
         CALL ERRINT ( '#', NPI                             )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                )
         CALL CHKOUT ( 'ZZEKPGRI'                           )
         RETURN
 
      END IF
 
      ADDR   =   PGBASI  +  (P-1)*PGSIZI + 1
 
      CALL DASRDI ( HANDLE, ADDR, ADDR+PGSIZI-1, PAGEI )
 
      RETURN
 
 
 
 
 
 
C$Procedure  ZZEKPGWC ( Private: EK, write character page )
 
      ENTRY ZZEKPGWC ( HANDLE, P, PAGEC )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Write a specified character page.
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
C     EK
C
C$ Keywords
C
C     EK
C     PRIVATE
C
C$ Declarations
C
C     INTEGER               HANDLE
C     INTEGER               P
C     CHARACTER*(*)         PAGEC
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Paged EK file handle.
C     P          I   Page number.
C     PAGEC      I   Character page.
C
C$ Detailed_Input
C
C     HANDLE         is a handle of a paged EK file.  The file must
C                    be open for write access.
C
C     P              is the number of an allocated character page to
C                    write.
C
C     PAGEC          is a string to be written to the specified page.
C                    PAGEC must be declared with length at PGSIZC
C                    characters.  This parameter is declared in the
C                    include file ekpage.inc.
C
C$ Detailed_Output
C
C     None.  See $Particulars for a description of the effect of this
C     routine.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  Any errors detected during reading or writing the DAS file
C         will be diagnosed by routines called by this routine.
C
C     2)  If the number of the page to write is not that of an
C         allocated character page, the error SPICE(INVALIDINDEX) is
C         signalled.
C
C     3)  If the input string has length less than PGSIZC characters,
C         the error SPICE(STRINGTOOSHORT) is signalled.
C
C$ Files
C
C     This suite of routines provides paged access to DAS files.  Only
C     DAS files initialized via a call to ZZEKPGIN may be written or
C     read by these routines.
C
C$ Particulars
C
C     This routine writes the input string to the DAS address range
C     corresponding to the specified page.  The file must be closed
C     properly (via EKCLS) in order to make the change permanent.
C
C$ Examples
C
C     See ZZEKAD03.
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
C     N.J. Bachman       (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 18-OCT-1995 (NJB)
C
C-&
 
C
C     Use discovery check-in.
C
C     Validate the file.
C
      CALL ZZEKPGCH ( HANDLE, 'WRITE' )
 
      IF ( FAILED() ) THEN
         RETURN
      END IF
 
C
C     Find out how many character pages are in use.
C
      CALL DASRDI ( HANDLE, EPNPC, EPNPC, NPC )
 
      IF (  ( P .LT. 1 )  .OR.  ( P .GT. NPC )  ) THEN
 
         CALL CHKIN  ( 'ZZEKPGWC'                           )
         CALL SETMSG ( 'CHR page = #; valid range is [1:#]' )
         CALL ERRINT ( '#', P                               )
         CALL ERRINT ( '#', NPC                             )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                )
         CALL CHKOUT ( 'ZZEKPGWC'                           )
         RETURN
 
      END IF
 
 
      L  =  LEN( PAGEC )
 
      IF ( L .LT. PGSIZC ) THEN
 
         CALL CHKIN  ( 'ZZEKPGWC'                                    )
         CALL SETMSG ( 'Input CHR page size = #; valid size is [#:]' )
         CALL ERRINT ( '#', L                                        )
         CALL ERRINT ( '#', PGSIZC                                   )
         CALL SIGERR ( 'SPICE(STRINGTOOSHORT)'                       )
         CALL CHKOUT ( 'ZZEKPGWC'                                    )
         RETURN
 
      END IF
 
 
      ADDR   =   (P-1)*PGSIZC + 1
 
      CALL DASUDC ( HANDLE, ADDR, ADDR+PGSIZC-1, 1, PGSIZC, PAGEC )
 
      RETURN
 
 
 
 
 
C$Procedure  ZZEKPGWD ( Private: EK, write d.p. page )
 
      ENTRY ZZEKPGWD ( HANDLE, P, PAGED )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Write a specified double precision page.
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
C     EK
C
C$ Keywords
C
C     EK
C     PRIVATE
C
C$ Declarations
C
C     INTEGER               HANDLE
C     INTEGER               P
C     DOUBLE PRECISION      PAGED ( * )
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Paged EK file handle.
C     P          I   Page number.
C     PAGED      I   Double precision page.
C
C$ Detailed_Input
C
C     HANDLE         is a handle of a paged EK file.  The file must
C                    be open for write access.
C
C     P              is the number of an allocated double precision
C                    page to write.
C
C     PAGED          is a double precision array to be written to
C                    the specified page.  PAGED must be declared with
C                    dimension at PGSIZD.  This parameter is
C                    declared in the include file ekpage.inc.
C
C$ Detailed_Output
C
C     None.  See $Particulars for a description of the effect of this
C     routine.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  Any errors detected during reading or writing the DAS file
C         will be diagnosed by routines called by this routine.
C
C     2)  If the number of the page to write is not that of an
C         allocated d.p. page, the error SPICE(INVALIDINDEX) is
C         signalled.
C
C$ Files
C
C     This suite of routines provides paged access to DAS files.  Only
C     DAS files initialized via a call to ZZEKPGIN may be written or
C     read by these routines.
C
C$ Particulars
C
C     This routine writes the input array to the DAS address range
C     corresponding to the specified page.  The file must be closed
C     properly (via EKCLS) in order to make the change permanent.
C
C$ Examples
C
C     See ZZEKAD02.
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
C     N.J. Bachman       (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 18-OCT-1995 (NJB)
C
C-&
 
C
C     Use discovery check-in.
C
C
C     Validate the file.
C
      CALL ZZEKPGCH ( HANDLE, 'WRITE' )
 
      IF ( FAILED() ) THEN
         RETURN
      END IF
 
C
C     Find out how many d.p. pages are in use.
C
      CALL DASRDI ( HANDLE, EPNPD, EPNPD, NPD )
 
      IF (  ( P .LT. 1 )  .OR.  ( P .GT. NPD )  ) THEN
 
         CALL CHKIN  ( 'ZZEKPGWD'                           )
         CALL SETMSG ( 'DP page = #; valid range is [1:#]'  )
         CALL ERRINT ( '#', P                               )
         CALL ERRINT ( '#', NPD                             )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                )
         CALL CHKOUT ( 'ZZEKPGWD'                           )
         RETURN
 
      END IF
 
 
      ADDR   =  (P-1)*PGSIZD + 1
 
      CALL DASUDD ( HANDLE, ADDR, ADDR+PGSIZD-1, PAGED )
 
      RETURN
 
 
 
 
 
 
C$Procedure  ZZEKPGWI ( Private: EK, write integer page )
 
      ENTRY ZZEKPGWI ( HANDLE, P, PAGEI )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Write a specified integer page.
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
C     EK
C
C$ Keywords
C
C     EK
C     PRIVATE
C
C$ Declarations
C
C     INTEGER               HANDLE
C     INTEGER               P
C     INTEGER               PAGEI ( * )
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Paged EK file handle.
C     P          I   Page number.
C     PAGEI      I   Integer page.
C
C$ Detailed_Input
C
C     HANDLE         is a handle of a paged EK file.  The file must
C                    be open for write access.
C
C     P              is the number of an allocated integer
C                    page to write.
C
C     PAGEI          is an integer array to be written to
C                    the specified page.  PAGEI must be declared with
C                    dimension at PGSIZI.  This parameter is
C                    declared in the include file ekpage.inc.
C
C$ Detailed_Output
C
C     None.  See $Particulars for a description of the effect of this
C     routine.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  Any errors detected during reading or writing the DAS file
C         will be diagnosed by routines called by this routine.
C
C     2)  If the number of the page to write is not that of an
C         allocated integer page, the error SPICE(INVALIDINDEX) is
C         signalled.
C
C$ Files
C
C     This suite of routines provides paged access to DAS files.  Only
C     DAS files initialized via a call to ZZEKPGIN may be written or
C     read by these routines.
C
C$ Particulars
C
C     This routine writes the input array to the DAS address range
C     corresponding to the specified page.  The file must be closed
C     properly (via EKCLS) in order to make the change permanent.
C
C$ Examples
C
C     See ZZEKAD01.
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
C     N.J. Bachman       (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 18-OCT-1995 (NJB)
C
C-&
 
C
C     Use discovery check-in.
C
C     Validate the file.
C
      CALL ZZEKPGCH ( HANDLE, 'WRITE' )
 
      IF ( FAILED() ) THEN
         RETURN
      END IF
 
C
C     Find out how many integer pages are in use.
C
      CALL DASRDI ( HANDLE, EPNPI, EPNPI, NPI )
 
      IF (  ( P .LT. 1 )  .OR.  ( P .GT. NPI )  ) THEN
 
         CALL CHKIN  ( 'ZZEKPGWI'                           )
         CALL SETMSG ( 'INT page = #; valid range is [1:#]' )
         CALL ERRINT ( '#', P                               )
         CALL ERRINT ( '#', NPI                             )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                )
         CALL CHKOUT ( 'ZZEKPGWI'                           )
         RETURN
 
      END IF
 
 
      ADDR   =   PGBASI + (P-1)*PGSIZI + 1
 
      CALL DASUDI ( HANDLE, ADDR, ADDR+PGSIZI-1, PAGEI )
 
      RETURN
 
 
 
 
 
 
C$Procedure  ZZEKPGBS ( Private: EK, map page to base address )
 
      ENTRY ZZEKPGBS ( TYPE, P, BASE )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Map a page to its base address.
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
C     EK
C
C$ Keywords
C
C     EK
C     PRIVATE
C
C$ Declarations
C
C     INTEGER               TYPE
C     INTEGER               P
C     INTEGER               BASE
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     TYPE       I   Data type of page.
C     P          I   Page number.
C     BASE       O   DAS base address of page.
C
C$ Detailed_Input
C
C     TYPE           is the data type of the page whose base address
C                    is requested.  The type may be CHR, DP, or INT.
C                    Values of these parameters are defined in
C                    ektype.inc.
C
C     P              is the number of the page of interest.
C
C$ Detailed_Output
C
C     BASE           is the base DAS address of the page.  This address
C                    is the predecessor of the first DAS word of the
C                    page.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the requested data type is not recognized, the error
C         SPICE(INVALIDTYPE) is signalled.
C
C     2)  Range checking is not performed on the input page number P.
C
C$ Files
C
C     This suite of routines provides paged access to DAS files.  Only
C     DAS files initialized via a call to ZZEKPGIN may be written or
C     read by these routines.
C
C$ Particulars
C
C     This routine provides tranlation from page numbers to DAS
C     addresses.
C
C$ Examples
C
C     See EKDELR.
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
C     N.J. Bachman       (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 18-OCT-1995 (NJB)
C
C-&
 
      IF ( TYPE .EQ. CHR ) THEN
 
         BASE  =   ( P - 1 ) * PGSIZC
 
 
      ELSE IF ( TYPE .EQ. DP ) THEN
 
         BASE  =   ( P - 1 ) * PGSIZD
 
 
      ELSE IF ( TYPE .EQ. INT ) THEN
 
         BASE  =   ( P - 1 ) * PGSIZI   +   PGBASI
 
      ELSE
 
         CALL CHKIN  ( 'ZZEKPGBS'                                 )
         CALL SETMSG ( 'The data type code # was not recognized.' )
         CALL ERRINT ( '#', TYPE                                  )
         CALL SIGERR ( 'SPICE(INVALIDTYPE)'                       )
         CALL CHKOUT ( 'ZZEKPGBS'                                 )
         RETURN
 
      END IF
 
      RETURN
 
 
 
 
 
 
C$Procedure  ZZEKPGPG ( Private: EK, map address to page )
 
      ENTRY ZZEKPGPG ( TYPE, ADDRSS, P, BASE )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Map a DAS address to the number of the page containing it.  Also
C     return the base address of the page.
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
C     EK
C
C$ Keywords
C
C     EK
C     PRIVATE
C
C$ Declarations
C
C     INTEGER               TYPE
C     INTEGER               ADDRSS
C     INTEGER               P
C     INTEGER               BASE
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     TYPE       I   Data type of address.
C     ADDRSS     I   DAS address to be mapped.
C     P          O   Page number.
C     BASE       O   DAS base address of page.
C
C$ Detailed_Input
C
C     TYPE           is the data type of a DAS address to be mapped to
C                    a page number.  The type may be CHR, DP, or INT.
C                    Values of these parameters are defined in
C                    ektype.inc.
C
C     ADDRSS         is a DAS address to be mapped to a page number.
C
C$ Detailed_Output
C
C     P              is the number of the page containing the input
C                    address.
C
C     BASE           is the base DAS address of the page.  This address
C                    is the predecessor of the first DAS word of the
C                    page.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the requested data type is not recognized, the error
C         SPICE(INVALIDTYPE) is signalled.
C
C     2)  Range checking is not performed on the input address.
C
C$ Files
C
C     This suite of routines provides paged access to DAS files.  Only
C     DAS files initialized via a call to ZZEKPGIN may be written or
C     read by these routines.
C
C$ Particulars
C
C     This routine provides tranlation from DAS addresses to page
C     numbers.
C
C$ Examples
C
C     See ZZEKAD01.
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
C     N.J. Bachman       (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 18-OCT-1995 (NJB)
C
C-&
 
 
 
      IF ( TYPE .EQ. CHR ) THEN
 
         P     =   ( ADDRSS  + PGSIZC - 1 ) / PGSIZC
         BASE  =   ( P - 1 ) * PGSIZC
 
 
      ELSE IF ( TYPE .EQ. DP ) THEN
 
         P     =   ( ADDRSS  + PGSIZD - 1 ) / PGSIZD
         BASE  =   ( P - 1 ) * PGSIZD
 
 
      ELSE IF ( TYPE .EQ. INT ) THEN
 
         P     =   ( ( ADDRSS-PGBASI )  +   PGSIZI - 1 ) / PGSIZI
         BASE  =   ( P - 1 ) * PGSIZI   +   PGBASI
 
      ELSE
 
         CALL CHKIN  ( 'ZZEKPGBS'                                 )
         CALL SETMSG ( 'The data type code # was not recognized.' )
         CALL ERRINT ( '#', TYPE                                  )
         CALL SIGERR ( 'SPICE(INVALIDTYPE)'                       )
         CALL CHKOUT ( 'ZZEKPGBS'                                 )
         RETURN
 
      END IF
 
      RETURN
 
 
 
 
C$Procedure  ZZEKPGST ( Private: EK, return paging statistics )
 
      ENTRY ZZEKPGST ( HANDLE, STAT, VALUE )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Return paging statistics.
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
C     EK
C
C$ Keywords
C
C     EK
C     PRIVATE
C
C$ Declarations
C
C     INTEGER               HANDLE
C     CHARACTER*(*)         STAT
C     INTEGER               VALUE
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Paged EK file handle.
C     STAT       I   Name of requested statistic.
C     VALUE      O   Value of requested statistic.
C
C$ Detailed_Input
C
C     HANDLE         is a handle of a paged EK file.  The file may
C                    be open for read or write access.
C
C     STAT           is the name of the requested statistic.  Possible
C                    values and meanings of STAT are:
C
C                      'N_C_ALLOC'     Number of character pages
C                                      allocated.  Pages on the free
C                                      list are not included.
C
C                      'N_D_ALLOC'     Number of d.p. pages allocated.
C
C                      'N_I_ALLOC'     Number of integer pages
C                                      allocated.
C
C                      'N_C_FREE'      Number of pages in character free
C                                      list.
C
C                      'N_D_FREE'      Number of pages in d.p. free
C                                      list.
C
C                      'N_I_FREE'      Number of pages in integer free
C                                      list.
C
C$ Detailed_Output
C
C     VALUE          is the value of the requested statistic.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the requested statistic is not recognized, the error
C         SPICE(INVALIDOPTION) is signalled.
C
C$ Files
C
C     This suite of routines provides paged access to DAS files.  Only
C     DAS files initialized via a call to ZZEKPGIN may be written or
C     read by these routines.
C
C$ Particulars
C
C     This routine provides tranlation from DAS addresses to page
C     numbers.
C
C$ Examples
C
C     1)  Find the number of pages on the integer free list of the
C         paged EK designated by HANDLE:
C
C            CALL ZZEKPGST ( HANDLE, 'N_I_FREE', NFREE )
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
C     N.J. Bachman       (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 18-OCT-1995 (NJB)
C
C-&
 
 
      CALL CHKIN ( 'ZZEKPGST' )
 
 
      IF (  EQSTR( STAT, 'N_C_ALLOC' )  ) THEN
 
         CALL DASRDI ( HANDLE, EPNPC, EPNPC, VALUE   )
 
 
      ELSE IF (  EQSTR( STAT, 'N_D_ALLOC' )  ) THEN
 
         CALL DASRDI ( HANDLE, EPNPD, EPNPD, VALUE   )
 
 
      ELSE IF (  EQSTR( STAT, 'N_I_ALLOC' )  ) THEN
 
         CALL DASRDI ( HANDLE, EPNPI, EPNPI, VALUE   )
 
 
      ELSE IF (  EQSTR( STAT, 'N_C_FREE' )  ) THEN
 
         CALL DASRDI ( HANDLE, EPNFPC, EPNFPC, VALUE   )
 
 
      ELSE IF (  EQSTR( STAT, 'N_D_FREE' )  ) THEN
 
         CALL DASRDI ( HANDLE, EPNFPD, EPNFPD, VALUE   )
 
 
      ELSE IF (  EQSTR( STAT, 'N_I_FREE' )  ) THEN
 
         CALL DASRDI ( HANDLE, EPNFPI, EPNFPI, VALUE   )
 
      ELSE
 
         CALL SETMSG ( 'Statistic # is not supported.' )
         CALL ERRCH  ( '#',  STAT                      )
         CALL SIGERR ( 'SPICE(INVALIDOPTION)'          )
         CALL CHKOUT ( 'ZZEKPGST'                      )
         RETURN
 
      END IF
 
      CALL CHKOUT ( 'ZZEKPGST' )
      RETURN
 
      END

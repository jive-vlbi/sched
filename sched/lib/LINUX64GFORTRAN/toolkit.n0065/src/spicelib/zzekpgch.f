C$Procedure   ZZEKPGCH ( EK, paging system access check )
 
      SUBROUTINE ZZEKPGCH ( HANDLE, ACCESS )
 
C$ Abstract
C
C     Check that an EK is valid for a specified type of access by the
C     paging system.
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
 
      INCLUDE 'ekpage.inc'
      INCLUDE 'ekarch.inc'
      INCLUDE 'ektype.inc'
 
      INTEGER               HANDLE
      CHARACTER*(*)         ACCESS
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle attached to EK file.
C     ACCESS     I   Access type.
C
C$ Detailed_Input
C
C     HANDLE         is an EK file handle.  The specified file is to be
C                    checked to see whether it is a valid paged EK and
C                    whether it is open for the specified type of
C                    access.
C
C     ACCESS         is a short string indicating the type of access
C                    desired.  Possible values are 'READ' and 'WRITE'.
C
C                    Leading and trailing blanks in ACCESS are ignored,
C                    and case is not significant.
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
C     1)  If HANDLE is invalid, the error will be diagnosed by routines
C         called by this routine.
C
C     2)  If the EK architecture version is not current, the error
C         SPICE(WRONGARCHITECTURE) is signalled.
C
C     3)  If the DAS logical address ranges occupied by the EK are
C         not consistent with those recorded by the paging system,
C         the error SPICE(INVALIDFORMAT) is signalled.
C
C     4)  If the EK is not open for the specified type of access, the
C         error will be diagnosed by routines called by this routine.
C
C$ Files
C
C     See the EK Required Reading for a discussion of the EK file
C     format.
C
C$ Particulars
C
C     This routine centralizes a validation check performed by many
C     EK routines.  The EK designated by HANDLE is tested to see
C     whether some aspects of its structure are valid, and whether
C     the specified type of access (read or write) is allowed.
C     The tests performed are:
C
C        - Is the file a DAS file open for the specified type of access?
C
C        - Is the file's EK architecture version correct?
C
C        - Are the DAS address ranges in use consistent with those
C          recorded in the file by the paging system?
C
C     If the file fails any test, an error is signalled.
C
C$ Examples
C
C     See EKINSR.
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
C
C$ Version
C
C-    Beta Version 1.0.0, 19-OCT-1995 (NJB)
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               FAILED
 
C
C     Local variables
C
      INTEGER               ID
      INTEGER               LASTC
      INTEGER               LASTD
      INTEGER               LASTI
      INTEGER               NPC
      INTEGER               NPD
      INTEGER               NPI
      INTEGER               TOPC
      INTEGER               TOPD
      INTEGER               TOPI
      INTEGER               UNIT
 
 
      CALL CHKIN ( 'ZZEKPGCH' )
 
C
C     Check whether the DAS is opened for the specified access method.
C
      CALL DASSIH ( HANDLE, ACCESS )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZEKPGCH' )
         RETURN
      END IF
 
C
C     Make sure the DAS file is of the right type.
C
      CALL DASRDI ( HANDLE, EPARCH, EPARCH, ID )
 
      IF ( ID .NE. ARCHID ) THEN
 
         CALL DASHLU ( HANDLE, UNIT )
         CALL SETMSG ( 'File # has architecture #, which is invalid '//
     .                 'for paged access.  You are using EK software '//
     .                 'version #.'                                  )
         CALL ERRFNM ( '#',   UNIT                                   )
         CALL ERRINT ( '#',   ID                                     )
         CALL ERRINT ( '#',   ARCHID                                 )
         CALL SIGERR ( 'SPICE(WRONGARCHITECTURE)'                    )
         CALL CHKOUT ( 'ZZEKPGCH'                                    )
         RETURN
 
      END IF
 
C
C     Obtain the page counts.  Set the `top' addresses.
C
      CALL DASRDI ( HANDLE, EPNPC, EPNPC, NPC )
      CALL DASRDI ( HANDLE, EPNPD, EPNPD, NPD )
      CALL DASRDI ( HANDLE, EPNPI, EPNPI, NPI )
 
      TOPC  =  NPC * PGSIZC
      TOPD  =  NPD * PGSIZD
      TOPI  =  NPI * PGSIZI + PGBASI
 
C
C     Verify that the last addresses in use are consistent with the
C     `top' addresses known to this system.
C
      CALL DASLLA ( HANDLE, LASTC, LASTD, LASTI )
 
      IF ( LASTC .GT. TOPC ) THEN
 
         CALL DASHLU ( HANDLE, UNIT )
         CALL SETMSG ( 'File # has last char address #; `top'' = #.' )
         CALL ERRFNM ( '#',   UNIT                                   )
         CALL ERRINT ( '#',   LASTC                                  )
         CALL ERRINT ( '#',   TOPC                                   )
         CALL SIGERR ( 'SPICE(INVALIDFORMAT)'                        )
         CALL CHKOUT ( 'ZZEKPGCH'                                    )
         RETURN
 
      ELSE IF ( LASTD .GT. TOPD ) THEN
 
         CALL DASHLU ( HANDLE, UNIT )
         CALL SETMSG ( 'File # has last d.p. address #; `top'' = #.' )
         CALL ERRFNM ( '#',   UNIT                                   )
         CALL ERRINT ( '#',   LASTD                                  )
         CALL ERRINT ( '#',   TOPD                                   )
         CALL SIGERR ( 'SPICE(INVALIDFORMAT)'                        )
         CALL CHKOUT ( 'ZZEKPGCH'                                    )
         RETURN
 
      ELSE IF ( LASTI .GT. TOPI ) THEN
 
         CALL DASHLU ( HANDLE, UNIT )
         CALL SETMSG ( 'File # has last int. address #; `top'' = #.' )
         CALL ERRFNM ( '#',   UNIT                                   )
         CALL ERRINT ( '#',   LASTI                                  )
         CALL ERRINT ( '#',   TOPI                                   )
         CALL SIGERR ( 'SPICE(INVALIDFORMAT)'                        )
         CALL CHKOUT ( 'ZZEKPGCH'                                    )
         RETURN
 
      END IF
 
 
      CALL CHKOUT ( 'ZZEKPGCH' )
      RETURN
      END

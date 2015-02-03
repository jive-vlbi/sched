C$Procedure  ZZEKSEI ( Private: EK, set encoded integer )
 
      SUBROUTINE ZZEKSEI ( HANDLE, ADDRSS, IVAL )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Set an encoded integer at a specifed address from a character
C     data page.
C
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
 
      INCLUDE 'ekdatpag.inc'
 
      INTEGER               HANDLE
      INTEGER               ADDRSS
      INTEGER               IVAL
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   EK file handle.
C     ADDRSS     I   DAS character address.
C     IVAL       I   Integer value to write.
C
C$ Detailed_Input
C
C     HANDLE         is the handle of an EK file open for write access.
C
C     ADDRSS         is the DAS character start address at which an
C                    integer, encoded as a string, is to be written.
C                    An encoded integer occupies ENCSIZ characters,
C                    where the parameter ENCSIZ is defined in the
C                    include file ekdatpag.inc.
C
C     IVAL           is an integer value to be written in encoded form.
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
C     2)  If the DAS character address range
C
C            ADDRSS .. ADDRSS+ENCSIZ-1
C
C         is not a range of DAS character addresses that have been
C         initialized, the error wll be diagnosed by routines
C         called by this routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine should be used for all EK applications requiring
C     storage of encoded integer values as characters.  Use of this
C     routine should ensure consistent encoding across the library.
C
C     Encoded integers written by this routine should be read using
C     ZZEKGEI.
C
C$ Examples
C
C     See ZZEKAD03.
C
C$ Restrictions
C
C     1)  Portability dictates that the base used for encoding be
C         no greater than 128.
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
C-    Beta Version 1.0.0, 10-OCT-1995 (NJB)
C
C-&
 
C
C     Local variables
C
      CHARACTER*(ENCSIZ)    CVAL
 
 
C
C     Discovery error handling should be used in this utility.
C
C
C     Encode the number.
C
      CALL PRTENC ( IVAL, CVAL )
 
C
C     Write the encoded value.
C
      CALL DASUDC ( HANDLE, ADDRSS, ADDRSS+ENCSIZ-1, 1, ENCSIZ, CVAL )
 
 
      RETURN
      END

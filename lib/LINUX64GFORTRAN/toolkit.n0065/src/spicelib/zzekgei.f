C$Procedure  ZZEKGEI ( Private: EK, get encoded integer )
 
      SUBROUTINE ZZEKGEI ( HANDLE, ADDRSS, IVAL )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Get an encoded integer at a specifed address from a character
C     data page.
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
C     ADDRSS     I   DAS character address to read encoded value from.
C     IVAL       O   Decoded integer value.
C
C$ Detailed_Input
C
C     HANDLE         is the handle of an EK file open for read or
C                    write access.
C
C     ADDRSS         is the DAS character start address from which an
C                    integer, encoded as a string, is to be read.
C                    An encoded integer occupies ENCSIZ characters,
C                    where the parameter ENCSIZ is defined in the
C                    include file ekdatpag.inc.
C
C$ Detailed_Output
C
C     IVAL           is an integer value obtained by decoding an
C                    encoded integer read from the specified
C                    location.
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
C     3)  If the character data starting at the specified address
C         does not represent an encoded integer, the error wll be
C         diagnosed by routines called by this routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine should be used for all EK applications reading
C     integer values that have been encoded as characters.  This
C     routine expects the encoding to have been done by ZZEKSEI.
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
C     Read the encoded value.  The value is represented by a string of
C     characters.
C
      CALL DASRDC ( HANDLE, ADDRSS, ADDRSS+ENCSIZ-1, 1, ENCSIZ, CVAL )
 
C
C     Decode the number.
C
      CALL PRTDEC ( CVAL, IVAL )
 
      RETURN
      END

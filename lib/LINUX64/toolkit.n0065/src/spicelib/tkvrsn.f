C$Procedure      TKVRSN ( Toolkit version strings )
 
      SUBROUTINE TKVRSN ( ITEM, VERSTR )
 
C$ Abstract
C
C     Given an item such as the toolkit or an entry point name, return
C     the latest version string.
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
 
      CHARACTER*(*)         ITEM
      CHARACTER*(*)         VERSTR
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ITEM       I   Item for which a version string is desired.
C     VERSTR     O   Version string.
C
C$ Detailed_Input
C
C     ITEM       is the item for which a version string is to be
C                returned. ITEM may be 'TOOLKIT', entry point names,
C                or program names. ITEM is case insensitive.
C
C                Currently, the only ITEM supported is 'TOOLKIT'
C                and it will return the toolkit version number.
C
C                Any other ITEM will return 'No version found.'
C
C$ Detailed_Output
C
C     VERSTR     is the latest version string for the specified ITEM.
C
C                If ITEM is not one of the items haveing a version,
C                the value 'No version found.' will be returned.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C        Error Free.
C
C     1) If the ITEM whose version string is requested is not
C        recognized, the string 'No version found.' is returned.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     None.
C
C$ Examples
C
C     Suppose you want to find out the recent Toolkit configuration
C     version number. Using the code fragment below:
C
C     CHARACTER*(80)        VERSN
C
C     CALL TKVRSN ( 'TOOLKIT', VERSN )
C
C     The variable VERSN would contain a string similar to the one
C     shown below:
C
C        'N0035'
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
C     K.R. Gehringer     (JPL)
C     H.A. Neilan        (JPL)
C
C$ Version
C
C-    SPICELIB Version 3.13.0, 15-JUL-2014 (WLT)
C
C        Version update, N0065
C
C-    SPICELIB Version 3.12.0, 09-JUN-2010 (WLT)
C
C        Version update, N0064
C
C-    SPICELIB Version 3.11.0, 15-APR-2009 (WLT)
C
C        Version update, N0063
C
C-    SPICELIB Version 3.10.0, 04-MAR-2008 (WLT)
C
C        Version update, N0062
C
C-    SPICELIB Version 3.9.0, 27-NOV-2006 (WLT)
C
C        Version update, N0061
C
C-    SPICELIB Version 3.8.0, 16-DEC-2005 (WLT)
C
C        Version update, N0060
C
C-    SPICELIB Version 3.7.0, 17-NOV-2005 (WLT)
C
C        Version update, N0059
C
C-    SPICELIB Version 3.6.0, 11-JAN-2005 (WLT)
C
C        Version update, N0058
C
C-    SPICELIB Version 3.5.0, 02-MAR-2004 (WLT)
C
C        Version update, N0057
C
C-    SPICELIB Version 3.4.0, 30-JUL-2003 (WLT)
C
C        Version update, N0056
C
C-    SPICELIB Version 3.3.0, 26-FEB-2003 (WLT)
C
C        Version update, N0055
C
C-    SPICELIB Version 3.2.0, 13-DEC-2002 (WLT)
C
C        Version update, N0054
C
C-    SPICELIB Version 3.1.0, 05-SEP-2002 (WLT)
C
C        Version update, N0053
C
C-    SPICELIB Version 3.0.0, 06-FEB-2002 (FST)
C
C        Version update, N0052a
C
C-    SPICELIB Version 2.9.0, 17-JAN-2002 (WLT)
C
C        Version update, N0052
C
C-    SPICELIB Version 2.8.0, 07-APR-2000 (WLT)
C
C        Version update, N0051
C
C-    SPICELIB Version 2.7.0, 06-OCT-1999 (WLT)
C
C        Version update, N0050
C
C-    SPICELIB Version 2.6.0, 04-SEP-1998 (WLT)
C
C        Version update, N0049
C
C-    SPICELIB Version 2.5.0, 01-MAY-1998 (WLT)
C
C        Version update, N0048
C
C-    SPICELIB Version 2.4.0, 31-JUL-1997 (WLT)
C
C        Version update, N0047
C
C-    SPICELIB Version 2.3.0, 27-JAN-1997 (WLT)
C
C        Version update, N0046
C
C-    SPICELIB Version 2.2.0, 15-OCT-1996 (WLT)
C
C        Version update, N0045
C
C-    SPICELIB Version 2.1.0, 26-AUG-1996 (WLT)
C
C        Version update, N0044
C
C-    SPICELIB Version 2.0.0, 09-MAY-1996 (KRG)
C
C        Removed the check of the spicelib function RETURN. This
C        routine is called by the error handling after an error
C        has been signalled to get the toolkit version, so it
C        cannot return on entry after an error.
C
C        The calls to CHKIN and CHKOUT have also been removed to
C        completly isolate this subroutine from the error handling.
C
C        Version update, N0043.
C
C-    SPICELIB Version 1.7.0, 2-JAN-1995 (WLT)
C
C        Version update, N0042.
C
C-    SPICELIB Version 1.6.0, 28-SEP-1995 (HAN)
C
C        Version update, N0041.
C
C-    SPICELIB Version 1.5.0, 19-AUG-1995 (HAN)
C
C        Version update, N0040.
C
C-    SPICELIB Version 1.4.0, 5-JUN-1995 (HAN)
C
C        Version update, N0039.
C
C-    SPICELIB Version 1.3.0, 28-MAR-1995 (HAN)
C
C        Version update, N0038.
C
C-    SPICELIB Version 1.2.0, 23-DEC-1994 (HAN)
C
C        Version update, N0037.
C
C-    SPICELIB Version 1.1.0, 31-OCT-1994 (HAN)
C
C        Version update, N0036.
C
C-    SPICELIB Version 1.0.0, 23-AUG-1994 (HAN)
C
C-&
 
C$ Index_Entries
C
C     Return version strings
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               EQSTR
 
C
C     At the current time only the TOOLKIT version number is
C     defined.
C
      IF ( EQSTR( ITEM, 'TOOLKIT' ) ) THEN
         VERSTR = 'N0065'
      ELSE
         VERSTR = 'No version found.'
      END IF
 
      RETURN
      END

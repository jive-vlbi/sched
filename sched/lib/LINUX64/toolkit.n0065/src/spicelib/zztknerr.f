C$Procedure  ZZTKNERR ( Create ZZTOKNS overflow error message )

      SUBROUTINE ZZTKNERR ( TEMPLT, STRING, TOKEN, ERROR, STATUS )

      IMPLICIT NONE
 
C$ Abstract
C
C     SPICE private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Create an error message for ZZTOKNS token buffer and picture
C     overflow exceptions.
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
C     TIME
C
C$ Keywords
C
C     TIME
C     PRIVATE
C     UTILITY
C
C$ Declarations

      CHARACTER*(*)         TEMPLT
      CHARACTER*(*)         STRING
      CHARACTER*(*)         TOKEN
      CHARACTER*(*)         ERROR
      LOGICAL               STATUS

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     TEMPLT     I   Error message template
C     STRING     I   First string to be substituted 
C     TOKEN      I   Second string to be substituted 
C     ERROR      O   Complete error message
C     STATUS     O   Status flag
C
C$ Detailed_Input
C
C     TEMPLT         is the input error message template. It should
C                    have two '#' characters in it.
C
C     STRING         is the string that should replace the first '#' in
C                    the template.
C
C     TOKEN          is the string that should replace the second '#'
C                    in the template.
C                    
C$ Detailed_Output
C
C     ERROR          is the complete output error message.
C
C     STATUS         is the output status flag always set .FALSE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C 
C     None.
C
C$ Files
C
C     None. 
C
C$ Particulars
C
C     The sole purpose of this routine is to encapsulate in a single
C     routine the two REPMC calls and one ZZTOKNS = .FALSE. assignments
C     needed to make the error messages and returned status used in
C     numerous overflow check IF blocks in ZZTOKNS.
C
C$ Examples
C
C     For these inputs 
C
C        TEMPLT = 'This time string ''#'' has a bad token ''#''.'
C
C        STRING = '2001-01-01 boo 00:00:00'
C
C        TOKEN  = 'boo'
C
C     this routine returns
C
C        ERROR  =  'This time string ''2001-01-01 boo 00:00:00'' has '
C       .          'a bad token ''boo''.'
C
C        STATUS = .FALSE.
C
C$ Restrictions
C
C     1) This is a private routine. SPICE user applications should not
C        call this routine.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 05-FEB-2014 (BVS)  
C
C-&
 
C$ Index_Entries
C
C     create error message for ZZTOKNS overflow failure
C
C-&

      CALL REPMC ( TEMPLT, '#', STRING, ERROR )
      CALL REPMC ( ERROR,  '#', TOKEN,  ERROR )
      STATUS = .FALSE.

      RETURN

      END

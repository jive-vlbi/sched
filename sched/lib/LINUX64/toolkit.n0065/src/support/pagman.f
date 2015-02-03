C$Procedure PAGMAN (Page Manager)
 
      SUBROUTINE PAGMAN ( WHICH, LINE, VALUE )
      IMPLICIT NONE

C$ Abstract
C
C     This routine serves as an umbrella for a collection of entry
C     points that manage the layout and printing of a series of
C     pages of text that may include fixed titles, headers, and
C     footers.
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
C     REPORTS
C
C$ Keywords
C
C     OUTPUT
C     TEXT
C     FORMATTING
C
C$ Declarations

      EXTERNAL              PROMPT

      CHARACTER*(*)         WHICH
      CHARACTER*(*)         LINE
      INTEGER               VALUE
 
      INTEGER               MXWDTH
      PARAMETER           ( MXWDTH = 255 )
 
      INTEGER               ROOMH
      PARAMETER           ( ROOMH =  15 )
 
      INTEGER               ROOMT
      PARAMETER           ( ROOMT =  10 )
 
      INTEGER               ROOMF
      PARAMETER           ( ROOMF =  10 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     WHICH      I   indicates which section to send lines of text to
C     LINE       I   a line of text
C     ATTR       I   the name of a global page attribute to be set
C     VALUE      I   the value of some global page attribute
C
C$ Detailed_Input
C
C     See the individual entry points for details
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     MXWDTH     is the width of the page in characters.
C
C     ROOMH      is the number of lines allowed for use in the
C                header section of the page.
C
C     ROOMT      is the amount of room allowed for the title section
C                of each page.
C
C     ROOMF      is the amount of room allowed for the footer of each
C                page.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C     This routine sends lines of output to the routine NSPWLN.
C
C$ Particulars
C
C     By combining the function of the entry points in this routine you
C     may create a page having fixed titles, headers, and footers.
C     together with a variable body for each page.  In addition you may
C     insert the current page number in either the title or footer
C     portion of each page.  In addition you may adjust the size of
C     each page and the width of the page.
C
C     In addition since the IO path has not been selected (you supply
C     your own routine for receiving lines for output.  You may be able
C     to use this routine to build up pages that you may then process
C     further adding touches to the output that have not been provided
C     here.
C
C     The capabilities provided are:
C
C     PAGRST   ---  sets the page number to zero, and empties all
C                   sections of the page format. It does not affect the
C                   global page properties such as the frequency of
C                   titles, headers and footers, page width and height.
C
C     PAGSCN   ---  allows you to set the section to which the next
C                   lines should be written.
C
C     PAGSET   ---  allows you to set page geometry and frequency
C                   parameters.
C
C     PAGSMK   ---  allows you to set the marker that indicates "put the
C                   current page number here."
C
C     PAGPUT   ---  allows you put a line of text on the page.  Note
C                   that the "printing" of title, header and footer
C                   text is deferred until the first line of text in
C                   the body is sent to PAGPUT.
C
C     PAGSFT   ---  is a soft page reset,  the page number is not
C                   altered but the current page is ended (causing a
C                   footer to be written if one is to be printed on the
C                   current page) and then empties the sections
C                   indicated so that they can be updated with new
C                   text.
C
C     PAGPMT   ---  allows you to determine if the last call to PAGPUT
C                   caused a prompt to be issued and if so to see what
C                   the user's response to that prompt was.
C
C                   Note that for a prompt to be issued you must take
C                   several steps.
C
C                   1) You must enable prompts through the PAGSET entry
C                      point.
C
C                      CALL PAGSET ( 'PROMPT', 0 )
C
C                   2) You next need to set the prompt that will be used
C                      This is done with two calls.
C
C                      CALL PAGSCN ( 'PROMPT' )
C                      CALL PAGPUT ( 'your prompt value:' )
C
C                   Having made these preparations, the page manager is
C                   now ready to issue your prompt and retain the user's
C                   response when a page is finished.
C
C                   Note that prompts are not issued as a result of
C                   calling a page reset for (either soft or hard)
C
C                   Also note that once a reset is issued, the prompt
C                   status is set back to the default value --- No
C                   Prompts.
C
C     You might use this routine in conjunction with NICEPR, or TABRPT.
C
C     A typical useage might go as shown here.
C
C     First set the basic global attributes of the page and report.
C
C     CALL PAGSET ( 'PAGEHEIGHT',        60 )
C     CALL PAGSET ( 'PAGEWIDTH',         80 )
C     CALL PAGSET ( 'HEADERFREQUENCY',   -1 )
C     CALL PAGSET ( 'TITLEFREQUENCY',     1 )
C     CALL PAGSET ( 'NOSPACEHEADER',      0 )
C     CALL PAGSET ( 'SPACETITLE',         0 )
C     CALL PAGSET ( 'FOOTERFREQUENCY',    1 )
C     CALL PAGSMK ( '#'                     )
C     CALL PAGRST
C
C     Create the title that will appear on every page.
C
C     CALL PAGSCN ( 'TITLE'                 )
C     CALL PAGPUT ( ' '                     )
C     CALL PAGPUT ( 'Results of Test'       )
C     CALL PAGPUT ( ' '                     )
C     CALL PAGPUT ( ' '                     )
C
C     Create the footer that will appear on every page.
C
C     CALL PAGSCN ( 'FOOTER'              )
C     CALL PAGPUT ( ' '                   )
C     CALL PAGPUT ( ' '                   )
C     CALL PAGPUT ( '           Page # '  )
C     CALL PAGPUT ( ' '                   )
C     CALL PAGPUT ( ' '                   )
C
C     CALL PAGSCN ( 'BODY'                )
C 
C     DO I = 1, NLINES
C        CALL PAGPUT ( TEXT(I) )
C     END DO
C 
C     CALL PAGSFT
C
C$ Examples
C
C     See above.
C
C$ Restrictions
C
C    Since these routines interact by side effect, you should
C    read carefully the required reading documentation.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 14-DEC-2010 (EDW)
C
C         Declared PROMPT as EXTERNAL.
C
C-    Beta Version 1.0.0, 9-JAN-1992 (WLT)
C
C-&
 
C$ Index_Entries
C
C     Control the format of output pages
C
C-&
 
C
C     SPICELIB functions
C
      INTEGER               RTRIM
 
C
C     Local variables
C
C     The arrays TITLE, HEADER and FOOTER are used to store the
C     text that will be written to the TITLE, HEADER and FOOTER
C     sections of a page.
C
      CHARACTER*(MXWDTH)    TITLE  ( ROOMT )
      CHARACTER*(MXWDTH)    HEADER ( ROOMH )
      CHARACTER*(MXWDTH)    FOOTER ( ROOMF )
      CHARACTER*(MXWDTH)    MYLINE
C
C     The variable RESPNS is used to keep track of any response
C     that the user may supply to a prompt that can be triggered
C     at the completion of a page.
C
      CHARACTER*(MXWDTH)    RESPNS
      CHARACTER*(MXWDTH)    QUESTN
 
C
C     The variable SECTN contains the name of the section to which
C     lines of text should be sent.
C
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )
      CHARACTER*(WDSIZE)    SECTN
C
C     The array FREQ is used to store the
C     frequency with which footers, headers and titles should
C     be displayed PAGESZ and PAGEWD give the size of the page
C     in height and width.
C
C     The array SIZE is  used to maintain the
C     size of the TITLE, HEADER, BODY, and FOOTER sections.
C
C     The array NEED is used to determine how many lines
C     need to be devoted to the TITLE, HEADER and FOOTER section
C     on a page (the value will be a function of FREQ, the page
C     number and the array KEEPSP)
C
C     The array KEEPSP is used to store whether or not sections
C     should be kept but presented as white space when the
C     page number and frequency imply that the section should
C     not be printed on a given page.
C
C     The array INVIS is used to keep track of whether or not
C     a section should be visible on the current page.
C
      CHARACTER*(WDSIZE)    PAGMRK
      LOGICAL               DOMARK
 
      INTEGER               T
      PARAMETER           ( T = 1 )
 
      INTEGER               H
      PARAMETER           ( H = 2 )
 
      INTEGER               F
      PARAMETER           ( F = 3 )
 
      INTEGER               B
      PARAMETER           ( B = 4 )
 
      INTEGER               P
      PARAMETER           ( P = 5 )
 
      INTEGER               NSECN
      PARAMETER           ( NSECN = P )
 
 
      INTEGER               PAGESZ
      INTEGER               PAGEWD
      INTEGER               PAGMLN
      INTEGER               QLENTH
      INTEGER               WFACTR
 
      INTEGER               FREQ  (NSECN)
      INTEGER               NEED  (NSECN)
      INTEGER               SIZE  (NSECN)
 
      LOGICAL               VISIBL(NSECN)
      LOGICAL               KEEPSP(NSECN)
 
C
C     The variable ROW points to the position of the last
C     row in the body portion of the page where text was last
C     written.  PAGENO is the page number of the page that is
C     currently being filled.
C
      INTEGER               ROW
      INTEGER               PAGENO
C
C     The logical BODY is used to indicate whether the section
C     has been set to BODY since the last call to PAGRST to reset
C     the dynamic page attributes.
C
      LOGICAL               BODY
 
C
C     The logical DOPRMT is used to indicate whether or not a prompt
C     should be issued when the production of a page is finished.
C
      LOGICAL               DOPRMT
      LOGICAL               DIDPMT
 
C
C     Loop counter
C
      INTEGER               I
 
 
C
C     Saved variables
C
      SAVE
 
C
C     Initial values
C
      DATA                PAGESZ /  24 /
      DATA                PAGEWD /  80 /
      DATA                FREQ   /  -1,  -1, -1, -1, -1 /
      DATA                NEED   /   0,   0,  0,  0,  0 /
      DATA                SIZE   /   0,   0,  0,  0,  0 /
      DATA                ROW    /   0              /
      DATA                PAGENO /   0              /
      DATA                BODY   /  .TRUE.          /
      DATA                DOMARK /  .FALSE.         /
      DATA                DOPRMT /  .FALSE.         /
      DATA                DIDPMT /  .FALSE.         /
      DATA                WFACTR /   0              /
      DATA                SECTN  /  'BODY'          /
      DATA                RESPNS /  ' '             /
 
 
 
      RETURN
 
 
 
 
 
 
 
 
C$Procedure PAGRST (Page Reset)
 
      ENTRY PAGRST
 
C$ Abstract
C
C     Reset the page to page zero and empty all sections of
C     the page.
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
C     REPORTS
C
C$ Keywords
C
C     OUTPUT
C     TEXT
C     FORMATTING
C
C$ Declarations
C
C     None.
C
C$ Brief_I/O
C
C     None.
C
C$ Detailed_Input
C
C     None
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     See the subroutine header
C
C$ Exceptions
C
C     None
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This entry point is used to reset the page manager
C     so that it may receive new section information and
C     so that the lines of text when output will start on the
C     first page of the sequence of pages.
C
C     This entry point should be called only prior to the beginning
C     of a sequence of page productions.
C
C     A call to this routine always halts production of the current
C     page.  No cleanup is performed.  In particular any footer
C     that was waiting to be output, will be elliminated and
C     not produced.  For this reason it is better to call the
C     soft reset PAGSFT (which will output any footers) prior to
C     calling this entry point if you have already begun production
C     of a document and want the last page of the document
C     to be finished prior to beginning a new document.
C
C$ Examples
C
C     See above.
C
C$ Restrictions
C
C     See particulars.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber     (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 9-JAN-1992 (WLT)
C
C-&
 
      ROW      =  0
      PAGENO   =  1
      SIZE(H)  =  0
      SIZE(F)  =  0
      SIZE(T)  =  0
      SIZE(P)  =  0
      DOPRMT   = .FALSE.
      DIDPMT   = .FALSE.
      RESPNS   = ' '
      WFACTR   =  0
      BODY     = .FALSE.
 
      RETURN
 
 
 
 
 
 
 
C$Procedure PAGSFT (Page Soft Reset)
 
      ENTRY PAGSFT
 
C$ Abstract
C
C     Finish production of the current page, and empty all section
C     of the page.
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
C     REPORTS
C
C$ Keywords
C
C     OUTPUT
C     TEXT
C     FORMATTING
C
C$ Declarations
C
C     None.
C
C$ Brief_I/O
C
C     None.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     See the subroutine header
C
C$ Exceptions
C
C     None
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This entry point is used to reset the page manager
C     so that a new style (header, title and footer, etc).
C
C     The page number is not altered.
C
C     This entry point differs from PAGRST in that it cleanly
C     finished the current page.  This routine should typically
C     be called after the last body text line has been sent to
C     the PAGE MANAGER.
C
C     To perform a complet reset, call the entry point PAGRST.
C
C$ Examples
C
C     See above.
C
C$ Restrictions
C
C     See particulars.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber     (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 9-JAN-1992 (WLT)
C
C-&
 
      IF ( ROW .GT. 0 ) THEN
 
         MYLINE = ' '
 
         DO WHILE ( ROW .LT. SIZE(B) )
            CALL NSPWLN ( MYLINE(1:PAGEWD) )
            ROW = ROW + 1
         END DO
 
C
C        The user may want to have the page number appear
C        in the footer.  So we replace the PAGMRK by the
C        number if this is the case.
C
         DO I = 1, NEED(F)
 
            IF ( VISIBL(F) ) THEN
               CALL REPMI  ( FOOTER(I), PAGMRK, PAGENO, MYLINE )
               CALL NSPWLN ( MYLINE(1:PAGEWD) )
            ELSE
            CALL NSPWLN ( MYLINE(1:PAGEWD) )
            END IF
 
         END DO
 
         PAGENO = PAGENO + 1
 
      END IF
 
      ROW      =  0
      SIZE(H)  =  0
      SIZE(F)  =  0
      SIZE(T)  =  0
      SIZE(P)  =  0
      DOPRMT   = .FALSE.
      DIDPMT   = .FALSE.
      RESPNS   = ' '
      WFACTR   =  0
      BODY     = .FALSE.
 
      RETURN




C$Procedure      PAGSET (Page Set attributes )
 
      ENTRY PAGSET ( WHICH, VALUE )
 
C$ Abstract
C
C     Set one of the global page attributes
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
C     REPORTS
C
C$ Keywords
C
C     OUTPUT
C     TEXT
C     FORMATTING
C
C$ Declarations
C
C     CHARACTER*(*)         WHICH
C     INTEGER               VALUE
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     WHICH      I   indicates which attribute to set
C     VALUE      I   the value of the attribute
C
C$ Detailed_Input
C
C     WHICH      is the name of some attribute to set.  The acceptable
C                values are:  PAGEWIDTH, PAGEHEIGHT, HEADERFREQUENCY,
C                TITLEFREQUENCY,  SPACETITLE, NOSPACETITLE, SPACEHEADER
C                NOSPACEHEADER, SPACEFOOTER, NOSPACEFOOTER.
C
C     VALUE      is the value to assign to one of the page attributes.
C                In the case of any of the frequency attributes the
C                values carry the following implication: If the
C                frequency is less than zero, that section never
C                appears in the page. If the frequency is 0, that
C                section appears on the first page.  However it does
C                not appear on any other pages.  If the frequency is N
C                > 0 then the section appears on the first page and
C                every page of the form  1 + K*N where K is a positive
C                integer.
C
C                The values supplied for the SPACE/NOSPACE WAIT/NOWAIT
C                attributes are ignored.  The text of WHICH is used to
C                determine if blank lines should be used in place of
C                the text of the section it is not supposed to appear
C                in output.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     See the subroutine header
C
C$ Exceptions
C
C     If one of the recognized values for WHICH is not entered the state
C     of the page manager will not change.
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
C     See above.
C
C$ Restrictions
C
C    It is intended that this routine be called to set up the page
C    manager prior to the productio of pages.  However, user's may
C    call this routine to change page attributes at any time.
C    Nevertheless, due to the method by which pages are produced, the
C    affects of a call to this routine may be delayed. Once the body
C    of a new page has begun, all attributes but PAGEWIDTH are ignored
C    until the page has been completed according to the attributes
C    that were in effect when the body section of the page was begun.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber     (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 9-JAN-1992 (WLT)
C
C-&
 
      IF ( WHICH .EQ. 'PAGEHEIGHT' ) THEN
         PAGESZ = VALUE
      ELSE IF ( WHICH .EQ. 'PAGEWIDTH' ) THEN
         PAGEWD = VALUE
 
      ELSE IF ( WHICH .EQ. 'HEADERFREQUENCY' ) THEN
         FREQ(H) = VALUE
      ELSE IF ( WHICH .EQ. 'TITLEFREQUENCY' ) THEN
         FREQ(T) = VALUE
      ELSE IF ( WHICH .EQ. 'FOOTERFREQUENCY' ) THEN
         FREQ(F) = VALUE
 
      ELSE IF ( WHICH .EQ.   'SPACETITLE'  ) THEN
         KEEPSP(T) = .TRUE.
      ELSE IF ( WHICH .EQ. 'NOSPACETITLE'  ) THEN
         KEEPSP(T) = .FALSE.
 
      ELSE IF ( WHICH .EQ.   'SPACEHEADER' ) THEN
         KEEPSP(H) = .TRUE.
      ELSE IF ( WHICH .EQ. 'NOSPACEHEADER' ) THEN
         KEEPSP(H) = .FALSE.
 
      ELSE IF ( WHICH .EQ.   'SPACEFOOTER' ) THEN
         KEEPSP(F) = .TRUE.
      ELSE IF ( WHICH .EQ. 'NOSPACEFOOTER' ) THEN
         KEEPSP(F) = .FALSE.
 
      ELSE IF ( WHICH .EQ.   'NOPAGEMARK'  ) THEN
         DOMARK    =.FALSE.
      ELSE IF ( WHICH .EQ.   'DOPAGEMARK'  ) THEN
         DOMARK    =.TRUE.
 
      ELSE IF ( WHICH .EQ. 'PROMPT' ) THEN
         DOPRMT    = .TRUE.
         WFACTR    =  1
         SIZE(P)   =  1
      ELSE IF ( WHICH .EQ. 'NOPROMPT' ) THEN
         DOPRMT    = .FALSE.
         DIDPMT    = .FALSE.
         RESPNS    = ' '
         WFACTR    =  0
         SIZE(P)   =  0
      END IF
 
 
 
      RETURN
 
 
 
 
 
 
C$Procedure      PAGSMK (Page set page number marker )
 
      ENTRY PAGSMK ( WHICH )
 
C$ Abstract
C
C     Set the mark that will be replaced by the current page number
C     within the title and footer sections of a page.
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
C     REPORTS
C
C$ Keywords
C
C     OUTPUT
C     TEXT
C     FORMATTING
C
C$ Declarations
C
C     None.
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     WHICH      I   mark to be replaced by current page number
C
C$ Detailed_Input
C
C     WHICH      is a string which when encountered as a substring
C                of a line of text in either the title or footer
C                section will be replaced by the current page number.
C
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     See the subroutine header
C
C$ Exceptions
C
C     None
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This entry point is used to set the "mark" that
C     the page manager will recognize as the position to
C     fill in the current page number in either the title
C     or footer section of a page.   It has no effect in the
C     HEADER or BODY section of the document.
C
C     Usually you will want to set the page number mark at the
C     beginning of a document and leave this unchanged throughout
C     the production of the document.
C
C     The effect of a call to PAGSMK will begin on the next call
C     to PAGPUT.
C
C
C$ Examples
C
C     See above.
C
C$ Restrictions
C
C     See particulars.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber     (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 9-JAN-1992 (WLT)
C
C-&
 
      PAGMRK = WHICH
      PAGMLN = RTRIM(PAGMRK)
      DOMARK = .TRUE.
      RETURN
 
 
 
 
 
 
C$Procedure      PAGSCN (Page Section)
 
      ENTRY PAGSCN ( WHICH )
 
C$ Abstract
C
C     Set the section to which lines should be sent.
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
C     REPORTS
C
C$ Keywords
C
C     OUTPUT
C     TEXT
C     FORMATTING
C
C$ Declarations
C
C     CHARACTER*(*)         WHICH
C     CHARACTER*(*)         LINE
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     WHICH      I   indicates which section to send lines of text to
C
C$ Detailed_Input
C
C     WHICH      the section to which lines will be sent by
C                the entry point PAGPUT.  Valid choices for
C                WHICH are 'TITLE', 'HEADER', 'FOOTER' and 'BODY'.
C                The routine is case sensitive.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     See the subroutine header
C
C$ Exceptions
C
C     If one of the recognized values is not entered, calls
C     to PAGPUT will have no effect.
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
C     See above.
C
C$ Restrictions
C
C    None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber     (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 9-JAN-1992 (WLT)
C
C-&
      SECTN = WHICH
      BODY  = SECTN .EQ. 'BODY'
 
      RETURN
 
 
 
 
 
C$Procedure PAGPUT (Page put a line of text )
 
      ENTRY PAGPUT ( LINE )
 
C$ Abstract
C
C     Put a line of text in the current section of the current
C     page.
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
C     REPORTS
C
C$ Keywords
C
C     OUTPUT
C     TEXT
C     FORMATTING
C
C$ Declarations
C
C     CHARACTER*(*)         LINE
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     LINE       I   a line of text to put on the current page
C
C$ Detailed_Input
C
C     LINE       is a line of text that should be output (eventually)
C                via the routine NSPWLN.
C
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     See the subroutine header
C
C$ Exceptions
C
C     None
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This entry point is used prepare a line of text for output.
C     Whether the text is sent immediately to NSPWLN or is defered
C     depends upon which section is currently active.
C
C     If the current section is the TITLE, HEADER or FOOTER section
C     the line of text is simply buffered and output is defered
C     until the appropriate line of the body of the page is
C     output.
C
C     If the  current section if BODY, the line will be output
C     in the appropriate order along with any of the TITLE,
C     HEADER and FOOTER sections that should be output along with
C     it.
C
C     The calling program should ensure that if sections other than
C     the BODY section are to be written, that their text be
C     established prior to calling this entry point when the body
C     section is active.
C
C$ Examples
C
C     See above.
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
C     W.L. Taber     (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 9-JAN-1992 (WLT)
C
C-&
C
C     We handle the TITLE, HEADER and FOOTER sections first.
C
      DIDPMT = .FALSE.
 
      IF ( .NOT. BODY ) THEN
 
         IF      ( SECTN .EQ. 'TITLE'  )    THEN
 
            SIZE (T)       = MIN( ROOMT, SIZE(T) + 1 )
            TITLE(SIZE(T)) = LINE
 
         ELSE IF ( SECTN .EQ. 'HEADER' )    THEN
 
            SIZE  (H)       = MIN( ROOMH, SIZE(H) + 1 )
            HEADER(SIZE(H)) = LINE
 
         ELSE IF ( SECTN .EQ. 'FOOTER' )  THEN
 
            SIZE  (F)       = MIN( ROOMF,SIZE(F) + 1 )
            FOOTER(SIZE(F)) = LINE
 
         ELSE IF ( SECTN .EQ. 'PROMPT' ) THEN
 
            SIZE (P) = 1
            QUESTN   = LINE
            QLENTH   = RTRIM(LINE) + 1
 
         END IF
 
         RETURN
 
      END IF
C
C     The only way to get to this point is if we are working on
C     the body section of a page.  If the row number is zero, then
C     we need to see how much room is available on this page for
C     the body.  And, if appropriate output the TITLE and
C     HEADER sections of this page.
C
      IF ( ROW .EQ. 0 ) THEN
C
C        We need to compute how much room is available
C        for the body of this page.
C
         DO I = T, F
C
C           First determine how much room is needed for
C           this section and whether or not it will be
C           visible on this page if we simply fill it with
C           blanks.
C
            IF ( FREQ(I) .LT. 0 ) THEN
               NEED(I)   =  0
               VISIBL(I) = .FALSE.
            ELSE IF ( PAGENO  .EQ. 1 ) THEN
               NEED(I)   =  SIZE(I)
               VISIBL(I) = .TRUE.
            ELSE IF ( FREQ(I) .EQ. 0 ) THEN
               NEED(I)   =  0
               VISIBL(I) = .TRUE.
            ELSE IF ( FREQ(I) .EQ. 1 ) THEN
               NEED(I)   =  SIZE(I)
               VISIBL(I) = .TRUE.
            ELSE IF ( MOD(PAGENO,FREQ(I)) .EQ. 1 ) THEN
               NEED(I)   =  SIZE(I)
               VISIBL(I) = .TRUE.
            ELSE
               NEED(I)   =  0
               VISIBL(I) = .TRUE.
            END IF
 
            IF ( KEEPSP(I) ) THEN
               NEED(I) = SIZE(I)
            END IF
 
         END DO
 
         SIZE(B) = PAGESZ - NEED(T) - NEED(H) - NEED(F) - WFACTR*SIZE(P)
C
C        We haven't yet written a line in the body of the
C        page, we will write out the title and header sections
C        (provided we are on the right page number)
C
C        We allow for the possibility that the user might
C        place the page number in the title section.
C
         MYLINE = ' '
 
         DO I = 1, NEED(T)
 
            IF ( VISIBL(T) ) THEN
               IF ( DOMARK ) THEN
                  CALL REPMI  ( TITLE(I), PAGMRK(1:PAGMLN), PAGENO,
     .                          MYLINE                           )
                  CALL NSPWLN ( MYLINE(1:PAGEWD)                 )
               ELSE
                  CALL NSPWLN ( TITLE(I)(1:PAGEWD)               )
               END IF
            ELSE
               CALL NSPWLN (    MYLINE(1:PAGEWD)                 )
            END IF
 
         END DO
C
C        Next output whatever portion of the header section is
C        appropriate.
C
         MYLINE = ' '
 
         DO I = 1, NEED(H)
            IF ( VISIBL(H) ) THEN
               CALL NSPWLN ( HEADER(I)(1:PAGEWD) )
            ELSE
               CALL NSPWLN ( MYLINE   (1:PAGEWD) )
            END IF
         END DO
 
      END IF
C
C     Write the line and update the number of lines we
C     have written so far.
C
      ROW     =   ROW  + 1
      MYLINE  =   LINE
      CALL NSPWLN ( MYLINE(1:PAGEWD) )
 
C
C     If we reached the end of the body section, write out
C     the footer (provided we are on the right page). And
C     update the page number.
C
      IF ( ROW .EQ. SIZE(B) ) THEN
C
C        The user may want to have the page number appear
C        in the footer.  So we replace the PAGMRK by the
C        number if this is the case.
C
         MYLINE = ' '
 
         DO I = 1, NEED(F)
 
            IF ( VISIBL(F) ) THEN
               IF ( DOMARK ) THEN
                  CALL REPMI  ( FOOTER(I), PAGMRK(1:PAGMLN), PAGENO,
     .                          MYLINE                    )
                  CALL NSPWLN ( MYLINE   (1:PAGEWD)       )
               ELSE
                  CALL NSPWLN ( FOOTER(I)(1:PAGEWD)       )
               END IF
            ELSE
               CALL NSPWLN    ( MYLINE   (1:PAGEWD)       )
            END IF
 
         END DO
C
C        Advance the page number and reset the row to zero.
C        (we won't have written anything in the body of the
C        next page until later.)
C
         PAGENO = PAGENO + 1
         ROW    = 0
 
 
         IF ( DOPRMT ) THEN
            CALL PROMPT ( QUESTN(1:QLENTH), RESPNS )
            DIDPMT = .TRUE.
         END IF
 
      END IF
 
      RETURN
 
 
 
C$Procedure PAGPMT ( Page prompt returned )
 
      ENTRY PAGPMT ( VALUE, LINE )
 
C$ Abstract
C
C    Determine if a prompt issued and a value returned on the last
C    call to PAGPUT.
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
C     PAGE MANAGER
C
C$ Declarations
C
C     INTEGER               VALUE
C     CHARACTER*(*)         LINE
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     VALUE      O   1 if a prompt was entered, 0 otherwise.
C     LINE       O   The value of the prompt supplied
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     VALUE      is an integer indicating whether or not a prompt was
C                displayed and a value returned.  If no prompt was
C                issued on the last call to PAGPUT, VALUE will have the
C                value zero.  Otherwise VALUE will have some non-zero
C                value.
C
C     LINE       is the value of the prompt returned if there was one.
C                Otherwise a blank is returned.
C
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This entry point returns information about what happened in the
C     last call to PAGPUT.  If a page was finished and a prompt was
C     displayed and the user responded, this routine will return two
C     a non-zero integer for VALUE and will place the results of the
C     prompt in the string LINE.
C
C     Note that this routine will return the same results until some
C     call to PAGPUT is made again.
C
C$ Examples
C
C     Suppose that you are using the page manager to send output
C     to some device.  But that you want to allow the user to
C     pause in the course of sending the output. The following
C     illustrates how you would do this.
C
C     CALL getnext  ( LINE, MORE )
C
C     DO WHILE ( MORE )
C
C        CALL PAGPUT ( LINE )
C        CALL PAGPMT ( VALUE, RESPNS )
C
C        IF ( VALUE .NE. 0 )
C
C           take some action concerning RESPNS
C
C        END IF
C
C        CALL getnext  ( LINE, MORE )
C
C     END DO
C
C
C
C     Alternatively you might like to just have the page manager
C     wait for the user after a page has been finished.  To do this
C     you could set things up as follows.
C
C     CALL PAGSET ( 'PROMPT', 0 )
C     CALL PAGSCN ( 'PROMPT'    )
C     CALL PAGPUT ( '(Hit Return to Continue) >'
C
C     CALL getnext  ( LINE, MORE )
C
C     DO WHILE ( MORE )
C
C        CALL PAGPUT  ( LINE )
C        CALL getnext ( LINE, MORE )
C
C     END DO
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C       None.
C
C$ Author_and_Institution
C
C       W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 16-AUG-1995 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     See if the last line sent resulted in a prompt
C
C-&
 
      IF ( DIDPMT ) THEN
         VALUE = 1
         LINE  = RESPNS
      ELSE
         VALUE = 0
         LINE  = ' '
      END IF
 
 
      RETURN
      END

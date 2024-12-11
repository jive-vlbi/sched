	SUBROUTINE SYMSUB (A,B,L,N)
C-----------------------------------------------------------------------
C SYMSUB: perform DCL symbol substitution in a string. The routine
C looks for symbol names delimited by braces ({...}) in the input
C string; if the string between the braces is a defined DCL symbol
C name, the value of the symbol is substituted for the {...}.
C Example: suppose the symbol XX has been defined via the DCL command:
C	$ XX = "RESULT",
C then the following input string to SYMSUB
C	'THIS IS THE {XX}!'
C produces the following output
C	'THIS IS THE RESULT!'.
C A null string is substituted if the symbol is undefined, or if the 
C process is not run under DCL, or if any other error occurs.  
C DCL symbols consist of letters, digits, underscores and
C dollar signs. 
C
C Arguments:
C  Input:  CHARACTER*(*) A	Input string.
C  Output: CHARACTER*(*) B	Output string; may be same as A.
C  Output: INTEGER L		The number of characters in B.
C  Output: INTEGER N		The number of substitutions performed
C				(equal to the number of &s in A).
C
C Subroutines required:
C  LIB$GET_SYMBOL (VMS)
C
C History:
C  Version 1.0:  1983 Dec 5	T.J. Pearson
C  Version 1.1:  1984 Jan 2	TJP; change from &XX to {XX}
C-----------------------------------------------------------------------
	IMPLICIT NONE
	CHARACTER*(*) A,B
	CHARACTER*1 C
	CHARACTER*80 BADSYM
	INTEGER I, J, K, L, N, NBAD, LBAD
	INTEGER LIB$GET_SYMBOL
	LOGICAL VALID
C
C		Function VALID defines the legal characters for
C		DCL symbols.
C
	VALID(C) = C.EQ.'_' .OR. C.EQ.'$' .OR.
     1		   ('A'.LE.C.AND.C.LE.'Z') .OR.
     2		   ('a'.LE.C.AND.C.LE.'z') .OR.
     3		   ('0'.LE.C.AND.C.LE.'9')
C
C		Copy the input string to the output, taking special
C		action if we find an ampersand.
C
	NBAD = 0
	N = 0
	I = 1
	J = 0
	DO WHILE (I.LE.LEN(A))
	   IF (A(I:I).EQ.'{') THEN
		N = N+1
		K = I+1
		I = I+1
		DO WHILE (I.LE.LEN(A) .AND. VALID(A(I:I)))
		    I = I+1
		END DO
		IF (I.GT.K) THEN
		    IF (LIB$GET_SYMBOL(A(K:I-1),B(J+1:),L).EQ.1) THEN
			J = J+L
		    ELSE
			NBAD = NBAD+1
			IF (NBAD.EQ.1) THEN
			    BADSYM = A(K:I-1)
			    LBAD = MIN(LEN(BADSYM),I-K)
			ELSE
			    BADSYM = BADSYM(:LBAD)//', '//A(K:I-1)
			    LBAD = MIN(LEN(BADSYM),LBAD+I-K+2)
			END IF
		    END IF
		END IF
		IF (A(I:I).EQ.'}') THEN
		    I = I+1
		ELSE
		    CALL LIB$PUT_OUTPUT('+++ERROR+++ } missing')
		END IF
	   ELSE
		J = J+1
		IF (J.LE.LEN(B)) B(J:J) = A(I:I)
		I = I+1
	   END IF
	END DO
	L = MIN(J,LEN(B))
	IF (NBAD.GT.0) THEN
	    CALL LIB$PUT_OUTPUT('+++ERROR+++ Unknown symbol(s): '//
     1		BADSYM(1:LBAD))
	END IF
	RETURN
	END

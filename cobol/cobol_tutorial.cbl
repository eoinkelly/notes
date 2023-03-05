       >>SOURCE FORMAT IS FREE
*>     ^^^ is a compiler directive which tells the open-cobol compiler that the
*>     source format is free not fixed

*> * Comments
*> In free format COBOL the comment is *>
*> In fixed format COBOL the comment is * in col 7

*> *********************************************************************
*> *********************************************************************
IDENTIFICATION DIVISION.
PROGRAM-ID. cobol_tutorial.

*> These are marked as obsolete in GnuCOBOL
*> AUTHOR. Eoin Kelly.
*> DATE-WRITTEN. 31 Jan 2023

*> *********************************************************************
*> *********************************************************************
ENVIRONMENT DIVISION.

*> *********************************************************************
*> *********************************************************************
DATA DIVISION.

FILE SECTION. *> *******************************************************
*> define data both sent and received from storage

WORKING-STORAGE SECTION.
*> Define global variables for the program
*> ***************************************

*> create a local variable which has type alphanumeric, max length of 30 and an initial value of "JohnDoe"
*> COBOL is not strongly typed so you can put any type in any variable
*> so why bother with the types?
*> you can declare a var has a type but you can assign any type to it
*> numeric can be float or integer
01 UserName PIC X(30) VALUE "JohnDoe".

*> store a numeric with a single digit
01 Num1 PIC 9 VALUE ZEROS.
01 Num2 PIC 9 VALUE ZEROS.
01 Total PIC 99 VALUE 0.
01 SSNum.
      02 SSArea PIC 999.
			02 SSGroup PIC 99.
			02 SSSerial PIC 9999.
*> define a constant
01 PiValue CONSTANT AS 3.14.

*> figurative constants ZERO, ZEROS, SPACE, SPACES, HIGH-VALUE, HIGH-VALUES
*> (largest value of the defined type), LOW-VALUE, LOW-VALUES (lowest defined type)

01 SampleData PIC X(50).

01 AnyAlphanumeric PIC X(10) VALUE "Default12".
*> If you put the wrong kind of data in a variable you just get a warning at compile time. What happens at runtime?
01 JustLetters PIC A(10) VALUE "default".
01 JustNumbers PIC 9(4) VALUE 1234.
01 SignedInt PIC S9(4) VALUE -2345.
01 Floaty PIC 9(4)V99 VALUE ZEROS.

01 Customer.
	02 Ident PIC 9(3).
	02 CustName PIC 9(10).
	02 DateOfBirth.
		03 DOB PIC 99.
		03 MOB PIC 99.
		03 YOB PIC 9999.

*> PICTURE string must contain at least one of the set A, N, X, Z, 1, 9 and *; or at least two of the set +, - and the currency symbol

01 Num4 PIC 9 VALUE 5.
01 Num5 PIC 9 VALUE 6.
01 Num6 PIC 9 VALUE 7.
01 Answer PIC S99v99 VALUE 0.
01 Rem PIC 9V99.

01 ProgNameArea PIC X(50).
01 Pid PIC 9(5).

REPORT SECTION. *> *****************************************************

*> For some reason adding this breaks the program
*> LINKAGE SECTION. *> *************************************************

*> *********************************************************************
*> *********************************************************************
PROCEDURE DIVISION.

*> DISPLAY "hello" WITH NO ADVANCING
*> DISPLAY " there"
*> DISPLAY "done"
*> DISPLAY "Enter username:" WITH NO ADVANCING
*>
*> *> ACCEPT UserName
*> DISPLAY "Hi: " UserName
*>
*> *> Perform assignment with MOVE .. TO
*> MOVE ZERO TO UserName *> generates a compiler warning because UserName is alphanumeric
*> DISPLAY "Hi: " UserName
*>
*> MOVE 0 TO UserName *> generates a compiler warning because UserName is alphanumeric
*> DISPLAY "Hi: " UserName
*>
*> MOVE "placeholder" TO UserName
*> DISPLAY "Hi: " UserName
*>
*> DISPLAY "Enter two single-digit numbers to sum:"
*> ACCEPT Num1
*> Accept Num2
*> COMPUTE Total = Num1 + Num2
*> DISPLAY Total
*> if you enter 4, 88 the anser will be 12 because they are both single digit

*> DISPLAY "ENter your SSN (9 digits):"
*> ACCEPT SSNum
*> DISPLAY SSArea "-" SSGroup "-" SSSerial

*> QUESTION: does cobol use EDCBIC by default too on my mac? or does it use ascii?

MOVE "stuff" TO SampleData
MOVE "more stuff" TO SampleData

*> This is allowed but I get a compiler warning about it. I get no error at runtime
*> ./cobol_tutorial.cbl:121: warning: alphanumeric value is expected [-Wstrict-typing]
*> ./cobol_tutorial.cbl:54: note: 'SampleData' defined here as PIC X(50) [-Wstrict-typing]
*> MOVE 123 TO SampleData

DISPLAY SampleData
DISPLAY Floaty

*> You can parse a string into a data structure based on lenghts of each part of
*> the structure
MOVE "123JohnDoe   22011980" TO Customer

DISPLAY DOB
DISPLAY MOB
DISPLAY YOB
DISPLAY DateOfBirth
DISPLAY "'" CustName "'"

MOVE ZERO TO SampleData
DISPLAY SampleData

MOVE SPACES TO SampleData
DISPLAY SampleData

*> Move 0xFF into each character
MOVE HIGH-VALUE TO SampleData
DISPLAY SampleData

*> Move 0x00 into each character
MOVE LOW-VALUE TO SampleData
DISPLAY SampleData

MOVE ALL "3" TO SampleData
DISPLAY SampleData

MOVE QUOTE TO SampleData
DISPLAY SampleData

*> discover the name of the process which called this cobol process and put it in ProgNameArea
CALL "C$CALLEDBY" USING ProgNameArea
display ProgNameArea

call "C$GETPID"
display RETURN-CODE


STOP RUN.

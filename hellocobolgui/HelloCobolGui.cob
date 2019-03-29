GCobol >>SOURCE FORMAT IS FREE
*> HelloCobolGui.cob
*>*************************************
*> Tektonics: cobc -x -luser32 -fstatic-call -Wall -debug  HelloCobolGui.cob
*>*************************************
IDENTIFICATION DIVISION.
PROGRAM-ID. HelloCobolGui.
     REMARKS.  This is test of Window MessageBox.

environment division. configuration section. 
    repository. 
    function all intrinsic.

DATA DIVISION.
   WORKING-STORAGE SECTION.
       01 MB-OK PIC 9(18) VALUE 0.
       01 MESSAGE-TEXT PIC X(12).
       01 MESSAGE-CAPTION PIC X(12).
       01 RET PIC 9(9).

PROCEDURE DIVISION.
MAIN SECTION.
   MOVE "Hello Cobol!" TO MESSAGE-TEXT.
   MOVE "GUI" TO MESSAGE-CAPTION.
   PERFORM MESSAGE-BOX.
   STOP RUN.
    
MESSAGE-BOX SECTION.
   CALL 'MessageBoxA' USING 
       NULL, 
       BY REFERENCE MESSAGE-TEXT, 
       BY REFERENCE MESSAGE-CAPTION,
       BY CONTENT MB-OK
       RETURNING RET
   END-CALL.
   DISPLAY RET.

END PROGRAM HelloCobolGui.

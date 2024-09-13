       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALLSUB.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NUM1       PIC 9(4) VALUE 10.
       01 WS-NUM2       PIC 9(4) VALUE 20.
       01 WS-RESULT     PIC 9(4).
       01 WS-RETURN-CODE PIC 9(4).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "Calling subprogram to add two numbers..."
           CALL 'SUBPROG' USING BY VALUE WS-NUM1, BY CONTENT WS-NUM2, BY REFERENCE WS-RESULT GIVING WS-RETURN-CODE
               ON EXCEPTION
                   DISPLAY "Subprogram not found or error occurred during execution"
                   MOVE 1 TO WS-STATUS-CODE
               NOT ON EXCEPTION
                   DISPLAY "Subprogram executed successfully"
                   MOVE 0 TO WS-STATUS-CODE
           END-CALL.
           IF WS-RETURN-CODE = 0 THEN
               DISPLAY "Addition successful, result: " WS-RESULT
           ELSE
               DISPLAY "Subprogram returned an error with code: " WS-RETURN-CODE
           END-IF.
           STOP RUN.
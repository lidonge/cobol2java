       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTSTRING.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WS-STRING-A           PIC X(20) VALUE 'Hello, COBOL'.
       01 WS-STRING-B           PIC X(20) VALUE 'Welcome to COBOL'.
       01 WS-CONCATENATED       PIC X(40).
       01 WS-SEARCH-STRING      PIC X(20) VALUE 'COBOL'.
       01 WS-EXTRACTED-PART1    PIC X(10).
       01 WS-EXTRACTED-PART2    PIC X(10).
       01 WS-SEARCH-RESULT      PIC 9(2) COMP.
       01 WS-REPLACED-STRING    PIC X(20).
       01 WS-UPPER-STRING       PIC X(20).
       01 WS-LOWER-STRING       PIC X(20).

       PROCEDURE DIVISION.
           DISPLAY 'Original String A: ' WS-STRING-A.
           DISPLAY 'Original String B: ' WS-STRING-B.

* STRING
           STRING WS-STRING-A WS-STRING-A DELIMITED BY ' '
                  WS-STRING-B WS-STRING-B DELIMITED BY '#'
                  INTO WS-CONCATENATED
           END-STRING
           DISPLAY 'Concatenated String: ' WS-CONCATENATED.

* UNSTRING
           UNSTRING WS-STRING-A
               DELIMITED BY ','
               INTO WS-EXTRACTED-PART1 WS-EXTRACTED-PART2
           END-UNSTRING
           DISPLAY 'Extracted Part 1: ' WS-EXTRACTED-PART1
           DISPLAY 'Extracted Part 2: ' WS-EXTRACTED-PART2.

* INSPECT
           INSPECT WS-STRING-B TALLYING WS-SEARCH-RESULT
               FOR ALL WS-SEARCH-STRING BEFORE INITIAL 'X'
           DISPLAY 'Occurrences of "COBOL" in String B: ' WS-SEARCH-RESULT.

* INSPECT with REPLACING
           MOVE WS-STRING-B TO WS-REPLACED-STRING
           INSPECT WS-REPLACED-STRING
               REPLACING FIRST 'COBOL' BY 'JAVA'
           DISPLAY 'Replaced String: ' WS-REPLACED-STRING.

* COMPARE
           IF WS-STRING-A = WS-STRING-B
               DISPLAY 'Strings are equal.'
           ELSE
               DISPLAY 'Strings are not equal.'
           END-IF.

* CASE
           MOVE FUNCTION UPPER-CASE(WS-STRING-A) TO WS-UPPER-STRING
           MOVE FUNCTION LOWER-CASE(WS-STRING-A) TO WS-LOWER-STRING
           DISPLAY 'Uppercase String A: ' WS-UPPER-STRING
           DISPLAY 'Lowercase String A: ' WS-LOWER-STRING.

           STOP RUN.
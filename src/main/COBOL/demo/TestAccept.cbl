    IDENTIFICATION DIVISION.
    PROGRAM-ID. TEST-ACCEPT.
    DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 SOME-PERSON.
          05 PERSON-NAME PIC X(20) VALUE "Grace Hopper".
       01 DESC.
          COPY  DESC.
       01 WS-STUDENT-NAME PIC X(25).
       01 WS-DATE PIC X(10).
    PROCEDURE DIVISION.
       ACCEPT WS-STUDENT-NAME.
       ACCEPT WS-DATE FROM DATE.
       DISPLAY "Name :  " WS-STUDENT-NAME.
       DISPLAY "Date : " WS-DATE.
       DISPLAY PERSON-NAME.
    STOP RUN.
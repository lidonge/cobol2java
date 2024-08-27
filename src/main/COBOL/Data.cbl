       IDENTIFICATION DIVISION.
       PROGRAM-ID. WorkingStorageExample.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-VARIABLES.
           05 WS-NUMBER      PIC 9(5)V99 VALUE 12345.
           05 WS-TEXT        PIC X(20) VALUE 'HELLO WORLD'.
           05 WS-COUNTER     PIC 9(3) VALUE 0.
           05 WS-FLAG        PIC X VALUE 'N'.

       01 WS-RECORDS.
           05 WS-RECORD-ITEM.
               10 WS-ITEM-1    PIC X(10) VALUE 'ITEM1'.
               10 WS-ITEM-2    PIC 9(5) VALUE 10000.

       01 WS-COMPUTED-VALUE.
           05 WS-RESULT      PIC 9(5)V99 VALUE 0.
           05 WS-TEMP        PIC 9(3) VALUE 0.

       01 WS-ARRAYS.
           05 WS-NUMBER-ARRAY.
               10 WS-NUMBER-ITEM OCCURS 10 TIMES PIC 9(3) VALUE 0.

       01 WS-MULTI-DIMENSIONAL-ARRAY.
           05 WS-MATRIX.
               10 WS-MATRIX-ROW OCCURS 5 TIMES.
                   15 WS-MATRIX-CELL OCCURS 4 TIMES PIC 9(3) VALUE 0.
       PROCEDURE DIVISION.
           DISPLAY 'Number: ' WS-NUMBER
           DISPLAY 'Text: ' WS-TEXT OF WS-VARIABLES
           DISPLAY 'Counter: ' WS-COUNTER
           DISPLAY 'Flag: ' WS-FLAG
           DISPLAY 'Result: ' WS-RESULT
           DISPLAY 'Temp: ' WS-TEMP
           DISPLAY 'Array Item 1: ' WS-NUMBER-ITEM (1)
           DISPLAY 'Matrix Cell [1][1]: ' WS-MATRIX-CELL (1,1)


           STOP RUN.
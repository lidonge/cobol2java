       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUBPROG.

       DATA DIVISION.
       LINKAGE SECTION.
       01 LK-NUM1       PIC 9(4).
       01 LK-NUM2       PIC 9(4).
       01 LK-RESULT     PIC 9(4).
       01 LK-RETURN-CODE PIC 9(4).
       01 LK-CLS.
           05 LK-NUMA       PIC 9(4).
           05 LK-NUMB       PIC 9(4).
       PROCEDURE DIVISION USING LK-NUM1 LK-NUM2 LK-RESULT
                                            RETURNING LK-RETURN-CODE.
       SUB-LOGIC.
           COMPUTE LK-RESULT = LK-NUM1 + LK-NUM2
           MOVE 0 TO LK-RETURN-CODE
           EXIT PROGRAM.
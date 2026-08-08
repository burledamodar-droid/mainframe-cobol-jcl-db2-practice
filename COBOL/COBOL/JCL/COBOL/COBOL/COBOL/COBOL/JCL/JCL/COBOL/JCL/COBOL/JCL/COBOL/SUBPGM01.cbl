       ID DIVISION.
       PROGRAM-ID. SUBPGM01.
       AUTHOR. NAME.
       DATE-WRITTEN. TODAY.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.

       01 WS-SPACE-LEN    PIC 9(02) VALUE ZERO.
       01 WS-I            PIC 9(02) VALUE ZERO.

       LINKAGE SECTION.

       01 CUST-NAME       PIC X(20).
       01 STR-LEN         PIC 9(02).
       01 LETTER-FOUND    PIC X(01).

       PROCEDURE DIVISION USING CUST-NAME
                                STR-LEN
                                LETTER-FOUND.

       000-MAIN-PARA.

           DISPLAY 'AM IN SUBPGM01'.

           PERFORM 100-FIND-STR-LEN-PARA THRU 100-EXIT.
           PERFORM 200-FIND-LETTER-PARA THRU 200-EXIT.

           GOBACK.

       100-FIND-STR-LEN-PARA.

           MOVE ZERO TO WS-SPACE-LEN.

           INSPECT FUNCTION REVERSE(CUST-NAME)
               TALLYING WS-SPACE-LEN
               FOR LEADING SPACE.

           DISPLAY 'EXTRA SPACE COUNT ' WS-SPACE-LEN.

           COMPUTE STR-LEN =
                   LENGTH OF CUST-NAME - WS-SPACE-LEN.

       100-EXIT.
           EXIT.

       200-FIND-LETTER-PARA.

           MOVE +1 TO WS-I.

           PERFORM UNTIL WS-I > 20
                      OR WS-I > STR-LEN
                      OR LETTER-FOUND = 'Y'

               IF CUST-NAME(WS-I:1) = 'A'
                   MOVE 'Y' TO LETTER-FOUND
               END-IF

               DISPLAY 'WS-I IS ' WS-I

               ADD +1 TO WS-I

           END-PERFORM.

       200-EXIT.
           EXIT.

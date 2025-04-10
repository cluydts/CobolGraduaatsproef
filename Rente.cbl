       IDENTIFICATION DIVISION.
       PROGRAM-ID. RENTE-BEREKENING.
       environment division.
       input-output section.
           file-control.
               select input-file assign to "Inputfile.csv"
                   organization is line sequential.
               select output-file assign to "Outputfile.csv"
                   organization is line sequential.


       DATA DIVISION.
       file section.
           FD input-file.
            01 Leesregel pic x(80).

           FD output-file.
           01  CAPITAL-OUT PIC X(10).
           01  RATE-OUT PIC X(4).
           01  Years-OUT PIC x(2).
           01  INTEREST-OUT PIC x(10).

           01 EOF-Flag PIC 9 VALUE 0.

            01  CAPITAL-F PIC Z(8).99.
           01  RATE-F PIC Z(2).99.
        *>    01  Years-F PIC 9(2).
           01  INTEREST-F PIC Z(8).99.

              
       WORKING-STORAGE SECTION.
          
           01  TOTAL-AMOUNT PIC 9(8)V99.
           01  CAPITAL PIC x(10).
           01  RATE PIC x(4).
           01  Years PIC x(2).
           01  INTEREST PIC 9(8)V99.

           01  CAPITAL-N PIC 9(8)V99.
           01  RATE-N PIC 9(2)V99.
           01  Years-N PIC 9(2).

          


       PROCEDURE DIVISION.

           OPEN INPUT input-file
           OPEN OUTPUT output-file
           READ input-file INTO Leesregel
           PERFORM UNTIL EOF-Flag = 1
              read input-file INTO Leesregel
           AT END
               MOVE 1 to EOF-Flag
           NOT AT END
               UNSTRING Leesregel DELIMITED BY ","
                   INTO CAPITAL RATE Years
               END-UNSTRING
    
               MOVE function numval(CAPITAL) TO CAPITAL-N
               MOVE function numval(RATE) TO RATE-N
               MOVE function numval(Years) TO Years-N
               COMPUTE INTEREST = (CAPITAL-N * RATE-N * Years-N) / 100
               COMPUTE TOTAL-AMOUNT = CAPITAL-N + INTEREST

                move CAPITAL-N to CAPITAL-F
                move RATE-N to RATE-F
                move Years-N to Years-out
                move INTEREST to INTEREST-F

                write CAPITAL-F
                write RATE-F
                write Years-out
                write INTEREST-F

           END-READ

           END-PERFORM.
           close input-file.
              close output-file.
           STOP RUN.

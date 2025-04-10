       IDENTIFICATION DIVISION.
       PROGRAM-ID. RENTE-BEREKENING.

       input-output section.
           file-control.
               select input-file assign to "input.txt"
                   organization is line sequential.
               select output-file assign to "output.txt"
                   organization is line sequential.


       DATA DIVISION.
       file section.
           FD input-file.
            01 Leesregel pic x(80).
           FD output-file.
           01  CAPITAL-OUT PIC 9(8)V99.
           01  RATE-OUT PIC 9(2)V99.
           01  Years-OUT PIC 9(2).
           01  INTEREST-OUT PIC 9(8)V99.

              
       WORKING-STORAGE SECTION.
          
           01  TOTAL-AMOUNT PIC 9(8)V99.
           01  CAPITAL PIC x(10)V99.
           01  RATE PIC x(4)V99.
           01  Years PIC x(2).
           01  INTEREST PIC 9(8)V99.

           01  CAPITAL-N PIC 9(8)V99.
           01  RATE-N PIC 9(2)V99.
           01  Years-N PIC 9(2).


       PROCEDURE DIVISION.

           OPEN INPUT input-file
           OPEN OUTPUT output-file
           READ input-file INTO Leesregel
           PERFORM UNTIL input-file = "EOF"
               READ input-file INTO input-record
           AT END
               MOVE "EOF" TO input-file
           NOT AT END
               UNSTRING Leesregel DELIMITED BY ","
                   INTO CAPITAL RATE Years
               END-UNSTRING
    
               MOVE function numval(CAPITAL) TO CAPITAL-N
               MOVE function numval(RATE) TO RATE-N
               MOVE function numval(Years) TO Years-N
               COMPUTE INTEREST = (CAPITAL-N * RATE-N * Years-N) / 100
               COMPUTE TOTAL-AMOUNT = CAPITAL-N + INTEREST

               write CAPITAL-OUT
               write RATE-OUT
               write Years-OUT
               write INTEREST

           END-READ

           WRITE output-file FROM output-record

           END-PERFORM.
           STOP RUN.

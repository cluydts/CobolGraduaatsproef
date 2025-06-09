       IDENTIFICATION DIVISION.
       PROGRAM-ID. MorgageCalculation.
       
       environment division.
       input-output section.
           file-control.
               select input-file assign to DYNAMIC-INFILE
                   organization is line sequential
                   file status is WS-INPUT-STATUS.
               select output-file assign to DYNAMIC-OUTFILE
                   organization is line sequential
                   file status is WS-OUTPUT-STATUS.
       

       DATA DIVISION.
    
       file section.
           FD input-file.
       01 FILE-LINE pic x(100).

           FD output-file.
       01 Output-Regel PIC X(100).
                   
       WORKING-STORAGE SECTION.

       01  EOF-Flag PIC X(1) VALUE "0".
       01  DYNAMIC-INFILE pic x(30).
       01  DYNAMIC-OUTFILE pic x(30).

       01  WS-INPUT-STATUS  PIC XX.            
       01  WS-OUTPUT-STATUS PIC XX.
       01  OUTPUT-PREFIX pic x(8) value "Output-".

       01  HEADER PIC X(21) value "Capital,Rate,Interest".

       01  TOTAL-AMOUNT-S PIC 9(8)V99.
       01  CAPITAL-S PIC x(10).
       01  RATE-S PIC x(4).
       01  Years-S PIC x(2).

       01  DISPLAY-CAPITAL-S PIC Z(9).ZZ.
       01  DISPLAY-RATE-S PIC Z(2).ZZ.
       01  DISPLAY-Years-S PIC Z(2).
       01 DISPLAY-INTEREST-S PIC Z(8).ZZ.
       01  INTEREST PIC 9(8)V99.
       01  CAPITAL-N PIC 9(8)V99.
       01  RATE-N PIC 9(2)V99.
       01  Years-N PIC 9(2).

       linkage section.
       01 LINK-INPUT-FILE pic x(30).

       PROCEDURE DIVISION using LINK-INPUT-FILE.

           MOVE LINK-INPUT-FILE TO DYNAMIC-INFILE.
             string
              OUTPUT-PREFIX delimited by size
              DYNAMIC-INFILE delimited by size
              into DYNAMIC-OUTFILE
             end-string

           OPEN INPUT input-file
               DISPLAY "Status after OPEN input-file: " WS-INPUT-STATUS
               IF WS-INPUT-STATUS NOT = "00"
                   DISPLAY "Error opening input file: " WS-INPUT-STATUS
                   GOBACK
               END-IF
            OPEN OUTPUT output-file
            DISPLAY "Status after OPEN output-file: " WS-OUTPUT-STATUS
               IF WS-OUTPUT-STATUS NOT = "00"
                   DISPLAY "Error opening output file: " WS-OUTPUT-STATUS
                   GOBACK
               END-IF

           move HEADER to Output-Regel
           write Output-Regel
           move spaces to Output-Regel

           READ input-file INTO FILE-LINE *> skips Header
             DISPLAY "Status after 1st READ (header skip): " WS-INPUT-STATUS
           IF WS-INPUT-STATUS = "10" *> "10" is standard for EOF
               DISPLAY "EOF reached immediately after header read. Input file might be empty or just a header."
               MOVE "1" TO EOF-FLAG *> Ensure loop doesn't run if file is truly empty after header
           ELSE IF WS-INPUT-STATUS NOT = "00"
               DISPLAY "Error on 1st READ (header skip): " WS-INPUT-STATUS
               GOBACK
           END-IF
           
           PERFORM UNTIL EOF-Flag = "1"
              read input-file INTO FILE-LINE
                   AT END
                       MOVE "1" to EOF-Flag
           DISPLAY "AT END encountered in loop. Final input status: " WS-INPUT-STATUS
                       NOT AT END
                     DISPLAY "Status after data READ: " WS-INPUT-STATUS
                IF WS-INPUT-STATUS NOT = "00"
                  DISPLAY "I/O Error during data read: " WS-INPUT-STATUS
                   MOVE "1" TO EOF-FLAG *> Stop processing on error
                ELSE
      *>   -------------------------------------------------------------
               UNSTRING function trim(FILE-LINE)
               DELIMITED BY ","
                         OR ", "
                         INTO CAPITAL-S 
                              RATE-S 
                              Years-S
               END-UNSTRING
                DISPLAY "Capital: " CAPITAL-S
                DISPLAY "Rate: " RATE-S
                DISPLAY "Years: " Years-S
                display " "
      *>   -------------------------------------------------------------
           MOVE function numval-c(function trim(CAPITAL-S)) TO CAPITAL-N
           MOVE function numval-c(function trim(RATE-S)) TO RATE-N
           MOVE function numval-c(function trim(Years-S)) TO Years-N
      *>   -------------------------------------------------------------
                   
              DISPLAY "Capital-N: " CAPITAL-N
                DISPLAY "Rate-N: " RATE-N
                DISPLAY "Years-N: " Years-N
                DISPLAY " "
      *>   -------------------------------------------------------------
               COMPUTE INTEREST = (CAPITAL-N * RATE-N * Years-N)

               compute INTEREST = INTEREST /100
               COMPUTE TOTAL-AMOUNT-S = CAPITAL-N + INTEREST
           
                DISPLAY "INTEREST: " INTEREST
      *>   -------------------------------------------------------------

                   move CAPITAL-S to DISPLAY-CAPITAL-S
                   move RATE-S to DISPLAY-RATE-S
                   move Years-S to DISPLAY-Years-S

                   move INTEREST to DISPLAY-INTEREST-S 
                DISPLAY "INTEREST-S: " DISPLAY-INTEREST-S
                display " "

      *>   -------------------------------------------------------------     

           string
                function trim(DISPLAY-CAPITAL-S) delimited by size
                   "," delimited by size
                  function trim(DISPLAY-RATE-S) delimited by size
                   "," delimited by size
                   function trim(DISPLAY-Years-S) delimited by size
                   "," delimited by size
                  function trim(DISPLAY-INTEREST-S) delimited by size
                  into Output-Regel
                  end-string

                  display "Output-Regel: " Output-Regel
                   display "---------------------------"

                  write Output-Regel

      *>  -------------------------------------------------------------            
               
            DISPLAY "Status after WRITE output data: " WS-OUTPUT-STATUS
             IF WS-OUTPUT-STATUS NOT = "00"
           DISPLAY "Error writing data to output: " WS-OUTPUT-STATUS
           MOVE "1" TO EOF-FLAG *> Stop processing
             END-IF
           display "Output-regel: " output-regel
               
               move zeroes to DISPLAY-CAPITAL-S
               move zeroes to DISPLAY-RATE-S
               move zeroes to DISPLAY-Years-S
               move zeroes to DISPLAY-INTEREST-S
           end-if
            END-READ
              END-PERFORM.

           close input-file.
           DISPLAY "Status after CLOSE input-file: " WS-INPUT-STATUS
           close output-file.
            DISPLAY "Status after CLOSE output-file: " WS-OUTPUT-STATUS
                
           
           goback.

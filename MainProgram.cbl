       identification division.
       program-id. MainProgram.

       data division.
       working-storage section.

           01 USER-INPUT pic x(30).
           01 CSV-INPUT-FILE pic x(30).
           
       procedure division.
           display "Which calculation do you want to do?"
           display "1. VAT calculation"
           display "2. Morgage calculation"
           display "3. Salary calculation"
           accept USER-INPUT

            evaluate USER-INPUT
                when "1"
                   display "You have chosen VAT calculation"
                   display "Please enter the input file name:"
                   accept CSV-INPUT-FILE
                   call 'VAT-Calculation' using CSV-INPUT-FILE
                   
                when "2"
                   display "You have chosen Morgage calculation"
                   display "Please enter the input file name:"
                   accept CSV-INPUT-FILE
                   call 'Morgage-Calculation' using CSV-INPUT-FILE
                   
                when "3"
                   display "You have chosen Salary calculation"
                   display "Please enter the input file name:"
                   accept CSV-INPUT-FILE
                   call 'Salary-Calculation' using CSV-INPUT-FILE
                   
                when other
                display "Invalid choice stopping program."
                   stop run
            end-evaluate

            display "Program finished."
            
           stop run.    
           
           
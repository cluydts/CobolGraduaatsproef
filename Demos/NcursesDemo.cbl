           identification division.
           program-id. NcursesDemo.

           environment division.
           

           data division.
           working-storage section.
           01 UserInput PIC X(10).

           procedure division.
           DISPLAY "Welkom bij de UI!" AT 0101.
           DISPLAY "Geef een naam in: " AT 0201.
           ACCEPT UserInput AT 0220.
           DISPLAY "Hallo, " UserInput AT 0309.
           STOP RUN.

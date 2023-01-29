       identification division.
       program-id. A2_ItemList.
       author. Rob Savoie.
       date-written. Jan 23/2023.
      *
       environment division.
       configuration section.
       input-output section.
      *
       file-control.
      *input-file declaration
           select input-file
               assign to "../../../data/A2.dat"
               organization is line sequential.
      *output-file declaration
           select output-file
               assign to "../../../data/A2-ItemList.dat"
               organization is line sequential.
      *
       data division.
       file section.
      *this is incorrect for now
       fd input-file
           data record is input-line
           record contains 28 characters.
      *
       01 input-line                   pic x(28).
      *
       fd output-file
           data record is output-line
           record contains 132 characters.
      *
       01 output-line                  pic x(132).
       working-storage section.
      *
       01 ws-eof-flag                  pic x value "N".
      *
       01 ws-heading-line              pic x(18) value
                                       "This is my heading".
       procedure division.
      *
           goback.
      *
       100-process-file.
      *
      *clear output buffer
           move spaces to output-line.

      *move detail output data
      *    move il-number to ol-number.
      *    move il-name to ol-name.

      *write detail output
      *    write output-line.

      *read next record from input-file
           read input-file
               at end
                   move "Y" to ws-eof-flag.
      *
       end program A2_ItemList.
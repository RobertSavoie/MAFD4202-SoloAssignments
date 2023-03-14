       identification division.
       program-id. A6-DataValidation.
      *
       environment division.
      *
       input-output section.
       file-control.
      *
           select input-file
               assign to "../../../data/A6.dat"
               organization is line sequential.
      *
           select output-file
               assign to "../../../data/A6-DataValidation.out"
               organization is line sequential.
      *
       data division.
      *
       file section.
       fd input-file
           data record is input-line
           record contains 24 characters.
      *
       01 input-line.
           05 il-prt-mnt-code          pic x.
               88 il-addition              value "A".
               88 il-change                value "C".
               88 il-deletion              value "D".
           05 il-prt-num               pic 999.
               88 il-prt-numeric           value 0     thru 999.
           05 il-prt-desc              pic x(10).
               88 il-blank                 value " ".
      *        88 il-desc-numeric          value 0 thru 99.
           05 il-prt-unit-price        pic 99v99.
               88 il-price-range           value 1.00  thru 50.00.
           05 il-prt-vend-num          pic 9(6).
      *        88 il-vend-range            value 1     thru 3.
      *
       fd output-file
           data record is output-line
           record contains 80 characters.
      *
       01 output-line                  pic x(80)   value spaces.
      *
       working-storage section.
      *
      *name line
       01 ws-name-line.
           05 filler                   pic x(24)   value
                                       "ROB SAVOIE, ASSIGNMENT 6".
           05 filler                   pic x(16)   value spaces.
      *
       procedure division.
       000-main.
      *
           goback.
      *
       end program A6-DataValidation.

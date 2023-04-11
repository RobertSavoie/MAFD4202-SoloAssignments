       identification division.
       program-id. A7-CallCenterOpReport.
       author. Rob Savoie.
       date-written.  Apr 03/2023.
      *Program Description: 
      *
       environment division.
       input-output section.
       file-control.
      *
           select emp-file
               assign to '../../../data/A7.dat'
               organization is line sequential.
      *
           select report-file
               assign to '../../../data/A7-CallCenterOpReport.out'
               organization is line sequential.
      *
       data division.
       file section.
      *
       fd emp-file
           data record is emp-rec
           record contains 51 characters.
      *
       01 emp-rec.
           05 emp-rec-num              pic x(3).
           05 emp-rec-name             pic x(12).
           05 emp-rec-calls-tbl.
               10 emp-rec-calls-month      pic 999 occurs 12 times.
      *
       fd report-file
           data record is report-line
           record contains 132 characters.
      *
       01 report-line                  pic x(132).
      *
       working-storage section.
      *
      *create the necessary working storage variables
      *
       01 ws-constants.
           05 ws-number-of-months      pic 99      value 12.
      *
       01 ws-calculated-fields.
           05 ws-non-zero-month-count  pic 9(2)    value 0.
      *
       01 ws-eof-flag                  pic x       value 'n'.
           88 ws-end-of-file                       value "y".
      *
       01 ws-totals.
           05 ws-grand-total           pic 9(5)    value 0.
           05 ws-emp-total             pic 9(5)    value 0.
           05 ws-total-no-calls        pic 9(5)    value 0.
           05 ws-total-zero-mnths      pic 9(5)    value 0.
      *
       01 ws-counter.
           05 cntr-index               pic 99      value 1.
           05 cntr-zero-mnths          pic 99      value 0.
           05 cntr-average-calc        pic 99      value 0.
      *
       01 calc-tbl.
           05 cntr-mnth-ops            pic 99      occurs 12 times
                                                   value 0.
           05 total-mnth-calls         pic 9(4)    occurs 12 times
                                                   value 0.
           05 average-mnth-calls       pic 9(4)    occurs 12 times
                                                   value 0.
      *
       01 ws-math.
           05 ws-average               pic 9(5)    value 0.
           05 ws-rem                   pic 9       value 0.
      *
       01 ws-name-line.
           05 filler                   pic x(5)
               value spaces.
           05 filler                   pic x(25)
               value '    Y o u r   N a m e    '.
      *               ----+----1----+----2----+
           05 filler                   pic x(29)
               value '                        lab 7'.
      *               ----+----1----+----2----+----
           05 filler                   pic x(5)
               value spaces.
           05 ws-name-line-date        pic 9(6).
           05 filler                   pic x(4)
               value spaces.
           05 ws-name-line-time        pic 9(8).
           05 filler                   pic x(50)
               value spaces.
      *
       01 ws-report-heading.
           05 filler                   pic x(40)
               value spaces.
           05 filler                   pic x(40)
               value 'call centre volumes for july - june     '.
      *               ----+----1----+----2----+----3----+----4
           05 filler                   pic x(40)
               value spaces.
           05 filler                   pic x(12)
               value spaces.
      *
       01 ws-heading-line1.
           05 filler                   pic x(2)    value spaces.
           05 filler                   pic x(8)    value 'operator'.
           05 filler                   pic x(2)    value spaces.
           05 filler                   pic x(8)    value 'operator'.
           05 filler                   pic x(6)    value spaces.
           05 filler                   pic x(3)    value 'jul'.
           05 filler                   pic x(4)    value spaces.
           05 filler                   pic x(3)    value 'aug'.
           05 filler                   pic x(4)    value spaces.
           05 filler                   pic x(3)    value 'sep'.
           05 filler                   pic x(4)    value spaces.
           05 filler                   pic x(3)    value 'oct'.
           05 filler                   pic x(4)    value spaces.
           05 filler                   pic x(3)    value 'nov'.
           05 filler                   pic x(4)    value spaces.
           05 filler                   pic x(3)    value 'dec'.
           05 filler                   pic x(4)    value spaces.
           05 filler                   pic x(3)    value 'jan'.
           05 filler                   pic x(4)    value spaces.
           05 filler                   pic x(3)    value 'feb'.
           05 filler                   pic x(4)    value spaces.
           05 filler                   pic x(3)    value 'mar'.
           05 filler                   pic x(4)    value spaces.
           05 filler                   pic x(3)    value 'apr'.
           05 filler                   pic x(4)    value spaces.
           05 filler                   pic x(3)    value 'may'.
           05 filler                   pic x(4)    value spaces.
           05 filler                   pic x(3)    value 'jun'.
           05 filler                   pic x(4)    value spaces.
           05 filler                   pic x(5)    value 'total'.
           05 filler                   pic x(6)    value spaces.
           05 filler                   pic x(3)    value 'avg'.
           05 filler                   pic x(2)    value spaces.
           05 filler                   pic x(3)    value 'rem'.
           05 filler                   pic x(3)    value spaces.
      *
       01 ws-heading-line2.
           05 filler                   pic x(5)    value spaces.
           05 filler                   pic x(1)    value '#'.
           05 filler                   pic x(8)    value spaces.
           05 filler                   pic x(4)    value 'name'.
           05 filler                   pic x(114) 
               value spaces.
      *
       01 ws-detail-line.
           05 filler                   pic x(4) 
               value spaces.
           05 ws-detail-line-num       pic x(3).
           05 filler                   pic x(6) 
               value spaces.
           05 ws-detail-line-name      pic x(12).
           05 filler                   pic x(1) 
               value spaces.
           05 ws-detail-line-jul       pic zz9.
           05 filler                   pic x(4).
           05 ws-detail-line-aug       pic zz9.
           05 filler                   pic x(4).
           05 ws-detail-line-sep       pic zz9.
           05 filler                   pic x(4).
           05 ws-detail-line-oct       pic zz9.
           05 filler                   pic x(4).
           05 ws-detail-line-nov       pic zz9.
           05 filler                   pic x(4).
           05 ws-detail-line-dec       pic zz9.
           05 filler                   pic x(4).
           05 ws-detail-line-jan       pic zz9.
           05 filler                   pic x(4).
           05 ws-detail-line-feb       pic zz9.
           05 filler                   pic x(4).
           05 ws-detail-line-mar       pic zz9.
           05 filler                   pic x(4).
           05 ws-detail-line-apr       pic zz9.
           05 filler                   pic x(4).
           05 ws-detail-line-may       pic zz9.
           05 filler                   pic x(4).
           05 ws-detail-line-jun       pic zz9.
           05 filler                   pic x(4).
           05 ws-detail-line-total     pic zzzz9.
           05 filler                   pic x(4) 
               value spaces.
           05 ws-detail-line-avg       pic zzzz9.
           05 filler                   pic x(4) 
               value spaces.
           05 ws-detail-line-rem       pic 9.
           05 filler                   pic x(3)    value spaces.

      *
       01 ws-footer1.
           05 filler                   pic x(4)    value spaces.
           05 filler                   pic x(20)   value
                                         "Operator with Calls:".
           05 filler                   pic x(3).
           05 ws-operator-jul          pic z9.
           05 filler                   pic x(5).
           05 ws-operator-aug          pic z9.
           05 filler                   pic x(5).
           05 ws-operator-sep          pic z9.
           05 filler                   pic x(5).
           05 ws-operator-oct          pic z9.
           05 filler                   pic x(5).
           05 ws-operator-nov          pic z9.
           05 filler                   pic x(5).
           05 ws-operator-dec          pic z9.
           05 filler                   pic x(5).
           05 ws-operator-jan          pic z9.
           05 filler                   pic x(5).
           05 ws-operator-feb          pic z9.
           05 filler                   pic x(5).
           05 ws-operator-mar          pic z9.
           05 filler                   pic x(5).
           05 ws-operator-apr          pic z9.
           05 filler                   pic x(5).
           05 ws-operator-may          pic z9.
           05 filler                   pic x(5).
           05 ws-operator-jun          pic z9.
      *
       01 ws-footer2.
           05 filler                   pic x(4)    value spaces.
           05 filler                   pic x(7)   value "Totals:".
           05 filler                   pic x(14).
           05 ws-total-call-jul        pic zzz9.
           05 filler                   pic x(3).
           05 ws-total-call-aug        pic zzz9.
           05 filler                   pic x(3).
           05 ws-total-call-sep        pic zzz9.
           05 filler                   pic x(3).
           05 ws-total-call-oct        pic zzz9.
           05 filler                   pic x(3).
           05 ws-total-call-nov        pic zzz9.
           05 filler                   pic x(3).
           05 ws-total-call-dec        pic zzz9.
           05 filler                   pic x(3).
           05 ws-total-call-jan        pic zzz9.
           05 filler                   pic x(3).
           05 ws-total-call-feb        pic zzz9.
           05 filler                   pic x(3).
           05 ws-total-call-mar        pic zzz9.
           05 filler                   pic x(3).
           05 ws-total-call-apr        pic zzz9.
           05 filler                   pic x(3).
           05 ws-total-call-may        pic zzz9.
           05 filler                   pic x(3).
           05 ws-total-call-jun        pic zzz9.
           05 filler                   pic x(3).
           05 ws-grand-totals          pic zzzz9.
           05 filler                   pic x(3)    value spaces.
           05 ws-grand-avg             pic zzz9.
           05 filler                   pic x(3)    value spaces.
           05 ws-grand-rem             pic 999.
      *
       01 ws-footer3.
           05 filler                   pic x(4)    value spaces.
           05 filler                   pic x(22)   value "Averages:".
           05 ws-average-jul           pic zz9.
           05 filler                   pic x(4).
           05 ws-average-aug           pic zz9.
           05 filler                   pic x(4).
           05 ws-average-sep           pic zz9.
           05 filler                   pic x(4).
           05 ws-average-oct           pic zz9.
           05 filler                   pic x(4).
           05 ws-average-nov           pic zz9.
           05 filler                   pic x(4).
           05 ws-average-dec           pic zz9.
           05 filler                   pic x(4).
           05 ws-average-jan           pic zz9.
           05 filler                   pic x(4).
           05 ws-average-feb           pic zz9.
           05 filler                   pic x(4).
           05 ws-average-mar           pic zz9.
           05 filler                   pic x(4).
           05 ws-average-apr           pic zz9.
           05 filler                   pic x(4).
           05 ws-average-may           pic zz9.
           05 filler                   pic x(4).
           05 ws-average-jun           pic zz9.
      *
       01 ws-total-line1.
           05 filler                   pic x(4) 
               value spaces.
           05 filler                   pic x(35)
               value "number of operators with no calls: ".
      *               ----+----1----+----2----+----3----+
           05 ws-total-line-no-calls   pic zzzz9.
           05 filler                   pic x(86) 
               value spaces.
      *
       01 ws-total-line2.
           05 filler                   pic x(4) 
               value spaces.
           05 filler                   pic x(35)
               value "number of months with no calls:    ".
      *               ----+----1----+----2----+----3----+
           05 ws-total-line-zero-mths  pic zzzz9.
           05 filler                   pic x(86) 
               value spaces.
      *
       01 ws-total-line3.
           05 filler                   pic x(4) 
               value spaces.
           05 filler                   pic x(35)
               value "overall total calls:               ".
      *               ----+----1----+----2----+----3----+
           05 ws-total-line-calls      pic zzzz9.
           05 filler                   pic x(86) 
               value spaces.
      *
       procedure division.
      *
       000-main.
      *
      *open files  
           open input  emp-file,
                output report-file.
      *
      *get the current date & time
           accept ws-name-line-date from date.
           accept ws-name-line-time from time.
      *
      *output first headings
           perform 100-print-headings.
      *
      *process input file & output results
           perform 200-read-input-file.
      *
           perform 300-process-records 
               until ws-end-of-file.
      *
      *output total lines
           perform 400-print-totals.
      *
      *close files
           close emp-file
                 report-file.
      *
           stop run.
      *
       100-print-headings.
      *
           write report-line from ws-name-line 
               after advancing 1 line.
      *
           write report-line from ws-report-heading
               after advancing 1 line.
      *
           write report-line from ws-heading-line1 
               after advancing 2 lines.
      *
           write report-line from ws-heading-line2 
               after advancing 1 line.
      *
       200-read-input-file.
      *reads a line from input file & stores it in emp-rec
      * - unless eof is encountered in which case it sets
      *    ws-eof-flag to y
           read emp-file 
           	   at end move 'y'         to ws-eof-flag.

       300-process-records.
      *
           perform 350-cycle-months
             varying cntr-index from 1 by 1
             until cntr-index > ws-number-of-months.

           move 1 to cntr-index.
      *
      * TODO: Implement average calculation logic
      *       as outlined in the requirments
           if ws-emp-total = 0
               add 1 to ws-total-no-calls
           end-if.

           divide ws-emp-total
               by cntr-average-calc
           giving ws-average rounded
           remainder ws-rem.

           perform 360-calculate-table-average
             varying cntr-index from 1 by 1
             until cntr-index > ws-number-of-months.
      *
           move emp-rec-num             to ws-detail-line-num.
           move emp-rec-name            to ws-detail-line-name.
           move emp-rec-calls-month(1)  to ws-detail-line-jul.
           move emp-rec-calls-month(2)  to ws-detail-line-aug.
           move emp-rec-calls-month(3)  to ws-detail-line-sep.
           move emp-rec-calls-month(4)  to ws-detail-line-oct.
           move emp-rec-calls-month(5)  to ws-detail-line-nov.
           move emp-rec-calls-month(6)  to ws-detail-line-dec.
           move emp-rec-calls-month(7)  to ws-detail-line-jan.
           move emp-rec-calls-month(8)  to ws-detail-line-feb.
           move emp-rec-calls-month(9)  to ws-detail-line-mar.
           move emp-rec-calls-month(10) to ws-detail-line-apr.
           move emp-rec-calls-month(11) to ws-detail-line-may.
           move emp-rec-calls-month(12) to ws-detail-line-jun.
           move ws-emp-total            to ws-detail-line-total.
           move ws-average              to ws-detail-line-avg.
           move ws-rem                  to ws-detail-line-rem.
           move ws-total-no-calls       to ws-total-line-no-calls.
      *
      * print detail line
           write report-line from ws-detail-line
               after advancing 1 lines.
      *
           move 0                       to ws-counter.
           move 0                       to ws-emp-total.
           move 0                       to ws-average.
           move 0                       to ws-rem.
      *
      * read next record (if any)
           perform 200-read-input-file.
      *
       350-cycle-months.
           if emp-rec-calls-month(cntr-index) = 0
               add 1   to cntr-zero-mnths
               add 1   to ws-total-zero-mnths
           else if emp-rec-calls-month(cntr-index) > 0
               add 1 to cntr-mnth-ops(cntr-index)
               add 1   to cntr-average-calc
           end-if
           end-if.
      *
           add emp-rec-calls-month(cntr-index)
                       to total-mnth-calls(cntr-index).
           add emp-rec-calls-month(cntr-index)
                       to ws-grand-total.
           add emp-rec-calls-month(cntr-index)
                       to ws-emp-total.
      *
       360-calculate-table-average.
           divide total-mnth-calls(cntr-index)
               by cntr-mnth-ops(cntr-index)
           giving average-mnth-calls(cntr-index).
       400-print-totals.
      * Move required data to total lines for output
      *
           move cntr-mnth-ops(1)        to ws-operator-jul.
           move cntr-mnth-ops(2)        to ws-operator-aug.
           move cntr-mnth-ops(3)        to ws-operator-sep.
           move cntr-mnth-ops(4)        to ws-operator-oct.
           move cntr-mnth-ops(5)        to ws-operator-nov.
           move cntr-mnth-ops(6)        to ws-operator-dec.
           move cntr-mnth-ops(7)        to ws-operator-jan.
           move cntr-mnth-ops(8)        to ws-operator-feb.
           move cntr-mnth-ops(9)        to ws-operator-mar.
           move cntr-mnth-ops(10)       to ws-operator-apr.
           move cntr-mnth-ops(11)       to ws-operator-may.
           move cntr-mnth-ops(12)       to ws-operator-jun.

           move total-mnth-calls(1)     to ws-total-call-jul.
           move total-mnth-calls(2)     to ws-total-call-aug.
           move total-mnth-calls(3)     to ws-total-call-sep.
           move total-mnth-calls(4)     to ws-total-call-oct.
           move total-mnth-calls(5)     to ws-total-call-nov.
           move total-mnth-calls(6)     to ws-total-call-dec.
           move total-mnth-calls(7)     to ws-total-call-jan.
           move total-mnth-calls(8)     to ws-total-call-feb.
           move total-mnth-calls(9)     to ws-total-call-mar.
           move total-mnth-calls(10)    to ws-total-call-apr.
           move total-mnth-calls(11)    to ws-total-call-may.
           move total-mnth-calls(12)    to ws-total-call-jun.

           move average-mnth-calls(1)   to ws-average-jul.
           move average-mnth-calls(2)   to ws-average-aug.
           move average-mnth-calls(3)   to ws-average-sep.
           move average-mnth-calls(4)   to ws-average-oct.
           move average-mnth-calls(5)   to ws-average-nov.
           move average-mnth-calls(6)   to ws-average-dec.
           move average-mnth-calls(7)   to ws-average-jan.
           move average-mnth-calls(8)   to ws-average-feb.
           move average-mnth-calls(9)   to ws-average-mar.
           move average-mnth-calls(10)  to ws-average-apr.
           move average-mnth-calls(11)  to ws-average-may.
           move average-mnth-calls(12)  to ws-average-jun.

           move ws-total-zero-mnths     to ws-total-line-zero-mths.
           move ws-total-no-calls       to ws-total-line-no-calls.
           move ws-grand-total          to ws-total-line-calls.
      *
           write report-line from ws-footer1
             after advancing 2 lines.
           write report-line from ws-footer2
             after advancing 1 lines.
           write report-line from ws-footer3
             after advancing 1 lines.
      *
           write report-line from ws-total-line1
               after advancing 2 lines.
           write report-line from ws-total-line2
               after advancing 2 lines.
           write report-line from ws-total-line3
               after advancing 2 lines.
      *
       end program A7-CallCenterOpReport.
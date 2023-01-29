       identification division.
       program-id. A1-ContactList.
       author. Rob Savoie.
       date-written. Jan 16/2023.
      *
       environment division.
       configuration section.
      *
       input-output section.
      *
       file-control.
           select output-file
               assign to "..\..\..\data\A1-ContactList.out"
               organization is line sequential.
      *
       data division.
       file section.
      *
       fd output-file
           data record is output-line
               record contains 58 characters.
      *output line
       01 output-line.
           05 ol-name                  pic x(16).
           05 ol-email                 pic x(30).
           05 ol-number                pic x(12).
       working-storage section.
      *heading line
       01 ws-heading-line.
           05 filler                   pic x(18).
           05 ws-heading               pic x(22) value
                                       "MAFD 4202 Contact List".
           05 filler                   pic x(18).
      *contact one
       01 ws-contact-one.
           05 ws-name-one              pic x(16) value "Rob Savoie".
           05 ws-email-one             pic x(30) value
                                       "robert.savoie1@dcmail.ca".
           05 ws-number-one            pic x(12) value "705-561-5095".
      *contact two
       01 ws-contact-two.
           05 ws-name-two              pic x(16) value "John Doe".
           05 ws-email-two             pic x(30) value "jdoe@dcmail.ca".
           05 ws-number-two            pic x(12) value "999-999-9999".
      *contact three
       01 ws-contact-three.
           05 ws-name-three            pic x(16) value "Jane Doe".
           05 ws-email-three           pic x(30) value
                                       "janedoe@dcmail.ca".
           05 ws-number-three          pic x(12) value "123-456-7890".
      *filler line
       01 ws-filler                    pic x.
      *
       procedure division.
       000-main.
      *open files
           open output output-file.
      *write header
           write output-line from ws-filler.
           write output-line from ws-heading-line.
           write output-line from ws-filler.
      *
           move ws-name-one to ol-name.
           move ws-email-one to ol-email.
           move ws-number-one to ol-number.
           write output-line.
           move spaces to output-line.
      *
           move ws-name-two to ol-name.
           move ws-email-two to ol-email.
           move ws-number-two to ol-number.
           write output-line.
           move spaces to output-line.
      *
           move ws-name-three to ol-name.
           move ws-email-three to ol-email.
           move ws-number-three to ol-number.
           write output-line.
      *
           goback.
      *
       end program A1-ContactList.
C ********************************************************************
C ***                    UTILITY:GETFORMAT.FOR                     ***
C ********************************************************************
C ***   An integer function that figures out which type of input   ***
C *** file was given: GIA Solution, Lico's Catalog, NEIS catalog,  ***
C ***     or Engdahl's catalog by examining the first record.      ***
C ***  The file pointer is then backspaced such that the calling   ***
C ***           program will not skip the first record.            ***
C ********************************************************************
C *** The entry define_location_formats(format_string_array) must  ***
C ***        be called once before the function can be used.       ***
C ********************************************************************
C ***   A returned value of: 0 = ERROR, UNRECOGNIZED FORMAT        ***
C ***                        1 = Lico's catalog format (historic)  ***
C ***                        2 = GIA solution format               ***
C ***                        3 = NEIS catalog format               ***
C ***                        4 = Engdahl's catalog format          ***
C ***                        5 = Hypoellipse 88 summary format     ***
C ***                        6 = NOAA catalog format 2 as of 6/88  ***
C ********************************************************************
C ***  WARNING: This routine is not too smart.  It may mistakenly  ***
C ***  identify a format if it is given one for which it was not   ***
C ***  designed.  Also, it may not work for cut down forms of the  ***
C ***        file formats that it is supposed to recognize.        ***
C ********************************************************************
C ***           Written by G. H. Cole Sonafrank, 3/5/87.           ***
C ********************************************************************

	INTEGER FUNCTION get_file_format (in_unit, status_str)

	CHARACTER record*132, format_str(4)*(*), status_str*(*)
	INTEGER in_unit, ifmt

	READ (in_unit,'(A)') record

C ---   Check for GIA heading record and skip it, if present.
	IF (record(1:5).EQ.' DATE') THEN
	  ifmt = 2
          status_str =
     &    'Input file presumed to be in GIA solution file format.'

	ELSE IF ((record(35:35).NE.' ' .OR. record(50:50).NE.' ') .AND.
     &    record(12:12).EQ.' ' .AND. record(18:18).EQ.' ') THEN
	  ifmt = 2			
	  BACKSPACE (UNIT=in_unit)		
          status_str =
     &    'Input file presumed to be in GIA solution file format.'

	ELSE IF (
C    &    INDEX(record,'.').EQ.0 .AND.
     &    (record(81:81).EQ.'/' .OR. record(81:81).EQ.'\\')) THEN
	  ifmt = 5
	  BACKSPACE (UNIT=in_unit)
          status_str =
     &    'Input file presumed to be in Hypoe88 summary format.'

	ELSE IF (record(7:8).EQ.'19'.AND. record(22:22).EQ.'.'.AND.
     &    record(30:30).EQ.'.'.AND.record(38:38).EQ.'.'.AND.
     &    (record(27:27).EQ.' '.OR.record(27:27).EQ.'-')) THEN
	  ifmt = 6				
	  BACKSPACE (UNIT=in_unit)
          status_str =
     &    'Input file presumed to be in NOAA catalog format.'

	ELSE IF (record(1:1).EQ.' '.AND. record(11:11).EQ.' '.AND.
     &    record(25:25).EQ.' ') THEN
	  ifmt = 4				
	  BACKSPACE (UNIT=in_unit)
          status_str =
     &    'Input file presumed to be in Engdahl''s format.'

	ELSE IF (INDEX(record,'.').EQ.0) THEN
C ---     There needs to be a better determinant here.
	  ifmt = 3
	  BACKSPACE (UNIT=in_unit)
          status_str =
     &    'Input file presumed to be in NEIS catalog format.'

	ELSE IF (record(10:11).EQ.'19'.AND.record(27:27).EQ.' '.AND.
     &    record(30:30).EQ.'.'.AND.record(35:35).EQ.' '.AND.
     &    record(39:39).EQ.'.'.AND.record(44:44).EQ.' ') THEN
	  ifmt = 1
	  BACKSPACE (UNIT=in_unit)
          status_str =
     &    'Input file presumed to be in Lico''s catalog format.'

        ELSE
          ifmt = 0
	  BACKSPACE (UNIT=in_unit)
          status_str =
     &    'Error, Input file format was not recognizable.'

	END IF

	get_file_format = ifmt

	RETURN
C ********************************************************************

	ENTRY define_location_formats (format_str)

C ---  1 = Lico's catalog format
C ---  2 = GIA solution format
C ---  3 = NEIS catalog format
C ---  4 = Engdahl's catalog format
C ---  5 = Hypoellipse 88 summary format
C ---  6 = NOAA catalog format (old commented out).

	format_str(1)='(27X,F6.3,A1,X,F7.3,A1,X,F5.1,X,3(F4.2,3X),F3.1)'
	format_str(2)='(17X,I3,A1,F5.2,I4,A1,F5.2,X,F6.2,3X,F4.2)'
	format_str(3)='(19X,I2,F3.3,A1,I3,F3.3,A1,F3.0)'
	format_str(4)='(25X,F7.3,F8.3,F5.1,12X,F4.2,3(7X,F4.2))'
	format_str(5)='(14X,I2,A1,F4.2,I3,A1,F4.2,F5.2,F2.1)'
C	format_str(6)='(25X,F5.3,2X,F6.3,6X,F3.2,X,F2.1,2(X,F3.2))'
	format_str(6)=
     &    '(26X,F7.3,F8.3,F5.1,7X,F3.1,2X,F3.1,3X,F4.2,7X,F4.2)'

	RETURN
C ********************************************************************

	END

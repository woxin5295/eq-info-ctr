C *****************************************************************
C ***                  /usr/tools/f/get_env.f                   ***
C *****************************************************************
C ***  A set of Fortran functions that obtain the value of an   ***
C ***   "environment" variable.  There are three forms, each    ***
C *** expects and returns a different data type for its second  ***
C ***  argument: getenv_r for REAL, getenv_i for INTEGER, and   ***
C ***               getenv_s for a CHARACTER*(*).               ***
C *****************************************************************
C *** The functions themselves return: 1 if a value is returned ***
C *** successfully; -1 if the the environment variable did not  ***
C ***  exist or had a blank or null value; or -2 if the value   ***
C ***   was a string when a number was expected.  The second    ***
C ***   argument is only changed if the function returns a 1.   ***
C *****************************************************************
C ***        Written by G. H. Cole Sonafrank, 5/18/88.          ***
C *****************************************************************

      INTEGER FUNCTION getenv_r (env_var, value)

      INTEGER max_str
      PARAMETER (max_str = 512)

      CHARACTER env_var*(*), val_str*(max_str)
      INTEGER status
      REAL value
      
      CALL GETENV (env_var, val_str)

      IF (val_str(1:5).EQ.'     ') THEN
        getenv_r = -1
      ELSE
        IF (ICHAR(val_str(1:1)).GE.ICHAR('0') .AND.
     *      ICHAR(val_str(1:1)).LE.ICHAR('9') .OR.
     *      val_str(1:1).EQ.'-' .OR. val_str(1:1).EQ.'+' .OR.
     *      val_str(1:1).EQ.'.') THEN
C         READ (val_str(1:max_str), '(F512)', IOSTAT=status) value
          READ (val_str(1:max_str), '(F512)', IOSTAT=status) value
          IF (status.NE.0) THEN
            getenv_r = -2
          ELSE
            getenv_r = 1
          ENDIF
        ELSE
          getenv_r = -2
        ENDIF
      ENDIF

      RETURN
      END
C **************************************************
      INTEGER FUNCTION getenv_i (env_var, ivalue)

      INTEGER max_str
      PARAMETER (max_str = 512)

      CHARACTER env_var*(*), val_str*(max_str)
      INTEGER ivalue, status
      
      CALL GETENV (env_var, val_str)

      IF (val_str(1:5).EQ.'     ') THEN
        getenv_i = -1
      ELSE
        IF (ICHAR(val_str(1:1)).GE.ICHAR('0') .AND.
     *      ICHAR(val_str(1:1)).LE.ICHAR('9') .OR.
     *      val_str(1:1).EQ.'-' .OR. val_str(1:1).EQ.'+') THEN
          READ (val_str(1:max_str), '(I12)', IOSTAT=status) ivalue
          IF (status.NE.0) THEN
            getenv_i = -2
          ELSE
            getenv_i = 1
          ENDIF
        ELSE
          getenv_i = -2
        ENDIF
      ENDIF

      RETURN
      END
C **************************************************
      INTEGER FUNCTION getenv_s (env_var, string)

      INTEGER max_str
      PARAMETER (max_str = 512)

      CHARACTER*(*) env_var, string, val_str*(max_str)
      
      CALL GETENV (env_var, val_str)

      IF (val_str(1:5).EQ.'     ') THEN
        getenv_s = -1
      ELSE
        string = val_str
        getenv_s = 1
      ENDIF

      RETURN
      END
C **************************************************

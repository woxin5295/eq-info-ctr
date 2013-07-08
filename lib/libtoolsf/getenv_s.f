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

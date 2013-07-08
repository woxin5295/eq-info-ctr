      SUBROUTINE report (string, str_value)

C     IMPLICIT UNDEFINED(a-z)
      IMPLICIT NONE

      CHARACTER string*(*), str_value*(*), temp_str*(13)
      INTEGER i, log_unit, unit_arg, status, strlen
      INTEGER length, val_len, int_value
      REAL real_value

      DATA log_unit /6/

      SAVE log_unit, prefix, command, oper, comment

C --- C-Shell version.
      CHARACTER prefix*1, command*7, oper*2, comment*2, trail*1
      DATA prefix    /' '/
      DATA command   /'setenv '/
      DATA oper      /' "'/
      DATA comment   /'# '/
      DATA trail     /'"'/

C --- Bourne Shell version.
C     CHARACTER prefix*1, command*1, oper*2, comment*2, trail*1
C     DATA prefix    /' '/
C     DATA command   /' '/
C     DATA oper      /'="'/
C     DATA comment   /'# '/
C     DATA trail     /'"'/

C --- DCL version.
C     CHARACTER prefix*2, command*1, oper*5, comment*3, trail*1
C     DATA prefix    /'$ '/
C     DATA command   /' '/
C     DATA oper      /':== "'/
C     DATA comment   /'$! '/
C     DATA trail     /'"'/

C --- VMS version.
C     CHARACTER prefix*2, command*7, oper*1, comment*3, trail*1
C     DATA prefix    /'$ '/
C     DATA command   /'define '/
C     DATA oper      /'"'/
C     DATA comment   /'$! '/
C     DATA trail     /'"'/

C *****************************************************************
      ENTRY report_s (string, str_value)

        length  = strlen(string)
        val_len = strlen(str_value)
        IF (length.GT.0 .AND. val_len.GT.0)
     &    WRITE (log_unit, '(6A)')
     &      prefix, command, string(1:length),
     &      oper, str_value(1:val_len), trail

        RETURN

C *****************************************************************
      ENTRY report_r (string, real_value)

        length  = strlen(string)
        IF (length.GT.0) THEN
          WRITE (temp_str, '(F13.4)') real_value
          DO 101 i=1, 13
101         IF (temp_str(i:i).NE.' ') GOTO 201
201       WRITE (log_unit, '(6A)')
     &      prefix, command, string(1:length),
     &      oper, temp_str(i:13), trail
        ENDIF

        RETURN

C *****************************************************************
      ENTRY report_i (string, int_value)

        length  = strlen(string)
        IF (length.GT.0) THEN
          WRITE (temp_str, '(I13)') int_value
          DO 102 i=1, 13
102         IF (temp_str(i:i).NE.' ') GOTO 202
202       WRITE (log_unit, '(6A)')
     &      prefix, command, string(1:length),
     &      oper, temp_str(i:13), trail
        ENDIF

        RETURN

C *****************************************************************
      ENTRY report_comment (string)

        length  = strlen(string)
        IF (length.GT.0)
     &    WRITE (log_unit, '(2A)') comment, string(1:length)

        RETURN

C *****************************************************************
      ENTRY report_end (string)

        length = strlen(string)
        IF (length.GT.0)
     &    WRITE (log_unit, '(/2A/)') prefix, string(1:length)

        WRITE (log_unit, '(A,"END_LOG"/)') comment

        CLOSE (UNIT=log_unit)

        RETURN

C *****************************************************************
      ENTRY report_init (unit_arg, string)

        log_unit = unit_arg

        IF (log_unit.NE.6) THEN

          length = strlen(string)
          IF (length.GT.0) THEN
            OPEN (UNIT=log_unit, FILE=string(1:length),
     &            IOSTAT=status)
C    &            FORM='NOPRINT', IOSTAT=status)           ! Masscomp
C           OPEN (UNIT=log_unit, FILE=string(1:length),    !VAX
C    &            CARRIAGECONTROL='LIST', IOSTAT=status)   !VAX
            IF (status.NE.0) THEN
              OPEN (UNIT=log_unit)
C             OPEN (UNIT=log_unit, FORM='NOPRINT')         !Masscomp
C             OPEN (UNIT=log_unit, CARRIAGECONTROL='LIST') !VAX
            ENDIF
          ELSE
            OPEN (UNIT=log_unit)
C           OPEN (UNIT=log_unit, FORM='NOPRINT')         ! Masscomp
C           OPEN (UNIT=log_unit, CARRIAGECONTROL='LIST') !VAX
          ENDIF

          REWIND (UNIT=log_unit)

        ENDIF

        WRITE (log_unit, '(/A,"LOG_FILE/PROCEDURE")')
     &    comment

        RETURN

C *****************************************************************
      ENTRY report_getunit (unit_arg)

        unit_arg = log_unit

        RETURN

      END
C *****************************************************************

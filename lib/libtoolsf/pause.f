      SUBROUTINE pause

      CHARACTER*1 response

      WRITE (06, '(''Press RETURN to continue: '', $)')
      READ (05, '(A)') response

      IF (response.EQ.'q' .OR. response.EQ.'Q') CALL EXIT(1)

      RETURN
      END

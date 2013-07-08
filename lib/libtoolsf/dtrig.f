      REAL FUNCTION SIND (degree)
      REAL to_radians, degree
      PARAMETER (to_radians = 3.141592654 / 180.)
      SIND = SIN(degree*to_radians)
      RETURN
      END

      REAL FUNCTION COSD (degree)
      REAL to_radians, degree
      PARAMETER (to_radians = 3.141592654 / 180.)
      COSD = COS(degree*to_radians)
      RETURN
      END

      REAL FUNCTION TAND (degree)
      REAL to_radians, degree
      PARAMETER (to_radians = 3.141592654 / 180.)
      TAND = TAN(degree*to_radians)
      RETURN
      END

      REAL FUNCTION ASIND (degree)
      REAL to_radians, degree
      PARAMETER (to_radians = 3.141592654 / 180.)
      ASIND = ASIN(degree*to_radians)
      RETURN
      END

      REAL FUNCTION ACOSD (degree)
      REAL to_radians, degree
      PARAMETER (to_radians = 3.141592654 / 180.)
      ACOSD = ACOS(degree*to_radians)
      RETURN
      END

      REAL FUNCTION ATAND (degree)
      REAL to_radians, degree
      PARAMETER (to_radians = 3.141592654 / 180.)
      ATAND = ATAN(degree*to_radians)
      RETURN
      END

      REAL FUNCTION ATAN2D (top_deg, bottom_deg)
      REAL to_radians, top_deg, bottom_deg
      PARAMETER (to_radians = 3.141592654 / 180.)
      ATAN2D = ATAN2((top_deg*to_radians), (bottom_deg*to_radians))
      RETURN
      END
      

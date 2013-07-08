      subroutine sort_on_first_vector(num_sta_used,sta_corr,sta_names,
     &  stderr_sta_corr,num_obs_per_sta)
      integer       num_sta_used
      real*8        sta_corr(500),temp_sta_corr
      character*4   sta_names(500),temp_name
      real*8        stderr_sta_corr(500),temp_stderr
      integer*4     num_obs_per_sta(500),temp_num
      integer       i,j,jj,m

      do 100 j=1,num_sta_used-1
          m=j
          jj=j+1
          do 200 i=jj,num_sta_used
              if (sta_corr(m).lt.sta_corr(i)) goto 200
              m=i
 200      continue
          temp_sta_corr=sta_corr(m)
          sta_corr(m)=sta_corr(j)
          sta_corr(j)=temp_sta_corr

          temp_name=sta_names(m)
          sta_names(m)=sta_names(j)
          sta_names(j)=temp_name

          temp_stderr=stderr_sta_corr(m)
          stderr_sta_corr(m)=stderr_sta_corr(j)
          stderr_sta_corr(j)=temp_stderr

          temp_num=num_obs_per_sta(m)
          num_obs_per_sta(m)=num_obs_per_sta(j)
          num_obs_per_sta(j)=temp_num
 100  continue
      return
      end


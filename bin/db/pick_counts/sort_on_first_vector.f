      subroutine sort_on_first_vector(num_sta_used,num_obs_per_sta,
     &sta_names)
      integer       num_sta_used
      character*4   sta_names(500),temp_name
      integer*4     num_obs_per_sta(500),temp_num
      integer       i,j,jj,m

      do 100 j=1,num_sta_used-1
          m=j
          jj=j+1
          do 200 i=jj,num_sta_used
              if (num_obs_per_sta(m).lt.num_obs_per_sta(i)) goto 200
              m=i
 200      continue

          temp_name=sta_names(m)
          sta_names(m)=sta_names(j)
          sta_names(j)=temp_name

          temp_num=num_obs_per_sta(m)
          num_obs_per_sta(m)=num_obs_per_sta(j)
          num_obs_per_sta(j)=temp_num
 100  continue
      return
      end


/* cc -L/usr/local/mysql/lib/mysql -R/usr/local/mysql/lib/mysql -I/usr/local/mysql/include/mysql -o select select.c -lmysqlclient -lnsl -lsocket */

#include <sys/time.h>
#include <stdio.h>
#include <mysql.h>

int main(char **args) {
    MYSQL_RES *result;
    MYSQL_ROW row;
    MYSQL *connection, mysql;
    int state;
    
     /* connect to the mySQL database at athens.imaginary.com */
    mysql_init(&mysql);
    connection = mysql_real_connect(&mysql,
                                    "localhost",
                                    "pipe", "user_pipe_password",
                                    "pipeline",0, 0, 0);
    /* check for a connection error */
    if( connection == NULL ) {
        /* print the error message */
        printf(mysql_error(&mysql));
        return 1;
    }
    state = mysql_query(connection, 
                       "SELECT station_id, trigger_low * 1, trigger_medium * 1, trigger_high * 1, system_status * 1, test * 1,  maintenance * 1 FROM occ_display"); 
    if( state != 0 ) {
        printf(mysql_error(connection));
        return 1;
    }
    /* must call mysql_store_result() before we can issue any
     * other query calls
     */  
    result = mysql_store_result(connection);
    printf("Rows: %d\n", mysql_num_rows(result));
    /* process each row in the result set */
    printf("station_id trigger_low trigger_medium trigger_high system_status test maintenance\n");
    while( ( row = mysql_fetch_row(result)) != NULL ) {
        printf("    %2s         %s              %s            %s           %s           %s        %s\n",
		row[0],row[1],row[2],row[3],row[4],row[5],row[6]); 

    }
    /* free the result set */
    mysql_free_result(result);
    
    /* event table information */
    state = mysql_query(connection, 
                       "SELECT event_detected * 1,  event_watchdog FROM events"); 
    if( state != 0 ) {
        printf(mysql_error(connection));
        return 1;
    }
    /* must call mysql_store_result() before we can issue any
     * other query calls
     */  
    result = mysql_store_result(connection);
    printf("Rows: %d\n", mysql_num_rows(result));
    /* process each row in the result set */
    printf("event_detected event_watchdog\n");
    while( ( row = mysql_fetch_row(result)) != NULL ) {
        printf("    %2s              %s \n", row[0],row[1]); 

    }
    /* free the result set */
    mysql_free_result(result);
    
    /* close the connection */
    mysql_close(connection);
    printf("Done.\n");
}

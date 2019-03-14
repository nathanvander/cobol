GCobol >>SOURCE FORMAT IS FREE
*>*************************************
*> Tektonics: cobc -x -lsqlite3 test.sqlite3.cob sqlite3-connection.cob sqlite3-statement.cob
*>***************************************
identification division. 
    program-id. test-sqlite3.
    AUTHOR. Nathan Vanderhoofven.
    DATE-WRITTEN. 6/9/18.
    REMARKS.  This tests the sqlite3-connection and sqlite3-statement methods.

environment division. configuration section. 
    repository.
    *> sqlite3-connection functions
    FUNCTION SQLITE3-OPEN
    FUNCTION SQLITE3-CLOSE
    FUNCTION SQLITE3-CHANGES
    FUNCTION SQLITE3-EXEC
    FUNCTION SQLITE3-LAST-INSERT-ROWID
    FUNCTION SQLITE3-LIBVERSION
    FUNCTION SQLITE3-ERRCODE
    FUNCTION SQLITE3-ERRMSG
    *> sqlit3-statement functions
    FUNCTION SQLITE3-STMT-OPEN
    FUNCTION SQLITE3-STMT-CLOSE
    FUNCTION SQLITE3-STMT-STEP
    FUNCTION SQLITE3-STMT-BUSY
    FUNCTION SQLITE3-STMT-RESET
    FUNCTION SQLITE3-COLUMN-INT
    FUNCTION SQLITE3-COLUMN-INT64
    FUNCTION SQLITE3-COLUMN-DOUBLE
    FUNCTION SQLITE3-COLUMN-TEXT
    FUNCTION SQLITE3-COLUMN-TYPE
    FUNCTION SQLITE3-COLUMN-NAME
    FUNCTION SQLITE3-COLUMN-COUNT
    FUNCTION SQLITE3-COLUMN-BLOB
    FUNCTION SQLITE3-COLUMN-BYTES    
    function all intrinsic.

data division.
working-storage section.
    01 file-name pic x(32).
    01 db-status pic 9(9) comp.
        88 SQLITE_OK VALUE 0.
    01 sqlite3-version-text pic x(10). 
    01 bool 		pic 9.
    01 db-handle        PIC 9(18) COMP.    
    01 sql-text pic x(128).
    01 first-name pic x(20).
    01 last-name pic x(20).
    01 age pic 99.
    01 row-id                          PIC 9(18) COMP.
    01 error-message pic x(128).
    *> stmt specific variables
    01 stmt-handle        PIC 9(18) COMP.        
    01 step-result pic s9(9) comp.  
        88  SQLITE_BUSY value is 5.
        88  SQLITE_ROW value is 100.
        88  SQLITE_DONE value is 101.
    01 num-cols pic 99.
    01 string1 pic x(20).
    01 int1 pic 9(9).
    01 long1 pic 9(18).

procedure division.
main section.
    
    *>//get the file name
    accept file-name from command-line.
    if file-name = SPACE or file-name = LOW-VALUE then 
        move "test.sdb" to file-name
    end-if.
    display "file: ", file-name.

    *>-----------------------------------------------------   
    *>//print out the library version
    *>// the version is returned through the out parameter
    move SQLITE3-LIBVERSION(sqlite3-version-text) to bool.
    display "SQLite3 version: ", sqlite3-version-text.

    *>-----------------------------------------------------   
    *> open the connection
    move SQLITE3-OPEN(file-name,db-handle) to db-status.
    perform check-error.
    display "connection open"

    *>-----------------------------------------------------       
    *> create a table
    move "create table if not exists person (id INTEGER PRIMARY KEY AUTOINCREMENT, first_name text, last_name text, age INT )" to sql-text.
    display sql-text.
    move SQLITE3-EXEC(db-handle,sql-text) to db-status.
    perform check-error.
    display "table created".

    *>-----------------------------------------------------       
    *> do an insert
    move " " to sql-text.  *> clear the variable
    move "Bob" to first-name.
    move "Jones" to last-name.
    move 21 to age.
    string
       "insert into person (first_name, last_name, age) values ('"
       trim(first-name)
       "','"
       trim(last-name)
       "',"
       age
       ")"
       into sql-text
    end-string.
    display sql-text.
    move SQLITE3-EXEC(db-handle,sql-text) to db-status.
    perform check-error.
    move SQLITE3-LAST-INSERT-ROWID(db-handle) to row-id.
    display "rowid ", row-id.

    *>-----------------------------------------------------       
    *> now do a select
    move " " to sql-text.
    string
       "select * from person where first_name = 'Bob'"
       into sql-text
    end-string.
    display sql-text.
    move SQLITE3-STMT-OPEN(db-handle,sql-text,stmt-handle) to db-status.
    move SQLITE3-STMT-STEP(stmt-handle) to step-result.
    *> get number of columns
    move SQLITE3-COLUMN-COUNT(stmt-handle) to num-cols.
    display "number of columns ", num-cols.
    
    *> get rowid
    move SQLITE3-COLUMN-INT64(stmt-handle,0) to row-id.
    display "id ",row-id.
    
    *> get name
    move SQLITE3-COLUMN-TEXT(stmt-handle,1,string1) to bool.
    display "first-name: ",string1.
    
    *> get age
    move SQLITE3-COLUMN-INT(stmt-handle,3) to int1.
    display "age ",int1.    
    
    *>-----------------------------------------------------       
    *> close the statement
    move SQLITE3-STMT-CLOSE(stmt-handle) to db-status.
    
    *> close the connection
    move SQLITE3-CLOSE(db-handle) to db-status.
    
    stop run.
    
check-error.
    if db-status IS NOT ZERO then
        perform display-error
    end-if.

display-error.
    display "error ", db-status.
    move SQLITE3-ERRMSG(db-handle,error-message) to bool.
    display error-message.
    stop run.
        
end program test-sqlite3.


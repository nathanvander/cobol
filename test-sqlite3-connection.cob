*> test-sqlite3-connection
*> pass in an sql statement and this will execute it
*>***************************************
identification division. program-id. test-sqlite3-connection.
environment division. configuration section. 
    repository. 
    FUNCTION SQLITE3-LIBVERSION
    function all intrinsic.
data division.
working-storage section.
  01 file-name pic x(32).
  01 db-status pic s9(9) comp.
  01 sqlite3-version-text pic x(10). 
  01 bool 		pic 9.
procedure division.
   *>//get the file name
   accept file-name from command-line.
   if file-name = SPACE or file-name = LOW-VALUE then 
       display "usage: test-sqlite3-connection <file-name>";
       stop run
   end-if.
   
    *>//print out the library version
    *>// the version is returned through the out parameter
    move SQLITE3-LIBVERSION(sqlite3-version-text) to bool.
    display "SQLite3 version: ", sqlite3-version-text.

    *> open the connection
    stop run.
end program test-sqlite3-connection.


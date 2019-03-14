*>***************************************************
*> Function sqlite3-version.
*>
*> This function calls a c function with the following signature:
*>     const char *sqlite3_libversion(void);
*>
*> In Cobol-land, all functions return either numbers or pointers.
*> So we call the C function, get the pointer to the char array and convert it to text.
*>
*> Next problem - we want this available to other Cobol code, but again we can't return the value.
*> So this is done through an OUT parameter.
*> And we have to return something, so we return the dummy variable bool.
*>*****************************************************
identification division. function-id. sqlite3-version.
environment division. configuration section. 
    repository. 
    FUNCTION SQLITE3-LIBVERSION
    function all intrinsic.
data division.

working-storage section.
   01  sqlite3-temporary-pointer      usage pointer.
   01  sqlite3-data                   pic x(128) based. 
linkage section.
   01 bool pic 9.
   01 sqlite3-version-text pic x(10).  *>//version text will be something like "3.23.1"
   
procedure division using sqlite3-version-text returning bool.
    call static "sqlite3_libversion" returning sqlite3-temporary-pointer end-call.
    set address of sqlite3-data to sqlite3-temporary-pointer.
    string
        sqlite3-data delimited by low-value
        into sqlite3-version-text
    end-string
    set address of sqlite3-data to NULL.
    move 1 to bool.
    goback.
end function sqlite3-version.
*>*************************************************************
*> This is the test code for the function that demonstrates how to use it.
*>*************************************************************
identification division. program-id. test-sqlite3-version2.
environment division. configuration section. 
    repository. 
    FUNCTION sqlite3-version
    function all intrinsic.
data division.
working-storage section.
    01 sqlite3-version-text pic x(10). 
    01 bool pic 9.
procedure division.
    move sqlite3-version(sqlite3-version-text) to bool.
    display "SQLite3 version: ", sqlite3-version-text.
    stop run.
end program test-sqlite3-version2.
*>**********************************************************

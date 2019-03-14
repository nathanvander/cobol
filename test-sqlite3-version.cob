identification division. program-id. test-sqlite3-version.
environment division. configuration section. 
    repository. 
    FUNCTION SQLITE3-LIBVERSION
    function all intrinsic.
data division.

working-storage section.
   01  sqlite3-library-version        pic x(010).
   01  sqlite3-temporary-pointer      usage pointer.
   01  sqlite3-data                   pic x(128) based.  
procedure division.
    call static "sqlite3_libversion" returning sqlite3-temporary-pointer end-call
    set address of sqlite3-data to sqlite3-temporary-pointer
    string
      sqlite3-data delimited by low-value
      into sqlite3-library-version
    end-string
    set address of sqlite3-data to NULL
    display "SQLite3 Library Version ", trim(sqlite3-library-version) end-display
    stop run.    
end program test-sqlite3-version.

GCobol >>SOURCE FORMAT IS FREE
*>***************************************************************************
*> The author disclaims copyright to this source code.  In place of
*> a legal notice, here is a blessing:
*>
*>    May you do good and not evil.
*>    May you find forgiveness for yourself and forgive others.
*>    May you share freely, never taking more than you give.
*> 
*>*************************************************************************
*> This is an SQLITE3 connection written in Cobol.  It wraps around the C code as 
*> closely as possible. 
*>
*> For another Cobol SQLite3 driver see
*> https://sourceforge.net/p/open-cobol/contrib/492/tree/trunk/tools/CobolSQLite3/ 
*>
*> Constructor:
*>    <db-status> SQLITE3-OPEN (<filename>, OUT <db-handle>).
*>       This open an SQLite database file as specified by the filename argument.
*> 
*> Destructor:
*>    <db-status> SQLITE3-CLOSE(<db-handle>).
*>
*> Methods:
*>    <int num-changes> SQLITE3-CHANGES(<db-handle>)
*>	  This function returns the number of rows modified, inserted
*>        or deleted by the most recently completed INSERT, UPDATE or 
*>        DELETE statement on the database connection specified.
*>
*>    <db-status> SQLITE3-EXEC(<db-handle>,<sql-string>)
*>        The sqlite3_exec() interface is a convenience wrapper around 
*>        sqlite3_prepare_v2(), sqlite3_step(), and sqlite3_finalize(), 
*>        that allows an application to run multiple statements of SQL 
*>        without having to use a lot of C code.
*>
*>    <long rowid> SQLITE3-LAST-INSERT-ROWID(<db-handle>)
*>        sqlite3_int64 sqlite3_last_insert_rowid(sqlite3*);
*>        The sqlite3_last_insert_rowid(D) interface usually returns the
*>        rowid of the most recent successful INSERT into a rowid table
*>
*>    <bool> SQLITE3-LIBVERSION (OUT <version-text>)
*>        Returns the sqlite version through the OUT parameter.
*>
*>    <bool> SQLITE3-ERRMSG (<db-handle>, OUT <error-message)
*>        Returns English-language text as an OUT parameter that describes the last error on the connection.
*>
*>    <int> SQLITE3-ERRCODE(<db-handle>);
*>        If the most recent sqlite3 API call associated with database connection D failed, 
*>        then the sqlite3_errcode(D) interface returns the numeric result code for that API call
*>
*> Techtonics: cobc -m -lsqlite3 sqlite3-connection.cob
*>
*>*********************************************************************************
*> usage: <bool> SQLITE3-LIBVERSION (OUT <version-text>)
*>*********************************************************************************
*> NOTE: There is another function, sqlite3_libversion_number, which is supposed to return
*> the version number as an int, however, I could not get it to work.
*>
identification division. function-id. SQLITE3-LIBVERSION.
environment division. configuration section. 
    repository. 
    function sqlite3_libversion
    function all intrinsic.
data division.
working-storage section.
    01  text-pointer      usage pointer.
    01  sqlite3-data      pic x(128) based.
linkage section.
    01 bool 			pic 9.
    01 sqlite3-version-text pic x(10). 

procedure division using sqlite3-version-text returning bool.
    CALL static "sqlite3_libversion" returning text-pointer.
    set address of sqlite3-data to text-pointer.
    string
      sqlite3-data delimited by low-value
      into sqlite3-version-text
    end-string      
    set address of sqlite3-data to NULL.
    goback.    
end function SQLITE3-LIBVERSION.
*>*****************************************************************************
*> USAGE: <int db-status> = SQLITE3-OPEN(<string file-name>, OUT <dh-handle>).
*>*****************************************************************************
identification division. function-id. SQLITE3-OPEN.
environment division. configuration section. 
    repository. 
    function sqlite3_open
    function all intrinsic.
data division.
linkage section.
    01  db-status pic s9(9) comp.
    01  file-name                      pic x(32).
    01  db-handle                      PIC 9(18) COMP.
    01  redefines db-handle.
      05  db-object-ptr                usage pointer.

*> Note:  dh-handle is just a long.  Pass in an empty long and it will be filled with the
*> object handle.  Cobol will automatically convert the pointer to a long handle.
*>
procedure division using file-name, db-handle returning db-status.
    call static "sqlite3_open" using concatenate(trim(file-name), x"00"),
                                     by reference db-object-ptr
                           returning db-status
    end-call 
    goback.
end function SQLITE3-OPEN.
*>*****************************************************************************
*> USAGE: <int num-changes> = SQLITE3-CHANGES(<db-handle>)
*>*****************************************************************************
identification division. function-id. SQLITE3-CHANGES.
environment division. configuration section. 
    repository. 
    function sqlite3_changes
    function all intrinsic.
data division.
working-storage section.
    linkage section.
    01  db-handle                      PIC 9(18) COMP.
    01  redefines db-handle.
        05  db-object-ptr                usage pointer.
    01 num-changes                     pic s9(9) comp.
procedure division using db-handle returning num-changes.
    call static "sqlite3_changes" using by value db-object-ptr 
        returning num-changes
    end-call.
    goback returning num-changes.
end function SQLITE3-CHANGES.
*>*******************************************************************************
*> USAGE: <db-status> =  SQLITE3-CLOSE(<db-handle>)
*>*****************************************************************************
identification division. function-id. SQLITE3-CLOSE.
environment division. configuration section. 
    repository. 
    function sqlite3_close
    function all intrinsic.
data division.
  linkage section.
    01 db-status pic s9(9) comp.  
    01 db-handle                      PIC 9(18) COMP.
    01 redefines db-handle.
        05  db-object-ptr                usage pointer.
      
procedure division using db-handle returning db-status.
    call static "sqlite3_close" using by value db-object-ptr 
        returning db-status
    end-call.
    goback returning db-status.
end function SQLITE3-CLOSE.
*>*******************************************************************************
*> USAGE: <db-status> SQLITE3-EXEC (<db-handle>, <sql-string>)
*>    sql-string is limited to 1 million in length per sqlite, here we say "any length" 
*>*****************************************************************************
identification division. function-id. SQLITE3-EXEC.
environment division. configuration section. 
    repository. 
    function sqlite3_exec
    function all intrinsic.
data division.  
  linkage section.
    01 db-status pic s9(9) comp.    
    01 db-handle                      PIC 9(18) COMP.
    01 redefines db-handle.
        05  db-object-ptr                usage pointer.
    01  sql-string                     pic x any length.

procedure division using db-handle, sql-string returning db-status.
    call static "sqlite3_exec" using by value db-object-ptr,
                                           by content concatenate(trim(sql-string), x"00"),
                                           NULL,
                                           NULL,
                                           NULL
                                 returning db-status
    end-call.
    goback returning db-status.
end function SQLITE3-EXEC.
*>*******************************************************************************
*> USAGE: <long row-id> SQLITE3-LAST-INSERT-ROWID(<db-handle>)
*>    row-id can be a *really* big number 
*>*****************************************************************************
identification division. function-id. SQLITE3-LAST-INSERT-ROWID.
environment division. configuration section. 
    repository. 
    function sqlite3_last_insert_rowid
    function all intrinsic.
data division.
    linkage section.
    01  db-handle                      PIC 9(18) COMP.
    01  redefines db-handle.
      05  db-object-ptr                usage pointer.
    01 row-id                          PIC 9(18) COMP.
    
procedure division using db-handle returning row-id.
    call static "sqlite3_last_insert_rowid" using by value db-object-ptr 
        returning row-id
    end-call.
    goback returning row-id.
end function SQLITE3-LAST-INSERT-ROWID.
*>*****************************************************************************
*> USAGE: <bool> SQLITE3-ERRMSG (<db-handle>, OUT <error-message)
*>     const char *sqlite3_errmsg(sqlite3*);
*>**********************************************************************************
identification division. function-id. SQLITE3-ERRMSG.
environment division. configuration section. 
    repository. 
    function sqlite3_errmsg
    function all intrinsic.
data division. 
working-storage section.
    01  text-pointer      	usage pointer.
    01  sqlite3-data      	pic x(256) based.
linkage section.
    01 bool 			pic 9.
    01 error-message  		pic x any length.   
    01 db-handle                PIC 9(18) COMP.
    01 redefines db-handle.
      	05  db-object-ptr 		usage pointer.

procedure division using db-handle, error-message returning bool.
    call static "sqlite3_errmsg" using by value db-object-ptr
        returning text-pointer
    end-call.
    set address of sqlite3-data to text-pointer.
    string
        sqlite3-data delimited by low-value
        into error-message
    end-string
    set address of sqlite3-data to NULL.
    move 1 to bool.
    goback returning bool.
end function SQLITE3-ERRMSG.
*>***********************************************************************************
*> USAGE: <int> SQLITE3-ERRCODE(<db-handle>);
*>**********************************************************************************
identification division. function-id. SQLITE3-ERRCODE.
environment division. configuration section. 
    repository. 
    function sqlite3_errcode
    function all intrinsic.
data division.  
  linkage section.
    01 error-code 			PIC S9(9) COMP.
    01 db-handle                        PIC 9(18) COMP.
    01 redefines db-handle.
        05  db-object-ptr               usage pointer.

procedure division using db-handle returning error-code.
    call static "sqlite3_errcode" using by value db-object-ptr 
                                 returning error-code
    end-call.
    goback returning error-code.
end function SQLITE3-ERRCODE.

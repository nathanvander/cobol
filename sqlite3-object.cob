*>***************************************************************************
*> The author disclaims copyright to this source code.  In place of
*> a legal notice, here is a blessing:
*>
*>    May you do good and not evil.
*>    May you find forgiveness for yourself and forgive others.
*>    May you share freely, never taking more than you give.
*>
*> This is a rewrite of CobolSQLite3.cob 
*> at https://sourceforge.net/p/open-cobol/contrib/492/tree/trunk/tools/CobolSQLite3/ 
*>*************************************************************************
*> This is an SQLITE3-OBJECT written in Cobol.  It wraps around the C code.
*>
*> Constructor:
*>    DB-OPEN using db-name returning db-object.
*>       The db-object is used by the methods.
*>
*> Fields:
*>    DB-STATUS int
*>        accessible via GET-STATUS
*>        Holds the internal status code returned by OPEN() or EXEC()
*>        0 means success.  Any other code is an error message.
*>
*> Methods:
*>    CHANGES	
*>        int sqlite3_changes(sqlite3*);
*>	  This function returns the number of rows modified, inserted
*>        or deleted by the most recently completed INSERT, UPDATE or 
*>        DELETE statement on the database connection specified.
*>
*>    DB-CLOSE
*>        int sqlite3_close(sqlite3*);
*>        Calls to sqlite3_close() return SQLITE_OK if the sqlite3 
*>        object is successfully destroyed and all associated resources 
*>        are deallocated.
*>
*>    EXEC
*>        The sqlite3_exec() interface is a convenience wrapper around 
*>        sqlite3_prepare_v2(), sqlite3_step(), and sqlite3_finalize(), 
*>        that allows an application to run multiple statements of SQL 
*>        without having to use a lot of C code.
*>
*>    GET-STATUS
*>        returns the status of the open or exec call.
*>        If the database is opened (and/or created) successfully, then SQLITE_OK is returned. 
*>	  Otherwise an error code is returned. 
*>
*>    LAST-INSERT-ROWID
*>        sqlite3_int64 sqlite3_last_insert_rowid(sqlite3*);
*>        The sqlite3_last_insert_rowid(D) interface usually returns the
*>        rowid of the most recent successful INSERT into a rowid table
*>
*>    VERSION
*>        Returns the sqlite version, as a number.
*>*********************************************************************************
*> usage: <int version-number> = VERSION
*>*********************************************************************************
identification division. function-id. VERSION.
data division.
linkage section.
    01  version-number pic S9(9) COMP.

procedure division returning version-number.
    CALL static "sqlite3_libversion_number" returning version-number.
    goback.
end function VERSION.
*>*****************************************************************************
*> USAGE: <db-handle> = DB-OPEN(<string file-name>)
*>    call GET-STATUS to see if there were any errors
*>*****************************************************************************
identification division. function-id. DB-OPEN.
environment division. configuration section. repository. function all intrinsic.
data division.
  working-storage section.
    01 DB-STATUS pic s9(04) comp external.
  linkage section.
    01  file-name                      pic x(32).
    01  db-handle                      PIC 9(18) COMP.
    01  redefines db-handle.
      05  db-object-ptr                usage pointer.
        
procedure division using file-name returning db-handle.
    call static "sqlite3_open" using concatenate(trim(file-name), x"00"),
                                     by reference db-object-ptr
                           returning DB-STATUS
    end-call 
    goback returning db-handle.
end function DB-OPEN.
*>*****************************************************************************
*> USAGE: <int num-changes> = CHANGES(<db-handle>)
*>*****************************************************************************
identification division. function-id. CHANGES.
data division.
working-storage section.
    linkage section.
    01  db-handle                      PIC 9(18) COMP.
    01  redefines db-handle.
      05  db-object-ptr                usage pointer.
    01 num-changes                     pic 9999.
procedure division using db-handle returning num-changes.
    call static "sqlite3_changes" using by value db-object-ptr 
        returning num-changes
    end-call.
    goback returning num-changes.
end function CHANGES.
*>*******************************************************************************
*> USAGE: <int stat> DB-CLOSE(<db-handle>)
*>*****************************************************************************
identification division. function-id. DB-CLOSE.
data division.
  working-storage section.
    01 DB-STATUS pic s9(04) comp external.
  linkage section.
    01  db-handle                      PIC 9(18) COMP.
    01  redefines db-handle.
      05  db-object-ptr                usage pointer.
    01 stat pic 999. 
procedure division using db-handle returning stat.
    call static "sqlite3_close" using by value db-object-ptr 
        returning stat
    end-call.
    move stat to DB-STATUS.
    goback returning stat.
end function DB-CLOSE.
*>*******************************************************************************
*> USAGE: <int stat> EXEC (<db-handle>, <sql-string>)
*>    sql-string is limited to 1 million in length per sqlite, here we say "any length" 
*>*****************************************************************************
identification division. function-id. EXEC.
environment division. configuration section. repository. function all intrinsic.
data division.
  working-storage section.
    01 DB-STATUS pic s9(04) comp external.
  linkage section.
    01  db-handle                      PIC 9(18) COMP.
    01  redefines db-handle.
      05  db-object-ptr                usage pointer.
    01  sql-string                     pic x any length.
    01 stat pic 999.     
procedure division using db-handle, sql-string returning stat.
    call static "sqlite3_exec" using by value db-object-ptr,
                                           by content concatenate(trim(sql-string), x"00"),
                                           NULL,
                                           NULL,
                                           NULL
                                 returning stat
    end-call.
    move stat to DB-STATUS.
    goback returning stat.
end function EXEC.
*>*******************************************************************************
*> USAGE: <int DB-STATUS> GET-STATUS()
*>*****************************************************************************
identification division. function-id. GET-STATUS.
data division.
working-storage section.
    01 DB-STATUS pic s9(04) comp external.
  linkage section.
    01 my-status pic 999.    
procedure division returning my-status.
    move DB-STATUS to my-status.
    goback returning my-status.
end function GET-STATUS.
*>*******************************************************************************
*> USAGE: <long row-id> LAST-INSERT-ROWID(<db-handle>)
*>    row-id can be a *really* big number 
*>*****************************************************************************
identification division. function-id. LAST-INSERT-ROWID.
data division.
working-storage section.
    linkage section.
    01  db-handle                      PIC 9(18) COMP.
    01  redefines db-handle.
      05  db-object-ptr                usage pointer.
    01 row-id                          PIC S9(18) BINARY. 
procedure division using db-handle returning row-id.
    call static "sqlite3_last_insert_rowid" using by value db-object-ptr 
        returning row-id
    end-call.
    goback returning row-id.
end function LAST-INSERT-ROWID.
*>*****************************************************************************


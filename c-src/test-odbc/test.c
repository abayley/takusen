
#include <stdio.h>
#include <windows.h>
#include <sql.h>
#include <sqlext.h>


SQLRETURN rc;

void printDiagRecs(SQLHANDLE h, SQLSMALLINT htype, SQLSMALLINT msgSeq)
{
  char strState[20];
  char strMsg[1025];
  SQLINTEGER errNum;
  SQLSMALLINT msgLen;
  SQLRETURN rc2;
  rc2 = SQLGetDiagRec(htype, h, msgSeq, strState, &errNum, strMsg, 1024, &msgLen);
  if (SQL_SUCCESS == rc2)
  {
    printf("%d %s %s\n", errNum, strState, strMsg);
    printDiagRecs(h, htype, msgSeq+1);
  }
  /* else { printf("printDiagRecs: rc2 = %d\n", rc2); } */
}

void checkRc()
{
  printf("rc = %d\n", rc);
  if (! SQL_SUCCEEDED(rc))
  {
  	exit(1);
  }
}

void checkError(SQLRETURN rc, SQLHANDLE h, SQLSMALLINT htype)
{
  //printf("checkError: rc = %d\n", rc);
  if (! SQL_SUCCEEDED(rc))
  {
  	printDiagRecs(h, htype, 1);
  	exit(1);
  }
}


SQLHANDLE allocHdl(SQLHANDLE parent, SQLSMALLINT htype)
{
  SQLHANDLE ptr;
  rc = SQLAllocHandle(htype, parent, &ptr);
  checkError(rc, parent, htype);
  return ptr;
}

SQLHENV allocEnv()  { return allocHdl(NULL, SQL_HANDLE_ENV); }
SQLHDBC allocConn(SQLHENV env)  { return allocHdl(env, SQL_HANDLE_DBC); }
SQLHSTMT allocStmt(SQLHDBC conn) { return allocHdl(conn, SQL_HANDLE_STMT); }


void setOdbcVer(SQLHENV env)
{
  rc = SQLSetEnvAttr(env, SQL_ATTR_ODBC_VERSION, (void*) SQL_OV_ODBC3, 0);
  checkError(rc, env, SQL_HANDLE_ENV);
}


void odbc_connect(SQLHDBC conn, char* connstr)
{
  char outStr[1000];
  SQLSMALLINT outSz;
  rc = SQLDriverConnect(conn, NULL, connstr, strlen(connstr), outStr, 1000, &outSz, SQL_DRIVER_NOPROMPT);
  checkError(rc, conn, SQL_HANDLE_DBC);
}


void odbc_disconnect(SQLHDBC conn)
{
  rc = SQLDisconnect(conn);
  checkError(rc, conn, SQL_HANDLE_DBC);
}


void freeHdl(SQLSMALLINT htype, SQLHANDLE h)
{
  rc = SQLFreeHandle(htype, h);
  checkError(rc, h, htype);
}

void freeEnv(SQLHENV env) { freeHdl(SQL_HANDLE_ENV, env); }
void freeConn(SQLHDBC conn) { freeHdl(SQL_HANDLE_DBC, conn); }
void freeStmt(SQLHSTMT stmt) { freeHdl(SQL_HANDLE_STMT, stmt); }


void prepareStmt(SQLHSTMT stmt, char* sqltext)
{
  rc = SQLPrepare(stmt, sqltext, SQL_NTS);
  checkError(rc, stmt, SQL_HANDLE_STMT);
}

void bindParamDatetime(SQLHSTMT stmt, SQLUSMALLINT pos, char* val)
{
  SQLLEN inputSz;
  rc = SQLBindParameter(stmt, pos, SQL_PARAM_INPUT, SQL_C_CHAR, SQL_TYPE_TIMESTAMP, 23, 0, val, strlen(val), &inputSz);
  checkError(rc, stmt, SQL_HANDLE_STMT);
}

void bindParamString(SQLHSTMT stmt, SQLUSMALLINT pos, char* val)
{
  SQLLEN inputSz;
  rc = SQLBindParameter(stmt, pos, SQL_PARAM_INPUT, SQL_C_CHAR, SQL_CHAR, 0, 0, val, strlen(val), &inputSz);
  checkError(rc, stmt, SQL_HANDLE_STMT);
}

void bindParamInt(SQLHSTMT stmt, SQLUSMALLINT pos, int val)
{
  SQLLEN inputSz;
  rc = SQLBindParameter(stmt, pos, SQL_PARAM_INPUT, SQL_C_LONG, SQL_INTEGER, 0, 0, &val, 4, &inputSz);
  checkError(rc, stmt, SQL_HANDLE_STMT);
}

void executeStmt(SQLHSTMT stmt)
{
  rc = SQLExecute(stmt);
  checkError(rc, stmt, SQL_HANDLE_STMT);
}

void bindColString (SQLHSTMT stmt, SQLUSMALLINT pos, char* buffer, SQLLEN size, SQLLEN* outSz)
{
  rc = SQLBindCol(stmt, pos, SQL_CHAR, buffer, size, outSz);
  checkError(rc, stmt, SQL_HANDLE_STMT);
}

void bindColInt (SQLHSTMT stmt, SQLUSMALLINT pos, int* buffer)
{
  SQLLEN outSz;
  rc = SQLBindCol(stmt, pos, SQL_INTEGER, buffer, 4, &outSz);
  checkError(rc, stmt, SQL_HANDLE_STMT);
}

void bindColDatetime (SQLHSTMT stmt, SQLUSMALLINT pos, char* buffer, SQLLEN* outSz)
{
  rc = SQLBindCol(stmt, pos, SQL_TYPE_TIMESTAMP, buffer, 50, outSz);
  checkError(rc, stmt, SQL_HANDLE_STMT);
}


void fetch(SQLHSTMT stmt)
{
  rc = SQLFetch(stmt);
  checkError(rc, stmt, SQL_HANDLE_STMT);
}


SQLHDBC createConn(char *connstr)
{
  SQLHENV env;
  SQLHDBC conn;
  SQLHSTMT stmt;
  printf("allocEnv\n");
  env = allocEnv();
  printf("setOdbcVer\n");
  setOdbcVer(env);
  printf("allocConn\n");
  conn = allocConn(env);
  printf("odbc_connect\n");
  odbc_connect(conn, "DSN=postgres");
  printf("allocStmt\n");
  stmt = allocStmt(conn);
  printf("prepareStmt\n");
  prepareStmt(stmt, "select ?, ?, ?");
  //prepareStmt(stmt, "select 'hello1', 'hello2'");
  
  printf("bind param 1\n");
  //bindParamString(stmt, 1, "hello1");
  bindParamDatetime(stmt, 1, "1753-01-01 00:00:00");
  printf("bind param 2\n");
  bindParamString(stmt, 2, "hello2");
  printf("bind param 3\n");
  bindParamInt(stmt, 3, 4444);

  printf("executeStmt\n");
  executeStmt(stmt);

  char col1Buf[2000];
  SQLLEN col1Sz;
  char col2Buf[2000];
  SQLLEN col2Sz;
  int col3Buf;
  SQLLEN col3Sz;
  bindColDatetime(stmt, 1, col1Buf, &col1Sz);
  //bindColString(stmt, 1, col1Buf, 2000, &col1Sz);
  bindColString(stmt, 2, col2Buf, 2000, &col2Sz);
  bindColInt(stmt, 3, &col3Buf);

  fetch(stmt);
  printf("col1: %d\n", col1Buf, col1Sz);
  printf("col2: %s %d\n", col2Buf, col2Sz);
  printf("col3: %d %d\n", col3Buf, col3Sz);

  printf("freeStmt\n");
  freeStmt(stmt);
  printf("disconnect\n");
  odbc_disconnect(conn);
  printf("freeConn\n");
  freeConn(conn);
  printf("freeEnv\n");
  freeEnv(env);
}

int main(int argc, char** argv)
{
  createConn("DSN=postgres");
}

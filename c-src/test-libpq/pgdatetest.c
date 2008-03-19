#include <stdio.h>
#include <stdlib.h>
/* for ntohl/htonl
#include <winsock.h>
#include <sys/types.h>
*/
#include "libpq-fe.h"


static void exit_nicely(PGconn *conn)
{
    PQfinish(conn);
    exit(1);
}


void check_sql(PGconn *conn, PGresult *res, ExecStatusType expected)
{
    if (PQresultStatus(res) != expected)
    {
        fprintf(stderr, "SQL failed: %s", PQerrorMessage(conn));
        PQclear(res);
        exit_nicely(conn);
    }
}

void check_cmd(PGconn *conn, PGresult *res)
{
    check_sql(conn, res, PGRES_COMMAND_OK);
}

void check_qry(PGconn *conn, PGresult *res)
{
    check_sql(conn, res, PGRES_TUPLES_OK);
}

void revbytes2(int n, char *pfrom, char *pto)
{
    if (n == 0) return;
    *pto = *pfrom;
    revbytes2(--n, ++pfrom, --pto);
}

void revbytes(int n, void *pfrom, void *pto)
{
    revbytes2(n, (char*)pfrom, ((char*)pto)+n-1);
}


void printColOne(PGresult *res)
{
    double t, *tptr;
    tptr = (double *) PQgetvalue(res, 0, 0);
    revbytes(8, tptr, &t);
    /* t = ntohl(*tptr);  -- this doesn't work!? must be me... */
    printf("%f\n", t);
}


int main(int argc, char **argv)
{
    const char *conninfo;
    PGconn     *conn;
    PGresult   *res;
    double t, *tptr;

    /*
     * If the user supplies a parameter on the command line, use it as the
     * conninfo string; otherwise default to setting dbname=postgres and using
     * environment variables or defaults for all other connection parameters.
     */
    if (argc > 1)
        conninfo = argv[1];
    else
        conninfo = "user = postgres";

    /* Make a connection to the database */
    conn = PQconnectdb(conninfo);

    /* Check to see that the backend connection was successfully made */
    if (PQstatus(conn) != CONNECTION_OK)
    {
        fprintf(stderr, "Connection to database failed: %s", PQerrorMessage(conn));
        exit_nicely(conn);
    }

    res = PQexecParams(conn, "select timestamp with time zone '1916-10-01 02:25:20'"
        , 0, NULL, NULL, NULL, NULL, 1 );
    check_qry(conn, res);
    printColOne(res);
    PQclear(res);

    res = PQexecParams(conn, "select timestamp without time zone '1916-10-01 02:25:20'"
        , 0, NULL, NULL, NULL, NULL, 1 );
    check_qry(conn, res);
    printColOne(res);
    PQclear(res);

    res = PQexecParams(conn, "select timestamp with time zone '1916-10-01 02:25:21'"
        , 0, NULL, NULL, NULL, NULL, 1 );
    check_qry(conn, res);
    printColOne(res);
    PQclear(res);

    res = PQexecParams(conn, "select timestamp without time zone '1916-10-01 02:25:21'"
        , 0, NULL, NULL, NULL, NULL, 1 );
    check_qry(conn, res);
    printColOne(res);
    PQclear(res);

    /* close the connection to the database and cleanup */
    PQfinish(conn);

    return 0;
}

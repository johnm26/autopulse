#sys.argv[1] should be a .txt file (full name)
#sys.argv[2] should be a kid
import qatsSQL
import sys

sntrim_txt = sys.argv[1]
kid = sys.argv[2]
resultssql,runssql = qatsSQL.TXTtoSQL(sntrim_txt,kid)
qatsSQL.insertDB(resultssql,'qatsResults')
qatsSQL.insertDB(runssql,'qatsRuns')
print 'inserted data into database??'

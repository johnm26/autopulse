#sys.argv[1] should be a .txt file (full name)
#sys.argv[2] should be a kid
import qatsSQL
import sys

sntrim_txt = sys.argv[1]
#kid = sys.argv[2]
kid = int(sys.argv[2])+int(1E8) #TEMP, REMOVE THIS CHANGE, JUST FOR TESTING
resultssql,runssql = qatsSQL.TXTtoSQL(sntrim_txt,kid)
qatsSQL.insertDB(resultssql,'qatsResults')
qatsSQL.insertDB(runssql,'qatsRuns')
print 'inserted data into database??'

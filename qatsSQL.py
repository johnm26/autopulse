import pyfits
import tarfile
import MySQLdb
import os
import numpy as num
import csv
import pdb #for testing

dbHost = 'tddb.astro.washington.edu'
dbName = 'Kepler'
dbUser = 'bvegaff'
dbPass = 'tddbKepler'
            
def openDB():
    db = MySQLdb.connect(host=dbHost, user=dbUser,passwd =dbPass,db=dbName)
    cursor = db.cursor()
    return cursor

def dropTable(TableName):
    """ remove a table, to be used as a reset """

    cursor = openDB()
    execstatement = """DROP TABLE IF EXISTS %s""" % TableName
    cursor.execute(execstatement)
    print '%s Table dropped' % TableName

##EDIT THIS IF I'M CHANGING KEYS AROUND##
def createTable(TableName):
    """ create a table, this is where you change your keys """

    cursor = openDB()

    create_statement = 'CREATE TABLE %s (' % TableName
    if TableName.lower() == 'qatsresults':
        create_statement.append(\
            'qatsID bigint not null primary key,\
            qatsVals blob);')
    elif TableName.lower() == 'qatsruns':
        create_statement.append(\
            'runID bigint not null primary key,\
            KID bigint,\
            depth float,\
            width float,\
            qatsValues bigint,\
            qatsNoise bigint);')
    else:
        print 'dont recognize table name'

    print create_statement
    cursor.execute(create_statement)

def insertDB(filename,TableName):
    """ make entries from file """
    
    cursor = openDB()
    fileload = 'LOAD DATA LOCAL INFILE %s INTO TABLE %s' % ("'"+filename+"'",TableName)
    fileload += ' FIELDS TERMINATED BY %s LINES TERMINATED BY %s ;' % ("' '","'\\n'")
    print fileload
    cursor.execute( fileload )

def TXTtoSQL(resultstxt,kid):
    """convert .txt files to .sql files that can simply be inserted with insertDB()"""
    """automatically adds a depthdur.sql file, too, (so put depthdur into runstxt location)"""
    
    #need this to keep our primary key incrementing properly?
    cursor = openDB()
    cursor.execute('select max(qatsID) from qatsResults;')
    try:
        maxID = int(cursor.fetchone()[0])
    except:
        print 'first id!'
        maxID = 0 
    
    lines = open(resultstxt).readlines()
    resultssql = resultstxt[0:len(resultstxt)-3] + 'sql'
    resultsbuff = open(resultssql,'w')
    for i in range(len(lines)):
        resultsbuff.write("%d %s\n" % (i+maxID+1,",".join(lines[i].split())))
    resultsbuff.close
    
    #now the depth dur sql file
    runstxt = '/astro/store/student-scratch1/bvegaff/QATSruns/DB_input_chiSq/depthdur'
    runssql = resultstxt[0:len(resultstxt)-3]+'depdur.sql'
    runsbuff = open(runssql,'w')
    dur,dep = open(runstxt).readlines()
    dur = dur.split()
    dep = dep.split()
    i=1
    for de in dep:
        for du in dur:
            runsbuff.write("%d %d %s %s %d 0\n" % (i+maxID,int(kid),de,du,i+maxID))
            i+=1
    runsbuff.close()
    return resultssql,runssql


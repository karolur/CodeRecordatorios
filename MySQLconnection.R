library(RMySQL)
databasename <- dbConnect(MySQL(),user="genome",host="genome-mysql.cse.ucsc.edu")
result<-dbGetQuery(databasename,"show databases;")

dbDisconnect(databasename)

allTables <- dbListTables(databasename)
dbListFields(databasename,"tablename")
dbGetQuery(databasename,"select count(*) from tablename")
data <-dbReadTable(databasename,"tablename")
to only send a subset of the table to R (in case data is too big)
query<-dbSendQuery(databasename,"select * from tablename where...")
subsetdata<- fetch(query)
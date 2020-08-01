library('RPostgreSQL')

pg = dbDriver("PostgreSQL")

con = dbConnect(pg, user="postgres", password="admin",
                host="localhost", port=5432, dbname="hotspot_australia")

AUSTRALIA_OCTOBER <- read.csv("E:/ALGORITMA/Checklist/1.1/Dashboard/data_input/data_october_new.csv")
AUSTRALIA_NOVEMBER <- read.csv("E:/ALGORITMA/Checklist/1.1/Dashboard/data_input/data_november_new.csv")
AUSTRALIA_DECEMBER <- read.csv("E:/ALGORITMA/Checklist/1.1/Dashboard/data_input/data_december_new.csv")
JOIN_TABLE <- read.csv("E:/ALGORITMA/Checklist/1.1/Dashboard/data_input/jointable.csv")



dbWriteTable(con,'oct_aus',AUSTRALIA_OCTOBER, row.names=FALSE)
dbWriteTable(con,'nov_aus',AUSTRALIA_NOVEMBER, row.names=FALSE)
dbWriteTable(con,'dec_aus',AUSTRALIA_DECEMBER, row.names=FALSE)
dbWriteTable(con,'join_table',JOIN_TABLE, row.names=FALSE)


# mystorefile <- file.path("E:/Data Meteorologi","gdas1.oct19.rds")
# 
# saveRDS(mydata, file = mystorefile)

#how to connect to PDW_Bulk

#library
library("RODBC", lib.loc="~/R/win-library/3.4")

#query
chan = odbcConnect("PDW_Bulk")
sqlQuery(chan, "select top 10 * from [PRODW].[ORG_TREE_DRAD]")


SCUser <- Sys.getenv("SC_ID")
SCToken <- Sys.getenv("SC_KEY")
start <- "2016-06-01"
end <- "2016-06-02"

library("RSiteCatalyst")


#####################################################
######### retrieve data from SiteCatalyst ###########
#####################################################

SCAuth(SCUser, SCToken)
#curDate <- today()
suite <- "cdcgov"
#elements <- c("page", "eVar8") 
#metrics <- c("pageviews", "totalevent2", "participationevent2")

#features <- QueueRanked(suite, start, end, c("pageviews"),  c("page","prop46"), search = " | Features |", top = 5000)


features <- QueueRanked(suite, start, end, c("pageviews"),  c("page"), search = " | Features |", top = 5000)

names(features)[2] <- "pageURL"
features <- features[,1:3]

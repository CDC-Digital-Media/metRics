library("RSiteCatalyst")
library(dplyr)
SCAuth(Sys.getenv(SC_ID), Sys.getenv(SC_KEY))
library(ggplot2)
library(xlsx)

# rs <- GetReportSuites()
# elems <- GetElements("cdcgov")
# vidmetrics <- GetMetrics("cdcgovvistorid")
# videlems <- GetElements("cdcgovvistorid")



flutrendedID <-  1047695110 #QueueTrended("cdcgov", metrics =  c("pageviews", "visitors", "visits"), elements = c("page"), search = "Influenza (Flu) | CDC", date.granularity = "hour", date.from = "2017-03-7", date.to="2017-03-20", max.attempts = 120, interval.seconds = 30, top = 30, enqueueOnly = TRUE)
zikatrendedID <-  2136121170 #QueueTrended("cdcgov", metrics =  c("pageviews", "visitors", "visits"), elements = c("page"), search = "Zika Virus | CDC", date.granularity = "hour", date.from = "2017-03-7", date.to="2017-03-20", max.attempts = 120, interval.seconds = 30, top = 30, enqueueOnly = TRUE)
vstrendedID <-  2138883137 #QueueTrended("cdcgov", metrics =  c("pageviews", "visitors", "visits"), elements = c("page"), search = "CDC Vital Signs", date.granularity = "hour", date.from = "2017-03-7", date.to="2017-03-20", max.attempts = 120, interval.seconds = 30, top = 30, enqueueOnly = TRUE)
hometrendedID <-  1718059122 #QueueTrended("cdcgov", metrics =  c("pageviews", "visitors", "visits"), elements = c("page"), search = "Centers for Disease Control and Prevention", date.granularity = "hour", date.from = "2017-03-7", date.to="2017-03-20", max.attempts = 120, interval.seconds = 30, top = 30, enqueueOnly = TRUE)
dctrendedID <-  1574816706 #QueueTrended("cdcgov", metrics =  c("pageviews", "visitors", "visits"), elements = c("page"), search = "Diseases & Conditions | CDC", date.granularity = "hour", date.from = "2017-03-7", date.to="2017-03-20", max.attempts = 120, interval.seconds = 30, top = 30, enqueueOnly = TRUE)
aztrendedID <-  1351710698 #QueueTrended("cdcgov", metrics =  c("pageviews", "visitors", "visits"), elements = c("page"), search = "A-Z Index", date.granularity = "hour", date.from = "2017-03-7", date.to="2017-03-20", max.attempts = 120, interval.seconds = 30, top = 300, enqueueOnly = TRUE)
mediatrendedID <- 1985612015 #QueueTrended("cdcgov", metrics =  c("pageviews", "visitors", "visits"), elements = c("page"), search = "CDC Online Newsroom", date.granularity = "hour", date.from = "2017-03-7", date.to="2017-03-20", max.attempts = 120, interval.seconds = 30, top = 30, enqueueOnly = TRUE)




flutrended <- GetReport(flutrendedID)
zikatrended <- GetReport(zikatrendedID)
vstrended <- GetReport(zikatrendedID)
hometrended <- GetReport(hometrendedID)
dctrended <- GetReport(dctrendedID)
aztrended <- GetReport(aztrendedID)
mediatrended <- GetReport(mediatrendedID)

fulltrended <- rbind(flutrended, zikatrended, vstrended, hometrended, dctrended, aztrended, mediatrended)
fulltrended <- fulltrended[order(fulltrended$name),]


print(c("flu", flutrendedID))
print(c("zika", zikatrendedID))
print(c("vs", vstrendedID))
print(c("home", hometrendedID))
print(c("dc", dctrendedID))
print(c("az", aztrendedID))
print(c("media", mediatrendedID))


wide_trended <- reshape(fulltrended, direction = "wide", timevar = "hour", idvar = c("name", "url"))
write.xlsx(wide_trended, "ThirdPartyTestAnalysis.xlsx")

library(dplyr)
library(lubridate)
library(stringr)
library(urltools)
library("RSiteCatalyst")

SCUser <- Sys.getenv("SC_ID")
SCToken <- Sys.getenv("SC_KEY")
SCAuth(SCUser, SCToken)

### need to compare variances by date as well in the final anaylsis

visvar <- GetUniqueVisitorVariable(c("cdcgov","cdcgovvistorid"))
visidelements <- GetElements("cdcgovvistorid")
govelements <- GetElements("cdcgov")
visidmetrics <- GetMetrics("cdcgovvistorid")
govmetrics <- GetMetrics("cdcgov")


govcookies <- QueueRanked("cdcgov", metrics =  "pageviews", elements = c("page","cookiesenabled"), date.from = "2017-01-14", date.to="2017-02-01", max.attempts = 120, top = 500)
visidcookies <- QueueRanked("cdcgovvistorid", metrics =  "pageviews", elements = c("page","cookiesenabled"), date.from = "2017-01-14", date.to="2017-02-01", max.attempts = 120, top = 500)
write.csv(govcookies, "cdcgovcookies.csv")
write.csv(visidcookies, "visidcookies.csv")

urlList <- QueueRanked("cdcgovvistorid", metrics =  "pageviews", elements = c("page"), date.from = "2017-01-14", date.to="2017-02-01", search = "https://www.cdc.gov", max.attempts = 120, top = 5000)
write.csv(urlList, "urlList.csv")



#datelist <- c('2017-01-14','2017-01-15','2017-01-16','2017-01-17','2017-01-18','2017-01-19','2017-01-20','2017-01-21','2017-01-22','2017-01-23','2017-01-24','2017-01-25','2017-01-26','2017-01-27','2017-01-28','2017-02-01')
metricsCompareDF <- data.frame()
for (i in 1:50) {
  
  print(i)
  curPageViewsTrend <- QueueTrended("cdcgov", metrics =  c("pageviews", "visitors"), elements = c("prop46"), date.from = "2017-01-14", date.to="2017-02-01", search = urlList[i,1], max.attempts = 120)
  
  curPageViewsTrend <- curPageViewsTrend[,c(1,2,4,5)]
  curPageViewsTrend$jcol <- paste(curPageViewsTrend$datetime, curPageViewsTrend$name)
  names(curPageViewsTrend)[3] <- "GovPageViews"
  names(curPageViewsTrend)[4] <- "GovPageVisitors"
  curPageViewsTrend[,1] <- as.character(curPageViewsTrend[,1])
  
  curPageViewsVIDTrend <- QueueTrended("cdcgovvistorid", metrics =  c("pageviews", "visitors"), elements = c("page"), date.from = "2017-01-14", date.to="2017-02-01", search = urlList[i,1], max.attempts = 120)
  curPageViewsVIDTrend <- curPageViewsVIDTrend[,c(1,2,4,5)]
  for(j in 1:nrow(curPageViewsVIDTrend)){
    if (is.na(str_locate(curPageViewsVIDTrend[j,2], ".htm")[1])) {
      curPageViewsVIDTrend[j,2] <- paste(curPageViewsVIDTrend[j,2], "/", sep = "")
    }
  }
  curPageViewsVIDTrend$jcol <- paste(curPageViewsVIDTrend$datetime, curPageViewsVIDTrend$name)
  names(curPageViewsVIDTrend)[3] <- "VIDPageViews"
  names(curPageViewsVIDTrend)[4] <- "VIDPageVisitors"
  curPageViewsVIDTrend[,1] <- as.character(curPageViewsVIDTrend[,1])
  
  mergedTrends <- left_join(curPageViewsTrend, curPageViewsVIDTrend, by = "jcol")
  
  
  #capture variance
  #temp <- cbind(urlList[i,1],curDate, curPageViews$pageviews, curPageViewsVID$pageviews,curPageViews$pageviews - curPageViewsVID$pageviews, row.names = NULL)
  metricsCompareDF <- rbind(metricsCompareDF, mergedTrends)
}



metricsCompareDF <- metricsCompareDF[!is.na(metricsCompareDF$name.y),]
metricsCompareDF <- metricsCompareDF[,c(1,2,3,4,8,9)]
metricsCompareDF$delta <- -1*(metricsCompareDF$GovPageViews - metricsCompareDF$VIDPageViews)
metricsCompareDF$percentDelta <- -1*(100 - round((metricsCompareDF$VIDPageViews / metricsCompareDF$GovPageViews)*100, 1))

sum(metricsCompareDF$delta)

sum(metricsCompareDF$GovPageViews)
sum(metricsCompareDF$VIDPageViews)
print(-1*(100 - round((sum(metricsCompareDF$VIDPageViews) / sum(metricsCompareDF$GovPageViews))*100, 1)))

sum(metricsCompareDF$GovPageVisitors)
sum(metricsCompareDF$VIDPageVisitors)
print(-1*(100 - round((sum(metricsCompareDF$VIDPageVisitors) / sum(metricsCompareDF$GovPageVisitors))*100, 1)))


write.csv(metricsCompareDF, "metricscompare.csv")



########### stict partials together ########

urlList <- read.csv("urlList.csv")
urlList <- urlList[,c(2,3,4,5,6)]


file1 <- read.csv("metricscompare.csv")
file2 <- read.csv("metricscompare2.csv")
file3 <- read.csv("metricscompare3.csv")

fullCompareDF <- data.frame()
fullCompareDF <- rbind(fullCompareDF, file1)
fullCompareDF <- rbind(fullCompareDF, file2)
fullCompareDF <- rbind(fullCompareDF, file3)

sum(fullCompareDF$delta)

sum(fullCompareDF$GovPageViews)
sum(fullCompareDF$VIDPageViews)
print(-1*(100 - round((sum(fullCompareDF$VIDPageViews) / sum(fullCompareDF$GovPageViews))*100, 1)))

sum(fullCompareDF$GovPageVisitors)
sum(fullCompareDF$VIDPageVisitors)
print(-1*(100 - round((sum(fullCompareDF$VIDPageVisitors) / sum(fullCompareDF$GovPageVisitors))*100, 1)))

write.csv(fullCompareDF, "fullcompare.csv")

############# subset compare ###########

testURL <- strsplit(path(urlList$url[121]), "/")
print(testURL[[1]][1])

urlList$section <- strsplit(path(urlList$url), "/")

for (i in 1:nrow(urlList)) {
  urlList[i,6]  <- urlList[i,6][[1]][1]
}



sitesections <- unique(urlList[,6])

sectionsDF <- data.frame()

for(curSection in sitesections) {
  
  sectionCompareDF <- fullCompareDF[grep(curSection, fullCompareDF$name.x),]
  
  
  sectionDelta <- sum(sectionCompareDF$delta)
  
  sectionGovPageViews <- sum(sectionCompareDF$GovPageViews)
  sectionVIDPageViews <- sum(sectionCompareDF$VIDPageViews)
  sectionPageViewDelta <- -1*(100 - round((sum(sectionCompareDF$VIDPageViews) / sum(sectionCompareDF$GovPageViews))*100, 1))
  
  sectionGovVisitors <- sum(sectionCompareDF$GovPageVisitors)
  sectionVIDVisitors <- sum(sectionCompareDF$VIDPageVisitors)
  sectionVisitorsDelta <- -1*(100 - round((sum(sectionCompareDF$VIDPageVisitors) / sum(sectionCompareDF$GovPageVisitors))*100, 1))
  
  temp <- data.frame(curSection, sectionDelta, sectionGovPageViews, sectionVIDPageViews, sectionPageViewDelta, sectionGovVisitors, sectionVIDVisitors, sectionVisitorsDelta)
  sectionsDF <- rbind(sectionsDF, temp)
}


sectionsDF <- sectionsDF[-1,]
sectionsDF <- sectionsDF[sectionsDF$sectionGovPageViews >0,]
sectionsDF <- sectionsDF[c(1,3,4,2,5,6,7,8),]

write.csv(sectionsDF, "sitesectionsSummary.csv")





################## let's compare DAP to AA ##############
##### time frame, 1/7/2017 - 2/6/2017 ###############

AADF <- QueueRanked("cdcgov", metrics =  c("pageviews", "visitors"), elements = c("prop46"), date.from = "2017-01-07", date.to="2017-02-06", search = "https://www.cdc.gov", max.attempts = 120, top = 500)
write.csv(AADF, "AA.csv")

AADF$name <- str_replace_all(AADF$name, "https://www.", "")
for(j in 1:nrow(AADF)){
  if (is.na(str_locate(AADF[j,1], ".htm")[1])) {
    AADF[j,1] <- paste(AADF[j,1], "index.html", sep = "")
  }
}

AADF <- AADF[,c(1,3)]
names(AADF)[1] <- "Page"
names(AADF)[2] <- "AAPageviews"


GADF <- read.csv("data/HHSGAJanFeb.csv", skip = 6)
GADF <- GADF[1:500,1:2]
names(GADF)[2] <- "GAPageviews"
GADF$GAPageviews <- str_replace_all(GADF$GAPageviews, ",", "")
GADF$GAPageviews <- as.numeric(GADF$GAPageviews)

AAGADF <- inner_join(AADF, GADF, by="Page")

AAGADF$delta <- AAGADF$AAPageviews - AAGADF$GAPageviews
AAGADF$deltaPercent <- -1 * (100 - round((AAGADF$GAPageviews / AAGADF$AAPageviews) * 100, 2))


sum(AAGADF$AAPageviews)
sum(AAGADF$GAPageviews)
sum(AAGADF$delta)
mean(AAGADF$deltaPercent)
median(AAGADF$deltaPercent)

write.csv(AAGADF, "AAGACompare.csv")

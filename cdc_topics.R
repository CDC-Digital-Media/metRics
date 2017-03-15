# devtools::install_github("PMassicotte/gtrendsR") #CRAN version is currently out of date, use this one
# install.packages("tcltk2")
library(jsonlite)
library(dplyr)
library(gtrendsR)
library(tcltk2)

topics <- fromJSON("https://tools.cdc.gov/api/v2/resources/tags?max=0")
results <- topics$results
eng_topics <- subset(results, results$language == "English" & results$type == "Topic")
eng_topics$rank <- -1 #add a column for rank so I can populate it from Google Trends

PopulateNext = function() {
  #acquire first topic that does not have trend data, populate it
  row_index <- which(eng_topics$rank == -1)[1]
  row <- eng_topics[row_index,]
  print(row$name)


  tryCatch({
    trends <- gtrends(keyword = row$name, cat = "45") #Health
    eng_topics[row_index,"rank"] <<- tail(trends$interest_over_time, 1)$hits
    
  }, warning = function(w) {
   print(w)
  }, error = function(e) {
    print(e)
    eng_topics[row_index,"rank"] <<-5
  }, finally = {
    #cleanup-code
  })
  
  #If there are any more topics, do this again after waiting 1 second to avoid Google rate limiting
  if(length(which(eng_topics$rank == -1)) > 0) { tclTaskSchedule(1000, PopulateNext())
  }
  else
  {
    trending_topics <- subset(eng_topics, rank == 100)
    trending_topics$id <- NULL #remove these columns
    trending_topics$language <- NULL
    trending_topics$type <- NULL
    trending_topics$recentSearches <- NULL
  }
}

PopulateNext()
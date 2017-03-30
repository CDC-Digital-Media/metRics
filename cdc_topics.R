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
    PopulateTrending()
  }
}

PopulateNext()

PopulateTrending = function()
{
  trending_topics <<- subset(eng_topics, rank > 90)
  trending_topics$id <<- NULL #remove these columns
  trending_topics$language <<- NULL
  trending_topics$type <<- NULL
  trending_topics$recentSearches <<- NULL
  ignore <<- c("Other", "Funding", "Travel Health - delete")
  trending_topics <<- trending_topics[-which(trending_topics$name %in% ignore),]
  #trending_topics <<- trending_topics[-which(trending_topics$name == "Other" | trending_topics$name == "Funding" | trending_topics$name == "Travel health - delete"),]
}

rows = nrow(trending_topics)
#chunk it by 5

s5 <- seq(1, rows, 5)
trending_topics$rank15 <- -1
row.names(trending_topics) <- trending_topics$name

for(i in s5) {
  last <- ifelse(i +4 < rows, i + 4, rows)
  terms <- trending_topics[i:last,]$name
  trends <- gtrends(keyword = terms, cat = "45") #Health
  overtime <- trends$interest_over_time

  for(j in 1:5) {
    termj_overtime <- overtime[overtime$keyword == terms[j] & overtime$gprop == "web",]
    trending_topics[terms[j],"rank15"] <- max(termj_overtime$hits)
  }
}
trending_topics2 <- subset(trending_topics, rank15 != 0)

trending_topics2$rank15b <- -1

#Cull again
for(i in s5) {
  last <- ifelse(i +4 < rows, i + 4, rows)
  terms <- trending_topics2[i:last,]$name
  trends <- gtrends(keyword = terms, cat = "45") #Health
  overtime <- trends$interest_over_time
  
  for(j in 1:5) {
    termj_overtime <- overtime[overtime$keyword == terms[j] & overtime$gprop == "web",]
    trending_topics2[terms[j],"rank15b"] <- max(termj_overtime$hits)
  }
}
trending_topics3 <- subset(trending_topics2, rank15b != 0)

trending_topics3$rank15c <- -1

#Cull again
for(i in s5) {
  last <- ifelse(i +4 < rows, i + 4, rows)
  terms <- trending_topics3[i:last,]$name
  trends <- gtrends(keyword = terms, cat = "45") #Health
  overtime <- trends$interest_over_time
  
  for(j in 1:5) {
    termj_overtime <- overtime[overtime$keyword == terms[j] & overtime$gprop == "web",]
    trending_topics3[terms[j],"rank15c"] <- max(termj_overtime$hits)
  }
}
trending_topics4 <- subset(trending_topics3, rank15c != 0)


#now slice into different groups
rows4 <- nrow(trending_topics4)
s1_26 <- seq(1, rows4, 25) #1, 26, 51, etc
trending_topics4$rank16 <- -1

for (i in s1_26) {
  s1_6 <- seq(1, rows4, 5) #1, 6, 11, etc
  last <- ifelse(i + 4 < rows4, i + 4, rows4)
  for (j in i:last) {
    lastj <- ifelse(j +20 < rows4, j + 20, rows4)
    indexes <- seq(j, lastj, 5)
    terms <- trending_topics4[indexes, "name"]
    trends <- gtrends(keyword = terms, category = "45") #Health
    overtime <- trends$interest_over_time
    for (k in terms){
      termk_overtime <- overtime[overtime$keyword == k & overtime$gprop == "web",]
      trending_topics4[k, "rank16"] <- max(termk_overtime$hits)
    }
    
  }
}
trending_topics5 <- subset(trending_topics4, rank16 != 0)

#third and final grouping
rows5 <- nrow(trending_topics5)


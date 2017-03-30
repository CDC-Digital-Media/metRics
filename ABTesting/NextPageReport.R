#########################################
############ Edit this block ############
#########################################

SCUser <- Sys.getenv("SC_ID")
SCToken <- Sys.getenv("SC_KEY")
currentABTestSegment <- "s570_573c5ef1e4b0becdc3b444ce" 
#ABTestPage <- "CDC Newsroom | CDC Online Newsroom | CDC"
ABTestPage <- "Pink Eye: Usually Mild and Easy to Treat | Features | CDC"
#ABTestPageURL <- "http://www.cdc.gov/media/index.html"
start <- "2016-04-22"
end <- "2016-06-22"
suite <- "cdcgov"

clickThreshold <- ".5"






#########################################
############ End Edit block  ############
#########################################


#######################
###### Libraries ######
#######################


library("RSiteCatalyst")
library("networkD3")
library("webshot")
library("dplyr")
library("ggplot2")
library("stringr")

#######################
######  Set WD   ######
#######################

result = tryCatch({
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}, warning = function(w) {
  print("warning")
}, error = function(e) {
  print("error")
}, finally = {
  print("")
} )

#### Authentication
SCAuth(SCUser, SCToken)

#### First find to 20 features #####

features <- QueueRanked(suite, start, end, c("pageviews"),  c("page"), search = " | Features |", top = 40)

names(features)[2] <- "pageURL"
names(features)[1] <- "pageName"
features <- features[,1:3]

### initialize all pathing report data
returnedData <- data.frame() 

#nrow(features)
for(i in 1 : 1 ){ 
  
  #checkpage <- features[i,1]
  checkpage <- ABTestPage
  
  rootName <- str_replace_all(str_replace_all(str_replace(checkpage, " \\| Features \\| CDC", ""),"([[:punct:]]+)",""),"([[:space:]]+)","")
  fname <- paste(rootName,".html", sep = "")
  imgName <- paste(rootName,"NextPage.png", sep = "")
  
  reportPath <- paste("//cdc.gov/ahb_apps/prototyped_cdc_gov/abtest/", fname, sep = "")
  viewURL <- paste("http://prototyped.cdc.gov/abtest/",fname, sep = "")
  
  #### Get Pathing data: Single page, then ::anything:: pattern
  pathpattern <- c( checkpage, "::anything::")
  suite <- "cdcgov"
  next_page <- QueuePathing(suite,
                            start,
                            end,
                            metric="pageviews",
                            element="page",
                            pathpattern,
                            top = 50000)
  
  tot <- sum(next_page$count)
  conrate <- round(next_page$count[2] / tot * 100, 1)
  
  
  #write.csv(next_page, "3pages.csv")
  
  next_page <- read.csv("3pages.csv")
  
  page2 <- next_page[,3:4]
  page2$X <- as.numeric(row.names(page2))
  page2 <- page2[,c(3,1,2)]
  names(page2)[1] <- "step.1"
  names(page2)[2] <- "step.2"
  next_page <- next_page[,1:3]
  next_page <- rbind(next_page, page2)
  
  #Optional step: Cleaning my pagename URLs to remove to domain for clarity
  next_page$step.1 <- sub("http://www.cdc.gov/","", 
                          next_page$step.1, ignore.case = TRUE)
  next_page$step.2 <- sub("http://www.cdc.gov","", 
                          next_page$step.2, ignore.case = TRUE)
  
  next_page <- next_page[,1:3]
  
  totClicks <- sum(next_page$count)
  
  next_page$clickPercent <- round(next_page$count/totClicks * 100, 1)
  
  ##### store these values for summary later ######
  if(nrow(returnedData) == 0){
    returnedData <- next_page
  } else {
    returnedData <- rbind(returnedData, next_page)
  }
  
  next_page <- next_page[as.numeric(next_page$clickPercent) >= as.numeric(clickThreshold), ]
  
  ###for display reasons ###
  next_page$step.2 <- paste(next_page$step.2," - ", next_page$clickPercent, "%", sep = "")
  next_page <- next_page[,1:3]
  
  
  
  #Get unique values of page name to create nodes df
  #Create an index value, starting at 0
  nodes <- as.data.frame(unique(c(next_page$step.1, next_page$step.2)))
  names(nodes) <- "name"
  nodes$nodevalue <- as.numeric(row.names(nodes)) - 1
  
  #Convert string to numeric nodeid
  links <- merge(next_page, nodes, by.x="step.1", by.y="name")
  names(links) <- c("step.1", "step.2", "value", "source")
  
  links <- merge(links, nodes, by.x="step.2", by.y="name")
  names(links) <- c("step.1", "step.2", "value", "source", "target")
  
  #Create next page Sankey chart
  d3output = reportPath
  
  sankeyNetwork(Links = links, Nodes = nodes, Source = "source",
                Target = "target", Value = "value", NodeID = "name",
                nodeWidth = 30, fontSize = 12, width = 650, height = 900) %>%
    saveNetwork(file = reportPath)
  
  webshot(viewURL, imgName, delay = 0.2)
  
}

returnedData <- returnedData[,1:4]
returnedData$reviewURL <- paste("http://prototyped.cdc.gov/abtest/",str_replace_all(str_replace_all(str_replace(returnedData$step.1, " \\| Features \\| CDC", ""),"([[:punct:]]+)",""),"([[:space:]]+)",""), ".html", sep="")

returnedData2 <- returnedData[as.numeric(returnedData$clickPercent) >= as.numeric(clickThreshold),]    


write.csv(returnedData, "fullSet.csv")
write.csv(returnedData2, "smallSet.csv")

exits <- filter(returnedData2, step.2 == "Exited Site")

############### basic clickthrough plot
p = ggplot(data=exits, 
           aes(x=str_replace(step.1, " \\| Features \\| CDC", ""),
               y=clickPercent
           )
)

labelPrint <- paste("Site Exits by Feature Page\n")
p=p + geom_bar(stat = "identity", fill="blue") +
  xlab("Page") + ylab("Percent Exited") +
  ggtitle(labelPrint)
p=p + guides(fill=FALSE) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p

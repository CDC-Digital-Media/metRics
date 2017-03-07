library(XML)
library(RCurl)
library(dplyr)
library(ggplot2)

#I have to use this because of a JRE error loading rJava for the XLSX library
#options(java.home="C:\\Program Files\\Java\\jre1.8.0_111\\")
#Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_111')
options(java.home=Sys.getenv("JAVA_HOME"))
library(xlsx)



#load test frame from CSV
#compURLs <- read.csv("searchlinks.csv")
compTerms <- read.xlsx("searchterms.xlsx", 1, header=FALSE)
names(compTerms)[1] <- "term"


#set up results DataFrames
GSAResults <- data.frame("", "", "")
names(GSAResults)[1] = "searchResults"
names(GSAResults)[2] = "searchTerm"
names(GSAResults)[3] = "GSA_Rank"

SolrResults <- data.frame("","", "")
names(SolrResults)[1] = "searchResults"
names(SolrResults)[2] = "searchTerm"
names(SolrResults)[3] = "SOLR_Rank"



for(i in 1:nrow(compTerms)) {
  
  toParse <- paste("http://widv-kmti-oadc1.cdc.gov/connects/search/?ddlTitleWeight=50&ddlKeyWeight=30&ddlContentWeight=1&ddlTitleWeightExact=100&ddlKeyWeightExact=30&ddlContentWeightExact=5&ddlPFTitleWeight=50&ddlPFContentWeight=1&ddlPFTitleWeightExact=100&ddlPFContentWeightExact=5&query=", as.character(compTerms[i,1]), sep="")
  toParseGSA <- paste("https://search.cdc.gov/search?utf8=%E2%9C%93&affiliate=cdc-main&query=", as.character(compTerms[i,1]), sep="")
  
  #SOLR Results
  
  con <- url(toParse)
  SOLRPage <- readLines(con)
  close(con)
  
  
  SOLRPage2 <- htmlParse(SOLRPage)
  SolrLinkList <- data.frame(xpathSApply(SOLRPage2, '//div[@class="section listing searchResults"]//a', xmlGetAttr, 'href'))
  
  
  names(SolrLinkList)[1] = "searchResults"
  #SolrLinkList$searchTerm = compURLs[i,1]
  SolrLinkList$searchTerm <- compTerms[i,1]
  SolrLinkList <- SolrLinkList[-1,]
  SolrLinkList$SOLR_Rank = as.character(1:nrow(SolrLinkList))
  SolrResults <- rbind(SolrResults, SolrLinkList)
  
  # GSA results
  
  con <- url(toParseGSA)
  GSAPage <- readLines(con)
  close(con)
  
  GSAPage2 <- htmlParse(GSAPage)
  GSALinkList <- data.frame(xpathSApply(GSAPage2, '//div[@id="results"]//a', xmlGetAttr, 'href'))
  
  names(GSALinkList)[1] = "searchResults"
  GSALinkList$searchTerm = compTerms[i,1]
  GSALinkList$GSA_Rank = as.character(1:nrow(GSALinkList))
  GSAResults <- rbind(GSAResults, GSALinkList)
  
  GSAResults$searchResults <- as.character(GSAResults$searchResults)
  GSAResults$searchTerm <- as.character(GSAResults$searchTerm)
  SolrResults$searchResults <- as.character(SolrResults$searchResults)
  SolrResults$searchTerm <- as.character(SolrResults$searchTerm)
  
  GSA2Solr <- left_join(GSAResults, SolrResults, by = c("searchResults" = "searchResults", "searchTerm"="searchTerm"))
  Solr2GSA <- left_join( SolrResults,GSAResults, by = c("searchResults" = "searchResults", "searchTerm"="searchTerm"))
  commonResults <- inner_join(GSAResults, SolrResults, by = c("searchResults" = "searchResults", "searchTerm"="searchTerm"))
  

  
  GSA2Solr <- GSA2Solr[-1,]
  GSA2Solr$SOLR_Rank = factor(GSA2Solr$SOLR_Rank, levels=c(levels(GSA2Solr$SOLR_Rank), 0))
  GSA2Solr$SOLR_Rank[is.na(GSA2Solr$SOLR_Rank)] <- 0
  
  Solr2GSA <- Solr2GSA[-1,]
  Solr2GSA$GSA_Rank = factor(Solr2GSA$GSA_Rank, levels = c(levels(Solr2GSA$GSA_Rank), 0))
  Solr2GSA$GSA_Rank[is.na(Solr2GSA$GSA_Rank)] <- 0
  
  commonResults <- commonResults[-1,]
  
  
  #Save Results
  write.csv(GSA2Solr, paste(compTerms[i,1], "GSA2SOLR.csv", sep = ""))
  write.csv(Solr2GSA, paste(compTerms[i,1], "SOLR2GSA.csv", sep = ""))
  write.csv(commonResults, paste(compTerms[i,1], "CommonResults.csv", sep = ""))
  
  if(nrow(commonResults) < 2)
    print(paste0("no common results for term ", compTerms[i,1]))

  #graph
  
  
  p <- ggplot(na.omit(GSA2Solr), aes(x=GSA_Rank, y=SOLR_Rank)) + geom_point() + geom_hline(aes(yintercept=10), color="red") + geom_abline(intercept = 0, slope=1, color="blue")
  p + facet_wrap( ~ searchTerm) + ggtitle("Comparing SOLR Results to GSA Results\nRed Line is GSA first page cut off\nBlue line are GSA results")
  
  ggsave(paste(compTerms[i,1], "ResultsPlot.pdf", sep = ""), width=7, height=10, dpi=100)

  #reset results DataFrames
  ### shortcut because I didn't do this correctly.  
  GSAResults <- data.frame("", "", "")
  names(GSAResults)[1] = "searchResults"
  names(GSAResults)[2] = "searchTerm"
  names(GSAResults)[3] = "GSA_Rank"
  
  SolrResults <- data.frame("","", "")
  names(SolrResults)[1] = "searchResults"
  names(SolrResults)[2] = "searchTerm"
  names(SolrResults)[3] = "SOLR_Rank"
  
  
}

library(XML)
library(RCurl)
library(dplyr)
library(ggplot2)

#I have to use this because of a JRE error loading rJava for the XLSX library
#options(java.home="C:\\Program Files\\Java\\jre1.8.0_111\\")
#Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_111')
options(java.home=Sys.getenv("JAVA_HOME"))
library(xlsx)
source("UrlTools.R")


#load test frame from CSV
#compURLs <- read.csv("searchlinks.csv")
compTerms <- read.xlsx("searchterms.xlsx", 1, header=FALSE)
names(compTerms)[1] <- "term"

for(i in 1:nrow(compTerms)) {
  ExecuteSearches(compTerms[i,1])  
}

ExecuteSearches = function(term) {
  
  #set up results DataFrames
  GSAResults <- data.frame("", "", "")
  names(GSAResults)[1] = "searchResults"
  names(GSAResults)[2] = "searchTerm"
  names(GSAResults)[3] = "GSA_Rank"
  
  SolrResults <- data.frame("","", "")
  names(SolrResults)[1] = "searchResults"
  names(SolrResults)[2] = "searchTerm"
  names(SolrResults)[3] = "SOLR_Rank"
  
  toParse <- paste("http://widv-kmti-oadc1.cdc.gov/connects/search/?ddlTitleWeight=50&ddlKeyWeight=30&ddlContentWeight=1&ddlTitleWeightExact=100&ddlKeyWeightExact=30&ddlContentWeightExact=5&ddlPFTitleWeight=50&ddlPFContentWeight=1&ddlPFTitleWeightExact=100&ddlPFContentWeightExact=5&query=", as.character(term), sep="")
  toParseGSA <- paste("https://search.cdc.gov/search?utf8=%E2%9C%93&affiliate=cdc-main&query=", as.character(term), sep="")
  
  #SOLR Results
  
  con <- url(toParse)
  SOLRPage <- readLines(con)
  close(con)
  
  
  SOLRPage2 <- htmlParse(SOLRPage)
  SolrLinkList <- data.frame(xpathSApply(SOLRPage2, '//div[@class="section listing searchResults"]//a', xmlGetAttr, 'href'))
  
  
  names(SolrLinkList)[1] = "searchResults"
  SolrLinkList$searchTerm <- term
  SolrLinkList <- SolrLinkList[-1,]
  SolrLinkList$SOLR_Rank = as.character(1:nrow(SolrLinkList))
  SolrResults <- rbind(SolrResults, SolrLinkList)
  SolrResults$searchResults <- CleanUrl(SolrResults$searchResults)
  
  # GSA results
  
  con <- url(toParseGSA)
  GSAPage <- readLines(con)
  close(con)
  
  GSAPage2 <- htmlParse(GSAPage)
  GSALinkList <- data.frame(xpathSApply(GSAPage2, '//div[@id="results"]//a', xmlGetAttr, 'href'))
  
  names(GSALinkList)[1] = "searchResults"
  GSALinkList$searchTerm = term
  GSALinkList$GSA_Rank = as.character(1:nrow(GSALinkList))
  GSAResults <- rbind(GSAResults, GSALinkList)
  GSAResults$searchResults <- CleanUrl(GSAResults$searchResults)
  
  GSAResults$searchResults <- as.character(GSAResults$searchResults)
  GSAResults$searchTerm <- as.character(GSAResults$searchTerm)
  SolrResults$searchResults <- as.character(SolrResults$searchResults)
  SolrResults$searchTerm <- as.character(SolrResults$searchTerm)
  
  GSA2Solr <- left_join(GSAResults, SolrResults, by = c("searchResults" = "searchResults", "searchTerm"="searchTerm"))
  Solr2GSA <- left_join( SolrResults,GSAResults, by = c("searchResults" = "searchResults", "searchTerm"="searchTerm"))
  commonResults <- inner_join(GSAResults, SolrResults, by = c("searchResults" = "searchResults", "searchTerm"="searchTerm"))
  
  
  
  GSA2Solr <- GSA2Solr[-1,]
  Solr2GSA <- Solr2GSA[-1,]
  commonResults <- commonResults[-1,]
  
  
  #Save Results
  write.csv(GSA2Solr, paste(term, "GSA2SOLR.csv", sep = ""))
  write.csv(Solr2GSA, paste(term, "SOLR2GSA.csv", sep = ""))
  write.csv(commonResults, paste(term, "CommonResults.csv", sep = ""))

  if(nrow(commonResults) < 1) {
    print(paste0("no common results for term ", term))
  } else {
    print(paste0(nrow(commonResults), " results for term ", term))
    
    #graph
    
    p <- ggplot(na.omit(GSA2Solr), aes(x=GSA_Rank, y=SOLR_Rank)) + geom_point() + geom_hline(aes(yintercept=10), color="red") + geom_abline(intercept = 0, slope=1, color="blue")
    p + facet_wrap( ~ searchTerm) + ggtitle("Comparing SOLR Results to GSA Results\nRed Line is GSA first page cut off\nBlue line are GSA results")
    
    ggsave(paste(term, "ResultsPlot.pdf", sep = ""), width=7, height=10, dpi=100)
  }
}

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


ExecuteSearches = function(term) {

  toParse <- paste("http://widv-kmti-oadc1.cdc.gov/connects/search/?ddlTitleWeight=50&ddlKeyWeight=30&ddlContentWeight=1&ddlTitleWeightExact=100&ddlKeyWeightExact=30&ddlContentWeightExact=5&ddlPFTitleWeight=50&ddlPFContentWeight=1&ddlPFTitleWeightExact=100&ddlPFContentWeightExact=5&query=", as.character(term), sep="")
  toParseGSA <- paste("https://search.cdc.gov/search?utf8=%E2%9C%93&affiliate=cdc-main&query=", as.character(term), sep="")
  
  
  #SOLR Results
  
  con <- url(toParse)
  SOLRPage <- readLines(con)
  close(con)
  
  
  SOLRPage2 <- htmlParse(SOLRPage)
  SolrLinks <- xpathSApply(SOLRPage2, '//div[@class="section listing searchResults"]//a', xmlGetAttr, 'href') %>% CleanUrl %>% as.character
  SolrResults <- data.frame(SolrLinks, as.character(rep(term, length(SolrLinks))), as.character(1:length(SolrLinks)), stringsAsFactors=FALSE)
  colnames(SolrResults) <- c("searchResults", "searchTerm", "SOLR_Rank")
  
  
  # GSA results
  
  con <- url(toParseGSA)
  GSAPage <- readLines(con)
  close(con)
  
  GSAPage2 <- htmlParse(GSAPage)
  GSALinks <- xpathSApply(GSAPage2, '//div[@id="results"]//a', xmlGetAttr, 'href') %>% CleanUrl 
  GSAResults <- data.frame(GSALinks, as.character(rep(term, length(GSALinks))), as.character(1:length(GSALinks)), stringsAsFactors=FALSE)
  colnames(GSAResults) <- c("searchResults", "searchTerm", "GSA_Rank")
  
  
  GSA2Solr <- left_join(GSAResults, SolrResults, by = c("searchResults" = "searchResults", "searchTerm"="searchTerm"))
  Solr2GSA <- left_join(SolrResults, GSAResults, by = c("searchResults" = "searchResults", "searchTerm"="searchTerm"))
  commonResults <- inner_join(GSAResults, SolrResults, by = c("searchResults" = "searchResults", "searchTerm"="searchTerm"))
  
  
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

for(i in 1:nrow(compTerms)) {
  ExecuteSearches(compTerms[i,1])  
}

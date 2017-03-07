library(XML)
library(RCurl)
library(dplyr)
library(ggplot2)



#load test frame from CSV
compURLs <- read.csv("searchlinks.csv")



#set up results DataFrames
GSAResults <- data.frame("", "", "")
names(GSAResults)[1] = "searchResults"
names(GSAResults)[2] = "searchTerm"
names(GSAResults)[3] = "GSA_Rank"

SolrResults <- data.frame("","", "")
names(SolrResults)[1] = "searchResults"
names(SolrResults)[2] = "searchTerm"
names(SolrResults)[3] = "SOLR_Rank"


for(i in 1:nrow(compURLs)) {

        toParse <- as.character(compURLs[i,3])
        toParseGSA <- as.character(compURLs[i,2])

        #SOLR Results
        
        con <- url(toParse)
        SOLRPage <- readLines(con)
        close(con)
        
        
        SOLRPage2 <- htmlParse(SOLRPage)
        SolrLinkList <- data.frame(xpathSApply(SOLRPage2, '//div[@class="section listing searchResults"]//a', xmlGetAttr, 'href'))
        

        names(SolrLinkList)[1] = "searchResults"
        SolrLinkList$searchTerm = compURLs[i,1]
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
        GSALinkList$searchTerm = compURLs[i,1]
        GSALinkList$GSA_Rank = as.character(1:nrow(GSALinkList))
        GSAResults <- rbind(GSAResults, GSALinkList)

}



GSA2Solr <- left_join(GSAResults, SolrResults, by = c("searchResults" = "searchResults", "searchTerm"="searchTerm"))
Solr2GSA <- left_join( SolrResults,GSAResults, by = c("searchResults" = "searchResults", "searchTerm"="searchTerm"))
commonResults <- inner_join(GSAResults, SolrResults, by = c("searchResults" = "searchResults", "searchTerm"="searchTerm"))

GSA2Solr <- GSA2Solr[-1,]
Solr2GSA <- Solr2GSA[-1,]
commonResults <- commonResults[-1,]


#Save Results
write.csv(GSA2Solr, "GSA2SOLR.csv")
write.csv(Solr2GSA, "SOLR2GSA.csv")
write.csv(commonResults, "CommonResults.csv")

#graph


p <- ggplot(na.omit(GSA2Solr), aes(x=GSA_Rank, y=SOLR_Rank)) + geom_point() + geom_hline(aes(yintercept=10), color="red") + geom_abline(intercept = 0, slope=1, color="blue")
p + facet_wrap( ~ searchTerm) + ggtitle("Comparing SOLR Results to GSA Results\nRed Line is GSA first page cut off\nBlue line are GSA results")

ggsave("ResultsPlot.pdf", width=7, height=10, dpi=100)



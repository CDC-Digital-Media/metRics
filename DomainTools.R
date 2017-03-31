### This first function will return the original dataFrame with one additional column
### for 'CanonicalDomain" in order to retain original data and frame length


### A later funtion [reducedDomains()] and flag to first function will reduce the result frame
### I want to pass a dataframe and a column number, and return a new dataframe
### with simplified by canonical domains with a column with lists of combined domains
### eg:  cdc.gov, list("cdc.gov", www.cdc.gov", "tools.cdc.gov", "prototype.cdc.gov")
### or:  healthyarkansas.com, list("healthyarkansas.com", "healthyarkansas.org")


combineDomains <- function(myDF, domainIndex, reduce = 0) {
  
  #myDF <- Z_microsites[,1:6]
  #domainIndex = 6
  #reduce=0
  library("plyr")
  library("dplyr")
  library("stringr")
  library("magrittr")
  
  
  
  ## to be used later for merging on col name
  preserveName <- names(myDF)[domainIndex]
  names(myDF)[domainIndex] <- "Domain"
  
  
  newFrame <- data.frame()
  
  #splitting the domain into parts (explode on .)
  for(curDomain in myDF$Domain) {
    newFrame[nrow(newFrame) + 1, 1] <- curDomain
    tempVar <- strsplit(as.character(curDomain), ".", fixed = TRUE)
    for (element in tempVar) {
      i= length(element)
      j=2
      while(i >= 1) {
        newFrame[nrow(newFrame), j] <- element[i]
        i = i -1
        j=j+1
      }
      
    }
    
    
  }
  
  #Rename Cols
  names(newFrame)[1] <- "Domain"
  names(newFrame)[2] <- "TopLevel"
  names(newFrame)[3] <- "SecondLevel"
  ### next two tests are for when early domain data is only top and second level, eg nih.gov
  if(ncol(newFrame) < 4) {newFrame[,4] <- ""}
  names(newFrame)[4] <- "ThirdLevel"
  if(ncol(newFrame) < 5) {newFrame[,5] <- ""}
  names(newFrame)[5] <- "FourthLevel"
  
  ### Why is this now reducing the frame down to the single column?
  ### Do I even need this frame?
  newFrame <- newFrame %>% distinct(Domain)
  #newFrame <- newFrame[!is.na(newFrame$Domain),]
  
  combinedData <- left_join(myDF, newFrame, by = "Domain")
  
  
  combinedData$normalizedDomain <- str_c(combinedData$SecondLevel,  combinedData$TopLevel, sep = ".")
  
  
  
  
  ### garbage collection
  rm(newFrame)
  
  #### loop through the data frame removing staging, development, IP and other known non production environments
  for(i in 1:nrow(combinedData)) {
    if(is.na(combinedData$TopLevel[i])) {
      
      combinedData$normalizedDomain[i] <- "void"
      combinedData$normalizedTopLevel[i] <- "void"
      combinedData$normalizedSecondLevel[i] <- "void"
    } else {
      
      # one bad domain that was .net;b....and a long GUID
      if(str_detect(combinedData$TopLevel[i], ";")) {combinedData$TopLevel[i] <- str_sub(combinedData$TopLevel[i], 1, 3)}
      # remove the servers that are clearly development, staging, IP only and not production systems.  One item was removed that is probably an intranet of Miami Dade DOH. It probably should put back in.
      if(str_detect(combinedData$Domain[i], "dev") || str_detect(combinedData$Domain[i], "stag") || str_detect(combinedData$Domain[i], "local") || str_detect(combinedData$Domain[i], "scraplist") || str_detect(combinedData$Domain[i], "report") || str_detect(combinedData$Domain[i], "lcl") || str_detect(combinedData$Domain[i], "beta") || str_detect(combinedData$Domain[i], "nettrekker") || str_detect(combinedData$Domain[i], "googledrive") ) {
        combinedData$normalizedDomain[i] <- "void"
        combinedData$normalizedTopLevel[i] <- "void"
        combinedData$normalizedSecondLevel[i] <- "void"
      }
      # NIAID has several development and staging servers, but also a number of libraries around the world use an ezproxy product to copy
      # the entire NIAID site.  That still only constitutes one parter.
      else if(str_detect(combinedData$Domain[i], "niaid") ) {
        combinedData$normalizedDomain[i] <- "niaid.nih.gov"
        combinedData$normalizedTopLevel[i] <- "gov"
        combinedData$normalizedSecondLevel[i] <- "niaid.nih"
      }
      
      else {
        # many local governments use the county.st.us domain model, so to distinguish, we must go to third level domains.
        # The same applies for county.ca.gov domains for California
        # Indiana is also an exception.  For the .us domains, it's 4 levels deep --co.monroe.in.us v state.in.us
        # need to generalize this for other states....
        if( combinedData$TopLevel[i] == "us" || (combinedData$SecondLevel[i] == "ca" && combinedData$TopLevel[i] == "gov")) {
          if(combinedData$SecondLevel[i] == "in") {
            combinedData$normalizedDomain[i] <- str_c(combinedData$FourthLevel[i], combinedData$ThirdLevel[i],  combinedData$SecondLevel[i], combinedData$TopLevel[i], sep = ".")
            combinedData$normalizedTopLevel[i] <- combinedData$TopLevel[i]
            combinedData$normalizedSecondLevel[i] <- str_c(combinedData$ThirdLevel[i],combinedData$SecondLevel[i], sep = ".")
          }
          else {
            combinedData$normalizedDomain[i] <- str_c(combinedData$ThirdLevel[i],  combinedData$SecondLevel[i], combinedData$TopLevel[i], sep = ".")
            combinedData$normalizedTopLevel[i] <- combinedData$TopLevel[i]
            combinedData$normalizedSecondLevel[i] <- combinedData$SecondLevel[i]
          }
        }else {
          if(is.na(combinedData$SecondLevel[i])) {
            combinedData$normalizedDomain[i] <- "void"
            combinedData$normalizedTopLevel[i] <- "void"
            combinedData$normalizedSecondLevel[i] <- "void"
          }
          else {
            # other known errant URLs
            if(combinedData$TopLevel[i] == "pdf" || combinedData$TopLevel[i] < "a" || combinedData$SecondLevel[i] == "rarepath") {
              combinedData$normalizedDomain[i] <- "void"
              combinedData$normalizedTopLevel[i] <- "void"
              combinedData$normalizedSecondLevel[i] <- "void"
            }
            else {combinedData$normalizedDomain[i] <- str_c(  combinedData$SecondLevel[i],  combinedData$TopLevel[i], sep = ".")
            combinedData$normalizedTopLevel[i] <- combinedData$TopLevel[i]
            combinedData$normalizedSecondLevel[i] <- combinedData$SecondLevel[i]
            }
          }
        }
      }
    }
  }
  
  # next line would remove the bad URLs.  However for first pass to return
  # a data set of the same length, I'll remove.
  # resultFrame <- combinedData[combinedData$normalizedDomain != "void",]
  
  
  resultFrame <- combinedData
  
  ### More garbage Collection
  rm(combinedData)
  
  
  resultFrame <- resultFrame[order(resultFrame$normalizedSecondLevel),]
  #set the default
  resultFrame$canonicalPartnerDomain <- resultFrame$normalizedDomain
  
  
  for(i in 1:nrow(resultFrame)) {
    if(i == 1){skipFlag = 0}
    if(skipFlag > 0) {
      skipFlag = skipFlag -1
    }
    else {
      #exclude last row.  Cannot test forward
      #plus there is an empty row of NA values, so have to end nrow - 1
      #skipping blogspot as a large hosted service with multiple country instances (.ro, .ru, .mx)
      ### need to fix for blogspot
      ### labmed is screwed up as well.
      if(i < nrow(resultFrame) -1 && resultFrame$normalizedSecondLevel[i] != "blogspot"){
        if(resultFrame$normalizedSecondLevel[i] != resultFrame$normalizedSecondLevel[i+1]) {
          resultFrame$canonicalPartnerDomain[i] <- resultFrame$normalizedDomain[i]
        }
        else {
          #going to presume no more than 3 current TL domains per organization (.com, .org, .net)
          if(resultFrame$normalizedSecondLevel[i] != resultFrame$normalizedSecondLevel[i+2]) {
            #only 2 TL domains
            #but we need to correct for .us, ca and others that include third level
            if( resultFrame$normalizedTopLevel[i] != "us" && str_c(resultFrame$normalizedSecondLevel[i],resultFrame$normalizedTopLevel[i], sep=".")  != "ca.gov") {
              resultFrame$canonicalPartnerDomain[i] <- resultFrame$normalizedDomain[i]
              resultFrame$canonicalPartnerDomain[i + 1] <- resultFrame$normalizedDomain[i]
              skipFlag = 1
            }
          }
          else {
            #three TL domains
            #correcting for .us and ca.gov
            if( resultFrame$normalizedTopLevel[i] != "us" && str_c(resultFrame$normalizedSecondLevel[i],resultFrame$normalizedTopLevel[i], sep=".")  != "ca.gov") {
              resultFrame$canonicalPartnerDomain[i] <- resultFrame$normalizedDomain[i]
              resultFrame$canonicalPartnerDomain[i + 1] <- resultFrame$normalizedDomain[i]
              resultFrame$canonicalPartnerDomain[i + 2] <- resultFrame$normalizedDomain[i]
              skipFlag = 2
            }
          }
        }
      }
    }
  }
  
  
  ##### gather multiple domains of the same organization together:
  ##### eg: health.gov, health.com, health.org
  
  
  
  
  
  resultFrame <- resultFrame[,c("Domain", "canonicalPartnerDomain")]
  
  #cbind????
  #resultFrame <- merge(myDF, resultFrame, by.x = "Domain", by.y = "Domain")
  #resultFrame <- cbind(myDF, resultFrame)
  resultFrame <- resultFrame[order(resultFrame$Domain), ]
  myDF <- myDF[order(myDF$Domain), ]
  resultFrame <- cbind(myDF, resultFrame[2])
  if(reduce == TRUE) {
    resultFrame <- resultFrame[resultFrame$canonicalPartnerDomain != "void",]
    resultFrame <- resultFrame %>% distinct(canonicalPartnerDomain)
    resultFrame <- resultFrame[order(resultFrame$canonicalPartnerDomain), ]
  }
  
  return(resultFrame)
  #### end function
}


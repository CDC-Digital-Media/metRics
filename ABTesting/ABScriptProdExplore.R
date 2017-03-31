


#######################################################################
################## First load up required libs  #######################
################## And set necessary vaiables   #######################
################## eg, current conv percentage, #######################
################## Confidence interval,         #######################
################## SCAuth details, segment ID,  #######################
################## page name, etc               #######################
#######################################################################


#########################################
############ Edit this block ############
#########################################

SCUser <- Sys.getenv("SC_ID")
SCToken <- Sys.getenv("SC_KEY")
currentABTestSegment <- "s570_56e706c2e4b007118bee364d" 
ABTestPage <- "Pink Eye: Usually Mild and Easy to Treat | Features | CDC"
ABTestPageURL <- "http://www.cdc.gov/Features/Conjunctivitis/index.html"
start <- "2016-06-24"
end <- "2016-07-24"

VarAMediaID <- 128154
VarBMediaID <- 278477
VarAFriendlyName <- "Current 'Treatment' Link"
VarBFriendlyName <- "'Treament' Link in Blue Box"

currentConversionRate <- .05
minEffect <- .03
sampleVariance <- currentConversionRate*(1-currentConversionRate)
SampleSize <- 16*(sampleVariance/minEffect^2)

#segments <- GetSegments(suite)

#########################################
############ End Edit block  ############
#########################################


#######################
###### Libraries ######
#######################

library("RSiteCatalyst")
library("urltools")
library("stringr")
library("ggplot2")
library("knitr")
library("dplyr")
library("lubridate")
library("knitr")
library("webshot")

#webshot::install_phantomjs()

##############################################
####         set working directory       #####
#### works only when run through RStudio #####
####   important for writing reports     #####
##############################################


result = tryCatch({
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}, warning = function(w) {
  print("warning")
}, error = function(e) {
  print("error")
}, finally = {
  #Always executes
  #print("")
} )


#####################################################
######### retrieve data from SiteCatalyst ###########
#####################################################

SCAuth(SCUser, SCToken)
curDate <- today()
suite <- "cdcgov"
elements <- c("page", "eVar8") 
metrics <- c("pageviews", "totalevent2", "participationevent2")


prod.prop18 <- QueueRanked(suite, start, end, c("pageviews", "participationevent2"),  c("page", "prop18"), segment.id = currentABTestSegment)
# prod.prop42 <- QueueRanked(suite, start, end, c("pageviews", "participationevent2"),  c("page", "prop42"), segment.id = currentABTestSegment)



#####################################
########## process data #############
#####################################

## Filter for specific page name ##
##### TO DO:  Need to verify that noise in page name is a hold over from the cookie in SC, not a JS bleed #####

prod.onlyMediaPage <- subset(prod.prop18, page==ABTestPage)

#### Aggregate by 
prod.clicks <- subset(prod.onlyMediaPage, grepl(ABTestPage, prod.onlyMediaPage[[2]]), drop = TRUE)

#### drop unneeded columns
prod.clicks <- prod.clicks[,2:4]


###########Extract Media ID from prop18 as factor###############
prod.clicks$mediaID <- as.factor(str_extract(prod.clicks$prop18, "^[0-9a-z]+"))

########## by creating a factor and an ordered list, allows for labels to be added by list position
########## based on an alphabetical ordering of media IDs.  

if(VarAMediaID > VarBMediaID) {
  variantLabels <- c("Variant B", "Variant A", "Undefined")
} else {
  variantLabels <- c("Variant A", "Variant B", "Undefined")
}

prod.clicks$variant <- variantLabels[as.numeric(prod.clicks$mediaID)]
prod.clicks$conversion <- prod.clicks$participationevent2 / prod.clicks$pageviews

prod.clicks.ABonly <- prod.clicks[as.numeric(prod.clicks$variant) != 3,]
prod.clicks.ABonly <- prod.clicks[prod.clicks$variant != "Undefined",]
prod.clicks.ABonly <- arrange(prod.clicks.ABonly, variant)
keepCols <- c(5,2,3,6)
prod.clicks.ABonly <- prod.clicks.ABonly[,keepCols]

prod.clicks.ABonly$variant <- as.factor(prod.clicks.ABonly$variant)
prod.clicks.ABonly$conversion <- paste(round(prod.clicks.ABonly$conversion * 100, 1), "%", sep = "")

############ Summary Data Print Out ##############

#### clean up column names for display
names(prod.clicks.ABonly)[1] <- "Variant"
names(prod.clicks.ABonly)[2] <- "Impressions"
names(prod.clicks.ABonly)[3] <- "Conversions"
names(prod.clicks.ABonly)[4] <- "Conversion Rate"

print(prod.clicks)

print(prod.clicks.ABonly)

############# Sample Size Checking and Reporting #############

if(min(prod.clicks.ABonly$Impressions) < SampleSize) {
  print("Sample size has not yet reached target for statistical validity.")
  print("It is not recommended to stop the test.")
  
  daysRunning <- as.numeric(as.Date(ymd(curDate)) - as.Date(ymd(start)))
  prod.clicks.ABonly$impressionsPerDay <- round(prod.clicks.ABonly$Impressions / daysRunning, 0)
  prod.clicks.ABonly$impressionsNeeded <- ceiling(SampleSize - prod.clicks.ABonly$Impressions)
  prod.clicks.ABonly$daysNeeded <- ceiling(prod.clicks.ABonly$impressionsNeeded / prod.clicks.ABonly$impressionsPerDay)
  
  
  print(paste("Given a current conversion rate of ", currentConversionRate*100, "% and a minimal detected change of ", minEffect*100, "%:\n", sep = ""))
  print(paste(prod.clicks.ABonly$impressionsNeeded[1], "more impressions needed for", prod.clicks.ABonly$Variant[1],"\n", sep = " "))
  print(paste(prod.clicks.ABonly$impressionsNeeded[2], "more impressions needed for", prod.clicks.ABonly$Variant[2],"\n", sep = " "))
  print(paste("An estimated", max(prod.clicks.ABonly$daysNeeded) , "day(s) needed until target sample size is reached.\n", sep = " "))
}




##### Add a percentage as numeric column for graphing
prod.clicks.ABonly$CRateGraph <- round(prod.clicks.ABonly$Conversions/prod.clicks.ABonly$Impressions * 100, 1)

################## Analysis Code #######################
################## May 18, 2016  #######################





############### basic pageview plot
p = ggplot(data=prod.clicks.ABonly, 
           aes(x=factor(Variant, levels = variantLabels),
               y=Impressions
           )
)

labelPrint <- paste("Impressions by Variant\n")
p=p + geom_bar(stat = "identity", fill="blue") +
  xlab("Variant") + ylab("Impressions") +
  ggtitle(labelPrint)
p=p + guides(fill=FALSE)
p


############### basic clickthrough plot
p = ggplot(data=prod.clicks.ABonly, 
           aes(x=factor(Variant, levels = variantLabels),
               y=Conversions
           )
)

labelPrint <- paste("Conversions by Variant\n")
p=p + geom_bar(stat = "identity", fill="blue") +
  xlab("Variant") + ylab("Conversions") +
  ggtitle(labelPrint)
p=p + guides(fill=FALSE)
p


############### basic click/pageview percentage plot
p = ggplot(data=prod.clicks.ABonly, 
           aes(x=factor(Variant, levels = variantLabels),
               y=CRateGraph
           )
)

labelPrint <- paste("Conversion Rate by Variant\nRed line represents current conversion rate.\nGreen lines represent margin of error.")
p=p + geom_bar(stat = "identity", fill='blue') +
  xlab("Variant") + ylab("Conversion Rate") +
  ggtitle(labelPrint) + geom_hline(aes(yintercept=currentConversionRate * 100), color="red") + geom_hline(aes(yintercept=currentConversionRate * 100 + minEffect * 100), color="green")+ geom_hline(aes(yintercept=currentConversionRate * 100 - minEffect * 100), color="green")
p=p + guides(fill=FALSE)
p


#webshot version


# while in testing, the JS and CSS for the highlight is not working, so to get screeshots
# need to point to dev page
# first element in paste below should be ABTestPageURL

fname <- "VariantA.png"
target <- paste(pageURL,"?variant=",VarAMediaID,"&highlight=true", sep = "")
webshot(target, fname, delay = 0.5)

fname <- "VariantB.png"
target <- paste(pageURL,"?variant=",VarBMediaID,"&highlight=true", sep = "")
webshot(target, fname, delay = 0.5)


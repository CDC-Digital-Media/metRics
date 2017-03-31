# install.packages(c("jsonlite","plyr","httr","stringr","digest","base64enc"))
# install.packages("devtools")
#devtools::install_github("rstudio/addinexamples", type = "source")
#library(devtools)
# install_github("randyzwitch/RSiteCatalyst", ref="master")
library(RSiteCatalyst)


SCAuth(Sys.getenv("SC_ID"), Sys.getenv("SC_KEY"))
GetProps(reportsuite.id)


reportsuite.id <- "devcdc"
elements <- c("prop18", "prop46", "prop42")
metrics <- c("pageviews", "event1")

ranked <- QueueRanked(reportsuite.id, "2016-03-18", "2016-04-18", metrics, elements, segment.id = "s570_56e706c2e4b007118bee364d")
ranked_cleaned <- subset(ranked, select = -c(segment.id, segment.name))


write.table(ranked_cleaned, "r_ab.txt", quote = FALSE, sep = ",", row.names = FALSE)

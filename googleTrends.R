#Changes in Google Trends API
# Due to recent changes to Google Trends API, the CRAN version of the package is no longer working. If you wan to continue to query Google Trends, you have to install the development version of the package. This will be soon deployed on CRAN.
# install.packages("gtrendsR")
# devtools::install_github("dvanclev/GTrendsR")
# devtools::install_github("trinker/gtrend")

devtools::install_github("PMassicotte/gtrendsR")
#save google credentials in GOOGLE_USER and GOOGLE_PASSWORD

library(gtrendsR)

# https://github.com/PMassicotte/gtrendsR

# gsession <- gconnect()
gtrends_categories <- data("categories")

gtrends(keyword = "epidemic", cat = "45") #Health

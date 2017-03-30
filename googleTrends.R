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

epidemic <- gtrends(keyword = "epidemic", cat = "45") #Health
word_word <- gtrends(keyword = "word word", cat = "45") #Health
a_s <- gtrends(keyword = "Abortion Surveillance", cat = "45") #Health
aah <- gtrends(keyword = "African American health", cat = "45") #Health
ax <- gtrends(keyword = "anxiety", cat = "45") #Health
ax_rq <- ax$related_queries
ax_rt <- ax$related_topics

ax_vb <- gtrends(keyword = )

bv <- gtrends(keyword = "bacterial vaginosis", cat = "45") #Health
last_one <- tail(epidemic$interest_over_time, 1)$hits


TrendsForTopic = function(topic) {
  return (gtrends(keyword = topic, cat = "45")) #Health
}
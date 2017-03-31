
#SocialMediaMineR is giving OAuth errors inside CDC network.  Works fine from Mac though.
library(SocialMediaMineR)

get_facebook("https://www.cdc.gov/")
get_facebook("https://www.cdc.gov/std/widgets/widget-3b.html")


library(Rfacebook)

# Do this once to save Facebook OAuth token
appId <- ""
secret <- ""
# token <- fbOAuth(appId, secret)
# save(token, file="fb_oauth")

load("fb_oauth")

#Searching for posts was deprecated with version 2.0 of the Facebook Graph API.
#searchFacebook("CDC", token = token, n=1000)

#Gets a reference to pages that mention CDC.  Mostly CDC pages.  Meh.
pages <- searchPages("CDC", token)

# SocialMediaLab won't install on my version of R due to the following errors
# there is no package called ‘slam’
# package ‘slam’ is not available (for R version 3.2.3)
install.packages("SocialMediaLab")
library(SocialMediaLab)

Authenticate("facebook", appID = appId, appSecret = secret)


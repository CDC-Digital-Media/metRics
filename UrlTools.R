CleanUrl <- function(url) {
  
  #remove common default pages
  url <- gsub("/default.html$", "", url)
  url <- gsub("/index.html$", "", url)
  url <- gsub("/default.htm$", "", url)
  url <- gsub("/index.htm$", "", url)
  url <- gsub("/default.asp$", "", url)
  url <- gsub("/default.aspx$", "", url)
  
  url <- gsub("http://www.cdc", "https://www.cdc", url) # switch from http to https for cdc.gov sites
  
  url <- gsub("/$", "", url) # remove any final trailing slash
  
  return (url)
}
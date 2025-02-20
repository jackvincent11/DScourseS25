
library(httr)

url <- "https://www.vizgr.org/historical-events/search.php"
params <- list(
  format = "json",
  begin_date = "00000101",
  end_date = "20240209",
  lang = "en"
)

response <- GET(url, query = params)
write(rawToChar(response$content), "dates.json")

system("cat dates.json")

library(jsonlite)
library(tidyverse)
mylist <- fromJSON("dates.json")
mydf <- bind_rows(mylist$result[-1])
class(mydf$date)
head(mydf, n = 10)

class(mydf$date)

head(mydf, n = 10)

install.packages("rvest")
library(rvest)
library(dplyr)

url <- "https://en.wikipedia.org/wiki/1500_metres#All-time_top_25"
webpage <- read_html(url)

tables <- webpage %>% html_nodes("table.wikitable")
men_top_25 <- tables[[2]] %>%
  html_table(fill = TRUE) 

head(men_top_25)

write.csv(men_top_25, "1500m_top_25.csv", row.names = FALSE)



install.packages("fredr")
library(fredr)

fredr_set_key("fe93f258b3eeb81c7b74bf95ef8bec57")
sp500_data <- fredr(series_id = "SP500", observation_start = as.Date("2020-01-01"))
head(sp500_data)
write.csv(sp500_data, "sp500_data.csv", row.names = FALSE)
tail(sp500_data)


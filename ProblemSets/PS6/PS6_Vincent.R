library(rvest)
library(dplyr)
library(ggplot2)

url <- "https://fbref.com/en/squads/822bd0ba/2021-2022/Liverpool-Stats#all_stats_standard"
webpage <- read_html(url)

liverpool_stats <- webpage %>%
  html_node(xpath = '//*[@id="all_stats_standard"]') %>%
  html_table(fill = TRUE)
head(liverpool_stats)

colnames(liverpool_stats)

colnames(liverpool_stats) <- c("Player", "Age", "Born", "Nation", "Pos", "Squad", "Comp", "Min", "Games", "Goals", 
                               "Assists", "xG", "xA", "xG+assists", "xG/90", "xA/90", "xG+xA/90", "Prog", "Prog/90", 
                               "Dist", "Shots", "Sho./90", "Shots/90", "Sh/SoT", "Sh/SoT%", "SoT%", "SoT/90", 
                               "KeyPasses", "KeyPass/90", "Passes", "Passes/90", "PassCmp%", "PassCmp/90", 
                               "PassDist", "Dribbles", "Dribbles/90", "DribbleSucc%", "DribbleSucc/90", "Tackles", 
                               "Tackles/90", "Interceptions", "Interceptions/90", "Blocks", "Blocks/90", "Clearances", 
                               "Clearances/90", "Errors", "Errors/90", "OwnGoals", "OwnGoals/90")
colnames(liverpool_stats)

liverpool_stats_clean <- liverpool_stats %>%
  select(Player, Goals, xG) %>%
  filter(!is.na(Goals) & !is.na(xG)) 

liverpool_stats_clean$Goals <- as.character(liverpool_stats_clean$Goals)
liverpool_stats_clean$xG <- as.character(liverpool_stats_clean$xG)

liverpool_stats_clean$Goals[liverpool_stats_clean$Goals == ""] <- NA
liverpool_stats_clean$xG[liverpool_stats_clean$xG == ""] <- NA

liverpool_stats_clean$Goals <- as.numeric(liverpool_stats_clean$Goals)
liverpool_stats_clean$xG <- as.numeric(liverpool_stats_clean$xG)

sum(is.na(liverpool_stats_clean$Goals))
sum(is.na(liverpool_stats_clean$xG))

liverpool_stats_clean <- liverpool_stats_clean %>%
  filter(!is.na(Goals) & !is.na(xG))
head(liverpool_stats_clean)

ggplot(liverpool_stats_clean, aes(x = xG, y = Goals)) +
  geom_point(aes(color = Player), size = 3) +
  labs(title = "Comparison of Expected Goals vs Goals Scored",
       x = "Expected Goals (xG)",
       y = "Goals Scored") +
  theme_minimal()
<<<<<<< HEAD
png("PS6a_Vincent.png", bg = "white")
=======
>>>>>>> 2543ed7b9e00dd15357ee8fe0d6b93dfba14af59
ggsave("PS6a_Vincent.png")

ggplot(liverpool_stats_clean, aes(x = xG, y = Goals)) +
  geom_point(aes(color = Player), size = 3) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) + # Add regression line
  labs(title = "Goals vs Expected Goals (xG)",
       x = "Expected Goals (xG)",
       y = "Goals Scored") +
  theme_minimal() +
  theme(legend.position = "none")
<<<<<<< HEAD
png("PS6b_Vincent.png", bg = "white")
=======
>>>>>>> 2543ed7b9e00dd15357ee8fe0d6b93dfba14af59
ggsave("PS6b_Vincent.png")

liverpool_stats_clean$Goals_minus_xG <- liverpool_stats_clean$Goals - liverpool_stats_clean$xG
ggplot(liverpool_stats_clean, aes(x = reorder(Player, Goals_minus_xG), y = Goals_minus_xG)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Difference Between Goals and Expected Goals (xG) by Player",
       x = "Player",
       y = "Goals - xG") +
  theme_minimal() +
  coord_flip()
<<<<<<< HEAD
png("PS6c_Vincent.png", bg = "white")
=======
>>>>>>> 2543ed7b9e00dd15357ee8fe0d6b93dfba14af59
ggsave("PS6c_Vincent.png")

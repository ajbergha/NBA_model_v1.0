#Scrape MVP Votes
#Libraries/datasets--------------------------
library(rvest)
library(dplyr)

#Create url slugs---------------

slugs <- c(1980:2017)

urls <- c()

for (slug in slugs) {
  temp <- paste0("https://www.basketball-reference.com/awards/awards_",
                 as.character(slug),".html")
  urls <- rbind(urls, temp)
  
}

#Scrape that bad boy-------------

html <- read_html(urls[1])
table <- data.frame(html_table(html, header=TRUE)[1])


mvp_vote_df <- data.frame()

for (i in 1:length(slugs)) {
  
  html <- read_html(urls[i])
  table <- data.frame(html_table(html, header=TRUE)[1])
  
  table$end_year <- slugs[i]
  
  mvp_vote_df <- bind_rows(mvp_vote_df, table)
  
}

mvp_vote <- mvp_vote_df

#Data Management-------------------
names(mvp_vote) <- mvp_vote[1,]

mvp_vote <- mvp_vote[which(mvp_vote$Rank != "Rank"),]


mvp_vote$Age  <- as.numeric(mvp_vote$Age)
mvp_vote$First <- as.numeric(mvp_vote$First)
mvp_vote$`Pts Won` <- as.numeric(mvp_vote$`Pts Won`)
mvp_vote$`Pts Max` <- as.numeric(mvp_vote$`Pts Max`)
mvp_vote$Share <- as.numeric(mvp_vote$Share)
mvp_vote$G     <- as.numeric(mvp_vote$G)
mvp_vote$MP    <- as.numeric(mvp_vote$MP)
mvp_vote$PTS   <- as.numeric(mvp_vote$PTS)
mvp_vote$TRB   <- as.numeric(mvp_vote$TRB)
mvp_vote$AST   <- as.numeric(mvp_vote$AST)
mvp_vote$STL   <- as.numeric(mvp_vote$STL)
mvp_vote$BLK   <- as.numeric(mvp_vote$BLK)
mvp_vote$`FG%` <- as.numeric(mvp_vote$`FG%`)
mvp_vote$`3P%` <- as.numeric(mvp_vote$`3P%`)
mvp_vote$`FT%` <- as.numeric(mvp_vote$`FT%`)
mvp_vote$WS    <- as.numeric(mvp_vote$WS)
mvp_vote$`WS/48` <- as.numeric(mvp_vote$`WS/48`)
mvp_vote$`1980`  <- as.numeric(mvp_vote$`1980`)

mvp_vote <- mvp_vote %>% 
  rename(end_year = `1980`,
         ws_48 = `WS/48`,
         fg_perc = `FG%`,
         three_perc = `3P%`,
         ft_perc = `FT%`,
         mvp_pts = `Pts Won`,
         mvp_pts_max = `Pts Max`)

mvp_vote$start_year <- mvp_vote$end_year - 1

mvp_vote$Rank <- sub("T", "",mvp_vote$Rank)
mvp_vote$Rank <- as.numeric(mvp_vote$Rank)

write.csv(mvp_vote, "C:/Users/berghammeraj/Desktop/Projects/NBA/Data/mvp_vote.csv")
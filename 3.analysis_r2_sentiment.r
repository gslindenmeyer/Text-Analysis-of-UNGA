# Name: Guilherme Schultz Lindenmeyer
# Date: 10.11.2023
# Lecture: Programming Course for Economists
# Purpose: Do the second analysis of the project 

# clean environment
rm(list = ls())
gc()
cat("\014")
## define the working directory
setwd("D:/GDrive/Faculdade/Mestrado/3.WS 2023/Lecture - Programming for Economists/Assignment")
getwd()

# relevant packages
packages <- c("tidyverse", "tm", "slam", "rvest", "xml2", "stringdist", "countrycode", "quanteda",
              "SnowballC", "ggformula", "ggpubr", "scales", "readxl" , "stopwords", "wordcloud",
              "syuzhet", "tidytext", "gridExtra", "ggstream")

# check if installed, otherwise, install, omit warnings
for(p in packages){
  if(!require(p, character.only = TRUE)){
    suppressMessages(suppressWarnings(install.packages(p)))
    library(p, character.only = TRUE)
  }
}

## set seed
set.seed(1000)

## let's do the second analysis: 
## (2) Sentiment analysis of the speeches from each region over time. In question format: 
## is there any difference between the sentiment of the speeches from each region? 
## And is there any variation over time? 

## for this, let's do the analysis for one continent then, once it's set up, we can do it for all of them

load("data/corpus/all.rda")

continents <- unique(mydata$continent)
years <- unique(mydata$year)

## let's do a dataframe. Columns: year, continent, anger, anticipation, disgust, fear, joy, sadness, surprise, trust, negative, positive
## rows: 50*5 = 250 rows
sentiment_matrix <- data.frame(matrix( ncol = 13))
colnames(sentiment_matrix) <- c("year", "continent", "anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust", "negative", "positive","total")

i <- 1
t <- years[1]

for(i in 1:length(continents)) { 
  for(t in years){
    temp.data <- mydata %>% filter(continent == continents[i]) %>% filter(year == t)
  
    ## let's do the sentiment analysis
    sentiment.df <- get_nrc_sentiment(temp.data$text)
    
    ## do a median aggregation
    
    summ.sentiment.df <- sentiment.df %>% summarise_all(sum)
    summ.sentiment.df$total <- sum(summ.sentiment.df)
    summ.sentiment.df$year <- t
    summ.sentiment.df$continent <- continents[i]
    sentiment_matrix <- rbind(sentiment_matrix, summ.sentiment.df)
    
    }
}
## drop NA
sentiment_matrix <- sentiment_matrix %>% drop_na()
sentiment_matrix$year <- as.numeric(sentiment_matrix$year)
## let's save sentiment_matrix
save(sentiment_matrix, file = "data/sentiment_matrix.rda")


#clear
rm(list = ls())
load("data/sentiment_matrix.rda")


## now let's produce the loop
continents <- unique(sentiment_matrix$continent)
i <- continents[1]
# Loop over each continent
for(i in continents) {
  # Filter the data for the specified continent
  continent_data <- subset(sentiment_matrix, continent == i)
  
  # Drop continent column and total column
  pos_neg_continent_data <- continent_data %>% select(year, positive, negative)
  sentiment_continent_data <- continent_data %>% select(year, anger, anticipation, disgust, fear, joy, sadness, surprise, trust)
  
  # Reshape the data into long format for plotting
  pos_neg_continent_data <- reshape2::melt(pos_neg_continent_data, id.vars="year")
  sentiment_continent_data <- reshape2::melt(sentiment_continent_data, id.vars="year")
  
  # Define custom colors
  cols8 <- hcl.colors(8, palette = "viridis", alpha = 0.8)
  cols2 <- hcl.colors(2, palette = "Blue-Red 3", alpha = 0.7)
  
  # Create the sentiment proportion plot
  plot1 <- ggplot(sentiment_continent_data, aes(x = year, y = value, fill = variable)) +
    geom_stream(type = "proportional") +
    #geom_stream_label(aes(label = variable)) +
    theme_minimal() +
    labs(title = paste0("Sentiment for ", i),
         x = "Year",
         y = "Value") +
    theme(plot.title = element_text(size = 14, face = "bold")) +
    #theme(legend.position = "none") +
    scale_fill_manual(values = cols8)
  
  # Create the positive/negative proportion plot
  plot2 <- ggplot(pos_neg_continent_data, aes(x = year, y = value, fill = variable)) +
    geom_stream(type = "proportional") +
    #geom_stream_label(aes(label = variable)) +
    theme_minimal() +
    labs(title = paste0("Aggregated for ", i),
         x = "Year",
         y = "Value") +
    theme(plot.title = element_text(size = 14, face = "bold")) +
    #theme(legend.position = "none") +
    scale_fill_manual(values = cols2)
  
  # Arrange the plots side by side
  combined_plots <- grid.arrange(plot1, plot2, ncol = 2)
  
  # Save the combined plots to files
  path <- paste0("results/analysis/r2/", i, "_plots.pdf")
  #path2 <- paste0("results/analysis/r2/", i, "_plots.eps")
  ggsave(path, combined_plots, width = 8, height = 2.5)
  #ggsave(path2, combined_plots, width = 7.5, height = 3)
}








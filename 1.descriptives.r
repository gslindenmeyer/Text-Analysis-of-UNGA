# Name: Guilherme Schultz Lindenmeyer
# Date: 31.10.2023
# Lecture: Programming Course for Economists
# Purpose: Prepare some descriptives for the project

# clean environment
rm(list = ls())
gc()
cat("\014")
## define the working directory
setwd("D:/GDrive/Faculdade/Mestrado/3.WS 2023/Lecture - Programming for Economists/Project")
getwd()

# relevant packages
packages <- c("tidyverse", "tm", "slam", "rvest", "xml2", "stringdist", "countrycode", "quanteda",
              "SnowballC", "ggformula", "ggpubr", "scales", "readxl" , "stopwords", "wordcloud" )

# check if installed, otherwise, install, omit warnings
for(p in packages){
  if(!require(p, character.only = TRUE)){
    suppressMessages(suppressWarnings(install.packages(p)))
    library(p, character.only = TRUE)
  }
}

## read the project_corpus.rda
load("data/corpus/all.rda")

## let's do some descriptives: 

## create a column which is number of unique country per year
mydata$unique_country <- NA
for(i in 1:nrow(mydata)){
  mydata$unique_country[i] <- length(unique(mydata$country[mydata$year == mydata$year[i]]))
}
## let's create a vector of unique countries: for each year, take the first unique country 
vector_unique_countries <- c()
for(i in 1973:2022){
  vector_unique_countries <- c(vector_unique_countries, mydata$unique_country[mydata$year == i][1])
}
## do a frame of how many unque countries per year from 1973 to 2022
table <- data.frame(Years = 1973:2022, Countries = vector_unique_countries)
table                    

## plot 
ggplot(table, aes(x = Years, y = Countries)) + geom_line(color = '#4C72B0') + 
  labs(title = "Countries at the UNGA (1973-2022)", x = "Years", y = "Number of countries") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank()) + 
  #theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme(axis.text = element_text(size = 12)) + 
  theme(axis.title = element_text(size = 14)) + 
  theme(plot.title = element_text(size = 16, face = "bold")) + 
  theme(legend.position = "none") + 
  theme(plot.margin = unit(c(1,1,1,1), "cm")) + 
  theme(panel.grid.major = element_line(colour = "grey", size = 0.5)) + 
  theme(panel.grid.minor = element_line(colour = "grey", size = 0.5)) + 
  theme(axis.ticks.length = unit(0.1, "cm")) + 
  theme(axis.ticks.margin = unit(0.1, "cm")) 

# save as pdf
ggsave("results/countries.eps", width = 6, height = 4, units = "in")

## let's now create a data frame with the number of speeches per region per year
speeches_region <- data.frame(year = 1973:2022, africa = NA, asia = NA, europe = NA, americas = NA, oceania = NA)

for(i in 1973:2022){
  speeches_region$africa[speeches_region$year == i] <- length(unique(mydata$country[mydata$year == i & mydata$continent == "Africa"]))
  speeches_region$asia[speeches_region$year == i] <- length(unique(mydata$country[mydata$year == i & mydata$continent == "Asia"]))
  speeches_region$europe[speeches_region$year == i] <- length(unique(mydata$country[mydata$year == i & mydata$continent == "Europe"]))
  speeches_region$americas[speeches_region$year == i] <- length(unique(mydata$country[mydata$year == i & mydata$continent == "Americas"]))
  speeches_region$oceania[speeches_region$year == i] <- length(unique(mydata$country[mydata$year == i & mydata$continent == "Oceania"]))
}

print(speeches_region)

## palette
color_palette <- c("darkblue", "#EA922F", "#55A868", "#C44E52", "#4C72B0")
## plot
ggplot(speeches_region, aes(x = year, y = africa)) +
  geom_line(aes(y = asia, color = "Asia"), size = 1) +
  geom_line(aes(y = europe, color = "Europe"), size = 1) +
  geom_line(aes(y = americas, color = "Americas"), size = 1) +
  geom_line(aes(y = oceania, color = "Oceania"), size = 1) +
  geom_line(aes(color = "Africa"), size = 1) +
  # Add labels
  labs(title = "Speeches per Region at UNGA (1973-2022)",
       x = "Years",
       y = "Number of Speeches") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank()) + 
  #theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme(axis.text = element_text(size = 12)) + 
  theme(axis.title = element_text(size = 14)) + 
  theme(plot.title = element_text(size = 16, face = "bold")) + 
  theme(legend.position = "none") + 
  theme(plot.margin = unit(c(1,1,1,1), "cm")) + 
  theme(panel.grid.major = element_line(colour = "grey", size = 0.5)) + 
  theme(panel.grid.minor = element_line(colour = "grey", size = 0.5)) + 
  theme(axis.ticks.length = unit(0.1, "cm")) + 
  theme(axis.ticks.margin = unit(0.1, "cm")) +
# Customize legend
  theme(legend.title = element_blank(), legend.position="bottom", 
        legend.background = element_rect(fill = "white", color = "black")) +
  scale_color_manual(values = color_palette, 
                     labels = c("Africa", "Asia", "Europe", "Americas", "Oceania")) +
  # Adjust plot margins
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  # Customize panel border
  # Customize grid lines
  theme(panel.grid.major = element_line(color = "grey", size = 0.5)) +
  theme(panel.grid.minor = element_line(color = "grey", size = 0.5))

# save as pdf
ggsave("results/speeches_region.eps", width = 6, height = 4, units = "in")



## text quantification

# word counts per document
mydata.wordcounts <- as.data.frame(apply(dtm1,1,sum))
colnames(mydata.wordcounts) <- "totalWords"
mydata.wordcounts["uniqueWords"] <- apply(dtm1,1,Matrix::nnzero)
## let's add year column, which are the numbers in the ID
mydata.wordcounts["year"] <- readr::parse_number(rownames(mydata.wordcounts))
## let's add country column, which are all the letters in the id
mydata.wordcounts["country"] <- gsub("[^[:alpha:]]", "", rownames(mydata.wordcounts))

## simple summary statistics
summary(mydata.wordcounts)


# word counts per unigram
mydata.termcounts <- as.data.frame(apply(dtm1,2,sum)) # 2: the column
colnames(mydata.termcounts) <- "freq"


## create empty data frame 
words <- data.frame(year = 1973:2022, bottom1 = NA, top1 = NA, mean = NA, median = NA)

## fill the data frame
for(i in 1973:2022){
  words$bottom1[words$year == i] <- quantile(mydata.wordcounts$totalWords[mydata.wordcounts$year == i], 0.01)
  words$top1[words$year == i] <- quantile(mydata.wordcounts$totalWords[mydata.wordcounts$year == i], 0.99)
  words$mean[words$year == i] <- mean(mydata.wordcounts$totalWords[mydata.wordcounts$year == i])
  words$median[words$year == i] <- median(mydata.wordcounts$totalWords[mydata.wordcounts$year == i])
}

# Create a color palette
color_palette <- c("#EA922F", "#2765A1", "grey", "#55A868")


# Plot the data in a fancy ggplot
ggplot(words, aes(x = year, y = mean)) +
  geom_line(aes(y = top1, color = "Top 1%"), size = 1) +
  geom_line(aes(color = "Mean"), size = 1) +
  geom_line(aes(y = median, color = "Median"), size = 1) +
  geom_line(aes(y = bottom1, color = "Bottom 1%"), size = 1) +
  # Add labels
  labs(title = "Words per Speech at UNGA (1973-2022)",
       x = "Years",
       y = "Number of Words") +
  # Customize theme
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank()) + 
  #theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme(axis.text = element_text(size = 12)) + 
  theme(axis.title = element_text(size = 14)) + 
  theme(plot.title = element_text(size = 16, face = "bold")) + 
  theme(legend.position = "none") + 
  theme(plot.margin = unit(c(1,1,1,1), "cm")) + 
  theme(panel.grid.major = element_line(colour = "grey", size = 0.5)) + 
  theme(panel.grid.minor = element_line(colour = "grey", size = 0.5)) + 
  theme(axis.ticks.length = unit(0.1, "cm")) + 
  theme(axis.ticks.margin = unit(0.1, "cm")) +
  theme(legend.title = element_blank(), legend.position = c(0.98, 0.98), legend.justification = c(1, 1),
        legend.background = element_rect(fill = "white", color = "black")) +
  scale_color_manual(values = color_palette, 
                     labels = c("Bottom 1%", "Mean", "Median", "Top 1%")) +
  # Adjust plot margins
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  # Customize panel border
  # Customize grid lines
  theme(panel.grid.major = element_line(color = "grey", size = 0.5)) +
  theme(panel.grid.minor = element_line(color = "grey", size = 0.5))

# save as pdf
ggsave("results/words_cleaned.eps", width = 6, height = 4, units = "in")

## number of unique countries in 1973
## highest number of unique countries 
length(unique(mydata.wordcounts$country[mydata.wordcounts$year == 1973])) ## 120
length(unique(mydata.wordcounts$country[mydata.wordcounts$year == 2017])) ## 2017 and 2018, with 196 countries

# type-token ration
# (total number of different words divided by total number of words)

ttratio <- mydata$text %>%
  tokens() %>%
  quanteda.textstats::textstat_lexdiv(measure = c("TTR"))

mydata.wordcounts <- cbind(mydata.wordcounts,ttratio$TTR)
mydata.wordcounts["ttratio2"] <- mydata.wordcounts$uniqueWords/mydata.wordcounts$totalWords

summary(mydata.wordcounts)

text_frame <- data.frame(totalWords = mydata.wordcounts$totalWords, uniqueWords = mydata.wordcounts$uniqueWords, 
                         TTR =  mydata.wordcounts$`ttratio$TTR`, TTR2 = mydata.wordcounts$ttratio2)
#cor(text_frame, use = "pairwise.complete.obs")

# readability
scores <- quanteda.textstats::textstat_readability(mydata$text,
                                                   measure=c("Flesch.Kincaid", "Flesch"),
                                                   #min_sentence_length= 1,
                                                   #max_sentence_length= 10000,
                                                   remove_hyphens=FALSE,
                                                   intermediate=F)
text_frame <- cbind(text_frame,scores[,c("Flesch.Kincaid","Flesch")])

cor(text_frame, use = "pairwise.complete.obs")

### let's create some word clouds

wordcloud(rownames(mydata.termcounts), 
          freq = mydata.termcounts$freq,
          scale=c(2, .75),
          min.freq = 50,max.freq=200, max.words = 100,
          random.order = F,
          colors = brewer.pal(10,"Dark2"))

# save as pdf
dev.copy2pdf(file = "results/wordcloud.pdf")


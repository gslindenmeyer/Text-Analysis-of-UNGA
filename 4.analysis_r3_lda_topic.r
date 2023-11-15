# Name: Guilherme Schultz Lindenmeyer
# Date: 14.11.2023
# Lecture: Programming Course for Economists
# Purpose: Do the third analysis of the project 

# clean environment
rm(list = ls())
gc()
cat("\014")
## define the working directory
setwd("D:/GDrive/Faculdade/Mestrado/3.WS 2023/Lecture - Programming for Economists/Assignment")
getwd()

# relevant packages
packages <- c("tidyverse", "tm", "slam", "rvest", "xml2", "stringdist", "countrycode", "quanteda", "corrplot",
              "SnowballC", "ggformula", "ggpubr", "scales", "readxl" , "stopwords", "wordcloud", "fixest",
              "syuzhet", "tidytext", "gridExtra", "ggstream", "topicmodels", "data.table", "seededlda")

# check if installed, otherwise, install, omit warnings
for(p in packages){
  if(!require(p, character.only = TRUE)){
    suppressMessages(suppressWarnings(install.packages(p)))
    library(p, character.only = TRUE)
  }
}

## set seed
set.seed(1000)

## let's do the third analysis: 
## (3) Analysis of the distribution of topics being covered 
## in each region over time. In other words, what are the regions talking about the most? 
## for this, let's do the analysis for one continent then, once it's set up, we can do it for all of them

## since it's a topic analysis, let's use the stemmed corpus
load("data/corpus/stemmed/all_stemmed.rda")

continents <- unique(mydata$continent)
years <- unique(mydata$year)

corp2 <- corpus(
  mydata,
  docid_field = "doc_id",
  text_field = "text",
  meta = list(),
  unique_docnames = TRUE)

toks <- tokens(corp2, remove_punct = TRUE, remove_symbols = TRUE, remove_number = TRUE)
# Apply stemming to the tokens
toks_stemmed <- tokens_wordstem(toks, language = "english")

dfmt <- dfm(toks_stemmed) %>%
  dfm_remove(stopwords("en"), min_nchar = 2) %>%
  dfm_trim(max_docfreq = 0.1, docfreq_type = "prop")


mdg_dict <- dictionary(list(
  MDG1 = c("poverty", "hunger", "starvation", "malnutrition", "deprivation", "famine", "undernourished", "employment", "income", "indigence", "extreme poverty", "nutrition", "food security"),
  MDG2 = c("education", "schooling", "literacy", "dropout", "teachers", "primary","access", "scholarship", "academia", "learning", "basic"),
  MDG3 = c("gender", "equality", "empowerment", "women", "parity", "female", "girls", "discrimination", "equity", "gender disparity", "rights", "mainstreaming"),
  MDG4 = c("child", "infant", "pediatric", "survival", "neonatal", "under-five", "kids"),
  MDG5 = c("maternal", "maternity", "antenatal", "postnatal", "childbirth", "maternal mortality", "pregnancy", "birth", "reproductive health", "fertility", "midwifery", "motherhood", "care"),
  MDG6 = c("disease", "HIV", "AIDS", "malaria", "tuberculosis", "epidemic", "healthcare", "vaccine", "virus", "infection", "disease","prevention", "HIV", "public", "malaria"),
  MDG7 = c("environmental", "climate", "sustainability", "sanitation", "water", "renewable", "energy", "conservation", "biodiversity", "ecology", "natural", "resources", "clean","water", "sustainable", "development", "sanitation","access"),
  MDG8 = c("global partnership", "trade", "economic","development", "aid", "debt", "technology","transfer", "financial","system", "LDCs", "landlocked", "states", "affordable", "private","sector", "information","technology"),
  Conflict_and_War = c("war", "conflict", "military", "violence", "terrorism", "battle", "soldier", "weapons", "nuclear", "armament")
))

# Function to stem a vector of words
stem_words <- function(words) {
  sapply(words, wordStem, language = "en")
}

# Apply the stem_words function to each element of the mdg_dict
mdg_dict_stemmed <- dictionary(lapply(mdg_dict, stem_words))

# View the stemmed dictionary
print(mdg_dict_stemmed)


## let's see how long it takes to run
start <- Sys.time()
lda_seed <- textmodel_seededlda(dfmt, mdg_dict_stemmed, residual = 2, auto_iter = T)
end <- Sys.time()
end - start

terms(lda_seed)

###
topic.dist <- data.frame(lda_seed$theta)
topic.dist$doc_id <- row.names(topic.dist)

all.topic.dist <- merge(topic.dist, mydata, by = "doc_id", suffixes = c("",""))

## save
save(all.topic.dist, file = "data/all.topic.dist.rda")

###### don't need to run above

# load data
load("data/all.topic.dist.rda")

## let's aggregate into a matrix: 

topic_matrix <- data.frame(matrix( ncol = 13))
colnames(topic_matrix) <- c("year", "continent", "MDG1", "MDG2", "MDG3", "MDG4", "MDG5", "MDG6", 
                                "MDG7", "MDG8", "Conflict_and_War", "other1","other2")

continents <- all.topic.dist$continent %>% unique()
years <- all.topic.dist$year %>% unique()
i <- 1
t <- years[1]

for(i in 1:length(continents)) { 
  for(t in years){
    temp.data <- all.topic.dist %>% filter(continent == continents[i]) %>% filter(year == t)
    temp.data <- temp.data %>% select(colnames(topic_matrix))
    ## do a median aggregation
    
    median.distribution.df <- temp.data %>% summarise_all(median)
    
    topic_matrix <- rbind(topic_matrix, median.distribution.df)
    
  }
}

## drop NA
topic_matrix <- topic_matrix %>% drop_na()
topic_matrix$year <- as.numeric(topic_matrix$year)
## let's save sentiment_matrix
save(topic_matrix, file = "data/topic_matrix.rda")


# List of continents including "ALL" for the combined plot
continents <- c("Africa", "Americas", "Asia", "Europe", "Oceania", "All")
i <- "All"
# Loop over each continent and "ALL"
for(i in continents) {
  # Check if the current iteration is for all continents
  if(i == "All") {
    continent_data <- topic_matrix %>% select(-continent)
    ## let's do an average over the 5 continents
    continent_data <- continent_data %>% group_by(year) %>% summarise_all(mean)
  } else {
    # Filter the data for the specified continent
    continent_data <- subset(topic_matrix, continent == i)
  }
  
  # Select relevant columns and reshape data
  if(i == "All"){
    continent_data <- continent_data %>% select(-c(other1, other2))
  } else {
    continent_data <- continent_data %>% select(-c(continent, other1, other2))
  }
  continent_data <- reshape2::melt(continent_data, id.vars="year")
  
  # Define nice colors
  cols <- hcl.colors(9, palette = "Plasma", alpha = 0.8)
  
  # Create the plot
  plot <- ggplot(continent_data, aes(x = year, y = value, fill = variable)) +
    geom_stream(type = "proportional") +
    #geom_stream_label(aes(label = variable)) +
    theme_minimal() +
    labs(title = paste0("Topics for ", i),
         x = "Year",
         y = "Value") +
    theme(plot.title = element_text(size = 14, face = "bold"))+
          #legend.position = "none") +
    scale_fill_manual(values = cols)
  
  # Save the plot as a PDF
  path <- paste0("results/analysis/r3/", i, "_plot.pdf")
  ggsave(path, plot, width = 5.5, height = 3)
}




## let's import the world bank gdp growth data: 
gdp_growth <- read.csv("data/worldbank/data_wb.csv", stringsAsFactors = F)
head(gdp_growth,5)

## the dataset is ugly, we want to format it in a way that we can combine it to the all.topic.dist dataset


# let's write some NAs
gdp_growth <- gdp_growth %>%
  mutate(across(starts_with("X"), ~na_if(.x, "..")))

# fix year columns
gdp_long <- gdp_growth %>%
  gather(key = "year", value = "gdp_growth", starts_with("X")) %>%
  mutate(
    year = as.numeric(sub(".*\\.YR(\\d{4})\\.", "\\1", year)), # Extract the year from the column names
    gdp_growth = as.numeric(gdp_growth) # Convert gdp_growth to numeric
  ) #%>%
  #filter(!is.na(gdp_growth)) # Remove rows with NAs in gdp_growth

# Select and rename the columns to match the desired format
panel_data <- gdp_long %>%
  select(year, Country.Name, Country.Code, gdp_growth) %>%
  rename(country = Country.Name, country_code = Country.Code)

panel_data$year <- as.character(panel_data$year)

str(all.topic.dist, 5)

merged_data <- merge(panel_data, all.topic.dist, by.x = c("year", "country_code"), by.y = c("year", "countrycode_fix"), all = TRUE)

## delete merged_data empty doc_id 
merged_data <- merged_data %>% filter(!is.na(doc_id))

# save
save(merged_data, file = "data/regression_merged_data.rda")
merged_data$gdp_growth_rate <- merged_data$gdp_growth/100 + 1
## let's do some correlations
corr_matrix <- merged_data %>% select(MDG1, MDG2, MDG3, MDG4, MDG5, MDG6, MDG7, MDG8, Conflict_and_War, gdp_growth_rate) %>% cor(use  = "complete")

# plot correlation matrix
corrplot(corr_matrix, method = "color", type = 'lower',
         col = colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))(200),
         tl.col = "black", tl.srt = 45, tl.cex = 0.7)

# Add a title
title("Correlation Matrix")

## let's run a regression of gdp growth on topic distribution

# let's transform variables in log
merged_data$log_gdp_growth <- log(merged_data$gdp_growth/100 + 1)
merged_data$log_MDG1 <- log(merged_data$MDG1 + 1)
merged_data$log_MDG2 <- log(merged_data$MDG2 + 1)
merged_data$log_MDG3 <- log(merged_data$MDG3 + 1)
merged_data$log_MDG4 <- log(merged_data$MDG4 + 1)
merged_data$log_MDG5 <- log(merged_data$MDG5 + 1)
merged_data$log_MDG6 <- log(merged_data$MDG6 + 1)
merged_data$log_MDG7 <- log(merged_data$MDG7 + 1)
merged_data$log_MDG8 <- log(merged_data$MDG8 + 1)
merged_data$log_Conflict_and_War <- log(merged_data$Conflict_and_War + 1)

model1 <- feols(log_gdp_growth ~ log_MDG1 + log_MDG2 + log_MDG3 + log_MDG4 + log_MDG5 + log_MDG6 + log_MDG7 + log_MDG8 + log_Conflict_and_War | year + continent, data = merged_data, cluster = "continent")
model2 <- feols(log_gdp_growth ~ log_MDG1 + log_MDG2 + log_MDG3 + log_MDG4 + log_MDG5 + log_MDG6 + log_MDG7 + log_MDG8 + log_Conflict_and_War | year^continent, data = merged_data, cluster = "continent")
model3 <- model1 <- feols(log_gdp_growth ~ log_MDG1 + log_MDG2 + log_MDG3 + log_MDG4 + log_MDG5 + log_MDG6 + log_MDG7 + log_MDG8 + log_Conflict_and_War | year^region, data = merged_data, cluster = "continent")

etable(model1, model2, model3, digits = 2, tex=T, se.below = FALSE)


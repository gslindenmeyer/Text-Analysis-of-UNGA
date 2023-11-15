# Name: Guilherme Schultz Lindenmeyer
# Date: 05.11.2023
# Lecture: Programming Course for Economists
# Purpose: Do the first analysis of the project 

# clean environment
rm(list = ls())
gc()
cat("\014")
## define the working directory
setwd("D:/GDrive/Faculdade/Mestrado/3.WS 2023/Lecture - Programming for Economists/Project")
getwd()

# relevant packages
packages <- c("tidyverse", "tm", "slam", "rvest", "xml2", "stringdist", "countrycode", "quanteda", "ggrepel",
              "SnowballC", "ggformula", "ggpubr", "scales", "readxl" , "stopwords", "wordcloud" )

# check if installed, otherwise, install, omit warnings
for(p in packages){
  if(!require(p, character.only = TRUE)){
    suppressMessages(suppressWarnings(install.packages(p)))
    library(p, character.only = TRUE)
  }
}

## let's do the first analysis: 
## (1) Are the discourses of each country over the 50 years similar to each other? 
## for this, let's open the dataset of panel stemmed and do text similarity analysis

load("data/corpus/all_panel_stemmed.rda")

## let's use stringdist

# create a function to calculate the similarity between two texts
similarity <- function(x, y){
  stringdist(x, y, method = "jw", p = 0.1)
}

# test
similarity("hello", "hell3o")


dtm.m <- as.matrix(dtm1)
# years
years <- unique(mydata$year)

countries <- unique(mydata$country)

# begin loop
year <- years[1]
country <- countries[1]

# create a matrix to store the results, has 50 rows and 74 columns
# the rows are the years
# the columns are the countries

similarity.matrix.cosine <- matrix(NA, nrow = length(years), ncol = length(countries))
rownames(similarity.matrix.cosine) <- years
colnames(similarity.matrix.cosine) <- countries

# first year is zero for all countries
similarity.matrix.cosine[1,] <- 0
similarity.matrix.jaccard <- similarity.matrix.cosine


## let's do a loop over countries and inside a loop over years
years <- years[1:(length(years)-1)] # remove last year

for (country in countries){
  n_row <- 2
  for(year in years){
    ## id is country_year
    
    id <- paste0(country, "_", year)
    id2 <- paste0(country, "_", as.numeric(year)+1)
    ids <- c(id, id2)
    
    tmp.dtm <- dtm.m[ids,]
    tmp.dtm.m <- as.matrix(tmp.dtm)

    # cosine distance: proxy
    cosine.distance <- proxy::dist(tmp.dtm.m, method="cosine") # cosine distances matrix
    jaccard.distance <- proxy::dist(tmp.dtm.m, method="jaccard")
    
    # fill in the matrix, 
    
    similarity.matrix.cosine[n_row, country] <- cosine.distance[1]
    similarity.matrix.jaccard[n_row, country] <- jaccard.distance[1]
    
    n_row <- n_row + 1
  }
}

## let's plot all 74 lines in a fancy plot as curves: 
## we need to transform the matrix into a data frame
similarity.matrix.cosine.df <- as.data.frame(similarity.matrix.cosine)
similarity.matrix.cosine.df$year <- rownames(similarity.matrix.cosine.df)
similarity.matrix.cosine.df$year <- as.numeric(similarity.matrix.cosine.df$year)
similarity.matrix.cosine.df <- gather(similarity.matrix.cosine.df, key = "country", value = "similarity", -year)
## remove if distance is zero
similarity.matrix.cosine.df <- similarity.matrix.cosine.df[similarity.matrix.cosine.df$similarity != 0,]


similarity.matrix.jaccard.df <- as.data.frame(similarity.matrix.jaccard)
similarity.matrix.jaccard.df$year <- rownames(similarity.matrix.jaccard.df)
similarity.matrix.jaccard.df$year <- as.numeric(similarity.matrix.jaccard.df$year)
similarity.matrix.jaccard.df <- gather(similarity.matrix.jaccard.df, key = "country", value = "similarity", -year)
## remove if distance is zero
similarity.matrix.jaccard.df <- similarity.matrix.jaccard.df[similarity.matrix.jaccard.df$similarity != 0,]



## cosine distance, lowest 10 countries mean across years
low.cos <- similarity.matrix.cosine.df %>% 
  group_by(country) %>% 
  summarise(mean = mean(similarity)) %>% 
  arrange(mean) %>% 
  head(3)

## highest 1 country mean
high.cos <- similarity.matrix.cosine.df %>% 
  group_by(country) %>% 
  summarise(mean = mean(similarity)) %>% 
  arrange(desc(mean)) %>% 
  head(1)

## jacard distance, lowest 10 countries mean across years
low.jac <- similarity.matrix.jaccard.df %>% 
  group_by(country) %>% 
  summarise(mean = mean(similarity)) %>% 
  arrange(mean) %>% 
  head(3)

## highest 1 country mean
high.jac <- similarity.matrix.jaccard.df %>% 
  group_by(country) %>% 
  summarise(mean = mean(similarity)) %>% 
  arrange(desc(mean)) %>% 
  head(1)


ggplot(data = similarity.matrix.cosine.df, aes(x = year, y = similarity, group = country), ) +
  geom_line(aes(color = country %in% low.cos $country), size = 1) +
  geom_line(data = filter(similarity.matrix.cosine.df, country %in% low.cos$country),
            aes(color = country), size = 1) +  # Highlight the highest country
  # Highlight lowest 10 countries
  geom_line(data = filter(similarity.matrix.cosine.df, country %in% high.cos $country),
            aes(color = country), size = 1) +  # Highlight the highest country
  geom_text_repel(data = filter(similarity.matrix.cosine.df, year == max(year) & country %in% c(low.cos $country, high.cos $country)),
                  aes(label = country), nudge_x = 0.5, nudge_y = 0.02, 
                  size = 4, segment.color = NA) +  # Add labels to the lines
  scale_color_manual(values = c("#2765A1", "lightgrey", "#EA922F", "#2765A1", "#2765A1", "lightgrey")) +  # Colors for the lines
  theme_minimal() +
  labs(title = "Cosine Similarity Across Years",
       subtitle = "Highlighting the 3 countries with most similarity and the least one",
       x = "Year",
       y = "Cosine Similarity",
       color = "Country") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12))+ 
  theme(legend.position = "none")  # Hide the legend

## export as pdf
ggsave("results/analysis/r1/cosine_similarity.eps", width = 6, height = 4)



ggplot(data = similarity.matrix.jaccard.df, aes(x = year, y = similarity, group = country), ) +
  geom_line(aes(color = country %in% low.jac $country), size = 1) +
  geom_line(data = filter(similarity.matrix.jaccard.df, country %in% low.jac$country),
            aes(color = country), size = 1) +  # Highlight the highest country
  # Highlight lowest 10 countries
  geom_line(data = filter(similarity.matrix.jaccard.df, country %in% high.jac $country),
            aes(color = country), size = 1) +  # Highlight the highest country
  geom_text_repel(data = filter(similarity.matrix.jaccard.df, year == max(year) & country %in% c(low.jac $country, high.jac $country)),
                  aes(label = country), nudge_x = 0.5, nudge_y = 0.02, 
                  size = 4, segment.color = NA) +  # Add labels to the lines
  scale_color_manual(values = c("#2765A1", "#2765A1", "lightgrey", "#2765A1", "#EA922F", "lightgrey")) +  # Colors for the lines
  theme_minimal() +
  labs(title = "Jaccard Similarity Across Years",
       subtitle = "Highlighting the 3 countries with most similarity and the least one",
       x = "Year",
       y = "Jaccard Similarity",
       color = "Country") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12))+  # Adjust the text for better legibility

  theme(legend.position = "none")  # Hide the legend

ggsave("results/analysis/r1/jaccard_similarity.eps", width = 6, height = 4)

## plot only lowest 15 averages
similarity.matrix.cosine.df %>%
  group_by(country) %>%
  summarise(mean = mean(similarity, na.rm = TRUE)) %>%  # Make sure to remove NA values
  arrange(mean) %>%
  head(15) %>%
  ggplot(aes(x = reorder(country, mean), y = mean)) +
  geom_bar(stat = "identity", fill = 'steelblue') +  # Use a more visually appealing color
  theme_minimal() +
  labs(title = "Average in Time - Cosine Similarity",
       subtitle = "Lowest 15 Countries by Average Similarity",
       x = "Country",
       y = "Cosine Similarity") +
  ylim(0,0.5) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12))  # Adjust the text for better legibility

## export as pdf 
ggsave("results/analysis/r1/cosine_similarity_15.eps", width = 5, height = 4)

## plot only lowest 15 averages
similarity.matrix.jaccard.df %>%
  group_by(country) %>%
  summarise(mean = mean(similarity, na.rm = TRUE)) %>%  # Make sure to remove NA values
  arrange(mean) %>%
  head(15) %>%
  ggplot(aes(x = reorder(country, mean), y = mean)) +
  geom_bar(stat = "identity", fill = '#EA922F') +  # Use a more visually appealing color
  theme_minimal() +
  labs(title = "Average in Time - Jaccard Similarity",
       subtitle = "Lowest 15 Countries by Average Similarity",
       x = "Country",
       y = "Jaccard Similarity") +
  ylim(0,1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12))  # Adjust the text for better 
## export as pdf 
ggsave("results/analysis/r1/jaccard_similarity_15.eps", width = 5, height = 4)



## check how many countries are in common in the lowest 15 averages between cosine and jaccard: 
similarity.matrix.cosine.df %>% 
  group_by(country) %>% 
  summarise(mean = mean(similarity)) %>% 
  arrange(mean) %>% 
  head(15) %>% 
  inner_join(similarity.matrix.jaccard.df %>% 
               group_by(country) %>% 
               summarise(mean = mean(similarity)) %>% 
               arrange(mean) %>% 
               head(15), by = "country") 
### 11 countries

country.region <- mydata %>% 
  select(country, region) %>% 
  distinct()

country.continent <- mydata %>% 
  select(country, continent) %>% 
  distinct()
## check if these countries belong to the same region
similarity.matrix.cosine.df %>% 
  group_by(country) %>% 
  summarise(mean = mean(similarity)) %>% 
  arrange(mean) %>% 
  head(3) %>%  ## or tail
  inner_join(., country.region, by = "country") %>% 
  select(country, region) %>% 
  distinct() %>% 
  count(region) %>% 
  arrange(desc(n))

## check if these countries belong to the same continent
similarity.matrix.cosine.df %>% 
  group_by(country) %>% 
  summarise(mean = mean(similarity)) %>% 
  arrange(mean) %>% 
  head(15) %>% 
  inner_join(., country.continent, by = "country") %>% 
  select(country, continent) %>% 
  distinct() %>% 
  count(continent) %>% 
  arrange(desc(n))

## check if these countries belong to the same region
similarity.matrix.jaccard.df %>% 
  group_by(country) %>% 
  summarise(mean = mean(similarity)) %>% 
  arrange(mean) %>% 
  head(15) %>% 
  inner_join(., country.region, by = "country") %>% 
  select(country, region) %>% 
  distinct() %>% 
  count(region) %>% 
  arrange(desc(n))

## check if these countries belong to the same continent
similarity.matrix.jaccard.df %>% 
  group_by(country) %>% 
  summarise(mean = mean(similarity)) %>% 
  arrange(mean) %>% 
  head(15) %>% 
  inner_join(., country.continent, by = "country") %>% 
  select(country, continent) %>% 
  distinct() %>% 
  count(continent) %>% 
  arrange(desc(n))

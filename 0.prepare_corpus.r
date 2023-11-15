# Name: Guilherme Schultz Lindenmeyer
# Date: 29.10.2023
# Lecture: Programming Course for Economists
# Purpose: Prepare the dataset for the analysis

# clean environment
rm(list = ls())
gc()
cat("\014")
## define the working directory
setwd("D:/GDrive/Faculdade/Mestrado/3.WS 2023/Lecture - Programming for Economists/Project")
getwd()

# relevant packages
packages <- c("tidyverse", "tm", "slam", "rvest", "xml2", "stringdist", "countrycode",
              "SnowballC", "ggformula", "ggpubr", "scales", "readxl" , "stopwords" )

# check if installed, otherwise, install, omit warnings
for(p in packages){
  if(!require(p, character.only = TRUE)){
    suppressMessages(suppressWarnings(install.packages(p)))
    library(p, character.only = TRUE)
  }
}


### now we want to build the corpus
# path for the txt files
path <- "D:/GDrive/Faculdade/Mestrado/3.WS 2023/Lecture - Programming for Economists/Project/data/UN General Debate Corpus/df"

### adjusting the speaker dataset, excel file Speakers_by_session.xlsx
speakers <- read_excel("data/UN General Debate Corpus/speakers_by_session.xlsx")
colnames(speakers)[colnames(speakers)=="ISO Code"] <- "country_code"
colnames(speakers)[colnames(speakers)=="Name of Person Speaking"] <- "speaker"
colnames(speakers)[colnames(speakers)=="Post"] <- "position"

# save all folders in the path
folders <- list.dirs(path, recursive = FALSE)

## lets merge all files in folder in a dataframe
# create an empty dataframe, ## columns: year, UN (UN-GeneralAssembly number),country, speech, speaker, ID

df <- data.frame(year = numeric(),
                 UN = numeric(),
                 country = character(),
                 text = character(),
                 speaker = character(),
                 Position = character(),
                 doc_id = character(),
                 stringsAsFactors = FALSE)

# loop over all folders in path
for(folder in folders){
  # list all files in the folder
  files <- list.files(folder)
  
  # loop over all files in folder
  for(file in files){
    # read the file with utf8 enconding
    text <- readLines(paste0(folder, "/", file), encoding = "UTF-8")
    
    # get the year
    year <- str_extract(file, "\\d{4}")
    
    # get the UN number, it's the first 2 numbers in the file name
    UN <- str_extract(file, "\\d{1,2}")
    
    # get the country, the first 2 or 3 letters before the first _
    country <- str_extract(file, "[A-Z]{2,3}(?=_)")
    
    # get the speech
    speech <- paste(text, collapse = " ")
    
    # get the speaker, with is in the speaker dataset, by matching the year, session and country_code
    # sometimes is empty, then return NA
    speaker <- speakers %>% filter(Year == year, Session == UN, country_code == country) %>% select(speaker)
    if(nrow(speaker) == 0){
      speaker <- NA
    } 
    # get the position
    position <- speakers %>% filter(Year == year, Session == UN, country_code == country) %>% select(position)
    if(nrow(position) == 0){
      position <- NA
    } 
    # generate the ID, which is the combination of country code and year
    ID <- paste(country, year, sep = "_")
    
    # add the row to the dataframe
    df <- rbind(df, data.frame(year = year,
                               UN = UN,
                               country = country,
                               text = speech,
                               speaker = speaker,
                               position = position,
                               doc_id = ID,
                               stringsAsFactors = FALSE))
  }
}

mydata = df
# save the df as a rda file
save(mydata, file = "data/mydata.rda")

## clear
rm(list = ls())
gc()

## part 2
###### prepare corpus

## read the df
load("data/mydata.rda")


## let's now categorize each countrycode to a region, based on the UNO M49 classification
## https://unstats.un.org/unsd/methodology/m49/

# problematic codes: CSK, DDR, EU, PSE, YMD, YUG
## YMD is the same region as YEM
## DDR is the same as DEU
## CSK is the same as CZE
## YUG is the same as SRB
## EU is the same as DEU
## PSE is the same as ISR (needed for aggregation reasons)

## let's fix the problematic codes
mydata$countrycode_fix <- mydata$country
mydata$countrycode_fix[mydata$countrycode_fix == "YMD"] <- "YEM"
mydata$countrycode_fix[mydata$countrycode_fix == "DDR"] <- "DEU"
mydata$countrycode_fix[mydata$countrycode_fix == "CSK"] <- "CZE"
mydata$countrycode_fix[mydata$countrycode_fix == "YUG"] <- "SRB"
mydata$countrycode_fix[mydata$countrycode_fix == "EU"] <- "DEU"
mydata$countrycode_fix[mydata$countrycode_fix == "PSE"] <- "ISR"

mydata$continent <- countrycode(mydata$countrycode_fix, origin = "genc3c", destination = "un.region.name", warn = TRUE)
mydata$region <- countrycode(mydata$countrycode_fix, origin = "genc3c", destination = "un.regionsub.name", warn = TRUE)



## create a function that takes as input mydata and returns a corpus
gen_corpus <- function(mydata, stemming = F, path = "data/corpus/", name = "all"){
  corp <- VCorpus(DataframeSource(mydata), readerControl=list(language = "en"))
  corp <- tm_map(corp, content_transformer(function(x) {iconv(enc2utf8(x), sub = "byte")}))
  corp <- tm_map(corp, content_transformer(tolower))
  corp <- tm_map(corp, removeWords, stopwords("en", source = "smart") )
  corp <- tm_map(corp, content_transformer(function(x) {stringr::str_replace_all(x,"[^[:alnum:]]", " ")}))
  corp <- tm_map(corp, removeNumbers)
  corp <- tm_map(corp, removeWords, letters)
  corp <- tm_map(corp, removeWords, tolower(as.roman(1:200)))
  corp <- tm_map(corp, stripWhitespace)
  if(stemming == T){
    corp <- tm_map(corp, stemDocument, language="en")
  }
  ## CONSTRUCT DTMs
  
  # UNI-GRAMS: term frequencies
  dtm1 <- DocumentTermMatrix(corp)
  dim(dtm1)
  dtm1 <- dtm1[slam::row_sums(dtm1) > 0, ]; dim(dtm1)
  
  # BI-GRAMS
  BigramTokenizer <-
    function(x) {unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)}
  
  dtm2 <- DocumentTermMatrix(corp, control = list(tokenize = BigramTokenizer))
  dim(dtm2)
  dtm2 <- dtm2[slam::row_sums(dtm2) > 0, ]; dim(dtm2)
  ## saved if it's stemmed or not
  if(stemming == T){
    save(corp,
         dtm1,
         dtm2,
         mydata,
         file=file.path(paste0(path, name, "_stemmed.rda"))) 
  } else{ 
    save(corp,
         dtm1,
         dtm2,
         mydata,
         file=file.path(paste0(path, name, ".rda"))) 
    return(corp) }
}

## test the function
corp <- gen_corpus(mydata, stemming = F, path = "data/corpus/", name = "all")

#### loop all over the regions
regions <- unique(mydata$region)

for(i in 1:length(regions)){
  region <- regions[i]
  print(region)
  mydata_region <- mydata[mydata$region == region, ]
  corp <- gen_corpus(mydata_region, stemming = T, path = "data/corpus/", name = region)
}

continents <- unique(mydata$continent)

for(i in 1:length(continents)){
  continent <- continents[i]
  print(continent)
  mydata_continent <- mydata[mydata$continent == continent, ]
  corp <- gen_corpus(mydata_continent, stemming = T, path = "data/corpus/", name = continent)
}



# create a panel of the data from mydata, i.e. keep only countries that have data for all years

# get the unique countries
countries <- unique(mydata$country)
panel <- mydata
tab <- table(panel$country, panel$year)
countries <- rownames(tab)[rowSums(tab) == 50] # countries that have data for all years
panel <- panel[panel$country %in% countries, ]
###### prepare corpus
mydata <- panel # rename the dataframe to keep consistency
corp <- gen_corpus(mydata, stemming = F, path = "data/corpus/", name = "all_panel")
corp <- gen_corpus(mydata, stemming = T, path = "data/corpus/", name = "all_panel")

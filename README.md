# Programming for Economists Project

This repository contains the code and documentation for the "Programming for Economists" course project from the Master in Economics program at the University of Mannheim. The project focuses on text analysis of United Nations General Assembly (UNGA) speeches from 1973 to 2022, 50 years.

## Project Overview

The project aims to answer three research questions related to UNGA speeches:

1. **Are the discourses of each country over the 50 years similar to each other?**
   - This question explores the similarity of discourses among the 74 countries that have delivered speeches over the 50-year period.

2. **Sentiment analysis of the speeches from each region over time.**
   - This question investigates the sentiment of speeches from different regions and examines whether there are variations over time.

3. **Analysis of the distribution of topics covered in each region over time.**
   - This question focuses on understanding the topics that are most frequently discussed in different regions over the years, using Latent Dirichlet Allocation (LDA) for topic analysis.

## Project Structure

The project is organized into several R scripts to address each aspect of the analysis:

- `0.prepare_corpus.r`: Preprocessing and cleaning of the UNGA speech corpus.
- `1.descriptives.r`: Descriptive analysis of the dataset, providing insights into the data.
- `2.analysis_r1_distances.r`: Analysis of the similarity of discourses among countries.
- `3.analysis_r2_sentiment.r`: Sentiment analysis of speeches from different regions.
- `4.analysis_r3_lda_topic.r`: Analysis of topic distribution in different regions over time using Latent Dirichlet Allocation (LDA).

## Getting Started

To run and replicate this project, you will need the following:

- R
- Required R packages (listed in the code and available in the `packages` vector)

Run the R scripts in the specified order to perform the analysis. You will have to create the corpus from 0.prepare_corpus. The raw data is included

## Project Documentation

- The code files contain comments and explanations to guide you through the analysis steps.
- Additional project documentation, including the project proposal and any research findings, can be found in the repository.

## Data

The text input is collected from: 
Jankin, Slava; Baturo, Alexander; Dasandi, Niheer, 2017, "United Nations General Debate Corpus 1946-2022", https://doi.org/10.7910/DVN/0TJX8Y, Harvard Dataverse, V11

The GDP growth from 1973 to 2022 is collected from: 
World Bank national accounts data, and OECD National Accounts data files.

The World Bank panel is collected from World Bank API.

## Author
- Guilherme Schultz Lindenmeyer

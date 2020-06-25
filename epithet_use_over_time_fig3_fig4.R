### PROPORTION SEARCH RESULTS OVER TIME ###

## adapted from 
# Jones, T.; Doane, W. Package “textmineR”; Functions for Text Mining and Topic Modeling; CRAN, 2019;
# https://rdrr.io/cran/textmineR/

library(tidyverse)
library(ggplot2)
#library(viridisLite)

percentages <- read.csv("", header = TRUE)

# loads funny... rename and select only what you are interested in
percentages <- percentages %>% 
  rename("Time_Start" = "ï..Time_Start") %>% 
  rename("irregular" = "irreular")

percentages <- percentages[1:10,]

# transform data into time, word, percent for graph
transformed <- gather(percentages, key = "search_term", value = "percent_results",
                      arid, discontinuous, dry, ephemeral, episodic, intermittent, 
                      interrupted, irregular, perennial, permanent, seasonal, temporary)

########

# to plot, had to reconfigure dataframe so that each year has its own row - used excel

library(ggplot2)
library(colorRamps)
library(RColorBrewer)
library(tidyverse)

yearly <- read.csv("", header = TRUE)

yearly <- yearly %>% 
  rename("year" = "ï..year") %>% 
  rename("irregular" = "irreular")

# transform data into time, word, percent for graph
year_transformed <- gather(yearly, key = "search_term", value = "percent_results",
                           arid, discontinuous, dry, ephemeral, episodic, intermittent, interrupted, 
                           irregular, perennial, permanent, seasonal, temporary)

# plot

colourCount = length(unique(year_transformed$search_term))
getPalette = colorRampPalette(brewer.pal(8, "Set2"))

# change epithets to make rest of paper
year_transformed$search_term <- gsub("perennial", "non-perennial", year_transformed$search_term)
year_transformed$search_term <- gsub("permanent", "non-permanent", year_transformed$search_term)


p1 <- ggplot(year_transformed, aes(x = year, y = percent_results)) + 
  geom_col(aes(fill = search_term), size = 5, width = 1) +
  scale_fill_manual(values = getPalette(colourCount)) +
  # scale_fill_viridis_d() +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("Year") +
  ylab("Proportion of Results") +
  guides(fill = guide_legend(title = "Epithet")) +
  #geom_text(aes(label = year_transformed$sample_size, y=1.02, size = 35), show.legend = FALSE) +
  theme(axis.text = element_text(size=28),
        axis.title = element_text(size=30,face="bold"),
        legend.text = element_text(size=28),
        legend.title = element_text(size = 30, face = "bold")) +
  geom_segment(x = 1899.5, y = 0, xend = 1899.5, yend = 1, size = 1) +
  geom_segment(x = 1925.5, y = 0, xend = 1925.5, yend = 1, size = 1) +
  geom_segment(x = 1950.5, y = 0, xend = 1950.5, yend = 1, size = 1) +
  geom_segment(x = 1975.5, y = 0, xend = 1975.5, yend = 1, size = 1) +
  geom_segment(x = 1990.5, y = 0, xend = 1990.5, yend = 1, size = 1) +
  geom_segment(x = 1995.5, y = 0, xend = 1995.5, yend = 1, size = 1) +
  geom_segment(x = 2000.5, y = 0, xend = 2000.5, yend = 1, size = 1) +
  geom_segment(x = 2005.5, y = 0, xend = 2005.5, yend = 1, size = 1) +
  geom_segment(x = 2010.5, y = 0, xend = 2010.5, yend = 1, size = 1) +
  geom_segment(x = 2015.5, y = 0, xend = 2015.5, yend = 1, size = 1) +
  geom_segment(x = 2019.5, y = 0, xend = 2019.5, yend = 1, size = 1) 

# plotting this with the rates of papers per time frame
p2 <- ggplot(data = year_transformed, aes(x = year, y = sample_size)) +
  geom_line(size = 1) +
  theme(text = element_text(size=30)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(y = "Papers Published", x = "Year") +
  theme_minimal()+
  theme(axis.text = element_text(size=30),
        axis.title = element_text(size=30,face="bold"),
        legend.text = element_text(size=20),
        legend.title = element_text(size = 30, face = "bold"))

# making a 2 paneled plot
library(cowplot)

plot_grid(p2, p1, labels = c('A', 'B'), ncol = 1, align = "h", axis = "rl", label_size = 30)

# Fisher's Exact Test
# run on data with time-frames, not individual years - need full number results
totals <- read.csv("", header = TRUE)

# remove unnecessary cols
table <- totals[c(2, 4:15)]

set.seed(20150828)   # For the sake of reproducibility.
fisher.test(table, simulate.p.value = TRUE, B = 1e5) # too large, need to simulate p


#########################################################################################

# plotting individual epithet publication rates over time

#load libraries
library(tidytext)
library(dplyr)
library(readr)
library(stringr)
library(tidyverse)
library(ggplot2)

# steps:
# upload each individual time period
# search for the descriptor terms wihtin each time frame
# get raw numbers
# also want to get percentages

# set working directory
time_abstracts <- ("")
setwd(time_abstracts)

# want to upload all the files and get frequencies of the ires search terms

# epithets of interest
ires_words = (c("intermittment", "ephemeral", "temporary", "interrupted", "discontinuous", "seasonal", 
                "permanent", "perennial", "dry", "episodic", "irregular", "arid"))

# all possible variations of epithets
possible_words <- (c("intermittment", "intermitt", "intermittency", "intermittently", "intermittence",
                     "ephemeral", "ephemer", "ephemerally", "ephemerality", "ephemeralness",
                     "temporary", "temp", "temporal", "temporarily", "temporariness", 
                     "interrupted", "interrupt", "interruptedly", "interruptedness","interruptible", 
                     "interruptive",
                     "discontinuous", "discont", "discontinuously", "discontinuousness", "discontin",
                     "seasonal", "season", "seasonally", "seasonalness",
                     "permanent", "semipermanent", "nonpermanent", "semipermanently", "nonpermanently",
                     "perennial", "semiperennial", "nonperennial", "semiperennially", "nonperennially",
                     "dry", "dri", "dried", "drying", "drier", "driest",
                     "episodic", "episod", "episodically",
                     "arid", "aridity", "aridness",
                     "irregular", "irregularity", "irregularly"))

# abstracts start here after1990 - that is time frame 5
time5 <- read.csv("", header = TRUE) %>% 
  mutate(time = "1991-1995")
time6 <- read.csv("", header = TRUE)%>% 
  mutate(time = "1996-2000")
time7 <- read.csv("", header = TRUE)%>% 
  mutate(time = "2001-2005")
time8 <- read.csv("", header = TRUE)%>% 
  mutate(time = "2006-2010")
time9 <- read.csv("", header = TRUE)%>% 
  mutate(time = "2011-2015")
time10 <- read.csv("", header = TRUE) %>% 
  mutate(time = "2016-2018")

# to see how topics change over time, need to combine all time frames to get full corpus
x <- rbind(time5, time6, time7, time8, time9, time10)

# gives author, year published, abstracts, and individual ids
ab_au_yr <- x %>% 
  ungroup() %>% 
  select(c("AU", "PY", "AB", "time")) %>% 
  mutate(id = rownames(x)) 

# get the proper data.frame format for getting frequencies
# tokenized list of bigrams, counts, and which doucment they are from
tidy_abstracts <- ab_au_yr %>% 
  select(id, AB) %>% 
  unnest_tokens(output = word, 
                input = AB,
                token = "ngrams",
                n_min = 1, n = 2) %>%
  count(id, word)

# merging id, author, year, bigrams, counts
words_years <- merge(ab_au_yr, tidy_abstracts, by = "id")

words_years <- words_years %>% 
  select("id", "AU", "PY", "word", "n", "time")

# make this into a .csv so you don't have to re-run everything every time

############################################################################################

# continued...

library(tidytext)
library(dplyr)
library(readr)
library(stringr)
library(tidyverse)
library(ggplot2)

possible_words <- (c("intermittment", "intermitt", "intermittency", "intermittently", "intermittence",
                     "ephemeral", "ephemer", "ephemerally", "ephemerality", "ephemeralness",
                     "temporary", "temp", "temporal", "temporarily", "temporariness", 
                     "interrupted", "interrupt", "interruptedly", "interruptedness","interruptible", 
                    "interruptive",
                     "discontinuous", "discont", "discontinuously", "discontinuousness", "discontin",
                     "seasonal", "season", "seasonally", "seasonalness",
                     "permanent", "semipermanent", "nonpermanent", "semipermanently", "nonpermanently",
                     "perennial", "semiperennial", "nonperennial", "semiperennially", "nonperennially",
                     "dry", "dri", "dried", "drying", "drier", "driest",
                     "episodic", "episod", "episodically",
                     "arid", "aridity", "aridness",
                     "irregular", "irregularity", "irregularly"))

words_years <- read.csv("", header = TRUE)

# now you need to filter out ires words you are interested in. 
  # used bigrams (two words next to eachother) in model
  # so you have two word columns, need to check each one!

# to filter word 1
ires_abstracts2 <- words_years %>% 
  separate(col = "word", into = c("word1", "word2"), remove = TRUE) %>% 
  filter(word1 %in% possible_words) 

# to filter word 2
ires_abstracts1 <- words_years %>% 
  separate(col = "word", into = c("word1", "word2"), remove = TRUE) %>% 
  filter(word2 %in% possible_words) 

# alter each possible word you looked for to one of 12 epithets

ires_abstracts2$word1[ires_abstracts2$word1 == "intermitt"] <- "intermittent"
ires_abstracts2$word2[ires_abstracts2$word2 == "intermitt"] <- "intermittent"

ires_abstracts2$word1[ires_abstracts2$word1 == "intermittency"] <- "intermittent"
ires_abstracts2$word2[ires_abstracts2$word2 == "intermittency"] <- "intermittent"

ires_abstracts2$word1[ires_abstracts2$word1 == "intermittence"] <- "intermittent"
ires_abstracts2$word2[ires_abstracts2$word2 == "intermittence"] <- "intermittent"

ires_abstracts2$word1[ires_abstracts2$word1 == "intermittently"] <- "intermittent"
ires_abstracts2$word2[ires_abstracts2$word2 == "intermittently"] <- "intermittent"

ires_abstracts2$word1[ires_abstracts2$word1 == "ephemerality"] <- "ephemeral"
ires_abstracts2$word2[ires_abstracts2$word2 == "ephemerality"] <- "ephemeral"

ires_abstracts2$word1[ires_abstracts2$word1 == "ephemerally"] <- "ephemeral"
ires_abstracts2$word2[ires_abstracts2$word2 == "ephemerally"] <- "ephemeral"

ires_abstracts2$word1[ires_abstracts2$word1 == "temp"] <- "temporary"
ires_abstracts2$word2[ires_abstracts2$word2 == "temp"] <- "temporary"

ires_abstracts2$word1[ires_abstracts2$word1 == "temporal"] <- "temporary"
ires_abstracts2$word2[ires_abstracts2$word2 == "temporal"] <- "temporary"

ires_abstracts2$word1[ires_abstracts2$word1 == "temporarily"] <- "temporary"
ires_abstracts2$word2[ires_abstracts2$word2 == "temporarily"] <- "temporary"

ires_abstracts2$word1[ires_abstracts2$word1 == "temporariness"] <- "temporary"
ires_abstracts2$word2[ires_abstracts2$word2 == "temporariness"] <- "temporary"

ires_abstracts2$word1[ires_abstracts2$word1 == "interrupted"] <- "interrupt"
ires_abstracts2$word2[ires_abstracts2$word2 == "interrupted"] <- "interrupt"

ires_abstracts2$word1[ires_abstracts2$word1 == "interruptible"] <- "interrupt"
ires_abstracts2$word2[ires_abstracts2$word2 == "interruptible"] <- "interrupt"

ires_abstracts2$word1[ires_abstracts2$word1 == "interruptive"] <- "interrupt"
ires_abstracts2$word2[ires_abstracts2$word2 == "interruptive"] <- "interrupt"

ires_abstracts2$word1[ires_abstracts2$word1 == "discontinuously"] <- "discontinuous"
ires_abstracts2$word2[ires_abstracts2$word2 == "discontinuously"] <- "discontinuous"

ires_abstracts2$word1[ires_abstracts2$word1 == "seasonally"] <- "seasonal"
ires_abstracts2$word2[ires_abstracts2$word2 == "seasonally"] <- "seasonal"

ires_abstracts2$word1[ires_abstracts2$word1 == "season"] <- "seasonal"
ires_abstracts2$word2[ires_abstracts2$word2 == "season"] <- "seasonal"

ires_abstracts2$word1[ires_abstracts2$word1 == "nonpermanent"] <- "permanent"
ires_abstracts2$word2[ires_abstracts2$word2 == "nonpermanent"] <- "permanent"

ires_abstracts2$word1[ires_abstracts2$word1 == "semipermanent"] <- "permanent"
ires_abstracts2$word2[ires_abstracts2$word2 == "semipermanent"] <- "permanent"

ires_abstracts2$word1[ires_abstracts2$word1 == "semipermanently"] <- "permanent"
ires_abstracts2$word2[ires_abstracts2$word2 == "semipermanently"] <- "permanent"

ires_abstracts2$word1[ires_abstracts2$word1 == "nonperennial"] <- "perennial"
ires_abstracts2$word2[ires_abstracts2$word2 == "nonperennial"] <- "perennial"

ires_abstracts2$word1[ires_abstracts2$word1 == "dried"] <- "dry"
ires_abstracts2$word2[ires_abstracts2$word2 == "dried"] <- "dry"

ires_abstracts2$word1[ires_abstracts2$word1 == "drier"] <- "dry"
ires_abstracts2$word2[ires_abstracts2$word2 == "drier"] <- "dry"

ires_abstracts2$word1[ires_abstracts2$word1 == "driest"] <- "dry"
ires_abstracts2$word2[ires_abstracts2$word2 == "driest"] <- "dry"

ires_abstracts2$word1[ires_abstracts2$word1 == "drying"] <- "dry"
ires_abstracts2$word2[ires_abstracts2$word2 == "drying"] <- "dry"

ires_abstracts2$word1[ires_abstracts2$word1 == "episodically"] <- "episodic"
ires_abstracts2$word2[ires_abstracts2$word2 == "episodically"] <- "episodic"


ires_abstracts2$word1[ires_abstracts2$word1 == "aridity"] <- "arid"
ires_abstracts2$word2[ires_abstracts2$word2 == "aridity"] <- "arid"

ires_abstracts2$word1[ires_abstracts2$word1 == "aridness"] <- "arid"
ires_abstracts2$word2[ires_abstracts2$word2 == "aridness"] <- "arid"


ires_abstracts2$word1[ires_abstracts2$word1 == "irregularity"] <- "irregular"
ires_abstracts2$word2[ires_abstracts2$word2 == "irregularity"] <- "irregular"

ires_abstracts2$word1[ires_abstracts2$word1 == "irregularly"] <- "irregular"
ires_abstracts2$word2[ires_abstracts2$word2 == "irregularly"] <- "irregular"

# collapse your data by paper / year
ires_collapsed <- ires_abstracts2 %>% 
  select(-n) %>% 
  distinct(AU, PY, word1) %>% 
  group_by(PY, word1) %>% 
  count()

library(colorRamps)
library(RColorBrewer)

# make sure epithets match paper / figures
ires_collapsed$word1 <- gsub("perennial", "non-perennial", ires_collapsed$word1)
ires_collapsed$word1 <- gsub("permanent", "non-permanent", ires_collapsed$word1)
ires_collapsed$word1 <- gsub("interrupt", "interrupted", ires_collapsed$word1)

colourCount = length(unique(ires_collapsed$word1))
getPalette = colorRampPalette(brewer.pal(8, "Set2"))

ggplot(data = ires_collapsed, aes(x = PY, y = n, color = word1, group = word1)) +
  #geom_line(aes(color = word1)) +
  geom_point(aes(shape = word1), size = 3) +
  geom_line(size = 1) +
  scale_shape_manual(values = c(0, 1, 2, 3, 4, 5, 8, 15, 16, 17, 18, 13, 19, 21)) + 
  scale_fill_manual(values = getPalette(colourCount)) +
  # scale_size_manual(15) +
  theme(text = element_text(size=40)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  # xlim(1990, 2017) +
  # ylim(0, 325) +
  labs(color = "Epithet", shape = "Epithet", x = "Year Published", y = "Number of Papers") +
  # scale_fill_brewer(palette = "Spectral") +
  theme_minimal()+
  theme(axis.text = element_text(size=40),
        axis.title = element_text(size=40,face="bold"),
        legend.text = element_text(size=30),
        legend.title = element_text(size = 40, face = "bold"))



##########################################################################################

# TOPIC MODELING OVER TIME
# can only do from 1990 on, before there are no abstracts

#load libraries
library(tidytext)
library(dplyr)
library(readr)
library(stringr)
library(tidyverse)
library(ggplot2)
library(tm)
library(topicmodels)
library(textmineR)
library(Matrix)
library(textstem)

# steps:
# upload each individual time period
# search for epithets wihtin each time frame
# dan wants to run LDA on time series corpora to see if topics have changed over time

time_abstracts <- ("")
setwd(time_abstracts)

# add all possible terms for epithets
possible_words <- (c("intermittment", "intermitt", "intermittency", "intermittently", "intermittence",
                     "ephemeral", "ephemer", "ephemerally", "ephemerality", "ephemeralness",
                     "temporary", "temp", "temporal", "temporarily", "temporariness", 
                     "interrupted", "interrupt", "interruptedly", "interruptedness","interruptible", 
                     "interruptive",
                     "discontinuous", "discont", "discontinuously", "discontinuousness",
                     "seasonal", "season", "seasonally", "seasonalness",
                     "permanent", "semipermanent", "nonpermanent", "semipermanently", "nonpermanently",
                     "perennial", "semiperennial", "nonperennial", "semiperennially", "nonperennially",
                     "dry", "dri", "dried", "drying", "drier", "driest",
                     "episodic", "episod", "episodically",
                     "irregular", "irregularly", "irregularity",
                     "arid", "aridity", "aridness"))

# found from previous attempts at running code
mystopwords <- c('c', 'm', 'b', 'n', 'c', 'h', 'p', 'eg', 'two', 'ltd', 'elsevier', 'v', 'rights', 
                 'reserved', 'e', 'g', 'x', 'ray', 'wiley', 'son')

# you have these different csv files with complete abstracts
time5 <- read.csv("", header = TRUE) %>% 
  mutate(time = "")
time6 <- read.csv("", header = TRUE)%>% 
  mutate(time = "")
time7 <- read.csv("", header = TRUE)%>% 
  mutate(time = "")
time8 <- read.csv("", header = TRUE)%>% 
  mutate(time = "")
time9 <- read.csv("", header = TRUE)%>% 
  mutate(time = "")
time10 <- read.csv("", header = TRUE) %>% 
  mutate(time = "")

### topic modeling on full mega corpus, combine all timeframes
x <- rbind(time5, time6, time7, time8, time9, time10)

# gives author, year published, abstracts, and individual ids
ab_au_yr <- x %>% 
  ungroup() %>% 
  select(c("AU", "PY", "AB", "time")) %>% 
  mutate(id = rownames(x)) 

# takes the rest of the columns and makes the meta data attached to the text
corpus <- Corpus(VectorSource(ab_au_yr$AB))

# clean up the corpus
corpus <- tm_map(corpus, removePunctuation) #removes punctuation
corpus <- tm_map(corpus, removeNumbers) # removes numbers
corpus <- tm_map(corpus, content_transformer(tolower)) #makes everything lowercase
corpus <- tm_map(corpus, removeWords, mystopwords)
corpus <- tm_map(corpus, removeWords, (stopwords(kind = "en"))) #removes common stopwords from corpus
corpus <- tm_map(corpus, stem_strings)
# get rid of extra white space from removing punctuation 
corpus <- tm_map(corpus, stripWhitespace)

# make dtm
dtm_corpus <- DocumentTermMatrix(corpus)
inspect(dtm_corpus) # should give matrix of word frequency in each document
# check point

# back into format for LDA modeling
dataframe_abstracts <- data.frame(text=sapply(corpus, identity), 
                                  stringsAsFactors=F) %>% 
  mutate(id = row_number())

# combine the dataframe_abstracts back with the ab_au_yr dataframe
au_yr <- ab_au_yr %>% 
  select(AU, PY, id, time)

dataframe_total <- merge(au_yr, dataframe_abstracts, by = "id")

# get the proper data.frame format for transformation
tidy_docs <- dataframe_total %>% 
  select(id, text) %>% 
  unnest_tokens(output = word, 
                input = text,
                token = "ngrams",
                n_min = 1, n = 2) %>% 
  count(id, word) %>% 
  filter(n>1)

# turn a tidy tbl into a sparse dgCMatrix for use in textmineR
dgCMatrix <- tidy_docs %>% 
  cast_sparse(id, word, n)


# choose a range of k (topics) 
k_list <- seq(1,50, by=1)

# Fit a bunch of LDA models
# # even on this trivial corpus, it will a bit of time to fit all of these models
    # in line 500, k = k_list to determine number of topics

model_list <- TmParallelApply(X = k_list, FUN = function(k){
  
  m <- FitLdaModel(dtm = nih_sample_dtm,
                   k = k,
                   iterations = 200,
                   burnin = 180,
                   alpha = 0.1,
                   beta = colSums(nih_sample_dtm) / sum(nih_sample_dtm) * 100,
                   optimize_alpha = TRUE,
                   calc_likelihood = FALSE,
                   calc_coherence = TRUE,
                   calc_r2 = FALSE,
                   cpus = 1)
  m$k <- k
  
  m
}, export= ls(), # c("nih_sample_dtm"), # export only needed for Windows machines
cpus = 2)

# Get average coherence for each model
coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)),
                            coherence = sapply(model_list, function(x) mean(x$coherence)),
                            stringsAsFactors = FALSE)

# Plot the result
# On larger (~1,000 or greater documents) corpora, you will usually get a clear peak
plot(coherence_mat, type = "o")

pdf(file = "figure_S2.pdf", width = 20, height = 15)

# for supplementary materials
ggplot(data = coherence_mat, aes(x = k, y = coherence)) +
  #geom_line(aes(color = word1)) +
  geom_point(aes(size = 3), show.legend = FALSE) +
  geom_line(size = 1) +
  theme(text = element_text(size=40)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Number of Topics", y = "Coherence Score") +
  # scale_fill_brewer(palette = "Spectral") +
  theme_minimal()+
  theme(axis.text = element_text(size=40),
        axis.title = element_text(size=40,face="bold"))

dev.off()

#run the model!

set.seed(12345) #no explaination, pretty sure this is just random

model <- FitLdaModel(dtm = dgCMatrix, 
                     k = 9, # number of topics, can test with multiple iterations of lda
                     iterations = 500, # I usually recommend at least 500 iterations or more, number of Gibbs iterations
                     burnin = 180,
                     alpha = 0.1, # parameter for distribution of topics over documents
                     beta = 0.05, # parameter for distribution of words over topics
                     optimize_alpha = TRUE,
                     calc_likelihood = TRUE,
                     calc_coherence = TRUE,
                     calc_r2 = TRUE,
                     cpus = 2) 

# Get average coherence for each model
coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)),
                            coherence = sapply(model_list, function(x) mean(x$coherence)),
                            stringsAsFactors = FALSE)

# alpha - distribution of topics over documents
# theta - probability of topic given document
# phi - probability of word given topic
# gamma - probability of topic given word 
# coherence!!! higher coherence score means better topic
#r2 - proportion of variability in data explained by the model

# Get the top terms of each topic
model$top_terms <- GetTopTerms(phi = model$phi, M = 20)

# Get the prevalence of each topic
# You can make this discrete by applying a threshold, say 0.05, for
# topics in/out of docuemnts. 
model$prevalence <- colSums(model$theta) / sum(model$theta) * 100

# prevalence should be proportional to alpha
# just a check
plot(model$prevalence, model$alpha, xlab = "prevalence", ylab = "alpha")

# textmineR has a naive topic labeling tool based on probable bigrams
# (different from LDA which cannot assign topics, that is something you have to do separately)
model$labels <- LabelTopics(assignments = model$theta > 0.05, 
                            dtm = dgCMatrix,
                            M = 1)


# put them together, with coherence into a summary table
model$summary <- data.frame(topic = rownames(model$phi),
                            label = model$labels,
                            coherence = round(model$coherence, 3),
                            prevalence = round(model$prevalence,3),
                            top_terms = apply(model$top_terms, 2, function(x){
                              paste(x, collapse = ", ")
                            }),
                            stringsAsFactors = FALSE)

theta <- data.frame(model$theta)

theta <- tibble::rownames_to_column(theta, "id")

theta$id <- as.numeric(theta$id)

theta <- merge(theta, ab_au_yr, by.y = "id", by.x = "id")



######################################################
theta <- read.csv("", header = TRUE)[,-c(1:2, 19:21)]

summary <- read.csv("", header = TRUE)

theta <- theta %>% 
  mutate(id = row_number())

# need to transform theta into binary, Y/N for over 0.8
binary <- ifelse(theta[, -c(10:14)] >= 0.80, 1, 0)
binary <- data.frame(binary)
binary <- binary %>% 
  mutate(id = row_number())

# binary.sum <- data.frame(rowSums(binary))

id <- theta[, c(10:14)]

topic.time <- merge(id, binary, by = "id")

topic.sum <- aggregate(topic.time[, 6:14], list(topic.time$time), sum)

# combine all topics
topic.sum$total <- topic.sum$t_1 + topic.sum$t_2 + topic.sum$t_3 + topic.sum$t_4 + topic.sum$t_5 + 
  topic.sum$t_6 + topic.sum$t_7 + topic.sum$t_8 + topic.sum$t_9 

# get proportions over time
topic.sum$t1.prop <- topic.sum$t_1 / topic.sum$total 
topic.sum$t2.prop <- topic.sum$t_2 / topic.sum$total 
topic.sum$t3.prop <- topic.sum$t_3 / topic.sum$total 
topic.sum$t4.prop <- topic.sum$t_4 / topic.sum$total 
topic.sum$t5.prop <- topic.sum$t_5 / topic.sum$total 
topic.sum$t6.prop <- topic.sum$t_6 / topic.sum$total 
topic.sum$t7.prop <- topic.sum$t_7 / topic.sum$total 
topic.sum$t8.prop <- topic.sum$t_8 / topic.sum$total 
topic.sum$t9.prop <- topic.sum$t_9 / topic.sum$total 

# transpose to plot
topic.plot <- as.data.frame(t(as.matrix(topic.sum[, c(1,12:20)])))

topic.plot <- topic.plot %>% 
  rename("1991-1995" = "V1") %>% 
  rename("1996-2000" = "V2") %>% 
  rename("2001-2005" = "V3") %>% 
  rename("2006-2010" = "V4") %>% 
  rename("2011-2015" = "V5") %>% 
  rename("2016-2018" = "V6")

# not sure how to get this in proper format...write a .csv and do it in excel
# write.csv(topic.plot, "C:/Users/mhope/Documents/What_R_IRES/Data/timeseries/topic.plot.k9.prop.csv")

time.plot <- read.csv("", header = TRUE)

# display data!
library(colorRamps)
library(RColorBrewer)
library(ggplot2)

colourCount = 9
getPalette = colorRampPalette(brewer.pal(8, "Set2"))

# Supp Fig 3
ggplot(data = time.plot,
       aes(x = time.frame, 
           y = prop, 
           fill = label)) + 
  scale_fill_manual(values = getPalette(colourCount)) +
  theme_minimal() +
  geom_col() +
  labs(color = "Topic Name", x = "Time Frame", y = "Proportion") +
  guides(fill = guide_legend(title = "Topic")) +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=25,face="bold"),
        legend.text = element_text(size=25),
        legend.title = element_text(size = 20, face = "bold")) 

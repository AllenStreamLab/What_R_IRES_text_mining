#### Topic modeling ####

## adapted from 
# Jones, T.; Doane, W. Package “textmineR”; Functions for Text Mining and Topic Modeling; CRAN, 2019;
# https://rdrr.io/cran/textmineR/

# using STEMMING over lemmetization

library(tm)
library(topicmodels)
library(tidytext)
library(textmineR)
library(Matrix)
library(tidyverse)
library(textstem)

# found and edited after running results multiple times - 
  # sometimes a word may get cut off at the end of a sentence (e.g., c, b, n)
  # to include legal parts that aren't for analysis (e.g., elsevier, rights, reserved)
mystopwords <- c('c', 'b', 'n', 'c', 'h', 'p', 'eg', 'two', 'ltd', 'elsevier', 
                 'v', 'rights', 'reserved', 'e', 'g', 'x', 'ray', 'wiley', 'son', 'm')

setwd("")
getwd()

# upload the abstract csv files - full WoS search results
# create new "des" column for epithet term (was called "descriptor" at one point)
intermitten <- read.csv("intermittent.csv", header = TRUE)
intermitten$des <- "intermittent"

ephemer <- read.csv("ephemeral.csv", header = TRUE)
ephemer$des <- "ephemeral"

tempora <- read.csv("temporary.csv", header = TRUE)
tempora$des <- "temporary"

interrupt <- read.csv("interrupted.csv", header = TRUE)
interrupt$des <- "interrupted"

discontin <- read.csv("discontinuous.csv", header = TRUE)
discontin$des <- "discontin"

season <- read.csv("seasonal.csv", header = TRUE)
season$des <- "seasonal"

perman <- read.csv("permanent.csv", header = TRUE)
perman$des <- "permanent"

perennial <- read.csv("perennial.csv", header = TRUE)
perennial$des <- "perennial"

dry <- read.csv("dry.csv", header = TRUE)
dry$des <- "dry"

episod <- read.csv("episodic.csv", header = TRUE)
episod$des <- "episodic"

irreg <- read_csv("irregular.csv")
irreg$des <- "irregular"

arid <- read.csv("arid.csv", header = TRUE)
arid$des <- "arid"

# cleaning columns
irreg <- irreg %>% 
  rename("ï..PT" = "PT")

# all abstracts together for LDA model
all.abstracts <- rbind(intermitten, ephemer, tempora, interrupt, discontin, season, perman, 
                       perennial, dry, episod, arid, irreg)

x <- all.abstracts

abstracts <- x[!sapply(x, function(x) all(is.na(x)))] # remove columns that are empty

# need to convert .csv into a dtm to get term frequencies from abstracts
# pull out abstract, author, year data
ab_au_yr <- abstracts %>% 
  ungroup() %>% 
  select(c("AU", "PY", "AB", "des")) %>% 
  mutate(id = rownames(abstracts))

# takes the rest of the columns and makes the meta data attached to the text
corpus <- Corpus(VectorSource(ab_au_yr$AB))

# clean up the corpus
corpus <- tm_map(corpus, removePunctuation) #removes punctuation
corpus <- tm_map(corpus, removeNumbers) # removes numbers
corpus <- tm_map(corpus, content_transformer(tolower)) # makes everything lowercase
corpus <- tm_map(corpus, removeWords, mystopwords) # "mystopwords" vector as words that mess up the analysis
corpus <- tm_map(corpus, removeWords, (stopwords(kind = "en"))) # removes common stopwords from corpus (english)
corpus <- tm_map(corpus, stem_strings) # reduces words to base form

# get rid of extra white space from removing punctuation 
corpus <- tm_map(corpus, stripWhitespace)

# make dtm
dtm_corpus <- DocumentTermMatrix(corpus)
inspect(dtm_corpus) # should be a matrix of words (columns) and document numbers (rows)
  # displays how many times each word appears in every document

# clean the corpus
dataframe_abstracts <- data.frame(text=sapply(corpus, identity), 
                                  stringsAsFactors=F) %>% 
  mutate(id = row_number())

# combine the dataframe_abstracts back with the ab_au_yr dataframe
au_yr <- ab_au_yr[c(1,2,4,5)] # leave out abstract text (AB column)

dataframe_total <- merge(au_yr, dataframe_abstracts, by = "id")

# get the proper data.frame format for transformation
tidy_docs <- dataframe_total %>% 
  select(id, text) %>% 
  unnest_tokens(output = word, 
                input = text,
                token = "ngrams",
                n_min = 1, n = 2) %>% 
  count(id, word) %>% 
  filter(n>1) # removes words that only appear once - otherwise too large to run

# turn a tidy tbl into a sparse dgCMatrix for use in textmineR
dgCMatrix <- tidy_docs %>% 
  cast_sparse(id, word, n)

# ### choose number of topics ### #

# choose a range of k 
# range runs into the corpus size. Not recommended for large corpora!
  # also consider what is managable to look at. but want a wide enough range to see where line levels off
k_list <- seq(1,50, by=2)

# Fit a bunch of LDA models, it takes time!
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

#############################################
#run the model!

set.seed(12345) #no explaination, pretty sure this is just random

# based on your coherence plot, select k

model <- FitLdaModel(dtm = dgCMatrix, 
                     k = 6, ### number of topics, can test with multiple iterations of lda
                     iterations = 500, # I usually recommend at least 500 iterations or more, number of Gibbs iterations
                     burnin = 180,
                     alpha = 0.1, # parameter for distribution of topics over documents
                     beta = 0.05, # parameter for distribution of words over topics
                     optimize_alpha = TRUE,
                     calc_likelihood = TRUE,
                     calc_coherence = TRUE,
                     calc_r2 = TRUE,
                     cpus = 2) 
### alpha - distribution of topics over documents
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

head(model$labels)

# put them together, with coherence into a summary table
model$summary <- data.frame(topic = rownames(model$phi),
                            label = model$labels,
                            coherence = round(model$coherence, 3),
                            prevalence = round(model$prevalence,3),
                            top_terms = apply(model$top_terms, 2, function(x){
                              paste(x, collapse = ", ")
                            }),
                            stringsAsFactors = FALSE)

# create the theta table
theta <- data.frame(model$theta)

theta <- tibble::rownames_to_column(theta, "id")

theta <- merge(theta, ab_au_yr, by = "id")

# save as .csv's to save time later on

#####################################################

theta <- read.csv("", header = TRUE)[, c(2:8, 12)]
theta.info <- read.csv("", header = TRUE)[, c(2, 9:12)]
# contains meta data - authors, abstracts, corpora, etc.
model <- read.csv("", header = TRUE)

library(vegan)
library(tidyverse)

### need to transpose data frame so topics (communities) are rows and documents (species) are columns
theta_transpose <- as.data.frame(t(as.matrix(theta)))

rownames(theta_transpose) <- colnames(theta)
colnames(theta_transpose) <- rownames(theta)
theta_transpose <- data.frame(as.numeric(theta_transpose))

nmds <- metaMDS(theta_transpose, k=2, trymax = 100)
stressplot(nmds)
# Large scatter around the line suggests that original dissimilarities
  # are not well preserved in the reduced number of dimensions.
    # looks ok! not great...but still!

# try plotting
plot(nmds)
ordiplot(nmds,type="n")
orditorp(nmds,display="species",col="red",air=0.01)
orditorp(nmds,display="sites",cex=1.25,air=0.01)


# really messy... separate the theta matrix by descriptor
#    and average the prob of each topic?
theta.avg <- aggregate(theta[, 2:7], list(theta$des), mean)

theta.avg.trans <- as.data.frame(t(as.matrix(theta.avg)))
rownames(theta.avg.trans) <- colnames(theta.avg)
colnames(theta.avg.trans) <- rownames(theta.avg)

grp.nmds <- metaMDS(theta.avg[,c(2:7)], k=2, trymax = 100)

grp.nmds$stress
scores(grp.nmds)

# plot(grp.nmds)
ordiplot(grp.nmds,type="n", cex = 5)
orditorp(grp.nmds,display="species",col="blue",air=0.01,
         labels = c("geomorphology", "vegetation", "ecohydrology", "agriculture",
                    "climate", "hydrology"), cex = 3)
orditorp(grp.nmds,display="sites",cex=3,air=0.01,
         labels = c("arid", "discontinuous", "dry", "ephemeral", "episodic", "intermittent",
                    "interrupted", "irregular", "non-perennial", "non-permanent", "seasonal", "temporary"))

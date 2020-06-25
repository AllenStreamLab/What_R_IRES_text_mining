##### Definition analysis ######

## adapted from 
# Jones, T.; Doane, W. Package “textmineR”; Functions for Text Mining and Topic Modeling; CRAN, 2019;
# https://rdrr.io/cran/textmineR/

############################################################################

# run *TF_IDF* on the abstracts to see what the most important words are
  # upload all abstract results for each epithet

# ZIPF'S LAW #

#need to start with the table of source, word, n and total
##code from tf_idf loop.R

library(tidytext)
library(stringr)
library(tm)
library(ggplot2)
library(topicmodels)
library(textmineR)
library(tidyverse)
library(textstem)

setwd()
getwd()

mystopwords <- c('c', 'b', 'n', 'c', 'h', 'p', 'eg', 'two', 'ltd', 'elsevier', 'v', 'rights', 
                 'reserved', 'e', 'g', 'x', 'ray', 'wiley', 'son')
# from mutliple analyses with various data

# upload the abstract csv files
intermitten <- read.csv("", header = TRUE)
ephemer <- read.csv("", header = TRUE)
tempora <- read.csv("", header = TRUE)
interrupt <- read.csv("", header = TRUE)
discontin <- read.csv("", header = TRUE)
season <- read.csv("", header = TRUE)
perman <- read.csv("", header = TRUE)
perennial <- read.csv("", header = TRUE)
dry <- read.csv("", header = TRUE)
episod <- read.csv("", header = TRUE)

# do tf_idf within each corpora
x <- #epithet corpora# %>% 
  select(AU, PY, AB) %>% 
  mutate(id = paste(AU, PY, sep = "_"))

x$AB <- as.character(x$AB)

# need to unnest tokens first to get counts
unnested <- x %>% 
  select(id, AB) %>% 
  unnest_tokens(output = word, 
                input = AB,
                token = "words") %>% 
  count(id, word) 

# doing the tf_idf calculations...need number of words per abstract
total_words <- unnested %>% 
  group_by(id) %>% #group final_tf by source (paper)
  summarize(total = sum(n)) #total number of words in source (paper)

#combine results into one table
abstract_sums <- left_join(unnested, total_words)

#to get term frequency
freq_by_rank <- abstract_sums %>% 
  dplyr::mutate(rank = row_number(),
                'term frequency' = n/total)

#tf-idf finds words that are common, but not too common
abstract_words <- abstract_sums %>%
  group_by(id) %>% 
  ungroup() %>% 
  bind_tf_idf(word, id, n)

#words with tf-idf are zero (or close) for really common words

unigrams_tf_idf <- abstract_words %>% 
  arrange(desc(tf_idf)) %>% 
  group_by(id)

unigrams_tf_idf #will give you tf_idf for every word

### PULL OUT IRES_WORDS HERE! ###

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
                     "episodic", "episod", "episodically"))
## to find papers where these words matter most, use individual searches 

ires_unigrams <- unigrams_tf_idf %>% 
  filter(word %in% possible_words) #want to only analyze ires_words

ires_unigrams
#basically tells you which words are the most important to each paper

# SCRIPTS BELOW WILL GIVE YOU THE TOP TF_IDF PAPERS FOR DEFINITION ANALYSES !!! #

intermittent <- (c("intermittment", "intermitt", "intermittency", "intermittently", "intermittence"))

intermit <- unigrams_tf_idf %>% 
  filter(word %in% intermittent) #want to only analyze ires_words

# write.csv(intermit, "")

#####

ephemeral <- (c("ephemeral", "ephemer", "ephemerally", "ephemerality", "ephemeralness"))

ephem <- unigrams_tf_idf %>% 
  filter(word %in% ephemeral) #want to only analyze ires_words

# write.csv(ephem, "")

######

temporary <- (c("temporary", "temp", "temporal", "temporarily", "temporariness"))

temp <- unigrams_tf_idf %>% 
  filter(word %in% temporary) #want to only analyze ires_words

# write.csv(temp, "")

######

interrupted <- (c("interrupted", "interrupt", "interruptedly", "interruptedness","interruptible", 
                  "interruptive"))

interr <- unigrams_tf_idf %>% 
  filter(word %in% interrupted) #want to only analyze ires_words

# write.csv(interr, "")

######

discontinuous <- (c("discontinuous", "discont", "discontinuously", "discontinuousness", "discontin"))

discon <- unigrams_tf_idf %>% 
  filter(word %in% discontinuous) #want to only analyze ires_words

# write.csv(discon, "")

######

seasonal <- (c("seasonal", "season", "seasonally", "seasonalness"))

seas <- unigrams_tf_idf %>% 
  filter(word %in% seasonal) #want to only analyze ires_words

# write.csv(seas, "")

######

permanent <- (c("permanent", "semipermanent", "nonpermanent", "semipermanently", "nonpermanently"))

perm <- unigrams_tf_idf %>% 
  filter(word %in% permanent) #want to only analyze ires_words

# write.csv(perm, "")

######

perennial <- (c("perennial", "semiperennial", "nonperennial", "semiperennially", "nonperennially"))

peren <- unigrams_tf_idf %>% 
  filter(word %in% perennial) #want to only analyze ires_words

# write.csv(peren, "")

######

dry <- (c("dry", "dri", "dried", "drying", "drier", "driest"))

dri <- unigrams_tf_idf %>% 
  filter(word %in% dry) #want to only analyze ires_words

# write.csv(dri, "")

######

episodic <- (c("episodic", "episod", "episodically"))

epi <- unigrams_tf_idf %>% 
  filter(word %in% episodic) #want to only analyze ires_words

# write.csv(epi, "")


###################################################################
### selecting random papers for additional deinition analysis

setwd()

# read in the data sheets

library(tidyverse)

file.names <- # read in sheets

randomized <- NULL

for (i in 1:length(file.names)) {
  file <- read.csv(file.names[i], header = TRUE) 
  file <- tibble::rowid_to_column(file, "orig.nrow")
  file <- file %>% 
    plyr::mutate("search_term" = file.names[i]) %>%
    plyr::rename(c("AU" = "authors.full", "TI" = "title", 
                   "PY" = "year")) %>% 
    dplyr::select(c(orig.nrow, search_term, authors.full, title, year))
  r.file <- sample_n(file, 25)
  randomized <- rbind(randomized, r.file)
}

# just need to delete the "combined" values (-25)
randomized <- randomized[!(randomized$search_term == "combined_top_cited.csv"),]

# put into random order
randomized <- randomized[sample(nrow(randomized)),] # select top 25

# write.csv(randomized, file = "")


###################################################################
# after selecting papers with high tf_df values, download the full papers
# search for definition / collect other data
# now ready to analyze definitions

library(tidyverse)

# first need to rbind the first set of defs to the randomized set
def1 <- read.csv("", header = TRUE)[,-1]

# select data of interest
def1 <- def1[, c(1,2,24,25,30:38)]

def1 <- unique(def1)

# search term includes river vs stream...remove!
def1 <- def1 %>% 
  separate(col = "word", into = c("search_term", "NA"), sep = " ", remove = TRUE) %>% 
  select(-c("NA"))

# search_term are shortened, need to extend to match with def2
def1 <- def1 %>% 
  # arid fine!
  mutate(search_term = str_replace(search_term, "discontinu", "discontinuous")) %>% 
  mutate(search_term = str_replace(search_term, "dri", "")) %>% # this one is paper 51, should only be 50
  # dry fine!
  mutate(search_term = str_replace(search_term, "ephemer", "ephemeral")) %>% 
  mutate(search_term = str_replace(search_term, "episod", "episodic")) %>% 
  mutate(search_term = str_replace(search_term, "intermitt", "intermittent")) %>% 
  mutate(search_term = str_replace(search_term, "interrupt", "interrupted")) %>% 
  # irregular fine! 
  mutate(search_term = str_replace(search_term, "nonperenni", "perennial")) %>%
  mutate(search_term = str_replace(search_term, "nonperman", "permanent")) %>%
  mutate(search_term = str_replace(search_term, "season", "seasonal")) %>%
  mutate(search_term = str_replace(search_term, "semiperman", "permanent")) %>%
  mutate(search_term = str_replace(search_term, "temporari", "temporary"))

def1 <- unique(def1)

# check to make sure each epithet only appears once
data.frame(table(def1$search_term))

####

# load second round of definition papers (25 random)
def2 <- read.csv("", header = TRUE)

def2 <- def2[, c(3,4,23,24,29:37)]

def2 <- unique(def2)

# search term includes .csv
def2 <- def2 %>% 
  separate(col = "search_term", into = c("search_term", "NA"), sep = ".", remove = TRUE) %>% 
  select(-c("NA")) %>% 
  rename("authors" = "authors.full")
#####

# combine original and random definitions
definitions <- rbind(def1, def2)

####

# want to include original corpora for extra definitions that were present in papers
  # e.g. if an "arid" paper also defined the epithet "ephemeral", keep track of where definition originated (arid corpus)
definitions$ogcor <- definitions$search_term

# now you can separate out the extra terms, remove empty rows, and go from there with def analysis - LDA modeling
# in the end you want a single combined list of all defintions available
word1 <- definitions[, c(1:3,5,14)]
word2 <- definitions[, c(1,6,7,5,14)]
word3 <- definitions[, c(1,8,9,5,14)]
word4 <- definitions[, c(1,10,11,5,14)]
word5 <- definitions[, c(1,12,13,5,14)]

# rename so all colnames match
word2 <- word2 %>%
  rename("search_term" = "term2") %>%
  rename("definition_description" = "defin2")

word3 <- word3 %>%
  rename("search_term" = "term3") %>%
  rename("definition_description" = "defin3")

word4 <- word4 %>%
  rename("search_term" = "term4") %>%
  rename("definition_description" = "defin4")

word5 <- word5 %>%
  rename("search_term" = "term5") %>%
  rename("definition_description" = "defin5")

# remove empty rows
word1 <- word1[!(word1$search_term==""), ]
word2 <- word2[!(word2$search_term==""), ]
word3 <- word3[!(word3$search_term==""), ]
word4 <- word4[!(word4$search_term==""), ]
word5 <- word5[!(word5$search_term==""), ]

# bind them for analysis!
analysis <- rbind(word1, word2, word3, word4, word5)

# separate out papers that were not about IRES after inspection
analysis_ires_y <- analysis[!(analysis$definition_description == "none"), ]
analysis_ires_y <- analysis_ires_y[!(analysis_ires_y$definition_description == ""), ]
analysis_ires_y <- analysis_ires_y[!(analysis_ires_y$IRES. == "N"), ]


# write.csv(definitions, "C:/Users/mhope/Documents/What_R_IRES/Data/definitions.corpora.by.paper.csv")
# write.csv(analysis, "C:/Users/mhope/Documents/What_R_IRES/Data/all.definitions.corpora.csv")
# write.csv(analysis_ires_y,"C:/Users/mhope/Documents/What_R_IRES/Data/all.definitions.corpora.ires.only.csv")

####################################################################################
# questions to answer:

### 1. how many total papers for each term?
definitions <- read.csv("", header = TRUE)

definitions <- definitions[!(is.na(definitions$search_term) | definitions$search_term==""), ]

definitions <- unique(definitions)

data.frame(table(definitions$search_term))

####################################################################################

### 2. how many of each term not about IRES?
table(definitions$IRES.)

ires <- definitions[,c(2,5)]
table(ires)

####################################################################################

### 3. how many of each term with definition?
definz <- definitions[, c(3,4)]
length(which(definz$definition_description == "none")) # 365 without any defintions

# for individ descriptors...
def_arid <- definz[c(which(definz$search_term == "arid")), ]
length(which(def_arid$definition_description == "none")) # 35

def_disc <- definz[c(which(definz$search_term == "discontinuous")), ]
length(which(def_disc$definition_description == "none")) # 30

def_dry <- definz[c(which(definz$search_term == "dry")), ]
length(which(def_dry$definition_description == "none")) # 48

def_eph <- definz[c(which(definz$search_term == "ephemeral")), ]
length(which(def_eph$definition_description == "none")) # 25

def_epi <- definz[c(which(definz$search_term == "episodic")), ]
length(which(def_epi$definition_description == "none")) # 36

def_mit <- definz[c(which(definz$search_term == "intermittent")), ]
length(which(def_mit$definition_description == "none")) # 30

def_rupt <- definz[c(which(definz$search_term == "interrupted")), ]
length(which(def_rupt$definition_description == "none")) # 28

def_irreg <- definz[c(which(definz$search_term == "irregular")), ]
length(which(def_irreg$definition_description == "none")) # 29

def_pere <- definz[c(which(definz$search_term == "perennial")), ]
length(which(def_pere$definition_description == "none")) # 5

def_perm <- definz[c(which(definz$search_term == "permanent")), ]
length(which(def_perm$definition_description == "none")) # 17

def_sea <- definz[c(which(definz$search_term == "seasonal")), ]
length(which(def_sea$definition_description == "none")) # 52

def_temp <- definz[c(which(definz$search_term == "temporary")), ]
length(which(def_temp$definition_description == "none")) # 29


### also do when removing the papers not about IRES
Ydef <- definitions[c(which(definitions$IRES. == "Y")), ]

Ydef <- Ydef[, c(3,4)] # 452 total
length(which(Ydef$definition_description == "none")) # 176 without any defintions

# for individ descriptors...
def_arid <- Ydef[c(which(Ydef$search_term == "arid")), ] # 60
length(which(def_arid$definition_description == "none")) # 24

def_disc <- Ydef[c(which(Ydef$search_term == "discontinuous")), ] # 12
length(which(def_disc$definition_description == "none")) # 8

def_dry <- Ydef[c(which(Ydef$search_term == "dry")), ] # 50
length(which(def_dry$definition_description == "none")) # 26

def_eph <- Ydef[c(which(Ydef$search_term == "ephemeral")), ] # 74
length(which(def_eph$definition_description == "none")) # 24

def_epi <- Ydef[c(which(Ydef$search_term == "episodic")), ] # 12
length(which(def_epi$definition_description == "none")) # 2

def_mit <- Ydef[c(which(Ydef$search_term == "intermittent")), ] # 66
length(which(def_mit$definition_description == "none")) # 22

def_rupt <- Ydef[c(which(Ydef$search_term == "interrupted")), ] # 11
length(which(def_rupt$definition_description == "none")) # 6

def_irreg <- Ydef[c(which(Ydef$search_term == "irregular")), ] # 4
length(which(def_irreg$definition_description == "none")) # 3

def_pere <- Ydef[c(which(Ydef$search_term == "perennial")), ] # 28
length(which(def_pere$definition_description == "none")) # 6

def_perm <- Ydef[c(which(Ydef$search_term == "permanent")), ] #23
length(which(def_perm$definition_description == "none")) # 12

def_sea <- Ydef[c(which(Ydef$search_term == "seasonal")), ] # 45
length(which(def_sea$definition_description == "none")) # 23

def_temp <- Ydef[c(which(Ydef$search_term == "temporary")), ] # 67
length(which(def_temp$definition_description == "none")) # 21

# about IRES, final with definitions

#### START HERE INTERMITTENT POOLS?
# adding them to intermittent, talking about lotic systems, so we are good! 

analysis_ires_y <- read.csv("", header = TRUE)

analysis_ires_y <- analysis_ires_y[!(analysis_ires_y$IRES. == "N"), c(-1)]

analysis_ires_y <- analysis_ires_y[!(is.na(analysis_ires_y$search_term) | analysis_ires_y$search_term==""), ]

analysis_ires_y <- unique(analysis_ires_y)

data.frame(table(analysis_ires_y$search_term))


####################################################################################

#### definition theme analysis ####

library(tidyverse)
library(vegan)

themes <- read.csv("", headers = TRUE)

# need to sum up the occurrences for each theme

themeprop <- themes %>%
  replace(is.na(.), 0) %>%
  group_by(search_term) %>% 
  select(c(2,5:18))

themeprop <- aggregate(themeprop[,-1],themeprop["search_term"],sum)
# need to divide each one by the number of definitions... going to do it in excel cause...#easier

# write.csv(themeprop, "C:/Users/mhope/Documents/What_R_IRES/Data_Processed/definition_themes.csv")

## display the proportions
prop <- read.csv("", headers = TRUE)

prop <- prop %>% 
  remove_rownames %>% 
  column_to_rownames(var="search_term")

# need to transpose data frame so topics (communities) are rows and documents (species) are columns
prop_transpose <- as.data.frame(t(as.matrix(prop)))

rownames(prop_transpose) <- colnames(prop)
colnames(prop_transpose) <- rownames(prop[1])

nmds <- metaMDS(prop_transpose, k=2, trymax = 100)
stressplot(nmds)

plot(nmds)
ordiplot(nmds,type="n")
orditorp(nmds,display="species",col="black", air=0.01, 
         labels = c("arid", "discontinuous", "dry", "ephemeral", "episodic", "intermittent",
                    "interrupted", "irregular", "non-perennial", "non-permanent", "seasonal", "temporary"), cex = 1.5)
orditorp(nmds,display="sites", col="blue", air=0.01, cex = 1.5,
         labels = c("precip", "grnd", "season", "variab", 
                    "time", "scarcity", "land", "extreme",
                    "pod:low", "pod:no", "pod:pool", "pod:nosur",
                    "pod:nosub", "pod"))

# remove pod:nosub and scarcity, they are pretty far out on their own
# also remove irregular - only one def available
propmin <- prop_transpose[c(1:5,7:12,14),c(1:7,9:12)]

nmds2 <- metaMDS(propmin, k=2, trymax = 100)
nmds2$stress
scores(nmds2)
stressplot(nmds2)

# plot(nmds2)
ordiplot(nmds2,type="n")
orditorp(nmds2,display="species",col="black",air=0.01,
         labels = c("arid", "discontinuous", "dry", "ephemeral", "episodic", "intermittent",
                    "interrupted", "non-perennial", "non-permanent", "seasonal", "temporary"), cex = 2)
orditorp(nmds2,display="sites",cex=2, col = "blue", air=0.01, 
         labels = c("source: precipitation", "source: groundwater", "predictability", "variability", 
                    "time frame", "landform / climate", "extremes",
                    "pod:low flow", "pod:no flow", "pod:isolated pool", "pod:no surface water", 
                    "pod: not specific"))


##########################################################################
## reconnect back to WoS categories...
# HOW DO DIFFERENT FIELDS USE THE SAME TERMS?

library(tidyverse)

ires.def <- read.csv("", header = TRUE)[,-1]

full.wos <- read.csv("", header = TRUE)

names(full.wos)

au.wc <- full.wos[, c("AU", "WC", "PY")]

ires.def <- separate(ires.def, authors, into = c("AU", "year"), sep = "_", remove = TRUE)

def.cat <- merge(ires.def, au.wc, by.x = "AU", by.y = "AU")

def.cat <- def.cat[, -2]

def.cat <- unique(def.cat)

def.cat <- separate(def.cat, WC, into = c("cat1", "cat2", "cat3"), sep = ";", remove = TRUE)
# not sure why def.cat has more rows than ires.def... almost 40 more?

cat.arid <- def.cat[c(which(def.cat$search_term == "arid")), ]
cat.disc <- def.cat[c(which(def.cat$search_term == "discontinuous")), ]
cat.dry <- def.cat[c(which(def.cat$search_term == "dry")), ]
cat.ephem <- def.cat[c(which(def.cat$search_term == "ephemeral")), ]
cat.epi <- def.cat[c(which(def.cat$search_term == "episodic")), ]
cat.int <- def.cat[c(which(def.cat$search_term == "intermittent")), ]
cat.irreg <- def.cat[c(which(def.cat$search_term == "irregular")), ]
cat.rupt <- def.cat[c(which(def.cat$search_term == "interrupted")), ]
cat.perm <- def.cat[c(which(def.cat$search_term == "permanent")), ]
cat.peren <- def.cat[c(which(def.cat$search_term == "perennial")), ]
cat.temp <- def.cat[c(which(def.cat$search_term == "temporary")), ]
cat.sea <- def.cat[c(which(def.cat$search_term == "seasonal")), ]

# visually went through the 5 epithets that we focused on, carefully read each one
  # compiled categories together to create a summary definition for larger research fields
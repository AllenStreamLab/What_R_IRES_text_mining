#### O1 - difference in use among epithets between WoS categories  ####
            ## creating figure 1 ##

## adapted from 
# Jones, T.; Doane, W. Package “textmineR”; Functions for Text Mining and Topic Modeling; CRAN, 2019;
  # https://rdrr.io/cran/textmineR/

library(tidyverse)
library(ggplot2)

setwd("")
getwd()

# upload the abstract csv files - this is orig data downloaded from WoS
# to compare topics need to have separate files
intermitten <- read.csv("intermittent.csv", header = TRUE)
ephemer <- read.csv("ephemeral.csv", header = TRUE)
tempora <- read.csv("temporary.csv", header = TRUE)
interrupt <- read.csv("interrupted.csv", header = TRUE)
discontin <- read.csv("discontinuous.csv", header = TRUE)
season <- read.csv("seasonal.csv", header = TRUE)
perman <- read.csv("permanent.csv", header = TRUE)
perennial <- read.csv("perennial.csv", header = TRUE)
dry <- read.csv("dry.csv", header = TRUE)
episod <- read.csv("episodic.csv", header = TRUE)
irreg <- read_csv("irregular.csv")
arid <- read.csv("arid.csv", header = TRUE)

# break down categories, include search term col, select author / year / categories 1 and 2 / search term
inter_field <- intermitten %>% 
  separate(col = "WC", into = c("field1", "field2"), sep = ";", remove = TRUE) %>% 
  mutate("search_term" = "intermittent") %>% 
  select(c("AU", "PY", "field1", "field2", "search_term"))

ephem_field <- ephemer %>% 
  separate(col = "WC", into = c("field1", "field2"), sep = ";", remove = TRUE) %>% 
  mutate("search_term" = "ephemeral") %>% 
  select(c("AU", "PY", "field1", "field2", "search_term"))

tempora_field <- tempora %>% 
  separate(col = "WC", into = c("field1", "field2"), sep = ";", remove = TRUE) %>% 
  mutate("search_term" = "temporary") %>% 
  select(c("AU", "PY", "field1", "field2", "search_term"))

interrupt_field <- interrupt %>% 
  separate(col = "WC", into = c("field1", "field2"), sep = ";", remove = TRUE) %>% 
  mutate("search_term" = "interrupted") %>% 
  select(c("AU", "PY", "field1", "field2", "search_term"))

discontin_field <- discontin %>% 
  separate(col = "WC", into = c("field1", "field2"), sep = ";", remove = TRUE) %>% 
  mutate("search_term" = "discontinuous") %>% 
  select(c("AU", "PY", "field1", "field2", "search_term"))

season_field <- season %>% 
  separate(col = "WC", into = c("field1", "field2"), sep = ";", remove = TRUE) %>% 
  mutate("search_term" = "seasonal") %>% 
  select(c("AU", "PY", "field1", "field2", "search_term"))

perman_field <- perman %>% 
  separate(col = "WC", into = c("field1", "field2"), sep = ";", remove = TRUE) %>% 
  mutate("search_term" = "permanent") %>% 
  select(c("AU", "PY", "field1", "field2", "search_term"))

perennial_field <- perennial %>% 
  separate(col = "WC", into = c("field1", "field2"), sep = ";", remove = TRUE) %>% 
  mutate("search_term" = "perennial") %>% 
  select(c("AU", "PY", "field1", "field2", "search_term")) 

epis_field <- episod %>% 
  separate(col = "WC", into = c("field1", "field2"), sep = ";", remove = TRUE) %>% 
  mutate("search_term" = "episodic") %>% 
  select(c("AU", "PY", "field1", "field2", "search_term"))

dry_field <- dry %>% 
  separate(col = "WC", into = c("field1", "field2"), sep = ";", remove = TRUE) %>% 
  mutate("search_term" = "dry") %>% 
  select(c("AU", "PY", "field1", "field2", "search_term"))

irreg_field <- irreg %>% 
  separate(col = "WC", into = c("field1", "field2"), sep = ";", remove = TRUE) %>% 
  mutate("search_term" = "irregular") %>% 
  select(c("AU", "PY", "field1", "field2", "search_term"))

arid_field <- arid %>% 
  separate(col = "WC", into = c("field1", "field2"), sep = ";", remove = TRUE) %>% 
  mutate("search_term" = "arid") %>% 
  select(c("AU", "PY", "field1", "field2", "search_term"))

# combine them all
all_terms_all_fields <- rbind(discontin_field, dry_field, ephem_field, epis_field, inter_field, 
                              interrupt_field, perennial_field, perman_field, season_field, tempora_field,
                              irreg_field, arid_field)

# load file of author, year, categories, search term
all_terms_all_fields <- read.csv("", header = TRUE)

# create separate data frames so you can bind them together 
field1 <- all_terms_all_fields %>% 
  select("X", "AU", "PY", "field1", "search_term") %>% 
  rename("field" = "field1")
field2 <- all_terms_all_fields %>% 
  select("X", "AU", "PY", "field2", "search_term") %>% 
  rename("field" = "field2")

field_combined <- rbind(field1, field2)


#############################################################################################################################
library(tidyverse)
library(ggplot2)


fields_combined <- read.csv("", 
                            header = TRUE)

fields_combined$field <- gsub(' ', '', fields_combined$field) # to make sure no spaces are in the data
fields_combined$search_term <- gsub(' ', '', fields_combined$search_term)

# want to plot the number of fields for each topic
# need counts
fields <- fields_combined %>% 
  select("field", "search_term") %>% 
  count(field, search_term) %>% 
  na.omit(field) 

# get totals per field FOR GRAPHING IN ORDER
field_totals <- fields %>% 
  group_by(field) %>% 
  summarise(field_total = sum(n))

#combine to add total into fields!
final_fields <- merge(fields, field_totals, by = "field")

final_fields$field <- gsub(' ', '', final_fields$field) # to make sure no spaces are in the data
final_fields$search_term <- gsub(' ', '', final_fields$search_term)

# instead of "n" column need a proportion column
final_fields$prop <- final_fields$n / final_fields$field_total
final_fields$total <- 1

# plot it out!
library(colorRamps)
library(RColorBrewer)
library(EnvStats)

colourCount = length(unique(final_fields$search_term))
getPalette = colorRampPalette(brewer.pal(8, "Set2"))

# https://earlglynn.github.io/RNotes/package/RColorBrewer/index.html
  # color packages

# need to filter some out so readable, want categories with at least 50 papers
final_fields_plot <- final_fields %>% 
  filter(field_total > 49) %>% 
  rename("sample size" = "field_total")

final_fields_plot$search_term <- gsub("perennial", "non-perennial", final_fields_plot$search_term)
final_fields_plot$search_term <- gsub("permanent", "non-permanent", final_fields_plot$search_term)

final_fields_plot$field = with(final_fields_plot, reorder(field, `sample size`, max)) # REORDER!!!!


p1 <- ggplot(data = final_fields_plot,
             aes(x = field,
                 y = prop,
                 fill = search_term)) +
  scale_fill_manual(values = getPalette(colourCount)) +
  coord_flip() +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid = element_blank()) +
  # geom_text(aes(label = c(176, 214, 600, 153, 362, 123, 109, 94, 55, 161, 103, 1795, 57, 729, 382, 152, 2398, 69, 86, 370, 363, 
  #                       690, 236, 2065, 61, 132, 693, 1210, 150, 399, 72, 346, 252, 61, 618, 81, 110, 460, 1430, 402))) +
  # geom_text(aes(label = final_fields_plot$`sample size`), nudge_y = 0.5) +
  # stat_n_text() +
  # geom_col(color = "black", stat = "identity") + #adds boxes around diff stacks, looks really cluttered
  geom_col() +
  labs(color = "Epithet", x = "Web of Science Category", y = "Proportion") +
  guides(fill = guide_legend(title = "Epithet", reverse = TRUE)) +
  geom_text(aes(label = final_fields_plot$`sample size`, y=1.02, size = 30), show.legend = FALSE) + # adding the sample size!
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=30,face="bold"),
        legend.text = element_text(size=30),
        legend.title = element_text(size = 25, face = "bold"))

# using Fisher's exact test 

# first need to make a frequency table (rows = category, col = epithet)
library(reshape)
library(DescTools)
library(statmod)

table <- final_fields_plot[c(1,2,3)]

contable <- cast(table, field~search_term)

set.seed(20150828)   # For the sake of reproducibility.
fisher.test(contable, simulate.p.value = TRUE, B = 1e5) # too large, need to simulate p
install.packages("tidyverse")
library(tidyverse)

install.packages("readxl")
library(readxl)

#test


Kelly <- read_excel("Swedish-Kelly_M3_CEFR.xls", sheet = 2)

my_df <- as.data.frame(Kelly)

unique(my_df[c("Word classes\n")]) -> Grammar 

Kelly <- rename(Kelly, Grammar = "Gram-\nmar")

words <- Kelly[, c('Word classes\n')]

categories <- unique(Kelly$`Word classes\n`) 

numberOfCategories <- length(categories)

Kelly %>%
  filter(`Word classes\n` == "numeral") %>%
  count(`CEFR levels`) -> numeral 

Kelly %>%
    filter(`Word classes\n` == "adjective") %>%
    count(`CEFR levels`) -> adjective 

Kelly %>%
  filter(`Word classes\n` == "noun-en") %>%
  count(`CEFR levels`) -> noun_en
    
Kelly %>%
   filter(`Word classes\n` == "proper name") %>%
    count(`CEFR levels`) -> proper_name 

Kelly %>%
  filter(`Word classes\n` == "noun-ett") %>%
  count(`CEFR levels`) -> noun_ett

Kelly %>%
  filter(`Word classes\n` == "noun") %>%
  count(`CEFR levels`) -> noun 

Kelly %>%
  filter(`Word classes\n` == "conj") %>%
  count(`CEFR levels`) -> conj 

Kelly %>%
  filter(`Word classes\n` == "verb") %>%
  count(`CEFR levels`) -> verb 

Kelly %>%
  filter(`Word classes\n` == "prep") %>%
  count(`CEFR levels`) -> prep 

Kelly %>%
  filter(`Word classes\n` == "pronoun") %>%
  count(`CEFR levels`) -> pronoun 

Kelly %>%
  filter(`Word classes\n` == "det") %>%
  count(`CEFR levels`) -> det 

Kelly %>%
  filter(`Word classes\n` == "subj") %>%
  count(`CEFR levels`) -> subj 

Kelly %>%
  filter(`Word classes\n` == "aux verb") %>%
  count(`CEFR levels`) -> aux_verb 

Kelly %>%
  filter(`Word classes\n` == "adverb") %>%
  count(`CEFR levels`) -> adverb

Kelly %>%
  filter(`Word classes\n` == "particle") %>%
  count(`CEFR levels`) -> particle 

Kelly %>%
  filter(`Word classes\n` == "interj") %>%
  count(`CEFR levels`) -> interj 

Kelly %>%
  filter(`Word classes\n` == "particip") %>%
  count(`CEFR levels`) -> particip 

Kelly %>%
  filter((`Word classes\n` == "noun-ett") | (`Word classes\n` == "noun-en")) %>%
  count(`CEFR levels`) -> noun_en_ett 

gender_tibble <- full_join(prep, noun_en_ett)

data_frame = data.frame(col1 = unique(my_df[c("CEFR levels")]), 
                        col2 = (prep[c("n")]), 
                        col3 = (noun_en_ett[c("n")]))

data_frame <- rename(data_frame, CEFR_levels = `CEFR.levels`)
data_frame <- rename(data_frame, n = `n`)
data_frame <- rename(data_frame, n1 = `n.1`)

ggplot(data_frame,
       aes(x = CEFR_levels,
           y = n,
           group = 1
           )) + geom_line()

ggplot(data_frame,
       aes(x = CEFR_levels,
           y = n1,
           group = 1
       )) + geom_line()

p = ggplot() + 
  geom_line(data = data_frame, aes(x = CEFR_levels, y = n, group = 1), color = "blue") +
  geom_line(data = data_frame, aes(x = CEFR_levels, y = n1, group = 1), color = "red") +
  xlab('CEFR_levels') +
  ylab('change')

print(p)

# Here I actually wanted to be able to show what the lines portray but I am not able. 
# The red is nouns and the blue prepositions. 




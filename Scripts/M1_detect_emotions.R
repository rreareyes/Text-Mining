# DETECT EMOTIONS

# This script uses the package sentimentr to assess the emotional content
# in sentences about the prospects related to a rival candidate would win
# the presidential election
# 
# It returns a dataframe where each row represents an emotion detected
# within their response. It displays the total number of words on their 
# submission, and the number of emotion phrases detected in it. 
# 
# Additionally, it provides an emotional rate per sentence, which is then
# averaged in the whole submission. This emotional rate is based in a basic
# dictionary lookup to find the emotion words. This rate ranges from 0 to 1.
# The dictionary used is a filtered version of the NRC emotion lexicon from
# Mohammad & Turney (2010).

# Load necessary packages -------------------------------------------------
require(tidyverse)
require(ggplot2)
require(sentimentr)

# Define base directories -------------------------------------------------
folder_root <- dirname(dirname(rstudioapi::getActiveDocumentContext()$path))
folder_data <- file.path(folder_root, "Data")

# Load base data ----------------------------------------------------------
raw_data <- read.csv(file = file.path(folder_data, "emotion_prime_scoring.csv")) %>% 
  select(1:3) %>% 
  rename("subject" = 1, "candidate" = 2, "response" = 3) %>% 
  na.omit() %>% 
  filter(response != "")


# Split sentences and perform emotional detection -------------------------
emotional_data <- raw_data %>%
  get_sentences() %>% 
  emotion_by(response, by = c("subject", "candidate")) %>% 
  filter(emotion_count != 0) %>% 
  mutate(candidate = as.factor(candidate),
         emotion_type = as.factor(emotion_type))


candidate_data <- emotional_data %>% 
  group_by(candidate) %>% 
  mutate(members = length(unique(subject))) %>% 
  ungroup() %>% 
  group_by(candidate, emotion_type, members) %>% 
  summarise(n_emotion = n(), proportion = n_emotion/members) %>% 
  ungroup() %>% 
  unique() %>% 
  mutate(emotion_type = fct_reorder(emotion_type, proportion)) %>% 
  filter(!str_detect(emotion_type, "negated"))
  
# Plot the data -----------------------------------------------------------
ggplot(data = candidate_data) +
  geom_col(aes(x = emotion_type,
               y = proportion,
               fill = emotion_type)) +
  facet_grid(.~candidate) +
  
  theme_bw() +
  
  scale_fill_brewer(palette = "Dark2")





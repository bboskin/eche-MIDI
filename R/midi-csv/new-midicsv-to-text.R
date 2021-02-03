library(tidyverse)
library(magrittr)
library(naniar)
library(gtools)

midi_raw <- read_csv("input/complex.csv")

# First stripping away uneccesarry rows and making column names better.  recoding "on" and "off"
midi_clean <-
  midi_raw %>% 
  magrittr::extract(,c(2, 3, 5)) %>% 
  set_names(c("time", "state", "note")) %>% 
  filter(state == "Note_on_c" | state == "Note_off_c") %>% 
  mutate(state = str_replace(state, "Note_on_c", "on"),
         state = str_replace(state, "Note_off_c", "off")) %>% 
  arrange(time, state) %>%
  mutate(note = chr(note))

# creating a data frame for the data to populate to
midi_reformat <- tibble(time = 0:max(midi_clean$time))

# creating a column for each note
for(column in midi_clean %>% count(note) %>% pull(note)){
  midi_reformat <- midi_reformat %>% 
                      mutate(!!column := midi_clean %>% 
                                            filter(note == !!column) %>% 
                                            bind_rows(midi_clean %>% 
                                                        filter(note == !!column) %>% 
                                                        select(time) %>% 
                                                        anti_join(tibble(time = 0:max(midi_clean$time)), .) %>% 
                                                        mutate(state = NA, note = NA)) %>% 
                                            arrange(time) %>% 
                                            transmute(note = paste(note, state, sep = ";")) %>% 
                                            na_if("NA;NA") %>% 
                                            fill(note) %>% 
                                            mutate(note = case_when(str_detect(note, "off") == TRUE ~ NA_character_,
                                                                    TRUE ~ note)) %>% 
                                            mutate(note = str_remove(note, ";on")) %>% 
                                            pull(note))
}

# Squashing all the columns together.  resulting df has two columns
midi_complete <-
  midi_reformat %>% 
    transmute(time, notes = midi_reformat %>% 
                              select(-time) %>% 
                              unite(notes, sep = "") %>% 
                              mutate(notes = str_remove_all(notes, "NA")) %>% 
                              na_if("") %>% 
                              pull(notes))

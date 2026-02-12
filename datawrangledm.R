library(tidyverse)
library(readxl)
library(tidyr)
library(dplyr)


bc_macro_lengths = read_csv("data/bc macro lengths.csv") %>% 
  mutate(order = str_to_lower(order),
         order = case_when(order == "odanata" ~ "odonata",
                           order == "nematode" ~ "oligochaeta",
                           TRUE ~ order))


macro_lw_coeffs = read_csv("data/macro_lw_coeffs.csv") %>% 
  mutate(order = case_when(taxon == "Oligochaeta" ~ "oligochaeta",
                           TRUE ~ order)) %>% 
  filter(is.na(family)) %>% 
  mutate(order = str_to_lower(order))

#converting lengths of macroinvertebrates into weights
macromass = left_join(bc_macro_lengths, macro_lw_coeffs) %>% 
    mutate(dm = a*length^b)

ggplot(aes(x= length, y = dm), data=macromass) + 
  geom_point()

saveRDS(macromass, file = "data/macromass.rds")

#compiling sediment distributions with macro lengths

macromass <- readRDS("data/macromass.rds")

sdvalues <- read.csv("data/bc sd dvalues.csv")
#changing the names of the sites to be in the same format as macromass
  sd_tidy <- sdvalues %>%
    pivot_longer(
      cols = -X,
      names_to = "site_section",
      values_to = "value"
    ) %>%
    extract(
      site_section,
      into = c("site", "section"),
      regex = "site\\.(\\d+)\\.s(\\d+)"
    ) %>%
    mutate(
      site = as.integer(site),
      section = as.integer(section)
    ) %>%
    pivot_wider(
      names_from = X,      # D5, D16, D50, D84
      values_from = value
    )
  
#joining all of the data together so that I can run the model like ch 1
bcdata <- macromass %>% left_join(sd_tidy, by = c("site", "section"))
saveRDS(bcdata, file = "data/bcdata")  
  
  
  
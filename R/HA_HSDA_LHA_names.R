# the purpose of this script is to scrape the local health authority names
# from a text scrape of a bccdc page .

library(tidyverse)
library(stringr)

step1 <-
  readr::read_lines(here::here("data/raw_text.txt")) %>% tibble() %>%
  set_names("input_line") %>%
  filter(str_length(input_line) > 0) %>%
  mutate(
    firstbyte  =   stringr::str_sub(input_line, 1, 1),
    secondbyte =   stringr::str_sub(input_line, 2, 2),
    thirdbyte  =   stringr::str_sub(input_line, 3, 3),
    fourthbyte =   stringr::str_sub(input_line, 4, 4)
    
  )
# this is awkward.. there must an easier way using $start.. but I could not see it.


a <- str_locate(step1$input_line, "PDF")
step1$PDF_start = a[, 1]


step2 <- step1 %>%
  mutate(newname = case_when(!is.na(PDF_start) ~ str_trim(str_sub(
    input_line, 1, PDF_start - 2
  )),
  TRUE              ~ input_line))


HA   <- step2 %>% filter(str_detect(secondbyte, " ")) %>%
  mutate(HA_number = str_sub(newname, 1, 1),
         HA_name = str_sub(newname, 3)) %>%
  mutate(
    HA_number = case_when(
      str_detect(input_line, "Alberni") ~ '4',
      TRUE                            ~ HA_number
    )
  ) %>% 
  select(dplyr::starts_with("HA"))


HSDA <- step2 %>%
  filter(str_detect(thirdbyte, " ")) %>%
  mutate(
    HA_number = str_sub(newname, 1, 1),
    HSDA_number = str_sub(newname, 1, 2),
    HSDA_name = str_sub(newname, 4)
  ) %>%
  mutate(
    HSDA_number = case_when(
      str_detect(input_line, "Alberni") ~ '42',
      TRUE                            ~ HSDA_number
    )
  ) %>% 
  
  select(dplyr::starts_with("H")) %>% 
  left_join(.,HA, by="HA_number") %>% 
  group_by(HSDA_number) %>% slice(1) %>% ungroup()


LHA  <- step2 %>% 
  filter(str_detect(fourthbyte, " ")) %>%
  mutate(
    HSDA_number      = str_sub(newname, 1, 2),
    LHA_number       = str_sub(newname, 1,3),
    LHA_name         = str_trim(str_sub(newname, 5)),
  ) %>% 
  mutate(
    LHA_number = case_when(
      str_detect(LHA_name, "Alberni") ~ '426',
      TRUE                            ~ LHA_number
      )
    ) %>% 
  mutate(
    HSDA_number = case_when(
      str_detect(LHA_name, "Alberni") ~ '42',
      TRUE                            ~ HSDA_number
    )
  ) %>% 
    
     select(LHA_name, LHA_number, HSDA_number)%>% 
     inner_join(., HSDA, by="HSDA_number")

# expect 89

notLHA  <- step2 %>% 
  filter(str_detect(fourthbyte, " ")) %>%
  mutate(
    HSDA_number      = str_sub(newname, 1, 2),
    LHA_number       = str_sub(newname, 1,3),
    LHA_name         = str_trim(str_sub(newname, 5)),
  ) %>% select(LHA_name, LHA_number, HSDA_number) %>% 
  anti_join(., HSDA, by="HSDA_number")

# expect 0

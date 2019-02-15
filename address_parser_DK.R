
library(tidyverse)

df <- read_delim("adresser.csv",delim=";") %>%
  select(SamletAdresse)

addr <- df$SamletAdresse

addr <- str_remove_all(addr,"DK-")

col1 <- str_split(a,"\n") %>% map_chr(1)
col2 <- str_split(a,"\n") %>% map_chr(2)
col3 <- str_split(a,"\n") %>% map_chr(3)
col4 <- str_split(a,"\n") %>% map_chr(tail,n=1)

df <- tibble(col1,col2,col3,col4)

df <- df %>% filter(col4 == "Danmark")

co_addrs <- df %>%
  filter(str_detect(col1,"c/o")) 

df <- df %>% anti_join(co_addrs)

co_addrs <- co_addrs %>% 
  select(col1 = col2, col2 = col3, col3 = col4) %>%
  mutate(col4 = "Danmark")

df <- rbind(df,co_addrs)

postnr1 <- df$col2 %>%
  str_extract("\\-*\\d+\\.*\\d*") %>%
  as.numeric()

postnr2 <- df$col3 %>%
  str_extract("\\-*\\d+\\.*\\d*") %>%
  as.numeric()

postnr <- ifelse(is.na(postnr1),postnr2,postnr1)

df <- df %>%
  mutate(col2 = str_remove(col2,"\\-*\\d+\\.*\\d *"),
         col3 = str_remove(col2,"\\-*\\d+\\.*\\d *"),
         postnr = postnr) %>%
  rename(bynavn = col2,
         land = col4) %>%
  select(-col3)

bynavn <- df$bynavn
land <- df$land

gade <- df$col1 %>% 
  str_remove_all("[:digit:]") %>%
  str_split(",") %>% 
  map(1) %>% 
  str_remove_all("(^| ).( |$)") %>% 
  str_trim()

husnr <- df$col1 %>%
  str_split(",") %>%
  map(1) %>%
  str_extract_all("[:digit:]") %>%
  sapply(paste, collapse = "")
  
  bogstav <- df$col1 %>%
  str_split(",") %>%
  map(1) %>%
  str_extract_all("[:digit:].*") %>%
  str_remove_all("[:digit:]") %>%
  replace(bogstav =="",NA)

etage <- df$col1 %>%
  str_split(",") %>%
  map(2) %>% 
  str_trim() %>%
  str_split(" ") %>%
  map(1) %>%
  unlist() %>%
  replace(etage == "NULL",NA)

dør <- df$col1 %>%
  str_split(",") %>%
  map(2) %>% 
  str_trim() %>%
  str_split(" ") %>%
  map(tail,n=1) %>%
  unlist() %>%
  replace(dør == "NULL",NA)

new_df <- tibble(gade, husnr, etage, dør, postnr, bynavn, land)

new_df

write_csv(new_df,"nye_adresser.csv")

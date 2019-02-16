# Parsing Danish addresses


library(tidyverse)

data <- read_delim("adresser.csv",delim=";") 

# The easiest column to work from
addr <- data$SamletAdresse

# remove postal code suffix
addr <- str_remove_all(addr,"DK-")

# split up the column and create new df
col1 <- str_split(addr,"\n") %>% map_chr(1)
col2 <- str_split(addr,"\n") %>% map_chr(2)
col3 <- str_split(addr,"\n") %>% map_chr(3)
col4 <- str_split(addr,"\n") %>% map_chr(tail,n=1)

df <- tibble(col1,col2,col3,col4)

# keep only Danish addresses
df <- df %>% filter(col4 == "Danmark")

# store c/o addresses in new variable
co_addrs <- df %>%
  filter(str_detect(col1,"c/o")) 

# remove c/o's from df
df <- df %>% anti_join(co_addrs)

# shift columns to the left
co_addrs <- co_addrs %>% 
  select(col1 = col2, col2 = col3, col3 = col4) %>%
  mutate(col4 = "Danmark")

# add back again to df
df <- rbind(df,co_addrs)

# get postal code from two columns
postnr1 <- df$col2 %>%
  str_extract("\\-*\\d+\\.*\\d*") 

postnr2 <- df$col3 %>%
  str_extract("\\-*\\d+\\.*\\d*") 

postnr <- ifelse(is.na(postnr1),postnr2,postnr1)

# update df
df <- df %>%
  mutate(col2   = str_remove(col2,"\\-*\\d+\\.*\\d *"),
         col3   = str_remove(col2,"\\-*\\d+\\.*\\d *"),
         postnr = postnr) %>%
  rename(bynavn = col2,
         land   = col4) %>%
  select(-col3)

# get city name
bynavn <- df$bynavn

# country
land <- df$land

# street name
gade <- df$col1  %>% 
  str_remove_all("[:digit:]")   %>%
  str_split(",") %>% 
  map(1)         %>% 
  str_remove_all("(^| ).( |$)") %>% 
  str_trim()

# house number
husnr <- df$col1 %>%
  str_split(",") %>%
  map(1)         %>%
  str_extract_all("[:digit:]") %>%
  sapply(paste, collapse = "")
  
# letter
bogstav <- df$col1 %>%
  str_split(",")   %>%
  map(1)           %>%
  str_extract_all("[:digit:].*") %>%
  str_remove_all("[:digit:]")

bogstav <- bogstav %>% replace(bogstav == "",NA)

# floor
etage <- df$col1 %>%
  str_split(",") %>%
  map(2)         %>% 
  str_trim()     %>%
  str_split(" ") %>%
  map(1)         %>%
  unlist()

etage <- etage %>% replace(etage == "NULL",NA)

# door
door <- df$col1  %>%
  str_split(",") %>%
  map(2)         %>% 
  str_trim()     %>%
  str_split(" ") %>%
  map(tail,n=1)  %>%
  unlist()

door <- door %>% replace(door == "NULL",NA)

# put together a new df
new_df <- tibble(gade, husnr, bogstav, etage, door, postnr, bynavn, land)

new_df

# write new file
write_csv(new_df,"nye_adresser.csv")

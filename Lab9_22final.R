#reading library packages
library(dplyr)
library(readr)
library(tidyr)

#reading in ipums data
a <- read_csv('./data_lab_2/9_22.csv')
#filter alaska/hawaii (code)
aa <- a %>% filter(!(STATEFIP %in% c(2,15)))
#Read in crosswalk, creating dataframe named "raced"
raced <- read_csv('./data_lab_2/raced.csv', col_types = cols(RACED='i'))
#joining raced and a
b <- left_join(aa, raced, by='RACED')
#aggregateing population
c <- b %>% group_by(RACEE,YEAR) %>% summarise(NUMBER = sum(PERWT))
#Spread year to be on the top, number below that, and race along the left side
d <- c %>% spread(YEAR, NUMBER)
#export dataframe to excell
write_csv(d,'./data_lab_2/raced_data.csv')

#30.05 notes
library(dplyr)
library(tidyr)
library(readr)

#reading in ipums data
a <- read_csv('./data_lab_2/9_22.csv')

#naming codes from codebook to read name of value (Alabama instead of 1)
#character variable, the value in R stores is the character strings
#in a factor variaboe, R stores a numeric value and a set of strings,
#runs faster
#character variable for SEX
b <- a %>% mutate(SEXC=ifelse(SEX==1,'male','female'))
#alternative
b <- a %>% 
#factor variable
c <- b %>% mutate(SEXF=factor(SEX,labels=c('male','female')))
#applied to race
d <- c %>% mutate(RACEF=factor(RACE,labels=c('White','Black',
        'American Indian or Alaska Native','Chinese',
        'Japanese','Other Asian of Pacific Islander','Other')))
#determine which RACE values are present in the dataset
table(c$RACE)
#shows histogram of race dispersion

#how do you do this with very many variables like state: crosswalk
#see slides
#population count
g <- f %>% group_by(YEAR) %>% summarise(number = sum(PERWT))
g <- f %>% group_by(YEAR, SEXF) %>% summarise(number = sum(PERWT))
#exporting data table
#one row for each year and one column for each sex
j <- i %>% spread(SEXF, NUMBER)
#export to csv
write_csv(j,'./data_lab_2/year_sex.csv')
#recoding
#starting with dataframe f, create a new REGION variable
#see slides
#ipums did not collect data from alaska nor hawaii
#in 1870, 1880, and 1940
#so dont use alaska and hawaii data pre 1960

#writing about methods and results

#2 paragraphs
#one is methods paragraph
  #explains what the table is and how it was created
#second is results paragraph
  #explain data, give reason for numbers, and citing sources






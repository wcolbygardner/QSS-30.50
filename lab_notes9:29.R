#library(dplyr)
#library(readr)
#library(ggplot2)
#library(RColorBrewer)

#read in ipums data
read_csv('./9_29.csv') %>% filter(AGE>=15 & AGE<=65 & !(STATEFIP %in% c(2,15)))
#character variables apear in alphabeticalorder where
#as factor variables will be in number order
#Ergo, you have more control over factor variables

b <- a %>% mutate(Region=factor
                  (ifelse(STATEFIP %in% c(4,6,8,16,30,32,35,41,49,53,56), 4,
                 ifelse(STATEFIP %in% c(17,18,19,20,26,27,29,31,38,39,46,55),3,
                   ifelse(STATEFIP %in% c(9,23,25,33,34,36,42,44,50),1,2))),
                 labels=c('Northeast','South','Midwest','West')))
c <- b %>% mutate(Industry=factor(ifelse(IND1950<100 | IND1950>976, 1
                                  ifelse(IND1950<246, 2,
                                         ifelse(IND1950==246 | IND1950=0076,4,
                                                ifelse(IND1950>))))))
#recode SEX
d <- c %>% mutate (Sex=ifelse(SEX=1,'Male','Female'))
e <- d select(YEAR,PERWT,Region,Industry,Sex)
#planning on making 2 graphs
f1 <- e %>% group_by(YEAR, Sex, Region) %>% summarise(Number=sum(PERWT))
f3 <- e %>% group_by(YEAR, Sex, Region, Industry) %>% summarise(Number=sum(PERWT))

#ploting figure 1
ggplot(data=f1,aes(x=YEAR,y=NUMBER,fill=Sex)) +
  geom_bar(stat='identity') +
  labs(x='Year',y='Population',fill='Sex',title='1. Population Aged 15-65 by Region, Year, and Sex, 1870-1920') +
  scale_y_continuous(labels=scales::comma) +
  scale_x_continuous(breaks=c(1870,1900,1920)) +
  scale_fill_brewer(palette='set2',guide=guide_legend(reverse=TRUE))+
  facet_wrap(~Region,ncol=2,scales='free_y') +
  theme_bw()
dev.off()
#plotting figure 3
ggplot(data=arrange(f3,Industry),aes(x=YEAR,y=NUMBER,fill=Industry)) +
  geom_bar(stat='identity', position='fill') +
  labs(x='Year',y='Percent of Population',fill='Industry',title='3. Industry for Persons Aged 15-65 by Region, Year, and Sex, 1870-1920') +
  scale_y_continuous(labels=scales::percent) +
  scale_x_continuous(breaks=c(1870,1900,1920)) +
  scale_fill_brewer(palette='set2')+
  facet_grid(Sex~.~Region) +
  theme_bw() + theme(legend.position='bottom')
  
  



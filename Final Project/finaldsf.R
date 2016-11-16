#read in librarys
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(RColorBrewer)
#read in data
aa <- read_csv('Final/Race:OCC.csv',col_types=cols(PERWT=col_double(),HHWT=col_double()))
#create Race variable
raced <- read_csv('Final/raced_copy1.csv', col_types = cols(RACED='i'))
b <- left_join(aa, raced, by='RACED')
#aggregateing population
c <- b %>% group_by(RACEC,YEAR) %>% summarise(NUMBER = sum(PERWT))
#Spread year to be on the top, number below that, and race along the left side
d <- c %>% spread(YEAR, NUMBER)
#graph
ggplot(data=c,aes(x=YEAR,y=NUMBER))+
  ggtitle('Population of Each Race 1900-1940')+
  geom_bar(stat='identity')+
  scale_y_continuous(labels=scales::comma)+
  facet_wrap(~RACEC,scales='free_y',ncol=5)

#Womens Occupation Graphs 

#read in librarys
library(dplyr)
library(readr)
library(ggplot2)
library(RColorBrewer)
#read in data
a2 <- read_csv('Final/Race:OCC.csv',col_types=cols(PERWT=col_double(),HHWT=col_double())) %>% filter(AGE>=15 & AGE<=65)
#create Race variable
b1 <- a2 %>% mutate(Race=factor
                    (ifelse(RACED %in% c(100,150), 1,
                            ifelse(RACED %in% c(200,210),2,
                                   ifelse(RACED %in% c(300),3,
                                          ifelse(RACED>400 & RACED<700,4,5)))),
                    labels=c('white','black','Native American','Asian',"Other")))
b2 <- b1 %>% filter(!Race=='Other')
#create occupation variable
c2 <- b2 %>% mutate(Occupation=factor(ifelse(OCC1950>979, 1,
                                             ifelse(OCC1950<100, 6,
                                                    ifelse(OCC1950<124 | OCC1950==810 | OCC1950==820 | OCC1950==830 | OCC1950==840, 2,
                                                           ifelse(OCC1950<491, 3,
                                                                  ifelse(OCC1950<691 | OCC1950>909,4, 5))))),
                                      labels=c('none','farmers and farm laborers','craftsmen/operatives/laborers',
                                               'managerial/clerical/sales','service','professional')))
#recoding sex
d2 <- c2 %>% mutate(Sex=ifelse(SEX==2,'Women','Men'))
dd2 <- d2 %>% filter(Sex=='Women')
#selecting only variables i need for graph
e2 <- dd2 %>% select(YEAR,PERWT,Race,Occupation)
#Planning graphing for f2 and f4
#planning on making 2 graphs
t1 <- e2 %>% group_by(YEAR, Race) %>% summarise(Number1=sum(PERWT))
f1 <- e2 %>% group_by(YEAR, Race, Occupation) %>% summarise(Number=sum(PERWT))
#graphing f2
ggplot(data=f1,aes(x=YEAR,y=Number,fill=Occupation)) +
  geom_bar(stat='identity',position='fill') +
  labs(x='Year',y='Percent of Population',fill='Occupation',title='Womens Occupation Percentage Aged 15-65 by Race, Year, and Sex, 1870-1920') +
  scale_y_continuous(labels=scales::percent) +
  scale_x_continuous(breaks=c(1900,1920,1940)) +
  scale_fill_brewer(palette='Set2',guide=guide_legend(reverse=TRUE))+
  facet_wrap(~Race,ncol=2,scales='free_y') +
  theme_bw()
#create data table, filter for farmers
table <- f1 %>% filter(Occupation=='farmers and farm laborers') 
table2 <- left_join(t1,table)
#create percentage(representation of graph) that i later didnt use
table3 <- table2 %>% mutate(pct=Number/Number1*100)
#format table of data
table4 <- table3 %>% select(Number,YEAR,Race,Occupation) %>% group_by(Race,YEAR) %>% spread(YEAR, Number)
#export dataframe to excell
write_csv(d,'./Final/raced_data.csv')

#Ancestory
library(readr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(ggmap)
library(maptools)
library(gtools)
#read in map data
mapdata <- read_csv('data/map.csv')=
  map1 <- ggplot() + theme_nothing(legend=TRUE) +
  geom_polygon(data=mapdata, aes(x=long,y=lat,group=group),fill='white',color='black')
png('map.png',width=1500,height=1000)
print(map1)
dev.off()
#read in data
ipums <- read_csv('Final/Anc1.csv',col_types = cols(PERWT=col_double()))
anc <- read_csv('Final/Ancode.csv',col_types = cols(ANCESTR1='i'))

#summerise for each ancestry in each year
anc1 <- ipums %>% group_by(YEAR,STATEFIP,ANCESTR1) %>% summarise(Number=sum(PERWT))

#filter for non responses
anc11 <- anc1 %>% filter(!ANCESTR1==999)

#create variable for most common ancestry
anc2 <- anc11 %>% group_by(YEAR,STATEFIP) %>% mutate(MAX=max(Number))
anc3 <- anc2 %>% filter(MAX==Number)

#join data with labels for ancestry variable
anc4 <- anc3 %>% left_join(anc,by='ANCESTR1')

#graph maps
mapdata$STATEFIP <- as.numeric(mapdata$STATEFIP)
anc4$STATEFIP <- as.numeric(anc4$STATEFIP)
anc5 <- left_join(anc4, mapdata, by = "STATEFIP") %>% arrange(order)

for (year in unique(anc4$YEAR)) {
  map2 <- map1 +scale_fill_brewer(palette='Set1')+
    ggtitle(paste('Map of Ancestry in',year))+
    geom_polygon(data=filter(anc5,YEAR==year), 
                 aes(x=long,y=lat,group=group,fill=Ancestry),color='black') 
  png(paste(('map_'),year,'.png',sep=''),width=1500,height=1000) 
  print(map2) 
  dev.off()
} 
print(map2)

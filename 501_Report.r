# packages required

# require()
install.packages('countrycode')
install.packages('raster')
install.packages('arm')
library(tidyverse)
library(ggplot2)
library(raster)
library(countrycode)
library(lme4)
library(lmerTest)
library(sjPlot)
library(sjmisc)
library(sjstats)
library(arm)

# read_dataset

mygpdcsv <- read_csv(file = "C://Users//mhwan//Desktop//501//data//gdpc.csv")
mynetcsv <- read_csv(file = "C://Users//mhwan//Desktop//501//data//netu.csv")

# select year 2014-2018

gdpyear <- dplyr::select(mygpdcsv, 'country', '2014', '2015', '2016', '2017', '2018')
netyear <- dplyr::select(mynetcsv, 'country', '2014', '2015', '2016', '2017', '2018')

# impute the missing data
# drop rows if there is NA value in year 2014

gdpna <- tidyr::drop_na(gdpyear, '2014')
netna <- tidyr::drop_na(netyear, '2014')


# LAST OBSERVATION CARRIED FORWARD
gdpna[is.na(gdpna$`2015`), '2015'] = gdpna[is.na(gdpna$`2015`), '2014']
gdpna[is.na(gdpna$`2016`), '2016'] = gdpna[is.na(gdpna$`2016`), '2015']
gdpna[is.na(gdpna$`2017`), '2017'] = gdpna[is.na(gdpna$`2017`), '2016']
gdpna[is.na(gdpna$`2018`), '2018'] = gdpna[is.na(gdpna$`2018`), '2017']

netna[is.na(netna$`2015`), '2015'] = netna[is.na(netna$`2015`), '2014']
netna[is.na(netna$`2016`), '2016'] = netna[is.na(netna$`2016`), '2015']
netna[is.na(netna$`2017`), '2017'] = netna[is.na(netna$`2017`), '2016']
netna[is.na(netna$`2018`), '2018'] = netna[is.na(netna$`2018`), '2017']

# Rename the columns before combine datasets

namgdp <- dplyr::rename(gdpna, 
                        '2014gdp/c'='2014',
                        '2015gdp/c'='2015',
                        '2016gdp/c'='2016',
                        '2017gdp/c'='2017',
                        '2018gdp/c'='2018')

namnet <- dplyr::rename(netna, 
                        '2014IAR'='2014',
                        '2015IAR'='2015',
                        '2016IAR'='2016',
                        '2017IAR'='2017',
                        '2018IAR'='2018')

# join the datasets

temptidy <- dplyr::inner_join(namnet, namgdp)

# calculate mean value and add to temp

temptidy <- dplyr::mutate(temptidy,'gdp_c' = (temptidy$`2014gdp/c`+
                            temptidy$`2015gdp/c`+
                            temptidy$`2016gdp/c`+
                            temptidy$`2017gdp/c`+
                            temptidy$`2018gdp/c`)/5)

temptidy <- dplyr::mutate(temptidy,'IAR' = (temptidy$`2014IAR`+
           temptidy$`2015IAR`+
           temptidy$`2016IAR`+
           temptidy$`2017IAR`+
           temptidy$`2018IAR`)/5)

temptidy <- dplyr::select(temptidy, 'country', 'gdp_c', 'IAR')

# grouped the country by continent for further analysis

temptidy <- as.data.frame(temptidy)
temptidy$continent=countrycode(
  sourcevar = temptidy[,"country"], 
  origin = "country.name",
  destination = "continent"
)

#tidy dataset is created
tidydata <- as_tibble(temptidy)


write.table(tidydata,"tidydata.csv",row.names=FALSE,col.names=TRUE,sep=",")

# Continent data
contidata <- tidyr::drop_na(tidydata,'continent')
contidata <- contidata %>% group_by(continent) %>% summarise(GDP=mean(gdp_c), IAR=mean(IAR))

gdp_con <- ggplot(
  data = contidata,
  mapping = aes(
    x = continent,
    y = GDP
  )) +
  geom_bar(stat = 'identity', fill='lightgreen',colour='white') +
  labs(x = "Continent", y = "GDP per Capita") +
  ggtitle("Average GDP/Capita in Different Continents")

IAR_con <- ggplot(
  data = contidata,
  mapping = aes(
    x = continent,
    y = IAR
  )) +
  geom_bar(stat = 'identity', fill='lightblue',colour='white') +
  labs(x = "Continent", y = "Internet Availability Rate") +
  ggtitle("Average IAR in Different Continents")

# Analysis

PtG_reg <- ggplot(
  data = tidydata,
  mapping = aes(
    x = IAR,
    y = gdp_c,
    colour = continent
  )) +
  geom_point() +
  labs(x = "Internet Availability Rate", y = "GDP per Capita") +
  ggtitle("Scatterplot by Continent")

PtG_smo <- ggplot(
  data = tidydata,
  mapping = aes(
    x = IAR,
    y = gdp_c,
  )) +
  geom_point() +
  stat_smooth() +
  labs(x = "Internet Availability Rate", y = "GDP per Capita") +
  ggtitle("Smooth modeling")

PtG_li <- ggplot(
  data = tidydata,
  mapping = aes(
    x = IAR,
    y = gdp_c,
  )) +
  geom_point() +
  stat_smooth(method = 'lm') +
  labs(x = "Internet Availability Rate", y = "GDP per Capita") +
  ggtitle("Linear modeling")

PtG_po <- ggplot(
  data = tidydata,
  mapping = aes(
    x = IAR^3,
    y = gdp_c,
  )) +
  geom_point() +
  stat_smooth(method = 'lm') +
  labs(x = "(Internet Availability Rate)^3", y = "GDP per Capita") +
  ggtitle("Linear modeling between GDP/Capia and the three power of IAR")

PtG_res <- ggplot(
  data = tidydata,
  mapping = aes(
    x = IAR,
    y = gdp_c,
    colour = continent
  )) +
  geom_point()+
  stat_smooth(method = 'lm') +
  labs(x = "Internet Availability Rate", y = "GDP per Capita") +
  ggtitle("Linear Modeling by Continent")

# Hierarchical modelling
tidylm <- lm(gdp_c~IAR, data = tidydata)
tidylmer1 <- lmer(gdp_c~IAR + (1|continent), data = tidydata)
summary(tidylmer1)
tidylmer1vc <- VarCorr(tidylmer1)
tidylmer2 <- lmer(gdp_c~IAR + (1+IAR|continent), data = tidydata)
summary(tidylmer2)
tidylmer2vc <- VarCorr(tidylmer2)

anova(tidylmer1,tidylmer2)



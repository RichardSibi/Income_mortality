library(tidyverse)
library(ggplot2)
library(ggridges)
library(mice)
library(VIM)
library(skimr)
library(viridis)
library(ggpubr)
#Loading the data
 # Mortality rates vs Income differences


##Adult Mortality Rate

# Dataset on adult mortality rates per country/region
mortality_all <- read.csv('Downloads/archive/Both sexes.csv')
View(mortality_all) 
glimpse(mortality_all)
names(mortality_all)


#Bringing the Adult.mortality.rate value for male and females
mortality_male <- read.csv('Downloads/archive/male.csv')
view(mortality_male)

mortality_female <- read.csv('Downloads/archive/female.csv')
view(mortality_female)

#Creating both tables to merge

mortality_male2 <- mortality_male %>% 
  rename(Country = Location) %>% 
  select(Country, Period, Adult.mortality.rate) %>% 
  arrange(Country, Period, Adult.mortality.rate) %>% 
  rename(Adult.mortality.rate_male = Adult.mortality.rate)
  
mortality_female2 <- mortality_female %>% 
  rename(Country = Location) %>% 
  select(Country, Period, Adult.mortality.rate) %>% 
  arrange(Country, Period, Adult.mortality.rate) %>% 
  rename(Adult.mortality.rate_female = Adult.mortality.rate)

view(mortality_male2)
view(mortality_female2)





#Creating one of the two tables to merge
mortality_all2 <- mortality_all %>%
  rename(Country = Location) %>% 
  select(Country, ParentLocation, Period, Adult.mortality.rate) %>% 
  arrange(Country, ParentLocation, Period, Adult.mortality.rate)  #Making country the key first


#Checking the mortality variable in the dataset
summary(mortality_all2$Adult.mortality.rate)


##Income index:
#Dimension: Income/composition of resources
#Definition: GNI per capita (2011 PPP International $, using natural logarithm) 
#expressed as an index using a minimum value of $100 and a maximum value $75,000.

# Dataset on income per countries/regions
income <- read_xlsx('Downloads/Income by Country.xlsx')
view(income)                              #Convert this into a proper CSV file
glimpse(income)


#Cleaning the data
income_new <- income %>% 
  select(-c(`1990`,`1991`,`1992`,`1993`,`1994`,`1995`,`1996`,`1997`,`1998`,`1999`)) #Remove uncommon years
view(income_new)

#To tidy the table
income_new <- income_new %>% 
  gather(key = year, value = income_index, -Country)

glimpse(income_new)

`%notin%` <- Negate(`%in%`) #To define a %notin% function

# This is the other table we're going to merge with Adult Mortality Rates table
income_new <- income_new %>% 
  arrange(Country, year, income_index) %>%  # To remove the uncommon years (2017,1018)
  filter(year %notin% c('2017','2018'))


#Check unique country values in both tables
view(unique(income_new$Country))
view(unique(mortality_all2$Country))


####CLEANING THE TABLES

#Filtering countries that aren't in both tables

income_new <- income_new %>%  filter(Country != 'Andorra') %>% 
  filter(Country != 'Arab States') %>% 
  filter(Country != 'Developing Countries') %>% 
  filter(Country != 'East Asia and the Pacific') %>% 
  filter(Country != 'Europe and Central Asia') %>% 
  filter(Country != 'Dominica') %>% 
  filter(Country != 'High human development') %>% 
  filter(Country != 'Hong Kong; China (SAR)') %>% 
  filter(Country != 'Human Development') %>% 
  filter(Country != 'Latin America and the Caribbean') %>% 
  filter(Country != 'Least Developed Countries') %>% 
  filter(Country != 'Liechtenstein') %>%
  filter(Country != 'Low human development') %>% 
  filter(Country != 'Marshall Islands') %>% 
  filter(Country != 'Medium human development') %>% 
  filter(Country != 'Nauru') %>% 
  filter(Country != 'North Macedonia') %>% 
  filter(Country != 'Organization for Economic Co-operation and Development') %>% 
  filter(Country != 'Palau') %>% 
  filter(Country != 'Palestine; State of') %>% 
  filter(Country != 'Regions') %>% 
  filter(Country != 'Saint Kitts and Nevis') %>% 
  filter(Country != 'Small Island Developing States') %>% 
  filter(Country != 'Sub-Saharan Africa') %>% 
  filter(Country != 'Tuvalu') %>% 
  filter(Country != 'Very high human development') %>% 
  filter(Country != 'World') %>% 
  filter(Country != 'South Asia') %>% 
  filter(Country != 'United Republic of Tanzania') %>% 
  filter(Country != 'Democratic Republic of the Congo')
  
  
mortality_all2 <- mortality_all2 %>% 
  filter(Country != 'Somalia') %>% 
  filter(Country != 'The former Yugoslav Republic of Macedonia') %>% 
  filter(Country != 'United Republic of Tanzania') %>% 
  filter(Country != "Democratic People's Republic of Korea") %>% 
  filter(Country != 'Democratic Republic of the Congo')


mortality_male2 <- mortality_male2 %>% 
  filter(Country != 'Somalia') %>% 
  filter(Country != 'The former Yugoslav Republic of Macedonia') %>% 
  filter(Country != 'United Republic of Tanzania') %>% 
  filter(Country != "Democratic People's Republic of Korea") %>% 
  filter(Country != 'Democratic Republic of the Congo')


mortality_female2 <- mortality_female2 %>% 
  filter(Country != 'Somalia') %>% 
  filter(Country != 'The former Yugoslav Republic of Macedonia') %>% 
  filter(Country != 'United Republic of Tanzania') %>% 
  filter(Country != "Democratic People's Republic of Korea") %>% 
  filter(Country != 'Democratic Republic of the Congo')

view(income_new)
view(mortality_all2)

income_new %>% arrange(Country)


#Rename countries that are the same but spelled differently in both tables

income_new$Country[income_new$Country == "CÃ´te d'Ivoire"] <- "Cote d’Ivoire"
income_new$Country[income_new$Country == 'Congo (Democratic Republic of the)'] <- 'Democratic Republic of the Congo'
income_new$Country[income_new$Country == 'Korea (Republic of)'] <- 'Republic of Korea'
income_new$Country[income_new$Country == 'Eswatini (Kingdom of)'] <- 'Eswatini'
income_new$Country[income_new$Country == 'Moldova (Republic of)'] <- 'Republic of Moldova'
income_new$Country[income_new$Country == 'Tanzania (United Republic of)'] <- 'United Republic of Tanzania'
income_new$Country[income_new$Country == 'United States'] <- 'United States of America'

mortality_all2$Country[mortality_all2$Country == 'United Kingdom of Great Britain and Northern Ireland'] <- 'United Kingdom'

mortality_male2$Country[mortality_male2$Country == 'United Kingdom of Great Britain and Northern Ireland'] <- 'United Kingdom'

mortality_female2$Country[mortality_female2$Country == 'United Kingdom of Great Britain and Northern Ireland'] <- 'United Kingdom'

# Adding the IDs to get a common column for both rows

income_new$ID <- c(1:3026) #To add IDs to the rows

income_new <- income_new %>% 
  arrange(Country, ID, year, income_index) %>% 
  select(ID, Country, year, income_index)
  

view(income_new)
glimpse(income_new)

mortality_all2$ID <- c(1:3026)
mortality_male2$ID <- c(1:3026)
mortality_female2$ID <- c(1:3026)

mortality_all2 <- mortality_all2 %>%  # Keeping ID as the first variable in the table
  select(ID, Country, ParentLocation, Period, Adult.mortality.rate)

mortality_male2 <- mortality_male2 %>% 
  select(ID, Adult.mortality.rate_male)

mortality_female2 <- mortality_female2 %>% 
  select(ID, Adult.mortality.rate_female)

glimpse(mortality_all2)


head(mortality_all2)
view(mortality_all2)

#To finally compare the variables one last time
view(unique(income_new$Country))

view(unique(mortality_all2$Country))

#### MERGING the datasets together
income_mortality <- merge(x = income_new,
                          y = mortality_all2,
                          by = 'ID', all = TRUE)

income_mortality <- merge(x = income_mortality,
                          y = mortality_male2,
                          by = 'ID', all = TRUE)
income_mortality <- merge(x = income_mortality,
                          y = mortality_female2,
                          by = 'ID', all = TRUE)
view(income_new)
view(income_mortality)
glimpse(income_mortality)

#### Cleaning the new table

#Remove duplicate columns
income_mortality <- income_mortality %>% 
  rename(Country = 'Country.x') %>% 
  select(-Country.y & -Period)

# Check countries with incorrect income indices
income_index_incorrect <- income_mortality %>% 
  filter(income_index == 1)

unique(income_index_incorrect$Country) #To check the regions with NA values


# Changing variable types in the table
glimpse(income_mortality)
income_mortality$ParentLocation <- as.factor(income_mortality$ParentLocation)
income_mortality$income_index <- as.numeric(income_mortality$income_index)
income_mortality$year <- as.factor(income_mortality$year)
income_mortality$Country <- as.factor(income_mortality$Country)

#### MISSING DATA HANDLING
summary(income_mortality)

missing_plot <- aggr(income_mortality, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(income_mortality), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

# Simple imputation could be done as follows:
# income_mortality$income_index[which(is.na(income_mortality$income_index))] <- mean(income_mortality$income_index, na.rm = TRUE)

# For better results, we'll do MICE imputation
income_mortality_final <- income_mortality

my_imp <- mice(income_mortality_final, m=5, method = 'pmm', maxit = 20)

summary(income_mortality_final$income_index)

my_imp$imp$income_index #To check the 5 imputated table values for income_index

income_mortality_final <- complete(my_imp, 5) # To select the 5th set for replacing NA values

## income_mortality2 will be our final table to work with


view(income_mortality_final)
glimpse(income_mortality_final)


#Checking for NA values in the table
any(is.na(income_mortality_final))


###
#EXPLORATORY DATA ANALYSIS
dim(income_mortality_final)
names(income_mortality_final)
skim(income_mortality_final)


#### DATA VISUALIZATION

# Let us analyze this dataset

## Analysis of single numerical variables

# Comparing income index vs Adult mortality rates for Africa and Europe
# The colourful scatter
income_mortality_final %>% 
  filter(ParentLocation %in% c('Africa', 'Europe')) %>% 
  ggplot(aes(x = Adult.mortality.rate,
             y = income_index,
             color = year))+
  geom_point()+
  facet_wrap(~ParentLocation)+
  labs(title = 'Income index vs Adult mortality rate for Africa and Europe',
       x = 'Adult Mortality Rate',
       y  = 'Income index')


p1 <- income_mortality_final %>% 
  ggplot(aes(x = income_index))+
  geom_histogram(binwidth = 0.1,
                 show.legend = F,
                 alpha = .5)+
  labs(title = 'Histogram',
       x = 'Income Index',
       y = 'Frequency')


#Density plot for income index
  
p2 <- income_mortality_final %>%
  ggplot(aes(x = income_index))+
  geom_density(aes(fill = 'blue'),
               show.legend = F,
               alpha = .5)+
  labs(title = 'Density Plot',
       x = 'Income Index',
       y = 'Probability')


#Density plot for Adult mortality rates

p3 <- income_mortality_final %>% 
  ggplot(aes(x = Adult.mortality.rate))+
  geom_density(aes(fill = 'blue'),
               show.legend = F,
               alpha = .5)+
  labs(title = 'Density Plot',
       x = 'Adult Mortality Rate',
       y = 'Probability')

#Boxplot for Adult mortality rates

p4 <- income_mortality_final %>%
  ggplot(aes(x = Adult.mortality.rate)) +
  geom_boxplot(show.legend = F,
               alpha = .5) +
  labs(title = "Boxplot",
       x = "Adult Mortality Rate")


## Analysis of one or more categorical variables

unique(income_mortality_final$ParentLocation)

#Barplot for region vs Adult mortality
p5 <- income_mortality_final %>% 
  filter(ParentLocation %in% c('Africa','Americas','Europe','South-East Asia')) %>% 
  ggplot(aes(x = ParentLocation, y = mean(Adult.mortality.rate)))+
  geom_bar(stat = 'identity', alpha = 1,
           show.legend = F)+
  labs(title = 'Barplot',
       x = 'Region',
       y = 'Mean Adult Mortality')

p6 <- income_mortality_final %>% 
  filter(year == 2000) %>% 
  filter(ParentLocation %in% c('Africa', 'Americas', 'Europe','South-East Asia')) %>% 
  ggplot(aes(x = ParentLocation, y = Adult.mortality.rate))+
  geom_bar(stat = 'identity', alpha = 1,
           show.legend = F)+
  labs(title = 'Barplot',
       x = 'Region',
       y = 'Adult mortality rates')

## One numeric (income_index) and two categorical variables
## (ParentLocation and Year)

p7 <- income_mortality_final %>% 
  ggplot(aes(income_index, fill = ParentLocation))+
  geom_boxplot(alpha = 0.3)+
  labs(title = 'Boxplot of income index',
       subtitle = 'disaggragated by Parent Location',
       x = 'Income Index')

p8 <- income_mortality_final %>% 
  filter(year %in% c("2000", "2016")) %>%
  filter(ParentLocation %in% c('Africa','Europe')) %>% 
  ggplot(aes(income_index, fill = ParentLocation)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~year) +
  labs(title = "Density plot of income index",
       subtitle = "disagregated by Parent Location and Year",
       x = "Income Index")

p9 <- income_mortality_final %>% 
  ggplot(aes(Adult.mortality.rate, fill = ParentLocation))+
  geom_boxplot(alpha = 0.3)+
  labs(title = 'Boxplot of Adult Mortality Rate (overall)',
       subtitle = 'disaggragated by Parent Location',
       x = 'Adult Mortality Rate')

p10 <- income_mortality_final %>% 
  filter(year %in% c("2000", "2016")) %>%
  filter(ParentLocation %in% c('Africa','Europe')) %>% 
  ggplot(aes(Adult.mortality.rate, fill = ParentLocation)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~year) +
  labs(title = "Density plot of Adult Mortality Rate",
       subtitle = "disagregated by Parent Location and Year",
       x = "Adult Mortality Rate",
       y = "Probability")

p11 <- income_mortality_final %>%
  filter(year %in% c("2000", "2016")) %>%
  ggplot(aes(Adult.mortality.rate, fill = ParentLocation)) +
  geom_boxplot(alpha = 0.3) +
  facet_wrap(~year) +
  labs(title = "Boxplot of Adult Mortality Rate",
       subtitle = "disagregated by Parent Location and Year",
       x = "Adult Mortality Rate")+
  theme(legend.position = "bottom")


## Two numeric (Male and Female mortality rates or Adult mortality rate and income index) and one categorical
## variable (ParentLocation or Year)

p12 <- income_mortality_final %>%
  ggplot(aes(x = Adult.mortality.rate,
             y = income_index)) +
  geom_point(size = 2,
             alpha = 0.7) +
  geom_smooth()+
  labs(title = "Scatter plot",
       subtitle = "with smoothed linear model",
       x = "Adult mortality rate",
       y = "Income index")

p13 <- income_mortality_final %>%
  filter(ParentLocation %in% c('Africa', 'Americas', 'Europe')) %>% 
  ggplot(aes(Adult.mortality.rate, income_index, colour = ParentLocation)) +
  geom_point(size = 1, show.legend = T) +
  labs(title = "Scatter plot",
       subtitle = "disagregated by colour",
       x = "Adult mortality rate",
       y = "Income Index")

p14 <- income_mortality_final %>%
  filter(year %in% c('2000','2016')) %>%
  ggplot(aes(Adult.mortality.rate, income_index, colour = year))+
  geom_point(size = 2, show.legend = F)+
  facet_wrap(~year) +
  labs(title = "Scatter plot",
       subtitle = "disagregated by colour and facets",
       x = "Adult Mortality Rate",
       y = "Income Index")

## Adult mortality by location lollipop graphic

p15 <- income_mortality_final %>%
  group_by(ParentLocation) %>%
  mutate(mean_AdultMortality = mean(Adult.mortality.rate)) %>%
  ungroup() %>%
  mutate(ParentLocation = fct_reorder(ParentLocation, mean_AdultMortality)) %>%
  ggplot(aes(ParentLocation, Adult.mortality.rate, colour = ParentLocation,
             show.legend = F)) +
  coord_flip() +
  geom_jitter(show.legend = F,
              size = 2,
              alpha = 0.2,
              width = 0.05) +
  stat_summary(fun = mean, geom = "point", size = 8, show.legend = F) +
  geom_hline(aes(yintercept = mean(Adult.mortality.rate)),
             colour = "gray70",
             size = 0.9) +
  geom_segment(aes(x = ParentLocation, xend = ParentLocation,
                   y = mean(Adult.mortality.rate), yend = mean_AdultMortality),
               size = 2, show.legend = F) +
  labs(title = "Adult mortality rate by location",
       x = "Regions",
       y = "Adult mortality rate") +
  theme(legend.position = "none") +
  theme_bw()

#Income index by location (lollipop plot)
p20 <- income_mortality_final %>%
  group_by(ParentLocation) %>%
  mutate(mean_incomeindex = mean(income_index)) %>%
  ungroup() %>%
  mutate(ParentLocation = fct_reorder(ParentLocation, mean_incomeindex)) %>%
  ggplot(aes(ParentLocation, income_index, colour = ParentLocation,
             show.legend = F)) +
  coord_flip() +
  geom_jitter(show.legend = F,
              size = 2,
              alpha = 0.2,
              width = 0.05) +
  stat_summary(fun = mean, geom = "point", size = 8, show.legend = F) +
  geom_hline(aes(yintercept = mean(income_index)),
             colour = "gray70",
             size = 0.9) +
  geom_segment(aes(x = ParentLocation, xend = ParentLocation,
                   y = mean(income_index), yend = mean_incomeindex),
               size = 2, show.legend = F) +
  labs(title = "Income Index by location",
       x = "Regions",
       y = "Income Index") +
  theme(legend.position = "none") +
  theme_bw()



## Plotting density plots of adult mortality rates for each year


p16 <- income_mortality_final %>% 
  ggplot(aes(x = Adult.mortality.rate, y = ParentLocation, fill = ..y..))+
  geom_density_ridges(scale = 3, rel_min_height = 0.01,
                      alpha = 5) +
  scale_fill_viridis(name = "Adult mortality rate", option = "C") +
  labs(title = 'Adult mortality rates per location') +
  theme_bw() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )


###

#### HYPOTHESIS TESTING ####

##state your null and alternative hypotheses
  ## null : there is no correlation between income index and Adult mortality rate
## alternative : there is a correlation between income index and Adult mortality rate

#is it a one-sided or two-sided test?
 ## two-sided. No directionality is implied

#To check if data is normally distributed graphically
ggqqplot(income_mortality_final$Adult.mortality.rate)
ggqqplot(income_mortality_final$income_index)
# It shows that it's not normally distributed.


#To check the same statistically
shapiro.test(income_mortality_final$Adult.mortality.rate)
#p-value < 2.2e -16. 
#This is far less than the minimum of 0.05
#This data is skewed.
 

#Applying the Pearson correlation test
cor.test(income_mortality_final$Adult.mortality.rate, income_mortality_final$income_index)
# The pearson correlation coefficient is -0.686096. It shows a negative correlation
# between the two variables
# As p-value is significantly lesser than the alpha value of 0.05.
# We can reject the null hypothesis and accept the alternative hypothesis that
# there is indeed a correlation between income index and adult mortality rate variables



#### 


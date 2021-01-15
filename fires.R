# Forest Fires in Portugal 2015
library(dplyr)
library(lubridate) # date format
library(xts) # time series
library(na.tools)
library(ggplot2) # visualization
library(tidyr) # for fill function
library(dlookr) # data transformation 
library(corrplot) # relations between variables
library(ggfortify)
library(ggplot2)



# load the data set and properly representation of missing values
fr <- read.csv("fires2015_train.csv", header = TRUE, na.strings = c("-", ""), 
               sep = ",", stringsAsFactors = FALSE)

#dimension Of the Object
# dim(fr)
# length(fr)
# nrow(fr)
# ncol(fr)
# dim(fr)[1]
# dim(fr)[2]

#search in data
#fr["cause_type"]
#summary(fr["alert_date"])
#fr[fr$district == "Viana Do Castelo",] <- "Viana do Castelo"
#[fr$district == "Ã???vora",] <-"Évora"
#fr
#fr[fr$district == "Viana do Castelo",]


# remove redundant or irrelevant attributes, and missing values
summary(fr)
f <- fr[,-c(1,4:7,15:17)]
f <- as_tibble(f)


# data type change
# factor
f$`region` <- as.factor(f$`region`)
f$`district` <- as.factor(f$`district`)
f$`origin` <- as.factor(f$`origin`)
f$`cause_type` <- as.factor(f$`cause_type`)
# datetime
class(f$alert_date) # character
f$alert <- as.POSIXct(paste(f$alert_date, f$alert_hour), 
                           format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
f$extinction <- as.POSIXct(paste(f$extinction_date, f$extinction_hour), 
                           format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
f$firstInterv<- as.POSIXct(paste(f$firstInterv_date, f$firstInterv_hour),
                           format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# remove attributes about date and time (alert, extinction and firstInterv)
f <- f[, -(4:9)]

# clean the text errors from levels factors
levels(f$district) <- c("Évora", "Aveiro", "Beja", "Braga", "Bragança", 
                        "Castelo Branco", "Coimbra", "Faro", "Guarda", "Leiria",
                        "Lisboa", "Portalegre", "Porto", "Santarém", "Setúbal", 
                        "Viana do Castelo", "Viana do Castelo", "Vila Real", "Viseu" )
levels(f$region) <- c("Alentejo", "Algarve", "Beira Interior", "Beira Litoral", 
                      "Centro", "Entre Douro e Minho", "Lisboa e Vale do Tejo",
                      "Norte", "Ribatejo e Oeste", "Trás-os-Montes")

# leading with n.a. values 
summary(f)
# region 
f <- f %>% arrange(region) %>% group_by(district) %>% fill(region)
# extinction and firstInterv
f <- f %>% na.rm()
#f$extinction[is.na(f$extinction)] <- f$alert[is.na(f$extinction)]
#f$firstInterv[is.na(f$firstInterv)] <- f$alert[is.na(f$firstInterv)]

# if are n.a. values in data
f %>% any_na()
f %>% n_na()

# data manipulation
# datetime

# select(f, alert, extinction, firstInterv) %>% slice(40:50)
# f[is.na(f$extinction),]
# f %>% filter(is.na(region)) 


str(f)

# search in data
min(f$alert)
max(f$alert)
range(f$alert)
max(f$alert) - min(f$alert)

# fire per weekday
table(format(f$alert, '%A'))

# create a new column - month
f$month <- month(f$alert)
names(f)
str(f$month)

# new dataset grouped by month
f_month <- group_by(f, month)
class(f_month)

# how many measurements were made each month?
tally(f_month) 
tally(f_month)  %>% max()
f %>% group_by(month) %>% tally()

# fire per month
table(format(f$alert, '%B')) 
table(format(f$alert, '%m')) %>% max()

# mean of total_area burned per month
summarise(f_month, mean(total_area))

# mean fires per day
table(format(f$alert, '%D')) %>% max()

# fire in summer time (June, July, August)
tally(f_month) %>% slice(6:8)

# top 5 in the burned total_area
arrange(f, desc(total_area)) 
arrange(f, desc(total_area)) %>% head(5)

# highest farmimg_area with highest total_area
arrange(f, desc(farming_area), desc(total_area))

# inspect extinction attribute - dealing with na values
f_month %>% group_by(month) %>% summarise(meanExt=mean(extinction, na.rm = TRUE))

# data quality and pre-processin
str(f)
summary(f)
# village_area attribute
str(f_month$village_veget_area)
vill_veget_area <- f_month %>% 
  mutate(village_veget_area=as.numeric(transform(village_veget_area,method="minmax")))  %>%
  pull(village_veget_area)
summary(vill_veget_area)


# farming_area attribute
farming1 <- f_month %>% 
  mutate(farming_area=as.numeric(transform(farming_area,method="minmax")))  %>%
  pull(farming_area)
summary(farming1)

# total_area attribute
total1 <- f_month %>% 
  mutate(total_area=as.numeric(transform(total_area,method="minmax")))  %>%
  pull(total_area)
summary(total1)


# a random sample
set.seed(123)
sample1 <- f_month %>% sample_frac(0.6,replace = TRUE)
sample2 <- f_month %>% group_by(cause_type) %>% sample_frac(0.6)

table(f_month$cause_type)

table(sample1$cause_type)

table(sample2$cause_type)

# correlations between variables
f_numeric <- f_month %>% select_if(is.numeric)
res <- cor(f_numeric) 

res1 <- cor.mtest(f_numeric, conf.level=0.95)
corrplot.mixed(res, p.mat = res1$p,    sig.level = 0.05)


# Inspect how each variable is obtained by the linear combination of each component.
res.pca <- prcomp(f_numeric)
res.pca
summary(res.pca)
autoplot(res.pca,data=f_numeric,label.size=3,label=TRUE,loadings=TRUE,loadings.label=TRUE,loadings.label.size=3)

# data exploration - summarization
f_month %>% count()
f_month %>% group_by(origin) %>%  count()
f_month %>% group_by(cause_type) %>%  count()

f_month %>% group_by(cause_type, origin) %>%  count()

f_month %>% summarise(total_area.mean = mean(total_area), total_area.sd = sd(total_area)) %>% arrange(month)

f_month %>% group_by(cause_type) %>% summarise_at(vars("alert", "extinction"), list(mean= mean, sd=sd, meadian= median, IQR=IQR))

# visualization

# show relationship between attributes
ggplot(f_month,aes(x=alert,y=total_area)) + geom_point() +
  ggtitle("Relationship between cause type and total area burned")

ggplot(f_month,aes(x=village_veget_area,y=total_area)) + geom_point() +
  ggtitle("Relationship between village and vegetable area and total area burned")

# new colimn time_extinct - time to extinct a fire
f_month$duration <- as.numeric(difftime(f_month$extinction, f_month$alert), units="hours")
names(f_month)
summary(f_month)

dur1 <- f_month %>% 
  mutate(duration=as.numeric(transform(duration,method="minmax")))  %>%
  pull(duration)
summary(dur1)

ggplot(f_month,aes(x=duration,y=total_area)) + geom_point() +
  ggtitle("Relationship between duration of the fire and total area burned")




# regiao mais afetada
ggplot(f, aes(x=district)) + 
  geom_bar() + 
  ggtitle("Distribution of region")



# municipality data is not readable
ggplot(f, aes(x=region)) + 
  geom_bar() + 
  ggtitle("Distribution of municipality")

#data quality
summary(f)
is_empty(levels(factor(f$region)))
names(table(f$region))


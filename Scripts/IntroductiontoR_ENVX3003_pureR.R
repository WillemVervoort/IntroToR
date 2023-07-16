## ----setup, echo=F, warning=F, message=F--------------------------------------------------------------------------------
# root dir
#knitr::opts_knit$set(root.dir = "D:/cloudstor/IntroToR")
#knitr::opts_knit$set(root.dir = "c:/user/rver4657/dropbox (Sydney Uni)/teaching/envx3003/scripts")
knitr::opts_chunk$set(echo = TRUE)
require(tidyverse)
require(lubridate)


## ---- eval=F------------------------------------------------------------------------------------------------------------
## install.packages("packageName") # case sensitive and you need quotes
## library(packageName) # to load the package, case sensitive


## ---- eval=F------------------------------------------------------------------------------------------------------------
## getwd() # this tells you your  current working directory
## dir() # this gives you a list of the files in your current working directory
## ?setwd # this give you the help file on how to set your working directory in code


## ---- eval=F------------------------------------------------------------------------------------------------------------
## knitr::opts_knit$set(root.dir = "c:/users/rver4657/Desktop") ## windows users
## knitr::opts_knit$set(root.dir = "~/Desktop") ## Mac users
## dir() # this gives you a list of the files in your current working directory


## ----calculator---------------------------------------------------------------------------------------------------------
3*5
50/100 + 0.1
10 - 20


## ----objects------------------------------------------------------------------------------------------------------------
# assign
x <- 5
y <- 2


## ----inspectObj---------------------------------------------------------------------------------------------------------
x


## ----objects_in_R-------------------------------------------------------------------------------------------------------
# a vector
x <- c(1,2,5,7,8,15,3,12,11,19)
# another vector
y <- 1:10
# you have now two objects
ls()
# you can add, multiply or subtract
z <- x + y
z
zz <- x * y
zz
zzz <- x - y
zzz
foo <- 0.5*x^2 - 3*x + 2
foo


## ---- eval=F------------------------------------------------------------------------------------------------------------
## install.packages("tidyverse")


## ---- eval=F------------------------------------------------------------------------------------------------------------
## require(tidyverse)


## ----dataframe----------------------------------------------------------------------------------------------------------
Rainfall <- tibble(City = c("Montevideo","New York",
                                "Amsterdam","Sydney",
                                "Moscow", "Hong Kong"),
                       Rain_mm = c(950, 1174, 838, 1215,
                                   707, 2400))
Rainfall


## ----colnames-----------------------------------------------------------------------------------------------------------
colnames(Rainfall)
names(Rainfall)


## ----workdf-------------------------------------------------------------------------------------------------------------
# call a column
Rainfall$City
# or
Rainfall["City"]
# or
Rainfall[,1]


## -----------------------------------------------------------------------------------------------------------------------
# see the first two rows
Rainfall[1:2,]




## ----filter-------------------------------------------------------------------------------------------------------------
Rainfall %>%
  filter(City=="Montevideo")


## -----------------------------------------------------------------------------------------------------------------------
# find a subset
lots <- Rainfall %>%
  filter(Rain_mm > 1000)
lots


## -----------------------------------------------------------------------------------------------------------------------
# call a column
Rainfall %>%
  select(City)


## ----read_csv-----------------------------------------------------------------------------------------------------------
UR_flow <- read_csv("Data/UruguayRiver_ConcordiaSt.csv")
# check the first few lines (6 by default)
UR_flow


## -----------------------------------------------------------------------------------------------------------------------
# I can add a column of countries
Rainfall_new <- Rainfall %>%
  mutate(country = c("UY", "US", "NL", "AU", "RU","CN")) %>%
  # and maybe a column of the average monthly rainfall
  mutate(M_rain = Rain_mm/12)

# You can use select to reorder the columns and put country to the front
Rainfall_new <- Rainfall_new %>%
  select(country, everything())

# And if you would like to drop the M_rain column you can use
Rainfall_new %>%
  select(-M_rain)



## -----------------------------------------------------------------------------------------------------------------------
Rainfall_new %>%
  arrange(desc(Rain_mm))


## ----aggregate_demo-----------------------------------------------------------------------------------------------------
# aggregate to annual flow
(annual_flow <- UR_flow %>% #then
  group_by(Year=Year) %>% #then
  summarise(Sumflow = sum(Flow)))



## -----------------------------------------------------------------------------------------------------------------------
# Mean and sd\ monthly flow
UR_flow %>% #then
  summarise(Meanflow = mean(Flow),
            SdFlow = sd(Flow))


## ----chemdata-----------------------------------------------------------------------------------------------------------
chemdata <- read_csv("data/semarang_chem.csv")
chemdata


## ----tv_select2---------------------------------------------------------------------------------------------------------
chemdata %>%
  select(Lat, Long:Depth)


## ----unselect-----------------------------------------------------------------------------------------------------------
chemdata %>%
  select(Lat, Long:Depth, -UTM_zone)


## ----arrange------------------------------------------------------------------------------------------------------------
chemdata %>%
  arrange(Aq, Fac) 


## ----mutate-------------------------------------------------------------------------------------------------------------
chemdata %>% 
  mutate(ratio_Cana = Ca / Na)


## ----summarise----------------------------------------------------------------------------------------------------------
chemdata %>% 
  summarise(mean_TDS = mean(TDS), 
            max_Cl = max(Cl),
            min_Cl = min(Cl),
            total = n())


## ----group_by-----------------------------------------------------------------------------------------------------------
chemdata%>% 
  group_by(Aq) %>%
  summarise(mean_TDS = mean(TDS), 
            max_Cl = max(Cl),
            min_Cl = min(Cl),
            total = n())


## ----plotting1----------------------------------------------------------------------------------------------------------
hist(chemdata$SO4, main = "histogram of SO4")
plot(chemdata$TDS, chemdata$Cl, main="scatter plot of TDS and Cl")


## ----plotting3----------------------------------------------------------------------------------------------------------
chemdata  %>%
  ggplot(aes(EC, fill = Fac)) + geom_histogram()

chemdata %>% 
  ggplot(aes(Cl, TDS, colour = Fac)) + geom_point()



## ----ifelseLoop---------------------------------------------------------------------------------------------------------
#We will first generate the data frame:
x <- UR_flow
# now wrote loop
if (nrow(x) > 10) {
print("the dataframe is LONG")
} else {
print("the dataframe is SHORT")
}


## ----vectorisedIfelse---------------------------------------------------------------------------------------------------
x <- UR_flow 
# add a column which identifies whether the flow < 5000
x[,4] <- ifelse(x[,3] > 5000, "large", "small")
# this creates a third column
tail(x,10)


## ----nestedifelse-------------------------------------------------------------------------------------------------------
x[,5] <- ifelse(x[,3] > 2500,ifelse(x[,3] > 10000, "large", "intermediate"), "small")
tail(x,10)


## ----helloworld---------------------------------------------------------------------------------------------------------
# Hello world
for (i in 1:5) {
print(paste(i, "hello world"))
}


## ----simple_loop--------------------------------------------------------------------------------------------------------
for (i in 1:5) {
print(paste(UR_flow$Flow[i], "is the flow (ML/day)"))
}

# or more complex:
for (i in 1:5) {
print(paste("in Year", UR_flow$Year[i],"and month", 
UR_flow$Month[i], 
"the flow is", UR_flow$Flow[i], "(ML/day)"))
}



## ----nested_loops-------------------------------------------------------------------------------------------------------
for (i in 1:5) {
for (j in c(1,3)) {
print(paste(UR_flow[i,j], colnames(UR_flow)[j]))
}
}



## ----lubridate----------------------------------------------------------------------------------------------------------
#install.packages("lubridated")  do this if you haven't done so.
require(lubridate)
UR_flow_d <- UR_flow %>%   
  mutate(Dates = make_datetime(Year, Month))


## ----plot_of_flow, fig.cap="Demonstration of dates in plotting R"-------------------------------------------------------
UR_flow_d %>%
  ggplot(aes(Dates,Flow)) + geom_line(colour="blue")


## ----helloFun-----------------------------------------------------------------------------------------------------------

HW <- function(n, outtext) {
  for (i in 1:n) {
    print(outtext)
  }
  # return("nothing")
}

# test
HW(5, "Hello World")
# switch input by naming
HW(outtext ="I can switch the inputs", n = 3)


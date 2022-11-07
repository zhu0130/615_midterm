library(tidyverse)
library(magrittr)
library(readxl)
strawb <- read_excel("~/Desktop/strawberries-2022oct30-a.xlsx")
view(strawb)
## Get the column names and index them
cnames <- colnames(strawb)
x <- 1:dim(strawb)[2]
## Set T as an indicator
T <- NULL

## Collect number of unique rows in each column
for(i in x){T <- c(T, dim(unique(strawb[i]))[1])}

## Use T to select columns to drop -- 
drop_cols <- cnames[which(T == 1)]

## Now, drop the columns with only one unique value.
strawb %<>% select(!all_of(drop_cols))

## Let's arrange the data frame by year and state.
strawb %<>% arrange(Year, State)


temp1 <- strawb %>% select(`Data Item`) %>% 
  distinct()

strawb %<>% separate(col=`Data Item`,
                     into = c("Strawberries", "type", "items", "units"),
                     sep = ",",
                     fill = "right")
view(strawb)

###########
#Q2
Q2 <- filter(strawb,Year == 2016 & State == 'CALIFORNIA'  
                             & Domain == 'ORGANIC STATUS')
view(Q2)


#Q3
library(gmodels)
library(Rmisc)
nonorgansales_2016_ca <- filter(strawb, State == 'CALIFORNIA' & Year == 2016 & Domain != 'ORGANIC STATUS')
new_non <- filter(nonorgansales_2016_ca, Value != "(NA)" & Value != "(D)" & Domain != "TOTAL")
CI(as.numeric(new_non$Value))
#Q4
chemical <- filter(strawb, Domain != 'ORGANIC STATUS' & Domain != "TOTAL" & Domain != 'FERTILIZER')
length(unique(chemical$`Domain Category`))

#Q5
chemical_florida <- filter(strawb, State == 'FLORIDA' & 
                             Domain != 'ORGANIC STATUS' & 
                             Domain != 'TOTAL' & 
                             Domain != 'FERTILIZER')
chemical_california <- filter(strawb, State == 'CALIFORNIA' & 
                                Domain != 'ORGANIC STATUS' & 
                                Domain != 'TOTAL' & 
                                Domain != 'FERTILIZER')
diff<-length(unique(chemical_california$`Domain Category`))-
  length(unique(chemical_florida$`Domain Category`))
diff
#so the difference is 138 - 115=23








#packages: Tidyverse, to run the package we type library(tidyverse) and the package will be in our script
library(tidyverse)
library(readxl)

#trying to create a conflict with Alexandra

## Analysis for Bumblebees in Aotearoa

#The author of this is Jon
#but not a good author

#I'm just editing for version control

Bombus_pascourum_Sitio_Periodo <- read_excel("Bombus pascourum Sitio_Periodo.xlsx", 
                                               +     sheet = "Codigo B. pascuorum + medidas")


Bombus_pascourum_Sitio_Periodo <- read_excel("Bombus_pascourum_Sitio_Periodo.xlsx", 
                                             +     sheet = "Codigo B. pascuorum + medidas")
View(Bombus_pascourum_Sitio_Periodo)   

dat <- Bombus_pascourum_Sitio_Periodo

total_length <- dat[,4]
period <- dat[,3]

apply(total_length,2,max)


apply(total_length, 2, mean)
apply(total_length,2,min)


max(dat[,4])
apply(total_length,2,sd)

1plot(period, total_length)




library(readxl)
Bombus_pascourum_Sitio_Periodo <- read_excel("Bombus_pascourum_Sitio_Periodo.xlsx", 
                                              +     sheet = "Codigo B. pascuorum + medidas_2")


#New names:                                                                                          
 # • `SITE` -> `SITE...2`
#• `SITE` -> `SITE...3`
# • `PERIOD` -> `PERIOD...4`
# • `PERIOD` -> `PERIOD...5`
 View(Bombus_pascourum_Sitio_Periodo)

dat <- Bombus_pascourum_Sitio_Periodo
period <- dat[,4]
total_length <- dat[,6]

plot(period, total_length)
  
plot(total_length)

plot(dat)

period2 <- dat[,5]

plot(period2)

?plot

hist(total_length$TOTAL_LENGTH_mm)

    ###IMPORTANT: ALWAYS RUN THE PACKAGE(LIBRARY) WHEN YOU OPEN RSTUDIO, IF NOT IT WON'T WORK###

#we create a new data frame with tidyverse, we just choose 5 variables with the comand "select" (the five columns we decided to appear: ID, SITE..2,etc.)

simplified_bombus_information <- select(Bombus_pascourum_Sitio_Periodo,ID,SITE...2,PERIOD...4,TOTAL_LENGTH_mm,Date_of_gut_extraction)#%>%
  ##### If we add %>% at the end of each line, we keep adding conditions to the comand line (we create a pipeline)
   ### in this case we can filter (choosing only the samples smaller than 18mm), group by site
   ##with summarise option we will make tidyverse create the mean values for every group (in this case site)
   ## finally we can arrange (shorten) by the length or other variable such as SITE...2, depending on what we want

  #filter(TOTAL_LENGTH_mm<18)%>%
  #group_by(SITE...2)%>%
  #summarise(mean_length= mean(TOTAL_LENGTH_mm, na.rm = TRUE))%>%
  #arrange(desc(mean_length))


##If we don't create an object the console will show as a brief image of what our comand does w/o creating a data frame
##with the option "print" we can see how many lines it will show as
simplified_bombus_information%>%
  group_by(PERIOD...4)%>%
  print(n=30)

#With this comands we can create new data frames for mean values based on site and period 
Mean_by_site <- simplified_bombus_information%>%
  group_by(SITE...2)%>%
  summarise(mean_length= mean(TOTAL_LENGTH_mm, na.rm = TRUE))%>%
  arrange(desc(SITE...2))

Mean_by_period <- simplified_bombus_information%>%
  group_by(PERIOD...4)%>%
  summarise(mean_length= mean(TOTAL_LENGTH_mm, na.rm = TRUE))%>%
  arrange(desc(PERIOD...4))

#same with SD
SD_by_site <- simplified_bombus_information%>%
  group_by(SITE...2)%>%
  summarise(sd_length= sd(TOTAL_LENGTH_mm, na.rm = TRUE))%>%
  arrange(desc(sd_length))

SD_by_period <- simplified_bombus_information%>%
  group_by(PERIOD...4)%>%
  summarise(sd_length= sd(TOTAL_LENGTH_mm, na.rm = TRUE))%>%
  arrange(desc(PERIOD...4))

#by doing print and print(data_name[-c(x),]) we eliminate the x row in the table, so with that our 
#new objects just have the raw data without the null values that my affect the statistics.
#I used the same name to change the object w/o creating new objects every time

simplified_bombus_information <- print(simplified_bombus_information[-c(129:130),])

#last version w/o null values
simplified_bombus_information <- print(simplified_bombus_information [-c(130),])

SD_by_period <- filter(SD_by_period, ID %in% -c("NL1", "NL2", "NL3"))

SD_by_site <- print(SD_by_site[-c(17:19),])

#easier way of filtering, rahter than deleting line by line
data_without_null <- filter(dat, !ID %in% c("NL1", "NL2", "NL3"))

Mean_by_period <- print(Mean_by_period[-c(7),])

Mean_by_site <-print(Mean_by_site[-c(2:3),])

Mean_by_site <- print(Mean_by_site[-c(4),])
max(dat$TOTAL_LENGTH_mm)

#we created a new csv file with the simplified data

write.csv(simplified_bombus_information,file = "Simplified_bombus_information_clean.csv",row.names = FALSE)

library(ggplot2)

hist(Mean_by_site$mean_length)

#INTENTAR QUITAR NULL VALUES, Y CREAR NUEVA TABLA
dotchart(Mean_by_period$mean_length,labels = 1:6)

dotchart(Mean_by_site$mean_length, labels = 1:16)

hist(Mean_by_period$mean_length)

plot(Mean_by_period)

plot(Mean_by_site, xlab="Site", ylab="Mean length", main= "Average total length by site", col="black")%>%
  scale_x_continuous(limits = c(1,16), breaks = seq(1,16,1), range())

?plot

boxplot(Mean_by_period$mean_length)

boxplot(Mean_by_site$mean_length)

simplified_bombus_information <- arrange(simplified_bombus_information, as.numeric(SITE...2))

#to combine two columns in a boxplot we need to add the simbol ~ and that will give us a combinatiom between the two chosen columns
# in this case I decided to combine the total length and each site
#IMPORTANT -> as.numeric
boxplot(simplified_bombus_information$TOTAL_LENGTH_mm~as.numeric(simplified_bombus_information$SITE...2), xlab= "Site", ylab = "Total length (mm)")
#we can also do it per period


boxplot(simplified_bombus_information$TOTAL_LENGTH_mm~simplified_bombus_information$PERIOD...4, xlab = "Period", ylab = "Total length (mm)")
plot(simplified_bombus_information)
plot(x=simplified_bombus_information$ID, y=simplified_bombus_information$TOTAL_LENGTH_mm)

simplified_bombus_information%>%
  group_by(Date_of_gut_extraction)%>%
  count(Date_of_gut_extraction)

counting_bombus <- simplified_bombus_information%>%
  group_by(SITE...2)%>%
  count(PERIOD...4)

simplified_bombus_information%>%
  count(SITE...2)

ggp





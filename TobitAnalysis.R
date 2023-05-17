#2019

##############Only For Cross Tab and COR#############################

require(foreign)
require(MASS)
require(pROC)
require(survey)
require(ResourceSelection)
require(ROCR)
require(car)
require(ggplot2)
require(maptools)
require(dplyr)
#library
library(frontier)
library(plm)
library(Benchmarking)
library(lmtest)
library(AER)


## importing dataset

setwd('E:\\Niaz Bhai')

data <- as.data.frame(read.spss('Data for final Analysis_AEIS_Momin.sav',use.value.labels=F),stringsAsFactors = FALSE)
View(data)
tobitmodel <- tobit(Adoption_GAP_Farmers ~ Age_Year + Edu_Year + Area_ha + Income_000
                    + Experience_Year + Innovativeness_Year + Profitability_ha_BCR + Extension_Contact + Valuechain_Contact
                    + Training_Days + Decision_Making + Farmers_satifaction + Farmers_Knowledge + Problem_Faced_Farmers, data = data)
summary(tobitmodel)

confint(tobitmodel)

data$Adoption_GAP_Farmers.cat[data$Adoption_GAP_Farmers < 30] <- "Low"
data$Adoption_GAP_Farmers.cat[data$Adoption_GAP_Farmers >= 31 & data$Adoption_GAP_Farmers < 60] <- "Medium"
data$Adoption_GAP_Farmers.cat[data$Adoption_GAP_Farmers >= 61] <- "High"

table(data$Adoption_GAP_Farmers.cat)

x <- table(data$Adoption_GAP_Farmers.cat)*100 / sum(table(data$Adoption_GAP_Farmers.cat))  
x


# Frequency: high (24) low (248) medium(97)
# Percentage: high (6.50) low (67.21) medium(26.29)

dat <- data[2:16]

colnames(dat) <- c("Age",
                   "Education",
                   "Area",
                   "Income",
                   "Experience",
                   "Innovativeness",
                   "Profitability",
                   "Extension contacts",
                   "Value chain contacts",
                   "Training",
                   "Decision making",
                   "Farmers satisfaction",
                   "Farmers knowledge",
                   "Problem faced farmers",
                   "Adaptation gap"
)

library(mgcv)
library(GGally)
library(mgcv)
library(visreg)



ggpairs(dat, 
        columnLabels = gsub('.', ' ', colnames(dat), fixed = T), 
        labeller = label_wrap_gen(10))

library(ggplot2)

#Box plot

ggplot(dat, aes(x = Age)) +
  geom_boxplot()

write.csv(dat,"dat")

#main data for boxplot
box <- read.table("E:\\Niaz Bhai\\boxplot.csv",sep=',',header=T)
str(box)


p1 <- ggplot(box, aes(x = ï..Variables, y = Values)) +
  geom_boxplot(colour = "black", fill = "#56B4E9") +
  scale_y_continuous(name = "Values",
                     breaks = seq(0, 350, 50),
                     limits=c(0, 350)) +
  scale_x_discrete(name = "Variables") +
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title=element_text(size = 12, family="xkcd-Regular"),
        text=element_text(size = 12, family="xkcd-Regular"),
        axis.text.x=element_text(colour="black", size = 10),
        axis.text.y=element_text(colour="black", size = 10)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p1






data_frame <- data.frame(values = c(77.02401099, 22.97598901),
                 Variables = c("TAQ", "AG"))

library(ggplot2)


# creating a pie chart in order 
data_frame %>%
  group_by(Variables) %>%
  summarise(ut_marks= sum(values)) %>%
  mutate(mean_ut=ut_marks/sum(ut_marks)) %>%
  ggplot(aes(x="", y= mean_ut, 
             fill=reorder(Variables, ut_marks))) +
  geom_col() + geom_text(aes(label = scales::percent(round(mean_ut,2))), 
                         position = position_stack(vjust = 0.5))+
  coord_polar(theta = "y")






data_frame <- data.frame(values = c(77.02401099, 22.97598901),
                         Variables = c("TAQ", "AG"))

library(ggplot2)


# creating a pie chart in order 
data_frame %>%
  group_by(Variables) %>%
  summarise(ut_marks= sum(values)) %>%
  mutate(mean_ut=ut_marks/sum(ut_marks)) %>%
  ggplot(aes(x="", y= mean_ut, 
             fill=reorder(Variables, ut_marks))) +
  geom_col() + geom_text(aes(label = scales::percent(round(mean_ut,2))), 
                         position = position_stack(vjust = 0.5))+
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  coord_polar(theta = "y")







data_frame <- data.frame(values = c(30.4269421,	38.83926088,	0.739490534,	3.781558004,	
                                    0.192497355,	0,	0.328959485,	0.328668536,	0.793664206,	
                                    1.592969884),
                         Variables = c("BRRI dhan28 (30.43%)", "BRRI dhan29 (38.84%)", "BRRI dhan50 (0.74%)", "BRRI dhan58 (3.78%)",
                         "BRRI dhan63 (0.19%)", "BRRI dhan67 (0.00%)","BRRI dhan74 (0.33%)", "BRRI dhan81 (0.33%)", "BRRI dhan88 (0.79%)", "BRRI dhan89 (1.59%)"))

library(ggplot2)
library(ggplot2)
library(ggrepel)
library(tidyverse)
# Install
install.packages("wesanderson")
# Load
library(wesanderson)


# creating a pie chart in order 
data_frame %>%
  group_by(Variables) %>%
  summarise(ut_marks= sum(values)) %>%
  mutate(mean_ut=ut_marks/sum(ut_marks)) %>%
  ggplot(aes(x="", y= mean_ut, 
             fill=reorder(Variables, ut_marks))) +
  geom_col(position = 'stack', width = 1)+
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_classic() +  scale_fill_brewer(palette = "Set3",direction = 1)+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  coord_polar(theta = "y")+
  labs(fill = "Variety",
       x = NULL,
       y = NULL,
       title = "")



data_frame <- data.frame(values = c(-20.4269421,	9.260509466,	6.218441996,	9.807502645,	
                                    9.67104515,	9.671331464,	9.206335794,	8.407030116,	-28.83926088,	
                                    10.0000000),
                         Variables = c("BRRI dhan28 (-20.43%)", "BRRI dhan29 (9.26%)", "BRRI dhan50 (6.22%)", "BRRI dhan58 (9.81%)",
                         "BRRI dhan63 (9.67%)", "BRRI dhan67 (9.67%)","BRRI dhan74 (9.21%)", "BRRI dhan81 (8.41%)", "BRRI dhan88 (-28.84%)", "BRRI dhan89 (10.00%)"))




# creating a pie chart in order 
data_frame %>%
  group_by(Variables) %>%
  summarise(ut_marks= sum(values)) %>%
  mutate(mean_ut=ut_marks/sum(ut_marks)) %>%
  ggplot(aes(x="", y= mean_ut, 
             fill=reorder(Variables, ut_marks))) +
  geom_col(position = 'stack', width = 1)+
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_classic() +  scale_fill_brewer(palette = "Set3",direction = 1)+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  coord_polar(theta = "y")+
  labs(fill = "Variety",
       x = NULL,
       y = NULL,
       title = "")


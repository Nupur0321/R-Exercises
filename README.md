# R-Exercises




1 .Using the xlsx or readxl package or otherwise, read each sheet in the ”Assignment1_2020.xlsx” file intoR.\
install.packages("readxl")\
library(readxl)\
dataset1 <- read_excel("Assignment1_2020.xlsx")\
dataset1\
View(dataset1)\
dataset2 <- read_excel("Assignment1_2020.xlsx", sheet = "Sheet2")\
dataset2\
View(dataset2)\
dataset3 <- read_excel("Assignment1_2020.xlsx", sheet = "Sheet3")\
dataset3\
View(dataset3)

2. Generate a data frame for each sheet in the file.\
dataset1 <- data.frame(dataset1)\
str(dataset1)  #structure of dataset1
dataset2 <- data.frame(dataset2)\
str(dataset2)  #structure of dataset2
dataset3 <- data.frame(dataset3)\
str(dataset3) #structure of dataset3


3. The dataset in the first sheet is a random selection from a larger dataset. Another separate sample of this larger dataset is contained in the second sheet;\ however, this file is formatted differently. Assumingthedatasetwascreatedon1st Sept2020, youneedtorecalculatethe“Age”foreach entry.\
New_Date <- '2020-09-01'\
x <- as.Date(New_Date)

New_Age_dataset1 <- as.numeric(difftime(x,dataset1$DoB, units = "weeks"))/52.17\
dataset1$Age <- round(New_Age_dataset1, digits = 2)\
View(dataset1)

New_Age_dataset2<- as.numeric(difftime(x,dataset2$DoB, units = "weeks"))/52.17\
dataset2$Age <- round(New_Age_dataset2, digits = 2)\
View(dataset2)

#52.17 is the number of weeks in non-leap year

4. Then merge these two data sets into a single data frame, making sure that the variable columns are consistent and that any repeated entries from both files are removed.

#rbind is use to combine the two dataset
New_Dataset <- rbind(dataset1,dataset2)\
View(New_Dataset)\
str(New_Dataset)\
library(dplyr) #To use distinct function
distinct(New_Dataset)  #Remove duplicate rows of data frame


5.You will never get access to the full dataset so you should regenerate a new identification number for each subject in the dataset. This should be the row number of each entry in this data frame. You do not need to do this for Sheet 3.\
New_Dataset$New_IDNum <- 1:nrow(New_Dataset)\
View(New_Dataset)

6. It is also required to have an additional identifier which is the number you h ave generated in (5) followed by the first letter of each subject’s forename\ and then followed by the first letter of each subject’s surname. You do not need to do this for Sheet 3. In the case of surnames with prefixes (such as “McXXX”\ or “O’XXX”), the prefix and the following letter should be included in the new identifier.\
install.packages("stringr")\
library("stringr")

#(\\w) extract the first element of string\
New_Dataset$Prefix_Surname <- str_extract(New_Dataset$Surname,"(\\w)")\
New_Dataset$Prefix_Forename <- str_extract(New_Dataset$Forename,"(\\w)")\
New_Dataset$Additional_ID <- paste(New_Dataset$New_IDNum,New_Dataset$Prefix_Forename,New_Dataset$Prefix_Surname)\
View(New_Dataset)


7. Although the data is not available for most subjects, some data highlighting subjects state of health is available in Sheet 3. You should use the subjects ID number to match it and merge it with the first data frame\
Total_Dataset<- merge(New_Dataset, dataset3, by = "IDNum", all = TRUE)\
View(Total_Dataset)\
str(Total_Dataset)

8. Not every subject has its ID number included in Sheet 3. You should attempt to match the remaining subjects using their first and surnames. This must be done using tidyverse in a robust manner. Your code for doing this should work again in the case of a new sample of data being provided.\
install.packages("tidyverse")\
library(dplyr)  #to use left_join\
Total_Dataset <- New_Dataset %>% left_join(dataset3,by=c("Forename","Surname"))\
View(Total_Dataset)


9.You should add a column for age range. This should be as follows: (x<y means inclusive of x and exclusive of y)\

library(dplyr) #to use mutate function
Total_Dataset <- mutate(Total_Dataset, Age_Category = ifelse(Age >= 0 & Age <= 17, "Child",
                                                ifelse(Age >= 18 & Age <= 24, "Youth",
                                                      ifelse(Age >= 25 & Age <=34, "Young Adult",
                                                             ifelse(Age >= 35 & Age <= 44, "Adult",
                                                                    ifelse(Age >=45 & Age <=54, "Middle Age",
                                                                           ifelse(Age >=55 & Age <=64, "Elderly","Pensioner")))))))


View(Total_Dataset)

10.You should filter the data by each age category. Generate a bar plot using ggplot2 for the criminal record variable.\

library(ggplot2)\
a <-  Total_Dataset %>% filter(Age_Category %in% c("Child","Youth","Young Adult","Adult","Middle Age","Elderly","Pensioner")) %>% group_by(Criminal_Record, Age_Category) %>%\
head(a) #first rows
tail(a) #last rows
  ggplot(a) + geom_bar(aes(factor(Age_Category) ,fill= Criminal_Record) ,colour = "black",position = "stack")+
  labs(subtitle="Analysis of Criminal Record & Age Category",
       y="Criminal Record",
       x="Age Category",
       title = "Barplot Of Criminal Record") +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
 theme(axis.title.x = element_text(colour="DarkGreen", size = 20),
           axis.title.y = element_text(colour="Red", size = 20),
           axis.text.x = element_text(size=10),
           axis.text.y = element_text(size = 10),
           legend.title = element_text(size=15),
           legend.text = element_text(size = 15),
           plot.title = element_text(colour = "DarkBlue", size = 30, family = "Courier"),
           plot.subtitle = element_text(colour = "black",size = 20))


11. Create a smaller dataframe where the health information is available.\
library(dplyr)\
Health_Data <- Total_Dataset %>% filter(!is.na(Health))  #is.na to remove the NA values
View(Health_Data)

12. Explore the variables in the dataset(both with and without the health variable)\
library(ggplot2)

ggplot(data=Health_Data, aes(x=Age, y=Salary, colour=Age_Category)) + geom_point(size=2)+
  labs(subtitle="Analysis of Age & Salary",
       y="Salary",
       x="Age",
       title = "Scatterplot") +
  geom_smooth(method=lm) +
theme(axis.title.x = element_text(colour="DarkGreen", size = 20),
           axis.title.y = element_text(colour="Red", size = 20),
           axis.text.x = element_text(size=10),
           axis.text.y = element_text(size = 10),
           legend.title = element_text(size=15),
           legend.text = element_text(size = 15),
      plot.title = element_text(colour = "DarkBlue", size = 30, family = "Courier"),
      plot.subtitle = element_text(colour = "black",size = 20))


#Regression - Building linear Model
model <- lm(Salary~Age, Health_Data)\
model\
summary(model)
#Calculating the correlation coefficient
cor(Health_Data$Age, Health_Data$Salary)


#.................................................................

library(ggplot2)\
ggplot(data=Health_Data, aes(x=Education, y=Salary,fill=Education))+ geom_bar(stat = "identity") +
  labs(subtitle="Analysis of Education & Salary",
       y="Salary",
       x="Education",
       title = "Barplot") +
  theme(axis.title.x = element_text(colour="DarkGreen", size = 20),
        axis.title.y = element_text(colour="Red", size = 20),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size=15),
        legend.text = element_text(size = 15),
        plot.title = element_text(colour = "DarkBlue", size = 30, family = "Courier"),
        plot.subtitle = element_text(colour = "black",size = 20))



library(ggplot2)\
Health_Data %>% ggplot(aes(Age_Category, Health, fill= factor(Health)))+ geom_bar(stat = "identity") +
  labs(subtitle="Analysis of Health & Age_Category",
       y="Health",
       x="Age_Category",
       title = "Barplot") +
  scale_fill_discrete(name = "Health") +  #this will change the name of legend
  theme(axis.title.x = element_text(colour="DarkGreen", size = 20),
        axis.title.y = element_text(colour="Red", size = 20),
        axis.text.x = element_text(size=15),
        axis.text.y = element_text(size = 15),
        legend.title = element_text(size=15),
        legend.text = element_text(size = 15),
        plot.title = element_text(colour = "DarkBlue", size = 30, family = "Courier"),
        plot.subtitle = element_text(colour = "black",size = 20))






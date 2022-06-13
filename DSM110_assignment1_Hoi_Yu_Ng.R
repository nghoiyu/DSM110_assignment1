#################################################################################
# File Name: DSM110_assignment1_Hoi_Yu_Ng.R 
# Project title: 
# Student: Hoi Yu Ng
# Student No.: 
#################################################################################

# Clear the environment
rm(list=ls())

# Load packages
library(ggplot2)
library(tidyverse)
library(scales)

# ================================================================
# Data import
# ================================================================

## Load and read the PORI's survey data from Github

# Import the 1st telephone survey (15-18 Nov 2021)
PORI_1 <- read.csv('https://raw.githubusercontent.com/nghoiyu/DSM110_assignment1/main/LC2021R1_15112021-18112021_tel_dataset_pori_v1.csv')
head(PORI_1) #inspect the first six rows of the data

# Import the 2nd telephone survey (29 Nov - 03 Dec 2021)
PORI_2<- read.csv('https://raw.githubusercontent.com/nghoiyu/DSM110_assignment1/main/LC2021R4_29112021-03122021_tel_dataset_pori_v1.csv')
head(PORI_2) #inspect the first six rows of the data

# Import the 3rd telephone survey (09-14 Dec 2021)
PORI_3 <- read.csv('https://raw.githubusercontent.com/nghoiyu/DSM110_assignment1/main/LC2021R6_09122021-14122021_online_dataset_pori_v1.csv')
head(PORI_3) #inspect the first six rows of the data

# Inspect the imported data
summary(PORI_1)
summary(PORI_2)
summary(PORI_3)

# ================================================================
# Data wrangling and cleaning
# ================================================================

# Create a new variable for each data set to indicate the order of surveys
PORI_1 <- PORI_1 %>% add_column(survey_no = 1)
PORI_2 <- PORI_2 %>% add_column(survey_no = 2)
PORI_3 <- PORI_3 %>% add_column(survey_no = 3)

## Subset the three data sets by selecting relevant variables
# Relevant variables: surveyCaseid, A003_rating, V3, WillVotegp, District10_2, sex, pob1, agegp_TP_7, 
# edugp_TP, inclin, midgp, civicactive, survey_no

# Define a function for variable selection
select_relvant_variables <- function(dataset) {
  dataset %>% select(SurveyCaseid, A003_rating, v3, WillVotegp, District10_2, 
                    sex, pob1, agegp2, edugp_TP, inclin, midgp, 
                    civicactive, survey_no)
}

PORI_1_1 <- select_relvant_variables(PORI_1) #select variables for PORI_1
PORI_2_1 <- select_relvant_variables(PORI_2) #select variables for PORI_2
PORI_3_1 <- select_relvant_variables(PORI_3) #select variables for PORI_3
                      
# Combine the three survey data sets into one single dataframe
PORI_combined <- bind_rows(PORI_1_1, PORI_2_1, PORI_3_1)
  
# Inspect the combined data set
glimpse(PORI_combined)

# Rename the variables to enhance readibility
PORI_combined <- PORI_combined %>% rename(case_id = SurveyCaseid) %>% 
  rename(ce_rating = A003_rating) %>%
  rename(voting_exp = v3) %>%
  rename(will_vote = WillVotegp) %>%
  rename(constituency = District10_2) %>%
  rename(birth_place = pob1) %>%
  rename(age_group = agegp2) %>%
  rename(education = edugp_TP) %>%
  rename(pol_orientaton = inclin) %>%
  rename(social_class = midgp) %>%
  rename(pol_active = civicactive)

# Recoding the missing values according to the dictionary (code book) of the surveys

PORI_combined <- PORI_combined %>% 
  mutate(ce_rating = ifelse(ce_rating < 0 | ce_rating > 100, NA, ce_rating)) %>% 
  mutate(voting_exp = ifelse(voting_exp < 0 | voting_exp > 100, NA, voting_exp)) %>%
  mutate(will_vote = ifelse(will_vote < 0 | will_vote > 100, NA, will_vote)) %>%
  mutate(constituency = ifelse(constituency < 0, NA, constituency)) %>%
  mutate(sex = ifelse(sex < 0, NA, sex)) %>%
  mutate(birth_place = ifelse(birth_place < 0 | birth_place > 100, NA, birth_place)) %>%
  mutate(age_group = ifelse(age_group < 0, NA, age_group)) %>%
  mutate(education = ifelse(education < 0, NA, education)) %>%
  mutate(pol_orientaton = ifelse(pol_orientaton < 0 | pol_orientaton > 100, NA, pol_orientaton)) %>%
  mutate(social_class = ifelse(social_class < 0 | social_class > 100, NA, social_class)) %>%
  mutate(pol_active = ifelse(pol_active < 0 | pol_active > 100, NA, pol_active))
  
## Correct data type and value labels of the variables

# Check the current data type of the variables
glimpse(PORI_combined) 

# Correct the data type and labels of voting experience
PORI_combined$voting_exp <- factor(PORI_combined$voting_exp, levels=c(1, 2), labels=c("yes", "no"))

# Correct the data type and labels of voting intention
PORI_combined$will_vote <- factor(PORI_combined$will_vote, levels=c(1, 3), labels=c("yes", "no"))

# Correct the data type and labels of constituency
PORI_combined$constituency <- factor(PORI_combined$constituency, 
                                     levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 
                                     labels=c("HK_Island_East", "HK_Island_West", 
                                              "Kowloon_East", "Kowloon_West", "Kowloon_Central",
                                              "NT_Southeast", "NT_North", "NT_Northwest",
                                              "NT_Southwest", "NT_Northeast"))

# Correct the data type and labels of sex
PORI_combined$sex <- factor(PORI_combined$sex, levels=c(1, 2), labels=c("male", "female"))

# Correct the data type and labels of birth place
PORI_combined$birth_place[PORI_combined$birth_place == 1] <- "Hong_Kong"
PORI_combined$birth_place[PORI_combined["birth_place"] == "2"|
                            PORI_combined["birth_place"] == "3"|
                            PORI_combined["birth_place"] == "4"|
                            PORI_combined["birth_place"] == "5"|
                            PORI_combined["birth_place"] == "6"|
                            PORI_combined["birth_place"] == "7"|
                            PORI_combined["birth_place"] == "8"|
                            PORI_combined["birth_place"] == "9"] <- "others"

PORI_combined$birth_place <- as.factor(PORI_combined$birth_place)

# Correct the data type and labels of age group
PORI_combined$age_group <- factor(PORI_combined$age_group, levels=c(1, 2, 3), labels=c("18-29", 
                                                                                       "30-49",
                                                                                       "50_or_above"))
# Correct the data type and labels of education
PORI_combined$education <- factor(PORI_combined$education, levels=c(1, 2, 3), 
                                  labels=c("primary_or_below", "secondary", "tertiary_or_above"))

# Correct the data type and labels of political orientation
PORI_combined$pol_orientaton <- factor(PORI_combined$pol_orientaton, levels=c(1, 2, 3), 
                                  labels=c("pan-democrats", "pro-Beijing", "centre"))

# Correct the data type and labels of social class
PORI_combined$social_class <- factor(PORI_combined$social_class, levels=c(1, 2, 3), 
                                       labels=c("upper", "middle", "lower"))

# Correct the data type and labels of level of political participation
PORI_combined$pol_active <- factor(PORI_combined$pol_active, levels=c(1, 2), labels=c("yes", "no"))

# Correct the data type of survey number
PORI_combined$survey_no <- as.integer(PORI_combined$survey_no)

glimpse(PORI_combined) #Check the current data type of the variables

# Check outliers and other abnormalities
summary(PORI_combined) #Check whole data set
table(PORI_combined$constituency, useNA = "ifany") #Check constituency

# ================================================================
# Exploratory data analysis
# ================================================================

##Visualizing the distribution of the variables

# A histogram to visualise Chief Executive Rating
ggplot(PORI_combined, aes(x = ce_rating)) + 
  geom_histogram(binwidth = 10) + 
  labs(title="Chief Executive rating histogram",
       y= "count",
       x="rating")

# A bar chart to visualise voting experience
ggplot(PORI_combined, aes(x = voting_exp)) + 
  geom_bar() + 
  labs(title="Voting experience bar chart",
       y= "count",
       x="Voting experience")

# A bar chart to visualise voting intention
ggplot(PORI_combined, aes(x = will_vote, y = (..count..)/sum(..count..))) + 
  geom_bar() + 
  scale_y_continuous(labels = percent) +
  labs(title="Voting intention bar chart",
       y= "percentage",
       x="Voting intention")

# A bar chart to visualise constituency distribution
ggplot(PORI_combined, aes(x = constituency, y = (..count..)/sum(..count..))) + 
  geom_bar() + 
  scale_y_continuous(labels = percent) +
  labs(title="Constituency bar chart",
       y= "percentage",
       x="Constituency")

# A bar chart to visualise sex distribution
ggplot(PORI_combined, aes(x = sex, y = (..count..)/sum(..count..))) + 
  geom_bar() + 
  scale_y_continuous(labels = percent) +
  labs(title="Sex bar chart",
       y= "percentage",
       x="Constituency")

# A bar chart to visualise distribution of birthplace
ggplot(PORI_combined, aes(x = birth_place, y = (..count..)/sum(..count..))) + 
  geom_bar() + 
  scale_y_continuous(labels = percent) +
  labs(title="birth place bar chart",
       y= "percentage",
       x="birth place")

# A bar chart to visualise distribution of age groups
ggplot(PORI_combined, aes(x = age_group, y = (..count..)/sum(..count..))) + 
  geom_bar() + 
  scale_y_continuous(labels = percent) +
  labs(title="Age group bar chart",
       y= "percentage",
       x="age group")

# A bar chart to visualise distribution of education levels
ggplot(PORI_combined, aes(x = education, y = (..count..)/sum(..count..))) + 
  geom_bar() + 
  scale_y_continuous(labels = percent) +
  labs(title="Education level bar chart",
       y= "percentage",
       x="education level")

# A bar chart to visualise distribution of political orientation
ggplot(PORI_combined, aes(x = pol_orientaton, y = (..count..)/sum(..count..))) + 
  geom_bar() + 
  scale_y_continuous(labels = percent) +
  labs(title="Political orientation bar chart",
       y= "percentage",
       x="Political orientation")

# A bar chart to visualise the distribution of social class
ggplot(PORI_combined, aes(x = social_class, y = (..count..)/sum(..count..))) + 
  geom_bar() + 
  scale_y_continuous(labels = percent) +
  labs(title="Social class bar chart",
       y= "percentage",
       x="Social class")

# A bar chart to visualise the distribution of politically active respondents
ggplot(PORI_combined, aes(x = pol_active, y = (..count..)/sum(..count..))) + 
  geom_bar() + 
  scale_y_continuous(labels = percent) +
  labs(title="Political participation bar chart",
       y= "percentage",
       x="politically active?")

## Explore patterns between the dependent variable (voting intention) and the independent variables

# Visualise the relationship between between Chief Executive rating and voting intention using a boxplot
ggplot(PORI_combined, aes(x = will_vote, y = ce_rating)) +
  geom_boxplot()

# Visualise the relationship between constituency and voting intention using a stacked bar chart
df <- PORI_combined %>% group_by(constituency) %>% count(will_vote) %>% mutate(Percent = n / sum(n)*100)

ggplot(df, aes(x = constituency, y = Percent, fill = will_vote)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label = paste(round(Percent, digits  = 2),"%"), y = Percent), 
            position = position_stack(vjust = 0.5)) +
  labs(x = "constituency", y = "Percentage",fill = "voting intention")

# Visualise the relationship between sex and voting intention using a stacked bar chart
df <- PORI_combined %>% group_by(sex) %>% count(will_vote) %>% mutate(Percent = n / sum(n)*100)

ggplot(df, aes(x = sex, y = Percent, fill = will_vote)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste(round(Percent, digits  = 2),"%"), y = Percent), 
            position = position_stack(vjust = 0.5)) +
  labs(x = "sex", y = "Percentage", fill = "voting intention")

# Visualise the relationship between sex and voting intention using a stacked bar chart
df <- PORI_combined %>% group_by(sex) %>% count(will_vote) %>% mutate(Percent = n / sum(n)*100)

ggplot(df, aes(x = sex, y = Percent, fill = will_vote)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste(round(Percent, digits  = 2),"%"), y = Percent), 
            position = position_stack(vjust = 0.5)) +
  labs(x = "sex", y = "Percentage", fill = "voting intention")

# Visualise the relationship between birthplace and voting intention using a stacked bar chart
df <- PORI_combined %>% group_by(birth_place) %>% count(will_vote) %>% mutate(Percent = n / sum(n)*100)

ggplot(df, aes(x = birth_place, y = Percent, fill = will_vote)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste(round(Percent, digits  = 2),"%"), y = Percent), 
            position = position_stack(vjust = 0.5)) +
  labs(x = "birth place", y = "Percentage", fill = "voting intention")

# Visualise the relationship between age group and voting intention using a stacked bar chart
df <- PORI_combined %>% group_by(age_group) %>% count(will_vote) %>% mutate(Percent = n / sum(n)*100)

ggplot(df, aes(x = age_group, y = Percent, fill = will_vote)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste(round(Percent, digits  = 2),"%"), y = Percent), 
            position = position_stack(vjust = 0.5)) +
  labs(x = "age group", y = "Percentage", fill = "voting intention")

# Visualise the relationship between education and voting intention using a stacked bar chart
df <- PORI_combined %>% group_by(education) %>% count(will_vote) %>% mutate(Percent = n / sum(n)*100)

ggplot(df, aes(x = education, y = Percent, fill = will_vote)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste(round(Percent, digits  = 2),"%"), y = Percent), 
            position = position_stack(vjust = 0.5)) +
  labs(x = "education", y = "Percentage", fill = "voting intention")

# Visualise the relationship between political orientation and voting intention using a stacked bar chart
df <- PORI_combined %>% group_by(pol_orientaton) %>% count(will_vote) %>% mutate(Percent = n / sum(n)*100)

ggplot(df, aes(x = pol_orientaton, y = Percent, fill = will_vote)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste(round(Percent, digits  = 2),"%"), y = Percent), 
            position = position_stack(vjust = 0.5)) +
  labs(x = "political orientation", y = "Percentage", fill = "voting intention")

# Visualise the relationship between level of political participation and voting intention using a stacked bar chart
df <- PORI_combined %>% group_by(pol_active) %>% count(will_vote) %>% mutate(Percent = n / sum(n)*100)

ggplot(df, aes(x = pol_active, y = Percent, fill = will_vote)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste(round(Percent, digits  = 2),"%"), y = Percent), 
            position = position_stack(vjust = 0.5)) +
  labs(x = "politically active?", y = "Percentage", fill = "voting intention")

# ================================================================
#                           END OF FILE
# ================================================================

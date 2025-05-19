# Libraries ####

library(tidyverse)
library(readxl)
library(lubridate)
library(dlookr)
library(ggpubr)
library(tools)
library(ggcorrplot)
library(corrplot)
library(performance)
library(see)
library(ggstatsplot)
library(parameters)
library(modelsummary)


# Data ####
Baseline <- read_excel("Data/SEL Bangladesh Data Cleaned.xlsx")

# Global Vars
theme_cmx<- function (base_size = 11, base_family = "") {
  theme(
    legend.position = "top",
    legend.margin = margin(t = 15), # modify x axis-legend spacing
    panel.background = element_rect(fill = "#FBFBFB"),
    panel.border = element_rect(colour = "#fbfefb", fill = NA),
    axis.line = element_blank(),
    panel.grid.major.x =  element_line(colour = "#dce2e5", size = 0.3),
    panel.grid.major.y =  element_line(colour = "#dce2e5", size = 0.3)
  )
}
sc1 <- "#fba536"
sc2 <- "#5B8BA5"
sc3 <- "#37B5FA"
sc4 <-"#FA6137"
sc5 <- "#A65B60"
sc6 <- "#5BA698"

# Global Vars
shade1 <- "#FAA537"
shade2 <- "#37B5FA"
shade3 <- "#586E7A"
shade4 <- "#8B5C1F"
shade5 <- "#FA6D37"


Baseline <- Baseline %>%
  rename(Date = date) %>%
  mutate(`% Maths score` = math.scores/60*100) %>%
  mutate(`% English score` = english.scores/60*100) %>%
  mutate(across(where(is.numeric), round, 0)) %>%
  rename(Grade = grade, 
         Gender = sex)


Baseline$Gender <- toTitleCase(Baseline$Gender)


# Metadata Analysis ####
# Convert date functions 
Baseline$started_time <- as.POSIXlt(Baseline$started_time, format = "%Y-%m-%d %H:%M:%OS")
Baseline$completed_time <- as.POSIXlt(Baseline$completed_time , format="%Y-%m-%d %H:%M:%OS")
Baseline$received_on <- as.POSIXlt(Baseline$received_on, format="%Y-%m-%d %H:%M:%OS")
Baseline$survey.duration <- difftime(Baseline$completed_time, Baseline$started_time, units = "mins")
Baseline$survey.duration <- as.numeric(Baseline$survey.duration)
Baseline$survey.duration <- round(Baseline$survey.duration, digits = 0)

## Analysis of Duration ####
Duration <- Baseline %>%
  select(hq_user, survey.duration) %>%
  group_by(hq_user) %>%
  describe()

Duration <- Duration %>%
  select(c(2, 3,11,5,27)) %>%
  rename(Enumerator=hq_user, 
         responses=n,
         `min`=p00, 
         `mean` = mean, 
         `max`=p100) %>%
  arrange(desc(responses)) %>%
  mutate(across(where(is.numeric), round, 0))

Baseline %>%
  select(survey.duration) %>%
  filter(survey.duration<=10) %>%
  summarise(count=n())


# Frequency Analysis #####

# Grade frequency 
grade.freq <- Baseline %>%
  select(Grade) %>%
  group_by(Grade) %>%
  summarise(count=n()) %>%
  mutate(`Percent (%)` = count/sum(count)*100) %>%
  mutate(across(where(is.numeric), round, 0))

# Gender frequency 
gender.freq <- Baseline %>%
  select(Gender) %>%
  group_by(Gender) %>%
  summarise(count=n()) %>%
  mutate(`Percent (%)` = count/sum(count)*100) %>%
  mutate(across(where(is.numeric), round, 0))

# Grade and gender frequency
grade.gender <- Baseline %>%
  select(Grade, Gender) %>%
  group_by(Grade, Gender) %>%
  summarise(count=n()) %>%
  mutate(`Percent (%)` = count/1431*100) %>%
  mutate(across(where(is.numeric), round, 0))


# Academic Scores Analysis ####

## Histogram of math scores ####
math.score <- ggplot(Baseline, aes(`% Maths score`)) + 
  geom_histogram(bins = 17, fill=shade3) +
  theme_cmx()


## Maths grade only ####
math.grade <- Baseline %>%
  select(Grade, `% Maths score`) %>%
  group_by(Grade) %>%
  describe(`% Maths score`) %>%
  mutate(across(where(is.numeric), round, 0))

  
math.grade <- math.grade %>%
  select(c(2, 3, 5,6,16,19,22)) %>%
  rename(`Mean (%)` = mean, 
         `Median (%)` =p50) 
math.grade
  
## Math grade gender ####
math.grade.gender <- Baseline %>%
  select(Gender, Grade, `% Maths score`) %>%
  group_by(Gender, Grade) %>%
  describe(`% Maths score`)

math.grade.gender <- math.grade.gender %>%
  select(c(2, 3,4,6,7, 17,20,23)) %>%
  rename(`Mean (%)` = mean, 
         `Median (%)` =p50) %>%
  mutate(across(where(is.numeric), round, 0))


math.grade.gender$Category <- paste("Grade", math.grade.gender$Grade, math.grade.gender$Gender, sep = " ")

# order the categories
math.grade.gender$Category <- factor(math.grade.gender$Category, ordered = T,
                                     levels = c("Grade 1 Male", "Grade 1 Female", "Grade 2 Male", "Grade 2 Female","Grade 3 Male", 
                                                "Grade 3 Female"))
# Re-order the columns
math.grade.gender <- math.grade.gender %>%
  select(9, 3:8)

math.grade.gender <- math.grade.gender[order(math.grade.gender$Category),]


## English histogram ####
english.score <- ggplot(Baseline, aes(`% English score`)) + 
  geom_histogram(bins = 17, fill=shade3) +
  theme_cmx()


## English grade only ####
eng.grade <- Baseline %>%
  select(Grade, `% English score`) %>%
  group_by(Grade) %>%
  describe(`% English score`) %>%
  mutate(across(where(is.numeric), round, 0))

eng.grade <- eng.grade %>%
  select(c(2, 3, 5,6,16,19,22)) %>%
  rename(`Mean (%)` = mean, 
         `Median (%)` =p50) 

## English grade gender ####
eng.grade.gender <- Baseline %>%
  select(Gender, Grade, `% English score`) %>%
  group_by(Gender, Grade) %>%
  describe(`% English score`)

eng.grade.gender <- eng.grade.gender %>%
  select(c(2, 3,4,6,7, 17,20,23)) %>%
  rename(`Mean (%)` = mean, 
         `Median (%)` =p50) %>%
  mutate(across(where(is.numeric), round, 0))


eng.grade.gender$Category <- paste("Grade", eng.grade.gender$Grade, eng.grade.gender$Gender, sep = " ")

# order the categories
eng.grade.gender$Category <- factor(eng.grade.gender$Category, ordered = T,
                                     levels = c("Grade 1 Male", "Grade 1 Female", "Grade 2 Male", "Grade 2 Female","Grade 3 Male", 
                                                "Grade 3 Female"))
# Re-order the columns
eng.grade.gender <- eng.grade.gender %>%
  select(9, 3:8)

eng.grade.gender <- eng.grade.gender[order(eng.grade.gender$Category),]

# Correlation Tests ####
Baseline %>%
  select(`Would you ask the child why they are not talking to you`) %>%
  group_by(`Would you ask the child why they are not talking to you`) %>%
  summarise(count=n()) %>%
  mutate(percent = count/sum(count)*100) %>%
  arrange(desc(count))


# Association / Relationship ####

## Relationship between the academic scores ####
Baseline$Grade <- as.character(Baseline$Grade)

### Scatter plot of math & English scores per Grade ####
math.eng.scatterplot <- ggplot(Baseline, aes(x = `% English score`, y = `% Maths score`)) + 
  geom_point(aes(color=Grade)) + 
  scale_color_manual(values = c(sc1, sc6, sc5)) +
  theme_cmx()
math.eng.scatterplot


## GG Scatter plot per grade ####
grade.corr <- ggscatter(Baseline, x = "% English score", y =  "% Maths score", 
          add = "reg.line", conf.int = TRUE, color = "#5798BA",
          cor.coef = TRUE, cor.method = "pearson", facet.by = 'Grade', ellipse = TRUE,
          xlab = "English scores (%)", ylab = "Math scores(%)") + theme_cmx()


# Machine Learning ####
## Linear Regression ####

# Create a df Subset of explanatory variables
raw.baseline.ml <- Baseline %>%
  select(12:14, 17, 18, 19, 20, 22:30, 32:57)


# Create coded categorical variables for Regression
raw.baseline.ml.2 <- raw.baseline.ml |> 
  mutate_if(is.character, as.factor) |>
  mutate_if(is.factor, as.numeric)

### Math model ####
math.data <- raw.baseline.ml.2 %>%
  select(-english.scores)

##
math.score.model = lm(math.scores ~., data = math.data)
summary(math.score.model)
plot(math.score.model)

#### Math model regression evaluation parameters ####
check_model(math.score.model)
model_performance(math.score.model)
math.model.residulas = math.score.model$residuals
qqnorm(math.model.residulas)
# ggcoefstats(math.score.model)
mp <- model_parameters(math.score.model, summary = TRUE)

#### Maths Partial Regression Coefficients ####
mp <- mp %>%
  mutate(across(where(is.numeric), round, 2))


# Sorted p-=Value
p.mp <- mp %>%
  mutate(across(where(is.numeric), round, 2))  %>%
  arrange(Coefficient)

coef.mp <- mp %>%
  mutate(across(where(is.numeric), round, 2))  %>%
  arrange(Coefficient)
  

#### Visualising of Model Summaries ####
# ggcoefstats(math.score.model) +
#   theme_cmx()


### English model ####
eng.data <- raw.baseline.ml.2 %>%
  select(-math.scores)

# Model
eng.score.model = lm(english.scores ~., data = eng.data)
summary(eng.score.model)
plot(eng.score.model)


# Interesting regression evaluation
check_model(eng.score.model)
eng.model.residulas = eng.score.model$residuals
qqnorm(eng.model.residulas)
# ggcoefstats(math.score.model)
eng.mp <- model_parameters(eng.score.model, summary = TRUE)

eng.mp <- eng.mp %>%
  mutate(across(where(is.numeric), round, 2))
  

# Sorted p-=Value
eng.p <- eng.mp %>%
  mutate(across(where(is.numeric), round, 2))  %>%
  arrange(Coefficient)

coef.eng <- eng.mp %>%
  mutate(across(where(is.numeric), round, 2))  %>%
  arrange(Coefficient)


# Compare Performance of English and Maths Models
plot(compare_performance(math.score.model, eng.score.model, verbose = F, rank = T))

# Remove the Math Response Variable
# math.explanatory.vars <- subset(math.data, select = -math.scores)
# corr_matrix = round(cor(math.explanatory.vars), 1)
# 
# corr.matrx <- round(cor(math.explanatory.vars),
#       digits = 1 # rounded to 2 decimals
# )
# corr.matrx <- as.data.frame(corr.matrx)
# 
# 
# ggcorrplot(corr_matrix, hc.order = TRUE, type = "lower",
#            lab = TRUE)

modelplot(math.score.model) + 
  theme_cmx()


BDG <- read.csv("Data/ENGAGE 2.0 Bangladesh -Full Scale Primary Classroom Observation_DL-12-MAY-CLEANED.csv")

write.csv(BDG %>%
            select(centre) %>%
            group_by(centre) %>%
            summarise(count=n()),"Output/count.csv")



###########################################################################################
###########################################################################################
#--------------------------XYZ Company----------------------------#
# -------------------------HR Analytics---------------------------- #
###########################################################################################
###########################################################################################
###########################################################################################
# Section 1: Business Understanding
# Section 2: Data Understanding
# Section 3: Data Preparation & EDA
# Section 4: Model Building
# Section 5: Model Evaluation
###########################################################################################

# SECTION 1: Business Understanding

# Total Number of employees 4000
# 16% leave every year - 600 employees in total

# Management believes this level of attrition is bad for the company, because of the following reasons -
# 1. Projects get delayed, which makes it difficult to meet timelines, resulting in a reputation loss
# 2. A sizeable HR department has to be maintained for the purposes of recruiting new talent
# 3. the new employees have to be trained for the job and/or given time to acclimatise themselves to 
# the company

# AIM of Case Study

# UNDERSTAND the factors behind this high rate of attrition in order to curb it
#   1. What changes should they make to the workplace so that most of their employees stay?
#   2. Which of the variables is most important and needs to be addressed right away?

###########################################################################################
###########################################################################################
#                                   Intall & Load libraries
###########################################################################################
###########################################################################################

load.libraries <- c('data.table', 'dplyr', 'MASS', 'car', 'lattice',
                    'cowplot', 'lubridate', 'corrplot', 'caTools', 'e1071',
                    'Rcpp', 'ddalpha', 'recipes', 'dimRed', 'gower', 'prodlim', 'caret', 
                    'stringr', 'tidyr', 'lubridate', 'ggplot2', 'GGally', 'reshape2',
                    'DescTools', 'gridExtra','scales', 'plotrix', 'psych',
                    'effects', 'RcppRoll', 'ROCR', 'Information')
install.libs <- load.libraries[!load.libraries %in% installed.packages()]

for (libs in install.libs) 
  install.packages(libs, dependencies = TRUE)

sapply(load.libraries, require, character = TRUE)

###########################################################################################
###########################################################################################
#                                   Data loding
###########################################################################################
###########################################################################################
general_data <-
  read.csv("general_data.csv",
    stringsAsFactors = F)

emp_survey_data <-
  read.csv("employee_survey_data.csv",
    stringsAsFactors = F)

manager_survey_data <-
  read.csv("manager_survey_data.csv",
    stringsAsFactors = F)

in_time <-
  read.csv("in_time.csv",
    stringsAsFactors = F)

out_time <-
  read.csv("out_time.csv",
    stringsAsFactors = F)

View(general_data)
View(emp_survey_data)
View(manager_survey_data)
View(in_time)
View(out_time)
###########################################################################################
###########################################################################################
#                           Data Cleaning and Basic processing
###########################################################################################
###########################################################################################
colnames(in_time)[colnames(in_time) == "X"] <- "EmployeeID"
colnames(out_time)[colnames(out_time) == "X"] <- "EmployeeID"

setdiff(emp_survey_data$EmployeeID, general_data$EmployeeID)
setdiff(emp_survey_data$EmployeeID, manager_survey_data$EmployeeID)
setdiff(emp_survey_data$EmployeeID, in_time$EmployeeID)
setdiff(emp_survey_data$EmployeeID, out_time$EmployeeID)

length(unique(tolower(emp_survey_data$EmployeeID)))
length(unique(tolower(manager_survey_data$EmployeeID)))
length(unique(tolower(general_data$EmployeeID)))
length(unique(tolower(in_time$EmployeeID)))
length(unique(tolower(out_time$EmployeeID)))

####################################################
# Considering only time as date is mentioned in the column header
date_time_converter <- function(data_frame) {
  data_frame[, 2:ncol(data_frame)] <-
    as.data.frame(sapply(data_frame[, 2:ncol(data_frame)],
                         function(x) {
                           as.POSIXct(strptime(x, format = "%Y-%m-%d %H:%M:%S"))
                         }))
  return(data_frame)
}

in_time <- date_time_converter(in_time)
out_time <- date_time_converter(out_time)
setdiff(in_time$EmployeeID, out_time$EmployeeID)

#create matrix to store working hours
working_hours <- matrix(nrow = nrow(in_time), ncol = ncol(in_time))
working_hours <- data.frame(working_hours)

for (i in 2:ncol(in_time)) {
  working_hours[, i] <- (out_time[, i] - in_time[, i]) / 3600
}
View(working_hours)
#calculate average working hours
avg_working_hours <- apply(working_hours, 1, mean, na.rm = T)
summary(avg_working_hours)

emp_work_hours <-
  data.frame(EmployeeID = general_data$EmployeeID,
             AvgWorkingHrs = avg_working_hours)

# Create merged data frame
emp_df <- merge(emp_survey_data, general_data, by = "EmployeeID")
emp_df <- merge(emp_df, manager_survey_data, by = "EmployeeID")
emp_df <- merge(emp_df, emp_work_hours, by = "EmployeeID")
View(emp_df)
##########################################################################################
###########################################################################################
#                                 Exploratory Data Analysis
###########################################################################################
###########################################################################################
# Check for missing values
sapply(emp_df, function(x) {
  sum(is.na(x))
})
#   Variable                    count of NA's
#   EnvironmentSatisfaction         25
#   JobSatisfaction                 20
#   WorkLifeBalance                 38
#   NumCompaniesWorked              19
#   TotalWorkingYears               9

# Calculate percentage of missing values
max(colMeans(is.na(emp_df)))
# 0.86%

emp_df$Attrition <- as.factor(emp_df$Attrition)
summary(emp_df$Attrition)

Attrition_Rate <-
  length(which(emp_df$Attrition == "Yes")) / nrow(emp_df)
Attrition_Rate # 16.12% Attrition rate.

###########################################################################################
#                                   Scatterplot matrix
###########################################################################################

pairs.panels(
  emp_df[, c(9, 17, 20, 25, 26, 27, 30)],
  method = "pearson",
  # correlation method
  hist.col = "#00AFBB",
  density = TRUE,
  # show density plots
  ellipses = TRUE # show correlation ellipses
)
# Scatter plots
scatter_plot <-
  data.frame(sapply(emp_df[, c(9, 17, 20, 25, 26, 27, 30)],
    function(x) {
      quantile(x, seq(0, 1, .1))
    }))

names <- rownames(scatter_plot)
scatter_plot <- cbind(names, scatter_plot)

d <- melt(scatter_plot, id.vars = "names")

###########################################################################################
# Everything on the same plot
ggplot(d, aes(names, value, col = variable)) +
  geom_point() +
  stat_smooth() +
  facet_wrap( ~ variable, scales = "free") +
  scale_x_discrete(limits = c(
    "0%",
    "10%",
    "20%",
    "30%",
    "40%",
    "50%",
    "60%",
    "70%",
    "80%",
    "90%",
    "100%"
  ))
###########################################################################################
#Average working hours
boxplot(
  emp_df$AvgWorkingHrs,
  col = "lightblue",
  medcol = "#B6FF00FF",
  whiskcol = "#00FF24FF",
  staplecol = "#00FFFFFF",
  boxcol = "#0024FFFF",
  outcol = "#B600FFFF",
  outbg = "#FF006D66",
  outcex = 3,
  outpch = 21
)
###########################################################################################
#distance from home
boxplot(
  emp_df$DistanceFromHome,
  col = "green",
  medcol = "#B6FF00FF",
  whiskcol = "#00FF24FF",
  staplecol = "#00FFFFFF",
  boxcol = "#0024FFFF",
  outcol = "#B600FFFF",
  outbg = "#FF006D66",
  outcex = 3,
  outpch = 21
)
###########################################################################################
#salary
salary <- boxplot(
  emp_df$MonthlyIncome,
  col = "darkgreen",
  medcol = "#B6FF00FF",
  whiskcol = "#00FF24FF",
  staplecol = "#00FFFFFF",
  boxcol = "#0024FFFF",
  outcol = "#B600FFFF",
  outbg = "#FF006D66",
  outcex = 3,
  outpch = 21
)
###########################################################################################
salary$out
##approx 300 outliers

bar_theme1 <-
  theme(axis.text.x = element_text(
    angle = 30,
    hjust = 1,
    vjust = 0.5
  ),
    legend.position = "none")
###########################################################################################
#                     Histogram and Boxplots for numeric variables
###########################################################################################
plot_grid(
  ggplot(emp_df, aes(x = Age, fill = Attrition)) + geom_histogram(binwidth =
      10) + bar_theme1,
  ggplot(emp_df, aes(x = DistanceFromHome, fill = Attrition)) + geom_histogram(binwidth =
      5) + bar_theme1,
  ggplot(emp_df, aes(x = MonthlyIncome, fill = Attrition)) + geom_histogram(binwidth =
      10000) + bar_theme1,
  ggplot(emp_df, aes(x = PercentSalaryHike, fill = Attrition)) + geom_histogram(binwidth =
      5) + bar_theme1,
  ggplot(emp_df, aes(x = TotalWorkingYears, fill = Attrition)) + geom_histogram(binwidth =
      5) + bar_theme1,
  ggplot(emp_df, aes(x = YearsAtCompany, fill = Attrition)) + geom_histogram(binwidth =
      10) + bar_theme1,
  ggplot(emp_df, aes(x = YearsSinceLastPromotion, fill = Attrition)) + geom_histogram(binwidth =
      2) + bar_theme1,
  ggplot(emp_df, aes(x = YearsWithCurrManager, fill = Attrition)) + geom_histogram(binwidth =
      2) + bar_theme1,
  align = "h"
)

###########################################################################################
#                                       Bar Charts
###########################################################################################
#Experience VS Attrition rate
ggplot(emp_df,
  aes(factor(emp_df$TotalWorkingYears), fill = Attrition)) +
  geom_bar(aes(y = prop.table(..count..) * 100 + 0.5)) +
  geom_text(
    aes(
      y = round(prop.table(..count..) * 100, 2),
      label = paste0(round(prop.table(..count..) * 100, 2), '%')
    ),
    stat = 'count',
    position = position_dodge(0.7),
    size = 3
  ) +
  labs(title = "Experience Vs Attrition",
    x = "Years of experience",
    y = "Total") +
  theme(plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90, hjust = 1))

###########################################################################################
# Department
ggplot(emp_df,
  aes(factor(emp_df$Department), fill = Attrition)) +
  geom_bar(aes(y = prop.table(..count..) * 100),
    position = "dodge") +
  geom_text(
    aes(
      y = round(prop.table(..count..) * 100 + 0.5, 2),
      label = paste0(round(prop.table(..count..) * 100, 2), '%')
    ),
    stat = 'count',
    position = position_dodge(.9),
    size = 3
  ) +
  labs(title = "Department Vs Attrition",
    x = "Department",
    y = "Total") +
  theme(plot.title = element_text(hjust = 0.5))

###########################################################################################
#travel vs. Attrition rate
ggplot(emp_df,
  aes(emp_df$BusinessTravel, fill = Attrition)) +
  geom_bar(aes(y = prop.table(..count..) * 100),
    position = "dodge") +
  geom_text(
    aes(
      y = round(prop.table(..count..) * 100 + 0.5, 2),
      label = paste0(round(prop.table(..count..) * 100, 2), '%')
    ),
    stat = 'count',
    position = position_dodge(.9),
    size = 3
  ) +
  labs(title = "Travel type Vs Attrition",
    x = "Travel Type",
    y = "Total") +
  theme(plot.title = element_text(hjust = 0.5))
###########################################################################################
#Education
ggplot(emp_df,
  aes(factor(emp_df$EducationField), fill = Attrition)) +
  geom_bar(aes(y = prop.table(..count..) * 100),
    position = "dodge") +
  geom_text(
    aes(
      y = round(prop.table(..count..) * 100 + 0.5, 2),
      label = paste0(round(prop.table(..count..) * 100, 2), '%')
    ),
    stat = 'count',
    position = position_dodge(.9),
    size = 3
  ) +
  labs(title = "Education Field Vs Attrition",
    x = "Education Field",
    y = "Total") +
  theme(plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1))
###########################################################################################
#Job Role
ggplot(emp_df,
  aes(factor(emp_df$JobRole), fill = Attrition)) +
  geom_bar(aes(y = prop.table(..count..) * 100),
    position = "dodge") +
  geom_text(
    aes(
      y = round(prop.table(..count..) * 100 + 0.5, 2),
      label = paste0(round(prop.table(..count..) * 100, 2), '%')
    ),
    stat = 'count',
    position = position_dodge(.9),
    size = 3
  ) +
  labs(title = "Job Role Vs Attrition",
    x = "Job Role",
    y = "Total") +
  theme(plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1))
###########################################################################################
#Job Satisfaction
ggplot(emp_df,
  aes(factor(emp_df$JobSatisfaction), fill = Attrition)) +
  geom_bar(aes(y = prop.table(..count..) * 100),
    position = "dodge") +
  geom_text(
    aes(
      y = round(prop.table(..count..) * 100 + 0.5, 2),
      label = paste0(round(prop.table(..count..) * 100, 2), '%')
    ),
    stat = 'count',
    position = position_dodge(.9),
    size = 3
  ) +
  labs(title = "Job Satisfaction Vs Attrition",
    x = "Job Satisfaction",
    y = "Total") +
  theme(plot.title = element_text(hjust = 0.5))
###########################################################################################
#No of Companies Worked
ggplot(emp_df,
  aes(factor(emp_df$NumCompaniesWorked), fill = Attrition)) +
  geom_bar(aes(y = prop.table(..count..) * 100),
    position = "dodge") +
  geom_text(
    aes(
      y = round(prop.table(..count..) * 100 + 0.5, 2),
      label = paste0(round(prop.table(..count..) * 100, 2), '%')
    ),
    stat = 'count',
    position = position_dodge(.9),
    size = 3
  ) +
  labs(title = "No of Companies Worked Vs Attrition",
    x = "No of Companies Worked",
    y = "Total") +
  theme(plot.title = element_text(hjust = 0.5))
###########################################################################################
#Environment Satisfaction
ggplot(emp_df,
  aes(factor(emp_df$EnvironmentSatisfaction), fill = Attrition)) +
  geom_bar(aes(y = prop.table(..count..) * 100),
    position = "dodge") +
  geom_text(
    aes(
      y = round(prop.table(..count..) * 100 + 0.5, 2),
      label = paste0(round(prop.table(..count..) * 100, 2), '%')
    ),
    stat = 'count',
    position = position_dodge(.9),
    size = 3
  ) +
  labs(title = "Environment Satisfaction Vs Attrition",
    x = "Environment Satisfaction",
    y = "Total") +
  theme(plot.title = element_text(hjust = 0.5))
####Employee tend to churn if environmental satisfaction score is 1
###########################################################################################
#work life balance
ggplot(emp_df,
  aes(factor(emp_df$WorkLifeBalance), fill = Attrition)) +
  geom_bar(aes(y = prop.table(..count..) * 100),
    position = "dodge") +
  geom_text(
    aes(
      y = round(prop.table(..count..) * 100 + 0.5, 2),
      label = paste0(round(prop.table(..count..) * 100, 2), '%')
    ),
    stat = 'count',
    position = position_dodge(.9),
    size = 3
  ) +
  labs(title = "Work Life Balance Vs Attrition",
    x = "Work Life Balance",
    y = "Total") +
  theme(plot.title = element_text(hjust = 0.5))
####Employee tend to churn if Work ife balance score is 1
###########################################################################################
#Marital status
ggplot(emp_df,
  aes(factor(emp_df$MaritalStatus), fill = Attrition)) +
  geom_bar(aes(y = prop.table(..count..) * 100),
    position = "dodge") +
  geom_text(
    aes(
      y = round(prop.table(..count..) * 100 + 0.5, 2),
      label = paste0(round(prop.table(..count..) * 100, 2), '%')
    ),
    stat = 'count',
    position = position_dodge(.9),
    size = 3
  ) +
  labs(title = "Marital status Vs Attrition",
    x = "Marital status",
    y = "Total") +
  theme(plot.title = element_text(hjust = 0.5))

###########################################################################################
###########################################################################################
#                           Missing Value Imputation & Clensing
###########################################################################################
###########################################################################################
# remove EmployeeCount, Over18 and StandardHours column since they
# hold same value for all rows.
emp_df$EmployeeCount <- NULL
emp_df$Over18 <- NULL
emp_df$StandardHours <- NULL
###########################################################################################
# assign numeric levels 1 and 0 to Attrition column
levels(emp_df$Attrition) <- c(0, 1)
emp_df$Attrition <-
  as.numeric(levels(emp_df$Attrition))[emp_df$Attrition]
###########################################################################################
# scale MonthlyIncome
emp_df$MonthlyIncome <- scale(emp_df$MonthlyIncome)
###########################################################################################
# create WOE and IV table for all variables
WOE_IV <-
  create_infotables(emp_df[, -1],
    y = "Attrition",
    bins = 10,
    parallel = FALSE)
###########################################################################################
# 1. Missing Value handling - Variable "EnvironmentSatisfaction"
WOE_IV$Tables$EnvironmentSatisfaction

#EnvironmentSatisfaction    N     Percent         WOE           IV
#1                      NA   25 0.005668934  0.26285100 0.0004272596
#2                   [1,1]  845 0.191609977  0.56154813 0.0727103366
#3                   [2,2]  856 0.194104308 -0.08912542 0.0742060233
#4                   [3,3] 1350 0.306122449 -0.18472559 0.0840105683
#5                   [4,4] 1334 0.302494331 -0.21532446 0.0970352143


# using WOE values for EnvironmentSatisfaction
emp_df$EnvironmentSatisfaction <-
  as.factor(emp_df$EnvironmentSatisfaction)
levels(emp_df$EnvironmentSatisfaction) <- c(.56, -.09, -.19, -.22)
emp_df$EnvironmentSatisfaction <-
  as.numeric(levels(emp_df$EnvironmentSatisfaction))[emp_df$EnvironmentSatisfaction]

# replace missing values with WoE for NA
emp_df$EnvironmentSatisfaction[which(is.na(emp_df$EnvironmentSatisfaction))] <-
  0.26

###########################################################################################
# 2. Missing Value handling - Variable "JobSatisfaction"
###########################################################################################
WOE_IV$Tables$JobSatisfaction

#JobSatisfaction    N     Percent         WOE          IV
#1              NA   20 0.004535147 -1.29529362 0.004831515
#2           [1,1]  860 0.195011338  0.43557410 0.047446739
#3           [2,2]  840 0.190476190  0.02246564 0.047543607
#4           [3,3] 1323 0.300000000  0.03152187 0.047844887
#5           [4,4] 1367 0.309977324 -0.40020037 0.091057122

# WOE is not monotonic. Need to perform coarse classing by combining value of 2 and 3 in one category

###################### Creating a generic function to compute WOE ######################
total_good <- length(emp_df$Attrition[which(emp_df$Attrition == 1)])
total_bad <- length(emp_df$Attrition[which(emp_df$Attrition == 0)])
computeWoE <- function(local_good, local_bad) {
  woe = log(local_good / total_good) - log(local_bad / total_bad)
  return(woe)
}
########################################################################################


# Performing coarse classing:
job_satisfaction_comb <-
  subset(emp_df, (JobSatisfaction == 2 | JobSatisfaction == 3))
comb_good <-
  length(job_satisfaction_comb$Attrition[which(job_satisfaction_comb$Attrition ==
      1)])
comb_bad <-
  length(job_satisfaction_comb$Attrition[which(job_satisfaction_comb$Attrition ==
      0)])
combined_woe <- computeWoE(comb_good, comb_bad)
combined_woe  # Combined WOE for 2 and 3 === .028


# using WOE values for JobSatisfaction
emp_df$JobSatisfaction <- as.factor(emp_df$JobSatisfaction)
levels(emp_df$JobSatisfaction) <- c(.43, .28, .28, -.40)
emp_df$JobSatisfaction <-
  as.numeric(levels(emp_df$JobSatisfaction))[emp_df$JobSatisfaction]

# replace missing values with WoE for NA
emp_df$JobSatisfaction[which(is.na(emp_df$JobSatisfaction))] <-
  -1.29

###########################################################################################
# 3. Missing Value handling - Variable "WorkLifeBalance"
###########################################################################################
WOE_IV$Tables$WorkLifeBalance

# WorkLifeBalance    N    Percent         WOE          IV
#1              NA   38 0.00861678 -0.49092080 0.001750523
#2           [1,1]  239 0.05419501  0.86676705 0.054752396
#3           [2,2] 1019 0.23106576  0.04792828 0.055291837
#4           [3,3] 2660 0.60317460 -0.14261411 0.066975398
#5           [4,4]  454 0.10294785  0.12201610 0.068572099

# WOE is not monotonic. Need to perform coarse classing by combining value of 3 and 4 in one category

emp_df$WorkLifeBalance <- as.factor(emp_df$WorkLifeBalance)

# Performing coarse classing:
worklife_bal_comb <-
  subset(emp_df, (WorkLifeBalance == 3 | WorkLifeBalance == 4))
comb_good <-
  length(worklife_bal_comb$Attrition[which(worklife_bal_comb$Attrition ==
      1)])
comb_bad <-
  length(worklife_bal_comb$Attrition[which(worklife_bal_comb$Attrition ==
      0)])
combined_woe <- computeWoE(comb_good, comb_bad)
combined_woe  # Combined WOE for 2 and 3 === -0.10

# using WOE values for WorkLifeBalance
levels(emp_df$WorkLifeBalance) <- c(.86, .04, -.10, -.10)
emp_df$WorkLifeBalance <-
  as.numeric(levels(emp_df$WorkLifeBalance))[emp_df$WorkLifeBalance]

# replace missing values with WoE for NA
emp_df$WorkLifeBalance[which(is.na(emp_df$WorkLifeBalance))] <- -.10


###########################################################################################
# 4. Missing Value handling - Variable "NumCompaniesWorked"
###########################################################################################
WOE_IV$Tables$NumCompaniesWorked

#  NumCompaniesWorked    N    Percent        WOE           IV
#1                 NA   19 0.00430839  0.3273895 0.0005142402
#2              [0,0]  586 0.13287982 -0.3647910 0.0160985101
#3              [1,1] 1558 0.35328798  0.1864906 0.0291736101
#4              [2,2]  438 0.09931973 -0.4458004 0.0460798546
#5              [3,3]  474 0.10748299 -0.5340930 0.0715324358
#6              [4,4]  415 0.09410431 -0.3387290 0.0811360223
#7              [5,6]  395 0.08956916  0.4853299 0.1058076762
#8              [7,9]  525 0.11904762  0.2628510 0.1147801276
# WOE is not monotonic. Need to perform coarse classing

# Performing coarse classing by combining value of 1,2,3,4 in one category
num_companies_1234 <-
  subset(
    emp_df,
    (
      NumCompaniesWorked == 1 | NumCompaniesWorked == 2 |
        NumCompaniesWorked == 3 | NumCompaniesWorked == 4
    )
  )
comb_good <-
  length(num_companies_1234$Attrition[which(num_companies_1234$Attrition ==
      1)])
comb_bad <-
  length(num_companies_1234$Attrition[which(num_companies_1234$Attrition ==
      0)])
combined_woe <- computeWoE(comb_good, comb_bad)
combined_woe  # Combined WOE  === -.07
# Performing coarse classing by combining value of 5,6,8,9 in one category
num_companies_56789 <-
  subset(
    emp_df,
    (
      NumCompaniesWorked == 5 | NumCompaniesWorked == 6 |
        NumCompaniesWorked == 7 |
        NumCompaniesWorked == 8 | NumCompaniesWorked == 9
    )
  )
comb_good <-
  length(num_companies_56789$Attrition[which(num_companies_56789$Attrition ==
      1)])
comb_bad <-
  length(num_companies_56789$Attrition[which(num_companies_56789$Attrition ==
      0)])
combined_woe <- computeWoE(comb_good, comb_bad)
combined_woe  # Combined WOE  === .36

# using WOE values for NumCompaniesWorked
emp_df$NumCompaniesWorked <- as.factor(emp_df$NumCompaniesWorked)
levels(emp_df$NumCompaniesWorked) <-
  c(-.36, -.07, -.07, -.07, -.07, -.36, .36, .36, .36, .36)
emp_df$NumCompaniesWorked <-
  as.numeric(levels(emp_df$NumCompaniesWorked))[emp_df$NumCompaniesWorked]

# replace missing values with WoE for NA
emp_df$NumCompaniesWorked[which(is.na(emp_df$NumCompaniesWorked))] <-
  .33

###########################################################################################
# 5. Missing Value handling - Variable "TotalWorkingYears"
###########################################################################################
WOE_IV$Tables$TotalWorkingYears


#TotalWorkingYears   N     Percent        WOE           IV
#1                 NA   9 0.002040816  0.3963824 0.0003648843
#2              [0,2] 368 0.083446712  1.3978309 0.2386678860
#3              [3,4] 315 0.071428571  0.2628510 0.2440513569
#4              [5,5] 264 0.059863946  0.1450680 0.2453738527
#5              [6,7] 618 0.140136054  0.2260370 0.2530919752
#6              [8,9] 594 0.134693878 -0.2550921 0.2611195532
#7            [10,12] 855 0.193877551 -0.2533655 0.2725253823
#8            [13,16] 432 0.097959184 -0.5026168 0.2932994067
#9            [17,22] 499 0.113151927 -0.6622893 0.3326690216
#10           [23,40] 456 0.103401361 -0.7203792 0.3743651832

# Total NA's are 0.2% of total Records. Hence removing the records.
emp_df <- emp_df[-(which(is.na(emp_df$TotalWorkingYears))),]

# WOE is not monotonic. Need to perform coarse classing by combining value
#of 5,6,7 in one category for binning

# Performing coarse classing:
total_ex_comb <-
  subset(emp_df,
    (
      TotalWorkingYears == 5 |
        TotalWorkingYears == 6 | TotalWorkingYears == 7
    ))
comb_good <-
  length(total_ex_comb$Attrition[which(total_ex_comb$Attrition == 1)])
comb_bad <-
  length(total_ex_comb$Attrition[which(total_ex_comb$Attrition == 0)])
combined_woe <- computeWoE(comb_good, comb_bad)
combined_woe  # Combined WOE for 5,6 and 7 === .20

# Performing Binning for TotalWorkingYears
emp_df$TotalWorkingYears <- as.numeric(emp_df$TotalWorkingYears)
emp_df$TotalWorkExBins <- ""
emp_df$TotalWorkExBins[which(emp_df$TotalWorkingYears >= 0 &
    emp_df$TotalWorkingYears <= 2)] <- '0-2'
emp_df$TotalWorkExBins[which(emp_df$TotalWorkingYears >= 3 &
    emp_df$TotalWorkingYears <= 4)] <- '3-4'
#combining 5,6,7 since the WOE is monotonic
emp_df$TotalWorkExBins[which(emp_df$TotalWorkingYears >= 5 &
    emp_df$TotalWorkingYears <= 7)] <- '5-7'
#combining 8,9,10,11,12 as WOE is almost same
emp_df$TotalWorkExBins[which(emp_df$TotalWorkingYears >= 8 &
    emp_df$TotalWorkingYears <= 12)] <- '8-12'
emp_df$TotalWorkExBins[which(emp_df$TotalWorkingYears >= 13 &
    emp_df$TotalWorkingYears <= 16)] <- '13-16'
emp_df$TotalWorkExBins[which(emp_df$TotalWorkingYears >= 17 &
    emp_df$TotalWorkingYears <= 22)] <- '17-22'
emp_df$TotalWorkExBins[which(emp_df$TotalWorkingYears >= 23 &
    emp_df$TotalWorkingYears <= 40)] <- '23-40'

summary(as.factor(emp_df$TotalWorkExBins))
#0-2 13-16 17-22 23-40   3-4   5-7  8-12
#368   432   499   456   315   882  1449


###########################################################################################
###########################################################################################
#                                 Handling Outliers & Binning
###########################################################################################
###########################################################################################

###########################################################################################
# Variable "PercentSalaryHike" - Checking for Outliers and Binning
############################################################################################

emp_df$PercentSalaryHike <- as.numeric(emp_df$PercentSalaryHike)
quantile(emp_df$PercentSalaryHike, seq(0, 1, .01))
# no outliers detected

WOE_IV$Tables$PercentSalaryHike

#PercentSalaryHike   N    Percent         WOE          IV
#1           [11,11] 630 0.14285714 -0.14261411 0.002767159
#2           [12,12] 594 0.13469388 -0.07362124 0.003479133
#3           [13,13] 627 0.14217687  0.01071991 0.003495530
#4           [14,14] 603 0.13673469 -0.13105328 0.005741036
#5           [15,16] 537 0.12176871  0.08592029 0.006666333
#6           [17,18] 513 0.11632653  0.01850524 0.006706418
#7           [19,20] 393 0.08911565  0.10250835 0.007675652
#8           [21,25] 513 0.11632653  0.18040733 0.011696557

# WOE is not monotonic. Need to perform coarse classing by combining value
#of 11-20 in one category for binning

# Performing coarse classing for 11-20:
hike_comb_11_20 <-
  subset(emp_df, (PercentSalaryHike >= 11 &
      PercentSalaryHike <= 20))
comb_good <-
  length(hike_comb_11_20$Attrition[which(hike_comb_11_20$Attrition == 1)])
comb_bad <-
  length(hike_comb_11_20$Attrition[which(hike_comb_11_20$Attrition == 0)])
combined_woe <- computeWoE(comb_good, comb_bad)
combined_woe  # Combined WOE for 11-20 === -0.027

# Performing coarse classing for 21-30:
hike_comb_21_30 <-
  subset(emp_df, (PercentSalaryHike >= 21 & 
      PercentSalaryHike <= 30))
comb_good <-
  length(hike_comb_21_30$Attrition[which(hike_comb_21_30$Attrition == 1)])
comb_bad <-
  length(hike_comb_21_30$Attrition[which(hike_comb_21_30$Attrition == 0)])
combined_woe <- computeWoE(comb_good, comb_bad)
combined_woe  # Combined WOE for 21-30  === 0.185

# Performing Binning for PercentSalaryHike
emp_df$PercentSalaryHike <- as.numeric(emp_df$PercentSalaryHike)
emp_df$PercentHikeBins <- ""
emp_df$PercentHikeBins[which(emp_df$PercentSalaryHike >= 11 &
    emp_df$PercentSalaryHike <= 20)] <- '11-20'
emp_df$PercentHikeBins[which(emp_df$PercentSalaryHike >= 21 &
    emp_df$PercentSalaryHike <= 30)] <- '21-30'

summary(as.factor(emp_df$PercentHikeBins))
# 11-20   21-30
# 3890    511 


###########################################################################################
# Variable "Age" - Checking for Outliers and Binning
###########################################################################################

emp_df$Age <- as.numeric(emp_df$Age)
quantile(emp_df$Age, seq(0, 1, .01))
# no outliers detected

WOE_IV$Tables$Age

#Age   N    Percent        WOE        IV
#1  [18,25] 369 0.08367347  1.0638871 0.1293502
#2  [26,28] 405 0.09183673  0.3530021 0.1421973
#3  [29,30] 384 0.08707483  0.3298617 0.1527561
#4  [31,33] 564 0.12789116  0.3722848 0.1727755
#5  [34,35] 465 0.10544218 -0.3190705 0.1823895
#6  [36,37] 357 0.08095238 -0.5387768 0.2018649
#7  [38,40] 471 0.10680272 -0.7557186 0.2486710
#8  [41,44] 453 0.10272109 -0.4835407 0.2689694
#9  [45,49] 423 0.09591837 -0.6379355 0.3002024
#10 [50,60] 519 0.11768707 -0.2259957 0.3057637

# WOE is not monotonic

# Performing coarse classing for 26-33:
age_comb_26_33 <- subset(emp_df, (Age >= 26 & Age <= 33))
comb_good <-
  length(age_comb_26_33$Attrition[which(age_comb_26_33$Attrition == 1)])
comb_bad <-
  length(age_comb_26_33$Attrition[which(age_comb_26_33$Attrition == 0)])
combined_woe <- computeWoE(comb_good, comb_bad)
combined_woe  # Combined WOE for 26-33 === .35

# Performing coarse classing for 34-37:
temp_age <- subset(emp_df, (Age >= 34 & Age <= 37))
comb_good <-
  length(temp_age$Attrition[which(temp_age$Attrition == 1)])
comb_bad <-
  length(temp_age$Attrition[which(temp_age$Attrition == 0)])
combined_woe <- computeWoE(comb_good, comb_bad)
combined_woe  # Combined WOE for 34-37 === -.41

# Performing coarse classing for 38-60:
age_comb_38_60 <- subset(emp_df, (Age >= 38 & Age <= 60))
comb_good <-
  length(age_comb_38_60$Attrition[which(age_comb_38_60$Attrition == 1)])
comb_bad <-
  length(age_comb_38_60$Attrition[which(age_comb_38_60$Attrition == 0)])
combined_woe <- computeWoE(comb_good, comb_bad)
combined_woe  # Combined WOE for 38-60 === -.50

# Performing Binning for Age
emp_df$AgeBins <- ""
emp_df$AgeBins[which(emp_df$Age >= 18 &
    emp_df$Age <= 25)] <- '18-25'
emp_df$AgeBins[which(emp_df$Age >= 26 &
    emp_df$Age <= 33)] <- '26-33'
emp_df$AgeBins[which(emp_df$Age >= 34 &
    emp_df$Age <= 37)] <- '34-37'
emp_df$AgeBins[which(emp_df$Age >= 38 &
    emp_df$Age <= 60)] <- '38-60'

summary(as.factor(emp_df$AgeBins))
# 18-25 26-33 34-37 38-60
#  369  1352   819  1861


###########################################################################################
# Variable "DistanceFromHome" - Checking for Outliers and Binning
###########################################################################################

emp_df$DistanceFromHome <- as.numeric(emp_df$DistanceFromHome)
quantile(emp_df$DistanceFromHome, seq(0, 1, .01))
# no outliers detected

WOE_IV$Tables$DistanceFromHome


#DistanceFromHome   N    Percent         WOE           IV
#1            [1,1] 624 0.14149660 -0.05560273 0.0004292633
#2            [2,2] 633 0.14353741  0.13343993 0.0031019798
#3            [3,4] 444 0.10068027 -0.15051929 0.0052684365
#4            [5,6] 372 0.08435374 -0.19047013 0.0081350601
#5            [7,8] 492 0.11156463  0.02500130 0.0082053873
#6           [9,10] 513 0.11632653  0.18040733 0.0122262925
#7          [11,16] 441 0.10000000  0.11145135 0.0135157858
#8          [17,22] 390 0.08843537  0.11181074 0.0146636576
#9          [23,29] 501 0.11360544 -0.28993882 0.0233046423

# WOE is not monotonic

# Performing coarse classing for 1-2:
temp_dist <-
  subset(emp_df, (DistanceFromHome >= 1 & DistanceFromHome <= 2))
comb_good <-
  length(temp_dist$Attrition[which(temp_dist$Attrition == 1)])
comb_bad <-
  length(temp_dist$Attrition[which(temp_dist$Attrition == 0)])
combined_woe <- computeWoE(comb_good, comb_bad)
combined_woe  # Combined WOE for 1-2 === .04

# Performing coarse classing for 3-10:
temp_dist <-
  subset(emp_df, (DistanceFromHome >= 3 & DistanceFromHome <= 10))
comb_good <-
  length(temp_dist$Attrition[which(temp_dist$Attrition == 1)])
comb_bad <-
  length(temp_dist$Attrition[which(temp_dist$Attrition == 0)])
combined_woe <- computeWoE(comb_good, comb_bad)
combined_woe  # Combined WOE for 3-10 === -.01

# Performing coarse classing for 11 onwards:
temp_dist <- subset(emp_df, (DistanceFromHome >= 11))
comb_good <-
  length(temp_dist$Attrition[which(temp_dist$Attrition == 1)])
comb_bad <-
  length(temp_dist$Attrition[which(temp_dist$Attrition == 0)])
combined_woe <- computeWoE(comb_good, comb_bad)
combined_woe  # Combined WOE for 11-29 === -.02

# Performing Binning for DistanceFromHome
emp_df$DistancefromHomeBins <- ""
emp_df$DistancefromHomeBins[which(emp_df$DistanceFromHome >= 1 &
    emp_df$DistanceFromHome <= 2)] <- '1-2'
emp_df$DistancefromHomeBins[which(emp_df$DistanceFromHome >= 3 &
    emp_df$DistanceFromHome <= 10)] <- '3-10'
emp_df$DistancefromHomeBins[which(emp_df$DistanceFromHome >= 11)] <-
  '11-29'

summary(as.factor(emp_df$DistancefromHomeBins))
#1-2  11-29  3-10
#1255  1328  1818

###########################################################################################
# Variable "MonthlyIncome" - Checking for Outliers and Binning
###########################################################################################

emp_df$MonthlyIncome <- as.numeric(emp_df$MonthlyIncome)
quantile(emp_df$MonthlyIncome, seq(0, 1, .01))
# jump at 90% to 91%, replacing all greater than 1.54392188 with 1.54392188
emp_df$MonthlyIncome[which(emp_df$MonthlyIncome > 1.54392188)] <-
  1.54392188

WOE_IV$Tables$MonthlyIncome
#Scaled wOE
#     MonthlyIncome   N    Percent         WOE          IV
#1    [10090,23130] 438 0.09931973  0.11975016 0.001482632
#2    [23140,26940] 441 0.10000000 -0.03564199 0.001608138
#3    [26950,33100] 441 0.10000000  0.06400014 0.002026671
#4    [33120,42210] 441 0.10000000 -0.03564199 0.002152178
#5    [42270,49070] 441 0.10000000 -0.14261411 0.004089189
#6    [49080,57360] 441 0.10000000  0.24575657 0.010641386
#7    [57430,68770] 441 0.10000000  0.24575657 0.017193582
#8    [68830,98520] 441 0.10000000 -0.14261411 0.019130594
#9   [98540,137580] 441 0.10000000 -0.32029528 0.028314582
#10 [137700,199990] 444 0.10068027 -0.09609409 0.029214289

#MonthlyIncome   N    Percent         WOE          IV
#1  [-1.17,-0.89] 438 0.09931973  0.11975016 0.001482632
#2  [-0.89,-0.81] 441 0.10000000 -0.03564199 0.001608138
#3  [-0.81,-0.68] 441 0.10000000  0.06400014 0.002026671
#4  [-0.68,-0.48] 441 0.10000000 -0.03564199 0.002152178
#5  [-0.48,-0.34] 441 0.10000000 -0.14261411 0.004089189
#6  [-0.34,-0.16] 441 0.10000000  0.24575657 0.010641386
#7   [-0.16,0.08] 441 0.10000000  0.24575657 0.017193582
#8    [0.08,0.71] 441 0.10000000 -0.14261411 0.019130594
#9    [0.71,1.54] 441 0.10000000 -0.32029528 0.028314582
#10   [1.54,2.87] 444 0.10068027 -0.09609409 0.029214289

# WOE is not monotonic

# Performing coarse classing for 23140-68770:
temp_inc <-
  subset(emp_df, (MonthlyIncome >= -0.89 & MonthlyIncome <= 0.08))
comb_good <-
  length(temp_inc$Attrition[which(temp_inc$Attrition == 1)])
comb_bad <-
  length(temp_inc$Attrition[which(temp_inc$Attrition == 0)])
combined_woe <- computeWoE(comb_good, comb_bad)
combined_woe  # Combined WOE for 23140-68770 === .06

# Performing coarse classing for 98540-199990:
temp_inc <-
  subset(emp_df, (MonthlyIncome >= 0.71 & MonthlyIncome <= 2.87))
comb_good <-
  length(temp_inc$Attrition[which(temp_inc$Attrition == 1)])
comb_bad <-
  length(temp_inc$Attrition[which(temp_inc$Attrition == 0)])
combined_woe <- computeWoE(comb_good, comb_bad)
combined_woe  # Combined WOE for 98540-199990 === -.20

# Performing Binning for MonthlyIncome
emp_df$MonthlyIncomeBins <- ""
emp_df$MonthlyIncomeBins[which(emp_df$MonthlyIncome >= -1.17 &
    emp_df$MonthlyIncome <= -0.88)] <- '10090-23130'
emp_df$MonthlyIncomeBins[which(emp_df$MonthlyIncome >= -0.89 &
    emp_df$MonthlyIncome <= 0.71)] <- '23140-68770'
emp_df$MonthlyIncomeBins[which(emp_df$MonthlyIncome > 0.71)] <-
  '68770+ '

summary(as.factor(emp_df$MonthlyIncomeBins))
#10090-23130 23140-68770  68770+
#  438        3076        887

###########################################################################################
# Variable "YearsAtCompany" - Checking for Outliers and Binning
###########################################################################################

emp_df$YearsAtCompany <- as.numeric(emp_df$YearsAtCompany)
quantile(emp_df$YearsAtCompany, seq(0, 1, .01))

WOE_IV$Tables$YearsAtCompany


# YearsAtCompany   N    Percent         WOE         IV
#1          [0,0] 132 0.02993197  1.08952957 0.04881279
#2          [1,1] 513 0.11632653  1.00818393 0.20821653
#3          [2,2] 381 0.08639456  0.33981204 0.21936952
#4          [3,4] 714 0.16190476  0.01940218 0.21943087
#5          [5,6] 816 0.18503401 -0.43859498 0.24999523
#6          [7,8] 510 0.11564626 -0.36575766 0.26362567
#7          [9,9] 246 0.05578231 -0.57547819 0.27873827
#8        [10,14] 624 0.14149660 -0.38773657 0.29733555
#9        [15,40] 474 0.10748299 -0.68161061 0.33667579

# WOE is not monotonic

# Performing coarse classing for 'less than 10':
temp_yrs <- subset(emp_df, (YearsAtCompany < 10))
comb_good <-
  length(temp_yrs$Attrition[which(temp_yrs$Attrition == 1)])
comb_bad <-
  length(temp_yrs$Attrition[which(temp_yrs$Attrition == 0)])
combined_woe <- computeWoE(comb_good, comb_bad)
combined_woe  # Combined WOE for <10 === 0.133

# Performing coarse classing for 'greater than or equal to 10':
temp_yrs <- subset(emp_df, (YearsAtCompany >= 10))
comb_good <-
  length(temp_yrs$Attrition[which(temp_yrs$Attrition == 1)])
comb_bad <-
  length(temp_yrs$Attrition[which(temp_yrs$Attrition == 0)])
combined_woe <- computeWoE(comb_good, comb_bad)
combined_woe  # Combined WOE for >=10 === -0.503

# Performing Binning for YearsAtCompany
emp_df$YearsAtCompanyBins <- ""
emp_df$YearsAtCompanyBins[which(emp_df$YearsAtCompany < 10)] <- 'less than 10'
emp_df$YearsAtCompanyBins[which(emp_df$YearsAtCompany >= 10)] <- 'greater than or equal to 10'

summary(as.factor(emp_df$YearsAtCompanyBins))
# less than 10      greater than or equal to 10
# 3306              1095

###########################################################################################
# Variable "YearsSinceLastPromotion" - Checking for Outliers and Binning
###########################################################################################

emp_df$YearsSinceLastPromotion <-
  as.numeric(emp_df$YearsSinceLastPromotion)
quantile(emp_df$YearsSinceLastPromotion, seq(0, 1, .01))

WOE_IV$Tables$YearsSinceLastPromotion


# YearsSinceLastPromotion    N    Percent         WOE         IV
#1                   [0,0] 1743 0.39523810  0.19476763 0.01599819
#2                   [1,1] 1071 0.24285714 -0.18913412 0.02413969
#3                   [2,3]  633 0.14353741  0.06787833 0.02481634
#4                   [4,6]  414 0.09387755 -0.61421902 0.05339376
#5                  [7,15]  549 0.12448980 -0.02051141 0.05344577
# WOE is not monotonic

# Performing coarse classing for 1-3:
temp_yrs <-
  subset(emp_df,
    (YearsSinceLastPromotion >= 1 & YearsSinceLastPromotion <= 3))
comb_good <-
  length(temp_yrs$Attrition[which(temp_yrs$Attrition == 1)])
comb_bad <-
  length(temp_yrs$Attrition[which(temp_yrs$Attrition == 0)])
combined_woe <- computeWoE(comb_good, comb_bad)
combined_woe  # Combined WOE for 1-3 === -.09

# Performing coarse classing for 4-15:
temp_yrs <-
  subset(emp_df,
    (YearsSinceLastPromotion >= 4 & YearsSinceLastPromotion <= 15))
comb_good <-
  length(temp_yrs$Attrition[which(temp_yrs$Attrition == 1)])
comb_bad <-
  length(temp_yrs$Attrition[which(temp_yrs$Attrition == 0)])
combined_woe <- computeWoE(comb_good, comb_bad)
combined_woe  # Combined WOE for 4-15 === -.24

# Performing Binning for YearsSinceLastPromotion
emp_df$YearsSinceLastPromotionBins <- ""
emp_df$YearsSinceLastPromotionBins[which(emp_df$YearsSinceLastPromotion == 0)] <-
  '0'
emp_df$YearsSinceLastPromotionBins[which(emp_df$YearsSinceLastPromotion >= 1 &
    emp_df$YearsSinceLastPromotion <= 3)] <- '1-3'
emp_df$YearsSinceLastPromotionBins[which(emp_df$YearsSinceLastPromotion >= 4 &
    emp_df$YearsSinceLastPromotion <= 15)] <- '4-15'


summary(as.factor(emp_df$YearsSinceLastPromotionBins))
#   0  1-3 4-15
#1739 1700  962

###########################################################################################
# Variable "YearsWithCurrManager" - Checking for Outliers and Binning
###########################################################################################

emp_df$YearsWithCurrManager <-
  as.numeric(emp_df$YearsWithCurrManager)
quantile(emp_df$YearsWithCurrManager, seq(0, 1, .01))

WOE_IV$Tables$YearsWithCurrManager


# YearsWithCurrManager    N    Percent        WOE        IV
#1                [0,0]  789 0.17891156  0.9100131 0.1950035
#2                [1,1]  228 0.05170068 -0.1273466 0.1958062
#3                [2,2] 1032 0.23401361 -0.1224114 0.1991691
#4                [3,3]  426 0.09659864 -0.2186000 0.2034510
#5                [4,6]  474 0.10748299 -0.3408896 0.2145519
#6                [7,8]  969 0.21972789 -0.2791896 0.2301069
#7               [9,17]  492 0.11156463 -0.8898285 0.2947473

# WOE is not monotonic

# Performing coarse classing for 1-3:
temp_yrs <-
  subset(emp_df, (YearsWithCurrManager >= 1 &
      YearsWithCurrManager <= 2))
comb_good <-
  length(temp_yrs$Attrition[which(temp_yrs$Attrition == 1)])
comb_bad <-
  length(temp_yrs$Attrition[which(temp_yrs$Attrition == 0)])
combined_woe <- computeWoE(comb_good, comb_bad)
combined_woe  # Combined WOE for 1-2 === -.12

# Performing coarse classing for 4-8:
temp_yrs <-
  subset(emp_df, (YearsWithCurrManager >= 4 &
      YearsWithCurrManager <= 8))
comb_good <-
  length(temp_yrs$Attrition[which(temp_yrs$Attrition == 1)])
comb_bad <-
  length(temp_yrs$Attrition[which(temp_yrs$Attrition == 0)])
combined_woe <- computeWoE(comb_good, comb_bad)
combined_woe  # Combined WOE for 4-8 === -.30

# Performing Binning for YearsWithCurrManager
emp_df$YearsWithCurrManagerBins <- ""
emp_df$YearsWithCurrManagerBins[which(emp_df$YearsWithCurrManager == 0)] <-
  '0'
emp_df$YearsWithCurrManagerBins[which(emp_df$YearsWithCurrManager >= 1 &
    emp_df$YearsWithCurrManager <= 3)] <- '1-3'
emp_df$YearsWithCurrManagerBins[which(emp_df$YearsWithCurrManager >= 4 &
    emp_df$YearsWithCurrManager <= 8)] <- '4-8'
emp_df$YearsWithCurrManagerBins[which(emp_df$YearsWithCurrManager >= 9)] <-
  '9-15'


summary(as.factor(emp_df$YearsWithCurrManagerBins))
# 0  1-3  4-8 9-15
# 787 1683 1440  491


#removing redundant columns after binning

emp_df <-
  subset(
    emp_df,
    select = -c(
      TotalWorkingYears,
      PercentSalaryHike,
      Age,
      DistanceFromHome,
      YearsWithCurrManager,
      MonthlyIncome,
      YearsAtCompany,
      YearsSinceLastPromotion
    )
  )
###########################################################################################
#                                 Dummy value creation:
###########################################################################################
# For variables having only two levels:
#gender "Male" is 1 and PerformanceRating "3" is 1
#AvgWorkingHrs: If AvgWorkingHrs > 8.5, value is 1
emp_df$Gender <- ifelse(emp_df$Gender == "Male", 1, 0)
emp_df$PerformanceRating <-
  ifelse(emp_df$PerformanceRating == "3", 1, 0)
emp_df$AvgWorkingHrs <- ifelse(emp_df$AvgWorkingHrs > 8.5, 1, 0)
###########################################################################################
#                       Dummy variable creation
###########################################################################################
emp_chr <- emp_df[, -c(1, 5)]

# converting categorical attributes to factor
emp_fact <- data.frame(sapply(emp_chr, function(x)
  factor(x)))
str(emp_fact)

# creating dummy variables for factor attributes
dummies <- data.frame(sapply(emp_fact,
  function(x)
    data.frame(model.matrix(~ x - 1, data = emp_fact))[, -1]))

# Final dataset
emp_final <- cbind(emp_df[, c(1, 5)], dummies)
View(emp_final) #4401 obs. of  76 variables
emp_final <- emp_final[, -1]

# Coorelation Matrix
emp_final$Attrition <- as.numeric(emp_final$Attrition)
cor_df <- cor(emp_final)
cor_df

###########################################################################################
# Checking Attrition rate of Employees
###########################################################################################
Attrition_Rate <- sum(emp_final$Attrition) / nrow(emp_final)
Attrition_Rate # 0.1610998

emp_final$Attrition = as.character(emp_final$Attrition)
emp_final$Attrition = as.factor(emp_final$Attrition)
###########################################################################################
###########################################################################################
#                                 Section 4: Model Building
###########################################################################################
###########################################################################################

###########################################################################################
#                                  Logistic Regression 
###########################################################################################

# splitting the data between train and test
set.seed(100)
indices = sample(1:nrow(emp_final), 0.7 * nrow(emp_final))
train = emp_final[indices,]
test = emp_final[-(indices),]
###########################################################################################
# first model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1)
###########################################################################################
# Stepwise selection
model_2 <- stepAIC(model_1, direction = "both") #takes around 10 mins. to execute due to large number of variables (~75)
summary(model_2)
vif(model_2)

# Coefficients:
#                                     Estimate    Std.Err  zvalue Pr(>|z|)

#  (Intercept)                         -0.6499     0.4620  -1.407 0.159511 
#  EnvironmentSatisfaction.x.0.22      -0.4035     0.1446  -2.791 0.005258 ** 
#  EnvironmentSatisfaction.x0.56        1.1116     0.1478   7.523 5.35e-14 ***
#  JobSatisfaction.x0.28                0.7191     0.1494   4.813 1.49e-06 ***
#  JobSatisfaction.x0.43                1.2585     0.1740   7.234 4.69e-13 ***
#  WorkLifeBalance.x0.04                0.2935     0.1402   2.094 0.036297 *  
#  WorkLifeBalance.x0.86                1.1982     0.2244   5.339 9.37e-08 ***
#  BusinessTravel.xTravel_Frequently    1.9041     0.2855   6.668 2.59e-11 ***
#  BusinessTravel.xTravel_Rarely        1.0368     0.2672   3.880 0.000104 ***
#  Department.xResearch...Development  -1.2646     0.2547  -4.965 6.86e-07 ***
#  Department.xSales                   -1.0725     0.2547  -4.210 2.55e-05 ***
#  EducationField.xLife.Sciences        0.4029     0.1551   2.599 0.009358 ** 
#  EducationField.xMedical              0.3346     0.1680   1.992 0.046335 *  
#  JobLevel.x2                          0.3138     0.1214   2.584 0.009768 ** 
#  JobRole.xManager                    -0.5066     0.2582  -1.962 0.049765 *  
#  JobRole.xManufacturing.Director     -0.6237     0.2258  -2.762 0.005748 ** 
#  JobRole.xResearch.Director           0.8784     0.2390   3.676 0.000237 ***
#  MaritalStatus.xSingle                0.8948     0.1227   7.292 3.06e-13 ***
#  NumCompaniesWorked.x0.36             1.0466     0.1543   6.785 1.16e-11 ***
#  StockOptionLevel.x1                 -0.2623     0.1223  -2.144 0.032003 *  
#  TrainingTimesLastYear.x5            -0.4753     0.2394  -1.986 0.047044 *  
#  TrainingTimesLastYear.x6            -1.4414     0.3986  -3.616 0.000299 ***
#  JobInvolvement.x2                   -0.3680     0.1825  -2.016 0.043768 *  
#  JobInvolvement.x3                   -0.5580     0.1590  -3.510 0.000448 ***
#  PerformanceRating                   -0.2636     0.1580  -1.668 0.095241 .  
#  AvgWorkingHrs                        1.6091     0.1293  12.440  < 2e-16 ***
#  TotalWorkExBins.x13.16              -1.5906     0.3213  -4.950 7.43e-07 ***
#  TotalWorkExBins.x17.22              -1.9795     0.3331  -5.943 2.79e-09 ***
#  TotalWorkExBins.x23.40              -1.9355     0.3702  -5.229 1.71e-07 ***
#  TotalWorkExBins.x3.4                -1.1170     0.2811  -3.973 7.09e-05 ***
#  TotalWorkExBins.x5.7                -1.0601     0.2328  -4.555 5.25e-06 ***
#  TotalWorkExBins.x8.12               -1.5350     0.2509  -6.117 9.54e-10 ***
#  AgeBins.x26.33                      -0.4836     0.2097  -2.306 0.021128 *  
#  AgeBins.x34.37                      -1.0817     0.2543  -4.254 2.10e-05 ***
#  AgeBins.x38.60                      -1.0805     0.2481  -4.355 1.33e-05 ***
#  MonthlyIncomeBins.x68770..          -0.3306     0.1621  -2.039 0.041409 *  
#  YearsSinceLastPromotionBins.x4.15    1.0417     0.1848   5.638 1.72e-08 ***
#  YearsWithCurrManagerBins.x1.3       -1.0161     0.1666  -6.100 1.06e-09 ***
#  YearsWithCurrManagerBins.x4.8       -0.8233     0.2012  -4.092 4.29e-05 ***
#  YearsWithCurrManagerBins.x9.15      -1.3099     0.3115  -4.205 2.61e-05 ***
#  JobRole.xResearch.Scientist          0.2556     0.1482   1.724 0.084651 .  
  
###########################################################################################
#AgeBins.x26.33  vif 3.016080 and high p-value
model_3 <-
  glm(
    formula = Attrition ~ AgeBins.x34.37 + AgeBins.x38.60 + AvgWorkingHrs + 
                    BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + Department.xResearch...Development + Department.xSales + 
                    EducationField.xLife.Sciences + EducationField.xMedical + EnvironmentSatisfaction.x.0.22 + EnvironmentSatisfaction.x0.56 + 
                    JobInvolvement.x2 + JobInvolvement.x3 + JobLevel.x2 + JobRole.xManager + 
                    JobRole.xManufacturing.Director + JobRole.xResearch.Director + JobRole.xResearch.Scientist + JobSatisfaction.x0.28 + 
                    JobSatisfaction.x0.43 + MaritalStatus.xSingle + MonthlyIncomeBins.x68770.. + NumCompaniesWorked.x0.36 + 
                    PerformanceRating + StockOptionLevel.x1 + TotalWorkExBins.x13.16 + TotalWorkExBins.x17.22 + 
                    TotalWorkExBins.x23.40 + TotalWorkExBins.x3.4 + TotalWorkExBins.x5.7 + TotalWorkExBins.x8.12 + 
                    TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + WorkLifeBalance.x0.04 + WorkLifeBalance.x0.86 + 
                    YearsSinceLastPromotionBins.x4.15 + YearsWithCurrManagerBins.x1.3 + YearsWithCurrManagerBins.x4.8 + YearsWithCurrManagerBins.x9.15,
    family = "binomial",
    data = train
  )

summary(model_3)
vif(model_3)
###########################################################################################
#PerformanceRating                   -0.2720     0.1574  -1.728 0.084038 .  
model_4 <-
  glm(
    formula = Attrition ~ AgeBins.x34.37 + AgeBins.x38.60 + AvgWorkingHrs + 
                    BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + Department.xResearch...Development + Department.xSales + 
                    EducationField.xLife.Sciences + EducationField.xMedical + EnvironmentSatisfaction.x.0.22 + EnvironmentSatisfaction.x0.56 + 
                    JobInvolvement.x2 + JobInvolvement.x3 + JobLevel.x2 + JobRole.xManager + 
                    JobRole.xManufacturing.Director + JobRole.xResearch.Director + JobRole.xResearch.Scientist + JobSatisfaction.x0.28 + 
                    JobSatisfaction.x0.43 + MaritalStatus.xSingle + MonthlyIncomeBins.x68770.. + NumCompaniesWorked.x0.36 + 
                    StockOptionLevel.x1 + TotalWorkExBins.x13.16 + TotalWorkExBins.x17.22 + 
                    TotalWorkExBins.x23.40 + TotalWorkExBins.x3.4 + TotalWorkExBins.x5.7 + TotalWorkExBins.x8.12 + 
                    TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + WorkLifeBalance.x0.04 + WorkLifeBalance.x0.86 + 
                    YearsSinceLastPromotionBins.x4.15 + YearsWithCurrManagerBins.x1.3 + YearsWithCurrManagerBins.x4.8 + YearsWithCurrManagerBins.x9.15,
    family = "binomial",
    data = train
  )

summary(model_4)
vif(model_4)
###########################################################################################
#JobRole.xResearch.Scientist          0.2676     0.1475   1.815 0.069593 .  
model_5 <-
  glm(
    formula = Attrition ~ AgeBins.x34.37 + AgeBins.x38.60 + AvgWorkingHrs + 
                    BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + Department.xResearch...Development + Department.xSales + 
                    EducationField.xLife.Sciences + EducationField.xMedical + EnvironmentSatisfaction.x.0.22 + EnvironmentSatisfaction.x0.56 + 
                    JobInvolvement.x2 + JobInvolvement.x3 + JobLevel.x2 + JobRole.xManager + 
                    JobRole.xManufacturing.Director + JobRole.xResearch.Director + JobSatisfaction.x0.28 + 
                    JobSatisfaction.x0.43 + MaritalStatus.xSingle + MonthlyIncomeBins.x68770.. + NumCompaniesWorked.x0.36 + 
                    StockOptionLevel.x1 + TotalWorkExBins.x13.16 + TotalWorkExBins.x17.22 + 
                    TotalWorkExBins.x23.40 + TotalWorkExBins.x3.4 + TotalWorkExBins.x5.7 + TotalWorkExBins.x8.12 + 
                    TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + WorkLifeBalance.x0.04 + WorkLifeBalance.x0.86 + 
                    YearsSinceLastPromotionBins.x4.15 + YearsWithCurrManagerBins.x1.3 + YearsWithCurrManagerBins.x4.8 + YearsWithCurrManagerBins.x9.15,
    family = "binomial",
    data = train
  )

summary(model_5)
vif(model_5)
###########################################################################################
#JobInvolvement.x2                   -0.3317     0.1819  -1.823 0.068231 .  
model_6 <-
  glm(
    formula = Attrition ~ AgeBins.x34.37 + AgeBins.x38.60 + AvgWorkingHrs + 
                    BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + Department.xResearch...Development + Department.xSales + 
                    EducationField.xLife.Sciences + EducationField.xMedical + EnvironmentSatisfaction.x.0.22 + EnvironmentSatisfaction.x0.56 + 
                    JobInvolvement.x3 + JobLevel.x2 + JobRole.xManager + 
                    JobRole.xManufacturing.Director + JobRole.xResearch.Director + JobSatisfaction.x0.28 + 
                    JobSatisfaction.x0.43 + MaritalStatus.xSingle + MonthlyIncomeBins.x68770.. + NumCompaniesWorked.x0.36 + 
                    StockOptionLevel.x1 + TotalWorkExBins.x13.16 + TotalWorkExBins.x17.22 + 
                    TotalWorkExBins.x23.40 + TotalWorkExBins.x3.4 + TotalWorkExBins.x5.7 + TotalWorkExBins.x8.12 + 
                    TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + WorkLifeBalance.x0.04 + WorkLifeBalance.x0.86 + 
                    YearsSinceLastPromotionBins.x4.15 + YearsWithCurrManagerBins.x1.3 + YearsWithCurrManagerBins.x4.8 + YearsWithCurrManagerBins.x9.15,
    family = "binomial",
    data = train
  )

summary(model_6)
vif(model_6)
###########################################################################################
#MonthlyIncomeBins.x68770..          -0.3002     0.1601  -1.874 0.060870 .  
model_7 <-
  glm(
    formula = Attrition ~ AgeBins.x34.37 + AgeBins.x38.60 + AvgWorkingHrs + 
                    BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + Department.xResearch...Development + Department.xSales + 
                    EducationField.xLife.Sciences + EducationField.xMedical + EnvironmentSatisfaction.x.0.22 + EnvironmentSatisfaction.x0.56 + 
                    JobInvolvement.x3 + JobLevel.x2 + JobRole.xManager + 
                    JobRole.xManufacturing.Director + JobRole.xResearch.Director + JobSatisfaction.x0.28 + 
                    JobSatisfaction.x0.43 + MaritalStatus.xSingle + NumCompaniesWorked.x0.36 + 
                    StockOptionLevel.x1 + TotalWorkExBins.x13.16 + TotalWorkExBins.x17.22 + 
                    TotalWorkExBins.x23.40 + TotalWorkExBins.x3.4 + TotalWorkExBins.x5.7 + TotalWorkExBins.x8.12 + 
                    TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + WorkLifeBalance.x0.04 + WorkLifeBalance.x0.86 + 
                    YearsSinceLastPromotionBins.x4.15 + YearsWithCurrManagerBins.x1.3 + YearsWithCurrManagerBins.x4.8 + YearsWithCurrManagerBins.x9.15,
    family = "binomial",
    data = train
  )

summary(model_7)
vif(model_7)
###########################################################################################
# removing TrainingTimesLastYear.x5 as p value is high
model_8 <-
  glm(
    formula = Attrition ~ AgeBins.x34.37 + AgeBins.x38.60 + AvgWorkingHrs + 
                      BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + Department.xResearch...Development + Department.xSales + 
                      EducationField.xLife.Sciences + EducationField.xMedical + EnvironmentSatisfaction.x.0.22 + EnvironmentSatisfaction.x0.56 + 
                      JobInvolvement.x3 + JobLevel.x2 + JobRole.xManager + 
                      JobRole.xManufacturing.Director + JobRole.xResearch.Director + JobSatisfaction.x0.28 + 
                      JobSatisfaction.x0.43 + MaritalStatus.xSingle + NumCompaniesWorked.x0.36 + 
                      StockOptionLevel.x1 + TotalWorkExBins.x13.16 + TotalWorkExBins.x17.22 + 
                      TotalWorkExBins.x23.40 + TotalWorkExBins.x3.4 + TotalWorkExBins.x5.7 + TotalWorkExBins.x8.12 + 
                      TrainingTimesLastYear.x6 + WorkLifeBalance.x0.04 + WorkLifeBalance.x0.86 + 
                      YearsSinceLastPromotionBins.x4.15 + YearsWithCurrManagerBins.x1.3 + YearsWithCurrManagerBins.x4.8 + YearsWithCurrManagerBins.x9.15,
    family = "binomial",
    data = train
  )

summary(model_8)
vif(model_8)
###########################################################################################
#removing EducationField.xMedical as p-value is high
model_9 <-
  glm(
    formula = Attrition ~ AgeBins.x34.37 + AgeBins.x38.60 + AvgWorkingHrs + 
                    BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + Department.xResearch...Development + Department.xSales + 
                    EducationField.xLife.Sciences + EnvironmentSatisfaction.x.0.22 + EnvironmentSatisfaction.x0.56 + 
                    JobInvolvement.x3 + JobLevel.x2 + JobRole.xManager + 
                    JobRole.xManufacturing.Director + JobRole.xResearch.Director + JobSatisfaction.x0.28 + 
                    JobSatisfaction.x0.43 + MaritalStatus.xSingle + NumCompaniesWorked.x0.36 + 
                    StockOptionLevel.x1 + TotalWorkExBins.x13.16 + TotalWorkExBins.x17.22 + 
                    TotalWorkExBins.x23.40 + TotalWorkExBins.x3.4 + TotalWorkExBins.x5.7 + TotalWorkExBins.x8.12 + 
                    TrainingTimesLastYear.x6 + WorkLifeBalance.x0.04 + WorkLifeBalance.x0.86 + 
                    YearsSinceLastPromotionBins.x4.15 + YearsWithCurrManagerBins.x1.3 + YearsWithCurrManagerBins.x4.8 + YearsWithCurrManagerBins.x9.15,
    family = "binomial",
    data = train
  )

summary(model_9)
vif(model_9)
###########################################################################################
# removing EducationField.xLife.Sciences as p-value is high
model_10 <-
  glm(
    formula = Attrition ~ AgeBins.x34.37 + AgeBins.x38.60 + AvgWorkingHrs + 
                    BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + Department.xResearch...Development + Department.xSales + 
                    EnvironmentSatisfaction.x.0.22 + EnvironmentSatisfaction.x0.56 + 
                    JobInvolvement.x3 + JobLevel.x2 + JobRole.xManager + 
                    JobRole.xManufacturing.Director + JobRole.xResearch.Director + JobSatisfaction.x0.28 + 
                    JobSatisfaction.x0.43 + MaritalStatus.xSingle + NumCompaniesWorked.x0.36 + 
                    StockOptionLevel.x1 + TotalWorkExBins.x13.16 + TotalWorkExBins.x17.22 + 
                    TotalWorkExBins.x23.40 + TotalWorkExBins.x3.4 + TotalWorkExBins.x5.7 + TotalWorkExBins.x8.12 + 
                    TrainingTimesLastYear.x6 + WorkLifeBalance.x0.04 + WorkLifeBalance.x0.86 + 
                    YearsSinceLastPromotionBins.x4.15 + YearsWithCurrManagerBins.x1.3 + YearsWithCurrManagerBins.x4.8 + YearsWithCurrManagerBins.x9.15,
    family = "binomial",
    data = train
  )

summary(model_10)
###########################################################################################
#StockOptionLevel.x1                 -0.2538     0.1208  -2.101 0.035604 *  
model_11 <-
  glm(
    formula = Attrition ~ AgeBins.x34.37 + AgeBins.x38.60 + AvgWorkingHrs + 
                    BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + Department.xResearch...Development + Department.xSales + 
                    EnvironmentSatisfaction.x.0.22 + EnvironmentSatisfaction.x0.56 + 
                    JobInvolvement.x3 + JobLevel.x2 + JobRole.xManager + 
                    JobRole.xManufacturing.Director + JobRole.xResearch.Director + JobSatisfaction.x0.28 + 
                    JobSatisfaction.x0.43 + MaritalStatus.xSingle + NumCompaniesWorked.x0.36 + 
                    TotalWorkExBins.x13.16 + TotalWorkExBins.x17.22 + 
                    TotalWorkExBins.x23.40 + TotalWorkExBins.x3.4 + TotalWorkExBins.x5.7 + TotalWorkExBins.x8.12 + 
                    TrainingTimesLastYear.x6 + WorkLifeBalance.x0.04 + WorkLifeBalance.x0.86 + 
                    YearsSinceLastPromotionBins.x4.15 + YearsWithCurrManagerBins.x1.3 + YearsWithCurrManagerBins.x4.8 + YearsWithCurrManagerBins.x9.15,
    family = "binomial",
    data = train
  )

summary(model_11)
###########################################################################################
# removing WorkLifeBalance.x0.04 as p-value is high
model_12 <-
  glm(
    formula = Attrition ~ AgeBins.x34.37 + AgeBins.x38.60 + AvgWorkingHrs + 
                    BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + Department.xResearch...Development + Department.xSales + 
                    EnvironmentSatisfaction.x.0.22 + EnvironmentSatisfaction.x0.56 + 
                    JobInvolvement.x3 + JobLevel.x2 + JobRole.xManager + 
                    JobRole.xManufacturing.Director + JobRole.xResearch.Director + JobSatisfaction.x0.28 + 
                    JobSatisfaction.x0.43 + MaritalStatus.xSingle + NumCompaniesWorked.x0.36 + 
                    TotalWorkExBins.x13.16 + TotalWorkExBins.x17.22 + 
                    TotalWorkExBins.x23.40 + TotalWorkExBins.x3.4 + TotalWorkExBins.x5.7 + TotalWorkExBins.x8.12 + 
                    TrainingTimesLastYear.x6 + WorkLifeBalance.x0.86 + 
                    YearsSinceLastPromotionBins.x4.15 + YearsWithCurrManagerBins.x1.3 + YearsWithCurrManagerBins.x4.8 + YearsWithCurrManagerBins.x9.15,
    family = "binomial",
    data = train
  )

summary(model_12)
vif(model_12)
###########################################################################################
#removing JobRole.xManager as p-value is high
model_13 <-
  glm(
    formula = Attrition ~ AgeBins.x34.37 + AgeBins.x38.60 + AvgWorkingHrs + 
                    BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + Department.xResearch...Development + Department.xSales + 
                    EnvironmentSatisfaction.x.0.22 + EnvironmentSatisfaction.x0.56 + 
                    JobInvolvement.x3 + JobLevel.x2 + 
                    JobRole.xManufacturing.Director + JobRole.xResearch.Director + JobSatisfaction.x0.28 + 
                    JobSatisfaction.x0.43 + MaritalStatus.xSingle + NumCompaniesWorked.x0.36 + 
                    TotalWorkExBins.x13.16 + TotalWorkExBins.x17.22 + 
                    TotalWorkExBins.x23.40 + TotalWorkExBins.x3.4 + TotalWorkExBins.x5.7 + TotalWorkExBins.x8.12 + 
                    TrainingTimesLastYear.x6 + WorkLifeBalance.x0.86 + 
                    YearsSinceLastPromotionBins.x4.15 + YearsWithCurrManagerBins.x1.3 + YearsWithCurrManagerBins.x4.8 + YearsWithCurrManagerBins.x9.15,
    family = "binomial",
    data = train
  )

summary(model_13)
vif(model_13)
###########################################################################################
#removing JobLevel.x2 as p-value is high
model_14 <-
  glm(
    formula = Attrition ~ AgeBins.x34.37 + AgeBins.x38.60 + AvgWorkingHrs + 
                    BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + Department.xResearch...Development + Department.xSales + 
                    EnvironmentSatisfaction.x.0.22 + EnvironmentSatisfaction.x0.56 + 
                    JobInvolvement.x3 + 
                    JobRole.xManufacturing.Director + JobRole.xResearch.Director + JobSatisfaction.x0.28 + 
                    JobSatisfaction.x0.43 + MaritalStatus.xSingle + NumCompaniesWorked.x0.36 + 
                    TotalWorkExBins.x13.16 + TotalWorkExBins.x17.22 + 
                    TotalWorkExBins.x23.40 + TotalWorkExBins.x3.4 + TotalWorkExBins.x5.7 + TotalWorkExBins.x8.12 + 
                    TrainingTimesLastYear.x6 + WorkLifeBalance.x0.86 + 
                    YearsSinceLastPromotionBins.x4.15 + YearsWithCurrManagerBins.x1.3 + YearsWithCurrManagerBins.x4.8 + YearsWithCurrManagerBins.x9.15,
    family = "binomial",
    data = train
  )

summary(model_14)
vif(model_14)
###########################################################################################
# removing EnvironmentSatisfaction.x.0.22 as p-value is high
model_15 <-
  glm(
    formula = Attrition ~ AgeBins.x34.37 + AgeBins.x38.60 + AvgWorkingHrs + 
                    BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + Department.xResearch...Development + Department.xSales + 
                    EnvironmentSatisfaction.x0.56 + 
                    JobInvolvement.x3 + 
                    JobRole.xManufacturing.Director + JobRole.xResearch.Director + JobSatisfaction.x0.28 + 
                    JobSatisfaction.x0.43 + MaritalStatus.xSingle + NumCompaniesWorked.x0.36 + 
                    TotalWorkExBins.x13.16 + TotalWorkExBins.x17.22 + 
                    TotalWorkExBins.x23.40 + TotalWorkExBins.x3.4 + TotalWorkExBins.x5.7 + TotalWorkExBins.x8.12 + 
                    TrainingTimesLastYear.x6 + WorkLifeBalance.x0.86 + 
                    YearsSinceLastPromotionBins.x4.15 + YearsWithCurrManagerBins.x1.3 + YearsWithCurrManagerBins.x4.8 + YearsWithCurrManagerBins.x9.15,
    family = "binomial",
    data = train
  )

summary(model_15)
vif(model_15)
###########################################################################################
# removing JobRole.xManufacturing.Director as p-value is high
model_16 <-
  glm(
    formula = Attrition ~ AgeBins.x34.37 + AgeBins.x38.60 + AvgWorkingHrs + 
                    BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + Department.xResearch...Development + Department.xSales + 
                    EnvironmentSatisfaction.x0.56 + 
                    JobInvolvement.x3 + 
                    JobRole.xResearch.Director + JobSatisfaction.x0.28 + 
                    JobSatisfaction.x0.43 + MaritalStatus.xSingle + NumCompaniesWorked.x0.36 + 
                    TotalWorkExBins.x13.16 + TotalWorkExBins.x17.22 + 
                    TotalWorkExBins.x23.40 + TotalWorkExBins.x3.4 + TotalWorkExBins.x5.7 + TotalWorkExBins.x8.12 + 
                    TrainingTimesLastYear.x6 + WorkLifeBalance.x0.86 + 
                    YearsSinceLastPromotionBins.x4.15 + YearsWithCurrManagerBins.x1.3 + YearsWithCurrManagerBins.x4.8 + YearsWithCurrManagerBins.x9.15,
    family = "binomial",
    data = train
  )
summary(model_16)
vif(model_16)
###########################################################################################
# removing JobInvolvement.x3 as p-value is high
model_17 <-
  glm(
    formula = Attrition ~ AgeBins.x34.37 + AgeBins.x38.60 + AvgWorkingHrs + 
                    BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + Department.xResearch...Development + Department.xSales + 
                    EnvironmentSatisfaction.x0.56 + 
                    JobRole.xResearch.Director + JobSatisfaction.x0.28 + 
                    JobSatisfaction.x0.43 + MaritalStatus.xSingle + NumCompaniesWorked.x0.36 + 
                    TotalWorkExBins.x13.16 + TotalWorkExBins.x17.22 + 
                    TotalWorkExBins.x23.40 + TotalWorkExBins.x3.4 + TotalWorkExBins.x5.7 + TotalWorkExBins.x8.12 + 
                    TrainingTimesLastYear.x6 + WorkLifeBalance.x0.86 + 
                    YearsSinceLastPromotionBins.x4.15 + YearsWithCurrManagerBins.x1.3 + YearsWithCurrManagerBins.x4.8 + YearsWithCurrManagerBins.x9.15,
    family = "binomial",
    data = train
  )

summary(model_17)
vif(model_17)
###########################################################################################
# removing TrainingTimesLastYear.x6 as p-value is high
model_18 <-
  glm(
    formula = Attrition ~ AgeBins.x34.37 + AgeBins.x38.60 + AvgWorkingHrs + 
                    BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + Department.xResearch...Development + Department.xSales + 
                    EnvironmentSatisfaction.x0.56 + 
                    JobRole.xResearch.Director + JobSatisfaction.x0.28 + 
                    JobSatisfaction.x0.43 + MaritalStatus.xSingle + NumCompaniesWorked.x0.36 + 
                    TotalWorkExBins.x13.16 + TotalWorkExBins.x17.22 + 
                    TotalWorkExBins.x23.40 + TotalWorkExBins.x3.4 + TotalWorkExBins.x5.7 + TotalWorkExBins.x8.12 + 
                    WorkLifeBalance.x0.86 + 
                    YearsSinceLastPromotionBins.x4.15 + YearsWithCurrManagerBins.x1.3 + YearsWithCurrManagerBins.x4.8 + YearsWithCurrManagerBins.x9.15,
    family = "binomial",
    data = train
  )

summary(model_18)
vif(model_18)
###########################################################################################
# removing JobRole.xResearch.Director
model_19 <-
  glm(
    formula = Attrition ~ AgeBins.x34.37 + AgeBins.x38.60 + AvgWorkingHrs + 
                    BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + Department.xResearch...Development + Department.xSales + 
                    EnvironmentSatisfaction.x0.56 + 
                    JobSatisfaction.x0.28 + 
                    JobSatisfaction.x0.43 + MaritalStatus.xSingle + NumCompaniesWorked.x0.36 + 
                    TotalWorkExBins.x13.16 + TotalWorkExBins.x17.22 + 
                    TotalWorkExBins.x23.40 + TotalWorkExBins.x3.4 + TotalWorkExBins.x5.7 + TotalWorkExBins.x8.12 + 
                    WorkLifeBalance.x0.86 + 
                    YearsSinceLastPromotionBins.x4.15 + YearsWithCurrManagerBins.x1.3 + YearsWithCurrManagerBins.x4.8 + YearsWithCurrManagerBins.x9.15,
    family = "binomial",
    data = train
  )

summary(model_19)
vif(model_19)
###########################################################################################
# removing AgeBins.x34.37
model_20 <-
  glm(
    formula = Attrition ~ AgeBins.x38.60 + AvgWorkingHrs + 
                    BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + Department.xResearch...Development + Department.xSales + 
                    EnvironmentSatisfaction.x0.56 + 
                    JobSatisfaction.x0.28 + 
                    JobSatisfaction.x0.43 + MaritalStatus.xSingle + NumCompaniesWorked.x0.36 + 
                    TotalWorkExBins.x13.16 + TotalWorkExBins.x17.22 + 
                    TotalWorkExBins.x23.40 + TotalWorkExBins.x3.4 + TotalWorkExBins.x5.7 + TotalWorkExBins.x8.12 + 
                    WorkLifeBalance.x0.86 + 
                    YearsSinceLastPromotionBins.x4.15 + YearsWithCurrManagerBins.x1.3 + YearsWithCurrManagerBins.x4.8 + YearsWithCurrManagerBins.x9.15,
    family = "binomial",
    data = train
  )

summary(model_20)
vif(model_20)
###########################################################################################
# removing AgeBins.x38.60
model_21 <-
  glm(
    formula = Attrition ~ AvgWorkingHrs + 
                    BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + Department.xResearch...Development + Department.xSales + 
                    EnvironmentSatisfaction.x0.56 + 
                    JobSatisfaction.x0.28 + 
                    JobSatisfaction.x0.43 + MaritalStatus.xSingle + NumCompaniesWorked.x0.36 + 
                    TotalWorkExBins.x13.16 + TotalWorkExBins.x17.22 + 
                    TotalWorkExBins.x23.40 + TotalWorkExBins.x3.4 + TotalWorkExBins.x5.7 + TotalWorkExBins.x8.12 + 
                    WorkLifeBalance.x0.86 + 
                    YearsSinceLastPromotionBins.x4.15 + YearsWithCurrManagerBins.x1.3 + YearsWithCurrManagerBins.x4.8 + YearsWithCurrManagerBins.x9.15,
    family = "binomial",
    data = train
  )

summary(model_21)
vif(model_21)
###########################################################################################
# removing YearsWithCurrManagerBins.x4.8
model_22 <-
  glm(
    formula = Attrition ~ AvgWorkingHrs + 
                    BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + Department.xResearch...Development + Department.xSales + 
                    EnvironmentSatisfaction.x0.56 + 
                    JobSatisfaction.x0.28 + 
                    JobSatisfaction.x0.43 + MaritalStatus.xSingle + NumCompaniesWorked.x0.36 + 
                    TotalWorkExBins.x13.16 + TotalWorkExBins.x17.22 + 
                    TotalWorkExBins.x23.40 + TotalWorkExBins.x3.4 + TotalWorkExBins.x5.7 + TotalWorkExBins.x8.12 + 
                    WorkLifeBalance.x0.86 + 
                    YearsSinceLastPromotionBins.x4.15 + YearsWithCurrManagerBins.x1.3 + YearsWithCurrManagerBins.x9.15,
    family = "binomial",
    data = train
  )

summary(model_22)
vif(model_22)
###########################################################################################
# removing YearsWithCurrManagerBins.x9.15
model_23 <-
  glm(
    formula = Attrition ~ AvgWorkingHrs + 
                    BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + Department.xResearch...Development + Department.xSales + 
                    EnvironmentSatisfaction.x0.56 + 
                    JobSatisfaction.x0.28 + 
                    JobSatisfaction.x0.43 + MaritalStatus.xSingle + NumCompaniesWorked.x0.36 + 
                    TotalWorkExBins.x13.16 + TotalWorkExBins.x17.22 + 
                    TotalWorkExBins.x23.40 + TotalWorkExBins.x3.4 + TotalWorkExBins.x5.7 + TotalWorkExBins.x8.12 + 
                    WorkLifeBalance.x0.86 + 
                    YearsSinceLastPromotionBins.x4.15 + YearsWithCurrManagerBins.x1.3,
    family = "binomial",
    data = train
  )

summary(model_23)
vif(model_23)
###########################################################################################
final_model <- model_23

##### Model Inference ########
#Variable	                    Value affecting Attrition
#Average Working Hours        Employees with avg working hours greater than 8.5 hrs/day
#Business Travel	            Employees who travel Frequently/Rarely
#Department	                  1.Research & Development      2.Sales
#Environment Satisfaction	    Level 1 signifying Low Satisfaction
#Job Satisfaction	            Level 1, 2 and 3 signifying Low/Average Satisfaction
#Marital Status	              Single
#Number of Companies Worked	  Employees who have worked in more than 5 companies
#Total Work Experience	      Employees who have more than 3 years of work experience
#Work Life Balance	          Level 1 signifying Bad Work Life Balance
#Years Since Last Promotion	  Employees who haven't been promoted in last 4 years or more
#Years With Current Manager	  Employees who have been working with the same manager for 1-3 years

###########################################################################################
###########################################################################################
#                                 Section 5: Model Evaluation
###########################################################################################
###########################################################################################

# predicted probabilities of Attrition for test data
test_pred = predict(final_model, test[, -1], type = "response")
summary(test_pred)
test$prob <- test_pred

# probability greater than .5 is 1 (employee will leave)
test_pred_attrition_50 <- factor(ifelse(test_pred >= 0.50, 1, 0))
test_actual_attrition <-
  factor(ifelse(test$Attrition == 1, "Yes", "No"))


# confusion matrix
test_conf <-
  confusionMatrix(test_pred_attrition_50, test$Attrition, positive = "1")

test_conf
#Sensitivity : 0.28780
#Specificity : 0.96774
#Accuracy : 0.8622


fourfoldplot(test_conf$table, color = c("indianred2", "lightgreen"))

test_attrition_factor <- factor(ifelse(test$Attrition == 1, "Yes", "No"), levels = c("Yes", "No"))
# compute optimal probalility cutoff for better model reliability
perform_fn <- function(cutoff)
{
  pred_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"), levels = c("Yes", "No"))
  conf <-
    confusionMatrix(pred_attrition, test_attrition_factor, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc)))
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values for plotting and initiallizing a matrix of 100 X 3.
prob_seq = seq(.002, .87, length = 100)
OUT = matrix(0, 100, 3)
for (i in 1:100)
{
  OUT[i, ] = perform_fn(prob_seq[i])
}

# plot sensitivity , specificity, and accuracy with different values of probability
plot(
  prob_seq,
  OUT[, 1],
  xlab = "Cutoff",
  ylab = "Value",
  cex.lab = 1.5,
  cex.axis = 1.5,
  ylim = c(0, 1),
  type = "l",
  lwd = 2,
  axes = FALSE,
  col = 2
)
axis(1, seq(0, 1, length = 5), seq(0, 1, length = 5), cex.lab = 1.5)
axis(2, seq(0, 1, length = 5), seq(0, 1, length = 5), cex.lab = 1.5)
lines(prob_seq, OUT[, 2], col = "darkgreen", lwd = 2)
lines(prob_seq, OUT[, 3], col = 4, lwd = 2)
box()
legend(
  0,
  .50,
  col = c(2, "darkgreen", 4, "darkred"),
  lwd = c(2, 2, 2, 2),
  c("Sensitivity", "Specificity", "Accuracy")
)
grid (NULL, NULL, lty = 6, col = "cornsilk2")

# find cutoff probability for threshold value above which represents that employee will leave
# value: 0.1598182 ~ 0.16
cutoff <- prob_seq[which(abs(OUT[, 1] - OUT[, 2]) < 0.02)]
cutoff
# Let's choose a cutoff value of 0.16 for final model

test_cutoff_attrition <-
  factor(ifelse(test_pred >= 0.16, "Yes", "No"))

conf_final <-
  confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

#Accuracy : 0.734
#Sensitivity : 0.741
#Specificity : 0.732

fourfoldplot(conf_final$table, color = c("indianred2", "lightgreen"))

###########################################################################################
### KS -statistic - Test Data ######
###########################################################################################
test_cutoff_attrition <- ifelse(test_cutoff_attrition == "Yes", 1, 0)
test_actual_attrition <- ifelse(test_actual_attrition == "Yes", 1, 0)


#on testing  data
pred_object_test <-
  prediction(test_cutoff_attrition, test_actual_attrition)

performance_measures_test <-
  performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] -
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)

# 0.4744383

plot(performance_measures_test, col = "green")
abline(0, 1, col = "tomato")
grid (NULL, NULL, lty = 6, col = "cornsilk2")

###########################################################################################
# Lift & Gain Chart
###########################################################################################
test1 <- test[, c(1, grep("prob", colnames(test)))]
test1 <- test1[order(-test1[, 2], test1[, 1]), ]

gainchart <-
  data.frame(
    Decile = integer(),
    AttritionCount = integer(),
    CumulativeAttrition = integer(),
    GainPercent = integer(),
    NonAttritionCount = integer(),
    CumulativeNonAttrition = integer(),
    GainPercentNonAttrition = integer(),
    KS_stat = integer()
  )
gainchart[c(1:10), ] <- 0
strt <- 1
stp <- nrow(test1) / 10
obs <- nrow(test1) / 10
for (i in 1:10) {
  gainchart[i, 1]  <- i
  gainchart[i, 2]  <-
    sum(as.numeric(as.character(test1[c(strt:stp), 1])))
  ifelse ((i >= 2),
    gainchart[i, 3] <-
      gainchart[i - 1, 3] + gainchart[i, 2],
    gainchart[i, 3] <- gainchart[i, 2])
  gainchart[i, 5]  <- obs - gainchart[i, 2]
  ifelse ((i >= 2),
    gainchart[i, 6] <-
      gainchart[i - 1, 6] + gainchart[i, 5],
    gainchart[i, 6] <- gainchart[i, 5])
  strt <- strt + nrow(test1) / 10
  stp <- stp + nrow(test1) / 10
}
totalattri <- sum(gainchart$AttritionCount)
totalnonattri <- sum(gainchart$NonAttritionCount)
for (i in 1:10) {
  gainchart[i, 4]  <- gainchart[i, 3] / totalattri * 100
  gainchart[i, 7]  <- gainchart[i, 6] / totalnonattri * 100
  gainchart[i, 8]  <- gainchart[i, 4] - gainchart[i, 7]
}
gainchart$GainRandomModel <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
gainchart$lift <- gainchart$GainPercent / gainchart$GainRandomModel
View(gainchart)
#Decile	AttritionCount	CumulativeAttrition	GainPercent	NonAttritionCount	CumulativeNonAttrition	GainPercentNonAttrition	KS_stat	  GainRandomModel	lift
#1	    76	            76	                37.07317	  56.1	            56.1	                  5.026882	              32.046289	10	            3.707317
#2	    43	            119	                58.04878    89.1              145.2                   13.010753               45.038028	20              2.902439
#3	    28	            147	                71.70732    104.1             249.3                   22.33871                49.368607	30	            2.390244
#4	    17	            164	                80          115.1             364.4                   32.65233                47.34767	40	            2
#5	    6	              170	                82.92683    126.1             490.5                   43.951613               38.975216	50	            1.658537
#6	    6	              176	                85.85366    126.1             616.6                   55.250896               30.602762	60	            1.430894
#7	    11	            187	                91.21951    121.1             737.7                   66.102151               25.117362	70	            1.303136
#8	    6		            193	                94.14634    126.1             863.8                   77.401434               16.744908	80	            1.176829
#9	    8	              201	                98.04878    124.1             987.9                   88.521505                9.527275	90	            1.089431
#10	    4	              205	                100         128.1             1116                    100                      0        100             1


###########################################################################################
# draw lift and gain chart
# ###########################################################################################
ggplot(gainchart, aes(Decile)) + geom_line(aes(y = 1, colour = "GainRandomModel")) +
  geom_line(aes(y = lift, colour = "lift")) + geom_point(aes(y = 1)) +
  geom_point(aes(y = lift, colour = "lift"))

ggplot(gainchart, aes(Decile)) + geom_line(aes(y = GainRandomModel, colour = "GainRandomModel")) +
  geom_line(aes(y = GainPercent, colour = "GainPercent")) + geom_point(aes(y =
      GainRandomModel)) + geom_point(aes(y = GainPercent, colour = "GainPercent"))


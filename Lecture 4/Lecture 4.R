
# Package Installation ----------------------------------------------------


ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

libs <- c("ggplot2", 
          "gridExtra",
          "tidyverse",
          "git2r",
          "car",
          "minpack.lm")

ipak(libs)
setwd("~/UNI/2019:2020/MT5763/MY5763/Lecture 4")


# 1 - Reading in the data -------------------------------------------------
#first load up the loan data
loanData <- read.csv("~/UNI/2019:2020/MT5763/MY5763/Lecture 4/hmeq.csv", header = T)
head(loanData)
#now the clinical data
clinicalData <- read.csv("~/UNI/2019:2020/MT5763/MY5763/Lecture 4/clinicalData.csv", header = T)
head(clinicalData)
#now the decode table
decodeTable <- read.csv("~/UNI/2019:2020/MT5763/MY5763/Lecture 4/decodeTable.csv", header = T)
head(decodeTable)


# 2 - Row and Column selection --------------------------------------------
#filtering
#extract rows meeting some conditions
example <- filter(loanData, loanData$JOB == "Other")
head(example)

#or combinations of conditions
example <- filter(loanData, (JOB == "Other" | JOB == "Office") & REASON == "HomeImp")
head(example)
#this is all of the data where the job is either office or other and the
# reason for the loan is home improvement

#selecting data to create new data sets of combinations of rows and columns from original
#get collumns meeting conditions
example <- select(loanData, BAD, VALUE)
head(example)
#this produces a new table with just the collumns you want

#we can select a range of names to reduce typing
example <- select(loanData, VALUE:DEROG)
head(example)
#this selects the collumns between value and derog and forms them into a new table
example<- select(example, -YOJ)
head(example)
#now we have removed the row YOJ


# 3 - Altering the data contents ------------------------------------------

#RENAMING
example <- rename(loanData, Defaulted = BAD)
head(example)
#renamed the BAD column a name that makes some form of sense

#ARRANGING
example <- arrange(loanData, MORTDUE)
head(example)
#this has arranged the rows in ascending order of the MORTDUE column
example <- arrange(loanData, JOB, MORTDUE)
head(example) 
#this arranges the rows in alphabetical JOB order and within each job in 
#ascending MORTDUE order

#Descending order
example <- arrange(loanData, desc(JOB), MORTDUE)
head(example)


#MUTATE FUNCTION
example <- mutate(loanData, logLoan = log(LOAN))
head(example)
# this added in a new column which was a mutation of the previous
# this would be useful for the temperature activity as you could use this
#function to translate from celsius to fahrenheit



# 3.5 Stacking Data -------------------------------------------------------

#contrive a couple of data sets to stack
dataset1 <- loanData %>% filter(BAD == 1)
dataset2 <- loanData %>% filter(BAD == 0)
stackedData <- dataset1 %>% bind_rows(dataset2)
stackedData

# 4 - Summarising Data ----------------------------------------------------

example <- summarise(loanData, BAD_mean = mean(BAD), BAD_sd = sd(BAD))
head(example)
example

#use summarise with group_by
exampleGroup <- group_by(loanData, JOB)
head(exampleGroup)
summarise(exampleGroup, LOAN_means = mean(LOAN))
# this code has produced a table of jobs and their mean loan value

summarise(exampleGroup, DEBTINC_means = mean(DEBTINC))
summarise(exampleGroup, DEBTINC_means = mean(DEBTINC, na.rm = T))
# if the column inclused NA arguments you may need to dismiss them 
#before further analysis can be carried out




# 5 - PIPES ---------------------------------------------------------------

example <- loanData %>% select(BAD, JOB) %>% filter(!(JOB == "")) %>% arrange(JOB)
# leaving just the columns bad and job, but getting rid of any rows where the 
# value of JOB is blank

example <- loanData %>% select(BAD, JOB) %>% filter(!(JOB == "")) %>% arrange(JOB) %>% group_by(JOB) %>% summarise(proportionDefault = mean(BAD))
example
# this produces a table of all the jobs apart from the blank one and their 
# respective mean BAD value


# 6 - Joining, spreading and gathering ------------------------------------

clinical_Decoded <- clinicalData %>% rename(code = Product) %>% right_join(decodeTable, by = 'code')
dim(clinical_Decoded)
dim(clinicalData)


# 7 - Long and wide formats -----------------------------------------------
# long to wide
clinicalSpread <- clinical_Decoded %>% select(Day, Hydration, SubjectNo, Site) %>% spread(key = Day, value = Hydration, sep = "_hyd_")
head(clinicalSpread)
# wide to long
clinicalGather <- clinicalSpread %>% gather(key = Day, value = Hydration, -SubjectNo, -Site)
head(clinicalGather)

#to check it was fully reversible
check <- clinical_Decoded %>% select(Day, Hydration, SubjectNo, Site) %>% arrange(Day, SubjectNo, Site)
head(check)
head(clinicalGather)


# 8 - Joining -------------------------------------------------------------

library(readxl)
Got_names <- read_xlsx("~/UNI/2019:2020/MT5763/MY5763/Lecture 4/GoT_merging_A (1).xlsx")
Got_details <- read_xlsx("~/UNI/2019:2020/MT5763/MY5763/Lecture 4/GoT_merging_B.xlsx")
# common variable is the house
# not all the houses are represented in each dataset
# more than one entry may exist for a house
example1 <- right_join(Got_names, Got_details)
# if you do not specify "by" which column you will be joining R will state which one it used
example2 <- right_join(Got_names, Got_details, by = "House")
example3 <- right_join(Got_details, Got_names, by = "House")
example4 <- left_join(Got_names, Got_details)
example5 <- inner_join(Got_names, Got_details)
example6 <- full_join(Got_names, Got_details)
example7 <- semi_join(Got_names, Got_details)
example8 <- anti_join(Got_names, Got_details)
example9 <- anti_join(Got_details, Got_names)
#note the output of e8 and e9 are different, as which data set you state first affects the reults


# Extra - excel output ----------------------------------------------------

library(writexl)
dataforexcel <- list(GoTNames = Got_names, GoTDetails = Got_details)
write_xlsx(dataforexcel, path = "GoTExcelOutput.xlsx")


# Exercise ----------------------------------------------------------------

ex1 <- loanData %>% select(JOB, VALUE, REASON, MORTDUE, DEBTINC)
ex2 <- filter(ex1, (ex1$JOB == "Mgr" | ex1$JOB == "Office" | ex1$JOB == "Other" | ex1$JOB == "ProfExe" | ex1$JOB == "Sales" | ex1$JOB == "Self"))
ex3 <- filter(ex2, (ex2$REASON == "DebtCon" | ex2$REASON == "HomeImp"))
ex4 <- group_by(ex3, REASON)
summarynow <- summarise(ex4, DI_means = mean(DEBTINC, na.rm = T), DI_sd = sd(DEBTINC, na.rm = T), DImax = max(DEBTINC, na.rm = T), DImin = min(DEBTINC, na.rm = T))
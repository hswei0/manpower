
datapath <- "/Users/hsuwei/Desktop/manpower/dataset"
setwd(datapath)
library(readxl)
library(tidyverse)
library(magrittr)


### 要 functionize 
# Data Clean --------------------------------------------------------------




# AGE ---------------------------------------------------------------------

making_age <- function(i){
  x <- list.files(datapath) %>% # 選取資料類別
    str_subset(i)
  
  a <- read_excel(x)
  
  b <- a %>%
    slice(-c(1:10)) %>%
    select(16, 15) %>% # 選取平均月薪，用英文當標籤
    slice(c(2, 5, 10)) %>% # 選取年齡 
    spread(1, 2) 
  
  b[["year"]] <- str_split(x, "_") %>% # 取得年份
    .[[1]] %>%
    .[1]
  
  return(b)  
}


# INDUSRTY ----------------------------------------------------------------

making_indusrty <- function(i){
x <- list.files(datapath) %>%
  str_subset(i)
  
a <- read_excel(x)

b <- a %>%
  slice(-c(1:10)) %>%
  select(16, 15) %>% # 選取平均月薪，用英文當標籤
  slice(c(2, 3, 9)) %>% # 選取產業
  spread(1, 2) 

b[["year"]] <- str_split(x, "_") %>% # 取得年份
  .[[1]] %>%
  .[1]

return(b)
}

# WORKTYPE ----------------------------------------------------------------

making_worktype <- function(i){
x <- list.files(datapath) %>%
  str_subset(i)

a <- read_excel(x)

b <- a %>%
  slice(-c(1:10)) %>%
  select(16, 15) %>% # 選取平均月薪，用英文當標籤
  slice(c(11, 12, 14, 15, 25,26)) %>% # 選取產業
  spread(1,2) %>%
  select(5, 2, 1, 4, 6, 3) # 調整倆倆相對順序

b[["year"]] <- str_split(x, "_") %>% # 取得年份
  .[[1]] %>%
  .[1]

return(b)
}

# Bind Data
age.df <- making_age("Age")
industry.df <- making_indusrty("Industry")
worktype.df <- making_worktype("Worktype")


all <- full_join(age.df, industry.df, by = "year") %>%
  full_join(worktype.df, by = "year") %>%
  select(year, everything())


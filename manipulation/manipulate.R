
datapath <- "/Users/hsuwei/Desktop/manpower/dataset"
setwd(datapath)
library(readxl)
library(tidyverse)
library(magrittr)


# Data Clean --------------------------------------------------------------


# AGE ---------------------------------------------------------------------

making_age <- function(i){
  j <- list.files(datapath) %>% # 選取資料類別
    str_subset(i)
  
  eachage.fn <- function(x){
    year <- str_split(x, "_") %>% # 取得年份
      .[[1]] %>%
      .[1]
    
    a <- read_excel(x)
    
    if (year %in% c("2015", "2016")) {
      b <- a %>%
        slice(-c(1:10)) %>%
        select(17, 16) %>% # 選取平均月薪，用英文當標籤
        slice(c(2, 5, 10, 45)) %>% # 選取年齡 
        spread(1, 2) 
    } else {
      b <- a %>%
        slice(-c(1:10)) %>%
        select(16, 15) %>% # 選取平均月薪，用英文當標籤
        slice(c(2, 5, 10, 45)) %>% # 選取年齡 
        spread(1, 2)
    }
    b[["year"]] <- year
    names(b) <- paste0("x", 1:length(b))
    return(b)
  }
  c <- map_df(j, eachage.fn)
  return(c)
  }


# INDUSRTY ----------------------------------------------------------------

making_indusrty <- function(i){
  j <- list.files(datapath) %>% # 選取資料類別
    str_subset(i)
  
  eachindustry.fn <- function(x){
    year <- str_split(x, "_") %>% # 取得年份
      .[[1]] %>%
      .[1]
    
    a <- read_excel(x)
    
    if (year %in% c("2015", "2016")) {
      b <- a %>%
        slice(-c(1:10)) %>%
        select(17, 16) %>% # 選取平均月薪，用英文當標籤
        slice(c(2, 3, 9)) %>% # 選取產業
        spread(1, 2) 
    } else {
      b <- a %>%
        slice(-c(1:10)) %>%
        select(16, 15) %>% # 選取平均月薪，用英文當標籤
        slice(c(2, 3, 9)) %>% # 選取產業
        spread(1, 2) 
    }
    b[["year"]] <- year
    names(b) <- paste0("x", 1:length(b))
    return(b)
  }
  c <- map_df(j, eachindustry.fn)
  return(c)
}

# WORKTYPE ----------------------------------------------------------------

making_worktype <- function(i){
  j <- list.files(datapath) %>% # 選取資料類別
    str_subset(i)
  
  eachworktype.fn <- function(x){
    year <- str_split(x, "_") %>% # 取得年份
      .[[1]] %>%
      .[1]
    
    a <- read_excel(x)
    
    if (year %in% c("2015", "2016")) {
      b <- a %>%
        slice(-c(1:10)) %>%
        select(17, 16) %>% # 選取平均月薪，用英文當標籤
        slice(c(11, 12, 14, 15, 25,26)) %>% # 選取產業
        spread(1,2) %>%
        select(5, 2, 1, 4, 6, 3) # 調整倆倆相對順序
    } else {
      b <- a %>%
        slice(-c(1:10)) %>%
        select(16, 15) %>% # 選取平均月薪，用英文當標籤
        slice(c(11, 12, 14, 15, 25,26)) %>% # 選取產業
        spread(1,2) %>%
        select(5, 2, 1, 4, 6, 3) # 調整倆倆相對順序
    }
    b[["year"]] <- year
    names(b) <- paste0("x", 1:length(b))
    return(b)
  }
    c <- map_df(j, eachworktype.fn)
    return(c)
  }

# Bind Data
age.df <- making_age("Age")
industry.df <- making_indusrty("Industry")
worktype.df <- making_worktype("Worktype")


# Naming ------------------------------------------------------------------



naming.fn <- function(i){
  
  agelab.en <- c("15-24 years", "25-44 years", "45-64 years", "65 years & over", "year")
  agelab.chi <- c("１５-２４歲", "２５-４４歲", "４５-６４歲", "６５歲及以上", "年份")
  
  industrylab.en <- c(
    "Agriculture, Forestry, Fishing & Animal Husbandry", 
    "Goods-Producing Industries", 
    "Services-Producing Industries", "year")
  industrylab.chi <- c("農、林、漁、牧業", "工業", "服務業", "年份")
  
  worktypelab.en <- c(
    "Part-time, temporary or dispatched workers", 
    "Non part-time, temporary or dispatched workers",
    "Full-time workers",
    "Part-time workers",
    "Temporary or dispatched workers",
    "Non temporary or dispatched workers", "year")
  worktypelab.chi <- c(
    "部分時間、臨時性或人力派遣工作", "非部分時間、臨時性或人力派遣工作",
    "全日時間工作", "部分時間工作", 
    "臨時性或人力派遣工作", "非臨時性或人力派遣工作", "年份") 
  
  if (i == "English"){
    names(age.df) <- agelab.en
    names(industry.df) <- industrylab.en
    names(worktype.df) <- worktypelab.en
    b <- bind_cols(age.df, industry.df, worktype.df) %>%
      select(year, everything(), -c(year1, year2)) 
  }else {
    names(age.df) <- agelab.chi
    names(industry.df) <- industrylab.chi
    names(worktype.df) <- worktypelab.chi
    b <- bind_cols(age.df, industry.df, worktype.df)%>%
      select(年份, everything(), -c(年份1, 年份2)) 
  }
  return(b)
}

alldf.en <- naming.fn("English")
alldf.chi <- naming.fn("Chinese")

save(alldf.en, alldf.chi, file = "/Users/hsuwei/Desktop/manpower/result/manpower.RData")
write_excel_csv(alldf.chi, "/Users/hsuwei/Desktop/manpower/result/manpower.xlsx")


# 繪圖 ----------------------------------------------------------------------

library(tidyverse)
library(magrittr)
library(gridExtra)
library(ggthemes)

## age specific
alldf.en %>%
  select(1:5) %>%
  gather("type", "income", -year) %>%
  mutate_at(c("year", "income"), as.numeric) %>%
ggplot(data = ., mapping = aes(x = year, y = income, color = type)) +
  geom_line(size=1) + theme_solarized() + scale_colour_solarized("blue") +
  ggtitle("Age-Specific Income")
  

## industry specific
alldf.en %>%
  select(1, 6:8) %>%
  gather("type", "income", -year) %>%
  mutate_at(c("year", "income"), as.numeric) %>%
  ggplot(data = ., mapping = aes(x = year, y = income, color = type)) + 
  geom_line(size = 1) + 
  theme_solarized() + scale_colour_solarized("blue") +
  ggtitle("Industry-specific Income")

## worktype specific
par(mfrow=c(1,3))

alldf.en %>%
  select(1, 9:10) %>%
  gather("type", "income", -year) %>%
  mutate_at(c("year", "income"), as.numeric) %>%
  ggplot(data = ., mapping = aes(x = year, y = income, color = type)) + 
  geom_line(size = 1) + 
  theme_solarized() + scale_colour_solarized("blue") +
  ggtitle("Worktype-Specific Income1")


alldf.en %>%
  select(1, 11:12) %>%
  gather("type", "income", -year) %>%
  mutate_at(c("year", "income"), as.numeric) %>%
  ggplot(data = ., mapping = aes(x = year, y = income, color = type)) + 
  geom_line(size = 1) + 
  theme_solarized() + scale_colour_solarized("blue") +
  ggtitle("Worktype-Specific Income2")

alldf.en %>%
  select(1, 13:14) %>%
  gather("type", "income", -year) %>%
  mutate_at(c("year", "income"), as.numeric) %>%
  ggplot(data = ., mapping = aes(x = year, y = income, color = type)) + 
  geom_line(size = 1) + 
  theme_solarized() + scale_colour_solarized("blue") +
  ggtitle("Worktype-Specific Income3")

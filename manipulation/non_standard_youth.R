setwd("/Users/hsuwei/Desktop/man")


read_excel("https://www.dgbas.gov.tw/public/data/dgbas04/bc4/mpwutility/106/mtable13.xls")



temp = tempfile(fileext = ".xls")
dataURL <- "https://www.dgbas.gov.tw/public/data/dgbas04/bc4/mpwutility/101/mtable18.xls"
download.file(dataURL, destfile=temp, mode='wb')

test <- readxl::read_excel(temp, range = "B9:I54", col_names = F) %>%
  slice(c(2, 3, 6, 11, 16)) %>%
  mutate_at(vars(X__2:X__8), as.numeric)
  
head(test)

year <- "101"
mtable <- "mtable18"


# 年齡別非典型比例 --------------------------------------------------------------------

library(tidyverse)
setwd("/Users/hsuwei/Desktop/man")


download_fn <- function(year, mtable) { ## 資料年份(民國), 資料表格號碼
  
  temp = tempfile(fileext = ".xls")
  dataURL <- str_c("https://www.dgbas.gov.tw/public/data/dgbas04/bc4/mpwutility", 
                   year, str_c(mtable, ".xls"), sep = "/")
  download.file(dataURL, destfile = temp, mode = 'wb')
  
  if (year < 104) {
    df1 <- readxl::read_excel(temp, col_names = F, range = "J9:Q54", col_types = c("text")) 
    
    df2 <- df1 %>%
      slice(c(2, 3, 6, 11, 16)) %>%
      mutate_at(vars(X__2:X__8), as.numeric) %>%
      mutate(X__1 = c("Total", "15-24 years", "25-44 years", "45-64 years", "65 years & over"))
    
    names(df2) <- df1[1, ]
  } else {
    df1 <- readxl::read_excel(temp, col_names = F, range = "K9:R54")
    
    df2 <- df1 %>%
      slice(c(2, 3, 6, 11, 16)) %>%
      mutate_at(vars(X__2:X__8), as.numeric) %>%
      mutate(X__1 = c("Total", "15-24 years", "25-44 years", "45-64 years", "65 years & over"))
    
    names(df2) <- df1[1, ]
  }
  
  return(df2)
}

y_range <- 2011 : 2017 - 1911
mtable <- ifelse(y_range < 104, "mtable18", "mtable17")
## 表 17 就業者主要工作之類別－按年齡分，有年份差異

all.lt <- map2(y_range, mtable, download_fn)
names(all.lt) <- str_c(2011:2017, "major_job")

aa <- all.lt$`2015major_job`

pickvar <- function(df, col_n) {
  out.df <- df %>%
    select(col_n) %>%
    slice(1:2) ## keep total, 15-24
  
  return(out.df)
 
}

PTD.df <- map_dfc(all.lt, function(x) {
  xx <- select(x, 3) #%>%
   # slice(1:2) ## keep total, 15-24
  xx
})

colnames(PTD.df) <- 2011:2017
rownames(PTD.df) <- c("Total", "15-24 years")

PTD.df %<>%
  mutate(age = c("Total", "15-24 years", "25-44 years", "45-64 years", "65 years & over")) %>%
  select(age, everything())
write_csv(PTD.df, path = str_c(getwd(), "result", "allPTD.csv", sep = "/"))


P.df <- map_dfc(all.lt, function(x) {
  xx <- select(x, 6) %>% #part time
    slice(1:2) ## keep total, 15-24
  xx
})

colnames(P.df) <- 2011:2017
rownames(P.df) <- c("Total", "15-24 years")

TD.df <- map_dfc(all.lt, function(x) {
  xx <- select(x, 7) %>% #part time
    slice(1:2) ## keep total, 15-24
  xx
})

colnames(TD.df) <- 2011:2017
rownames(TD.df) <- c("Total", "15-24 years")

out1 <- bind_rows(PTD.df, P.df) %>%
  bind_rows(TD.df) %>%
  mutate(rowname = rep(c("Total", "15-24 years"), 3), 
         cate = c(names(all.lt[["2017major_job"]])[3], "", 
                  names(all.lt[["2017major_job"]])[6], "", 
                  names(all.lt[["2017major_job"]])[7], "")) %>%
  select(cate, rowname, everything())

varnames <- names(all.lt$`2017major_job`)
write_csv(out1, path = str_c(getwd(), "result", "nonstandard.csv", sep = "/"))

# 年齡別佔非典型比例 --------------------------------------------------------------------

library(tidyverse)
setwd("/Users/hsuwei/Desktop/man")


download_fn <- function(year, mtable) { ## 資料年份(民國), 資料表格號碼
  
  temp = tempfile(fileext = ".xls")
  dataURL <- str_c("https://www.dgbas.gov.tw/public/data/dgbas04/bc4/mpwutility", 
                   year, str_c(mtable, ".xls"), sep = "/")
  download.file(dataURL, destfile = temp, mode = 'wb')
  
  if (year < 104) {
    df1 <- readxl::read_excel(temp, col_names = F, range = "B9:I54", col_types = c("text")) 
    
    df2 <- df1 %>%
      slice(c(2, 3, 6, 11, 16)) %>%
      mutate_at(vars(X__2:X__8), as.numeric) %>%
      mutate(X__1 = c("Total", "15-24 years", "25-44 years", "45-64 years", "65 years & over"))
    
    #names(df2) <- df1[1, ]
  } else {
    df1 <- readxl::read_excel(temp, col_names = F, range = "A9:H54")
    
    df2 <- df1 %>%
      slice(c(2, 3, 6, 11, 16)) %>%
      mutate_at(vars(X__2:X__8), as.numeric) %>%
      mutate(X__1 = c("Total", "15-24 years", "25-44 years", "45-64 years", "65 years & over"))
    
    #names(df2) <- df1[1, ]
  }
  
  return(df2)
}

y_range <- 2011 : 2017 - 1911
mtable <- ifelse(y_range < 104, "mtable18", "mtable17")
## 表 17 就業者主要工作之類別－按年齡分，有年份差異

all.lt <- map2(y_range, mtable, download_fn)
names(all.lt) <- str_c(2011:2017, "major_job")

x <- all.lt[[2]]

youth_nonstandard <- map_dfr(all.lt[1: 7], function(x){
names(x) <- varnames
x2 <- x %>%
  select(1, 3, 6, 7) %>%
  slice(-1) %>%
  gather(key = "type", value = "man", -1)

x2 %>% 
  group_by(type) %>%
  mutate(prop = round(man / sum(man), 2)) %>%
  select(-man) %>%
  spread(key = "type", value = prop) %>%
  slice(1)
}) %>%
  mutate("　Item" = 2011: 2017)

write_csv(youth_nonstandard, path = str_c(getwd(), "result", "y_nonstandard.csv", sep = "/"))

allage_nonstandard <- map_dfr(all.lt[1: 7], function(x){
  names(x) <- varnames
  x2 <- x %>%
    select(1, 3, 6, 7) %>%
    slice(-1) %>%
    gather(key = "type", value = "man", -1)
  
  x2 %>% 
    group_by(type) %>%
    mutate(prop = round(man / sum(man), 2)) %>%
    select(-man) %>%
    slice(1:4) %>%
    filter(type == "Part-time,\ntemporary or\ndispatched workers") %>%
    spread(key = "　Item", value = prop) 
}) %>%
  ungroup(type) %>%
  mutate(type = 2011 : 2017)
write_csv(allage_nonstandard, path = str_c(getwd(), "result", "all_nonstandard.csv", sep = "/"))


out2 <- xx%>%
  mutate(year = 2011:2017) %>%
  select(year, everything())

write_csv(out2, path = str_c(getwd(), "result", "nonstandard2.csv", sep = "/"))

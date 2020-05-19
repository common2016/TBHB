## code to prepare `profit` dataset goes here
rm(list = ls())
library(tidyverse)
profit <- openxlsx::read.xlsx('./data-raw/profit.xlsx',1,detectDates = T)
profit <- profit[profit$指标名称 >= as.Date('2012-2-28'),]

names(profit) <- c('date','cumValue','cumTB','value','TB')
profit$yr <- format(profit$date,'%Y') %>% as.numeric()
profit$mon <- format(profit$date,'%m') %>% as.numeric()

profit[profit$mon == 2,'value'] <- profit[profit$mon == 2,'cumValue']
# usethis::use_data(profit, overwrite = T)

devtools::load_all()
profit$HB <- ConvertRatio(data = profit,to = 'HB') %>% .[,'valueHB']
profit$DJB <- ConvertRatio(data = profit,from = 'HB', to = 'DJB', value.name = 'HB') %>% .[,'DJB']
profit$DJB <- ConvertRatio(data = profit,from = 'HB', to = 'TB', value.name = 'HB') %>% .[,'DJB']
profit <- na.omit(profit)

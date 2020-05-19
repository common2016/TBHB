# TBHB

## 功能

该包实现将原始水平数据如工业总产值转换成同比、环比和定基比的功能。以及将环比转成定基比和同比。

## 安装

```
devtools::install_github('common2016/TBHB')
```

## 使用

```R
# 转成同比
ConvertRatio(year = 'yr',mon = 'mon',value.name = 'cumValue',data = profit,to = 'TB')
# 转成环比
ConvertRatio(year = 'yr',mon = 'mon',value.name = 'cumValue',data = profit,to = 'HB')
# 转成定基比
ConvertRatio(year = 'yr',mon = 'mon',value.name = 'cumValue',data = profit,to = 'DJB',
            BaseTime = c(2012,5))
# 环比转成定基比或同比
profit$HB <- ConvertRatio(data = profit,to = 'HB') %>% .[,'valueHB']
profit$DJB <- ConvertRatio(data = profit,from = 'HB', to = 'DJB', value.name = 'HB')
profit$TB <- ConvertRatio(data = profit,from = 'HB', to = 'TB', value.name = 'HB')
```


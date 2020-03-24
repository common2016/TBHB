# TBHB

## 功能

该包实现将原始水平数据如工业总产值转换成同比、环比和定基比的功能。

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
```


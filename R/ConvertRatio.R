#' @title Convert Ratios
#' @description  Translation among year-on-year ratios, chain relative ratios and
#'   fixed base ratios.
#' @param year a column name which represents a year.
#' @param mon a column name which represents a month.
#' @param value.name a column name, and its column reprensents level data
#'   which are translated to year-on-year ratio, chain relative ratio or
#'   fixed base ratio.
#' @param data a data.frame which include the year, mon and value.name columns at least.
#' @param from a charactor which represents the type of original data, it only includes
#'   'level'and 'HB'. 'HB' data look likes such as 0.16, not 1.16.
#' @param to a charactor which represents a translated type. 'TB' means
#'   a year-on-year ratio, 'HB' means a chain relative ratio, and 'DJB' means
#'   a fixed base ratio.
#' @param BaseTime a numeric vector whose length is 2. When translated 'DJB',
#'   it reprensents fixed base period. The 1st of \code{BaseTime} element means year,
#'   and the 2nd element of \code{BaseTime} means month.
#' @details
#'  \itemize{
#'    \item year-on-year ratio = \eqn{(V_{yr,mon}-V_{yr-1,mon})/V_{yr-1,mon}},
#'      where \eqn{V_{yr,mon}} denotes a level value in \code{mon} month \code{yr} year.
#'    \item chain relative ratio = \eqn{(V_t-V_{t-1})/V_{t-1}},
#'     where \eqn{V_t} denotes a level value at \eqn{t} period.
#'    \item fixed base ratio = \eqn{(V_t-V_{base})/V_{base}},
#'     where \eqn{V_{base}} denotes a level value at fixed base period.
#'  }
#'
#' @return Return a data frame the 1st column of which is year, the 2nd column of which
#'   is month, and the 3rd coloumn of which is the translated ratio.
#' @examples
#' data("profit")
#' profit[1:24,c('yr','mon','cumValue')]
#' ##       yr mon cumValue
#' ##    yr mon cumValue
#' ##  2012   2  6060.06
#' ##  2012   3 10449.14
#' ##  2012   4 14525.20
#' ##  2012   5 18434.13
#' ##  2012   6 23116.61
#' ##  2012   7 26784.50
#' ##  2012   8 30596.66
#' ##  2012   9 35239.53
#' ##  2012  10 40240.05
#' ##  2012  11 46625.26
#' ##  2012  12 55577.70
#' ##  2013   2  7091.53
#' ##  2013   3 11740.08
#' ##  2013   4 16106.91
#' ##  2013   5 20812.45
#' ##  2013   6 25836.64
#' ##  2013   7 30032.16
#' ##  2013   8 34863.90
#' ##  2013   9 40452.81
#' ##  2013  10 46263.22
#' ##  2013  11 53338.01
#' ##  2013  12 62831.02
#' # converte the cumValue coloumn to a year-on-year ratio,
#' ConvertRatio(year = 'yr',mon = 'mon',value.name = 'cumValue',data = profit,to = 'TB')
#' #converte the cumValue coloumn to a chain relative ratio ratio,
#' ConvertRatio(year = 'yr',mon = 'mon',value.name = 'cumValue',data = profit,to = 'HB')
#' # converte the cumValue coloumn to a fixed base ratio,
#' ConvertRatio(year = 'yr',mon = 'mon',value.name = 'cumValue',data = profit,to = 'DJB',
#'             BaseTime = c(2012,5))
#' @importFrom magrittr `%>%`
#' @export

ConvertRatio <- function(year = 'yr', mon = 'mon', value.name = 'value',
                         data,from = 'level', to = 'DJB', BaseTime = c(2013,6)){
  med <- data[,c(year,mon,value.name)]
  if (from %in% 'level'){
    if (to %in% 'TB'){
      eval(parse(text = paste('formula <-', year,'~', mon,sep = '')))
      med <- reshape2::dcast(med, formula = formula, value.var = value.name)
      Tradata <- med
      Tradata[1,-1] <- NA
      for (i in 2:nrow(med)) {
        Tradata[i,-1] <- (med[i,-1] - med[i-1,-1])/med[i-1,-1]
      }
      Tradata <- reshape2::melt(Tradata, id.vars = year)
      # 重新排序
      eval(parse(text = paste('Tradata <- dplyr::arrange(Tradata,',year,',variable)',sep = '')))
      names(Tradata) <- c(year,mon,paste(value.name,to,sep = ''))
    }else if (to %in% 'HB'){
      med[,paste(value.name,to,sep = '')] <-
        (med[,value.name] - dplyr::lag(med[,value.name]))/dplyr::lag(med[,value.name])
      Tradata <- med[,c(year,mon,paste(value.name,to,sep = ''))]
    }else if (to %in% 'DJB'){
      med[,paste(value.name,to,sep = '')] <-
        (med[,value.name] - med[med[,year] == BaseTime[1] & med[,mon] == BaseTime[2],value.name])/
        med[med[,year] == BaseTime[1] & med[,mon] == BaseTime[2],value.name]
      Tradata <- med[,c(year,mon,paste(value.name,to,sep = ''))]
    }
  }else if (from %in% 'HB'){
    # 先转成DJB
    med$HB <- 1 + med$HB
    med[,'DJB'] <- NA
    med[1,'DJB'] <- 1
    for (i in 2:nrow(med)) {
      med[i,'DJB'] <- med[i,'HB'] * med[i-1,'DJB']
    }
    if (to %in% 'DJB'){
      med$DJB <- med$DJB/med$DJB[med$yr == BaseTime[1] & med$mon == BaseTime[2]]
      Tradata <- med[,c('yr','mon','DJB')]
    } else if (to %in% 'TB'){
      med <- reshape2::dcast(med,yr ~ mon, value.var = 'DJB')
      for (i in seq(nrow(med),2,-1)) {
        med[i,-1] <- (med[i,-1] - med[i-1,-1])/med[i-1,-1]
      }
      med[1,-1] <- NA
      med <- reshape2::melt(med, id.vars = 'yr')
      Tradata <- dplyr::rename(med, mon = variable, TB = value) %>%
        dplyr::arrange(yr,mon)
    }
  }
  return(Tradata)
}

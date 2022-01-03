library('dplyr')
library('R.utils')
library('stats')

#Extracts monthly data to a 1 dimensional array
#Parameter "monthly" is a nx12 array where each row
#is a different year and each column is a different month
getLinearMonthly<-function(monthly){
  count<-1;
  linearMonthly<-array()
  for(row in 1:dim(monthly)[1]){
    for(col in 1:dim(monthly)[2]){
      linearMonthly[count]<-as.numeric(monthly[row,col])
      count<-count+1
    }
  }
  linearMonthly
}

#takes mog data as input, and outputs a 1d array of monthly
#data, filled with NA for missing months. Assumes there 
#are less than or equal to 1932 rows.
clean_mog<-function(mog){
  months = mog[,2]
  wl = mog[,6]
  out = rep(NA, 1932)
  #for number of years
  for (i in 1:161) {
    for (j in 1:12) {
      day = (i-1)*12 + j
      if (j != months[day]) {
        #months/wl have skipped a month
        months = insert(months, day, j)
        wl = insert(wl, day, NA)
      }
      out[day] = wl[day]
    }
  }
  out
}

#appends erie monthly mean lakewide water levels and 
#erie monthly master gauge water levels to WL in that order
addErie<-function(WL){
  #mean monthly erie lakewide average data
  #1/1/1918 thru 12/31/2020
  erie.ave <- read.csv("data/LakeErie_MonthlyMeanWaterLevels_1918to2021.csv")
  erie.ave <- erie.ave[-nrow(erie.ave),-c(1)]
  erie.ave <- getLinearMonthly(erie.ave)

  #mean monthly erie master gauge data (Cleveland)
  #1/1/1860 thru 12/31/2020
  erie.mog <- read.csv(paste("https://api.tidesandcurrents.noaa.gov/api/prod/datagetter?",
                          "product=monthly_mean",
                          "&application=NOS.COOPS.TAC.WL",
                          "&begin_date=18600101",
                          "&end_date=20201201",
                          "&datum=IGLD",
                          "&station=9063063",
                          "&time_zone=lst_ldt",
                          "&units=metric",
                          "&format=csv", sep=""))
  erie.mog = clean_mog(erie.mog) # 04/1938 is missing

  #fill 1860-1918 data with NA for lakewide average
  diff = length(WL) - length(erie.ave)
  fill = rep(NA, diff)
  erie.ave = append(fill, erie.ave)
  WL = cbind(WL, erie.ave, erie.mog)
  WL
}

#assumes erie data was the last set added to WL
#NOTE: cleveland is not current mog, it switched over after 1991
plotErie<-function(WL){
  erie_out = 'Erie_plot.pdf'
  if (file.exists(erie_out)) {
    file.remove(erie_out)
  }
  pdf(file = erie_out, width = 7, height = 10, onefile = FALSE)
  plot.new()
  par(mfrow = c(2,1))

  plot(WL[,1], WL[,ncol(WL)-1], type = 'l', col = 'blue',
    xlab='Days since 1/1/1860', ylab='Water Level (m)')
  lines(WL[,1], WL[,ncol(WL)], col = 'red')
  legend(x = 'topleft', legend = c('Average', 'Master Gauge'), lty = c(1,1), col = c('blue', 'red'))
  abline(v=696, col="black", lty=2, lwd=2)
  title('Lake Erie water levels over time')

  plot(WL[,1], WL[,ncol(WL)] - WL[,ncol(WL)-1], col = 'black',
    xlab='Days since 1/1/1860', ylab='mog - average (m)')
  title('Difference between Master gauge and lakewide average')
  dev.off()
}

#separates data by month and creates 12x2 plots- water level
#and difference for each month.
plotErieMonthly<-function(WL){
  erie_out = 'monthly_plots/Erie_monthly_plot.pdf' #varies for each lake
  if (file.exists(erie_out)) {
    file.remove(erie_out)
  }
  pdf(file = erie_out, width=12, height=8, onefile = FALSE)
  plot.new()
  par(mfrow=c(3,4), mar=c(0,0,0,0), oma=c(7,7,7,7))
  mon_list = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

  for (i in 1:12) {
    mons = seq(i, nrow(WL), 12)
    plot(WL[mons,1]/12+1860, WL[mons,ncol(WL)] - WL[mons,ncol(WL)-1], col = 'black', ylim=c(-0.1, 0.1), xaxt='n', yaxt='n', ann=FALSE) #ylim varies for each lake
    if (i == 1 | i == 9) {
      axis(2, at=seq(-0.1, 0.1, 0.05)) #varies for each lake
    }
    if (i == 8) {
      axis(4, at=seq(-0.1, 0.1, 0.05)) #varies for each lake
    }
    if (i == 9 | i == 11) {
      axis(1, at=seq(1860, 2020, 20))
    }
    if (i == 2 | i == 4) {
      axis(3, at=seq(1860, 2020, 20))
    }
    legend('topleft', legend=mon_list[i], cex=1.5)
    abline(h=0, col='red', lty=2, lwd=1)
  }

  mtext('Lake Erie Master Gauge (Cleveland) vs lakewide average for each month', side=3, line=3, cex=2, outer=TRUE) #varies for each lake
  mtext('Year', side=1, line=5, cex=1.7, outer=TRUE)
  mtext('MOG - Lakewide Average (m)', side=2, line=4, cex=1.7, outer=TRUE)

  dev.off()
  copy = 'erie_monthly_plots/Cleveland_monthly_plot.pdf'
  if (file.exists(copy)) {
    file.remove(copy)
  }
  file.copy(erie_out, copy)
}

#takes average across each month to get yearly average then plots it
plotErieYearly<-function(WL, predict=TRUE){
  erie_out = 'extrapolated_yearly_water_levels/Erie_Yearly_plot.pdf'
  if (file.exists(erie_out)) {
    file.remove(erie_out)
  }
  pdf(file = erie_out, width = 7, height = 10, onefile = FALSE)
  plot.new()
  par(mfrow = c(2,1))

  WL_ave = data.frame()
  for (i in seq(1, nrow(WL), 12)) {
    sum.ave = 0
    sum.mog = 0
    for (j in 0:11) {
      sum.ave = sum.ave + WL[i+j,ncol(WL)-1]
      sum.mog = sum.mog + WL[i+j,ncol(WL)]
    }
    WL_ave = rbind(WL_ave, c(sum.ave/12, sum.mog/12))
  }
  if(predict){
    df = data.frame(matrix(ncol=2, nrow=nrow(WL_ave)))
    colnames(df) = c('years', 'diff')
    df$years = 1:nrow(WL_ave)
    df$diff = WL_ave[,2] - WL_ave[,1]
    model = lm(diff~years, data=df)
    pred_interval = predict(model, newdata=df, interval='prediction', level=0.95)
  }

  plot(1:nrow(WL_ave)+1860, WL_ave[,1], type = 'l', col = 'blue',
    xlab='Year', ylab='Water Level (m)', xaxt='n')
  lines(1:nrow(WL_ave)+1860, WL_ave[,2], col = 'red')
  if(predict){
    pred_days = 1:(696/12)+1860
    pred_vals = WL_ave[1:(696/12),2]-pred_interval[1:(696/12),1]
    pred_min = WL_ave[1:(696/12),2]-pred_interval[1:(696/12),2]
    pred_max = WL_ave[1:(696/12),2]-pred_interval[1:(696/12),3]
    lines(pred_days, pred_vals, lwd=0.5, col='black', lty=1)
    for(i in 1:length(pred_days)){
      segments(pred_days[i], pred_min[i], pred_days[i], pred_max[i], lwd=2, col='blue')
    }
  }
  axis(1, at=seq(1860, 2020, 20))
  legend(x = 'topleft', legend = c('Average', 'Master Gauge'), lty = c(1,1), col = c('blue', 'red'))
  abline(v=696/12+1860, col="black", lty=2, lwd=2)
  title('Lake Erie water levels over time')

  plot(1:nrow(WL_ave)+1860, WL_ave[,2] - WL_ave[,1], col = 'black',
      xlab='Year', ylab='mog - average (m)', xaxt='n')
  axis(1, at=seq(1860, 2020, 20))
  title('Difference between Master gauge and lakewide average')
  if(predict){
    lines(df$years+1860, pred_interval[,1], col='blue')
    lines(df$years+1860, pred_interval[,2], col='red')
    lines(df$years+1860, pred_interval[,3], col='red')
  }
  dev.off()
}

#appends superior monthly mean water levels and 
#superior monthly master gauge water levels to WL in that order
#TODO: FIX ERROR IN READING NEW MOG DATA
addSuperior<-function(WL, merge_mog=TRUE){
  #mean monthly superior lakewide average data
  #1/1/1918 thru 12/31/2020
  sup.ave <- read.csv("data/LakeSuperior_MonthlyMeanWaterLevels_1918to2021.csv")
  sup.ave <- sup.ave[-nrow(sup.ave),-c(1)]
  sup.ave <- getLinearMonthly(sup.ave)

  #mean monthly historical superior master gauge data (Marquette)
  #1/1/1860 thru 09/1/1980
  sup.mog <- read.csv(paste("https://api.tidesandcurrents.noaa.gov/api/prod/datagetter?",
                          "product=monthly_mean",
                          "&application=NOS.COOPS.TAC.WL",
                          "&begin_date=18600101",
                          "&end_date=19800901",
                          "&datum=IGLD",
                          "&station=9099016",
                          "&time_zone=lst_ldt",
                          "&units=metric",
                          "&format=csv", sep=""))
  sup.mog <- sup.mog[,6]

  if (merge_mog == TRUE) {
    #mean monthly current superior master gauge data (Marquette C.G.)
    #10/1/1980 thru 12/31/2020
    sup.mog_new <- read.csv(paste("https://api.tidesandcurrents.noaa.gov/api/prod/datagetter?",
                            "product=monthly_mean",
                            "&application=NOS.COOPS.TAC.WL",
                            "&begin_date=19801001",
                            "&end_date=20201201",
                            "&datum=IGLD",
                            "&station=9099018",
                            "&time_zone=lst_ldt",
                            "&units=metric",
                            "&format=csv", sep=""))                     
    sup.mog_new <- sup.mog_new[,6]
  }

  #fill 1860-1918 with NA for lakewide average
  diff = length(WL[,1]) - length(sup.ave)
  fill = rep(NA, diff)
  sup.ave = append(fill, sup.ave)

  if (merge_mog == TRUE) {
    #append new mog data to old mog data
    sup.mog = append(sup.mog, sup.mog_new)
    WL = cbind(WL, sup.ave, sup.mog)
  }

  if (merge_mog == FALSE) {
    #fill 1981-2020 with NA for master gauge
    diff = length(WL[,1]) - length(sup.mog)
    fill = rep(NA, diff)
    sup.mog = append(sup.mog, fill)
    WL = cbind(WL, sup.ave, sup.mog)
  }
  
  WL
}

#assumes superior data was the last set added to WL
plotSuperior<-function(WL){
  sup_out = 'Superior_plot.pdf'
  if (file.exists(sup_out)) {
    file.remove(sup_out)
  }
  pdf(file = sup_out, width = 7, height = 10, onefile = FALSE)
  plot.new()
  par(mfrow = c(2,1))

  plot(WL[,1], WL[,ncol(WL)-1], type = 'l', col = 'blue',
    xlab='Days since 1/1/1860', ylab='Water Level (m)')
  lines(WL[,1], WL[,ncol(WL)], col = 'red')
  legend(x = 'topleft', legend = c('Average', 'Master Gauge'), lty = c(1,1), col = c('blue', 'red'))
  abline(v=696, col="black", lty=2, lwd=2)
  abline(v=1449, col="black", lty=2, lwd=2)
  title('Lake Superior water levels over time')

  plot(WL[,1], WL[,ncol(WL)] - WL[,ncol(WL)-1], col = 'black',
    xlab='Days since 1/1/1860', ylab='mog - average (m)')
  abline(v=1449, col="red", lty=2, lwd=2)
  title('Difference between Master gauge and lakewide average')
  dev.off()
}

#separates data by month and creates 12x2 plots- water level
#and difference for each month.
plotSuperiorMonthly<-function(WL){
  out = 'monthly_plots/Superior_monthly_plot.pdf' #varies for each lake
  if (file.exists(out)) {
    file.remove(out)
  }
  pdf(file = out, width=12, height=8, onefile = FALSE)
  plot.new()
  par(mfrow=c(3,4), mar=c(0,0,0,0), oma=c(7,7,7,7))
  mon_list = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

  for (i in 1:12) {
    mons = seq(i, nrow(WL), 12)
    plot(WL[mons,1]/12+1860, WL[mons,ncol(WL)] - WL[mons,ncol(WL)-1], col = 'black', ylim=c(-0.06, 0.06), xaxt='n', yaxt='n', ann=FALSE) #ylim varies for each lake
    if (i == 1 | i == 9) {
      axis(2, at=seq(-0.06, 0.06, 0.03)) #varies for each lake
    }
    if (i == 8) {
      axis(4, at=seq(-0.06, 0.06, 0.03)) #varies for each lake
    }
    if (i == 9 | i == 11) {
      axis(1, at=seq(1860, 2020, 20))
    }
    if (i == 2 | i == 4) {
      axis(3, at=seq(1860, 2020, 20))
    }
    legend('topleft', legend=mon_list[i], cex=1.5)
    abline(h=0, col='red', lty=2, lwd=1)
  }

  mtext('Lake Superior master gauge vs lakewide average for each month', side=3, line=3, cex=2, outer=TRUE) #varies for each lake
  mtext('Year', side=1, line=5, cex=1.7, outer=TRUE)
  mtext('MOG - Lakewide Average (m)', side=2, line=4, cex=1.7, outer=TRUE)

  dev.off()
}

#averages across all months
plotSuperiorYearly<-function(WL, predict=TRUE) {
  sup_out = 'extrapolated_yearly_water_levels/Superior_Yearly_plot.pdf'
  if (file.exists(sup_out)) {
    file.remove(sup_out)
  }
  pdf(file = sup_out, width = 7, height = 10, onefile = FALSE)
  plot.new()
  par(mfrow = c(2,1))

  WL_ave = data.frame()
  for (i in seq(1, nrow(WL), 12)) {
    sum.ave = 0
    sum.mog = 0
    for (j in 0:11) {
      sum.ave = sum.ave + WL[i+j,ncol(WL)-1]
      sum.mog = sum.mog + WL[i+j,ncol(WL)]
    }
    WL_ave = rbind(WL_ave, c(sum.ave/12, sum.mog/12))
  }
  if(predict){
    df = data.frame(matrix(ncol=2, nrow=nrow(WL_ave)))
    colnames(df) = c('years', 'diff')
    df$years = 1:nrow(WL_ave)
    df$diff = WL_ave[,2] - WL_ave[,1]
    model = lm(diff~years, data=df)
    pred_interval = predict(model, newdata=df, interval='prediction', level=0.95)
  }

  plot(1:nrow(WL_ave)+1860, WL_ave[,1], type = 'l', col = 'blue',
    xlab='Year', ylab='Water Level (m)', xaxt='n')
  lines(1:nrow(WL_ave)+1860, WL_ave[,2], col = 'red')
  if(predict){
    pred_days = 1:(696/12)+1860
    pred_vals = WL_ave[1:(696/12),2]-pred_interval[1:(696/12),1]
    pred_min = WL_ave[1:(696/12),2]-pred_interval[1:(696/12),2]
    pred_max = WL_ave[1:(696/12),2]-pred_interval[1:(696/12),3]
    lines(pred_days, pred_vals, lwd=0.5, col='black', lty=1)
    for(i in 1:length(pred_days)){
      segments(pred_days[i], pred_min[i], pred_days[i], pred_max[i], lwd=2, col='blue')
    }

    # x = 1:(696/12)+1860
    # y1 = WL_ave[1:(696/12),2]-pred_interval[1:(696/12),2]
    # y2 = WL_ave[1:(696/12),2]-pred_interval[1:(696/12),3]
    # polygon(c(x, rev(x)), c(y1, rev(y2)), col='blue', lty=0)
  }
  axis(1, at=seq(1860, 2020, 20))
  legend(x = 'topleft', legend = c('Average', 'Master Gauge'), lty = c(1,1), col = c('blue', 'red'))
  abline(v=696/12+1860, col="black", lty=2, lwd=2)
  title('Lake Superior water levels over time')

  plot(1:nrow(WL_ave)+1860, WL_ave[,2] - WL_ave[,1], col = 'black',
      xlab='Year', ylab='mog - average (m)', xaxt='n')
  axis(1, at=seq(1860, 2020, 20))
  title('Difference between Master gauge and lakewide average')
  if(predict){
    lines(df$years+1860, pred_interval[,1], col='blue')
    lines(df$years+1860, pred_interval[,2], col='red')
    lines(df$years+1860, pred_interval[,3], col='red')
  }

  dev.off()
}

#appends michigan monthly mean lakewide water levels and
#michigan monthly master gauge water levels to WL in that order
addMichigan<-function(WL){
  #mean monthly Michigan-Huron lakewide average data
  #1/1/1918 thru 12/31/2020
  mich.ave <- read.csv("data/LakeMichiganHuron_MonthlyMeanWaterLevels_1918to2021.csv")
  mich.ave <- mich.ave[-nrow(mich.ave),-c(1)]
  mich.ave <- getLinearMonthly(mich.ave)

  #mean monthly historical Michigan-Huron master gauge data (Harbor Beach)
  #1/1/1860 thru 12/31/2020
  mich.mog <- read.csv(paste("https://api.tidesandcurrents.noaa.gov/api/prod/datagetter?",
                          "product=monthly_mean",
                          "&application=NOS.COOPS.TAC.WL",
                          "&begin_date=18600101",
                          "&end_date=20201201",
                          "&datum=IGLD",
                          "&station=9075014",
                          "&time_zone=lst_ldt",
                          "&units=metric",
                          "&format=csv", sep=""))
  mich.mog <- mich.mog[,6]
  
  #fill 1860-1918 data with NA for lakewide average
  diff = length(WL[,1]) - length(mich.ave)
  fill = rep(NA, diff)
  mich.ave = append(fill, mich.ave)
  WL = cbind(WL, mich.ave, mich.mog)
  WL
}

#assumes Michigan data was the last set added to WL
plotMichigan<-function(WL){
  mich_out = 'Michigan_Huron_plot.pdf'
  if (file.exists(mich_out)) {
    file.remove(mich_out)
  }
  pdf(file = mich_out, width = 7, height = 10, onefile = FALSE)
  plot.new()
  par(mfrow = c(2,1))

  plot(WL[,1], WL[,ncol(WL)-1], type = 'l', col = 'blue',
    xlab='Days since 1/1/1860', ylab='Water Level (m)')
  lines(WL[,1], WL[,ncol(WL)], col = 'red')
  legend(x = 'topleft', legend = c('Average', 'Master Gauge'), lty = c(1,1), col = c('blue', 'red'))
  abline(v=696, col="black", lty=2, lwd=2)
  title('Lake Michigan-Huron water levels over time')

  plot(WL[,1], WL[,ncol(WL)] - WL[,ncol(WL)-1], col = 'black',
    xlab='Days since 1/1/1860', ylab='mog - average (m)')
  title('Difference between Master gauge and lakewide average')
  dev.off()
}

#separates data by month and creates 12x2 plots- water level
#and difference for each month.
plotMichiganMonthly<-function(WL){
  out = 'monthly_plots/Michigan_monthly_plot.pdf' #varies for each lake
  if (file.exists(out)) {
    file.remove(out)
  }
  pdf(file = out, width=12, height=8, onefile = FALSE)
  plot.new()
  par(mfrow=c(3,4), mar=c(0,0,0,0), oma=c(7,7,7,7))
  mon_list = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

  for (i in 1:12) {
    mons = seq(i, nrow(WL), 12)
    plot(WL[mons,1]/12+1860, WL[mons,ncol(WL)] - WL[mons,ncol(WL)-1], col = 'black', ylim=c(-0.06, 0.06), xaxt='n', yaxt='n', ann=FALSE) #ylim varies for each lake
    if (i == 1 | i == 9) {
      axis(2, at=seq(-0.06, 0.06, 0.03)) #varies for each lake
    }
    if (i == 8) {
      axis(4, at=seq(-0.06, 0.06, 0.03)) #varies for each lake
    }
    if (i == 9 | i == 11) {
      axis(1, at=seq(1860, 2020, 20))
    }
    if (i == 2 | i == 4) {
      axis(3, at=seq(1860, 2020, 20))
    }
    legend('topleft', legend=mon_list[i], cex=1.5)
    abline(h=0, col='red', lty=2, lwd=1)
  }

  mtext('Lake Michigan master gauge vs lakewide average for each month', side=3, line=3, cex=2, outer=TRUE) #varies for each lake
  mtext('Year', side=1, line=5, cex=1.7, outer=TRUE)
  mtext('MOG - Lakewide Average (m)', side=2, line=4, cex=1.7, outer=TRUE)

  dev.off()
}

#averages across all months
plotMichiganYearly<-function(WL, predict=TRUE) {
  mich_out = 'extrapolated_yearly_water_levels/Michigan_Yearly_plot.pdf'
  if (file.exists(mich_out)) {
    file.remove(mich_out)
  }
  pdf(file = mich_out, width = 7, height = 10, onefile = FALSE)
  plot.new()
  par(mfrow = c(2,1))

  WL_ave = data.frame()
  for (i in seq(1, nrow(WL), 12)) {
    sum.ave = 0
    sum.mog = 0
    for (j in 0:11) {
      sum.ave = sum.ave + WL[i+j,ncol(WL)-1]
      sum.mog = sum.mog + WL[i+j,ncol(WL)]
    }
    WL_ave = rbind(WL_ave, c(sum.ave/12, sum.mog/12))
  }
  if(predict){
    df = data.frame(matrix(ncol=2, nrow=nrow(WL_ave)))
    colnames(df) = c('years', 'diff')
    df$years = 1:nrow(WL_ave)
    df$diff = WL_ave[,2] - WL_ave[,1]
    model = lm(diff~years, data=df)
    pred_interval = predict(model, newdata=df, interval='prediction', level=0.95)
  }

  plot(1:nrow(WL_ave)+1860, WL_ave[,1], type = 'l', col = 'blue',
    xlab='Year', ylab='Water Level (m)', xaxt='n')
  lines(1:nrow(WL_ave)+1860, WL_ave[,2], col = 'red')
  if(predict){
    pred_days = 1:(696/12)+1860
    pred_vals = WL_ave[1:(696/12),2]-pred_interval[1:(696/12),1]
    pred_min = WL_ave[1:(696/12),2]-pred_interval[1:(696/12),2]
    pred_max = WL_ave[1:(696/12),2]-pred_interval[1:(696/12),3]
    lines(pred_days, pred_vals, lwd=0.5, col='black', lty=1)
    for(i in 1:length(pred_days)){
      segments(pred_days[i], pred_min[i], pred_days[i], pred_max[i], lwd=2, col='blue')
    }
  }
  axis(1, at=seq(1860, 2020, 20))
  legend(x = 'topleft', legend = c('Average', 'Master Gauge'), lty = c(1,1), col = c('blue', 'red'))
  abline(v=696/12+1860, col="black", lty=2, lwd=2)
  title('Lake Michigan-Huron water levels over time')

  plot(1:nrow(WL_ave)+1860, WL_ave[,2] - WL_ave[,1], col = 'black',
      xlab='Year', ylab='mog - average (m)', xaxt='n')
  axis(1, at=seq(1860, 2020, 20))
  title('Difference between Master gauge and lakewide average')
  if(predict){
    lines(df$years+1860, pred_interval[,1], col='blue')
    lines(df$years+1860, pred_interval[,2], col='red')
    lines(df$years+1860, pred_interval[,3], col='red')
  }
  dev.off()
}

#appends Ontario monthly mean lakewide water levels and
#Ontario monthly master gauge water levels to WL in that order
addOntario<-function(WL){
  #mean monthly Ontario lakewide average data
  #1/1/1918 thru 12/31/2020
  ont.ave <- read.csv("data/LakeOntario_MonthlyMeanWaterLevels_1918to2021.csv")
  ont.ave <- ont.ave[-nrow(ont.ave),-c(1)]
  ont.ave <- getLinearMonthly(ont.ave)

  #mean monthly historical Ontario master gauge data (Oswego)
  #1/1/1860 thru 12/31/2020
  ont.mog <- read.csv(paste("https://api.tidesandcurrents.noaa.gov/api/prod/datagetter?",
                          "product=monthly_mean",
                          "&application=NOS.COOPS.TAC.WL",
                          "&begin_date=18600101",
                          "&end_date=20201201",
                          "&datum=IGLD",
                          "&station=9052030",
                          "&time_zone=lst_ldt",
                          "&units=metric",
                          "&format=csv", sep=""))
  ont.mog <- ont.mog[,6]
  
  #fill 1860-1918 data with NA for lakewide average
  diff = length(WL[,1]) - length(ont.ave)
  fill = rep(NA, diff)
  ont.ave = append(fill, ont.ave)
  WL = cbind(WL, ont.ave, ont.mog)
  WL
}

#assumes Ontario data was the last set added to WL
plotOntario<-function(WL){
  ont_out = 'Ontario_plot.pdf'
  if (file.exists(ont_out)) {
    file.remove(ont_out)
  }
  pdf(file = ont_out, width = 7, height = 10, onefile = FALSE)
  plot.new()
  par(mfrow = c(2,1))

  plot(WL[,1], WL[,ncol(WL)-1], type = 'l', col = 'blue',
    xlab='Days since 1/1/1860', ylab='Water Level (m)')
  lines(WL[,1], WL[,ncol(WL)], col = 'red')
  legend(x = 'topleft', legend = c('Average', 'Master Gauge'), lty = c(1,1), col = c('blue', 'red'))
  abline(v=696, col="black", lty=2, lwd=2)
  title('Lake Ontario water levels over time')

  plot(WL[,1], WL[,ncol(WL)] - WL[,ncol(WL)-1], col = 'black',
    xlab='Days since 1/1/1860', ylab='mog - average (m)')
  title('Difference between Master gauge and lakewide average')
  dev.off()
}

#separates data by month and creates 12x2 plots- water level
#and difference for each month.
plotOntarioMonthly<-function(WL){
  out = 'monthly_plots/Ontario_monthly_plot.pdf' #varies for each lake
  if (file.exists(out)) {
    file.remove(out)
  }
  pdf(file = out, width=12, height=8, onefile = FALSE)
  plot.new()
  par(mfrow=c(3,4), mar=c(0,0,0,0), oma=c(7,7,7,7))
  mon_list = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

  for (i in 1:12) {
    mons = seq(i, nrow(WL), 12)
    plot(WL[mons,1]/12+1860, WL[mons,ncol(WL)] - WL[mons,ncol(WL)-1], col = 'black', ylim=c(-0.06, 0.06), xaxt='n', yaxt='n', ann=FALSE) #ylim varies for each lake
    if (i == 1 | i == 9) {
      axis(2, at=seq(-0.06, 0.06, 0.03)) #varies for each lake
    }
    if (i == 8) {
      axis(4, at=seq(-0.06, 0.06, 0.03)) #varies for each lake
    }
    if (i == 9 | i == 11) {
      axis(1, at=seq(1860, 2020, 20))
    }
    if (i == 2 | i == 4) {
      axis(3, at=seq(1860, 2020, 20))
    }
    legend('topleft', legend=mon_list[i], cex=1.5)
    abline(h=0, col='red', lty=2, lwd=1)
  }

  mtext('Lake Ontario master gauge vs lakewide average for each month', side=3, line=3, cex=2, outer=TRUE) #varies for each lake
  mtext('Year', side=1, line=5, cex=1.7, outer=TRUE)
  mtext('MOG - Lakewide Average (m)', side=2, line=4, cex=1.7, outer=TRUE)

  dev.off()
}

#averages across all months
plotOntarioYearly<-function(WL, predict=TRUE) {
  ont_out = 'extrapolated_yearly_water_levels/Ontario_Yearly_plot.pdf'
  if (file.exists(ont_out)) {
    file.remove(ont_out)
  }
  pdf(file = ont_out, width = 7, height = 10, onefile = FALSE)
  plot.new()
  par(mfrow = c(2,1))

  WL_ave = data.frame()
  for (i in seq(1, nrow(WL), 12)) {
    sum.ave = 0
    sum.mog = 0
    for (j in 0:11) {
      sum.ave = sum.ave + WL[i+j,ncol(WL)-1]
      sum.mog = sum.mog + WL[i+j,ncol(WL)]
    }
    WL_ave = rbind(WL_ave, c(sum.ave/12, sum.mog/12))
  }
  if(predict){
    df = data.frame(matrix(ncol=2, nrow=nrow(WL_ave)))
    colnames(df) = c('years', 'diff')
    df$years = 1:nrow(WL_ave)
    df$diff = WL_ave[,2] - WL_ave[,1]
    model = lm(diff~years, data=df)
    pred_interval = predict(model, newdata=df, interval='prediction', level=0.95)
  }

  plot(1:nrow(WL_ave)+1860, WL_ave[,1], type = 'l', col = 'blue',
    xlab='Year', ylab='Water Level (m)', xaxt='n')
  lines(1:nrow(WL_ave)+1860, WL_ave[,2], col = 'red')
  if(predict){
    pred_days = 1:(696/12)+1860
    pred_vals = WL_ave[1:(696/12),2]-pred_interval[1:(696/12),1]
    pred_min = WL_ave[1:(696/12),2]-pred_interval[1:(696/12),2]
    pred_max = WL_ave[1:(696/12),2]-pred_interval[1:(696/12),3]
    lines(pred_days, pred_vals, lwd=0.5, col='black', lty=1)
    for(i in 1:length(pred_days)){
      segments(pred_days[i], pred_min[i], pred_days[i], pred_max[i], lwd=2, col='blue')
    }
  }
  axis(1, at=seq(1860, 2020, 20))
  legend(x = 'topleft', legend = c('Average', 'Master Gauge'), lty = c(1,1), col = c('blue', 'red'))
  abline(v=696/12+1860, col="black", lty=2, lwd=2)
  title('Lake Ontario water levels over time')

  plot(1:nrow(WL_ave)+1860, WL_ave[,2] - WL_ave[,1], col = 'black',
      xlab='Year', ylab='mog - average (m)', xaxt='n')
  axis(1, at=seq(1860, 2020, 20))
  title('Difference between Master gauge and lakewide average')
  if(predict){
    lines(df$years+1860, pred_interval[,1], col='blue')
    lines(df$years+1860, pred_interval[,2], col='red')
    lines(df$years+1860, pred_interval[,3], col='red')
  }
  dev.off()
}

plotErieauMonthly<-function(WL) {
  data = read.csv('data/erie_data_Ca.csv')
  last = data[nrow(data), ]
  data = data[-nrow(data), ]
  data = rbind(data[1:939, ], last, data[940:nrow(data), ])

  eriau = data['er_CaG1']
  eriau = eriau[-(1933:1941), ]
  WL = cbind(WL, eriau)

  out = 'erie_monthly_plots/Erieau_monthly_plot.pdf' #varies for each lake
  if (file.exists(out)) {
    file.remove(out)
  }
  pdf(file = out, width=12, height=8, onefile = FALSE)
  plot.new()
  par(mfrow=c(3,4), mar=c(0,0,0,0), oma=c(7,7,7,7))
  mon_list = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

  for (i in 1:12) {
    mons = seq(i, nrow(WL), 12)
    plot(WL[mons,1]/12+1860, WL[mons,ncol(WL)] - WL[mons,2], col = 'black', ylim=c(-0.08, 0.08), xaxt='n', yaxt='n', ann=FALSE) #ylim varies for each lake
    if (i == 1 | i == 9) { axis(2, at=seq(-0.08, 0.08, 0.02)) }
    if (i == 8) { axis(4, at=seq(-0.08, 0.08, 0.02)) }
    if (i == 9 | i == 11) { axis(1, at=seq(1860, 2020, 20)) }
    if (i == 2 | i == 4) { axis(3, at=seq(1860, 2020, 20)) }
    legend('topleft', legend=mon_list[i], cex=1.5)
    abline(h=0, col='red', lty=2, lwd=1)
  }

  mtext('Lake Erie Erieau gauge vs lakewide average for each month', side=3, line=3, cex=2, outer=TRUE) #varies for each lake
  mtext('Year', side=1, line=5, cex=1.7, outer=TRUE)
  mtext('Erieau - Lakewide Average (m)', side=2, line=4, cex=1.7, outer=TRUE)

  dev.off()
}

plotDoverMonthly<-function(WL){
  data = read.csv('data/erie_data_Ca.csv')
  last = data[nrow(data), ]
  data = data[-nrow(data), ]
  data = rbind(data[1:939, ], last, data[940:nrow(data), ])

  dover = data['er_CaG4']
  dover = dover[-(1933:1941), ]
  WL = cbind(WL, dover)

  out = 'erie_monthly_plots/Dover_monthly_plot.pdf' #varies for each lake
  if (file.exists(out)) {
    file.remove(out)
  }
  pdf(file = out, width=12, height=8, onefile = FALSE)
  plot.new()
  par(mfrow=c(3,4), mar=c(0,0,0,0), oma=c(7,7,7,7))
  mon_list = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

  for (i in 1:12) {
    mons = seq(i, nrow(WL), 12)
    plot(WL[mons,1]/12+1860, WL[mons,ncol(WL)] - WL[mons,2], col = 'black', ylim=c(-0.25, 0.25), xaxt='n', yaxt='n', ann=FALSE) #ylim varies for each lake
    if (i == 1 | i == 9) { axis(2, at=seq(-0.25, 0.25, 0.05)) }
    if (i == 8) { axis(4, at=seq(-0.20, 0.20, 0.05)) }
    if (i == 9 | i == 11) { axis(1, at=seq(1860, 2020, 20)) }
    if (i == 2 | i == 4) { axis(3, at=seq(1860, 2020, 20)) }
    legend('topleft', legend=mon_list[i], cex=1.5)
    abline(h=0, col='red', lty=2, lwd=1)
  }

  mtext('Lake Erie Port Dover gauge vs lakewide average for each month', side=3, line=3, cex=2, outer=TRUE) #varies for each lake
  mtext('Year', side=1, line=5, cex=1.7, outer=TRUE)
  mtext('Port Dover - Lakewide Average (m)', side=2, line=4, cex=1.7, outer=TRUE)

  dev.off()
}

plotStanleyMonthly<-function(WL){
  data = read.csv('data/erie_data_Ca.csv')
  last = data[nrow(data), ]
  data = data[-nrow(data), ]
  data = rbind(data[1:939, ], last, data[940:nrow(data), ])

  stan = data['er_CaG5']
  stan = stan[-(1933:1941), ]
  WL = cbind(WL, stan)

  out = 'erie_monthly_plots/Stanley_monthly_plot.pdf' #varies for each lake
  if (file.exists(out)) {
    file.remove(out)
  }
  pdf(file = out, width=12, height=8, onefile = FALSE)
  plot.new()
  par(mfrow=c(3,4), mar=c(0,0,0,0), oma=c(7,7,7,7))
  mon_list = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

  for (i in 1:12) {
    mons = seq(i, nrow(WL), 12)
    plot(WL[mons,1]/12+1860, WL[mons,ncol(WL)] - WL[mons,2], col = 'black', ylim=c(-0.25, 0.25), xaxt='n', yaxt='n', ann=FALSE) #ylim varies for each lake
    if (i == 1 | i == 9) { axis(2, at=seq(-0.25, 0.25, 0.05)) }
    if (i == 8) { axis(4, at=seq(-0.20, 0.20, 0.05)) }
    if (i == 9 | i == 11) { axis(1, at=seq(1860, 2020, 20)) }
    if (i == 2 | i == 4) { axis(3, at=seq(1860, 2020, 20)) }
    legend('topleft', legend=mon_list[i], cex=1.5)
    abline(h=0, col='red', lty=2, lwd=1)
  }

  mtext('Lake Erie Port Stanley gauge vs lakewide average for each month', side=3, line=3, cex=2, outer=TRUE) #varies for each lake
  mtext('Year', side=1, line=5, cex=1.7, outer=TRUE)
  mtext('Port Stanley - Lakewide Average (m)', side=2, line=4, cex=1.7, outer=TRUE)

  dev.off()
}

plotColborneMonthly<-function(WL){
  data = read.csv('data/erie_data_Ca.csv')
  last = data[nrow(data), ]
  data = data[-nrow(data), ]
  data = rbind(data[1:939, ], last, data[940:nrow(data), ])

  gauge = data['er_CaG2']
  gauge = gauge[-(1933:1941), ]
  WL = cbind(WL, gauge)

  out = 'erie_monthly_plots/Colborne_monthly_plot.pdf' #varies for each lake
  if (file.exists(out)) {
    file.remove(out)
  }
  pdf(file = out, width=12, height=8, onefile = FALSE)
  plot.new()
  par(mfrow=c(3,4), mar=c(0,0,0,0), oma=c(7,7,7,7))
  mon_list = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

  for (i in 1:12) {
    mons = seq(i, nrow(WL), 12)
    plot(WL[mons,1]/12+1860, WL[mons,ncol(WL)] - WL[mons,2], col = 'black', ylim=c(-0.25, 0.25), xaxt='n', yaxt='n', ann=FALSE) #ylim varies for each lake
    if (i == 1 | i == 9) { axis(2, at=seq(-0.25, 0.25, 0.05)) }
    if (i == 8) { axis(4, at=seq(-0.20, 0.20, 0.05)) }
    if (i == 9 | i == 11) { axis(1, at=seq(1860, 2020, 20)) }
    if (i == 2 | i == 4) { axis(3, at=seq(1860, 2020, 20)) }
    legend('topleft', legend=mon_list[i], cex=1.5)
    abline(h=0, col='red', lty=2, lwd=1)
  }

  mtext('Lake Erie Port Colborne gauge vs lakewide average for each month', side=3, line=3, cex=2, outer=TRUE) #varies for each lake
  mtext('Year', side=1, line=5, cex=1.7, outer=TRUE)
  mtext('Port Colborne - Lakewide Average (m)', side=2, line=4, cex=1.7, outer=TRUE)

  dev.off()
}

plotEriePAMonthly<-function(WL){
  data = read.csv('data/erie_data_Ca.csv')
  last = data[nrow(data), ]
  data = data[-nrow(data), ]
  data = rbind(data[1:939, ], last, data[940:nrow(data), ])

  gauge = data['er_SLG8']
  gauge = gauge[-(1933:1941), ]
  WL = cbind(WL, gauge)

  out = 'erie_monthly_plots/EriePA_monthly_plot.pdf' #varies for each lake
  if (file.exists(out)) {
    file.remove(out)
  }
  pdf(file = out, width=12, height=8, onefile = FALSE)
  plot.new()
  par(mfrow=c(3,4), mar=c(0,0,0,0), oma=c(7,7,7,7))
  mon_list = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

  for (i in 1:12) {
    mons = seq(i, nrow(WL), 12)
    plot(WL[mons,1]/12+1860, WL[mons,ncol(WL)] - WL[mons,2], col = 'black', ylim=c(-0.25, 0.25), xaxt='n', yaxt='n', ann=FALSE) #ylim varies for each lake
    if (i == 1 | i == 9) { axis(2, at=seq(-0.25, 0.25, 0.05)) }
    if (i == 8) { axis(4, at=seq(-0.20, 0.20, 0.05)) }
    if (i == 9 | i == 11) { axis(1, at=seq(1860, 2020, 20)) }
    if (i == 2 | i == 4) { axis(3, at=seq(1860, 2020, 20)) }
    legend('topleft', legend=mon_list[i], cex=1.5)
    abline(h=0, col='red', lty=2, lwd=1)
  }

  mtext('Lake Erie Erie, PA gauge vs lakewide average for each month', side=3, line=3, cex=2, outer=TRUE) #varies for each lake
  mtext('Year', side=1, line=5, cex=1.7, outer=TRUE)
  mtext('Erie - Lakewide Average (m)', side=2, line=4, cex=1.7, outer=TRUE)

  dev.off()
}

plotToledoMonthly<-function(WL){
  data = read.csv('data/erie_data_Ca.csv')
  last = data[nrow(data), ]
  data = data[-nrow(data), ]
  data = rbind(data[1:939, ], last, data[940:nrow(data), ])

  gauge = data['er_SLG12']
  gauge = gauge[-(1933:1941), ]
  WL = cbind(WL, gauge)

  out = 'erie_monthly_plots/Toledo_monthly_plot.pdf' #varies for each lake
  if (file.exists(out)) {
    file.remove(out)
  }
  pdf(file = out, width=12, height=8, onefile = FALSE)
  plot.new()
  par(mfrow=c(3,4), mar=c(0,0,0,0), oma=c(7,7,7,7))
  mon_list = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

  for (i in 1:12) {
    mons = seq(i, nrow(WL), 12)
    plot(WL[mons,1]/12+1860, WL[mons,ncol(WL)] - WL[mons,2], col = 'black', ylim=c(-0.25, 0.25), xaxt='n', yaxt='n', ann=FALSE) #ylim varies for each lake
    if (i == 1 | i == 9) { axis(2, at=seq(-0.25, 0.25, 0.05)) }
    if (i == 8) { axis(4, at=seq(-0.20, 0.20, 0.05)) }
    if (i == 9 | i == 11) { axis(1, at=seq(1860, 2020, 20)) }
    if (i == 2 | i == 4) { axis(3, at=seq(1860, 2020, 20)) }
    legend('topleft', legend=mon_list[i], cex=1.5)
    abline(h=0, col='red', lty=2, lwd=1)
  }

  mtext('Lake Erie Toledo gauge vs lakewide average for each month', side=3, line=3, cex=2, outer=TRUE) #varies for each lake
  mtext('Year', side=1, line=5, cex=1.7, outer=TRUE)
  mtext('Toledo - Lakewide Average (m)', side=2, line=4, cex=1.7, outer=TRUE)

  dev.off()
}

plotErieGauges<-function(){
  data = read.csv('data/erie_data_Ca.csv')
  #fix erroneous row in data
  last = data[nrow(data), ]
  data = data[-nrow(data), ]
  data = rbind(data[1:939, ], last, data[940:nrow(data), ])

  WL = c(1:1932) #number of months from Jan 1860 to Dec 2020

  #get cleveland from API
  cleveland <- read.csv(paste("https://api.tidesandcurrents.noaa.gov/api/prod/datagetter?",
                          "product=monthly_mean",
                          "&application=NOS.COOPS.TAC.WL",
                          "&begin_date=18600101",
                          "&end_date=20201201",
                          "&datum=IGLD",
                          "&station=9063063",
                          "&time_zone=lst_ldt",
                          "&units=metric",
                          "&format=csv", sep=""))
  cleveland = clean_mog(cleveland) # 04/1938 is missing
  #get all other gauge data
  toledo = data['er_SLG12']
  toledo = toledo[-(1933:1941), ]
  eriePA = data['er_SLG8']
  eriePA = eriePA[-(1933:1941), ]
  colborne = data['er_CaG2']
  colborne = colborne[-(1933:1941), ]
  stanley = data['er_CaG5']
  stanley = stanley[-(1933:1941), ]
  #compute average of these gauges
  average = (toledo + cleveland + eriePA + colborne + stanley) / 5.0

  WL = cbind(WL, toledo, cleveland, eriePA, colborne, stanley, average)
  colnames(WL)[1] = 'months'

  out = 'Erie_Gauges.pdf' 
  if (file.exists(out)) {
    file.remove(out)
  }
  pdf(file = out, width=12, height=8, onefile = FALSE)
  plot.new()
  # par(mfrow=c(3,4), mar=c(0,0,0,0), oma=c(7,7,7,7))
  # mon_list = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

  # for (i in 1:12) {
  #   mons = seq(i, nrow(WL), 12)
  #   # plot((mons-i)/12+1860, WL[,2][mons] - WL[,7][mons], type='l', xlim=c(1950, 2020), ylim=c(-0.4,0.4), col='black', xaxt='n', yaxt='n', ann=FALSE) #toledo
  #   # lines((mons-i)/12+1860, WL[,3][mons] - WL[,7][mons], col='red') #cleveland
  #   # lines((mons-i)/12+1860, WL[,4][mons] - WL[,7][mons], col='blue') #eriePA
  #   # lines((mons-i)/12+1860, WL[,5][mons] - WL[,7][mons], col='green') #colborne
  #   # lines((mons-i)/12+1860, WL[,6][mons] - WL[,7][mons], col='purple') #stanley
  #   plot((mons-i)/12+1860, WL[,2][mons] - mean(WL[,2][mons]), type='l', xlim=c(1950, 2020), ylim=c(-1,1), col='black', xaxt='n', yaxt='n', ann=FALSE) #toledo
  #   lines((mons-i)/12+1860, WL[,3][mons] - mean(WL[,3][mons], na.rm=TRUE), col='red') #cleveland
  #   lines((mons-i)/12+1860, WL[,4][mons] - mean(WL[,4][mons], na.rm=TRUE), col='blue') #eriePA
  #   lines((mons-i)/12+1860, WL[,5][mons] - mean(WL[,5][mons], na.rm=TRUE), col='green') #colborne
  #   lines((mons-i)/12+1860, WL[,6][mons] - mean(WL[,6][mons], na.rm=TRUE), col='purple') #stanley
  #   if (i == 1 | i == 9) { axis(2, at=seq(150, 200, 10)) }
  #   if (i == 8) { axis(4, at=seq(150, 200, 10)) }
  #   if (i == 9 | i == 11) { axis(1, at=seq(1860, 2020, 20)) }
  #   if (i == 2 | i == 4) { axis(3, at=seq(1860, 2020, 20)) }
  #   legend('topleft', legend=mon_list[i], cex=1.5)
  # }
  averages = array(dim=c(12, 6))
  for (i in 1:12) {
    mons = seq(i, nrow(WL), 12)
    averages[i,1] = mean(WL[,2][mons] - WL[nrow(WL),2], na.rm=TRUE) #toledo
    averages[i,2] = mean(WL[,3][mons] - WL[nrow(WL),3], na.rm=TRUE) #cleveland
    averages[i,3] = mean(WL[,4][mons] - WL[nrow(WL),4], na.rm=TRUE) #eriePA
    averages[i,4] = mean(WL[,5][mons] - WL[nrow(WL),5], na.rm=TRUE) #colborne
    averages[i,5] = mean(WL[,6][mons] - WL[nrow(WL),6], na.rm=TRUE) #stanley
    averages[i,6] = mean(WL[,7][mons] - WL[nrow(WL),7], na.rm=TRUE) #average
  }
  plot(1:12, averages[,1], xlab='', ylab='', ylim=c(-0.7, -0.1), pch=20, cex=2, col='red') #toledo
  points(1:12, averages[,2], pch=20, cex=2, col='green') #cleveland
  points(1:12, averages[,3], pch=20, cex=2, col='cyan') #eriePA
  points(1:12, averages[,4], pch=20, cex=2, col='blue') #colborne
  points(1:12, averages[,5], pch=20, cex=2, col='orange') #stanley
  points(1:12, averages[,6], pch=20, cex=2, col='black') #average
  lines(1:12, averages[,1], lwd=2, col='red') #toledo
  lines(1:12, averages[,2], lwd=2, col='green') #cleveland
  lines(1:12, averages[,3], lwd=2, col='cyan') #eriePA
  lines(1:12, averages[,4], lwd=2, col='blue') #colborne
  lines(1:12, averages[,5], lwd=2, col='orange') #stanley
  lines(1:12, averages[,6], lwd=2, col='black') #average

  legend('topleft', legend=c('Toledo', 'Cleveland', 'Erie PA', 'Port Colborne', 'Port Stanley', 'Average'), 
      lty=c(1,1,1,1,1,1), col=c('red', 'green', 'cyan', 'blue', 'orange', 'black'), cex=1.3)
  axis(1, at=seq(1:12))

  mtext('Monthly fluctuations in water level', side=3, line=2, cex=2)
  mtext('Month', side=1, line=2, cex=1.7)
  mtext('Average difference from Dec 2020 (ave - dec)', side=2, line=2.8, cex=1.7)
  # mtext('Lake Erie Toledo gauge vs lakewide average for each month', side=3, line=3, cex=2, outer=TRUE) #varies for each lake
  # mtext('Year', side=1, line=5, cex=1.7, outer=TRUE)
  # mtext('Toledo - Lakewide Average (m)', side=2, line=4, cex=1.7, outer=TRUE)

  dev.off()

  print(tail(WL))
}

main<-function(){
  # note: plotLake must be called right after addLake for each lake

  WL = c(1:1932) # number of months from 1860-2020

  WL = addErie(WL)
  #plotErie(WL)
  plotErieMonthly(WL)
  plotErieYearly(WL, predict=TRUE)

  WL = addSuperior(WL, merge_mog=TRUE)
  #plotSuperior(WL)
  plotSuperiorMonthly(WL)
  plotSuperiorYearly(WL, predict=TRUE)

  WL = addMichigan(WL)
  #plotMichigan(WL)
  plotMichiganMonthly(WL)
  plotMichiganYearly(WL, predict=TRUE)

  WL = addOntario(WL)
  #plotOntario(WL)
  plotOntarioMonthly(WL)
  plotOntarioYearly(WL, predict=TRUE)

  plotErieauMonthly(WL)
  plotDoverMonthly(WL)
  plotStanleyMonthly(WL)
  plotColborneMonthly(WL)
  plotEriePAMonthly(WL)
  plotToledoMonthly(WL)
}

main()
#plotErieGauges()
print('done')
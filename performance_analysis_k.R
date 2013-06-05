require(PerformanceAnalytics)

table.Stats(return_series)
table.CalendarReturns(return_series[,2], geometric=FALSE)
charts.PerformanceSummary(return_series[,1]+return_series[,2],main="Performance", geometric=FALSE)
charts.PerformanceSummary(return_series[,1],main="Performance", geometric=FALSE)
chart.RiskReturnScatter(return_series)
table.AnnualizedReturns(return_series)

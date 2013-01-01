# Retrieving Data
file1 <- "./Stock_mkt_viewer.csv"
file2 <- "./Stock_mkt_viewer_f.csv"

mkt_data <- read.csv(file1)
f_data <- read.csv(file2)

merge_data <- merge(mkt_data,f_data, by.x="Code", by.y="Code", all=FALSE)
rm(mkt_data)
rm(f_data)

names(merge_data)

require(psych)
require(ggplot2)

# Choose, change Variable to Plot
plot_data <- merge_data
names(plot_data)
#### 1.Standardize Net Buy with Mkt.Cap
elements <- c("Inst_1D", "Inst_5D", "Inst_20D", "Inst_60D", "Inst_120D",
              "Inst_250D", "Sec_1D", "Sec_5D", "Sec_20D", "Sec_60D", "Sec_120D",
              "Sec_250D", "Fund_1D", "Fund_5D", "Fund_20D", "Fund_60D", 
              "Fund_120D", "Fund_250D", "Pension_1D", "Pension_5D", 
              "Pension_20D", "Pension_60D", "Pension_120D", "Pension_250D", 
              "Others_1D", "Others_5D", "Others_20D", "Others_60D", "Others_120D",
              "Others_250D", "Foreign_1D", "Foreign_5D", "Foreign_20D", "Foreign_60D", 
              "Foreign_120D", "Foreign_250D")
for(element in elements) {
  plot_data[[element]] <- plot_data[[element]]/plot_data$Mkt.Cap
}
#### 2.ROE, PBR, PER
## ROE
plot_data$ROE_Trailing <- (plot_data$NI_FQ_3 + plot_data$NI_FQ_2 + plot_data$NI_FQ_1 + plot_data$NI_FQ0)/plot_data$Equity_FQ0
plot_data$ROE_FY1 <- plot_data$NI_FY1/plot_data$Equity_FY1
plot_data$ROE_FY2 <- plot_data$NI_FY2/plot_data$Equity_FY2
## PBR
plot_data$PBR_Trailing <- plot_data$Mkt.Cap/plot_data$Equity_FQ0
plot_data$PBR_FY1 <- plot_data$Mkt.Cap/plot_data$Equity_FY1
plot_data$PBR_FY2 <- plot_data$Mkt.Cap/plot_data$Equity_FY2
## PER
plot_data$PER_Trailing <- plot_data$Mkt.Cap/(plot_data$NI_FQ_3 + plot_data$NI_FQ_2 + plot_data$NI_FQ_1 + plot_data$NI_FQ0)
plot_data$EY_Trailing <- 1/plot_data$PER_Trailing
plot_data[which(plot_data$PER_Trailing < 0),]$PER_Trailing <- NA # NA Negative PER
plot_data$PER_FY1 <- plot_data$Mkt.Cap/plot_data$NI_FY1
plot_data$EY_FY1 <- 1/plot_data$PER_FY1
plot_data[which(plot_data$PER_FY1 < 0),]$PER_FY1 <- NA # NA Negative PER
plot_data$PER_FY2 <- plot_data$Mkt.Cap/plot_data$NI_FY2
plot_data$EY_FY2 <- 1/plot_data$PER_FY2
plot_data[which(plot_data$PER_FY2 < 0),]$PER_FY2 <- NA # NA Negative PER
## Growth
plot_data$NI_Growth_FQ0 <- plot_data$NI_FQ0/plot_data$NI_FQ_4 - 1
plot_data[which((plot_data$NI_FQ_4 < 0) | (plot_data$NI_FQ0 <0)),]$NI_Growth_FQ0 <- NA # NA Negative NI
plot_data$NI_Growth_FY1 <- plot_data$NI_FY1/plot_data$NI_FY0-1
plot_data[which((plot_data$NI_FY1 < 0) | (plot_data$NI_FY0 <0)),]$NI_Growth_FY1 <- NA # NA Negative NI
plot_data$NI_Growth_FY2 <- plot_data$NI_FY2/plot_data$NI_FY1-1
plot_data[which((plot_data$NI_FY2 < 0) | (plot_data$NI_FY1 <0)),]$NI_Growth_FY2 <- NA # NA Negative NI

fit <- lm(get(y_var)~get(x_var), data=plot_data[which((plot_data[[x_var]] < xlim_u) & 
  (plot_data[[x_var]] > xlim_d) & (plot_data[[y_var]] < ylim_u) & (plot_data[[y_var]] > ylim_d)),c(y_var,x_var)])
summary(fit)
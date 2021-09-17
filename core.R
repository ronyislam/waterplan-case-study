library(dplyr)
library(gridExtra)
library(ggplot2)
library(stats)

#Cube root helper function needed to find roots for CDF
cubeRoot<-function(x){
  sign(x)*abs(x)^(1/3)
}

#estimated cdf function based on Italian data (estimating distribution only, not actual rainfall amounts)
cdf <- function(x){
  return(.2*(x-1.2)^3+0.5)
}

#solved cdf for pdf
pdf <- function(u){
  return(cubeRoot((u-0.5)/0.2)+1.2)
}

#plot CDF
x<- seq(0,pdf(1), by =.01)
y <- c()

for(i in 1:length(x)){
  t<-cdf(x[i])
  y<-c(y,t)
}

png(file="Rainfall_CDF.png",
    width=600, height=600)
cdfPlot <- plot(x,y)
cdfPlot
dev.off()

#trials
ntrials <- 50

#assumptions, parameters, equations
surfaceArea = 40000 #sq meters
CN = 68
S = 25400/CN-254

#setup data frames for recording data
poolTrials <- setNames(data.frame(matrix(ncol = 1, nrow = 0)), c("Volume (m3)"))
pool.df <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("CN","Trial","Day","Volume"))

#run trials
for(j in 1:ntrials){
  
  #run simulation for one year of rainfall, 365 days
  nsamples <- 365
  
  #system starts with no water
  pool <- 0
  
  #get uniform distribution to input into the PDF
  u <- runif(nsamples, min =0, max = 1);
  z<-c()

  for(i in 1:nsamples){
    #get today's rain
    r<-pdf(u[i])
    z<-c(z,r)
    
    #rain volume is rainfall multiplied by surface area
    totalRain <- r/1000*surfaceArea
    
    #calculate runoff
    Q = ((totalRain-0.2*S)^2/(totalRain+0.8*S))
    
    #add rain to system
    poolVal <- setNames(data.frame(matrix(c(CN, j,i,pool),nrow=1,ncol=4)), c("CN","Trial","Day","Volume"))
    pool.df <- rbind(pool.df , poolVal)
    pool <- pool+totalRain-Q
  }
  
  #save data from each trial
  poolTrial <- setNames(data.frame(matrix(c(pool),nrow=1,ncol=1)), c("Volume (m3)"))
  poolTrials <- rbind(poolTrials, poolTrial)
  
}

##distribution of water system volume from all trials
png(file="Distribution_of_trial_results_CN_of_68.png",
    width=600, height=600)
hist(poolTrials[["Volume (m3)"]])
dev.off()

#plot water volume over time for all trials
pool.df$CN <- as.factor(pool.df$CN)

png(file="Water_Volumes_Over_Time_50_Trials_CN_of_68.png",
    width=600, height=600)
poolPlot <- ggplot(pool.df, aes(x=Day, y=Volume, group = Trial, color = CN)) +
         geom_line()
poolPlot
dev.off()

pool.df$CN <- as.numeric(as.character(pool.df$CN))

#########################################
#Fair hydrologic conditions instead of poor, CN = 49
#########################################

#number of trials
ntrials <- 50

#assumptions, parameters, equations
surfaceArea = 40000 #sq meters
CN = 49
S = 25400/CN-254

#setup data frames for recording data
poolTrials <- setNames(data.frame(matrix(ncol = 1, nrow = 0)), c("Volume (m3)"))

#run trials
for(j in 1:ntrials){
  
  #run simulation for one year of rainfall, 365 days
  nsamples <- 365
  
  #system starts with no water
  pool <- 0
  
  #get uniform distribution to input into the PDF
  u <- runif(nsamples, min =0, max = 1);
  z<-c()
  
  for(i in 1:nsamples){
    #get today's rain
    r<-pdf(u[i])
    z<-c(z,r)
    
    #rain volume is rainfall multiplied by surface area
    totalRain <- r/1000*surfaceArea
    
    #calculate runoff
    Q = ((totalRain-0.2*S)^2/(totalRain+0.8*S))
    
    #add rain to system
    poolVal <- setNames(data.frame(matrix(c(CN, j,i,pool),nrow=1,ncol=4)), c("CN","Trial","Day","Volume"))
    pool.df <- rbind(pool.df , poolVal)
    pool <- pool+totalRain-Q
  }
  
  ##save distribution plot of daily rainfall from the fifth trial, just for show
  if(j==5){
    png(file="Rainfall_Distribution.png",
        width=600, height=600)
    distributionPlot <- hist(z)
    distributionPlot
    dev.off()
  }
  
  #save data from each trial
  poolTrial <- setNames(data.frame(matrix(c(pool),nrow=1,ncol=1)), c("Volume (m3)"))
  poolTrials <- rbind(poolTrials, poolTrial)
  
}

pool.df$CN <- as.factor(pool.df$CN)

##Volume needed to start production
requiredVolume = 13000

png(file="Water_Volumes_Over_Time_50_Trials_2_CN.png",
    width=600, height=600)
poolPlot <- ggplot(pool.df, aes(x=Day, y=Volume, group = interaction(Trial, CN), color = CN)) +
  geom_line() + 
  geom_hline(yintercept  = requiredVolume)
poolPlot
dev.off()

#Calculate what percent of trials were successful
finalWaterVolume <- pool.df %>%
  filter(Day == 365) %>%
  mutate(thresholdCheck = ifelse(Volume>=requiredVolume, 1, NA)) %>%
  group_by(CN) %>%
  summarise(meetsThreshold = sum(!is.na(thresholdCheck))/ntrials)

#Table for project success chance by CN number
png("Project_Success_Chances.png")
p<-tableGrob(finalWaterVolume)
grid.arrange(p)
dev.off()
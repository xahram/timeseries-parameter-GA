if(!require(devtools)) {
  #install.packages('devtools')
  devtools::install_github("dleutnant/tsconvert")
}


#install.packages("remotes")
#remotes::install_github("dleutnant/tsconvert")
library("tsconvert")

solar_data = read.csv("./Data/SN_m_hem_V2.0.csv", sep = ";")

colnames(solar_data) = c("Year", "Month","Frac_date",
                         "Mon_mean_SS", "N_mean_SS","S_mean_SS",
                         "Mon_mean_SD", "Mon_mean_North_SS_SD","Mon_mean_South_SS_SD",
                         "Number_of_obs_calculated",
                         "Number_of_obs_calculated_N", "Number_of_obs_calculated_S",
                         "Difinitive_marker") 


solar_data = solar_data[,1:10]

solar_data$date = paste(solar_data$Year, 
                        solar_data$Month, sep = "/")

View(solar_data)


#print(max(solar_data[solar_data$Year == 2022,]$Month))
#print(max(solar_data[solar_data$Year == 2022 & solar_data$Month == 02,]$Date))
#print(max(solar_data[solar_data$Year ==1992,]$Month))
#print(max(solar_data[solar_data$Year == 1992,]$Date))

# 31/2/2022

#### -> Modify Parameters of ARIMA TO GET PREDCITIONS
#### -> FOR THAT USE ARIA IN FITNESS FUNCTION
### -> THE FITNESS FUNCTION WILL SHOW THE MNIMIZATION OF ERROR OR MA IN ARMA



###################### FITNESS FUNCTION
# AUTOCORRELATION FUNCTION TO EVALUATE FITNESS VALUES JUST LIKE AIC IN LAB




if(F){
  solar_data = ts(data = solar_data$Daily_sunspot_no, 
                  start = as.Date("1992-01-01"), 
                  end = as.Date("2022-02-28"),
                  frequency = 12)
  
}

View(solar_data)
library(forecast)
arima=auto.arima(as.data.frame(solar_data)[,4]) # detected order is AR=2, MA=1


plot(arima$fitted)
solar_data = ts(data = solar_data$Mon_mean_SS, 
                start = "1992", 
                end = "2022",
                frequency = 12)

p = plot(solar_data)
line(p)

View(solar_data)
library(RCurl) # load RCurl package
# get sunspot series


series=solar_data # read from file
#L=length(series) # series length
View(series)
L=nrow(series) # series length
forecasts=32 # number of 1-ahead forecasts 7.3 Time Series Forecasting 135
outsamples=as.data.frame(series[(L-forecasts+1):L,]) # out-of-samples
sunspots=as.data.frame(series[1:(L-forecasts),]) # in-samples
View(outsamples)
View(sunspots)
# mean absolute error of residuals
maeres=function(residuals) mean(abs(residuals))
# fit best ARIMA model:
INIT=10 # initialization period (no error computed before)
library(forecast) # load forecast package
arima=auto.arima(as.data.frame(solar_data)[,1]) # detected order is AR=2, MA=1

# https://www.researchgate.net/post/What-is-the-purpose-of-the-AR-Roots-graph-in-Eviews-when-dealing-with-VECM
plot(arima) # show ARIMA model
print(arima)
cat("arima fit MAE=",
    maeres(arima$residuals[INIT:length(sunspots)]),"\n")
# one-step ahead forecasts:
# (this code is needed because forecast function
# only issues h-ahead forecasts)
LIN= nrow(sunspots) # length of in-samples
f1=rep(NA,forecasts)


for(h in 1:forecasts)
{ # execute arima with fixed coefficients but with more  in-samples:
    arima1=arima(series[1:(LIN+h-1),1],
                 order=arima$arma[c(2,1,3)],
                 fixed=arima$coef)
    
    f1[h]=forecast(arima1,h=1)$mean[1]
}

e1=maeres(outsamples-f1)
text1=paste("arima (MAE=",round(e1,digits=1),")",sep="")
# fit genetic programming arithmetic model:
library(rgp) # load rgp
ST=inputVariableSet("x1","x2")#same order of AR arima component
cF1=constantFactorySet(function() rnorm(1)) # mean=0, sd=1
FS=functionSet("+","*","-","/") # arithmetic
# genetic programming time series function
# receives function f
# if(h>0) then returns 1-ahead forecasts
# else returns MAE over fitting period (in-samples)
gpts=function(f,h=0)
{
  if(h>0) TS=series
  else TS=series[1:LIN]
  LTS=length(TS)
  F=rep(0,LTS) # forecasts
  E=rep(0,LTS) # residuals
  if(h>0) I=(LTS-h+1):LTS # h forecasts
  else I=INIT:LTS # fit to in-samples
  for(i in I)
  {
    F[i]=f(TS[i-1],TS[i-2])
    if(is.nan(F[i])) F[i]=0 # deal with NaN
    E[i]=TS[i]-F[i]
  }
  if(h>0) return (F[I]) # forecasts
  else return(maeres(E[I])) # MAE on fit
}
# mutation function
mut=function(func)
{ mutateSubtree(func,funcset=FS,inset=ST,conset=cF1,
                mutatesubtreeprob=0.3,maxsubtreedepth=4)}
set.seed(12345) # set for replicability
gp=geneticProgramming(functionSet=FS,inputVariables=ST,
                      constantSet=cF1,
                      populationSize=100,
                      fitnessFunction=gpts,
                      stopCondition=makeStepsStopCondition(1000),
                      mutationFunction=mut,
                      verbose=TRUE)
f2=gpts(gp$population[[which.min(gp$fitnessValues)]],
        h=forecasts)
e2=maeres(outsamples-f2)
text2=paste("gp (MAE=",round(e2,digits=1),")",sep="")
cat("best solution:\n")
print(gp$population[[which.min(gp$fitnessValues)]])
cat("gp fit MAE=",min(gp$fitnessValues),"\n")
# show quality of one-step ahead forecasts:
ymin=min(c(outsamples,f1,f2))
ymax=max(c(outsamples,f1,f2))
pdf("fsunspots.pdf")
par(mar=c(4.0,4.0,0.1,0.1))
plot(outsamples,ylim=c(ymin,ymax),type="b",pch=1,
     xlab="time (years after 1980)",ylab="values",cex=0.8)
lines(f1,lty=2,type="b",pch=3,cex=0.5)
lines(f2,lty=3,type="b",pch=5,cex=0.5)
legend("topright",c("sunspots",text1,text2),lty=1:3,
       pch=c(1,3,5))
dev.off()








############### BINARY VECTOR

binary_vector <- sample(c(0,1), replace=TRUE, size=15)
binary_vector

p_value <- binary_vector[0:5]
p_value

d_value <-  binary_vector[6:10]
d_value

q_value <- binary_vector[11:15]
q_value


######## https://stackoverflow.com/questions/25411380/convert-binary-vector-to-decimal


bitsToInt<-function(x) {
    return(packBits(rev(c(rep(FALSE, 32-length(x)%%32), as.logical(x))), "integer"))
}


p_value <- bitsToInt(p_value)
d_value <- bitsToInt(d_value)
q_value <- bitsToInt(q_value)


library(forecast)

arima=auto.arima(as.data.frame(solar_data)[,4], 
                 max.p = p_value, 
                 max.q = q_value,
                 max.d = d_value) # detected order is AR=2, MA=1
plot(arima$residuals)


#################### GA CALCULATION





####################### RUN GA
library(GA)


rmse <- function(arima){
  return(sqrt(mean(arima$residuals)^2))
}


featureFitness <- function(string, init_v) {
  #print(string)                         #uncomment this line if you want to print every single solution
  
  
  #cat(paste("HHHHHHHHHHHHHHHHH ", string, "\n"))
  inc <- which(string == 1) 
   if (length(inc)==0) return (-10E20)    #if  no feature is selected then give a terrible fitness to this solution
                #create a matrix of values for all the variables contained in 'inc'
  
  
  p_value <- string[0:5]
  
  d_value <-  string[6:10]
  
  q_value <- string[11:15]
  
  
  
  
  p_value <- bitsToInt(p_value)
  d_value <- bitsToInt(d_value)
  q_value <- bitsToInt(q_value)
  
  cat(paste("p_value ", p_value, "q_value ", q_value,"d_value ", d_value, "\n"))
  mod <- auto.arima(as.data.frame(solar_data)[,4], 
                 max.p = p_value, 
                 max.q = q_value,
                 max.d = d_value)                  #lm.fit computes faster than the 'lm'; because we have to fit thousands of models, use something efficient. 
  #class(mod) <- "lm"
  rmse(mod)   
}

runGA2 <- function(){
  
  maxGenerations <<- 5    #<<- makes it a global variable. So it will be visible to other functions e.g. monitor()
  popSize = 100
  pcrossover = 0.8
  pmutation = 0.1
  type = "binary"
  fitness = featureFitness   
  
  GA <- ga(type=type, fitness = fitness, init_v = binary_vector, nBits = length(binary_vector), 
           seed=1, popSize = popSize, 
           pcrossover = pcrossover, pmutation = pmutation, 
           maxiter = maxGenerations, monitor= monitor)

  return(GA)
}

some <- runGA2()

runGA <- function(noRuns = 30, problem = "feature"){
  #Specify GA parameter values; using the default values below. 
  if (problem == "feature"){
    maxGenerations <<- 5    #<<- makes it a global variable. So it will be visible to other functions e.g. monitor()
    popSize = 200
    pcrossover = 0.8
    pmutation = 0.1
    type = "binary"
    fitness = featureFitness              #fitness function defined in feature-selection.R
  }
  
  else {
    cat("invalid problem specified. Exiting ... \n")
    return()
  }
  
  
  #Set up what stats you wish to note.    
  statnames = c("best", "mean", "median")
  thisRunResults <<- matrix(nrow=maxGenerations, ncol = length(statnames)) #stats of a single run
  resultsMatrix = matrix(1:maxGenerations, ncol = 1)  #stats of all the runs
  
  resultNames = character(length(statnames)*noRuns)
  resultNames[1] = "Generation"
  
  bestFitness <<- -Inf
  bestSolution <<- NULL
  for (i in 1:noRuns){
    cat(paste("Starting Run ", i, "\n"))
    if (problem == "feature")
      GA <- ga(type=type, fitness = fitness, init_v = binary_vector, nBits = length(binary_vector), 
               seed=i, popSize = popSize, 
               pcrossover = pcrossover, pmutation = pmutation, 
               maxiter = maxGenerations, monitor= monitor)
   
    resultsMatrix = cbind(resultsMatrix, thisRunResults)
    
    if (GA@fitnessValue > bestFitness){
      bestFitness <<- GA@fitnessValue
      bestSolution <<- GA@solution
    }
    #Create column names for the resultsMatrix
    for (j in 1:length(statnames)) resultNames[1+(i-1)*length(statnames)+j] = paste(statnames[j],i)
  }
  colnames(resultsMatrix) = resultNames
  return (resultsMatrix)
}

getBestFitness<-function(){
  return(bestFitness)
}

getBestSolution<-function(){
  return(bestSolution)
}





#This function is used by the GA to compute or report the statistics of your interest after every generation.
#This function overrides the default functionality provided by gaMonitor().
monitor <- function(obj){
  # gaMonitor(obj)                      #call the default gaMonitor to print the usual messages during evolution
  iter <- obj@iter                      #get the current iternation/generation number 
  if (iter <= maxGenerations){          #some error checking
    fitness <- obj@fitness              #get the array of all the fitness values in the present population
    #<<- assigns a value to the global variable declared outside the scope of this function.    
    thisRunResults[iter,1] <<- max(fitness)
    thisRunResults[iter,2] <<- mean(fitness)    
    thisRunResults[iter,3] <<- median(fitness)
    cat(paste("\rGA | generation =", obj@iter, "Mean =", thisRunResults[iter,2], "| Best =", thisRunResults[iter,1], "\n"))
    flush.console()
  }  
  else{                               #print error messages
    cat("ERROR: iter = ", iter, "exceeds maxGenerations = ", maxGenerations, ".\n")
    cat("Ensure maxGenerations == nrow(thisRunResults)")
  }
}














runGA(problem = "feature")

##################### FEATURE SELECTION

getBenchmark <- function(){
  #if the "UsingR" has not been installed on your system, install it. 
  data("fat", package = "UsingR")
  
  #The dataset is described here: https://rdrr.io/cran/UsingR/man/fat.html. 
  #str(fat)  #Uncomment/Run this command to check the structure of the dataset
  
  #Fit a linear model. Dependent/outcome variable is 'body.fat.siri'; independent variables are all those listed after '~'.
  mod <- lm(body.fat.siri ~ age + weight + height + neck + chest + abdomen +
              +    hip + thigh + knee + ankle + bicep + forearm + wrist, data = fat)
  return (mod)
}

getData<-function(){
  #if the "UsingR" has not been installed on your system, install it. 
  data("fat", package = "UsingR")
  
  #The dataset is described here: https://rdrr.io/cran/UsingR/man/fat.html. 
  #str(fat)  #Uncomment/Run this command to check the structure of the dataset
  
  #Fit a linear model. Dependent/outcome variable is 'body.fat.siri'; independent variables are all those listed after '~'.
  mod <- getBenchmark()
  
  #Extract the input data from the fitted model. You can extract the data directly from the variable 'fat' but you 
  #will have to explicitly mention all the variables used in the fitting above. 
  xx <- model.matrix(mod)[, -1]   
  yy <- fat$body.fat.siri          #the response variable
  data <- cbind(xx,yy)
  return (data)
}



featureFitness <- function(string,xx,yy) {
  #print(string)                         #uncomment this line if you want to print every single solution
  inc <- which(string == 1)              #'inc' includes those features/variables for which 'string' contains 1
  if (length(inc)==0) return (-10E20)    #if  no feature is selected then give a terrible fitness to this solution
  X <- cbind(1, xx[,inc])                #create a matrix of values for all the variables contained in 'inc'
  
  mod <- lm.fit(X, yy)                  #lm.fit computes faster than the 'lm'; because we have to fit thousands of models, use something efficient. 
  class(mod) <- "lm"
  -AIC(mod)                  #AIC should be minimised. But the GA package maximises. So let's turn it into a
  #maximisation problem. However, negative values will be a problem with roulette wheel
  #selection which requires positive values to build a roulette wheel. Therefore, consider
  #other ways of inverting the minimisation problem such as 1/(1+AIC); this normalises 
  #the values between 0 (the worst) and 1 (the best).
}


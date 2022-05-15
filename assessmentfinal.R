if(!require(devtools)) {
  #install.packages('devtools')
  devtools::install_github("dleutnant/tsconvert")
}
# https://www.youtube.com/watch?v=iwRtpJDDw5M
#load("ga1.RData")
load("gasuper.RData")


#install.packages("remotes")
#remotes::install_github("dleutnant/tsconvert")
library("tsconvert")

solar_data = read.csv("./Data/SN_m_hem_V2.0.csv", sep = ";")
# solar_data = read.csv("https://wwwbis.sidc.be/silso/INFO/snmshemcsv.php", sep = ";")

colnames(solar_data) = c("Year", "Month","Frac_date",
                         "Mon_mean_SS", "N_mean_SS","S_mean_SS",
                         "Mon_mean_SD", "Mon_mean_North_SS_SD","Mon_mean_South_SS_SD",
                         "Number_of_obs_calculated",
                         "Number_of_obs_calculated_N", "Number_of_obs_calculated_S",
                         "Difinitive_marker") 


solar_data = solar_data[,1:10]

solar_data$date = paste(solar_data$Year, 
                        solar_data$Month, sep = "/")


solar_data_train <- solar_data[1:nrow(solar_data)*0.8,]
solar_data_test <- solar_data[-(1:nrow(solar_data)*0.8),]


colnames(solar_data_test) = c("Date",   "Mon_mean_SS") 




###################### TIME SREIES SIMPLE PREDICTION
library(forecast)

solar_data_ts_data_train = ts(data = solar_data_train$Mon_mean_SS, 
                              start = "1992", 
                              end = "2016",
                              frequency = 12)


solar_data_ts_data_test = ts(data = solar_data_test$Mon_mean_SS, 
                             start = "2016", 
                             end = "2022",
                             frequency = 12)

plot(solar_data_ts_data_train, 
     main="Time Series Of Monthly Sunspot Since 1992",
     xlab="Time",
     ylab= "Mean Sunspots",
     col="red")

## ARIMA TRAIN

arima_train=auto.arima(solar_data_ts_data_train) # detected order is AR=2, MA=1


plot(arima_train$fitted,  main="Fitted Time Series of ARIMA model on monthly data",
     xlab="Time",
     ylab= "Mean Sunspots",
     col="green")


lines(solar_data_ts_data_train , col=2)
summary(arima_train)

################# GET PREDICTIONS

forecast = forecast(arima_train, h = 60)

plot(forecast, main="Forcasted values of the next 60 months",
     xlab="Time",
     ylab= "Mean Sunspots",
     col="blue"
)
#lines(solar_data_ts_data_test,col=2)

plot(arima_train$residuals)
qqnorm(forcast$residuals)
acf(forcast$residuals)
pacf(forcast$residuals, main="PACF Lag error bars ")
summary(arima_train)
summary(arima_test)
accuracy(arima_train)
accuracy(arima_test)


pred <- predict(arima_train, n.ahead=60)

plot(pred$pred, main="Forcasted values of the next 60 months",
     xlab="Time",
     ylab= "Mean Sunspots",
     col="blue"
)

x= ts(solar_data_ts_data_train, frequency = 12, start = c(1992, 02))
y = ts(pred$pred, frequency = 12, start = c(2016, 03))
ts.plot(x, y, gpars = list(col = c("black", "red")), main="Forcasted values of the next 60 months",
        xlab="Time",
        ylab= "Mean Sunspots")
accuracy(arima)

################## ARIMA TEST
arima_test=auto.arima(solar_data_ts_data_test) # detected order is AR=2, MA=1


plot(arima_test$fitted,  main="Fitted Time Series of ARIMA model on monthly data",
     xlab="Time",
     ylab= "Mean Sunspots",
     col="green")

forcast_test = forecast(arima_test, h = 60)

summary(forcast_train)

plot(forcast_test, main="Forcasted values of the next 60 months",
     xlab="Time",
     ylab= "Mean Sunspots",
     col="blue"
)

lines(solar_data_ts_data_test,col=2)


accuracy(arima_train)
accuracy(forcast_train)
summary(forcast_train)


############################## START OF GA #####################


############### BINARY VECTOR

binary_vector <- sample(c(0,1), replace=TRUE, size=10)
binary_vector

p_value <- binary_vector[0:4]
p_value

d_value <-  binary_vector[5:6]
d_value

q_value <- binary_vector[7:10]
q_value


######## https://stackoverflow.com/questions/25411380/convert-binary-vector-to-decimal


bitsToInt<-function(x) {
  return(packBits(rev(c(rep(FALSE, 32-length(x)%%32), as.logical(x))), "integer"))
}


p_value <- bitsToInt(p_value)
d_value <- bitsToInt(d_value)
q_value <- bitsToInt(q_value)

p_value
d_value
q_value


library(forecast)


########## THIS IS TEST PART ################
solar_data_ts_data = ts(data = solar_data$Mon_mean_SS, 
                        start = "1992", 
                        end = "2022",
                        frequency = 12)
plot(solar_data_ts_data)



out <- tryCatch(
  { 
    predicted_arima <<- Arima(solar_data_ts_data , 
                              order= c(11, 1, 12)  
    )
    # Just to highlight: if you want to use more than one 
    # R expression in the "try" part then you'll have to 
    # use curly brackets.
    # 'tryCatch()' will return the last evaluated expression 
    # in case the "try" part was completed successfully
    
    message("This is the 'try' part")
    
    
  },
  error=function(cond) {
    message("Here's the original error message:")
    message(cond)
    # Choose a return value in case of error
    return(-100000000000000)
  }
  
  
)

out

predicted_arima
plot(predicted_arima$x,col="red")
lines(fitted(predicted_arima),col="blue")
cat(paste("p: ", p_value), " ", paste("d: ", d_value), paste("q: ", q_value))
plot(predicted_arima$residuals)
forcast1 = forecast(predicted_arima, h = 17)
forcast1

plot(forcast1)
plot(forcast1$residuals)
qqnorm(forcast1$residuals)
acf(forcast1$residuals)
pacf(forcast1$residuals)
summary(arima1)
accuracy(arima1)




arima1=auto.arima(solar_data_ts_data,
                  start.p = p_value, 
                  start.q = q_value,
                  d = d_value) # detected order is AR=2, MA=1


arima1
plot(arima1$fitted)
forcast1 = forecast(arima1, h = 17)
forcast1

plot(forcast1)
plot(forcast1$residuals)
qqnorm(forcast1$residuals)
acf(forcast1$residuals)
pacf(forcast1$residuals)
summary(arima1)
accuracy(arima1)



arima=auto.arima(as.data.frame(solar_data)[,4], 
                 max.p = p_value, 
                 max.q = q_value,
                 max.d = d_value) # detected order is AR=2, MA=1

############################ TEST PART ENDS###################





#################### GA CALCULATION ##########################





####################### RUN GA
library(GA)

rmse <- function(arima){
  
  ########## VARIOUS FITNESS FUNCTIONS###########
  
  
  #return(arima$bic)
  #return (1/( 1 + AIC(arima$model) ))
  return(sqrt(mean(arima$residuals)^2))
}


featureFitness <- function(string, init_v) {
  #print(string)                         #uncomment this line if you want to print every single solution
  
  
  #cat(paste("HHHHHHHHHHHHHHHHH ", string, "\n"))
  inc <- which(string == 1) 
  if (length(inc)==0) return (-10E20)    #if  no feature is selected then give a terrible fitness to this solution
  #create a matrix of values for all the variables contained in 'inc'
  
  
  p_value <- string[0:4]
  
  d_value <-  string[5:6]
  
  q_value <- string[7:10]
  
  
  
  
  p_value <- bitsToInt(p_value)
  d_value <- bitsToInt(d_value)
  q_value <- bitsToInt(q_value)
  
  cat(paste("p_value ", p_value, "q_value ", q_value,"d_value ", d_value, "\n"))

  
  ############## error check is used in case lag parameters values come up with infinite solution.
  
  error_check <- F
  out <- tryCatch(
    {
      predicted_arima <<- Arima(solar_data_ts_data_train , 
                                order= c(p_value, d_value, q_value)  
      )  
    }, error=function(cond) {
      message("Here's the original error message:")
      message(cond)
      error_check = T
      # Choose a return value in case of error
      return(-10E20)
    }
    
  )
  
  if (error_check) return(-10E20)
  rmse(predicted_arima)   
}




##### FUNCTION TO CHECK BEST FITNESS VALUE
getBestFitness<-function(){
  return(bestFitness)
}


############## FUNCTION TO GET BEST BIT SOLUTION
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








runGA <- function(noRuns = 30, problem = "feature", crossover){
  #Specify GA parameter values; using the default values below. 
  if (problem == "feature"){
    maxGenerations <<- 30    #<<- makes it a global variable. So it will be visible to other functions e.g. monitor()
    popSize = 30
    pcrossover = 0.7
    pmutation = 0.1
    type = "binary"
    fitness = featureFitness              #fitness function defined in feature-selection.R
    crossover = ""
    
  }
  
  else {
    cat("invalid problem specified. Exiting ... \n")
    return()
  }
  
  if (crossover == "sp") { 
    defaultControl <- gaControl()
    gaControl("binary" = list(selection = "gabin_spCrossover"))
  } 
  else if (crossover == "uni")  gaControl("binary" = list(selection = "gabin_uCrossover"))
  else crossover = gaControl("binary" = list(selection = "gaperm_oxCrossover")) 
  
  
  
  
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








########################### SAMPLE GA RUNS
ga_1_ <- runGA(problem = "feature", crossover= "sp")
ga_1_best_fitness = bestFitness
ga_1_best_Solution = bestSolution

ga_2_ <- runGA(problem = "feature", crossover= "uni")
ga_2_best_fitness = bestFitness
ga_2_best_Solution = bestSolution

ga_3_ <- runGA(problem = "feature")
ga_3_best_fitness = bestFitness
ga_3_best_Solution = bestSolution


gaParsed_1 = parseData(ga_1_, firstcolumn = 2, noRuns=30)
ga_Parsed_2 = parseData(ga_2_, firstcolumn = 2, noRuns=30)
ga_Parsed_3 = parseData(ga_3_, firstcolumn = 2, noRuns=30)


plotbars(gaParsed1, gaParsed2, gaParsed3)



###############################################
ga1 <- runGA(problem = "feature", crossover= "sp")
ga1_bestfitness = bestFitness
ga1_bestSolution = bestSolution

save.image(file='ga1.RData')

View(solar_data)

ga2 <- runGA(problem = "feature", crossover= "uni")
save.image(file='ga2.RData')
ga2_bestfitness = bestFitness
ga2_bestSolution = bestSolution

ga1_bestfitness
ga1_bestSolution

ga2_bestSolution
ga2_bestfitness

ga3_bestfitness
ga3_bestSolution

ga1
ga2


ga3 <- runGA(problem = "feature")
save.image(file='ga3.RData')

ga3_bestfitness = bestFitness
ga3_bestSolution = bestSolution

######################### PLOT GRAPH
findminmax <- function(data, minimise = TRUE){
  minmax <- NA
  if (minimise) minmax <- min(data[,2])
  else minmax <- max(data[,2])
  
  rownum <- which(data[,2] == minmax)
  if (length(rownum) > 1) rownum <- rownum[1]
  
  if (minimise)
    return (minmax - data [rownum,3])
  else return (minmax + data [rownum,3])
}

plotbars<- function(data1, data2, data3, 
                    cap1 = "Single Point Crossover", 
                    cap2 = "Uniform Crossover", 
                    cap3 = "Crossover Order"){
  print(data1)
  print(data2)
  print(data3)
  data = data1
  hues = c("black","blue","green")
  
  min1 = findminmax(data1)   #min(data1) - data1 [which(data1 == min(data1))+2*nrow(data1)]
  min2 = findminmax(data2)   #min(data2) - data2 [which(data2 == min(data2))+nrow(data2)]
  min3 = findminmax(data3)   #min(data3) - data3 [which(data3 == min(data3))+nrow(data3)]
  
  max1 = findminmax(data1, FALSE)   #max(data1) + data1 [which(data1 == max(data1))+nrow(data1)]
  max2 = findminmax(data2, FALSE)   #max(data2) + data2 [which(data2 == max(data2))+nrow(data2)]
  max3 = findminmax(data3, FALSE)   #max(data3) + data3 [which(data3 == max(data3))+nrow(data3)]
  
  minn = min(min1, min2, min3)
  maxx = max(max1, max2, max3)
  
  
  df <- data.frame(x=data[,1], y=data[,2], dy = data[,3])  #dy = length of error bar
  plot(df$x, df$y, type = "l", col = hues[1],  ylim=c(minn, maxx), #ylim = c(0.96, 0.985),   #choose ylim CAREFULLY as per your data ranges
       main = "Best Fitness Values", xlab = "Generations", ylab = "Fitness")  #plot the line (mean values)
  segments(df$x, df$y - df$dy, df$x, df$y + df$dy, col = hues[1]);    #plot the error bars mean-errorbar, mean+errorbar
  
  data = data2
  df <- data.frame(x=data[,1], y=data[,2], dy = data[,3])  #dy = length of error bar  
  lines(df$x, df$y, col = hues[2])
  segments(df$x, df$y - df$dy, df$x, df$y + df$dy, col = hues[2]); 
  
  data = data3
  df <- data.frame(x=data[,1], y=data[,2], dy = data[,3])  #dy = length of error bar  
  lines(df$x, df$y, col = hues[3])
  segments(df$x, df$y - df$dy, df$x, df$y + df$dy, col = hues[3]); 
  
  legend("bottomright", legend = c(cap1, cap2, cap3), col = hues, lwd = 1,
         cex = 0.5)
}

parseData <- function(data, firstcolumn, noRuns){
  col <- firstcolumn
  
  allstats <- (ncol(data)-1)/noRuns   #how many stats were collected. Omit the first column (Generations)
  cols <- seq(col,noRuns*allstats, by=allstats)
  subdata <- data[,cols]
  noGens <- nrow(data)
  pdata <- matrix(nrow = noGens, ncol = 3)
  for (i in 1:noGens){
    pdata[i,1] = i
    pdata[i,2] = mean(subdata[i,])
    pdata[i,3] = 1.96*sd((subdata[i,]))/sqrt(noRuns)   #compute the length of error bar. 
  }
  
  return (pdata)
}


ga1
ga2
ga3

gaParsed1 = parseData(ga1, firstcolumn = 2, noRuns=30)
gaParsed2 = parseData(ga2, firstcolumn = 2, noRuns=30)
gaParsed3 = parseData(ga3, firstcolumn = 2, noRuns=30)

gaParsed1
gaParsed2
gaParsed3

cat("best solution:\n")
View(solar_data_train)

plotbars(gaParsed1, gaParsed2, gaParsed3)


ga1_bestfitness
ga1_bestSolution

ga2_bestfitness 
ga2_bestSolution

######## GA ARIMA FUNCTION 
ga3_bestSolution
ga3_bestfitness

bit1 <- as.vector(ga1_bestSolution)

bit1
p_value1 = bitsToInt(bit1[0:4])
d_value1 = bitsToInt(bit1[5:6])
q_value1 = bitsToInt(bit1[7:10])
p_value1
d_value1
q_value1



##################### USE above p, d ,q  values to train arima model

solar_data
predicted_arima_ga1 <<- Arima(solar_data_ts_data_train , 
                              order= c(13, 3, 14)  
)

predicted_arima_ga1
plot(predicted_arima_ga1$x,col="blue", main="Single Point Crossover p, d, q (13,3,14) Time Series",
     xlab="Time",
     ylab= "Mean Sunspots",)
lines(fitted(predicted_arima_ga1),col="red")
legend(x = "topright",          # Position
       legend = c("Fitted", "Actual"),  # Legend texts
       lty = c(1, 2),           # Line types
       col = c(2, 3),           # Line colors
       lwd = 2)


cat(paste("p: ", p_value), " ", paste("d: ", d_value), paste("q: ", q_value))
plot(predicted_arima_ga1$residuals)
forcast_ga1 = forecast(predicted_arima_ga1, h = 60)
forcast_ga1

plot(forcast_ga1,  main="Single Point Crossover p, d, q (13,3,14) Forecast",
     xlab="Time",
     ylab= "Mean Sunspots",)
plot(forcast_ga1$residuals)
qqnorm(forcast_ga1$residuals)
acf(forcast_ga1$residuals)
pacf(forcast_ga1$residuals)
summary(predicted_arima_ga1)
accuracy(predicted_arima_ga1)


####### GA2 ARIMA SOLUTION
ga2_bestSolution

bit2 <- as.vector(ga2_bestSolution)

bit2
p_value2 = bitsToInt(bit2[0:4])
d_value2 = bitsToInt(bit2[5:6])
q_value2 = bitsToInt(bit2[7:10])
p_value2
d_value2
q_value2



save.image(file='gasuper.RData')



predicted_arima_ga2 <<- Arima(solar_data_ts_data_train , 
                              order= c(13, 3, 14)  
)

predicted_arima_ga2
plot(predicted_arima_ga2$x,col="blue", main="Uniform Crossover p, d, q (13,3,14) Time Series",
     xlab="Time",
     ylab= "Mean Sunspots",)
lines(fitted(predicted_arima_ga2),col="red")
legend(x = "topright",          # Position
       legend = c("Fitted", "Actual"),  # Legend texts
       lty = c(1, 2),           # Line types
       col = c(2, 3),           # Line colors
       lwd = 2)



cat(paste("p: ", p_value), " ", paste("d: ", d_value), paste("q: ", q_value))
plot(predicted_arima_ga2$residuals)
forcast_ga2 = forecast(predicted_arima_ga2, h = 60)
forcast_ga2

plot(forcast_ga2, main="Uniform Crossover p, d, q (13,3,14) Forecast",
     xlab="Time",
     ylab= "Mean Sunspots")
plot(forcast_ga2$residuals)
qqnorm(forcast_ga2$residuals)
acf(forcast_ga2$residuals)
pacf(forcast_ga2$residuals)
summary(predicted_arima_ga2)
accuracy(predicted_arima_ga2)




######### GA3 ARIMA SOLUTION
ga3_bestfitness
ga3_bestSolution

bit3 <- as.vector(ga3_bestSolution)

bit3
p_value3 = bitsToInt(bit3[0:4])
d_value3 = bitsToInt(bit3[5:6])
q_value3 = bitsToInt(bit3[7:10])
p_value3
d_value3
q_value3



save.image(file='ga.RData')



predicted_arima_ga3 <<- Arima(solar_data_ts_data_train , 
                              order= c(p_value3, d_value3, q_value3)  
)

predicted_arima_ga3
plot(predicted_arima_ga3$x,col="blue", main="Crossover Order p, d, q (13,3,14) Time Series",
     xlab="Time",
     ylab= "Mean Sunspots")
lines(fitted(predicted_arima_ga3),col="red")
legend(x = "topright",          # Position
       legend = c("Fitted", "Actual"),  # Legend texts
       lty = c(1, 2),           # Line types
       col = c(2, 3),           # Line colors
       lwd = 2)


cat(paste("p: ", p_value), " ", paste("d: ", d_value), paste("q: ", q_value))
plot(predicted_arima_ga3$residuals)
forcast_ga3 = forecast(predicted_arima_ga3, h = 60)
forcast_ga3

plot(forcast_ga3, main="Crossover Order 60 months forecast",
     xlab="Time",
     ylab= "Mean Sunspots")
plot(forcast_ga3$residuals)
qqnorm(forcast_ga3$residuals)
acf(forcast_ga3$residuals)
pacf(forcast_ga3$residuals)
summary(predicted_arima_ga3)
accuracy(predicted_arima_ga3)


save.image(file='ga.RData')


plot(ga1)
plot(ga2)
plot(ga3)



############## CHECK RMSE OF GA VARIATIONS
accuracy(predicted_arima_ga1)
accuracy(forcast_ga1)
summary(predicted_arima_ga1)

accuracy(predicted_arima_ga2)
accuracy(forcast_ga2)
summary(predicted_arima_ga2)

accuracy(predicted_arima_ga3)
accuracy(forcast_ga3)
summary(predicted_arima_ga3)

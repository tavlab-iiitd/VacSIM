

library(dplyr)
library(ggplot2)
library(deSolve) 
setwd('˜/Result2_SEIR/')
########## param ################################
temp_Rx <- NULL
i = 20
incubation_period = 5
E = incubation_period
average_days_recover = 15
int_beta1 <- 0.05
int_beta2 <- 1
beta = 0.5
beta = runif(n = 10,min = 0.05,max = 0.7)
#gamma <- 1/average_days_recover
int_gamma1 <- 1/32
int_gamma2 <- 1/8
# gamma = runif(n = 10,min = 1/32,max = 1/8)
gamma = 1/20

#delta <- (1/incubation_period) + (7.21/1000)
int_delta1 <- 1/15
int_delta2 <- 1/4
# delta = runif(n = 10,min = 1/15,max = 1/4)
delta =  0.20721

################### SEIR model

seir_model = function (current_timepoint, state_values, parameters)
{
  # create state variables (local variables)
  S = state_values [1]        # susceptibles
  E = state_values [2]        # exposed
  I = state_values [3]        # infectious
  R = state_values [4]        # recovered
  with (
    as.list (parameters),     # variable names within parameters can be used
    {
      # compute derivatives
      dS = (-beta * S * I)/N
      dE = (beta * S * I)/N - (delta * E)
      dI = (delta * E) - (gamma * I)
      dR = (gamma * I)
      # combine results
      results = c (dS, dE, dI, dR)
      list (results)
    }
  )
}


################### Error

Err1 <- function(parameters) {
  #print("Hello")
  names(parameters) <- c("beta")##, "gamma","delta")
  #print(init)
  #out <- ode(y = init, times = Day, func = sis_model, parms = parameters)
  #print(out)
  out <- ode(init, Day, seir_model, parameters)
  #print(out)
  fit <- out[,3]
  #print(parameters["gamma"])
  #sum((Infected - fit)^2+(Removed - out[,4])^2)
  x <- sum((Infected-fit)^2)
  return(x)
}


population = c(31169272,16753235,32966238,112372972,1980602) ### Population of State
files = list.files(pattern = '.csv')[c(1,2)]   ###### 2 files Model output & Naive Approach

for(n in files){
  df1 = read.csv(n)
  names(df1)[2] = 'State'
  names(df1)[7] = 'Vaccine_perc'
  
  df1$Date <- as.Date(df1$Date, "%Y-%m-%d")
  print(n)
  Merge = data.frame(matrix(ncol = 6))
  names(Merge)=c("time","S","E" ,"I","R","Epoch")
  for(m in 1:10){
    set.seed(m)
    loc = unique(df1$State) 
    print(loc)
    print(m)
    j =1
    out = data.frame(matrix(ncol = 6))
    names(out)=c("time","S","E" ,"I","R","State")
    for(i in loc){
      print(i)
      tmp = dplyr::filter(df1,df1$State==i)
      N=population[j]
      print(N)
      j = j+1
      Infected =  tmp$Infected.People
      Recovered = tmp$Recovery
      Exposed  =  c(diff(Infected, E),rep(NA, E))
      Exposed[which(Exposed < 0)] <- 0
      Day <- 1:length(Infected)
      init <- c(S = tmp$Susceptible[1]-(df1$Vaccine_perc[1])*10000, E = Exposed[1],I = Infected[1],R = Recovered[1])
      
      
      Opt <- optim(c(beta[m],gamma,delta), Err1,
                   method = "L-BFGS-B",lower = c(int_beta1, int_gamma1, int_delta1), 
                   upper = c(int_beta2, int_gamma2, int_delta2)) # optimize with some sensible conditions
      
      Opt_par <- setNames(Opt$par, c("beta","gamma", "delta"))
      ##Opt_par <- c(Opt_par["beta"], gamma, delta)
      print(Opt_par)
      t <- c(1:45) 
      model1 <- ode(y = init, times = t, func = seir_model, parms = Opt_par)
      summary(model1)
      fit <- data.frame(ode(y = init, times = t, func = seir_model, parms = Opt_par))
      fit$State = i 
      fit$time = seq(as.Date('2020-09-16'),as.Date('2020-10-30'),by = 1)
      out = rbind(fit,out)
    }
    
    out = out[-226,]
    summ_out = out %>%
      group_by(time) %>%
      summarise_at(vars(names(out[2:5])),funs(sum)) %>%
      ungroup
    summ_out$Epoch = m
    Merge = rbind(summ_out,Merge)
    Merge = Merge[-c(226),]
    
  }
  write.csv(Merge,paste('SEIR',n,sep = '_'),row.names = F)
}

######### Plot 

Model = read.csv('SEIR_Model_output.csv')
Model = Model[,c('time','I')]
names(Model)[2] = 'Model_I'
Naive = read.csv('SEIR_Naive Approach Results.csv')
Naive = Naive[,c('time','I')]
names(Naive)[2] = 'Naive_I'
Model$diff = Naive$Naive_I-Model$Model_I
Model$Model_I = NULL

library(plyr)
df=ddply(Model,~time,summarise,mean=mean(diff),sd=sd(diff))
df=df[c(2:32),]
df$lwr = df$mean - 1.96*(df$sd)/sqrt(10)
df$upr = df$mean + 1.96*(df$sd)/sqrt(10)
df$sd = NULL

# write.csv(df,'SEIR_Evaluation.csv',row.names = F)

dat = read.csv('SEIR_Evaluation.csv')
dat$time = as.character.Date(dat$time)

ggplot(dat, aes(time,mean,group=1))+ 
  geom_line(aes(y=mean), colour="blue") + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  geom_ribbon(data = dat,aes(x=time,ymin= lwr,ymax= upr),alpha=0.2,fill='green')+
  ggtitle('') + ylab('')+xlab('Date')
ggsave('',dpi = 300)




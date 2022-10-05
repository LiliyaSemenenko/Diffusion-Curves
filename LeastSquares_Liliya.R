# How many observ. should be used for the training samples?



#########################################################
#                                                       #
#            Code to generate Linear Regression         #
#           prediction of technology adoption           #
#             curves for and their MAPEs for            #
#          Return Regression prediction research        #
#                                                       #
#                   Liliya Semenenko                    #
#                    September, 2022                    #
#                                                       #
#########################################################

# Read simulated data with 1001 observations
simdata = read.csv(file = "2022_24_09_Simulated_Data.csv", sep = ",", header=T)
plot(simdata$Time,simdata$Base.Return,pch=1, xlim=10)
#########################################################
#       Plot a baseline log return
#########################################################
# No noise data + lin reg
real = lm(Base.Return ~ Time, data = simdata)
plot(simdata$Time,simdata$Base.Return, pch=20, xlab="Time", ylab="Base log return") #plot(x-axis,y-axis)
abline(real, col = "red") #adds straight lines to a plot.

#########################################################
#             Set parameters
#########################################################
all_mape <- array(0, 20)
new_log = data.frame(matrix(ncol = 20, nrow = 0))
new_actual = data.frame(matrix(ncol = 20, nrow = 0))
train_time = data.frame(matrix(ncol = 0, nrow = 0))
k = array(0,1)

#########################################################
#  Generate MAPEs for 500 observations at each Noise level
#########################################################

mape_noise <- function(new_actual, new_log, train_time, k) {
  
  for(i in c(1:ncol(new_log))){ #1:20
    # split noisy log return
    train_return = new_log[2:(k+1),i] # 500 rows for each col
    lmodel = lm(train_return ~ train_time) # 500r for both
    
    #predict
    pred = predict(lmodel, newdata = data.frame(train_time = 1:1001))
    # predictions transform back to adoption curve
    pred_bell = rep(0, 1001)
    #pred_bell = data.frame(matrix(0,1001,1))
    pred_bell[1] = new_actual[1,i]
    for(j in c(2:1001)){
      pred_bell[j] = pred_bell[j-1] * exp(pred[j])
    }
    #MAPE
    ap = rep(0, k)
    for(p in c((k+2):1001)){
      ap[p-(k+1)] = abs((new_actual[p,i]-pred_bell[p])/new_actual[p,i])*100
    }
    all_mape[i] <- mean(ap)
  }
  #mape = data.frame(Mape = all_mape)
  mape = matrix(data = all_mape, 20, 1)
  formape = matrix(0,(1001-20),1)
  fin_mape = rbind(mape, formape)
  
  results = data.frame(matrix(NA,1001,2))
  results = cbind(pred,pred_bell,fin_mape) #mape[1:20,]
  return(results)
}

# resulting in 3 MAPE lists (20 points ea) for 1%, 5%, %10 Noise levels w/ 500 observ
a = mape_noise(simdata[,3:22],simdata[,64:83],simdata$Time[2:501],500) #1% Noise
b = mape_noise(simdata[,23:42],simdata[,84:103],simdata$Time[2:501],500) #5% Noise
c = mape_noise(simdata[,43:62],simdata[,104:123],simdata$Time[2:501],500) #10% Noise


#########################################################
#  Plot predicted adoption curves at each Noise level
#########################################################
seq = c(100,250,500)

#specify path to save PDF to
destination_LR = 'C:/Users/lilin/OneDrive/Документы/Stat Research/Grace_project_code/LSplots_curves.pdf'
#open PDF
pdf(file=destination_LR)

# 1% Noise
inp_Noise1 = function(k){
  curve_a = mape_noise(simdata[,3:22],simdata[,64:83],simdata$Time[2:(k+1)],k) #1% Noise
  plot(simdata$Time,simdata$Base,ldw=2, type="l",col="black", pch=20, xlab="Time", ylab="Base", main = sprintf("1%% Noise (%g observations)", k))
  lines(1:1001, curve_a[,2], lwd = 2, col = "red")
}
curve_aa = sapply(seq,inp_Noise1) #iterates over 100-500 observ, outputs 3 graphs

# 5% Noise
inp_Noise5 = function(k){
  curve_b = mape_noise(simdata[,23:42],simdata[,84:103],simdata$Time[2:(k+1)],k)
  plot(simdata$Time,simdata$Base,ldw=2, type="l",col="black", pch=20, xlab="Time", ylab="Base", main = sprintf("5%% Noise (%g observations)", k))
  lines(1:1001, curve_b[,2], lwd = 2, col = "red")
}
curve_bb = sapply(seq,inp_Noise5) 

# 10% Noise
inp_Noise10 = function(k){
  curve_c = mape_noise(simdata[,43:62],simdata[,104:123],simdata$Time[2:(k+1)],k)
  plot(simdata$Time,simdata$Base,ldw=2, type="l",col="black", pch=20, xlab="Time", ylab="Base", main = sprintf("10%% Noise (%g observations)", k))
  lines(1:1001, curve_c[,2], lwd = 2, col = "red")
}
curve_cc = sapply(seq,inp_Noise10)

# curve_aa
# curve_bb
# curve_cc

#turn off PDF plotting
dev.off()
#########################################################
#  Plot predicted log return at each Noise level
#########################################################
#specify path to save PDF to
destination_LR = 'C:/Users/lilin/OneDrive/Документы/Stat Research/Grace_project_code/LSplots_returns.pdf'
#open PDF
pdf(file=destination_LR)

# seq = c(100,250,500)

# 1% Noise
inp_Noise1 = function(k){
  return_a = mape_noise(simdata[,3:22],simdata[,64:83],simdata$Time[2:(k+1)],k) #1% Noise
  plot(simdata$Time,simdata$Base.Return,pch=1,
       xlab="Time", ylab="Log Return", main=sprintf("1%% Noise (%g observations)", k))
  lines(1:1001, return_a[,1], lwd = 2, col = "red")
}
return_aa = sapply(seq,inp_Noise1) #iterates over 100-500 observ, outputs 3 graphs

# 5% Noise
inp_Noise5 = function(k){
  return_b = mape_noise(simdata[,3:22],simdata[,64:83],simdata$Time[2:(k+1)],k) #1% Noise
  plot(simdata$Time,simdata$Base.Return, col="black",type="l",ldw=1,
       xlab="Time", ylab="Log Return", main=sprintf("5%% Noise (%g observations)", k))
  lines(1:1001, return_b[,1], lwd = 2, col = "red")
}
return_bb = sapply(seq,inp_Noise1) #iterates over 100-500 observ, outputs 3 graphs

# 10% Noise
inp_Noise10 = function(k){
  return_c = mape_noise(simdata[,3:22],simdata[,64:83],simdata$Time[2:(k+1)],k) #1% Noise
  plot(simdata$Time,simdata$Base.Return,col="black",type="l",ldw=1,
       xlab="Time", ylab="Log Return", main=sprintf("10%% Noise (%g observations)", k))
  lines(1:1001, return_c[,1], lwd = 2, col = "red")
}
return_cc = sapply(seq,inp_Noise1) #iterates over 100-500 observ, outputs 3 graphs

# return_aa
# return_bb
# return_cc

#turn off PDF plotting
dev.off()
#########################################################
#  Generate 3 matrices for 3 noise levels w/ r=20, c=5
#########################################################

# number of observations in training samples
seq1 = seq(from=50, to=100, by=50)
seq2 = c(100,250,500)

# 1% Noise
inp_Noise1 = function(k){
  mape_a = mape_noise(simdata[,3:22],simdata[,64:83],simdata$Time[2:(k+1)],k)
  newmape = mape_a[1:20,3]
  return(newmape)
}
mape_aa = sapply(seq2,inp_Noise1) #iterates over - observ, outputs  columns

# 5% Noise
inp_Noise5 = function(k){
  mape_b = mape_noise(simdata[,23:42],simdata[,84:103],simdata$Time[2:(k+1)],k)
  newmape = mape_b[1:20,3]
  return(newmape)
}
mape_bb = sapply(seq2,inp_Noise5)

# 10% Noise
inp_Noise10 = function(k){
  mape_c = mape_noise(simdata[,43:62],simdata[,104:123],simdata$Time[2:(k+1)],k)
  newmape = mape_c[1:20,3]
  return(newmape)
  }
mape_cc = sapply(seq2,inp_Noise10)

mape_aa
mape_bb
mape_cc
#########################################################
#  Plot 6 boxplots for each Noise level
#########################################################
#specify path to save PDF to
destination_LR = 'C:/Users/lilin/OneDrive/Документы/Stat Research/Grace_project_code/LSplots_MAPES.pdf'
#open PDF
pdf(file=destination_LR)


# 1% Noise
colnames(mape_aa)<-c("100","250","500")
boxplot(mape_aa, ylim = range(0:1000), main = "MAPE for 1% Noise")

# 5% Noise
colnames(mape_bb)<-c("100","250","500")
boxplot(mape_bb, ylim = range(0:1000), main = "MAPE for 5% Noise")

# 10% Noise
colnames(mape_cc)<-c("100","250","500")
boxplot(mape_cc, ylim = range(0:1000), main = "MAPE for 10% Noise")


#turn off PDF plotting
dev.off()
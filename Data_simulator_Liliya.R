#########################################################
#                                                       #
#              Code to generate simulated               #
#           technology adoption curves for              #
#        Return Regression prediction research          #
#                                                       #
#                       Dan Harris                      #
#                    September, 2022                    #
#                                                       #
#########################################################

# Set seed
set.seed(1)

#########################################################
#   Set simulaton parameters
#########################################################

n <- 1000   # number of observations per adoption curve
M <- 20     # number of adoption curves

#########################################################
#   Generate and plot baseline adoption curve
#########################################################

base = 1000*exp(-(0:n-n/2)^2/n^1.5)

# #specify path to save PDF to
# destination_base = 'C:/Users/lilin/OneDrive/Документы/Stat Research/Grace_project_code/Plot_AdoptionBase.pdf'
# #open PDF
# pdf(file=destination_base)

plot(0:n+1,base,ldw=2, type="l",col="blue",
     xlab="Time", ylab="Simulated Adoption", main="Baseline Adoption curve")
abline(h=c(0,1000),lty=3)

# #turn off PDF plotting
# dev.off()

#########################################################
#   Compute and plot base log(return) of adoption curves
#########################################################

base_log = rep(NA,n+1)
for (k in 1:n){
  base_log[k+1] <- log(base[k+1]/base[k])
}

#specify path to save PDF to
destination_bl = 'C:/Users/lilin/OneDrive/Документы/Stat Research/Grace_project_code/Plot_LogBase.pdf'
#open PDF
pdf(file=destination_bl)
plot(0:n,base_log,type="l",ldw=2,col="blue",
     xlab="Time", ylab="Log Return", main="Baseline Log Return")
#turn off PDF plotting
dev.off()

#########################################################
#   Generate adoption curves with added noise
#########################################################

my_function = function(n,M, p_noise){

curves = matrix(NA,n+1,M)
for (i in 1:M){ 
  for (j in 0:n+1){
    curves[j,i] = base[j]*(1+p_noise*rnorm(1)) 
  }
}

#########################################################
#   Compute log(return) of adoption curves
#########################################################

log_rtn <- matrix(NA,n+1,M)
for (i in 1:M){
  for (j in 1:n){
    log_rtn[j+1,i] <- log(curves[j+1,i]/curves[j,i])
  }
}
results = cbind(curves,log_rtn)
return(results)
}

#########################################################
#   Create data frame of Noise Curves and Log Returns to export
#########################################################

# Function Results: a= 1% Noise, b= 5% Noise, c= 10% Noise
a = my_function(n=1000, M=20, p_noise=0.01)
b = my_function(n=1000, M=20, p_noise=0.05)
c = my_function(n=1000, M=20, p_noise=0.1)

# Create a data frame with Base, Curves, Base Log, Log Return
allcurve = cbind(a[,1:20],b[,1:20],c[,1:20])
all_log = cbind(a[,21:40],b[,21:40],c[,21:40])
new = cbind(base, allcurve, base_log, all_log)

# Name Columns
names = c(rep(0, 60))
noise = 1:20
names[1:20] = c(sprintf("1%% Noise %s", noise))
names[21:40] = c(sprintf("5%% Noise %s", noise))
names[41:60] = c(sprintf("10%% Noise %s", noise))          

log_names = c(rep(0, 60))
log_names[1:20] = c(sprintf("1%% Return %s", noise))
log_names[21:40] = c(sprintf("5%% Return %s", noise))
log_names[41:60] = c(sprintf("10%% Return %s", noise)) 

colnames(new)[1] = c("Base")
colnames(new)[2:61] = names
colnames(new)[62] = c("Base Return")
colnames(new)[63:122] = log_names

# Make "Time" a 1st column
Time = data.frame(Time = 1:1001)
new = cbind(Time,new)

# Write a csv file with simulated data
write.csv(new, "C:/Users/lilin/OneDrive/Документы/Stat Research/Grace_project_code/2022_24_09_Simulated_Data.csv",row.names=F)

#########################################################
#   Plot all 20 curves for 3 noise levels in 1 pdf 
#########################################################

#specify path to save PDF to
destination = 'C:/Users/lilin/OneDrive/Документы/Stat Research/Grace_project_code/Plots_Noise.pdf'
#open PDF
pdf(file=destination)

p_noise1=0.01
p_noise5=0.05
p_noise10=0.1

### Curves for 1% Noise ###
plot(0:n,a[,1], type="l", ylim=c(0,1000*(1+2*p_noise1)),
     xlab="Time", ylab="Simulated Adoption", main="1% Noise Adoption curve")
for (i in 2:M){
  lines(0:n,a[,i],col=i)
}
abline(h=c(0,1000), lty=3)

### Curves for 5% Noise ###
plot(0:n,b[,1], type="l", ylim=c(0,1000*(1+2*p_noise5)), 
     xlab="Time", ylab="Simulated Adoption", main="5% Noise Adoption curve")
for (i in 2:M){
  lines(0:n,b[,i],col=i)
}
abline(h=c(0,1000), lty=3)

### Curves for 10% Noise ###
plot(0:n, c[,1], type="l", ylim=c(0,1000*(1+2*p_noise10)), 
     xlab="Time", ylab="Simulated Adoption", main="10% Noise Adoption curve")
for (i in 2:M){
  lines(0:n, c[,i], col=i)
}
abline(h=c(0,1000), lty=3)

#turn off PDF plotting
dev.off()


#########################################################
#   Plot all 20 Log Returns for 3 noise levels in 1 pdf 
#########################################################

#specify path to save PDF to
destination_LR = 'C:/Users/lilin/OneDrive/Документы/Stat Research/Grace_project_code/Plots_Return.pdf'
#open PDF
pdf(file=destination_LR)

### Return for 1% Noise ###
plot(1:n,a[2:1001,21], type="l", ylim=10*c(-p_noise1,p_noise1),
     xlab="Time", ylab="Log Return", main="1% Noise Log Return")
for (i in 2:M){
  lines(1:n,a[2:1001,20+i], col=i)
}

### Return for 5% Noise ###
plot(1:n,b[2:1001,21], type="l", ylim=10*c(-p_noise5,p_noise5),
     xlab="Time", ylab="Log Return", main="5% Noise Log Return")
for (i in 2:M){
  lines(1:n,b[2:1001,20+i], col=i)
}

### Return for 10% Noise ###
plot(1:n, c[2:1001,21], type="l", ylim=10*c(-p_noise10,p_noise10),
     xlab="Time", ylab="Log Return", main="10% Noise Log Return")
for (i in 2:M){
  lines(1:n,c[2:1001,20+i], col=i)
}

#turn off PDF plotting
dev.off()
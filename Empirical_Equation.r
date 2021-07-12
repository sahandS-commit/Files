mydata = read.table(file = "…/filename.csv", sep = ",", header=TRUE)
f <-function(x)  sum((x[1]*mydata$Load^x[2]*mydata$time^(x[3]+x[4]*mydata$Load)+x[5]*mydata$ï..Temp^x[6]  - mydata$output)^2)
r <- optim(c(0.008,-0.04,0.5,-0.004,-0.03,0.01), f,method="L-BFGS-B",lower = -1, upper = Inf,hessian = FALSE)
r$par
r$value
y <-r$par
f(y)
tts <- sum((mean(mydata$output) - mydata$output)^2)
rs <-1-f(y)/tts
x = y
x[1]*mydata$Load^x[2]*mydata$time^(x[3]+x[4]*(x[5]*mydata$ï..Temp^x[6]*mydata$Load)+x[7]*mydata$ï..Temp^x[8])+x[9]*mydata$ï..Temp^x[10]
mydata$output

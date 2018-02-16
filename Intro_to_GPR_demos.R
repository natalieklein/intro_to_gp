# Demos for data science club talk 2-15-18

library(zoo)
library(plotly)
library(gptk)
library(tidyverse)

###### Load data sets

bigdata = read_csv("google_trends_big_data.csv",skip=2) %>% as_tibble() %>% mutate(Time=1:n(),Month=as.Date(as.yearmon(Month)))
names(bigdata) = c("Date","Count","Time")
datascience = read_csv("google_trends_data_science.csv",skip=2) %>% as_tibble() %>% mutate(Time=1:n(),Month=as.Date(as.yearmon(Month)))
names(datascience) = c("Date","Count","Time")
statistics = read_csv("google_trends_statistics.csv",skip=2) %>% as_tibble() %>% mutate(Time=1:n(),Month=as.Date(as.yearmon(Month)))
names(statistics) = c("Date","Count","Time")
steelers = read_csv("google_trends_steelers.csv",skip=2) %>% as_tibble() %>% mutate(Time=1:n(),Month=as.Date(as.yearmon(Month)))
names(steelers) = c("Date","Count","Time")

###### Plot data sets with various regression fits

## 'Big Data' 
bigdata %>% ggplot(aes(x=Date,y=Count)) + geom_point() + 
  theme_classic() + ylab("Relative Google search popularity") + 
  ggtitle("Search term: 'Big Data'") + theme(text=element_text(size=20))

bigdata %>% filter(Date>"2011-01-01") %>%
  ggplot(aes(x=Date,y=Count)) + geom_point() + geom_smooth(method="lm",se=FALSE) + 
  theme_classic() + ylab("Relative Google search popularity") + 
  ggtitle("Search term: 'Big Data'") + theme(text=element_text(size=20))

bigdata %>% filter(Date>"2011-01-01") %>%
  ggplot(aes(x=Date,y=Count)) + geom_point() + 
  geom_smooth(method="lm",formula=y~splines::bs(x,3),se=FALSE) + 
  theme_classic() + ylab("Relative Google search popularity") + 
  ggtitle("Search term: 'Big Data'") + theme(text=element_text(size=20))

bigdata %>% filter(Date>"2011-01-01") %>%
  ggplot(aes(x=Date,y=Count)) + geom_point() + 
  geom_smooth(method="lm",formula=y~splines::bs(x,10),se=FALSE) + 
  theme_classic() + ylab("Relative Google search popularity") + 
  ggtitle("Search term: 'Big Data'") + theme(text=element_text(size=20))

bigdata %>% filter(Date>"2011-01-01") %>%
  ggplot(aes(x=Date,y=Count)) + geom_point() + 
  geom_smooth(method="lm",formula=y~splines::bs(x,30),se=FALSE) + 
  theme_classic() + ylab("Relative Google search popularity") + 
  ggtitle("Search term: 'Big Data'") + theme(text=element_text(size=20))

bigdata %>% filter(Date>"2011-01-01") %>%
  ggplot(aes(x=Date,y=Count)) + geom_line(color="#619CFF",lwd=1) + geom_point() + 
  theme_classic() + ylab("Relative Google search popularity") + 
  ggtitle("Search term: 'Big Data'") + theme(text=element_text(size=20))

g = bigdata %>% filter(Date>"2011-01-01") %>%
  ggplot(aes(x=Date,y=Count)) + geom_point() + 
  geom_smooth(method="lm",formula=y~splines::bs(x,10),se=FALSE) + 
  theme_classic() + ylab("Relative Google search popularity") + 
  ggtitle("Search term: 'Big Data'") + theme(text=element_text(size=20))
ggplotly(g)

## 'Data Science'
datascience %>% filter(Date>"2012-01-01") %>%
  ggplot(aes(x=Date,y=Count)) + geom_point() + geom_smooth(method="lm",se=FALSE) + 
  theme_classic() + ylab("Relative Google search popularity") + 
  ggtitle("Search term: 'Data Science'") + theme(text=element_text(size=20))

## 'Statistics' 
statistics %>% ggplot(aes(x=Date,y=Count)) + geom_point() + geom_smooth(method="lm",se=FALSE) + 
  theme_classic() + ylab("Relative Google search popularity") +
  ggtitle("Search term: 'Statistics'") + theme(text=element_text(size=20))

statistics %>% ggplot(aes(x=Date,y=Count)) + geom_point() + 
  geom_smooth(method="lm",formula=y~splines::bs(x,3),se=FALSE) + 
  theme_classic() + ylab("Relative Google search popularity") +
  ggtitle("Search term: 'Statistics'") + theme(text=element_text(size=20))

statistics %>% ggplot(aes(x=Date,y=Count)) + geom_point() + 
  geom_smooth(method="lm",formula=y~splines::bs(x,20),se=FALSE) + 
  theme_classic() + ylab("Relative Google search popularity") +
  ggtitle("Search term: 'Statistics'") + theme(text=element_text(size=20))

g = statistics %>% ggplot(aes(x=Date,y=Count)) + geom_point() + 
  geom_smooth(method="lm",formula=y~splines::bs(x,20),se=FALSE) + 
  theme_classic() + ylab("Relative Google search popularity")  +
  ggtitle("Search term: 'Statistics'") + theme(text=element_text(size=20))
ggplotly(g)

## 'Pittsburgh Steelers' 
steelers %>% ggplot(aes(x=Date,y=Count)) + geom_point() + geom_smooth(method="lm",se=FALSE) + 
  theme_classic() + ylab("Relative Google search popularity") +
  ggtitle("Search term: 'Pittsburgh Steelers'") + theme(text=element_text(size=20))

steelers %>% ggplot(aes(x=Date,y=Count)) + geom_point() + 
  geom_smooth(method="lm",formula=y~splines::bs(x,3),se=FALSE) + 
  theme_classic() + ylab("Relative Google search popularity") + 
  ggtitle("Search term: 'Pittsburgh Steelers'") + theme(text=element_text(size=20))

steelers %>% ggplot(aes(x=Date,y=Count)) + geom_point() + 
  geom_smooth(method="lm",formula=y~splines::bs(x,20),se=FALSE) + 
  theme_classic() + ylab("Relative Google search popularity") + 
  ggtitle("Search term: 'Pittsburgh Steelers'") + theme(text=element_text(size=20))

g = steelers %>% ggplot(aes(x=Date,y=Count)) + geom_point() + 
  geom_smooth(method="lm",formula=y~splines::bs(x,40),se=FALSE) + 
  theme_classic() + ylab("Relative Google search popularity") + 
  ggtitle("Search term: 'Pittsburgh Steelers'") + theme(text=element_text(size=20))
ggplotly(g)

## Compute autocovariance function
asteelers = acf(steelers$Count)
plot(asteelers,main="'Pittsburgh Steelers' search popularity autocovariance")


###### Posterior mean toy example
# sample size
n = 5
# true mean and variance
sigma = 3
mutrue = 2
# prior mean and variance
m = 0
s = 1.5

# draw samples
y = rnorm(n,mean=mutrue,sd=sigma)

# compute posterior mean and variance
postvar = sigma^2*s^2/(sigma^2+n*s^2)
postmean = (sigma^2*m+n*mean(y)*s^2)/(sigma^2+s^2*n)

# compute true, prior, and posterior densities
densvals = data.frame(x=seq(min(y)-1,max(y)+1,0.01)) %>% as_tibble() %>%
  mutate(truth=dnorm(x,mean=mutrue,sd=sigma),prior=dnorm(x,mean=m,sd=s),posterior=dnorm(x,mean=postmean,sd=sqrt(postvar))) %>%
  gather(key=dist,value=density,truth,prior,posterior)
maxdens = densvals %>% summarise(max(density)) %>% pull()

# plot data and densities
densvals %>% ggplot() +
  geom_histogram(data=data.frame(yobs=y), aes(x=yobs,y=..density..),breaks=seq(min(y)-1,max(y)+1,by=1),color="grey",fill="white") +
  geom_vline(xintercept=mean(y),lty=5) + geom_line( aes(x=x,y=density,group=dist,color=dist)) + 
  geom_label(data=data.frame(mean=mean(y)),aes(x=mean,y=maxdens-0.02,label="mean(y)")) +
  scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) + 
  theme_classic()


###### GP Regression examples
# Note: GPML toolbox in Matlab or GPFlow in Python are much better than R
# for Gaussian process stuff, but we can do a simple example here
dssub = datascience %>% filter(Date>"2012-01-01")
bigdatasub = bigdata %>% filter(Date>"2011-01-01") 

## Select training set (uncomment lines for whichever one you want)
# Big data
xTrain = as.matrix(bigdatasub$Time)
yTrain = as.matrix(bigdatasub$Count)

# Data science
#xTrain = as.matrix(dssub$Time)
#yTrain = as.matrix(dssub$Count)

# Steelers
#xTrain = as.matrix(steelers$Time)
#yTrain = as.matrix(steelers$Count)

# Statistics
#xTrain = as.matrix(statistics$Time)
#yTrain = as.matrix(statistics$Count)

## Initialize hyperparameters and create GP model
# See demo scripts in gptk R package for more examples
lengthScale = c(4, 8, 16)
inithypers = log(c(1/(lengthScale[1]), 1, 100))
options = gpOptions(); options$kern$comp = list('rbf','white')
x = as.matrix(seq(min(xTrain),max(xTrain)+24,0.1))
kern = kernCreate(x,"rbf")
Kx = kernCompute(kern,x,xTrain)
Ktrain = kernCompute(kern,xTrain)
model = gpCreate(dim(xTrain)[2], dim(yTrain)[2], xTrain, yTrain, options)
model = gpExpandParam(model, inithypers) ## This forces kernel computation.
ll_init = gpLogLikelihood(model) ## GP log-marginal likelihood for this model.
meanVar = gpPosteriorMeanVar(model, x, varsigma.return=TRUE) ## GP mean and variance.

## Optimize to find best hyperparameters
model = gpOptimise(model, display=TRUE, iters=600)
opthypers = gpExtractParam(model, only.values = FALSE);
opthypers = exp(opthypers);
ll_opt = gpLogLikelihood(model)
meanVar = gpPosteriorMeanVar(model, x, varsigma.return=TRUE)

## Plot results
gpPlot(model, x, meanVar$mu, meanVar$varsigma, xlim=range(x))


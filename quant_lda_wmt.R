# Required packages
library(quantmod)
library(PerformanceAnalytics)
library(MASS)
# Function to plot LDA density histograms
ggplotLDAPrep <- function(x){
  require(ggplot2)
  if (!is.null(Terms <- x$terms)) {
    data <- model.frame(x)
    X <- model.matrix(delete.response(Terms), data)
    g <- model.response(data)
    xint <- match("(Intercept)", colnames(X), nomatch = 0L)
    if (xint > 0L) 
      X <- X[, -xint, drop = FALSE]
  }
  means <- colMeans(x$means)
  X <- scale(X, center = means, scale = FALSE) %*% x$scaling
  rtrn <- as.data.frame(cbind(X,labels=as.character(g)))
  rtrn <- data.frame(X,labels=as.character(g))
  return(rtrn)
}
# Function to evaluate strategy performance with transaction fees
performance.transactions = function (WMT, strat.position, cost = .001, cum.return) {
  require(xts)
  require(quantmod)
  require(PerformanceAnalytics)
  tick = as.xts(WMT)
  ret = dailyReturn(tick)
  trade = as.numeric((strat.position != Lag(strat.position)));
  trade[1] = 1;
  trade = trade*cost;
  ret.sans.transactions = cum.return - trade;
  name = names(strat.position);
  newtitle = paste(name,'Strategy')
  charts.PerformanceSummary(cbind(ret,cum.return,ret.sans.transactions),main=newtitle)
  perform.out= rbind(SharpeRatio.annualized(ret.sans.transactions[,1],scale=252), 
                     Return.annualized(ret.sans.transactions[,1],scale=252),
                     Return.cumulative(ret.sans.transactions[,1], geometric=T))
  return(perform.out)
}
# Import data and build training / test sets
WMT = getSymbols('WMT', auto.assign = F)['2007::2016-06-13']
train = WMT["2007::2013"]
test = WMT["2014::"]
chartSeries(WMT,subset='2007::2016-06-13',theme=chartTheme('white'),TA="addVo();")
# Volume vectors
train.volume = Vo(train)
test.volume = Vo(test)
volume = Vo(WMT)
# Direction vectors
train.direction = ifelse((dailyReturn(train) > 0),'UP','DOWN')
test.direction = ifelse((dailyReturn(test) > 0),'UP','DOWN')
direction = ifelse((dailyReturn(WMT) > 0),'UP','DOWN')
# Past 10 days lagged return vectors
train.lags = Lag(dailyReturn(train),k=1:10);
test.lags = Lag(dailyReturn(test),k=1:10);
lags=Lag(dailyReturn(WMT),k=1:10);
# Bind the data by column
train.df = cbind.data.frame(train.volume,train.lags,train.direction);
test.df = cbind.data.frame(test.volume,test.lags,test.direction);
WMT.df=cbind.data.frame(volume,lags,direction);
# Rename categorical column
colnames(train.df)[12] = 'Direction';
colnames(test.df)[12] = 'Direction';
colnames(WMT.df)[12] = "Direction";
# Remove NA observations
train.df = na.omit(train.df);
test.df = na.omit(test.df);
WMT.df = na.omit(WMT.df);
# Build LDA(test)
set.seed(222)
lda.fit = lda(Direction ~ ., data=train.df)
lda.fit
# Plot LDA histogram
fitted.plot = ggplotLDAPrep(lda.fit); title.gg = paste("WMT","LDA");
print(ggplot(fitted.plot, aes(LD1,..density..,fill=labels))+geom_histogram() 
      + ggtitle(title.gg))
# Predict using test set and calculate error rate
lda.pred = predict(lda.fit,test.df)
lda.class=lda.pred$class 
lda.table=table(lda.class,test.df$Direction)
lda.table
lda.error=mean(lda.class!=test.df$Direction)
lda.error
set.seed(222)
lda.mod = lda(Direction ~ ., data=WMT.df)
lda.mod
lda.fullpreds = predict(lda.mod,WMT.df)
str(lda.fullpreds)
# Pull posterior probabilities to use for trade rules using post prob threshold
post.down = lda.fullpreds$posterior[,'DOWN']
# Long: posterior down <= 0.6
# Short: posterior down > 0.6
WMT <- merge(WMT, Position=as.xts(ifelse(post.down <= 0.6, 1, -1),
                                  dateFormat="Date"))
WMT=na.omit(WMT)
# Build return vector and plot returns
my.ret <- lag(WMT$Position) * dailyReturn(WMT)
# Performance Statistics
rbind(SharpeRatio.annualized(my.ret$Position, scale = 252), 
      Return.annualized(my.ret$Position,scale=252), 
      Return.cumulative(my.ret$Position, geometric=T))
# Plot daily returns, positions (Cum Return, Daily Return, Drawdown)
charts.PerformanceSummary(cbind(dailyReturn(WMT),my.ret))
chart.RiskReturnScatter(my.ret)
# Performance Evals
performance.transactions(WMT, WMT$Position, cum.return = my.ret)
# Multiplier
lda.fullclass = lda.fullpreds$class
perc.Up = sum(lda.fullclass == 'UP') / sum(WMT.df$Direction == 'UP');
perc.Down = sum(lda.fullclass == 'DOWN') / sum(WMT.df$Direction == 'DOWN');
multiplier = ifelse(perc.Up > perc.Down, (perc.Up / perc.Down), (perc.Down / perc.Up))
multiplier
# Build Multi Strats
WMT <- merge(WMT, MultiLong=as.xts(ifelse(post.down <= 0.6, 1*multiplier, -1),
                                   dateFormat="Date"))
WMT <- merge(WMT, MultiShort=as.xts(ifelse(post.down <= 0.6, 1, -1*multiplier),
                                    dateFormat="Date"))
WMT <- merge(WMT, DualMulti=as.xts(ifelse(post.down <= 0.6, 1*multiplier, -1*multiplier),
                                   dateFormat="Date"))
WMT=na.omit(WMT)
# Build return vectors and plot returns
ml.ret = lag(WMT$MultiLong) * dailyReturn(WMT)
ms.ret = lag(WMT$MultiShort) * dailyReturn(WMT)
dm.ret = lag(WMT$DualMulti) * dailyReturn(WMT)
# Multi Performance Evals
performance.transactions(WMT, WMT$Position, cum.return = my.ret)
performance.transactions(WMT, WMT$MultiLong, cum.return = ml.ret)
performance.transactions(WMT, WMT$MultiShort, cum.return = ms.ret)
performance.transactions(WMT, WMT$DualMulti, cum.return = dm.ret)
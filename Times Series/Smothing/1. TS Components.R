library(fpp)
data("a10")

plot(a10,main="Anti-diabetic drug sales in Australia")

lines(ma(a10,order = 3),col="red")

ma(a10,order = 3)

par(mfrow=c(2,2))

plot(a10,main="Span : 3")
lines(ma(a10,order = 3),col="red")

plot(a10,main="Span : 4")
lines(ma(a10,order = 4),col="green")

plot(a10,main="Span : 5")
lines(ma(a10,order = 5),col="brown")

plot(a10,main="Span : 6")
lines(ma(a10,order = 6),col="blue")


forecast(ma(a10,order = 3),h=4)

##### Additive Decomposition ####
fit <- decompose(a10, type="additive")
plot(fit)

##### Multiplicative Decomposition ####
fit <- decompose(a10, type="multiplicative")
plot(fit)




### Centered MA ###
s <- c(13,24,25,27,34,37,45,48,58,69,70)
s_ts <- ts(s)

ma(s_ts,order = 4,centre = FALSE)
ma(s_ts,order = 4,centre = TRUE)



consump <- read.csv("F:\\Statistics\\Datasets\\Individual_Consumption.csv")
consump_ts <- ts(consump$Ind_Consump,start=c(1999,1), frequency = 4)


BUNDESBANK <- read.csv("F:\\Statistics\\Datasets\\BUNDESBANK-BBK01_WT5511.csv")
BUNDESBANK_ts <- ts(BUNDESBANK$Value,start=c(1968,4), frequency = 12)



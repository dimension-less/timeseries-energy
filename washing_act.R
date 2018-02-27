mysqlconnection = dbConnect(MySQL(), user='himanshu', password='himanshu', 
                            host='127.0.0.1',port=3306,dbname="said")


dbListTables(mysqlconnection)

xyz = dbGetQuery(mysqlconnection,"select *  from ApparatType")
View(xyz)
rs = dbSendQuery(mysqlconnection, " SELECT * FROM said.ApparatType")

data= fetch(rs,n=-1)

xyz = dbGetQuery(mysqlconnection,"select *  from PCBoks limit 100")

logwatt = dbGetQuery(mysqlconnection,"select * from LogWatt limit 100")
logwatt

unit = dbGetQuery(mysqlconnection,"select * from Unit limit 200")


system("sudo su ")

data <- dbGetQuery()


washing = dbGetQuery(mysqlconnection,"select * from energy where apparatTypeID = 3 and PCboksID=54")

washing_house1<- dbGetQuery(mysqlconnection,"select * from energy where apparatTypeID = 3 and PCboksID=1")
write.csv(x = washing,file = 'washing.csv')

summary(washing)

washing$watt2 <- as.numeric(washing$watt)
summary(washing$watt2)

as.double(3.05)
as.numeric(3.02)
as.numeric('3.02')

?ts
ts(1:700,frequency = 365,start=c(1959,1))

ts(1:700,frequency = 24*60/10,start =c(2013,10,1))
time_series = ts(washing$watt,start = washing$date[1],end = washing$date[637444])

class(washing$date)

as.date(washing$date[1:5])

as.Date(washing$date[1:5])

ts(1:100,start = c(1959,2),frequency = 365*24*60/2)

washing$dd = as.Date(washing$date)

ts(1:10,start='2014-01-01',frequency = 365*24*30)
365*24*30

summary(washing$dd)

plot.ts(washing$watt2[500:1500])


washing$time  <- as.Date.(washing$date)

?as.zoo
ts =  zoo(washing$watt2,order.by = washing$time)
head(ts)
plot.zoo(ts[1:1000])
as.Date('2014-11-07 10:26:10')
strptime('2014-11-07 10:26:10')
strptime(x = '2014-11-07 10:26:10',format = '%Y')

a = as.POSIXct('2017-01-01 10:26')
b = as.POSIXct('2017-01-01 10:28')
washing$time = as.POSIXct(washing$date)
washing$dd<- NULL

plot(ts[500:50000])

Date('2017-01-01')


plot.ts(ts[0:1000])

plot.zoo(ts[1000:10000])

is.regular(ts,strict = T)

frequency(ts)

ts10 =  zoo(washing$watt2,order.by = washing$time,frequency = 1)
head(ts10)


system("sudo yum install gfortran")


system("locate lgfortran.so")

ts <- as.xts(ts)

head(ts)

plot.zoo(ts)

plot.xts(ts)

class(ts)
plot(ts>1000)
ts>1000
class(index(ts))


ts <- xts(washing$watt2,as.POSIXct(washing$date))

head(ts)
plot.xts(ts)
View(ts[1:1000])
plot.xts(ts[800:1000])

plot.xts(ts['2014-11-07/2014-11-10'])

ts_days <- to.period(ts,period='days')

head(ts_days)
View(ts_days)
index(ts_days)<-as.Date(index(ts_days))

plot.xts(ts_days[,2]['2014-11'],major.ticks = T)



nrow(ts_days)

View(ts_days)

head(washing_house1)

washing_house1$watt<- as.numeric(washing_house1$watt)
washing_house1$date<- as.POSIXct(washing_house1$date)
ts_house1<- xts(x = washing_house1$watt,order.by = washing_house1$date)

plot.xts(ts_house1)

summary(ts_house1)
quantile.zoo(ts_house1)

summary(ts_house1[ts_house1==4195992060])


# House 2 

washing_house2<- dbGetQuery(mysqlconnection,"select * from energy where apparatTypeID = 3 and PCboksID=2")
summary(washing_house2)
washing_house2$watt2<- as.numeric(washing_house2$watt)
washing_house2$time <- as.POSIXct(washing_house2$date)

ts_house2<-xts(x = washing_house2$watt2,order.by = washing_house2$time)

plot.xts(ts_house2)

ts_house2_hrs <- to_period(ts_house2,period='hours')
head(ts_house2_hrs)
plot.xts(ts_house2_hrs[,2])

ts_house2_days<- to_period(ts_house2,period='days')

plot.xts(ts_house2_days[,2]['2014-12/2015-06'],major.ticks = T)

plot.xts(ts_house2_days[,2]['2014-12-01/2015-03-31'],major.ticks = T)

View(ts_house2_days)

# For TV 

tv_house1<- dbGetQuery(mysqlconnection,"select * from energy where apparatTypeID = 1 and PCboksID=1")

tv_house1$watt <- as.numeric(tv_house1$watt)

tv_house1$date<-as.POSIXct(tv_house1$date)

ts_tv_house1<-xts(x = tv_house1$watt,order.by =tv_house1$date)
head(ts_tv_house1)

plot.xts(ts_tv_house1['2014-11-30 08:00/2014-11-30 08:30'],major.ticks = T)
plot.xts(first(ts_tv_house1,"10 day"),major.ticks = T,minor.ticks=T)

ts_tv_house1_hrs = to.period(ts_tv_house1,period = 'hours')
length(ts_tv_house1_hrs)

ts_tv_house1_days = to.period(ts_tv_house1,period = 'days')
length(ts_tv_house1_days)

plot.xts(ts_tv_house1_days[,2])

plot.xts(ts_tv_house1_hrs[,2])


house1<- dbGetQuery(mysqlconnection,"select * from energy where PCboksID=1 limit 1000")
View(house1)

# Refrigerator 

fridge_house2<- dbGetQuery(mysqlconnection,"select * from energy where apparatTypeID = 5 and PCboksID=2")
fridge_house2$watt <- as.numeric(fridge_house2$watt)
fridge_house2$date <- as.POSIXct(fridge_house2$date)

ts_fridge_house2<- xts(fridge_house2$watt,fridge_house2$date)

plot.xts(ts_fridge_house2[1:1000])

plot.xts(first(ts_fridge_house2,'9 hour'),major.ticks = T)
axis(side = 2,at = seq(0,1000,100))

# Refrigerator house 15 

fridge_house15<- dbGetQuery(mysqlconnection,"select * from energy where apparatTypeID = 5 and PCboksID=15")
fridge_house15$watt <- as.numeric(fridge_house15$watt)
fridge_house15$date <- as.POSIXct(fridge_house15$date)

ts_fridge_house15<- xts(fridge_house15$watt,fridge_house15$date)

plot.xts(ts_fridge_house15[1:1000])

View(ts_fridge_house15[1:1000])

plot.xts(first(ts_fridge_house2,'9 hour'),major.ticks = T)


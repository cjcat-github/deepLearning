
##########高级绘图函数：plot()########################
setwd("E://上课//数据可视化//data/mydata//")
data2_1.df<-read.csv(".//chap02//data2_1.csv")
mytable<-table(data2_1.df$性别)
mytable #一维列联表


#--只有一个变量的情况----
windows()
par(mfrow=c(2,2))
attach(data2_1.df)
plot(性别,xlab="性别",ylab="频数",main="条形图",ylim=c(0,20)) #类别变量
#plot(一维列联表)，画不出条形图
plot(mytable,xlab="性别",ylab="频数",main="条形图2",ylim=c(0,20))
plot(R,xlab="元素序号",ylab="R分数",main="散点图",type="p")#point数值变量
detach()

#--有两个变量,两个变量都是数值型变量-----
windows()
par(mfrow=c(2,2))
plot(data2_1.df$R,data2_1.df$Python,xlab="R的分数",ylab="Python分数",type="b") # 数值 vs 数值 #(x,y)
plot(Python~R,data=data2_1.df,xlab="R的分数",ylab="Python分数") #formula：Y~x
plot(R~Python,data=data2_1.df,xlab="Python的分数",ylab="R的分数")
#plot(R,Python,data=data2_1.df,xlab="R的分数",ylab="Python分数")


#--有两个变量，一个因子(x/y),一个数值(y,x) -----
windows()
par(mfrow=c(2,2))
attach(data2_1.df)
plot(R~性别,xlab="性别",ylab="R分数",main="数值vs因子") #箱线图 formula
plot(Python~R,xlab="R分数",ylab="Python分数",main="数值vs数值") 
plot(性别~R,xlab="R分数",ylab="性别",main="因子vs数值") # 脊形图
detach()

#--若两个变量都是因子-----------------
data("mtcars") # mtcars:R自带的df
mtcars
mtcars$cyl<-as.factor(mtcars$cyl)
mtcars$gear<-as.factor(mtcars$gear)
windows()
plot(cyl~gear,data=mtcars,xlab="gear",ylab="cyl") #马赛克图formula


#---关于plot函数的参数(坐标轴)--------------
windows()
par(mfrow=c(2,2))
plot(R~Python,data=data2_1.df,xlab="Python分数",
     ylab="R分数",xlim=c(50,70),ylim=c(60,100),
     main="R和Python之间的关系_1",
     type="o",col="blue",lwd=1,pch=23)
plot(R~Python,data=data2_1.df,type="b",xlab="Python分数",
     ylab="R分数",xlim=c(50,70),ylim=c(60,100),
     main="R和Python之间的关系_2",
     ann=FALSE)
plot(R~Python,data=data2_1.df,type="b",xlab="Python分数",
     ylab="R分数",xlim=c(50,70),ylim=c(60,100),
     main="R和Python之间的关系_3",
     axes=FALSE)
plot(R~Python,data=data2_1.df,type="b",xlab="Python分数",
     ylab="R分数",xlim=c(50,70),ylim=c(60,100),
     main="R和Python之间的关系_4",
     xaxt="n")

#---关于plot函数的参数(线型，颜色)------------
#--lty,pch,lwd,col,cex,type-------------------
windows()
plot(R~Python,data=data2_1.df,type="l",xlab="Python分数",
     ylab="R分数",xlim=c(50,85),ylim=c(60,100),
     main="R和Python之间的关系_5",
     lty=3,pch=20,lwd=3,col="red",cex.main=1,cex.lab=1)
# 《R实战》


################低级绘图函数#########################
#---abline()的使用------------------------
#---添加直线--------
windows()
par(mfrow=c(2,2))
plot(1:5, 1:5, xlim = c(0,6), ylim = c (0,6))
abline(h = 0, col = "red")   #添加水平直线
plot(1:5, 1:5, xlim = c(0,6), ylim = c (0,6))
abline(v = 0, col = "blue",lwd=3)  #添加垂直直线
#同时添加水平和垂直直线
plot(1:5, 1:5, xlim = c(0,6), ylim = c (0,6))
abline(h = c(0,1,2), v = c(0,1,2), 
       col = c("red", "green", "blue"))
#---添加斜线--------
windows()
plot(1:5, 1:5, xlim = c(0,6), ylim = c (0,6))
abline(h = 0, col = "gray")
abline(v = 0, col = "gray")
abline(a = 1 , b = 1, col = "red",lwd=2,lty=2)
abline(a = 2 ,b = 1, col="blue",lwd=3,lty=4)

#---abline应用在回归模型----
data(cars) #mtcars
fit<- lm(dist ~ speed, data = cars)
windows()
par(mfrow=c(1,2))
plot(cars)
abline(fit,col="red")
plot(cars)
abline(a=fit$coefficients[1],b=fit$coefficients[2],
       col="blue",lwd=3)

#---lines()的使用--------------
#---例子1-------
set.seed(0108)
nn<- 100
x<- sort(runif(nn))
eps <- rnorm(nn, mean = 0, sd = 1L)

#Simulated data and True data.
TrueY <- 1+x/2+4*x^2  #x=0,TrueY=1, simuY = 1+0.0001 = 1.00001
# = 1-0.0001 = 0.99999
SimuY <- 1 + x/2 + 4*x^2 + eps

windows()
plot(SimuY~x, type="p",main="Plot of SimuY vs x", cex.main = 1.25)
lines(x, TrueY, lty ="solid", lwd = 4, col = "red")

#--例子2--------
t<-rep(1:8) # 1~8点
v1<-c(245,356,210,189,345,234,191,452) #v1:非工作日1~8点访问量
v2<-c(345,451,541,548,600,720,650,560) #v2:工作日1~8点访问量
windows()
plot(c(1,8),c(100,750),type="n",xlab="时间",ylab="访问次数")
abline(h=seq(100,750,by=20),v=0:8,col="lightgray",lty=3)
lines(t,v1,col="red")
lines(t,v2,col="blue")


#--自定义坐标轴的示例(《R实战》p52)----
x<-c(1:10)
y<-x
z<-10/x
windows()
opar<-par(no.readonly = TRUE)
par(mar=c(5,4,4,8)+0.1)
plot(x,y,type="b",pch=21,col="red",
     yaxt="n",lty=3,ann=FALSE) 
#yaxt是否显示y轴刻度，ann是否显示x轴和y轴的标签
lines(x,z,type="b",pch=22,col="blue",lty=2)
axis(2,at=x,labels=x,col.axis="red",las=2)
axis(4,at=z,labels=round(z,digits=2),
     col.axis="blue",las=2,cex.axis=0.7,tck=-.01)
mtext("y=1/x",side = 4,line=1,las=2,col="blue")
title("An Example of Creative Axes",
      xlab="X values", ylab="Y=X")
par(opar)

#---legend()的示例(《R实战》p80)-----
dose<-c(20,30,40,45,60)
drugA<-c(16,20,27,40,60)
drugB<-c(15,18,25,31,40)
windows()
opar<-par(no.readonly = TRUE)
par(lwd=2,cex=1.5,font.lab=2)
plot(dose,drugA,type="b",pch=15,lty=1,col="red",
     ylim=c(0,60),main="DrugA vs. DrugB",
     xlab="Drug Dosage",ylab="Drug Response")
lines(dose,drugB,type="b",pch=17,lty=2,col="blue")
abline(h=c(30),lwd=1.5,lty=2,col="gray")
legend("top",title="Drug Type",legend=c("A","B"),
       lty=c(1,2),pch=c(15,17),col=c("red","blue"))
par(opar)

#--text()《R实战》---------------
data(mtcars)
attach(mtcars)
windows()
plot(wt,mpg,main="Mileage vs. Car Weight",type="p",
     xlab="Weight",ylab="Mileage",pch=18,col="blue")
text(wt,mpg,rownames(mtcars),cex=1.0,pos=3,col="red")
detach(mtcars)
opar<-par(no.readonly = TRUE)


#--par()函数--------------
set.seed(2015)
xx <- rnorm(100)
windows()
plot(xx)  # ①
# 保存原有的设置
windows()
opar <- par(no.readonly=TRUE) 
# 增加新的作图设置
par(lty=2, pch=17)           
plot(xx) # ②
par(opar)  # 复原设置 
windows()
plot(xx)  # ③
# ①与③的图是一样的 


#--关于颜色---------------
x<-1:10
a<-c("A","B","C","D","E","F","G","H","I","J")
windows()
par(mfrow=c(1,2),mai=c(0.6,0.8,0.2,0.2),
    cex.axis=0.7,cex.lab=1.2,cex.main=1.5)
barplot(x,names=a,col=c("red","green"),
        main="循环使用2种颜色",ylab="YYY")
barplot(x,names=a,col=1:9,
        main="重复使用颜色1:9",ylab="ZZZ")



# 关于rainbow()函数-------
windows()
par(mfrow=c(1,2))
n<-10
mycolors<-rainbow(n)
pie(rep(1,n),labels = mycolors, col=mycolors)

#--关于图形的尺寸和边界---
windows()
opar<-par(no.readonly = TRUE)
par(mfrow=c(1,2))
par(pin=c(4,3))
par(mai=c(1,0.6,1,.2))
par(lwd=2,cex=0.5)
plot(dose,drugA,type="b",pch=19,lty=2,col="red")
plot(dose,drugB,type="b",pch=23,lty=6,col="blue",bg="green")
par(opar)

#--关于图形的布局:mfrow=c(2,2)---
data("mtcars")
windows()
attach(mtcars)
opar<-par(no.readonly = TRUE)
par(mfrow=c(2,2))
plot(wt,mpg,main="Scatterplot of wt vs. mpg")
plot(wt,disp,main="Scatterplot of wt vs. disp")
hist(wt,main="Histogram of wt")
boxplot(wt,main="Boxplot of wt")
par(opar)
detach(mtcars)

#--关于图形的布局:layout()---
attach(mtcars)
windows()
layout(matrix(c(1,1,2,3),nrow=2,ncol=2,byrow=TRUE))
hist(wt)
hist(mpg)
hist(disp)
detach(mtcars)

windows()
layout(matrix(c(1,1,1,2,3,4),nrow=2,ncol=3,byrow=TRUE),
       widths=c(4,2,1),heights=c(2,1))
layout.show(4)

#--第一个综合练习-----------
# seq(from,to,step),
x=seq(-4*pi,4*pi,0.01)
y=sin(x)
z=cos(x)
windows()
par(mai=c(0.5,1,0.2,0.5))
plot(x,y,main="三角函数曲线",xlab="X",ylab="y(sin/cos)",
     xlim=c(-10,10),ylim=c(-1,3),
     col="red",type="l",lwd=1,lty=1,yaxt="n")
lines(x,z,col="green",lwd=2,lty=2)
mtext("sin(x)",side=2,line=3,las=2,col="black")
mtext("cos(x)",side=4,las=2,col="black")
axis(2,at=seq(-1,3,0.25),col.axis="red",las=2)
#axis(4,at=seq(-1,3,0.25),col.axis="green",las=2)
legend("topleft",title="sin vs. cos",legend=c("sin(x)","cos(x)"),
       lty=c(1,2),col=c("red","green"),cex=1.5)
abline(h=c(-0.5,0.5,1),v=c(-2*pi,2*pi),col="gray")

#--第二个综合练习-----------
data("mtcars")
mtcars$cyl<-factor(mtcars$cyl)
windows()
layout(matrix(c(1,1,2,3),nrow=2,ncol=2,byrow=TRUE),
       widths = c(2,1),heights = c(2,1))
opar<-par(no.readonly=TRUE)
par(pch=17,lty=2,col="red",cex.main=2.0)
plot(mpg~hp,data=mtcars,xlab="HP",ylab="MPG",
     main = "mpg vs. hp")
#par(opar)
#opar<-par(pch=20,lty=1,col="blue",cex=0.7)
boxplot(mpg~cyl,data = mtcars,xlab="CYL",ylab="MPG",
        main="mpg vs. cyl")

hist(mtcars$wt,col="green",xlab="WT",ylab="Density",
     main = "Density of WT")
par(opar)

#教材：p52,图2-14
#武汉非典疫情的数据，作图# 第一次实验报告



#######各种拟合###############################
data("cars")
windows()
plot(cars,pch=19)
model1=loess(dist~speed,data=cars,span=0.4)
lines(cars$speed,model1$fit,col='red',lty=2,lwd=2)
model2=loess(dist~speed,data=cars,span=0.8)
lines(cars$speed,model2$fit,col='blue',lty=2,lwd=2)
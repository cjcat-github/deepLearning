#---R的基本使用实例----------------
setwd("E://上课//数据可视化//data//mydata")
cherry.df<-read.csv(".//chap01//cherry.txt",sep="")
summary(cherry.df)

windows()
par(mfrow=c(2,2))
plot(cherry.df$volume~cherry.df$diameter)  #formula
plot(cherry.df$diameter,cherry.df$volume)
#plot(diameter,volume,data=cherry.df)
plot(volume~diameter,data=cherry.df)  #formula

install.packages("rgl")
library("rgl")
plot3d(cherry.df)





#---R的数据类型--------------------
#---R的向量-----
aa<-c(1,2,5,3,10)
bb<-c("Tom","Jone","York")
cc<-c(TRUE,FALSE,TRUE,TRUE)
class(aa)
class(cc)
class(bb)
aa[2]
aa[c(2,5)]
aa[2:4]


#---R的矩阵: 二维数组
a<-c(1:12)
mat<- matrix(a,nrow=3,ncol=4,byrow=TRUE )
mat
mat[1,2]
mat[1:3,c(1,3)]

#---R的数组: 3, 4,5维 -----------
data<-round(runif(24,50,100)) #按照均匀分布生成随机数，24个
data
dim1<-c("男","女")
dim2<-c("赞成","中立","反对")
dim3<-c("东部","西部","南部","北部")
d<-array(data,c(2,3,4),dimnames=list(dim1,dim2,dim3))

#--R的数据框----------
#--直接创建数据框----
names<-c("刘文涛","王宇翔","田思雨","徐丽娜","丁文斌")
stat<-c(68,85,74,88,63)
math<-c(85,91,74,100,82)
econ<-c(84,63,61,49,89)
table1.df<-data.frame(names,stat,math,econ)
table1.df.df<-data.frame(学生姓名=names,统计学=stat,数学=math,经济学=econ)


head(cherry.df,3)
tail(cherry.df)

summary(cherry.df)
str(table1.df)

class(table1.df)
class(table1.df$names)
class(table1.df$stat)
nrow(table1.df)
ncol(table1.df)
dim(table1.df)

sort(table1.df$names)
sort(table1.df$names,decreasing = TRUE)
df.df<-table1.df[order(table1.df$names), ]

names<-c("李志国","王智强","宋丽媛","袁芳芳","张建国")
stat<-c(78,90,80,58,63)
math<-c(84,78,100,51,70)
econ<-c(51,59,53,79,91)
table2.df<-data.frame(names,stat,math,econ)

mytable<-rbind(table1.df,table2.df)  #rbind(合并行)

#cbind(合并列)
names<-c("刘文涛","王宇翔","田思雨","徐丽娜","丁文彬","李志国","王智强","宋丽媛","芳芳","张建国")
jingrong<-c(89,76,80,71,78,60,72,73,91,85)
kuaiji<-c(86,66,69,66,80,60,66,70,85,82)
table3.df<-data.frame(names,jingrong,kuaiji)

yourtable<-cbind(mytable,table3.df[,2:3])  

#--R的因子-----
a<-c("金融","地产","医药","医药","金融","医药") #字符型vector
f<-factor(a)
nu<-as.numeric(f)
nu
f
class(a)
class(f)
class(nu)

b<-c("很好","好","一般","差","很差") #字符型vector
f<-factor(b)
nuu<-as.numeric(f)
nuu

fb<-factor(b,order=TRUE,levels=c("很好","好","一般","差","很差"))
fb
nu<-as.numeric(fb)
nu

# 对mtcars数据，gear,cyl,vs转一下，转成factor

#---数据类型转换-------
vector1<-as.vector(table1.df$stat)
vector1
vector2<-as.vector(table1.df[,2:3])
vector2
vector3<-as.vector(as.matrix(table1.df[,2:3]))
vector3

# f<-as.factor(a)

matrix1_1<-as.matrix(table1.df[,2:4])
matrix1_1
rownames(matrix1_1)=table1.df[,1]
colnames(matrix1_1)=c("统计学","数学","经济学")
matrix1_1

rownames(table1.df) = c("one","two","three","four","five")
colnames(table1.df) = c("学生姓名","统计学","数学","经济学")


#--将短格式数据转换为长格式数据
############################################

#--生成随机数------------------
rnorm(1000,mean=0,sd=1)


set.seed(12345)
runif(10,min = 10,max=50)
set.seed(12345)
round(runif(10,min = 10,max=50),1)



#--数据抽样--------------------
N<-1:20
n1<-sample(N,size=10)
n1
n2<-sample(N,size=40,replace=TRUE)
n2

#--1.4生成频数分布表---------
#--类别数据的频数分布表------
setwd("E://上课//数据可视化//data//mydata//")

data1.df<-read.csv(".//chap01//data1_1.csv")
str(data1.df)

#--一维列联表------
mytable1<-table(data1.df$性别)
mytable1
prop.table(mytable1)

#--二维列联表------
mytable2<-table(data1.df$性别,data1.df$满意度)
mytable2
addmargins(mytable2)
prop.table(mytable2)
addmargins(prop.table(mytable2))

#--多维列联表-----
install.packages("stats")
library(stats)
mytable3<-ftable(data1.df,row.vars = c("性别","网购次数"), col.vars="满意度")
mytable3
prop.table(mytable3)
mytable4<-ftable(data1.df,row.vars=c("网购次数"),col.vars=c("性别","满意度"))
mytable4

#--数值数据作成频数分布表
data2.df<-read.csv(".//chap01//data1_2.csv")
d<-table(cut(data2.df$销售额,breaks=7,right=FALSE,dig.lab=4))
d
dd<-data.frame(d)
percent<-round(dd$Freq/sum(dd$Freq)*100,2)
df<-data.frame(dd,percent)
colnames(df)<-c("区间","频数","频率")

d<-table(cut(data2.df$销售额,breaks = c(500,550,600,650,700,750),right=FALSE))
dd<-data.frame(d)
percent<-round(dd$Freq/sum(dd$Freq)*100,2)
dd$percent<-percent
dd
colnames(dd) <- c("区间","频数","比例")
dd

#--编写R函数-----------
mf<-function(x,y){
  plot(x,y,pch=21,xlab="自变量",ylab="因变量",
       main=paste("r=",round(cor(x,y),4),"的散点图"))
  abline(lm(y~x),col="red")
}

windows()
par(mfcol=c(1,2),mai=c(0.6,0.6,0.2,0.2),cex=0.7)
n<-200
set.seed(12345)
x1<-rnorm(n);y1<-1+4*x1+rnorm(n)
x2<-rnorm(n);y2<-(-2)-3*x2-rnorm(n)
mf(x1,y1)
mf(x2,y2)
#--练习写一个logit函数--
logit<-function(x){
  return(log10(x/(1-x)))
}
set.seed(12345)
v<-runif(10)
v
logit(v)
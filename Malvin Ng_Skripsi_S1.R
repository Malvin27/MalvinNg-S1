############
#Reserve
rm(list=ls())
library(CASdatasets)
data(usmedclaim)

#premi per member
premi=16.2
inflasi=c(1.042,1.047,1.05)
premi_1=rep(16.2*1.042, times=12)
premi_2=rep(16.2*1.042*1.047, times=12)
premi_3=rep(16.2*1.042*1.047*1.05, times=12)
premi_total=c(premi_1,premi_2,premi_3)

#memasukkan premi ke tabel
usmedclaim=cbind(usmedclaim,premi_total)
usmedclaim$premi_member=usmedclaim$members*usmedclaim$premi_total

#Perhitungan GB, WN, OB
origin_period=c()
for (i in 1:(sum(is.na(usmedclaim$DY13))+1)) {
  A = rep(i, 14 - i)
  origin_period = c(origin_period, A)
}

development_period=c()
for (i in 1:sum(origin_period==1)) {
  A = 1:i
  development_period=c(A,development_period)
}

paid_claims = data.frame(usmedclaim[24,(3:15)],usmedclaim[25,(3:14)],usmedclaim[26,(3:13)],
                         usmedclaim[27,(3:12)],usmedclaim[28,(3:11)],usmedclaim[29,(3:10)],
                         usmedclaim[30,(3:9)],usmedclaim[31,(3:8)],usmedclaim[32,(3:7)],
                         usmedclaim[33,(3:6)],usmedclaim[34,(3:5)],usmedclaim[35,(3:4)],usmedclaim[36,(3:3)])
paid_claims=t(paid_claims)
paid_claims=data.frame(paid_claims)
colnames(paid_claims)="paid_claims"

exposure=c(rep(usmedclaim[24,17],times=13),rep(usmedclaim[25,17],times=12),
           rep(usmedclaim[26,17],times=11),rep(usmedclaim[27,17],times=10),
           rep(usmedclaim[28,17],times=9),rep(usmedclaim[29,17],times=8),
           rep(usmedclaim[30,17],times=7),rep(usmedclaim[31,17],times=6),
           rep(usmedclaim[32,17],times=5),rep(usmedclaim[33,17],times=4),
           rep(usmedclaim[34,17],times=3),rep(usmedclaim[35,17],times=2),
           rep(usmedclaim[36,17],times=1))
library(dplyr)

#Membuat tabel
data_usmed_1=data.frame(origin_period,development_period,paid_claims,exposure)

#Membuat m_k dan m
data_usmed_2=data_usmed_1 %>%
  group_by(development_period) %>%
  summarise(m_k = sum(paid_claims) / sum(exposure))

m=sum(data_usmed_2$m_k)

data_usmed_4=merge(data_usmed_1,data_usmed_2,by="development_period")

#Membuat Burning cost Paid-to-Date, p_i, dan q_i
data_usmed_5=data_usmed_4 %>%
  group_by(origin_period)%>%
  summarise(expected_burning_cost=max(exposure)*m,
            cumulative_paid_claims=sum(paid_claims),
            p_i=sum(m_k)/m,
            q_i=1-p_i)

#Individual Reserve and Ultimate
individual_ultimate=data_usmed_5$cumulative_paid_claims/data_usmed_5$p_i
individual_reserve=individual_ultimate-data_usmed_5$cumulative_paid_claims
data_usmed_6=data.frame(origin_period=data_usmed_5$origin_period,cumulative_paid_claims=data_usmed_5$cumulative_paid_claims,individual_ultimate,individual_reserve)

#Collective Reserve and Ultimate
collective_reserve=data_usmed_5$q_i*data_usmed_5$expected_burning_cost
collective_ultimate=data_usmed_5$cumulative_paid_claims+collective_reserve
data_usmed_7=data.frame(origin_period=data_usmed_5$origin_period,collective_reserve,collective_ultimate)

#Benktander Reserve and Ultimate
GB_reserve=data_usmed_5$p_i*data_usmed_6$individual_reserve+data_usmed_5$q_i*data_usmed_7$collective_reserve
GB_ultimate=GB_reserve+data_usmed_5$cumulative_paid_claims
data_usmed_8=data.frame(origin_period=data_usmed_5$origin_period,GB_reserve,GB_ultimate)

#Neuhaus Reserve and Ultimate
neuhaus_reserve=data_usmed_5$p_i*m*data_usmed_6$individual_reserve+(1-data_usmed_5$p_i*m)*data_usmed_7$collective_reserve
neuhaus_ultimate=neuhaus_reserve+data_usmed_5$cumulative_paid_claims
data_usmed_9=data.frame(origin_period=data_usmed_5$origin_period,neuhaus_reserve,neuhaus_ultimate)

#Optimal Benktander Reserve and Ultimate
t_i=sqrt(data_usmed_5$p_i)
#z_i=model_5$p_i/(model_5$p_i+sqrt(model_5$p_i))
#atau bisa juga 
z_i=data_usmed_5$p_i/(data_usmed_5$p_i+t_i)
optimal_reserve=z_i*data_usmed_6$individual_reserve+(1-z_i)*data_usmed_7$collective_reserve
optimal_ultimate=optimal_reserve+data_usmed_5$cumulative_paid_claims
data_usmed_10=data.frame(origin_period=data_usmed_5$origin_period,optimal_reserve,optimal_ultimate)

#Cek Z_i Neuhaus
z_i_wn=data_usmed_5$p_i*m

#Ratemaking
premi_ratemaking=usmedclaim[24:36,17]
members_ratemaking=usmedclaim[24:36,1]
#Individu
#Pure Premium
PP_1=data_usmed_6$individual_ultimate/(1-0.435)/13/members_ratemaking
#Loss Ratio
LR_1=data_usmed_6$individual_ultimate/13/premi_ratemaking/(1-0.435)-1

#Kolektif
#Pure Premium
PP_2=data_usmed_7$collective_ultimate/(1-0.435)/13/members_ratemaking
#Loss Ratio
LR_2=data_usmed_7$collective_ultimate/13/premi_ratemaking/(1-0.435)-1

#Benktander
#Pure Premium
PP_3=data_usmed_8$GB_ultimate/(1-0.435)/13/members_ratemaking
#Loss Ratio
LR_3=data_usmed_8$GB_ultimate/13/premi_ratemaking/(1-0.435)-1

#Neuhaus
#Pure Premium
PP_4=data_usmed_9$neuhaus_ultimate/(1-0.435)/13/members_ratemaking
#Loss Ratio
LR_4=data_usmed_9$neuhaus_ultimate/13/premi_ratemaking/(1-0.435)-1

#Optimal Benktander
#Pure Premium
PP_5=data_usmed_10$optimal_ultimate/(1-0.435)/13/members_ratemaking
#Loss Ratio
LR_5=data_usmed_10$optimal_ultimate/13/premi_ratemaking/(1-0.435)-1

############
#Perbandingan MSE
rm(list=ls())
library(CASdatasets)
data(usmedclaim)

#premi per member
premi=16.2
inflasi=c(1.042,1.047,1.05)
premi_1=rep(16.2*1.042, times=12)
premi_2=rep(16.2*1.042*1.047, times=12)
premi_3=rep(16.2*1.042*1.047*1.05, times=12)
premi_total=c(premi_1,premi_2,premi_3)

#memasukkan premi ke tabel
usmedclaim=cbind(usmedclaim,premi_total)
usmedclaim$premi_member=usmedclaim$members*usmedclaim$premi_total
origin_period=c()
for (i in 1:(sum(is.na(usmedclaim$DY13))+1)) {
  A = rep(i, 14 - i)
  origin_period = c(origin_period, A)
}

development_period=c()
for (i in 1:sum(origin_period==1)) {
  A = 1:i
  development_period=c(A,development_period)
}

paid_claims = data.frame(usmedclaim[1,(3:15)],usmedclaim[2,(3:14)],usmedclaim[3,(3:13)],
                         usmedclaim[4,(3:12)],usmedclaim[5,(3:11)],usmedclaim[6,(3:10)],
                         usmedclaim[7,(3:9)],usmedclaim[8,(3:8)],usmedclaim[9,(3:7)],
                         usmedclaim[10,(3:6)],usmedclaim[11,(3:5)],usmedclaim[12,(3:4)],usmedclaim[13,(3:3)])
paid_claims=t(paid_claims)
paid_claims=data.frame(paid_claims)
colnames(paid_claims)="paid_claims"

exposure=c(rep(usmedclaim[1,17],times=13),rep(usmedclaim[2,17],times=12),
           rep(usmedclaim[3,17],times=11),rep(usmedclaim[4,17],times=10),
           rep(usmedclaim[5,17],times=9),rep(usmedclaim[6,17],times=8),
           rep(usmedclaim[7,17],times=7),rep(usmedclaim[8,17],times=6),
           rep(usmedclaim[9,17],times=5),rep(usmedclaim[10,17],times=4),
           rep(usmedclaim[11,17],times=3),rep(usmedclaim[12,17],times=2),
           rep(usmedclaim[13,17],times=1))

library(dplyr)

#Membuat tabel
data_usmed_1=data.frame(origin_period,development_period,paid_claims,exposure)

#Membuat m_k dan m
data_usmed_2=data_usmed_1 %>%
  group_by(development_period) %>%
  summarise(m_k = sum(paid_claims) / sum(exposure))

m=sum(data_usmed_2$m_k)

data_usmed_4=merge(data_usmed_1,data_usmed_2,by="development_period")

#Membuat Burning cost Paid-to-Date, p_i, dan q_i
data_usmed_5=data_usmed_4 %>%
  group_by(origin_period)%>%
  summarise(expected_burning_cost=max(exposure)*m,
            cumulative_paid_claims=sum(paid_claims),
            p_i=sum(m_k)/m,
            q_i=1-p_i)

#Individual Reserve and Ultimate
individual_ultimate=data_usmed_5$cumulative_paid_claims/data_usmed_5$p_i
individual_reserve=individual_ultimate-data_usmed_5$cumulative_paid_claims
data_usmed_6=data.frame(origin_period=data_usmed_5$origin_period,cumulative_paid_claims=data_usmed_5$cumulative_paid_claims,individual_ultimate,individual_reserve)

#Collective Reserve and Ultimate
collective_reserve=data_usmed_5$q_i*data_usmed_5$expected_burning_cost
collective_ultimate=data_usmed_5$cumulative_paid_claims+collective_reserve
data_usmed_7=data.frame(origin_period=data_usmed_5$origin_period,collective_reserve,collective_ultimate)

#Benktander Reserve and Ultimate
GB_reserve=data_usmed_5$p_i*data_usmed_6$individual_reserve+data_usmed_5$q_i*data_usmed_7$collective_reserve
GB_ultimate=GB_reserve+data_usmed_5$cumulative_paid_claims
data_usmed_8=data.frame(origin_period=data_usmed_5$origin_period,GB_reserve,GB_ultimate)

#Neuhaus Reserve and Ultimate
neuhaus_reserve=data_usmed_5$p_i*m*data_usmed_6$individual_reserve+(1-data_usmed_5$p_i*m)*data_usmed_7$collective_reserve
neuhaus_ultimate=neuhaus_reserve+data_usmed_5$cumulative_paid_claims
data_usmed_9=data.frame(origin_period=data_usmed_5$origin_period,neuhaus_reserve,neuhaus_ultimate)

#Optimal Benktander Reserve and Ultimate
t_i=sqrt(data_usmed_5$p_i)
#z_i=model_5$p_i/(model_5$p_i+sqrt(model_5$p_i))
#atau bisa juga 
z_i=data_usmed_5$p_i/(data_usmed_5$p_i+t_i)
optimal_reserve=z_i*data_usmed_6$individual_reserve+(1-z_i)*data_usmed_7$collective_reserve
optimal_ultimate=optimal_reserve+data_usmed_5$cumulative_paid_claims
data_usmed_10=data.frame(origin_period=data_usmed_5$origin_period,optimal_reserve,optimal_ultimate)

#Cek Z_i Neuhaus
z_i_wn=data_usmed_5$p_i*m

#Ind/OB
ItO=(1/data_usmed_5$p_i)/(data_usmed_5$q_i*(z_i^2/data_usmed_5$p_i+1/data_usmed_5$q_i+(1-z_i)^2/t_i))
#Coll/OB
CtO=(1+data_usmed_5$q_i/t_i)/(data_usmed_5$q_i*(z_i^2/data_usmed_5$p_i+1/data_usmed_5$q_i+(1-z_i)^2/t_i))
#GB/OB
BtO=(data_usmed_5$q_i*(data_usmed_5$p_i^2/data_usmed_5$p_i+1/data_usmed_5$q_i+(1-data_usmed_5$p_i)^2/t_i))/(data_usmed_5$q_i*(z_i^2/data_usmed_5$p_i+1/data_usmed_5$q_i+(1-z_i)^2/t_i))
#WN/OB
NtO=(data_usmed_5$q_i*((data_usmed_5$p_i*m)^2/data_usmed_5$p_i+1/data_usmed_5$q_i+(1-data_usmed_5$p_i*m)^2/t_i))/(data_usmed_5$q_i*(z_i^2/data_usmed_5$p_i+1/data_usmed_5$q_i+(1-z_i)^2/t_i))

#Perbandingan metode menggunakan data
#GLM Normal
tri=data_usmed_1[data_usmed_1$paid_claims > 0, ]
coba_1= glm(paid_claims ~ as.factor(origin_period)+as.factor(development_period), data = tri, family = gaussian(link = "log"))
summary(coba_1)

coefs = exp(as.numeric(coef(coba_1)))
alpha = c(1, coefs[2:13])*coefs[1]
beta  <- c(1, coefs[(13+1):(2*13-1)])
orig_fits <- alpha %*% t(beta)
dim(orig_fits)
future <- row(orig_fits) + col(orig_fits) - 1 > 13
orig_reserve_row <- numeric(13-1)
for(i in 2:13){
  orig_reserve_row[i-1] <- sum(orig_fits[i, (13-i+2):13])
}
point_est_glm <- data.frame(2:13, orig_reserve_row)
names(point_est_glm) <- c("row", "reserve")
GLM_normal=point_est_glm 

#GLM Gamma
tri=data_usmed_1[data_usmed_1$paid_claims > 0, ]
coba_1= glm(paid_claims ~ as.factor(origin_period)+as.factor(development_period), data = tri, family = Gamma(link = "log"))
summary(coba_1)

coefs = exp(as.numeric(coef(coba_1)))
alpha = c(1, coefs[2:13])*coefs[1]
beta  <- c(1, coefs[(13+1):(2*13-1)])
orig_fits <- alpha %*% t(beta)
dim(orig_fits)
future <- row(orig_fits) + col(orig_fits) - 1 > 13
orig_reserve_row <- numeric(13-1)
for(i in 2:13){
  orig_reserve_row[i-1] <- sum(orig_fits[i, (13-i+2):13])
}
point_est_glm <- data.frame(2:13, orig_reserve_row)
names(point_est_glm) <- c("row", "reserve")
GLM_gamma=point_est_glm 

#CL
df = data.frame(
  a = c(data_usmed_1$paid_claims[1:13]),
  b = c(data_usmed_1$paid_claims[14:25], rep(NA, 1)),
  c = c(data_usmed_1$paid_claims[26:36], rep(NA, 2)),
  d = c(data_usmed_1$paid_claims[37:46], rep(NA, 3)),
  e = c(data_usmed_1$paid_claims[47:55], rep(NA, 4)),
  f = c(data_usmed_1$paid_claims[56:63], rep(NA, 5)),
  g = c(data_usmed_1$paid_claims[64:70], rep(NA, 6)),
  h = c(data_usmed_1$paid_claims[71:76], rep(NA, 7)),
  i = c(data_usmed_1$paid_claims[77:81], rep(NA, 8)),
  j = c(data_usmed_1$paid_claims[82:85], rep(NA, 9)),
  k = c(data_usmed_1$paid_claims[86:88], rep(NA, 10)),
  l = c(data_usmed_1$paid_claims[89:90], rep(NA, 11)),
  m= c(data_usmed_1$paid_claims[91], rep(NA, 12))
)
df=data.frame(t(df))

df_cum = data.frame(
  a = c(cumsum(data_usmed_1$paid_claims[1:13])),
  b = c(cumsum(data_usmed_1$paid_claims[14:25]), rep(NA, 1)),
  c = c(cumsum(data_usmed_1$paid_claims[26:36]), rep(NA, 2)),
  d = c(cumsum(data_usmed_1$paid_claims[37:46]), rep(NA, 3)),
  e = c(cumsum(data_usmed_1$paid_claims[47:55]), rep(NA, 4)),
  f = c(cumsum(data_usmed_1$paid_claims[56:63]), rep(NA, 5)),
  g = c(cumsum(data_usmed_1$paid_claims[64:70]), rep(NA, 6)),
  h = c(cumsum(data_usmed_1$paid_claims[71:76]), rep(NA, 7)),
  i = c(cumsum(data_usmed_1$paid_claims[77:81]), rep(NA, 8)),
  j = c(cumsum(data_usmed_1$paid_claims[82:85]), rep(NA, 9)),
  k = c(cumsum(data_usmed_1$paid_claims[86:88]), rep(NA, 10)),
  l = c(cumsum(data_usmed_1$paid_claims[89:90]), rep(NA, 11)),
  m= c(cumsum(data_usmed_1$paid_claims[91]), rep(NA, 12))
)
df_cum=data.frame(t(df_cum))

df_bagi <- data.frame(
  X2 = df_cum$X2 / df_cum$X1,
  X3 = df_cum$X3 / df_cum$X2,
  X4 = df_cum$X4 / df_cum$X3,
  X5 = df_cum$X5 / df_cum$X4,
  X6 = df_cum$X6 / df_cum$X5,
  X7 = df_cum$X7 / df_cum$X6,
  X8 = df_cum$X8 / df_cum$X7,
  X9 = df_cum$X9 / df_cum$X8,
  X10 = df_cum$X10 / df_cum$X9,
  X11 = df_cum$X11 / df_cum$X10,
  X12 = df_cum$X12 / df_cum$X11,
  X13 = df_cum$X13 / df_cum$X12
)

f_average=c(mean(df_bagi[1:12,1]),mean(df_bagi[1:11,2]),mean(df_bagi[1:10,3]),
            mean(df_bagi[1:9,4]),mean(df_bagi[1:8,5]),mean(df_bagi[1:7,6]),
            mean(df_bagi[1:6,7]),mean(df_bagi[1:5,8]),mean(df_bagi[1:4,9]),
            mean(df_bagi[1:3,10]),mean(df_bagi[1:2,11]),mean(df_bagi[1,12]))

f_mean=c(sum(df_cum[1:12,2])/sum(df_cum[1:12,1]),sum(df_cum[1:11,3])/sum(df_cum[1:11,2]),
         sum(df_cum[1:10,4])/sum(df_cum[1:10,3]),sum(df_cum[1:9,5])/sum(df_cum[1:9,4]),
         sum(df_cum[1:8,6])/sum(df_cum[1:8,5]),sum(df_cum[1:7,7])/sum(df_cum[1:7,6]),
         sum(df_cum[1:6,8])/sum(df_cum[1:6,7]),sum(df_cum[1:5,9])/sum(df_cum[1:5,8]),
         sum(df_cum[1:4,10])/sum(df_cum[1:4,9]),sum(df_cum[1:3,11])/sum(df_cum[1:3,10]),
         sum(df_cum[1:2,12])/sum(df_cum[1:2,11]),sum(df_cum[1,13])/sum(df_cum[1,12]))

A=df_cum[2,12]*f_average[12]-df_cum[2,12]
B=df_cum[3,11]*f_average[11]*f_average[12]-df_cum[3,11]
C=df_cum[4,10]*f_average[10]*f_average[11]*f_average[12]-df_cum[4,10]
D=df_cum[5,9]*f_average[9]*f_average[10]*f_average[11]*f_average[12]-
  df_cum[5,9]
E=df_cum[6,8]*f_average[8]*f_average[9]*f_average[10]*f_average[11]*
  f_average[12]-df_cum[6,8]
F=df_cum[7,7]*f_average[7]*f_average[8]*f_average[9]*f_average[10]*
  f_average[11]*f_average[12]-df_cum[7,7]
G=df_cum[8,6]*f_average[6]*f_average[7]*f_average[8]*f_average[9]*
  f_average[10]*f_average[11]*f_average[12]-df_cum[8,6]
H= df_cum[9,5]*f_average[5]*f_average[6]*f_average[7]*f_average[8]*
  f_average[9]*f_average[10]*f_average[11]*f_average[12]-df_cum[9,5]
I= df_cum[10,4]*f_average[4]*f_average[5]*f_average[6]*f_average[7]*
  f_average[8]*f_average[9]*f_average[10]*f_average[11]*f_average[12]-df_cum[10,4]
J=df_cum[11,3]*f_average[3]*f_average[4]*f_average[5]*f_average[6]*
  f_average[7]*f_average[8]*f_average[9]*f_average[10]*f_average[11]*
  f_average[12]-df_cum[11,3]
K=df_cum[12,2]*f_average[2]*f_average[3]*f_average[4]*f_average[5]*
  f_average[6]*f_average[7]*f_average[8]*f_average[9]*f_average[10]*
  f_average[11]*f_average[12]-df_cum[12,2]
L=df_cum[13,1]*f_average[1]*f_average[2]*f_average[3]*f_average[4]*
  f_average[5]*f_average[6]*f_average[7]*f_average[8]*f_average[9]*
  f_average[10]*f_average[11]*f_average[12]-df_cum[13,1]

average_cl=c(A,B,C,D,E,F,G,H,I,J,K,L)
sum_average_cl=sum(average_cl)

A=df_cum[2,12]*f_mean[12]-df_cum[2,12]
B=df_cum[3,11]*f_mean[11]*f_mean[12]-df_cum[3,11]
C=df_cum[4,10]*f_mean[10]*f_mean[11]*f_mean[12]-df_cum[4,10]
D=df_cum[5,9]*f_mean[9]*f_mean[10]*f_mean[11]*f_mean[12]-
  df_cum[5,9]
E=df_cum[6,8]*f_mean[8]*f_mean[9]*f_mean[10]*f_mean[11]*
  f_mean[12]-df_cum[6,8]
F=df_cum[7,7]*f_mean[7]*f_mean[8]*f_mean[9]*f_mean[10]*
  f_mean[11]*f_mean[12]-df_cum[7,7]
G=df_cum[8,6]*f_mean[6]*f_mean[7]*f_mean[8]*f_mean[9]*
  f_mean[10]*f_mean[11]*f_mean[12]-df_cum[8,6]
H= df_cum[9,5]*f_mean[5]*f_mean[6]*f_mean[7]*f_mean[8]*
  f_mean[9]*f_mean[10]*f_mean[11]*f_mean[12]-df_cum[9,5]
I= df_cum[10,4]*f_mean[4]*f_mean[5]*f_mean[6]*f_mean[7]*
  f_mean[8]*f_mean[9]*f_mean[10]*f_mean[11]*f_mean[12]-df_cum[10,4]
J=df_cum[11,3]*f_mean[3]*f_mean[4]*f_mean[5]*f_mean[6]*
  f_mean[7]*f_mean[8]*f_mean[9]*f_mean[10]*f_mean[11]*
  f_mean[12]-df_cum[11,3]
K=df_cum[12,2]*f_mean[2]*f_mean[3]*f_mean[4]*f_mean[5]*
  f_mean[6]*f_mean[7]*f_mean[8]*f_mean[9]*f_mean[10]*
  f_mean[11]*f_mean[12]-df_cum[12,2]
L=df_cum[13,1]*f_mean[1]*f_mean[2]*f_mean[3]*f_mean[4]*
  f_mean[5]*f_mean[6]*f_mean[7]*f_mean[8]*f_mean[9]*
  f_mean[10]*f_mean[11]*f_mean[12]-df_cum[13,1]

mean_cl=c(A,B,C,D,E,F,G,H,I,J,K,L)
sum_mean_cl=sum(mean_cl)

#BF
loss_ratio=df_cum[1,13]/(13*data_usmed_1$exposure[1])
ult_2=13*data_usmed_1$exposure[14]*loss_ratio
ult_3=13*data_usmed_1$exposure[26]*loss_ratio
ult_4=13*data_usmed_1$exposure[37]*loss_ratio
ult_5=13*data_usmed_1$exposure[47]*loss_ratio
ult_6=13*data_usmed_1$exposure[56]*loss_ratio
ult_7=13*data_usmed_1$exposure[64]*loss_ratio
ult_8=13*data_usmed_1$exposure[71]*loss_ratio
ult_9=13*data_usmed_1$exposure[77]*loss_ratio
ult_10=13*data_usmed_1$exposure[82]*loss_ratio
ult_11=13*data_usmed_1$exposure[86]*loss_ratio
ult_12=13*data_usmed_1$exposure[89]*loss_ratio
ult_13=13*data_usmed_1$exposure[91]*loss_ratio

ult=c(df_cum[1,13],ult_2,ult_3,ult_4,ult_5,ult_6,ult_7,ult_8,ult_9,ult_10
      ,ult_11,ult_12,ult_13)

f_average_bf=c(1,cumprod(rev(f_average)))
f_mean_bf=c(1,cumprod(rev(f_mean)))

per_f_average=1-1/f_average_bf
per_f_mean=1-1/f_mean_bf

reserve_bf_average=ult*per_f_average
reserve_bf_mean=ult*per_f_mean

#Nilai claim seharusnya
A=as.numeric(usmedclaim[2,15])
B=sum(as.numeric(usmedclaim[3,14:15]))
C=sum(as.numeric(usmedclaim[4,13:15]))
D=sum(as.numeric(usmedclaim[5,12:15]))
E=sum(as.numeric(usmedclaim[6,11:15]))
F=sum(as.numeric(usmedclaim[7,10:15]))
G=sum(as.numeric(usmedclaim[8,9:15]))
H=sum(as.numeric(usmedclaim[9,8:15]))
I=sum(as.numeric(usmedclaim[10,7:15]))
J=sum(as.numeric(usmedclaim[11,6:15]))
K=sum(as.numeric(usmedclaim[12,5:15]))
L=sum(as.numeric(usmedclaim[13,4:15]))

reserve_real=c(A,B,C,D,E,F,G,H,I,J,K,L)

#Tabel perbandingan persentase
glm_gamma=(GLM_gamma$reserve-reserve_real)/reserve_real
glm_normal=(GLM_normal$reserve-reserve_real)/reserve_real
ind=(data_usmed_6$individual_reserve[2:13]-reserve_real)/reserve_real
coll=(data_usmed_7$collective_reserve[2:13]-reserve_real)/reserve_real
GB=(data_usmed_8$GB_reserve[2:13]-reserve_real)/reserve_real
WN=(data_usmed_9$neuhaus_reserve[2:13]-reserve_real)/reserve_real
OB=(data_usmed_10$optimal_reserve[2:13]-reserve_real)/reserve_real
cl_avrg=(average_cl-reserve_real)/reserve_real
cl_mean=(mean_cl-reserve_real)/reserve_real
bf_avrg=(reserve_bf_average[2:13]-reserve_real)/reserve_real
bf_mean=(reserve_bf_mean[2:13]-reserve_real)/reserve_real

#total
total_glm_gamma=(sum(GLM_gamma$reserve)-sum(reserve_real))/sum(reserve_real)
total_glm_normal=(sum(GLM_normal$reserve)-sum(reserve_real))/sum(reserve_real)
total_ind=(sum(data_usmed_6$individual_reserve[2:13])-sum(reserve_real))/sum(reserve_real)
total_coll=(sum(data_usmed_7$collective_reserve[2:13])-sum(reserve_real))/sum(reserve_real)
total_GB=(sum(data_usmed_8$GB_reserve[2:13])-sum(reserve_real))/sum(reserve_real)
total_WN=(sum(data_usmed_6$individual_reserve[2:12])+data_usmed_9$neuhaus_reserve[13]-sum(reserve_real))/sum(reserve_real)
total_OB=(sum(data_usmed_10$optimal_reserve[2:13])-sum(reserve_real))/sum(reserve_real)
total_cl_avrg=(sum(average_cl)-sum(reserve_real))/sum(reserve_real)
total_cl_mean=(sum(mean_cl)-sum(reserve_real))/sum(reserve_real)
total_bf_avrg=(sum(reserve_bf_average[2:13])-sum(reserve_real))/sum(reserve_real)
total_bf_mean=(sum(reserve_bf_mean[2:13])-sum(reserve_real))/sum(reserve_real)

#gamma test
library(fitdistrplus)
y = data_usmed_1$paid_claims[data_usmed_1$paid_claims > 0]
y_1=fitdist(y, "gamma")


#normality test
shapiro.test(y)

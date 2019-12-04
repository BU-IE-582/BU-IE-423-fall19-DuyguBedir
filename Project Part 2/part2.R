#IE423 Project Part 2
#Duygu Bedir - Ýpek Üstünboyacýoðlu
#Part2.2
library(jpeg)
img<-readJPEG("C:/Users/ipeku/Desktop/IE 423 P2/group_others_bw.jpeg")
img_grey<-img[,,1]
hist(img_grey)
str(img_grey)
memory.limit(size=90000000000000000)
window<-matrix(NA,1,2601)
data<-matrix(NA,122500,2601)

for(i in 1:350){
  for(j in 1:350){
    patch<-img_grey[i:(i+50),j:(j+50)]
    window<-as.vector(t(patch))
    data[(i-1)*350+j,1:2601]<-window[1:2601]
  }
}

data<-as.data.frame(data)
colnames(data)
str(data)
center<-data[,1301]
data<-data[,-1301]
data<-cbind(center,data)

model<-lm(center~.,data=data)
summary(model)
plot(model)

predicted<-predict(model)
predicted
plot(predicted$residuals)

plot(model$residuals, ylim = c(-1,1), main = "Residual Control Chart", xlab = "Period", ylab = "Residuals")
lines(model$residuals)
xbar <- mean(model$residuals)
std <- sd(model$residuals)
M = xbar
UB = xbar + 3*std
LB = xbar - 3*std
abline(LCL,0,col = "red")
abline(UCL,0,col = "red")
abline(CL,0,col = "blue")

for(i in 1:122500){
  if(model$residuals[i]>UCL){
    if((i%%350)==0){
      img_grey[((i/350)+25),((i-1)%%350)+26]=0
      }
    else{
    img_grey[((i-(i%%350))/350)+26,((i-1)%%350)+26]=0
   }
  }
  else if(model$residuals[i]<LCL){
    if((i%%350)==0){
      img_grey[((i/350)+25),((i-1)%%350)+26]=0
    }
    else{
      img_grey[((i-(i%%350))/350)+26,((i-1)%%350)+26]=0
    }
  }
}

plot(1:400, type='n')
rasterImage(img_grey, 1,1,400,400)
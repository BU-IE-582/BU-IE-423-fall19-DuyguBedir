#IE423 Project Part 1
#Part 2
#Part2.1
install.packages("jpeg")
library(jpeg)
img2<-readJPEG("C:/Users/ipeku/Desktop/IE423 Project P1/img3.jpg")
str(img2)
hist(img2)
#Part2.2
m<-mean(img2)
m
#[1] 0.574785
sd<-var(img2)^0.5
sd
#[1] 0.1032304
LB<-qnorm(0.0005,m,sd)
UB<-qnorm(0.9995,m,sd)
LB
#[1] 0.2351026
UB
#[1] 0.9144673

#Part2.3
for(i in 1:512){
  for(j in 1:512){
    if(img2[i,j,1]<LB|img2[i,j,1]>UB){
      img2[i,j,1]=0
      }
    if(img2[i,j,2]<LB|img2[i,j,2]>UB){
      img2[i,j,2]=0
      }
    if(img2[i,j,3]<LB|img2[i,j,3]>UB){
      img2[i,j,3]=0
    }
  }
}
img_original<-readJPEG("C:/Users/ipeku/Desktop/IE423 Project P1/img3.jpg")
par(mfrow=c(1,2))
plot(1:9, type='n')
rasterImage(img_original, 2, 2, 8, 8)
plot(1:9, type='n')
rasterImage(img2, 2, 2, 8, 8)

#Part2.4

newimage<-img_original
for(i in 1:10){
  for(j in 1:10){
    m=mean(newimage[(51*(i-1)+1):(51*(i-1)+51),(51*(j-1)+1):(51*(j-1)+51),1]) 
    sd<-var(newimage[(51*(i-1)+1):(51*(i-1)+51),(51*(j-1)+1):(51*(j-1)+51),1])^0.5
    LB<-qnorm(0.0005,m,sd)
    UB<-qnorm(0.9995,m,sd)
    for(k in 51*(i-1)+1:51*(i-1)+51){
      for(l in 51*(j-1)+1:51*(j-1)+51){
        if(newimage[k,l,1]<LB|newimage[k,l,1]>UB){
          newimage[k,l,1]=0
        }
        if(newimage[k,l,2]<LB|newimage[k,l,2]>UB){
          newimage[k,l,2]=0
        }
        if(newimage[k,l,3]<LB|newimage[k,l,3]>UB){
          newimage[k,l,3]=0
        }
      }
    }
  }
}
plot(1:9, type='n')
rasterImage(newimage, 2, 2, 8, 8)
plot(1:9, type='n')
rasterImage(img_original, 2, 2, 8, 8)
plot(1:9, type='n')
rasterImage(img2, 2, 2, 8, 8)
plot(1:9, type='n')
rasterImage(newimage, 2, 2, 8, 8)


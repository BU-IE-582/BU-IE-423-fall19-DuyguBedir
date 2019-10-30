#IE423 Project Part 1 
#Group 11
#Part1.1
install.packages("jpeg")
library(jpeg)
img<-readJPEG("C:/Users/ipeku/Desktop/IE423 Project P1/img2.jpg")
str(img)
#Part1.2
#a
plot(1:512, type='n')
rasterImage(img, 1, 1, 512, 512)
#b
par(mfrow=c(2,2))
plot(1:512, type='n')
rasterImage(img, 1, 1, 512, 512)
image(1:512,1:512,t(img[,,1]))
image(1:512,1:512,t(img[,,2]))
image(1:512,1:512,t(img[,,3]))

#Part1.3
average1<-img[,1,1]
for(j in 1:512){
    average1[j]<-sum(img[,j,1])/512
  }
average2<-img[,1,2]
for(j in 1:512){
  average2[j]<-sum(img[,j,2])/512
  }
average3<-img[,1,3]
for(j in 1:512){
  average3[j]<-sum(img[,j,3])/512
  }
par(mfrow=c(1,1))

lb<-min(min(average1),min(average2),min(average3))
ub<-max(max(average1),max(average2),max(average3))

plot(average1, type = "l", col="2", ylim = range(lb,ub))
lines(average2, type = "l", col="3" )
lines(average3, type = "l", col="4" )

#Part1.4

for(i in 1:512){
  for(j in 1:256){
    if((img[i,j,1]-img[i,j+256,1])>0){
       img[i,j,1]=img[i,j,1]-img[i,j+256,1]
    }
    else{
      img[i,j,1]=0
    }
    if((img[i,j,2]-img[i,j+256,2])>0){
      img[i,j,2]=img[i,j,2]-img[i,j+256,2]
    }
    else{
      img[i,j,2]=0
    }
    if((img[i,j,3]-img[i,j+256,3])>0){
      img[i,j,3]=img[i,j,3]-img[i,j+256,3]
    }
    else{
      img[i,j,3]=0
    }
    }
}

plot(1:9, type='n')
rasterImage(img, 2, 2, 8, 8)

#Part1.5
#5x5
img<-readJPEG("C:/Users/ipeku/Desktop/IE423 Project P1/img2.jpg")
mat<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
mat1<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
mat2<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
x<-img
count=1
for(i in 1:508){
  for(j in 1:508){
    for(k in 0:4){
      for(l in 0:4){
    mat[count]=img[i+k,j+l,1]
    mat1[count]=img[i+k,j+l,2]
    mat2[count]=img[i+k,j+l,3]
    count=count+1
      }
      }
    count=1
    x[i+2,j+2,1]=median(mat)
    x[i+2,j+2,2]=median(mat1)
    x[i+2,j+2,3]=median(mat2)
    }
}
plot(1:9, type='n')
rasterImage(x, 2, 2, 8, 8)
#11x11
img<-readJPEG("C:/Users/ipeku/Desktop/IE423 Project P1/img2.jpg")
for(z in 0:120){
  mat[z]<-0
  mat1[z]<-0
  mat2[z]<-0
}
img<-readJPEG("C:/Users/ipeku/Desktop/IE423 Project P1/img2.jpg")
x<-img
count=1
for(i in 1:501){
  for(j in 1:501){
    for(k in 0:10){
      for(l in 0:10){
        mat[count]=img[i+k,j+l,1]
        mat1[count]=img[i+k,j+l,2]
        mat2[count]=img[i+k,j+l,3]
        count=count+1
      }
    }
    count=1
    x[i+5,j+5,1]=median(mat)
    x[i+5,j+5,2]=median(mat1)
    x[i+5,j+5,3]=median(mat2)
  }
}
plot(1:9, type='n')
rasterImage(x, 2, 2, 8, 8)

#31x31
img<-readJPEG("C:/Users/ipeku/Desktop/IE423 Project P1/img2.jpg")
for(g in 0:960){
  mat[g]<-0
  mat1[g]<-0
  mat2[g]<-0
}
x<-img
count=1
for(i in 1:481){
  for(j in 1:481){
    for(k in 0:30){
      for(l in 0:30){
        mat[count]=img[i+k,j+l,1]
        mat1[count]=img[i+k,j+l,2]
        mat2[count]=img[i+k,j+l,3]
        count=count+1
      }
    }
    count=1
    x[i+15,j+15,1]=median(mat)
    x[i+15,j+15,2]=median(mat1)
    x[i+15,j+15,3]=median(mat2)
  }
}
plot(1:9, type='n')
rasterImage(x, 2, 2, 8, 8)
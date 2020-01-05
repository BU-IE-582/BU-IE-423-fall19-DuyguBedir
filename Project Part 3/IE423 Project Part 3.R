#IE423 Project Part 3
#Group 11
#Duygu Bedir- Ipek Üstünboyacioglu

library(jpeg)
library(wvtool)

#Fabric1
img<-readJPEG("C:/Users/ipeku/Desktop/IE 423/IE423 Project Part 3/Images_bw/Fabric1_.jpg")
img_grey<-img[,,1]
hist(img_grey)
str(img_grey)

test<-gabor.filter(img_grey,30,180,3,0,1,TRUE)
str(test$filtered_img)
m<-rep(NA)
s<-rep(NA)
newimage<-test$filtered_img
for(i in 1:16){
  for(j in 1:16){
    m[(i-1)*16+j]=mean(newimage[(32*(i-1)+1):(32*(i-1)+32),(32*(j-1)+1):(32*(j-1)+32)]) 
    s[(i-1)*16+j]<-sd(newimage[(32*(i-1)+1):(32*(i-1)+32),(32*(j-1)+1):(32*(j-1)+32)])
  }
}
CL<-mean(m)
UCL<-CL+2*sd(m)
LCL<-CL-2*sd(m)

plot(m, type="l", lwd=1)
points(m, pch=".", col="darkred", cex=3, type = "p")
abline(CL,0, col="blue")
abline(UCL,0, col="red")
abline(LCL,0, col="red")

out_of_control<-rep(0)
k<-1
for(i in 1:256){
if(m[i]>UCL|m[i]<LCL){
  out_of_control[k]=i
  k<-k+1
  }
}
out_of_control

img_original<-readJPEG("C:/Users/ipeku/Desktop/IE 423/IE423 Project Part 3/Grid images/grid1.jpg")
plot(1:512, type='n')
rasterImage(img_original, 1, 1, 512, 512)

#Fabric2
img<-readJPEG("C:/Users/ipeku/Desktop/IE 423/IE423 Project Part 3/Images_bw/Fabric2_.jpg")
img_grey<-img[,,1]
hist(img_grey)
str(img_grey)

test<-gabor.filter(img_grey,30,180,3,0,1,TRUE)
str(test$filtered_img)
m<-rep(NA)
s<-rep(NA)
newimage<-test$filtered_img
for(i in 1:16){
  for(j in 1:16){
    m[(i-1)*16+j]=mean(newimage[(32*(i-1)+1):(32*(i-1)+32),(32*(j-1)+1):(32*(j-1)+32)]) 
    s[(i-1)*16+j]<-sd(newimage[(32*(i-1)+1):(32*(i-1)+32),(32*(j-1)+1):(32*(j-1)+32)])
  }
}
CL<-mean(m)
UCL<-CL+2*sd(m)
LCL<-CL-2*sd(m)

plot(m, type="l", lwd=1)
points(m, pch=".", col="darkred", cex=3, type = "p")
abline(CL,0, col="blue")
abline(UCL,0, col="red")
abline(LCL,0, col="red")

out_of_control<-rep(0)
k<-1
for(i in 1:256){
  if(m[i]>UCL|m[i]<LCL){
    out_of_control[k]=i
    k<-k+1
  }
}
out_of_control

img_original<-readJPEG("C:/Users/ipeku/Desktop/IE 423/IE423 Project Part 3/Grid images/grid2.jpg")
plot(1:512, type='n')
rasterImage(img_original, 1, 1, 512, 512)

#Fabric3
img<-readJPEG("C:/Users/ipeku/Desktop/IE 423/IE423 Project Part 3/Images_bw/Fabric3_.jpg")
img_grey<-img[,,1]
hist(img_grey)
str(img_grey)

test<-gabor.filter(img_grey,30,180,3,0,1,TRUE)
str(test$filtered_img)
m<-rep(NA)
s<-rep(NA)
newimage<-test$filtered_img
for(i in 1:16){
  for(j in 1:16){
    m[(i-1)*16+j]=mean(newimage[(32*(i-1)+1):(32*(i-1)+32),(32*(j-1)+1):(32*(j-1)+32)]) 
    s[(i-1)*16+j]<-sd(newimage[(32*(i-1)+1):(32*(i-1)+32),(32*(j-1)+1):(32*(j-1)+32)])
  }
}
CL<-mean(m)
UCL<-CL+2*sd(m)
LCL<-CL-2*sd(m)

plot(m, type="l", lwd=1)
points(m, pch=".", col="darkred", cex=3, type = "p")
abline(CL,0, col="blue")
abline(UCL,0, col="red")
abline(LCL,0, col="red")

out_of_control<-rep(0)
k<-1
for(i in 1:256){
  if(m[i]>UCL|m[i]<LCL){
    out_of_control[k]=i
    k<-k+1
  }
}
out_of_control

img_original<-readJPEG("C:/Users/ipeku/Desktop/IE 423/IE423 Project Part 3/Grid images/grid3.jpg")
plot(1:512, type='n')
rasterImage(img_original, 1, 1, 512, 512)

#Fabric4
img<-readJPEG("C:/Users/ipeku/Desktop/IE 423/IE423 Project Part 3/Images_bw/Fabric4_.jpg")
img_grey<-img[,,1]
hist(img_grey)
str(img_grey)

test<-gabor.filter(img_grey,30,180,3,0,1,TRUE)
str(test$filtered_img)
m<-rep(NA)
s<-rep(NA)
newimage<-test$filtered_img
for(i in 1:16){
  for(j in 1:16){
    m[(i-1)*16+j]=mean(newimage[(32*(i-1)+1):(32*(i-1)+32),(32*(j-1)+1):(32*(j-1)+32)]) 
    s[(i-1)*16+j]<-sd(newimage[(32*(i-1)+1):(32*(i-1)+32),(32*(j-1)+1):(32*(j-1)+32)])
  }
}
CL<-mean(m)
UCL<-CL+2*sd(m)
LCL<-CL-2*sd(m)

plot(m, type="l", lwd=1)
points(m, pch=".", col="darkred", cex=3, type = "p")
abline(CL,0, col="blue")
abline(UCL,0, col="red")
abline(LCL,0, col="red")

out_of_control<-rep(0)
k<-1
for(i in 1:256){
  if(m[i]>UCL|m[i]<LCL){
    out_of_control[k]=i
    k<-k+1
  }
}
out_of_control

img_original<-readJPEG("C:/Users/ipeku/Desktop/IE 423/IE423 Project Part 3/Grid images/grid4.jpg")
plot(1:512, type='n')
rasterImage(img_original, 1, 1, 512, 512)

#Fabric5
img<-readJPEG("C:/Users/ipeku/Desktop/IE 423/IE423 Project Part 3/Images_bw/Fabric5_.jpg")
img_grey<-img[,,1]
hist(img_grey)
str(img_grey)

test<-gabor.filter(img_grey,30,180,3,0,1,TRUE)
str(test$filtered_img)
m<-rep(NA)
s<-rep(NA)
newimage<-test$filtered_img
for(i in 1:16){
  for(j in 1:16){
    m[(i-1)*16+j]=mean(newimage[(32*(i-1)+1):(32*(i-1)+32),(32*(j-1)+1):(32*(j-1)+32)]) 
    s[(i-1)*16+j]<-sd(newimage[(32*(i-1)+1):(32*(i-1)+32),(32*(j-1)+1):(32*(j-1)+32)])
  }
}
CL<-mean(m)
UCL<-CL+2*sd(m)
LCL<-CL-2*sd(m)

plot(m, type="l", lwd=1)
points(m, pch=".", col="darkred", cex=3, type = "p")
abline(CL,0, col="blue")
abline(UCL,0, col="red")
abline(LCL,0, col="red")

out_of_control<-rep(0)
k<-1
for(i in 1:256){
  if(m[i]>UCL|m[i]<LCL){
    out_of_control[k]=i
    k<-k+1
  }
}
out_of_control

img_original<-readJPEG("C:/Users/ipeku/Desktop/IE 423/IE423 Project Part 3/Grid images/grid5.jpg")
plot(1:512, type='n')
rasterImage(img_original, 1, 1, 512, 512)

#Fabric6
img<-readJPEG("C:/Users/ipeku/Desktop/IE 423/IE423 Project Part 3/Images_bw/Fabric6_.jpg")
img_grey<-img[,,1]
hist(img_grey)
str(img_grey)

test<-gabor.filter(img_grey,30,180,3,0,1,TRUE)
str(test$filtered_img)
m<-rep(NA)
s<-rep(NA)
newimage<-test$filtered_img
for(i in 1:16){
  for(j in 1:16){
    m[(i-1)*16+j]=mean(newimage[(32*(i-1)+1):(32*(i-1)+32),(32*(j-1)+1):(32*(j-1)+32)]) 
    s[(i-1)*16+j]<-sd(newimage[(32*(i-1)+1):(32*(i-1)+32),(32*(j-1)+1):(32*(j-1)+32)])
  }
}
CL<-mean(m)
UCL<-CL+2*sd(m)
LCL<-CL-2*sd(m)

plot(m, type="l", lwd=1)
points(m, pch=".", col="darkred", cex=3, type = "p")
abline(CL,0, col="blue")
abline(UCL,0, col="red")
abline(LCL,0, col="red")

out_of_control<-rep(0)
k<-1
for(i in 1:256){
  if(m[i]>UCL|m[i]<LCL){
    out_of_control[k]=i
    k<-k+1
  }
}
out_of_control

img_original<-readJPEG("C:/Users/ipeku/Desktop/IE 423/IE423 Project Part 3/Grid images/grid6.jpg")
plot(1:512, type='n')
rasterImage(img_original, 1, 1, 512, 512)

#Fabric7
img<-readJPEG("C:/Users/ipeku/Desktop/IE 423/IE423 Project Part 3/Images_bw/Fabric7_.jpg")
img_grey<-img[,,1]
hist(img_grey)
str(img_grey)

test<-gabor.filter(img_grey,30,180,3,0,1,TRUE)
str(test$filtered_img)
m<-rep(NA)
s<-rep(NA)
newimage<-test$filtered_img
for(i in 1:16){
  for(j in 1:16){
    m[(i-1)*16+j]=mean(newimage[(32*(i-1)+1):(32*(i-1)+32),(32*(j-1)+1):(32*(j-1)+32)]) 
    s[(i-1)*16+j]<-sd(newimage[(32*(i-1)+1):(32*(i-1)+32),(32*(j-1)+1):(32*(j-1)+32)])
  }
}
CL<-mean(m)
UCL<-CL+2*sd(m)
LCL<-CL-2*sd(m)

plot(m, type="l", lwd=1)
points(m, pch=".", col="darkred", cex=3, type = "p")
abline(CL,0, col="blue")
abline(UCL,0, col="red")
abline(LCL,0, col="red")

out_of_control<-rep(0)
k<-1
for(i in 1:256){
  if(m[i]>UCL|m[i]<LCL){
    out_of_control[k]=i
    k<-k+1
  }
}
out_of_control

img_original<-readJPEG("C:/Users/ipeku/Desktop/IE 423/IE423 Project Part 3/Grid images/grid7.jpg")
plot(1:512, type='n')
rasterImage(img_original, 1, 1, 512, 512)

#Fabric8
img<-readJPEG("C:/Users/ipeku/Desktop/IE 423/IE423 Project Part 3/Images_bw/Fabric8_.jpg")
img_grey<-img[,,1]
hist(img_grey)
str(img_grey)

test<-gabor.filter(img_grey,30,180,3,0,1,TRUE)
str(test$filtered_img)
m<-rep(NA)
s<-rep(NA)
newimage<-test$filtered_img
for(i in 1:16){
  for(j in 1:16){
    m[(i-1)*16+j]=mean(newimage[(32*(i-1)+1):(32*(i-1)+32),(32*(j-1)+1):(32*(j-1)+32)]) 
    s[(i-1)*16+j]<-sd(newimage[(32*(i-1)+1):(32*(i-1)+32),(32*(j-1)+1):(32*(j-1)+32)])
  }
}
CL<-mean(m)
UCL<-CL+2*sd(m)
LCL<-CL-2*sd(m)

plot(m, type="l", lwd=1)
points(m, pch=".", col="darkred", cex=3, type = "p")
abline(CL,0, col="blue")
abline(UCL,0, col="red")
abline(LCL,0, col="red")

out_of_control<-rep(0)
k<-1
for(i in 1:256){
  if(m[i]>UCL|m[i]<LCL){
    out_of_control[k]=i
    k<-k+1
  }
}
out_of_control

img_original<-readJPEG("C:/Users/ipeku/Desktop/IE 423/IE423 Project Part 3/Grid images/grid8.jpg")
plot(1:512, type='n')
rasterImage(img_original, 1, 1, 512, 512)

#Fabric9
img<-readJPEG("C:/Users/ipeku/Desktop/IE 423/IE423 Project Part 3/Images_bw/Fabric9_.jpg")
img_grey<-img[,,1]
hist(img_grey)
str(img_grey)

test<-gabor.filter(img_grey,30,180,3,0,1,TRUE)
str(test$filtered_img)
m<-rep(NA)
s<-rep(NA)
newimage<-test$filtered_img
for(i in 1:16){
  for(j in 1:16){
    m[(i-1)*16+j]=mean(newimage[(32*(i-1)+1):(32*(i-1)+32),(32*(j-1)+1):(32*(j-1)+32)]) 
    s[(i-1)*16+j]<-sd(newimage[(32*(i-1)+1):(32*(i-1)+32),(32*(j-1)+1):(32*(j-1)+32)])
  }
}
CL<-mean(m)
UCL<-CL+2*sd(m)
LCL<-CL-2*sd(m)

plot(m, type="l", lwd=1)
points(m, pch=".", col="darkred", cex=3, type = "p")
abline(CL,0, col="blue")
abline(UCL,0, col="red")
abline(LCL,0, col="red")

out_of_control<-rep(0)
k<-1
for(i in 1:256){
  if(m[i]>UCL|m[i]<LCL){
    out_of_control[k]=i
    k<-k+1
  }
}
out_of_control

img_original<-readJPEG("C:/Users/ipeku/Desktop/IE 423/IE423 Project Part 3/Grid images/grid9.jpg")
plot(1:512, type='n')
rasterImage(img_original, 1, 1, 512, 512)

#Fabric10
img<-readJPEG("C:/Users/ipeku/Desktop/IE 423/IE423 Project Part 3/Images_bw/Fabric10_.jpg")
img_grey<-img[,,1]
hist(img_grey)
str(img_grey)

test<-gabor.filter(img_grey,30,180,3,0,1,TRUE)
str(test$filtered_img)
m<-rep(NA)
s<-rep(NA)
newimage<-test$filtered_img
for(i in 1:16){
  for(j in 1:16){
    m[(i-1)*16+j]=mean(newimage[(32*(i-1)+1):(32*(i-1)+32),(32*(j-1)+1):(32*(j-1)+32)]) 
    s[(i-1)*16+j]<-sd(newimage[(32*(i-1)+1):(32*(i-1)+32),(32*(j-1)+1):(32*(j-1)+32)])
  }
}
CL<-mean(m)
UCL<-CL+2*sd(m)
LCL<-CL-2*sd(m)

plot(m, type="l", lwd=1)
points(m, pch=".", col="darkred", cex=3, type = "p")
abline(CL,0, col="blue")
abline(UCL,0, col="red")
abline(LCL,0, col="red")

out_of_control<-rep(0)
k<-1
for(i in 1:256){
  if(m[i]>UCL|m[i]<LCL){
    out_of_control[k]=i
    k<-k+1
  }
}
out_of_control

img_original<-readJPEG("C:/Users/ipeku/Desktop/IE 423/IE423 Project Part 3/Grid images/grid10.jpg")
plot(1:512, type='n')
rasterImage(img_original, 1, 1, 512, 512)

#Fabric11
img<-readJPEG("C:/Users/ipeku/Desktop/IE 423/IE423 Project Part 3/Images_bw/Fabric11_.jpg")
img_grey<-img[,,1]
hist(img_grey)
str(img_grey)

test<-gabor.filter(img_grey,30,180,3,0,1,TRUE)
str(test$filtered_img)
m<-rep(NA)
s<-rep(NA)
newimage<-test$filtered_img
for(i in 1:16){
  for(j in 1:16){
    m[(i-1)*16+j]=mean(newimage[(32*(i-1)+1):(32*(i-1)+32),(32*(j-1)+1):(32*(j-1)+32)]) 
    s[(i-1)*16+j]<-sd(newimage[(32*(i-1)+1):(32*(i-1)+32),(32*(j-1)+1):(32*(j-1)+32)])
  }
}
CL<-mean(m)
UCL<-CL+2*sd(m)
LCL<-CL-2*sd(m)

plot(m, type="l", lwd=1)
points(m, pch=".", col="darkred", cex=3, type = "p")
abline(CL,0, col="blue")
abline(UCL,0, col="red")
abline(LCL,0, col="red")

out_of_control<-rep(0)
k<-1
for(i in 1:256){
  if(m[i]>UCL|m[i]<LCL){
    out_of_control[k]=i
    k<-k+1
  }
}
out_of_control

img_original<-readJPEG("C:/Users/ipeku/Desktop/IE 423/IE423 Project Part 3/Grid images/grid11.jpg")
plot(1:512, type='n')
rasterImage(img_original, 1, 1, 512, 512)

#Fabric12
img<-readJPEG("C:/Users/ipeku/Desktop/IE 423/IE423 Project Part 3/Images_bw/Fabric12_.jpg")
img_grey<-img[,,1]
hist(img_grey)
str(img_grey)

test<-gabor.filter(img_grey,30,180,3,0,1,TRUE)
str(test$filtered_img)
m<-rep(NA)
s<-rep(NA)
newimage<-test$filtered_img
for(i in 1:16){
  for(j in 1:16){
    m[(i-1)*16+j]=mean(newimage[(32*(i-1)+1):(32*(i-1)+32),(32*(j-1)+1):(32*(j-1)+32)]) 
    s[(i-1)*16+j]<-sd(newimage[(32*(i-1)+1):(32*(i-1)+32),(32*(j-1)+1):(32*(j-1)+32)])
  }
}
CL<-mean(m)
UCL<-CL+2*sd(m)
LCL<-CL-2*sd(m)

plot(m, type="l", lwd=1)
points(m, pch=".", col="darkred", cex=3, type = "p")
abline(CL,0, col="blue")
abline(UCL,0, col="red")
abline(LCL,0, col="red")

out_of_control<-rep(0)
k<-1
for(i in 1:256){
  if(m[i]>UCL|m[i]<LCL){
    out_of_control[k]=i
    k<-k+1
  }
}
out_of_control

img_original<-readJPEG("C:/Users/ipeku/Desktop/IE 423/IE423 Project Part 3/Grid images/grid12.jpg")
plot(1:512, type='n')
rasterImage(img_original, 1, 1, 512, 512)

#Fabric13
img<-readJPEG("C:/Users/ipeku/Desktop/IE 423/IE423 Project Part 3/Images_bw/Fabric13_.jpg")
img_grey<-img[,,1]
hist(img_grey)
str(img_grey)

test<-gabor.filter(img_grey,30,180,3,0,1,TRUE)
str(test$filtered_img)
m<-rep(NA)
s<-rep(NA)
newimage<-test$filtered_img
for(i in 1:16){
  for(j in 1:16){
    m[(i-1)*16+j]=mean(newimage[(32*(i-1)+1):(32*(i-1)+32),(32*(j-1)+1):(32*(j-1)+32)]) 
    s[(i-1)*16+j]<-sd(newimage[(32*(i-1)+1):(32*(i-1)+32),(32*(j-1)+1):(32*(j-1)+32)])
  }
}
CL<-mean(m)
UCL<-CL+2*sd(m)
LCL<-CL-2*sd(m)

plot(m, type="l", lwd=1)
points(m, pch=".", col="darkred", cex=3, type = "p")
abline(CL,0, col="blue")
abline(UCL,0, col="red")
abline(LCL,0, col="red")

out_of_control<-rep(0)
k<-1
for(i in 1:256){
  if(m[i]>UCL|m[i]<LCL){
    out_of_control[k]=i
    k<-k+1
  }
}
out_of_control

img_original<-readJPEG("C:/Users/ipeku/Desktop/IE 423/IE423 Project Part 3/Grid images/grid13.jpg")
plot(1:512, type='n')
rasterImage(img_original, 1, 1, 512, 512)

#Fabric14
img<-readJPEG("C:/Users/ipeku/Desktop/IE 423/IE423 Project Part 3/Images_bw/Fabric14_.jpg")
img_grey<-img[,,1]
hist(img_grey)
str(img_grey)

test<-gabor.filter(img_grey,30,180,3,0,1,TRUE)
str(test$filtered_img)
m<-rep(NA)
s<-rep(NA)
newimage<-test$filtered_img
for(i in 1:16){
  for(j in 1:16){
    m[(i-1)*16+j]=mean(newimage[(32*(i-1)+1):(32*(i-1)+32),(32*(j-1)+1):(32*(j-1)+32)]) 
    s[(i-1)*16+j]<-sd(newimage[(32*(i-1)+1):(32*(i-1)+32),(32*(j-1)+1):(32*(j-1)+32)])
  }
}
CL<-mean(m)
UCL<-CL+2*sd(m)
LCL<-CL-2*sd(m)

plot(m, type="l", lwd=1)
points(m, pch=".", col="darkred", cex=3, type = "p")
abline(CL,0, col="blue")
abline(UCL,0, col="red")
abline(LCL,0, col="red")

out_of_control<-rep(0)
k<-1
for(i in 1:256){
  if(m[i]>UCL|m[i]<LCL){
    out_of_control[k]=i
    k<-k+1
  }
}
out_of_control

img_original<-readJPEG("C:/Users/ipeku/Desktop/IE 423/IE423 Project Part 3/Grid images/grid14.jpg")
plot(1:512, type='n')
rasterImage(img_original, 1, 1, 512, 512)

#Fabric15
img<-readJPEG("C:/Users/ipeku/Desktop/IE 423/IE423 Project Part 3/Images_bw/Fabric15_.jpg")
img_grey<-img[,,1]
hist(img_grey)
str(img_grey)

test<-gabor.filter(img_grey,30,180,3,0,1,TRUE)
str(test$filtered_img)
m<-rep(NA)
s<-rep(NA)
newimage<-test$filtered_img
for(i in 1:16){
  for(j in 1:16){
    m[(i-1)*16+j]=mean(newimage[(32*(i-1)+1):(32*(i-1)+32),(32*(j-1)+1):(32*(j-1)+32)]) 
    s[(i-1)*16+j]<-sd(newimage[(32*(i-1)+1):(32*(i-1)+32),(32*(j-1)+1):(32*(j-1)+32)])
  }
}
CL<-mean(m)
UCL<-CL+2*sd(m)
LCL<-CL-2*sd(m)

plot(m, type="l", lwd=1)
points(m, pch=".", col="darkred", cex=3, type = "p")
abline(CL,0, col="blue")
abline(UCL,0, col="red")
abline(LCL,0, col="red")

out_of_control<-rep(0)
k<-1
for(i in 1:256){
  if(m[i]>UCL|m[i]<LCL){
    out_of_control[k]=i
    k<-k+1
  }
}
out_of_control

img_original<-readJPEG("C:/Users/ipeku/Desktop/IE 423/IE423 Project Part 3/Grid images/grid15.jpg")
plot(1:512, type='n')
rasterImage(img_original, 1, 1, 512, 512)

#Fabric16
img<-readJPEG("C:/Users/ipeku/Desktop/IE 423/IE423 Project Part 3/Images_bw/Fabric16_.jpg")
img_grey<-img[,,1]
hist(img_grey)
str(img_grey)

test<-gabor.filter(img_grey,30,180,3,0,1,TRUE)
str(test$filtered_img)
m<-rep(NA)
s<-rep(NA)
newimage<-test$filtered_img
for(i in 1:16){
  for(j in 1:16){
    m[(i-1)*16+j]=mean(newimage[(32*(i-1)+1):(32*(i-1)+32),(32*(j-1)+1):(32*(j-1)+32)]) 
    s[(i-1)*16+j]<-sd(newimage[(32*(i-1)+1):(32*(i-1)+32),(32*(j-1)+1):(32*(j-1)+32)])
  }
}
CL<-mean(m)
UCL<-CL+2*sd(m)
LCL<-CL-2*sd(m)

plot(m, type="l", lwd=1)
points(m, pch=".", col="darkred", cex=3, type = "p")
abline(CL,0, col="blue")
abline(UCL,0, col="red")
abline(LCL,0, col="red")

out_of_control<-rep(0)
k<-1
for(i in 1:256){
  if(m[i]>UCL|m[i]<LCL){
    out_of_control[k]=i
    k<-k+1
  }
}
out_of_control

img_original<-readJPEG("C:/Users/ipeku/Desktop/IE 423/IE423 Project Part 3/Grid images/grid16.jpg")
plot(1:512, type='n')
rasterImage(img_original, 1, 1, 512, 512)

#Fabric17
img<-readJPEG("C:/Users/ipeku/Desktop/IE 423/IE423 Project Part 3/Images_bw/Fabric17_.jpg")
img_grey<-img[,,1]
hist(img_grey)
str(img_grey)

test<-gabor.filter(img_grey,30,180,3,0,1,TRUE)
str(test$filtered_img)
m<-rep(NA)
s<-rep(NA)
newimage<-test$filtered_img
for(i in 1:16){
  for(j in 1:16){
    m[(i-1)*16+j]=mean(newimage[(32*(i-1)+1):(32*(i-1)+32),(32*(j-1)+1):(32*(j-1)+32)]) 
    s[(i-1)*16+j]<-sd(newimage[(32*(i-1)+1):(32*(i-1)+32),(32*(j-1)+1):(32*(j-1)+32)])
  }
}
CL<-mean(m)
UCL<-CL+2*sd(m)
LCL<-CL-2*sd(m)

plot(m, type="l", lwd=1)
points(m, pch=".", col="darkred", cex=3, type = "p")
abline(CL,0, col="blue")
abline(UCL,0, col="red")
abline(LCL,0, col="red")

out_of_control<-rep(0)
k<-1
for(i in 1:256){
  if(m[i]>UCL|m[i]<LCL){
    out_of_control[k]=i
    k<-k+1
  }
}
out_of_control

img_original<-readJPEG("C:/Users/ipeku/Desktop/IE 423/IE423 Project Part 3/Grid images/grid17.jpg")
plot(1:512, type='n')
rasterImage(img_original, 1, 1, 512, 512)

#Fabric18
img<-readJPEG("C:/Users/ipeku/Desktop/IE 423/IE423 Project Part 3/Images_bw/Fabric18_.jpg")
img_grey<-img[,,1]
hist(img_grey)
str(img_grey)

test<-gabor.filter(img_grey,30,180,3,0,1,TRUE)
str(test$filtered_img)
m<-rep(NA)
s<-rep(NA)
newimage<-test$filtered_img
for(i in 1:16){
  for(j in 1:16){
    m[(i-1)*16+j]=mean(newimage[(32*(i-1)+1):(32*(i-1)+32),(32*(j-1)+1):(32*(j-1)+32)]) 
    s[(i-1)*16+j]<-sd(newimage[(32*(i-1)+1):(32*(i-1)+32),(32*(j-1)+1):(32*(j-1)+32)])
  }
}
CL<-mean(m)
UCL<-CL+2*sd(m)
LCL<-CL-2*sd(m)

plot(m, type="l", lwd=1)
points(m, pch=".", col="darkred", cex=3, type = "p")
abline(CL,0, col="blue")
abline(UCL,0, col="red")
abline(LCL,0, col="red")

out_of_control<-rep(0)
k<-1
for(i in 1:256){
  if(m[i]>UCL|m[i]<LCL){
    out_of_control[k]=i
    k<-k+1
  }
}
out_of_control

img_original<-readJPEG("C:/Users/ipeku/Desktop/IE 423/IE423 Project Part 3/Grid images/grid18.jpg")
plot(1:512, type='n')
rasterImage(img_original, 1, 1, 512, 512)

#Fabric19
img<-readJPEG("C:/Users/ipeku/Desktop/IE 423/IE423 Project Part 3/Images_bw/Fabric19_.jpg")
img_grey<-img[,,1]
hist(img_grey)
str(img_grey)

test<-gabor.filter(img_grey,30,180,3,0,1,TRUE)
str(test$filtered_img)
m<-rep(NA)
s<-rep(NA)
newimage<-test$filtered_img
for(i in 1:16){
  for(j in 1:16){
    m[(i-1)*16+j]=mean(newimage[(32*(i-1)+1):(32*(i-1)+32),(32*(j-1)+1):(32*(j-1)+32)]) 
    s[(i-1)*16+j]<-sd(newimage[(32*(i-1)+1):(32*(i-1)+32),(32*(j-1)+1):(32*(j-1)+32)])
  }
}
CL<-mean(m)
UCL<-CL+2*sd(m)
LCL<-CL-2*sd(m)

plot(m, type="l", lwd=1)
points(m, pch=".", col="darkred", cex=3, type = "p")
abline(CL,0, col="blue")
abline(UCL,0, col="red")
abline(LCL,0, col="red")

out_of_control<-rep(0)
k<-1
for(i in 1:256){
  if(m[i]>UCL|m[i]<LCL){
    out_of_control[k]=i
    k<-k+1
  }
}
out_of_control

img_original<-readJPEG("C:/Users/ipeku/Desktop/IE 423/IE423 Project Part 3/Grid images/grid19.jpg")
plot(1:512, type='n')
rasterImage(img_original, 1, 1, 512, 512)

#Fabric20
img<-readJPEG("C:/Users/ipeku/Desktop/IE 423/IE423 Project Part 3/Images_bw/Fabric20_.jpg")
img_grey<-img[,,1]
hist(img_grey)
str(img_grey)

test<-gabor.filter(img_grey,30,180,3,0,1,TRUE)
str(test$filtered_img)
m<-rep(NA)
s<-rep(NA)
newimage<-test$filtered_img
for(i in 1:16){
  for(j in 1:16){
    m[(i-1)*16+j]=mean(newimage[(32*(i-1)+1):(32*(i-1)+32),(32*(j-1)+1):(32*(j-1)+32)]) 
    s[(i-1)*16+j]<-sd(newimage[(32*(i-1)+1):(32*(i-1)+32),(32*(j-1)+1):(32*(j-1)+32)])
  }
}
CL<-mean(m)
UCL<-CL+2*sd(m)
LCL<-CL-2*sd(m)

plot(m, type="l", lwd=1)
points(m, pch=".", col="darkred", cex=3, type = "p")
abline(CL,0, col="blue")
abline(UCL,0, col="red")
abline(LCL,0, col="red")

out_of_control<-rep(0)
k<-1
for(i in 1:256){
  if(m[i]>UCL|m[i]<LCL){
    out_of_control[k]=i
    k<-k+1
  }
}
out_of_control

img_original<-readJPEG("C:/Users/ipeku/Desktop/IE 423/IE423 Project Part 3/Grid images/grid20.jpg")
plot(1:512, type='n')
rasterImage(img_original, 1, 1, 512, 512)


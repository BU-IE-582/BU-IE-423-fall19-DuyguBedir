#IE423 Project Part 2
#Duygu Bedir - Ipek Üstünboyacioglu
#Part2.1
install.packages("jpeg")
library(jpeg)
img<-readJPEG("C:/Users/ipeku/Desktop/IE 423 P2/group_others_bw.jpeg")
img_grey<-img[,,1]
str(img_grey)
plot(1:400, type='n')
rasterImage(img_grey, 1, 1, 400, 400)

#a
avg_row<-rep(0,400)
sd_row<-rep(0,400)
UCL_row<-rep(0,400)
LCL_row<-rep(0,400)
for (i in 1:400){
  avg_row[i]<-sum(img_grey[i,])/400
  sd_row[i]<-sd(img_grey[i,])
  UCL_row[i]<-avg_row[i]+3*sd_row[i]
  LCL_row[i]<-avg_row[i]-3*sd_row[i]
}

for (i in 1:400){
  for(j in 1:400){
    if(img_grey[i,j]>UCL_row[i]){
     img_grey[i,j]=0
     }
    if(img_grey[i,j]<LCL_row[i]){
     img_grey[i,j]=0
    }
  }
}
plot(1:800, type='n')
rasterImage(img_grey, 0, 200, 399, 600)
rasterImage(img, 400, 200, 800, 600)

img_grey_chart<-img[,,1]
CL = avg_row[200]
UCL = avg_row[200] + 3*sd_row[200]
LCL = avg_row[200] - 3*sd_row[200]
plot(img_grey_chart[200,], ylim = c(LCL-0.05,UCL+0.05))
abline(LCL,0,col = "red")
abline(UCL,0,col = "red")
abline(CL,0,col = "blue")


#b
img_grey2<-img[,,1]
avg_col<-rep(0,400)
sd_col<-rep(0,400)
UCL_col<-rep(0,400)
LCL_col<-rep(0,400)

for (i in 1:400){
  avg_col[i]<-sum(img_grey2[,i])/400
  sd_col[i]<-sd(img_grey2[,i])
  UCL_col[i]<-avg_col[i]+3*sd_col[i]
  LCL_col[i]<-avg_col[i]-3*sd_col[i]
}

for (i in 1:400){
  for(j in 1:400){
    if(img_grey2[j,i]>UCL_col[i]){
      img_grey2[j,i]=0
    }
    if(img_grey2[j,i]<LCL_col[i]){
      img_grey2[j,i]=0
    }
  }
}

plot(1:800, type='n')
rasterImage(img_grey2, 0, 200, 399, 600)
rasterImage(img, 400, 200, 800, 600)

img_grey_chart<-img[,,1]
CL2 = avg_col[200]
UCL2 = avg_col[200] + 3*sd_col[200]
LCL2 = avg_col[200] - 3*sd_col[200]
plot(img_grey_chart[,200], ylim = c(LCL2-0.05,UCL2+0.05))
abline(LCL2,0,col = "red")
abline(UCL2,0,col = "red")
abline(CL2,0,col = "blue")

plot(1:800, type='n')
rasterImage(img_grey, 1, 400, 400, 800)
rasterImage(img_grey2, 1, 1, 400, 400)
rasterImage(img, 400, 400, 800, 800)


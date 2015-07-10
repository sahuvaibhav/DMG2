setwd("C:\\Users\\Vaibhav\\Desktop\\ISB\\DMG2")

data = read.table("mushroom.txt",sep = ',',na.strings = "NA", stringsAsFactors=T)
names(data) = c('class','capshape','capsurface','capcolor','bruises','odor','gillattachment',
                'gillspacing','gillsize','gillcolor','stalkshape','stalkroot','stalksurfaceabovering',
                'stalksurfacebelowring','stalkcolorabovering','stalkcolorbelowring','veiltype',
                'veilcolor','ringnumber','ringtype','sporeprintcolor','population','habitat')

attach(data)
data[data$stalkroot == '?',c(12)] = names(which.max(table(data[data$stalkroot != '?',]$stalkroot)))

length(data[data$stalkroot == '?',]$stalkroot)
table(data$stalkroot)


library(ggplot2)
library(cowplot)
p1 = ggplot(aes(x=class), data=data, xlab='Stalk_root')+geom_histogram(aes(fill=class))
p2 = ggplot(aes(x=capshape), data=data, xlab='Stalk_root')+geom_histogram(aes(fill=capshape))
p3 = ggplot(aes(x=capcolor), data=data, xlab='Stalk_root')+geom_histogram(aes(fill=capcolor))
p4 = ggplot(aes(x=bruises), data=data, xlab='Stalk_root')+geom_histogram(aes(fill=bruises))
p5 = ggplot(aes(x=odor), data=data, xlab='Stalk_root')+geom_histogram(aes(fill=odor))
p6 = ggplot(aes(x=gillattachment), data=data, xlab='Stalk_root')+geom_histogram(aes(fill=gillattachment))
p7 = ggplot(aes(x=gillspacing), data=data, xlab='Stalk_root')+geom_histogram(aes(fill=gillspacing))
p8 = ggplot(aes(x=gillcolor), data=data, xlab='Stalk_root')+geom_histogram(aes(fill=gillcolor))
p9 = ggplot(aes(x=stalkshape), data=data, xlab='Stalk_root')+geom_histogram(aes(fill=stalkshape))
plot_grid(p1,p2,p3,p4,p5,p6,p7,p8,p9,align = 'h')



p1 = ggplot(aes(x=stalkroot), data=data, xlab='Stalk_root')+geom_histogram(aes(fill=stalkroot))
p2 = ggplot(aes(x=stalkshape), data=data, xlab='Stalk_root')+geom_histogram(aes(fill=stalkshape))
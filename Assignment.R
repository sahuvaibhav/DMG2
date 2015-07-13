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
p1 = ggplot(aes(x=class), data=data)+geom_histogram(aes(fill=class))
p2 = ggplot(aes(x=capshape), data=data)+geom_histogram(aes(fill=capshape))
p3 = ggplot(aes(x=capsurface), data=data)+geom_histogram(aes(fill=capsurface))
p4 = ggplot(aes(x=capcolor), data=data)+geom_histogram(aes(fill=capcolor))
p5 = ggplot(aes(x=bruises), data=data)+geom_histogram(aes(fill=bruises))
p6 = ggplot(aes(x=odor), data=data)+geom_histogram(aes(fill=odor))
plot_grid(p1,p2,p3,p4,p5,p6,align = 'h')

p1 = ggplot(aes(x=gillattachment), data=data)+geom_histogram(aes(fill=gillattachment))
p2 = ggplot(aes(x=gillspacing), data=data)+geom_histogram(aes(fill=gillspacing))
p3 = ggplot(aes(x=gillsize), data=data)+geom_histogram(aes(fill=gillsize))
p4 = ggplot(aes(x=gillcolor), data=data)+geom_histogram(aes(fill=gillcolor))
p5 = ggplot(aes(x=stalkshape), data=data)+geom_histogram(aes(fill=stalkshape))
p6 = ggplot(aes(x=stalkroot), data=data)+geom_histogram(aes(fill=stalkroot))
plot_grid(p1,p2,p3,p4,p5,p6,align = 'h')

p1 = ggplot(aes(x=stalksurfaceabovering), data=data)+geom_histogram(aes(fill=stalksurfaceabovering))
p2 = ggplot(aes(x=stalksurfacebelowring), data=data)+geom_histogram(aes(fill=stalksurfacebelowring))
p3 = ggplot(aes(x=stalkcolorabovering), data=data)+geom_histogram(aes(fill=stalkcolorabovering))
p4 = ggplot(aes(x=stalkcolorbelowring), data=data)+geom_histogram(aes(fill=stalkcolorbelowring))
p5 = ggplot(aes(x=veiltype), data=data)+geom_histogram(aes(fill=veiltype))
p6 = ggplot(aes(x=veilcolor), data=data)+geom_histogram(aes(fill=veilcolor))
plot_grid(p1,p2,p3,p4,p5,p6,align = 'h')

p1 = ggplot(aes(x=ringnumber), data=data)+geom_histogram(aes(fill=ringnumber))
p2 = ggplot(aes(x=ringtype), data=data)+geom_histogram(aes(fill=ringtype))
p3 = ggplot(aes(x=sporeprintcolor), data=data)+geom_histogram(aes(fill=sporeprintcolor))
p4 = ggplot(aes(x=population), data=data)+geom_histogram(aes(fill=population))
p5 = ggplot(aes(x=habitat), data=data)+geom_histogram(aes(fill=habitat))
plot_grid(p1,p2,p3,p4,p5,align = 'h')

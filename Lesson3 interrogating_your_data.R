library(ggplot2)


#读取encode数据并将初始的四列给予名字
HMM = read.table('Encode_HMM.bed',sep = '\t',
                 header = F)
names(HMM)[1:4] = c('chr','start','end','type')
head(HMM)
ggplot(data = HMM,aes(x = HMM$chr,fill = HMM$type)) + geom_bar()
#设置size列
HMM$size = HMM$end - HMM$start
mean(HMM$size)
sd(HMM$size)
median(HMM$size)
max(HMM$size)
min(HMM$size)

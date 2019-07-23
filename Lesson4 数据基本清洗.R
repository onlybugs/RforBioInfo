library(ggplot2)
library(plyr)
#读取数据
HMM = read.table('Encode_HMM.bed',sep = '\t',
                 header = F)
names(HMM)[1:4] = c('chr','start','end','type')
head(HMM)
HMM$size = HMM$end - HMM$start

#利用factor的levels对染色体进行排序并画图
HMM$chr = factor(gsub('chr',"",HMM$chr))
HMM$chr = factor(HMM$chr,levels = c(seq(1,22),'X',"Y"))
ggplot(data = HMM,
       aes(x = HMM$chr,fill = type)) + geom_bar()

#筛选一部分基因
HMM = HMM[HMM$type %in% c('8_Insulator','11_Weak_Txn'),]
#用revalue函数替换字符串
HMM$type = revalue(HMM$type,
                   c('8_Insulator' = '8','11_Weak_Txn' = '11'))
#画图
ggplot(data = HMM,
       aes(x = HMM$chr,fill = type)) + geom_bar(position = 'fill')


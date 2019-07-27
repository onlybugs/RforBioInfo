library(grid)
library(ggplot2)

#数据准备
{
HMM_name <- "Encode_HMM.bed"
HMM <- read.table(HMM_name, sep="\t", header=FALSE)
names(HMM) = c("chrom","start","stop","type")
HMM$chrom <- factor(gsub("chr", "", HMM$chrom, fixed=TRUE),
                    levels=c(seq(1,22),"X","Y"))
HMM <- HMM[HMM$type
           %in% c("1_Active_Promoter",
                  "4_Strong_Enhancer","8_Insulator"),]
library(plyr)
HMM$type <- revalue(HMM$type, 
                    c("1_Active_Promoter"="Promoter", 
                      "4_Strong_Enhancer"="Enhancer",
                      "8_Insulator"="Insulator"))

ggplot(HMM,aes(x=chrom,fill=type)) + geom_bar()
}

#facet函数的使用
{
  #facet函数绘制分面条形图
  ggplot(data = HMM,
         aes(x = chrom,fill = type)) + geom_bar() + facet_grid(type~.) + guides(fill = F)
  ggplot(data = HMM,
         aes(x = chrom,fill = type)) + geom_bar() + facet_grid(.~type) + guides(fill = F)
  
  #facets函数绘制分布
  a = ggplot(data = HMM,
         aes(x = chrom,fill = type)) + geom_density() + facet_grid(type~chrom) + guides(fill = F)
  a +  theme(axis.title.x=element_blank(),
             axis.text.x=element_blank(),
             axis.ticks.x=element_blank())
  #等等其他的东西自己挖掘
  
}

#大图套小图的做法(grid)包的使用
#建议用ps制作，非要用R语言，请到Code-Lesson1-8里的
#7.R读源码
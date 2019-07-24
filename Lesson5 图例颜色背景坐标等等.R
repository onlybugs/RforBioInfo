library(ggplot2)

#前几课做过的部分,数据的基本读取与清洗
{
#读取文件
HMM_name <- "Encode_HMM.bed"
HMM <- read.table(HMM_name, sep="\t", header=FALSE)
#命名
names(HMM) = c("chrom","start","stop","type")
#排序
HMM$chrom <- factor(gsub("chr", "", HMM$chrom, fixed=TRUE),
                    levels=c(seq(1,22),"X","Y"))
#筛选
HMM <- HMM[HMM$type
                   %in% c("1_Active_Promoter",
                          "4_Strong_Enhancer","8_Insulator"),]
#再次重命名
library(plyr)
HMM$type <- revalue(HMM$type, 
                        c("1_Active_Promoter"="Promoter", 
                          "4_Strong_Enhancer"="Enhancer",
                          "8_Insulator"="Insulator"))

# 尝试画图
ggplot(my_data,aes(x=chrom,fill=type)) + geom_bar()
}

#关于标题,图例标题，xy坐标轴标题的设置和对所有的字体大小的设置
{
# 保存对象
b = ggplot(HMM,aes(x=chrom,fill=type)) + geom_bar()

# 增加标题,用labs函数增加标题
b + labs(title="Regulatory features by chromosome")

# 改变xy以及图例的标题,fill表示fill图例标题的意思,colour表示colour标题意思
b + geom_bar() + labs(x = "Chromosome",y="Count",fill="Feature")

# 保存标题的配置
b = b +  geom_bar() + labs(x = "Chromosome",y="Count",fill="Feature")
# 绘制b
b

#  修改背景层，设置字体大小为8
b + theme_gray(base_size = 8)


# 这段代码里图片字体永久设置为8
theme_set(theme_gray(base_size = 8))
b

# 恢复默认
theme_set(theme_gray())
b

# 作者的喜好
theme_set(theme_gray(base_size = 16))
b
}

# 关于调色，统计图中颜色的展现
{
library(RColorBrewer)
display.brewer.all()

b = b + theme_gray(base_size = 12) 

#利用scale_fill_brewer函数指定调色板
b + scale_fill_brewer(palette="Set1")
b + scale_fill_brewer(palette="Pastel1")
b + scale_fill_brewer(palette="YlOrRd")

#利用scale_fill_manual函数指定颜色向量
b + scale_fill_manual(values = c("green","blue","red"))

#使用Rcolorbrewer的调色板
col = brewer.pal(3,'Set3')
b + scale_fill_manual(values = col)


# 新建一个巨大的调色板
palette1 <- brewer.pal(9,"Set1")
palette2 <- brewer.pal(8,"Set2")
palette3 <- brewer.pal(9,"Set3")
# 展示调色板
pie(rep(1,length(palette1)),col=palette1)
pie(rep(1,length(palette2)),col=palette2)
pie(rep(1,length(palette3)),col=palette3)
# 组合上述调色板
big_palette <- c(palette1,palette2,palette3)
# 绘制上述调色板并在统计图中使用
pie(rep(1,length(big_palette)),col=big_palette)
chr1 = ggplot(HMM,aes(x = type,fill = chrom)) + geom_bar()
chr1 + scale_fill_manual(values = big_palette)

# 利用sample进行随机取色
chr1 + scale_fill_manual(values = sample(big_palette))

#仍旧利用brewer取色
b + scale_fill_brewer(palette="Set2")
}

# 关于字体大小的分别的设置，x,y,title,legend的分别设置
{
#一切都恢复默认，然后画图
theme_set(theme_gray())
b

# 两个基础的背景的默认配置
b + theme_gray() # 默认
b + theme_bw() # 黑白


# 设置字体和字体大小
b + theme_gray(base_size = 24, 
               base_family = "Times New Roman")


#关于坐标轴和图例的处理
# 设置坐标轴上刻度字的字体大小
b + theme(axis.text=element_text(size=20)) # numbers on axes
# 设置坐标轴标签字体的大小
b + theme(axis.title=element_text(size=12)) # titles on axes
# 设置图例标题字体大小
b + theme(legend.title=element_text(size=20)) # legend title
# 设置图例内容的字体大小
b + theme(legend.text=element_text(size=20,family="Times New Roman"))
# 这一段的综合应用
b + theme(
  legend.text=element_text(size=20,family="Times New Roman"),
  axis.title=element_text(size=30),
  axis.text=element_text(size=20)
)
}

# 设置背景色和网格线以及擦除网格
{
  # 改变背景色
b + theme(panel.background = element_rect(fill="pink"))
b + theme(panel.background = element_rect(fill="white"))
# 改变网格线颜色
b + theme(panel.grid.major = element_line(colour = "pink"),
          panel.grid.minor = element_line(colour = "pink")
          )
# 擦除网格线
b + theme(panel.grid.major = element_line(NA),
              panel.grid.minor = element_line(NA))
# 凸显y网格线并擦除x网格线
b + theme(panel.grid.major.y = element_line(colour = "black",size=0.2),
              panel.grid.major.x = element_line(NA),
              panel.grid.minor = element_line(NA))
}

# 对坐标轴刻度线的调整，包括颜色，大小等等
{
#原图
b
# 设置坐标轴刻度大小更大
b + theme(axis.ticks = element_line(size=2))
#去除坐标轴的刻度线
b + theme(axis.ticks = element_line(NA))
# 设置刻度线的颜色
b + theme(axis.ticks = element_line(color="blue",size=2))
# 刻度线综合应用
b + theme(axis.ticks = element_line(size=2), 
              axis.ticks.x = element_line(color="blue"), 
              axis.ticks.y = element_line(color="red"))
}


# 改变图例的位置,移除图例标题，移除图例
{
  # 改变图例的位置
b + theme(legend.position="top")
b + theme(legend.position="bottom")
b + theme(legend.position=c(0,0))
b + theme(legend.position=c(1,1))
b + theme(legend.position=c(0.8,0.8))
# 移除图例标题
b + labs(fill="")
b + labs(fill="") + theme(legend.position=c(0.8,0.8))
# 完全移除图例
b + guides(fill=FALSE)
}



#theme实例以及设置自己配置的参数为默认参数的方法，
#使y轴和柱子挨在一起的方法
#恢复默认配置的方法
{
# 去除网格线和背景色，然后设置主坐标轴线size=0.5,
#设置标题和刻度值大小
pstyle <- b + guides(fill=FALSE) +  theme(axis.line = element_line(size=0.5),
                                   panel.background = element_rect(fill=NA,size=rel(20)), 
                                   panel.grid.minor = element_line(colour = NA), 
                                   axis.text = element_text(size=16), 
                                   axis.title = element_text(size=18))
#画图
pstyle
#让柱子和坐标挨在一起
pstyle + scale_y_continuous(expand=c(0,0))

#  将上述参数设为默认
theme_set(theme_gray()+theme(axis.line = element_line(size=0.5),
                             panel.background = element_rect(fill=NA,size=rel(20)), 
                             panel.grid.minor = element_line(colour = NA), 
                             axis.text = element_text(size=16), 
                             axis.title = element_text(size=18)))

b

# 指定expand = 0,0必须要没有图例才可以,即guides(fill=FALSE)
b + scale_y_continuous(expand=c(0,0)) + guides(fill=FALSE)
# 一切恢复默认
theme_set(theme_gray())
b
}











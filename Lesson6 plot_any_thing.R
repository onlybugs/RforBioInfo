library(ggplot2)
library(plyr)
library(RColorBrewer)

#初始化数据并且画一个简单的例图
{
HMM = read.table('Encode_HMM.bed',sep = '\t',header = F,
                 stringsAsFactors = T,quote = '')
names(HMM) = c('chrom','start','stop','gname')
HMM$size = HMM$stop - HMM$start
HMM$chrom = gsub('chr','',HMM$chrom)
HMM$chrom = factor(factor(HMM$chrom),levels = c(1:22,'X','Y'))
HMM = HMM[HMM$gname %in% c('12_Repressed','3_Poised_Promoter','8_Insulator'),]
HMM$gname = revalue(HMM$gname,c('12_Repressed' = '12',
                    '3_Poised_Promoter' = '3',
                    '8_Insulator' = '8'))
col = rev(brewer.pal(3,"Accent")) 
ggplot(data = HMM,
       aes(x = chrom,fill = gname)) + geom_bar() + scale_fill_manual(values = col)
}

#theme一下配置
theme_set(theme_gray(base_size = 16))
theme_set(theme_gray())

#画一个直方图(条形图)
{
  d = ggplot(data = HMM,
         aes(x = chrom,fill = gname))
  d + geom_bar()
  #加上y轴限制
  d +  geom_bar() + ylim(0,3000)
}

#画误差直方图以及如何指定xy的直方图
{
  df = count(HMM$chrom)
  d1 = ggplot(data = df,
         aes(x = x,y = df$freq,fill = x)) + geom_bar(stat="identity")
  d1 + geom_errorbar(ymax = df$freq + 100,
                     ymin = df$freq - 100,
                     width = .2,
                     cex = 1
                     ) + guides(fill = F) + xlab('chrom') + ylab('freq')
}

#画散点图,散点误差画法和误差直方图相似，参考上述即可
{
  p2 = ggplot(data = HMM,
         aes(x = HMM$start,y = HMM$size,colour = gname))
  p2 = p2 + geom_point(alpha = 0.5) + labs(x = "Start_p",
                                              y="Size",
                                              fill="gname")
  p2
}

#画箱线图，当使用coord(坐标函数)函数时使箱子横过来
{
  p3 = ggplot(data = HMM[HMM$chrom %in% c('1','2','3'),],
         aes(x = chrom,y = size)) + coord_flip()
  p3 = p3 + geom_boxplot(aes(colour = chrom))
  p3 + ylim(0,3000)
}

#画小提琴图
{
  p3 = ggplot(data = HMM[HMM$chrom %in% c('1','2','3'),],
              aes(x = chrom,y = size))
  p3 = p3 + geom_violin(aes(fill = chrom))
  p3 + ylim(0,3000) + guides(fill = F)
  
}

#画分布曲线
{
  #简单的分布曲线
  p3 = ggplot(data = HMM[HMM$chrom %in% c('1','2','3'),],
              aes(x = size,fill = chrom))
  p3 + geom_density(alpha = 0.5) + xlim(100,250)
  p3 + geom_density(alpha = 0.5,position = 'stack') + xlim(100,250)
  
}

#画折线图
{
  df = data.frame(x = c(2018:2021,2018:2021),
                  y = as.integer(rnorm(8,10,8)),
                  sam = factor(rep(1:2,each = 4)))
  p4 = ggplot(data = df,
         aes(x = x,
             y = y,
             colour = sam)) + geom_line(size = 1.3) + geom_point(size = 3)
  p4 + geom_errorbar(ymax = df$y + 0.5,
                     ymin = df$y - 0.8,
                     width = .1,
                     cex = 1
                     ) + labs(x = "year",y="price",colour = 'group')
}

#画dot图
{
  p5 = ggplot(data = HMM[HMM$chrom %in% c('1','2','3'),],
         aes(x = size,fill = chrom)) + xlim(10000,30000)
  p5 + geom_dotplot(stackgroups = TRUE,method = 'histodot')
  
}

#极坐标系绘图，任何统计图都可以使用极坐标
{
  #散点图的极坐标
  p2 = ggplot(data = HMM,
              aes(x = HMM$start,y = HMM$size,colour = gname)) + geom_point(alpha = 0.5) + labs(x = "Start_p",
                                           y="Size",
                                           fill="gname")
  p2 + coord_polar()
  #直方图
  d = ggplot(data = HMM,
             aes(x = chrom,fill = gname)) + geom_bar()
  d + coord_polar()
  #等等都可以使用极坐标
}

#饼图
#参考之前学过的R基本绘图

#Venn图
{
  #安装一个包
  #install.packages('VennDiagram')
  library(VennDiagram)
  #指定x为一个list，必须指定输出文件名字，指定填充颜色
  venn.diagram(
    x = list(
      B = c(61:70, 71:100),
      C = c(41:60, 61:70)
    ),
    filename = "1.png",
    fill = c('yellow','blue')
  )
}



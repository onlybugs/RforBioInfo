# 21-10-20
# ...
# barplot-

library(ggplot2)
library(forcats)
library(dplyr)

rt = read.table("factor.txt",header = T,sep = '\t',stringsAsFactors = F)

baseplot = ggplot(rt,aes(x = fct_reorder(name,desc(factor)),y = factor)) + 
  geom_bar(stat = 'identity',
           fill = 'lightblue',
           size = .3,
           color = 'black',
           width = 0.5) + 
  geom_hline(yintercept = 0.125,size = 1,linetype = 21) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1),
        axis.text = element_text(family = "serif"))



baseplot + scale_y_continuous(expand = c(0,0),
                              limits = c(0,max(rt$factor) + 0.05)) + 
  xlab("Name") + ylab("Factor")


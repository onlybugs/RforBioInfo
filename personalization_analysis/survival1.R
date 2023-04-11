
library(survival)
library(survminer)


rm(list = ls())

g1 = "ZFHX3"
g2 = "VDAC1P8"

# 读表达矩阵
df = read.csv("test.csv",header = T)
# 筛选一下列
# df1 = df[,c(names(df)[1:3],g1,g2)]


cutoff<-surv_cutpoint(df, 
                      time="futime",
                      event="fustat",
                      variables=c(g1,g2)
)
cutval = summary(cutoff)

# plot(cutoff, 
#      g2, 
#      palette = "lancet")


groups<-surv_categorize(cutoff)
groups$type = ifelse(groups[,g1] == 'high' & groups[,g2] == 'high',"IV",
                     ifelse(groups[,g1] == 'high' & groups[,g2] == 'low',"III",
                            ifelse(groups[,g1] == 'low' & groups[,g2] == 'low',"I","II")))

# df = cbind(df,groups$type)
# names(df)[6] = "Groups"
# write.csv(df1,"test.csv")

# 基因表达量在不同阶段中的散点图
library(ggExtra)
p = ggplot(data = df,
           aes(x = df[,g1],
               y = df[,g2])) + 
  geom_point(aes(color = Groups)) + geom_smooth(method = 'loess',formula = 'y ~ x') +
  geom_vline(xintercept = cutval[g1,'cutpoint'],color = '#ffb74d',size = 1) +
  geom_hline(yintercept = cutval[g2,'cutpoint'],color = '#ffb74d',size = 1) +
  theme_bw() + xlab(g1) + ylab(g2)
# p
ggMarginal(p,type = 'histogram')


# corr
cort = cor.test(df[,g1],df[,g2])
cort$p.value
cort$estimate
# por
table(df$Groups) / dim(df)[1]



# 生存分析
fit <- survfit(Surv(futime, fustat)~Groups, data=df)
ggsurvplot(fit, 
           data = df,    
           pval=TRUE,        
           pval.method=TRUE,  
           palette = "lancet",
           risk.table = F, 
           conf.int = F) 
# Cox风险分析
cutoff_cox<-coxph(Surv(futime, fustat)~Groups,
                  data=df);summary(cutoff_cox)


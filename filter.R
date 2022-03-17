#1.读取数据
read_data <- function(filename){
  df = read.csv(file = filename, row.names = 1)
  df = t(df)
  for (i in 2:ncol(df)){
    df[, i] = as.numeric(df[, i])
  }
  return (df)
}

#2.数据归一化(norm)
filter_write <- function(df, file, base = 1){
  filter_df = df
  #base = 1
  baseline =  as.numeric(df[, ncol(filter_df) - 3 + base])
  for (i in 2:ncol(df)){
    filter_df[, i] = as.numeric(df[, i]) / baseline
  }
  write.csv(t(filter_df),file = paste0("./filter_data/",file,"_", 
                                       colnames(df)[ncol(filter_df) - 3 + base] ,".csv"))
  return (filter_df)
}


#3.数据处理与绘图
#reshape2包可以实现在宽格式（wide-format）和长格式（long-format）之间转换数据。
library(reshape2)
library(ggplot2)
library(ggsignif)   #加显著性


##绘图
select_and_plot <- function(filter_df, file, col = 2){
  #(A)选出不同代谢物
  tmp_df = as.data.frame(filter_df[, c(1,col)])
  tmp_df$sample = rownames(tmp_df)
  tmp_df = melt(tmp_df, id = c("group", "sample"))
  tmp_df$value <- as.numeric(tmp_df$value)
  
  ##(B)分组计算
  #(1)chow组和HFD组对比
  richness_HFD <- subset(tmp_df, group %in% c('chow', 'HFD'))
  richness_HFD$group <- factor(richness_HFD$group)
  #(2)chow组和W组对比
  richness_W <- subset(tmp_df, group %in% c('chow', "Western diet"))
  richness_W$group <- factor(richness_W$group)
  #(3)chow组和HF组对比
  richness_HF <- subset(tmp_df, group %in% c('chow', "Western diet"))
  richness_HF$group <- factor(richness_HF$group)
  
  richness_ALL <- subset(tmp_df, group %in% c('chow', 'HFD', "Western diet", "high fat high cholesterol"))
  
  ##(C)t检验
  #(1)chow组和HFD组
  t_test_HFD <- t.test(value~group, richness_HFD, paired = FALSE, alternative = 'two.sided')
  #(2)chow组和W组
  t_test_W <- t.test(value~group, richness_W, paired = FALSE, alternative = 'two.sided')
  #(3)chow组和HF组
  t_test_HF <- t.test(value~group, richness_HF, paired = FALSE, alternative = 'two.sided')
  
  ##(D)计算mean和sd
  library(doBy) 
  ALL1 <- summaryBy(value~group, richness_ALL, FUN = c(mean, sd))
  ALL1$p_value <- c(1, t_test_HFD$p.value, t_test_W$p.value, t_test_W$p.value)    #增加p_value
  
  ##(E)添加标签
  label <- c("", "", "", "") 
  for (i in 2:4){
    if (ALL1$p_value[i] < 0.001){
      label[i] = "***"
    }
    else if (ALL1$p_value[i] < 0.01){
      label[i] = "**"
    }
    else if (ALL1$p_value[i] < 0.05){
      label[i] = "*"
    }
  }
  
  ##(F)绘图
  #png(filename = paste0("./figure/", file,"_", colnames(filter_df)[col], ".png"))
  png(filename = paste0("./boxplot_figure/", file,"_", colnames(filter_df)[col], ".png"))
  myplot <- ggplot(ALL1, aes(group, value.mean, fill = group))+ 
    geom_col(width = 0.4, show.legend = FALSE)+ 
    geom_errorbar(aes(ymin = value.mean , ymax = value.mean + value.sd), width = 0.15, size = 0.5)+ 
    theme(panel.grid = element_blank(), 
          panel.background = element_rect(color = 'black', fill = 'transparent'), 
          plot.title = element_text(hjust = 0.5)) +
    labs(x = 'Group', y = 'vaule', title = tmp_df$variable)+
    scale_y_continuous(expand = c(0,0),limits = c(0,(max(ALL1$value.mean) + 2 * max(ALL1$value.sd)))) +
    #scale_y_continuous(expand = c(0,0),limits = c(0,20)) +
    geom_text(aes(y = value.mean +  1.5 * value.sd , label = label, group = group))
  print(myplot)
  dev.off()
}


####main
data_process <- function(filename){
  #A.读取数据
  file = strsplit(filename, ".csv")[[1]][1]
  df <- read_data(filename = filename)
  
  #B.不同方法校正
  for (base in 1:3){
    filter_write(df, file, base)
  }
  
  ##C.绘图
  filter_df = filter_write(df, file, base = 1)
  filter_df = as.data.frame(filter_df)
  
  for (col in 2:nrow(filter_df)){
    select_and_plot(filter_df, file, col)
  }
  
}

setwd("E:/学习/研究生/Code/20211027_CoA/plot/")
##1.Heart
data_process("Heart.csv")

##2.Liver
data_process("Liver.csv")

##3.BAT
data_process("BAT.csv")

##4.Hypo
data_process("Hypo.csv")

##5.Musle
data_process("Musle.csv")














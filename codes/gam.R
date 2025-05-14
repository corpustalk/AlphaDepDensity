library(plyr)
library(dplyr)
library(ggplot2)
library(lme4)
library(gridExtra)
library(stringr)
library(corrplot)
library(MuMIn)
library(mgcv)
library(RColorBrewer)
library(grid)
library(tm)
library(gamm4)
library(lmtest)

##############################################################################
data_1 <- read.csv('../data/02_nsrts_with_trad_pred.csv')
data_2 <- read.csv("../data/02_brownrts_with_trad_pred.csv")
data_3 <- read.csv('../data/02_nsmaze_with_trad_pred.csv')


# selected the columns that are needed
column_rt_name <- c("words","dependency_density","alphadepdensity",
                    "word_length","frequency","surprisal","startofsentence","endofsentence",
                    "reading_time")

columns_1 <- c("words","dependency_density","alphadepdensity",
               "word_length","frequency","surprisal","startofsentence","endofsentence",
               "meanItemRT")

data_1 <- data_1[columns_1]
colnames(data_1) <- column_rt_name

# summary(data_2)
data_2 <- data_2[columns_1]
colnames(data_2) <- column_rt_name


data_3 <- data_3[columns_1]
colnames(data_3) <- column_rt_name

data_1 <- na.omit(data_1)
data_2 <- na.omit(data_2)
data_3 <- na.omit(data_3)


# calculate the correlation between alphadepdensity, dependency_density, frequency, surprisal, word_length
###############################################################################
plot_correlation <- function(data, output_filename) {

  data_subset <- data[, c( "frequency", "surprisal", "word_length","alphadepdensity", "dependency_density")]
  colnames(data_subset) <- c("Frequency", "Surprisal", "WordLength", "AlphaDepDensity", "DependencyDensity")

  cor_matrix <- cor(data_subset, use = "complete.obs")  # 排除NA值
  

  cor_melted <- as.data.frame(as.table(cor_matrix))
  colnames(cor_melted) <- c("Var1", "Var2", "correlation")
  # 
  levels_order <- colnames(cor_matrix)
  cor_melted$Var1 <- factor(cor_melted$Var1, levels = rev(levels_order))
  cor_melted$Var2 <- factor(cor_melted$Var2, levels = levels_order)
  
  

  science_palette <- brewer.pal(3, "Set2") 
  

  p <- ggplot(cor_melted, aes(x = Var1, y = Var2, fill = correlation)) +
    geom_tile() +
    geom_text(aes(label = sprintf("%.2f", correlation)), color = "black", size = 4) +
    scale_fill_gradient2(low = science_palette[1], high = science_palette[3], mid = "white", midpoint = 0) +
    theme_minimal() +
    theme(
      text = element_text(family = "Arial"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  
      axis.text.y = element_text(size = 14), 
      axis.title = element_blank(), 
      axis.ticks = element_blank(), 
      legend.position = "none"
    ) +
    coord_fixed()  
  
  # 保存图像
  ggsave(output_filename, plot = p, width = 5, height = 5, dpi = 750)
  
  return(p)  # 返回绘图对象，如果需要查看图像
}

library(Hmisc)
rcorr(as.matrix(data_1[, c("frequency", "surprisal", "word_length","alphadepdensity", "dependency_density")]))
rcorr(as.matrix(data_2[, c("frequency", "surprisal", "word_length","alphadepdensity", "dependency_density")]))
rcorr(as.matrix(data_3[, c("frequency", "surprisal", "word_length","alphadepdensity", "dependency_density")]))

# 例如：将数据框传入并保存图像
plot_correlation(data_1, "data_1_correlation.png")
plot_correlation(data_2, "data_2_correlation.png")
plot_correlation(data_3, "data_3_correlation.png")



###############################################################################
data_1$words <- as.factor(data_1$words)
data_1$log_reading_time <- log(data_1$reading_time)

model_data_1g0_gam <- gam(
  log_reading_time ~ s(dependency_density,bs="tp",k=5) + s(words, bs = "re"),
  data = data_1)
summary(model_data_1g0_gam)


model_data_1g1_gam <- gam(
  log_reading_time ~ s(alphadepdensity,bs="tp") + s(words, bs = "re"),
  data = data_1)
summary(model_data_1g1_gam)


model_data_1g2_gam <- gam(
  log_reading_time ~ s(dependency_density,bs="tp",k=5) + 
    s(alphadepdensity,bs="tp")+s(words, bs = "re"),
  data = data_1)
summary(model_data_1g2_gam)
save_figure_gam(model_data_1g2_gam, "data1_dd.png",1)
save_figure_gam(model_data_1g2_gam, "data1_add.png",2)


model_data_1g3_gam <- gam(
  log_reading_time ~ 
    s(frequency,k=5) + 
    s(word_length,k=5)+
    s(surprisal,k=5)+
    s(words, bs = "re"),
  data = data_1)

summary(model_data_1g3_gam)


model_data_1g4_gam <- gam(
  log_reading_time ~ s(dependency_density,bs="tp",k=5) + 
    s(alphadepdensity,bs="tp")+
    s(frequency) + 
    s(word_length)+
    s(surprisal)+
    s(words, bs = "re"),
  data = data_1)

summary(model_data_1g4_gam)

##### data 2
data_2$words <- as.factor(data_2$words)
data_2$log_reading_time <- log(data_2$reading_time)

model_data_2g0_gam <- gam(
  log_reading_time ~ s(dependency_density,bs="tp",k=5)+s(words, bs = "re"),
  data = data_2)
summary(model_data_2g0_gam)

model_data_2g1_gam <- gam(
  log_reading_time ~ s(alphadepdensity,bs="tp")
  +s(words, bs = "re"),
  data = data_2)
summary(model_data_2g1_gam)

model_data_2g2_gam <- gam(
  log_reading_time ~ s(dependency_density,bs="tp",k=5) + 
    s(alphadepdensity,bs="tp")+s(words, bs = "re"),
  data = data_2)
summary(model_data_2g2_gam)
save_figure_gam(model_data_2g2_gam, "data2_dd.png",1)
save_figure_gam(model_data_2g2_gam, "data2_add.png",2)


model_data_2g3_gam <- gam(
  log_reading_time ~ 
    s(frequency,k=5) + 
    s(word_length,k=5)+
    s(surprisal,k=5)+
    s(words, bs = "re"),
  data = data_2)

summary(model_data_2g3_gam)

model_data_2g4_gam <- gam(
  log_reading_time ~ s(dependency_density,bs="tp",k=5) + 
    s(alphadepdensity,bs="tp")+
    s(frequency,k=5) + 
    s(word_length,k=5)+
    s(surprisal,k=5)+
    s(words, bs = "re"),
  data = data_2)

summary(model_data_2g4_gam)

##### data 3
data_3$words <- as.factor(data_3$words)
data_3$log_reading_time <- log(data_3$reading_time)

model_data_3g0_gam <- gam(
  log_reading_time ~ s(dependency_density,bs="tp",k=5)+s(words, bs = "re"),
  data = data_3
)
summary(model_data_3g0_gam)


model_data_3g1_gam <- gam(
  log_reading_time ~ s(alphadepdensity,bs="tp")+s(words, bs = "re"),
  data = data_3)
summary(model_data_3g1_gam)


model_data_3g2_gam <- gam(
  log_reading_time ~ s(dependency_density,bs="tp",k=5) + 
    s(alphadepdensity,bs="tp")
  + s(words, bs = "re"),
  data = data_3)
summary(model_data_3g2_gam)
save_figure_gam(model_data_3g2_gam, "data3_dd.png",1)
save_figure_gam(model_data_3g2_gam, "data3_add.png",2)

model_data_3g3_gam <- gam(
  log_reading_time ~ 
    s(frequency) + 
    s(word_length)+
    s(surprisal)+
    s(words, bs = "re"),
  data = data_3)

summary(model_data_3g3_gam)


model_data_3g4_gam <- gam(
  log_reading_time ~ s(dependency_density,bs="tp",k=5) + 
    s(alphadepdensity,bs="tp")+
    s(frequency) + 
    s(word_length)+
    s(surprisal)+
    s(words, bs = "re"),
  data = data_3)

summary(model_data_3g4_gam)

##############################################################################
anova(model_data_1g0_gam, model_data_1g1_gam, test="Chisq")
anova(model_data_2g0_gam, model_data_2g1_gam, test="Chisq")
anova(model_data_3g0_gam, model_data_3g1_gam, test="Chisq")


anova(model_data_1g3_gam, model_data_1g4_gam, test="Chisq")
anova(model_data_2g3_gam, model_data_2g4_gam, test="Chisq")
anova(model_data_3g3_gam, model_data_3g4_gam, test="Chisq")
lrtest(model_data_3g3_gam, model_data_3g4_gam)
##############################################################################
data_1 <- read.csv('../data/02_nsrts_with_trad_pred.csv')
data_2 <- read.csv("../data/02_brownrts_with_trad_pred.csv")
data_3 <- read.csv('../data/02_nsmaze_with_trad_pred.csv')


data_1 <- data_1 %>%
  group_by(item,sentence_id) %>%
  arrange(zone, .by_group = TRUE) %>%
  mutate(
    reading_time_next1 = lead(meanItemRT, 1),
    reading_time_next2 = lead(meanItemRT, 2),
    reading_time_next3 = lead(meanItemRT, 3)
  ) %>%
  ungroup()

data_2 <- data_2 %>%
  group_by(text_id,sentence_id) %>%
  arrange(code, .by_group = TRUE) %>%
  mutate(
    reading_time_next1 = lead(meanItemRT, 1),
    reading_time_next2 = lead(meanItemRT, 2),
    reading_time_next3 = lead(meanItemRT, 3)
  ) %>%
  ungroup()



# selected the columns that are needed
column_rt_name <- c("words","dependency_density","alphadepdensity",
                    "word_length","frequency","surprisal",
                    "reading_time","reading_time_next1","reading_time_next2","reading_time_next3")

columns_1 <- c("words","dependency_density","alphadepdensity",
               "word_length","frequency","surprisal",
               "meanItemRT","reading_time_next1","reading_time_next2","reading_time_next3")

data_1 <- data_1[columns_1]
colnames(data_1) <- column_rt_name

# summary(data_2)
data_2 <- data_2[columns_1]
colnames(data_2) <- column_rt_name


data_1 <- na.omit(data_1)
data_2 <- na.omit(data_2)

data_1$log_reading_time_next1 <- log(data_1$reading_time_next1)
data_1$log_reading_time_next2 <- log(data_1$reading_time_next2)
data_1$log_reading_time_next3 <- log(data_1$reading_time_next3)
data_2$log_reading_time_next1 <- log(data_2$reading_time_next1)
data_2$log_reading_time_next2 <- log(data_2$reading_time_next2)
data_2$log_reading_time_next3 <- log(data_2$reading_time_next3)

# num_of_variable: 1:dependency density; 2: alphadepdensity
save_figure_gam <- function(model, filename, num_of_variable) {
  
  if (num_of_variable == 1) {
    xlab = "Dependency density"
  } else {
    xlab = "AlphaDepDensity"
  }
  
  # Save the plot
  png(filename, height = 4, width = 3, units = "in", res = 750)
  plot(model, select = num_of_variable,xlab=xlab, ylab = "log(Reading time)",
       shade = TRUE, shade.col = scales::alpha(science_palette[num_of_variable], alpha = 0.3), 
       se = TRUE, scale = 0)
  dev.off()
}


science_palette <- brewer.pal(3, "Set1")  # 使用 Set1 调色板，支持科学绘图

data_1$words <- as.factor(data_1$words)
model_data_1g_next1_gam <- gam(
  log_reading_time_next1 ~ s(dependency_density,bs="tp",k=5) + 
    s(alphadepdensity,bs="tp")+s(words, bs = "re"),
  data = data_1,
  method="REML")
summary(model_data_1g_next1_gam)
save_figure_gam(model_data_1g_next1_gam, "data1_next1_dd.png",1)
save_figure_gam(model_data_1g_next1_gam, "data1_next1_add.png",2)


model_data_1g_next2_gam <- gam(
  log_reading_time_next2 ~ s(dependency_density,bs="tp",k=5) + 
    s(alphadepdensity,bs="tp")+s(words, bs = "re"),
  data = data_1,
  method="REML")
summary(model_data_1g_next2_gam)

save_figure_gam(model_data_1g_next2_gam, "data1_next2_dd.png",1)
save_figure_gam(model_data_1g_next2_gam, "data1_next2_add.png",2)

model_data_1g_next3_gam <- gam(
  log_reading_time_next3 ~ s(dependency_density,bs="tp",k=5) + 
    s(alphadepdensity,bs="tp")+s(words, bs = "re"),
  data = data_1,
  method="REML")
summary(model_data_1g_next3_gam)

save_figure_gam(model_data_1g_next3_gam, "data1_next3_dd.png",1)
save_figure_gam(model_data_1g_next3_gam, "data1_next3_add.png",2)

###########################################################################
data_2$words <- as.factor(data_2$words)
model_data_2g_next1_gam <- gam(
  log_reading_time_next1 ~ s(dependency_density,bs="tp",k=5) + 
    s(alphadepdensity,bs="tp")+s(words, bs = "re"),
  data = data_2,
  method="REML")
summary(model_data_2g_next1_gam)
save_figure_gam(model_data_2g_next1_gam, "data2_next1_dd.png",1)
save_figure_gam(model_data_2g_next1_gam, "data2_next1_add.png",2)


model_data_2g_next2_gam <- gam(
  log_reading_time_next2 ~ s(dependency_density,bs="tp",k=5) + 
    s(alphadepdensity,bs="tp")+s(words, bs = "re"),
  data = data_2,
  method="REML")
summary(model_data_2g_next2_gam)
save_figure_gam(model_data_2g_next2_gam, "data2_next2_dd.png",1)
save_figure_gam(model_data_2g_next2_gam, "data2_next2_add.png",2)


model_data_2g_next3_gam <- gam(
  log_reading_time_next3 ~ s(dependency_density,bs="tp",k=5) + 
    s(alphadepdensity,bs="tp")+s(words, bs = "re"),
  data = data_2,
  method="REML")
summary(model_data_2g_next3_gam)
save_figure_gam(model_data_2g_next3_gam, "data2_next3_dd.png",1)
save_figure_gam(model_data_2g_next3_gam, "data2_next3_add.png",2)


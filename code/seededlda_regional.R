# 加载library =================================================

required_packages <- c(
  "tm", "topicmodels", "tidytext", "dplyr", "tibble", "readr",
  "ggplot2", "data.table", "jiebaR", "slam", "seededlda",
  "quanteda", "tidyr", "purrr", "knitr", "kableExtra", "fixest",
  "cowplot", "igraph", "ggraph", "tidygraph", "stringr", "modelsummary", "gt"
)

# 自动安装函数

load_or_install_package <- function(package_name) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    install.packages(package_name, dependencies = TRUE)
  }
  suppressPackageStartupMessages(
    library(package_name, character.only = TRUE)
  )
}

# 批量加载
invisible(lapply(required_packages, load_or_install_package))

# 加载中文字体 =============================================================================================

library(showtext)
font_add(family = "my_font", regular = "/System/Library/Fonts/STHeiti Light.ttc")

# 启用 showtext 自动加载字体
showtext_auto()

# 省份翻译

province_translation <- c(
  "北京" = "Beijing", "天津" = "Tianjin", "上海" = "Shanghai", "重庆" = "Chongqing",
  "河北" = "Hebei", "山西" = "Shanxi", "辽宁" = "Liaoning", "吉林" = "Jilin",
  "黑龙江" = "Heilongjiang", "江苏" = "Jiangsu", "浙江" = "Zhejiang", "安徽" = "Anhui",
  "福建" = "Fujian", "江西" = "Jiangxi", "山东" = "Shandong", "河南" = "Henan",
  "湖北" = "Hubei", "湖南" = "Hunan", "广东" = "Guangdong", "海南" = "Hainan",
  "四川" = "Sichuan", "贵州" = "Guizhou", "云南" = "Yunnan", "陕西" = "Shaanxi",
  "甘肃" = "Gansu", "青海" = "Qinghai", "宁夏" = "Ningxia", "新疆" = "Xinjiang",
  "西藏" = "Tibet", "中国澳门" = "Macau", "中国香港" = "Hong Kong", "中国台湾" = "Taiwan",
  "内蒙古" = "Inner Mongolia", "广西" = "Guangxi"
)

# 读取GDP、人口、民族成分数据 ==========================================================

GDP <- fread("data/GDP.csv")

Provinces <- c("北京", "天津", "河北", "山西", "内蒙古", "辽宁", "吉林", "黑龙江", 
              "上海", "江苏", "浙江", "安徽", "福建", "江西", "山东", "河南", 
              "湖北", "湖南", "广东", "广西", "海南", "重庆", "四川", 
              "贵州", "云南", "西藏", "陕西", "甘肃", "青海", "宁夏", "新疆")

GDP[, 1] <- Provinces
colnames(GDP) <- as.character(c("Provinces", 2023:2015))

population <- fread("data/population.csv")
population[, 1] <- Provinces
colnames(population) <- as.character(c("Provinces", 2023:2015))

urbanization <- fread("data/urbanization.csv")
urbanization[, 1] <- Provinces
urbanization <- urbanization[, -2]
colnames(urbanization) <- as.character(c("Provinces", 2023:2015))

# Step 1: 对三个数据框都移除“地区”列，提取纯数值部分
gdp_data <- GDP[, -1]
pop_data <- population[, -1]
urb_data <- urbanization[, -1]

# Step 2: 计算人均 GDP（单位：每人）和城镇人口比例
gdp_per_capita <- round(gdp_data / pop_data,2)
urb_rate <- round((urb_data / pop_data) * 100,2)

# Step 3: 添加“地区”作为第一列，构造新的数据框
GDPpp <- cbind(Provinces = GDP$Provinces, gdp_per_capita)
urb <- cbind(Provinces = GDP$Provinces, urb_rate)

ethnic <- fread("data/ethinic.csv")
ethnic$minority_share <- round(ethnic$`少数民族_总` / ethnic$`总人口` * 100, 2)

gdp_2020 <- GDPpp[, c("Provinces", "2020")]
colnames(gdp_2020)[2] <- "gdp_per_capita_2020"
urb_2020 <- urb[, c("Provinces", "2020")]
colnames(urb_2020)[2] <- "urbanization_2020"
ethnic_share_2020 <- ethnic[, c("Provinces", "minority_share")]

# 合并三个数据框
supplement_df <- merge(gdp_2020, urb_2020, by = "Provinces")
supplement_df <- merge(supplement_df, ethnic_share_2020, by = "Provinces")

supplement_scaled <- scale(supplement_df[,-1])
supplement_scaled <- cbind(Provinces = supplement_df$Provinces, as.data.frame(supplement_scaled))


# 读取数据-转词矩阵 ==============================================================================

cleaned_comments_provinces <- fread("data/cleaned_comments_provinces.csv")
cleaned_comments_provinces$doc_id <- as.factor(cleaned_comments_provinces$doc_id)

# 处理包含所有评论的数据集（转化为词矩阵）
corpus <- Corpus(DataframeSource(cleaned_comments_provinces[, c("doc_id", "text")]))

dtm <- DocumentTermMatrix(corpus)
# 删除全是零的矩阵 # 目前还没搞明白为什么会出现全是零的矩阵，有两个doc都是
# row_totals <- slam::row_sums(dtm_all)
# dtm_all <- dtm_all[row_totals > 0, ]

# 转化成dfm
dfm <- as.dfm(dtm)

# seed_words_morecata <- list(
#    "Novel" = c("莉莉丝", "作者", "女主", "小说", "多琳", "男主", "辛西娅","玛利亚","角色"),
#   "TraditionalNorms" = c("生育", "结婚", "丈夫", "孩子", "妈妈", "母亲","婚姻","妻子", "思想", "家务","家庭"),
#   "Stereotype" = c("美","温柔","贤惠", "化妆","取悦","美丽","漂亮","审美","打扮","裙子","服美役","高跟鞋"),
#   "Class" = c("歧视","尊重","平等", "权利", "权力", "男女平等","平权","公平","底层", "剥削","阶级矛盾", "特权", "利益","资源","打工","压迫",
#               "阶级","经济","钱","收入","职业"),
#   "GenderCrimes" = c("骚扰", "性侵", "家暴", "拐卖", "受害者","拐骗","清白","打","暴力","网暴",
#                      "施暴者","伤害","侵犯","罪犯","新闻","案件","案*", "qj","辱骂", "被", "经历"),
#   "Homosexuality" = c("耽美", "女同", "同性恋", "LGBT", "男同", "百合","铜","姛","耽", "腐女", "性取向"),
#   "Statism" = c("国家.*政策", "集体主义", "国家.*意识形态", "体制内", "主旋律", "为国牺牲",
#                 "民族主义", "国家机器", "国家*利益", "自上而下", "政策*", "统治","意识形态","政治"),
#   "HateSpeech" = c("蝻", "蝈蝻", "蛆","牠","该杀","阉", "该死","杀*","杀.*男",
#                    "精神男人","辱女","男人.*死", "诅咒","男.*去死")
# )

seed_words <- list(
  "TraditionalNorms" = c("生育", "结婚", "丈夫", "孩子", "妈妈", "母亲","婚姻","妻子", "思想", "家务","家庭"),
  "Stereotype" = c("美","温柔","贤惠", "化妆","取悦","美丽","漂亮","审美","打扮","裙子","服美役","高跟鞋"),
  # "WomenRights" = c("歧视","尊重","平等", "权利", "权力", "男女平等","平权","公平"),
  "Class" = c("底层", "剥削","阶级矛盾","利益","资源","打工","资本主义","资产","教育","压迫","地位","平等",
              "男女平等","平权","阶级","经济","钱","收入","职业","赚钱","职业"),
  "GenderViolence" = c("性骚扰", "性侵", "家暴", "拐卖", "受害者","被打","事件","网暴","暴力","犯罪",
                       "施暴者","侵犯","罪犯","新闻","案件","案","强奸", "qj","法律","杀","奸夫","死"),
  #  "Homosexuality" = c("耽美", "女同", "同性恋", "LGBT", "男同", "百合","铜","姛","耽", "腐女", "性取向"),
  "Statism" = c("社会", "权力", "政策","父权","历史","中国","国家","政府",
                "民族主义", "国家利益", "自上而下", "国家政策", "意识形态","政治","制度"),
  # "HateSpeech" = c("蝻", "蝈蝻", "男.*去死","蛆","牠","该杀","阉", "该死","奸夫",
  #                  "精神男人","辱女","男人死", "诅咒","杀","骂"),
  "Novel" = c("莉莉丝", "公主","女主", "小说", "多琳", "男主", "辛西娅","玛利亚","角色","虫","捉个")
)

seed_dict <- dictionary(seed_words)
# 运行 Seeded LDA 模型
set.seed(1234)
lda_model <- textmodel_seededlda(
  x = dfm,         # 文档-词矩阵
  dictionary = seed_dict,  # 种子词典
  residual = T,     # 建立一个"剩余"主题，捕捉未分类词语
  batch_size = 0.1,      # 参与迭代的数据库
  auto_iter = TRUE,  # 自动迭代至收敛或达到最大迭代次数（默认max_iter=2000）
  verbose = TRUE # 输出每轮迭代情况，以便观察
)

# # 1. 计算种子词覆盖率
# doc_seed_coverage <- function(dfm, seed_words) {
#   coverage <- map_dbl(seed_words, function(words) {
#     word_match <- dfm_select(dfm, pattern = words, selection = "keep")
#     doc_covered <- rowSums(word_match) > 0
#     mean(doc_covered)  # 覆盖率：命中文档数 / 总文档数
#   })
#   return(coverage)
# }
# 
# # 2. 计算平均特征熵 (AFE)
# afe_calculation <- function(dfm, seed_words) {
#   afe_values <- map_dbl(seed_words, function(words) {
#     word_match <- dfm_select(dfm, pattern = words, selection = "keep")
#     co_occur_matrix <- as.matrix(word_match)
#     co_occur_freq <- colSums(co_occur_matrix) / sum(co_occur_matrix)
#     entropy <- -sum(co_occur_freq * log2(co_occur_freq + 1e-10))
#     return(entropy)
#   })
#   return(afe_values)
# }
# 
# perplexity(lda_model)
# divergence(lda_model)
# doc_seed_coverage(dfm, seed_words)
# afe_calculation(dfm, seed_words)

# 解读：
# - Coverage 值越高，说明该主题的种子词在文档中的覆盖率越高。
# - AFE 值越高，说明该主题种子词的共现分布更均匀，模型鲁棒性可能更强。


# 获取每个主题的前10个关键词
top_terms <- terms(lda_model, 30)
print(top_terms)


# ===================================================================================

doc_topics <- as.data.frame(lda_model$theta)
doc_topics$Location <- cleaned_comments_provinces$Location

doc_topics_long <- doc_topics %>%
  pivot_longer(
    cols = c(-Location),        # 除了Location列以外的所有列
    names_to = "Topic",
    values_to = "Topic_Prob"
  )

# 一些可视化的尝试

# 先做一个汇总，看看各省份/地区的平均话题分布差异：
topic_by_location <- doc_topics %>%
  group_by(Location) %>%
  summarise(
    across(
      everything(),
      ~ mean(.x, na.rm = TRUE)
    )
  )

# topic_by_location_long <- topic_by_location[,1:6] %>%
#   pivot_longer(cols = -Location, names_to = "Topic", values_to = "Proportion")
# 
# ggplot(topic_by_location_long, aes(x = Location, y = Proportion, fill = Topic)) +
#   geom_bar(stat = "identity", position = "stack") +
#   coord_flip() +  # 如果 Location 太多，水平放更美观
#   labs(title = "Topic Distribution by Location", x = "Location", y = "Mean Topic Proportion") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   scale_fill_brewer(palette = "Set3")+
#   theme_minimal()
# 
topic_by_location_long <- topic_by_location[, 1:6] %>%
  pivot_longer(-Location, names_to = "Topic", values_to = "MeanProb")
# 
# topic_by_location_long$enLocation <- ifelse(
#   topic_by_location_long$Location %in% names(province_translation),
#   province_translation[topic_by_location_long$Location],
#   topic_by_location_long$Location
# )
# 
# ggplot(topic_by_location_long, aes(x = Topic, y = enLocation, fill = MeanProb)) +
#   geom_tile() +
#   scale_fill_gradient(low = "white", high = "steelblue") +
#   labs(title = "Heat Map of Topics", x = "Topics", y = "Provinces") +
#   theme_minimal()
# 
# ===========================
# 分话题绘制地图

topic_by_location_map <- doc_topics %>%
  group_by(Location) %>%
  summarise(
    across(1:6, ~ mean(.x, na.rm = TRUE))  # 选择主题
  )

library(chinamap)
library(sf)
library(fuzzyjoin)

# 获取中国省级地图（返回的是 SpatialPolygonsDataFrame，可以转换为 sf 对象）
china_map <- get_map_china()
# china_map_sf <- st_as_sf(china_map)

china_map <- china_map %>%
  mutate(province_cleaned = case_when(
    province == "香港特别行政区" ~ "中国香港",
    province == "台湾省" ~ "中国台湾",
    TRUE ~ province  # 其他不变
  ))

# 使用 fuzzy_left_join 进行模糊匹配
merged_map <- fuzzy_left_join(
  china_map, topic_by_location_map,
  by = c("province_cleaned" = "Location"),
  match_fun = function(prov, loc) str_detect(prov, fixed(loc))
)

merged_map_full <- fuzzy_left_join(
  merged_map, supplement_df,
  by = c("Location" = "Provinces"),
  match_fun = function(prov, loc) str_detect(prov, fixed(loc))
)

rescale01 <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

merged_map_std <- merged_map_full %>%
  mutate(
    GenderViolence_scaled = rescale01(GenderViolence),
    TraditionalNorms_scaled = rescale01(TraditionalNorms),
    Stereotype_scaled = rescale01(Stereotype),
    Class_scaled = rescale01(Class),
    Statism_scaled = rescale01(Statism),
    gdp_scaled = rescale01(gdp_per_capita_2020),
    urb_scaled = rescale01(urbanization_2020),
    minority_scaled = rescale01(minority_share)
  )

# 提取统一色阶范围
vmin <- min(merged_map_std[, c("GenderViolence_scaled", "TraditionalNorms_scaled", "Stereotype_scaled", "Statism_scaled", "Class_scaled",
                           "gdp_scaled", "urb_scaled", "minority_scaled")], na.rm = TRUE)
vmax <- max(merged_map_std[, c("GenderViolence_scaled", "TraditionalNorms_scaled", "Stereotype_scaled", "Statism_scaled", "Class_scaled",
                               "gdp_scaled", "urb_scaled", "minority_scaled")], na.rm = TRUE)

g1 <- ggplot(merged_map_std, aes(x = long, y = lat, group = group, fill = GenderViolence_scaled)) +
  geom_polygon(color = "black", size = 0.2) +
  coord_fixed() +
  scale_fill_gradient(low = "lightyellow", high = "red", na.value = "grey90", limits = c(vmin, vmax)) +
  labs(title = "Avg. Attention to Gender Violence",
       x = NULL, y = NULL, 
        fill = "Scale") +
  theme_minimal() +
  theme(plot.title = element_text(size = 10)) 
g2 <- ggplot(merged_map_std, aes(x = long, y = lat, group = group, fill = TraditionalNorms_scaled)) +
  geom_polygon(color = "black", size = 0.2) +
  coord_fixed() +
  scale_fill_gradient(low = "lightyellow", high = "red", na.value = "grey90", limits = c(vmin, vmax)) +
  labs(title = "Avg. Attention to Traditional Norms",
       x = NULL, y = NULL, 
       fill = "Traditional Norms") +
  theme_minimal() +
  theme(plot.title = element_text(size = 10)) 
g3 <- ggplot(merged_map_std, aes(x = long, y = lat, group = group, fill = Stereotype_scaled)) +
  geom_polygon(color = "black", size = 0.2) +
  coord_fixed() +
  scale_fill_gradient(low = "lightyellow", high = "red", na.value = "grey90", limits = c(vmin, vmax)) +
  labs(title = "Avg. Attention to Stereotype",
       x = NULL, y = NULL, 
       fill = "Stereotype") +
  theme_minimal() +
  theme(plot.title = element_text(size = 10)) 
g4 <- ggplot(merged_map_std, aes(x = long, y = lat, group = group, fill = Statism_scaled)) +
  geom_polygon(color = "black", size = 0.2) +
  coord_fixed() +
  scale_fill_gradient(low = "lightyellow", high = "red", na.value = "grey90", limits = c(vmin, vmax)) +
  labs(title = "Avg. Attention to Statism",
#       x = "longitude", y = "latitude", 
               x = NULL, y = NULL, 
        fill = "Statism") +
  theme_minimal() +
  theme(plot.title = element_text(size = 10)) 
g5 <- ggplot(merged_map_std, aes(x = long, y = lat, group = group, fill = Class_scaled)) +
  geom_polygon(color = "black", size = 0.2) +
  coord_fixed() +
  scale_fill_gradient(low = "lightyellow", high = "red", na.value = "grey90", limits = c(vmin, vmax)) +
  labs(title = "Avg. Attention to Class",
      # x = "longitude", y = "latitude", 
      x = NULL, y = NULL, 
      fill = "Class") +
  theme_minimal() +
  theme(plot.title = element_text(size = 10)) 
g6 <- ggplot(merged_map_std, aes(x = long, y = lat, group = group, fill = gdp_scaled)) +
  geom_polygon(color = "black", size = 0.2) +
  coord_fixed() +
  scale_fill_gradient(low = "lightyellow", high = "red", na.value = "grey90", limits = c(vmin, vmax)) +
  labs(title = "GDP per capita in 2020",
       # x = "longitude", y = "latitude", 
       x = NULL, y = NULL, 
       fill = "gdp_scaled") +
  theme_minimal() +
  theme(plot.title = element_text(size = 10)) 
g7 <- ggplot(merged_map_std, aes(x = long, y = lat, group = group, fill = urb_scaled)) +
  geom_polygon(color = "black", size = 0.2) +
  coord_fixed() +
  scale_fill_gradient(low = "lightyellow", high = "red", na.value = "grey90", limits = c(vmin, vmax)) +
  labs(title = "Proportion of urban population in 2020",
       # x = "longitude", y = "latitude", 
       x = NULL, y = NULL, 
       fill = "urb_scaled") +
  theme_minimal() +
  theme(plot.title = element_text(size = 10)) 
g8 <- ggplot(merged_map_std, aes(x = long, y = lat, group = group, fill = minority_scaled)) +
  geom_polygon(color = "black", size = 0.2) +
  coord_fixed() +
  scale_fill_gradient(low = "lightyellow", high = "red", na.value = "grey90", limits = c(vmin, vmax)) +
  labs(title = "Proportion of ethnic minority population in 2020",
       # x = "longitude", y = "latitude", 
       x = NULL, y = NULL, 
       fill = "minority_scaled") +
  theme_minimal() +
  theme(plot.title = element_text(size = 10))
# ggplot(merged_map, aes(x = long, y = lat, group = group, fill = HateSpeech)) +
#   geom_polygon(color = "black", size = 0.2) +
#   coord_fixed() +
#   scale_fill_gradient(low = "lightyellow", high = "red", na.value = "grey90") +
#   labs(title = "Avg. Attention to Hate Speech by Province",
#        x = "longitude", y = "latitude", fill = "Hate Speech") +
#   theme_minimal()
# ggplot(merged_map, aes(x = long, y = lat, group = group, fill = Homosexuality)) +
#   geom_polygon(color = "black", size = 0.2) +
#   coord_fixed() +
#   scale_fill_gradient(low = "lightyellow", high = "red", na.value = "grey90") +
#   labs(title = "Avg. Attention to Homosexuality by Province",
#        x = "longitude", y = "latitude", fill = "Homosexuality") +
#   theme_minimal()

# 去掉图例的图
g1 <- g1 + theme(legend.position = "none")
g2 <- g2 + theme(legend.position = "none")
g3 <- g3 + theme(legend.position = "none")
g4 <- g4 + theme(legend.position = "none")
g5 <- g5 + theme(legend.position = "none")
g6 <- g6 + theme(legend.position = "none")
g7 <- g7 + theme(legend.position = "none")
g8 <- g8 + theme(legend.position = "none")

legend <- get_legend(
  g1 + theme(legend.position = "right")  # 只保留图例
)

# 拼图体
plot_grid(
  g1, g2, g3, g4, g5, g6, g7, g8,
  ncol = 2, align = "hv"
)

# clustering
topic_cluster_data <- topic_by_location %>%
  select(-Location,-Novel, -other) 

sse <- sapply(1:10, function(k) {
  kmeans(scale(topic_cluster_data), centers = k, nstart = 25)$tot.withinss
})

# 绘制肘部法则图
plot(1:10, sse, type = "b", xlab = "Number of Clusters", ylab = "Sum of Squared Errors", pch = 19)

# center = 4
data_scaled <- scale(topic_cluster_data)  # 注意确保这部分数据确实是需要聚类的特征
kmeans_result_4 <- kmeans(data_scaled, centers = 4, nstart = 25)
topic_by_location$cluster_4 <- as.factor(kmeans_result_4$cluster)

kmeans_result_6 <- kmeans(data_scaled, centers = 6, nstart = 25)
topic_by_location$cluster_6 <- as.factor(kmeans_result_6$cluster)

merged_map_cluster <- fuzzy_left_join(china_map, topic_by_location,
                                      by = c("province_cleaned" = "Location"),
                                      match_fun = function(prov, loc) str_detect(prov, fixed(loc)))

# 绘制聚类地图
p1 <- ggplot(merged_map_cluster, aes(x = long, y = lat, group = group, fill = cluster_4)) +
  geom_polygon(color = "black", size = 0.2) +
  coord_fixed() +
  scale_fill_brewer(palette = "Set3", na.value = "grey90") +
  labs(title = "Provinces Clustering (k=4)",
       x = "longitude", y = "latitude", fill = "Cluster") +
  theme_minimal()

p2 <- ggplot(merged_map_cluster, aes(x = long, y = lat, group = group, fill = cluster_6)) +
  geom_polygon(color = "black", size = 0.2) +
  coord_fixed() +
  scale_fill_brewer(palette = "Set3", na.value = "grey90") +
  labs(title = "Figure 6. Provinces Clustering (k=6)",
       x = "longitude", y = "latitude", fill = "Cluster") +
  theme_minimal()

# 去掉图例的图
p1_nolegend <- p1 + theme(legend.position = "none")
p2_nolegend <- p2 + theme(legend.position = "none")

# 拼图 + 加共享图例
plot_grid(p2, 
          # g6, g7, g8,
          rel_widths = c(1, 1))  # 控制图例宽度占比

topic_by_cluster <- topic_by_location %>%
  group_by(cluster_6) %>%
  summarise(
    GenderViolence = mean(GenderViolence),
    TraditionalNorms = mean(TraditionalNorms),
    Stereotype = mean(Stereotype),
    Statism = mean(Statism),
    Class = mean(Class)
  )

save_kable(
  kable(topic_by_cluster,
        caption = "Average Prob. of Topics by Clustering",
        digits = 2, align = "c") %>%
    kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))  %>%
    add_footnote(label = "Note: Values represent the average probability of each topic within the provinces assigned to each cluster.", notation = "none"),
  file = "Output_pic/table_cluster.png"
)


# run回归 =========================================
topics <- unique(topic_by_location_long$Topic)

doc_topics$Month <- cleaned_comments_provinces$Post_Time

regression_df <- doc_topics %>%
  pivot_longer(
    cols = c(-Location, -Month),        # 除了Location列以外的所有列
    names_to = "Topic",
    values_to = "Topic_Prob"
  )

regression_df$Month <- as.factor(format(as.Date(paste0(regression_df$Month, "-01")), "%Y-%m"))

# regression_df <- regression_df %>%
#   left_join(supplement_df, by = c("Location" = "Provinces")) %>%
#   drop_na()

# # 定义回归函数（以广东为基准省份）
# run_topic_regression <- function(topic_name, data) {
#   topic_data <- data %>%
#     filter(Topic == topic_name) %>%
#     mutate(Location = factor(Location, levels = c("Guangdong", setdiff(unique(Location), "Guangdong"))))
# 
#   model <- lm(Topic_Prob ~ Location, data = topic_data)
# 
#   summary(model)$coefficients %>%
#     as.data.frame() %>%
#     tibble::rownames_to_column("Variable") %>%
#     mutate(Topic = topic_name)
# }
# 
# # 批量运行
# library(purrr)
# regression_results <- map_df(topics, ~run_topic_regression(.x, doc_topics_long))
# head(regression_results)


# 创建模型列表（每个话题一个模型）

regression_df$enLocation <- ifelse(
  regression_df$Location %in% names(province_translation),
  province_translation[regression_df$Location],
  regression_df$Location
)

# models <- setNames(
#   lapply(topics, function(topic) {
#     topic_data <- regression_df %>%
#       filter(Topic == topic) %>%
#       mutate(enLocation = factor(enLocation, levels = c("Guangdong", setdiff(unique(enLocation), "Guangdong"))))
#     
#     lm(Topic_Prob ~ enLocation + Month,
#        data = topic_data)
#   }),
#   topics
# )


models <- setNames(
  lapply(topics, function(topic) {
    topic_data <- regression_df %>%
      filter(Topic == topic) %>%
      mutate(enLocation = factor(enLocation, levels = c("Guangdong", setdiff(unique(enLocation), "Guangdong"))))
    
    feols(Topic_Prob ~ enLocation | Month, data = topic_data)
  }),
  topics
)


gt_table <- modelsummary(
  models,
  coef_rename = function(x) gsub("enLocation", "", x),
  estimate = "{estimate}{stars}",
  statistic = "[{conf.low}, {conf.high}]",
  conf.int = TRUE,
  stars = TRUE,
  gof_omit = ".*",
  title = "Effect of Province on Topic Attention (Reference: Guangdong)",
  notes = "95% confidence intervals in brackets. *p < .05, **p < .01, ***p < .001.",
  output = "gt"
)

write.csv(gt_table, file = "regression.csv", row.names = FALSE)


# 保存为图片（需要 webshot 或 webshot2）
gtsave(gt_table, "Output_pic/regression_table.png")

# R1 <- modelplot(
#   models[[1]],
#   coef_omit = "Intercept",  # 可选：不显示截距项
#   coef_rename = function(x) gsub("enLocation", "", x),  # 清理变量名
#   conf_level = 0.95,
#   draw = TRUE
# ) +
#   # geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, y = term), color = "grey50", height = 0.1) +
#   # geom_point(aes(x = estimate, y = term), color = "steelblue", size = 1) +
#   theme_minimal() +
#   labs(
#     title = "Province Effects on Traditional Norms (95% CI)",
#     x = "Coefficient Estimate",
#     y = "Province"
#   )+
#   theme(
#     plot.title = element_text(size = 10),
#     axis.title = element_text(size = 10),
#     axis.text = element_text(size = 6),
#   )
# 
# R2 <- modelplot(
#   models[[2]],
#   coef_omit = "Intercept",  # 可选：不显示截距项
#   coef_rename = function(x) gsub("enLocation", "", x),  # 清理变量名
#   conf_level = 0.95,
#   draw = TRUE
# ) +
#   # geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, y = term), color = "grey50", height = 0.1) +
#   # geom_point(aes(x = estimate, y = term), color = "steelblue", size = 1) +
#   theme_minimal() +
#   labs(
#     title = "Province Effects on Stereotype (95% CI)",
#     x = "Coefficient Estimate",
#     y = "Province"
#   )+
#   theme(
#     plot.title = element_text(size = 10),
#     axis.title = element_text(size = 10),
#     axis.text = element_text(size = 6),
#   )
# 
# R3 <- modelplot(
#   models[[3]],
#   coef_omit = "Intercept",  # 可选：不显示截距项
#   coef_rename = function(x) gsub("enLocation", "", x),  # 清理变量名
#   conf_level = 0.95,
#   draw = TRUE
# ) +
#   # geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, y = term), color = "grey50", height = 0.1) +
#   # geom_point(aes(x = estimate, y = term), color = "steelblue", size = 1) +
#   theme_minimal() +
#   labs(
#     title = "Province Effects on Class (95% CI)",
#     x = "Coefficient Estimate",
#     y = "Province"
#   )+
#   theme(
#     plot.title = element_text(size = 10),
#     axis.title = element_text(size = 10),
#     axis.text = element_text(size = 6),
#   )
# 
# R4 <- modelplot(
#   models[[4]],
#   coef_omit = "Intercept",  # 可选：不显示截距项
#   coef_rename = function(x) gsub("enLocation", "", x),  # 清理变量名
#   conf_level = 0.95,
#   draw = TRUE
# ) +
#   # geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, y = term), color = "grey50", height = 0.1) +
#   # geom_point(aes(x = estimate, y = term), color = "steelblue", size = 1) +
#   theme_minimal() +
#   labs(
#     title = "Province Effects on GenderViolence (95% CI)",
#     x = "Coefficient Estimate",
#     y = "Province"
#   )+
#   theme(
#     plot.title = element_text(size = 10),
#     axis.title = element_text(size = 10),
#     axis.text = element_text(size = 6),
#   )
# 
# R5 <- modelplot(
#   models[[5]],
#   coef_omit = "Intercept",  # 可选：不显示截距项
#   coef_rename = function(x) gsub("enLocation", "", x),  # 清理变量名
#   conf_level = 0.95,
#   draw = TRUE
# ) +
#   # geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, y = term), color = "grey50", height = 0.05) +
#   # geom_point(aes(x = estimate, y = term), color = "steelblue", size = 0.5) +
#   theme_minimal() +
#   labs(
#     title = "Province Effects on Statism (95% CI)",
#     x = "Coefficient Estimate",
#     y = "Province"
#   ) +
#   theme(
#     plot.title = element_text(size = 10),
#     axis.title = element_text(size = 10),
#     axis.text = element_text(size = 6),
#   )

df1 <- get_estimates(models[[1]], conf_level = 0.95) %>%
  filter(term != "(Intercept)") %>%
  mutate(term = gsub("enLocation", "", term)) %>%
  mutate(significance = ifelse(p.value < 0.05, "Significant", "Not Significant")) %>%
  arrange(estimate) %>%
  mutate(term = factor(term, levels = unique(term)))  # 排序 y 轴

# 画图
R1 <- ggplot(df1, aes(x = estimate, y = term)) +
  geom_point(aes(shape = significance), size = 2, fill = "steelblue", color = "steelblue") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.15, color = "grey50") +
  scale_shape_manual(values = c("Significant" = 16, "Not Significant" = 1)) +  # 实心 vs 空心
  theme_minimal() +
  labs(
    title = "Province Effects on Traditional Norms (95% CI)",
    x = "Coefficient Estimate",
    y = "Province",
    shape = NULL
  ) +
  theme(
    plot.title = element_text(size = 10),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 6),
    legend.position = "none"
  )

df2 <- get_estimates(models[[2]], conf_level = 0.95) %>%
  filter(term != "(Intercept)") %>%
  mutate(term = gsub("enLocation", "", term)) %>%
  mutate(significance = ifelse(p.value < 0.05, "Significant", "Not Significant")) %>%
  arrange(estimate) %>%
  mutate(term = factor(term, levels = unique(term)))  # 排序 y 轴

# 画图
R2 <- ggplot(df2, aes(x = estimate, y = term)) +
  geom_point(aes(shape = significance), size = 2, fill = "steelblue", color = "steelblue") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.15, color = "grey50") +
  scale_shape_manual(values = c("Significant" = 16, "Not Significant" = 1)) +  # 实心 vs 空心
  theme_minimal() +
  labs(
    title = "Province Effects on Stereotype (95% CI)",
    x = "Coefficient Estimate",
    y = "Province",
    shape = NULL
  ) +
  theme(
    plot.title = element_text(size = 10),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 6),
    legend.position = "none"
  )

df3 <- get_estimates(models[[3]], conf_level = 0.95) %>%
  filter(term != "(Intercept)") %>%
  mutate(term = gsub("enLocation", "", term)) %>%
  mutate(significance = ifelse(p.value < 0.05, "Significant", "Not Significant")) %>%
  arrange(estimate) %>%
  mutate(term = factor(term, levels = unique(term)))  # 排序 y 轴

# 画图
R3 <- ggplot(df3, aes(x = estimate, y = term)) +
  geom_point(aes(shape = significance), size = 2, fill = "steelblue", color = "steelblue") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.15, color = "grey50") +
  scale_shape_manual(values = c("Significant" = 16, "Not Significant" = 1)) +  # 实心 vs 空心
  theme_minimal() +
  labs(
    title = "Province Effects on Class (95% CI)",
    x = "Coefficient Estimate",
    y = "Province",
    shape = NULL
  ) +
  theme(
    plot.title = element_text(size = 10),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 6),
    legend.position = "none"
  )

df4 <- get_estimates(models[[4]], conf_level = 0.95) %>%
  filter(term != "(Intercept)") %>%
  mutate(term = gsub("enLocation", "", term)) %>%
  mutate(significance = ifelse(p.value < 0.05, "Significant", "Not Significant")) %>%
  arrange(estimate) %>%
  mutate(term = factor(term, levels = unique(term)))  # 排序 y 轴

# 画图
R4 <- ggplot(df4, aes(x = estimate, y = term)) +
  geom_point(aes(shape = significance), size = 2, fill = "steelblue", color = "steelblue") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.15, color = "grey50") +
  scale_shape_manual(values = c("Significant" = 16, "Not Significant" = 1)) +  # 实心 vs 空心
  theme_minimal() +
  labs(
    title = "Province Effects on Gender Violence (95% CI)",
    x = "Coefficient Estimate",
    y = "Province",
    shape = NULL
  ) +
  theme(
    plot.title = element_text(size = 10),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 6),
    legend.position = "none"
  )

df5 <- get_estimates(models[[5]], conf_level = 0.95) %>%
  filter(term != "(Intercept)") %>%
  mutate(term = gsub("enLocation", "", term)) %>%
  mutate(significance = ifelse(p.value < 0.05, "Significant", "Not Significant")) %>%
  arrange(estimate) %>%
  mutate(term = factor(term, levels = unique(term)))  # 排序 y 轴

# 画图
R5 <- ggplot(df5, aes(x = estimate, y = term)) +
  geom_point(aes(shape = significance), size = 2, fill = "steelblue", color = "steelblue") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.15, color = "grey50") +
  scale_shape_manual(values = c("Significant" = 16, "Not Significant" = 1)) +  # 实心 vs 空心
  theme_minimal() +
  labs(
    title = "Province Effects on Statism (95% CI)",
    x = "Coefficient Estimate",
    y = "Province",
    shape = NULL
  ) +
  theme(
    plot.title = element_text(size = 10),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 6),
    legend.position = "none"
  )

plot_grid(
  R1,R2,R3,R4,R5,
  ncol = 2, align = "hv"
)


topic_Class <- regression_df %>%
  filter(Topic == "Class")  # 换成你想研究的话题
#  group_by(Location) %>%
#  summarise(topic_prob = mean(Topic_Prob, na.rm = TRUE))

topic_Class_df <- topic_Class %>%
  left_join(supplement_scaled, by = c("Location" = "Provinces")) |> 
  drop_na()

model_Class <- feols(
  Topic_Prob ~ minority_share + gdp_per_capita_2020 + urbanization_2020 | Month,
  data = topic_Class_df,
  cluster = ~Location
)
# model_Class <- lm(Topic_Prob ~ minority_share + gdp_per_capita_2020 + urbanization_2020, data = topic_Class_df)

summary(model_Class)

topic_Statism <- regression_df %>%
  filter(Topic == "Statism")  # 换成你想研究的话题
  # group_by(Location) %>%
  # summarise(topic_prob = mean(Topic_Prob, na.rm = TRUE))

topic_Statism_df <- topic_Statism %>%
  left_join(supplement_scaled, by = c("Location" = "Provinces")) |> 
  drop_na()

# model_Statism <- lm(Topic_Prob ~ minority_share + gdp_per_capita_2020 + urbanization_2020, data = topic_Statism_df)
model_Statism <- feols(
  Topic_Prob ~ minority_share + gdp_per_capita_2020 + urbanization_2020 | Month,
  data = topic_Statism_df,
  cluster = ~Location
)
summary(model_Statism)

topic_GenderViolence <- regression_df %>%
  filter(Topic == "GenderViolence")  # 换成你想研究的话题
  # group_by(Location) %>%
  # summarise(topic_prob = mean(Topic_Prob, na.rm = TRUE))

topic_GenderViolence_df <- topic_GenderViolence %>%
  left_join(supplement_scaled, by = c("Location" = "Provinces")) |> 
  drop_na()

# model_GenderViolence <- lm(Topic_Prob ~ minority_share + gdp_per_capita_2020 + urbanization_2020, data = topic_GenderViolence_df)
model_GenderViolence <- feols(
  Topic_Prob ~ minority_share + gdp_per_capita_2020 + urbanization_2020 | Month,
  data = topic_GenderViolence_df,
  cluster = ~Location
)
summary(model_GenderViolence)

topic_Stereotype <- regression_df %>%
  filter(Topic == "Stereotype")  # 换成你想研究的话题
  # group_by(Location) %>%
  # summarise(topic_prob = mean(Topic_Prob, na.rm = TRUE))

topic_Stereotype_df <- topic_Stereotype %>%
  left_join(supplement_scaled, by = c("Location" = "Provinces")) |> 
  drop_na()

model_Stereotype <- feols(
  Topic_Prob ~ minority_share + gdp_per_capita_2020 + urbanization_2020 | Month,
  data = topic_Stereotype_df,
  cluster = ~Location
)
summary(model_Stereotype)

topic_TraditionalNorms <- regression_df %>%
  filter(Topic == "TraditionalNorms")  # 换成你想研究的话题
  # group_by(Location) %>%
  # summarise(topic_prob = mean(Topic_Prob, na.rm = TRUE))

topic_TraditionalNorms_df <- topic_TraditionalNorms %>%
  left_join(supplement_scaled, by = c("Location" = "Provinces")) |> 
  drop_na()

# model_TraditionalNorms <- lm(Topic_Prob ~ minority_share + gdp_per_capita_2020 + urbanization_2020, 
#                              data = topic_TraditionalNorms_df)
model_TraditionalNorms <- feols(
  Topic_Prob ~ minority_share + gdp_per_capita_2020 + urbanization_2020 | Month,
  data = topic_TraditionalNorms_df,
  cluster = ~Location
)
summary(model_TraditionalNorms)

modelsummary(
  list(
    "Class" = model_Class,
    "Statism" = model_Statism,
    "Gender Violence" = model_GenderViolence,
    "Stereotype" = model_Stereotype,
    "Traditional Norms" = model_TraditionalNorms
  ),
  coef_rename = c(
    "minority_share" = "Minority Share",
    "gdp_per_capita_2020" = "GDP per Capita",
    "urbanization_2020" = "Urbanization"
  ),
  # estimate = "{estimate}{stars}",
  # statistic = "[{conf.low}, {conf.high}]",
  conf.int = T,
  stars = TRUE,
  gof_omit = "^(?!Num\\.Obs\\.).*",  # 正则：隐藏除了 Num.Obs. 的所有指标
  title = "Table 2. Regression of Topic Attention on Regional Variables",
  notes = "Note: Missing Hong Kong data. 95% CI in brackets. *p < .05, **p < .01, ***p < .001."
)

modelsummary(
  list(
    "Class" = model_Class,
    "Statism" = model_Statism,
    "Gender Violence" = model_GenderViolence,
    "Stereotype" = model_Stereotype,
    "Traditional Norms" = model_TraditionalNorms
  ),
  coef_rename = c(
    "minority_share" = "Minority Share",
    "gdp_per_capita_2020" = "GDP per Capita",
    "urbanization_2020" = "Urbanization"
  ),
  conf.int = TRUE,
  stars = TRUE,
  gof_omit = "^(?!Num\\.Obs\\.).*",  # 正则：隐藏除了 Num.Obs. 的所有指标
  title = "Table 2. Regression of Topic Attention on Regional Variables",
  notes = "Note: Missing Hong Kong data. 95% CI in brackets. *p < .05, **p < .01, ***p < .001.",
  output = "regression_table.csv"  # <<-- 指定导出为 CSV 文件
)

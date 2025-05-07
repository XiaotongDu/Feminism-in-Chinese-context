# library(tm)
# library(topicmodels)
# library(tidytext)
# library(dplyr)
# library(tibble)
# library(readr)
# library(ggplot2)
# library(data.table)
# library(jiebaR)
# library(slam)
# library(seededlda)
# library(quanteda)
# library(tidyr)
# library(purrr)
# library(knitr)
# library(kableExtra)
# library(cowplot)
# library(igraph)
# library(ggraph)
# library(tidygraph)

# 加载library =================================================

required_packages <- c(
  "tm", "topicmodels", "tidytext", "dplyr", "tibble", "readr",
  "ggplot2", "data.table", "jiebaR", "slam", "seededlda",
  "quanteda", "tidyr", "purrr", "knitr", "kableExtra",
  "cowplot", "igraph", "ggraph", "tidygraph", "lubridate", "forcats"
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

# 回填地址后的subset数据建模 ===================================================

filled_comments <- fread("data/cleaned_comments_provinces.csv")

corpus_sub <- Corpus(DataframeSource(filled_comments[, c("doc_id", "text")]))

dtm_sub <- DocumentTermMatrix(corpus_sub)

# 转化成dfm
dfm_sub <- as.dfm(dtm_sub)

set.seed(1234)
lda_model_sub <- textmodel_seededlda(
  x = dfm_sub,         # 文档-词矩阵
  #  valuetype = "regex",           # 模糊匹配
  dictionary = seed_dict,  # 种子词典
  residual = T,     # 建立一个"剩余"主题，捕捉未分类词语
  batch_size = 0.1,      # 参与迭代的数据库
  auto_iter = TRUE,  # 自动迭代至收敛或达到最大迭代次数（默认max_iter=2000）
  verbose = TRUE # 输出每轮迭代情况，以便观察
)

doc_topics_sub <- as.data.frame(lda_model_sub$theta)

top_contriwords <- terms(lda_model_sub, 30)

kable(top_contriwords, 
      caption = "Top 30 Words Contributing to Topics") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE, 
                position = "center")

# doc_topics_sub$Post_Month <- filled_comments$Post_Month
# doc_topics_sub_long <- doc_topics_sub[, !names(doc_topics_sub) %in% c("Novel","other")] %>%
#   pivot_longer(
#     cols = -Post_Month,
#     names_to = "Topic",
#     values_to = "Prob"
#   )
# 
# topic_by_month_sub <- doc_topics_sub_long %>%
#   group_by(Post_Month, Topic) %>%
#   summarise(Avg_Prob = mean(Prob, na.rm = TRUE), .groups = "drop")
# 
# # 按时间顺序排序：
# topic_by_month_sub <- topic_by_month_sub %>%
#   arrange(Post_Month)
# 
# # 可视化
# ggplot(topic_by_month_sub, aes(x = Post_Month, y = Avg_Prob, color = Topic, group = Topic)) +
#   geom_smooth(se = FALSE, linewidth = 0.8, method = "loess", span = 0.5) +  # 平滑线
#   geom_point(size = 1) +
#   labs(title = "Comments' Topic Attention Over Time",
#        x = "Post Month",
#        y = "Average Topic Probability",
#        color = "Topic") +
#   theme_minimal(base_size = 10) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 按照时间进行可视化，看话题变化趋势

doc_topics_sub$Post_Month <- filled_comments$Post_Month
doc_topics_sub$Likes <- filled_comments$Likes
doc_topics_sub$doc_id <- as.character(filled_comments$doc_id)
doc_topics_sub_long <- doc_topics_sub[, !names(doc_topics_sub) %in% c("Novel", "other")] %>%
  pivot_longer(
    cols = -c(Post_Month, Likes, doc_id),
    names_to = "Topic",
    values_to = "Prob"
  )

# 不加权平均
topic_raw <- doc_topics_sub_long %>%
  group_by(Post_Month, Topic) %>%
  summarise(Avg_Prob = mean(Prob, na.rm = TRUE), .groups = "drop") %>%
  mutate(Weight = "Unweighted")

# 加权平均
topic_weighted <- doc_topics_sub_long %>%
  group_by(Post_Month, Topic) %>%
  summarise(Avg_Prob = weighted.mean(Prob, w = Likes, na.rm = TRUE), .groups = "drop") %>%
  mutate(Weight = "Weighted")

# 合并两组
topic_compare <- bind_rows(topic_raw, topic_weighted)

ggplot(topic_compare, aes(x = Post_Month, y = Avg_Prob, color = Topic, group = Topic)) +
  geom_smooth(se = FALSE, linewidth = 0.8, method = "loess", span = 0.5) +
  geom_point(size = 1) +
  facet_wrap(~ Weight, ncol = 1) +
  labs(
    title = "Figure 3. Topic Attention Over Time (Unweighted vs. Weighted by Likes)",
    x = "Post Month",
    y = "Average Topic Probability",
    color = "Topic"
  ) +
  theme_minimal(base_size = 9) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 每一个主题的构成随时间变化 ===============

months <- sort(unique(doc_topics_sub$Post_Month))

terms_by_topic <- terms(lda_model_sub, 30)

## 1. TN ========
topic_terms <- terms_by_topic[, "TraditionalNorms"]

TraditionalNorms_trend <- data.frame()

for (m in months) {
  label <- m
  
  sub_df <- doc_topics_sub %>% filter(Post_Month == m, TraditionalNorms > 0.4)
  ids <- as.character(sub_df$doc_id)
  if (length(ids) < 5) next
  
  sub_dfm <- dfm_sub[ids, ]
  matched_terms <- intersect(topic_terms, colnames(sub_dfm))
  freq_vec <- colSums(sub_dfm[, matched_terms, drop = FALSE])
  total_tokens <- sum(sub_dfm)
  
  df_m <- data.frame(
    term = matched_terms,
    freq = as.numeric(freq_vec),
    prop = as.numeric(freq_vec) / total_tokens,
    month = label  # ← 字符型，直接可用于图表
  )
  
  TraditionalNorms_trend <- rbind(TraditionalNorms_trend, df_m)
}

selected_terms <- c("女儿", "妈妈", "婚姻", "生育","父亲","工作")
term_labels_TN <- c("女儿" = "Daughter", 
                    #"妻子" = "Wife", 
                    "妈妈" = "Mother",
                    "婚姻" = "Marriage", 
                    "生育" = "Childbirth", 
                    "父亲" = "Father", 
                    "工作" = "Work")

TraditionalNorms_plot <- TraditionalNorms_trend %>%
  filter(term %in% selected_terms) %>%
  mutate(term_en = term_labels_TN[term])

a1 <- ggplot(TraditionalNorms_plot, aes(x = month, y = prop, color = term_en, group = term_en)) +
  geom_smooth(se = FALSE, linewidth = 0.5, method = "loess", span = 0.3) +
  geom_point(size = 0.5) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Top Terms Trend in Traditional Norms", x = NULL, y = NULL, color = "Top Terms") +
  theme_minimal(base_size = 8) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## 2. stereotype ======

topic_terms <- terms_by_topic[, "Stereotype"]
Stereotype_trend <- data.frame()
for (m in months) {
  label <- m
  
  sub_df <- doc_topics_sub %>% filter(Post_Month == m, Stereotype > 0.4)
  ids <- as.character(sub_df$doc_id)
  if (length(ids) < 5) next
  
  sub_dfm <- dfm_sub[ids, ]
  matched_terms <- intersect(topic_terms, colnames(sub_dfm))
  freq_vec <- colSums(sub_dfm[, matched_terms, drop = FALSE])
  total_tokens <- sum(sub_dfm)
  
  df_m <- data.frame(
    term = matched_terms,
    freq = as.numeric(freq_vec),
    prop = as.numeric(freq_vec) / total_tokens,
    month = label  # ← 字符型，直接可用于图表
  )
  
  Stereotype_trend <- rbind(Stereotype_trend, df_m)
}

selected_terms <- c("美", "化妆", "自由", "服美役", "温柔")
term_labels_ST <- c(
  "美" = "Beauty",
  "化妆" = "Makeup",
  "自由" = "Freedom",
  #  "取悦" = "Please Others",
  "服美役" = "Beauty Servitude",
  "温柔" = "Gentle"
  #  "肌肉" = "Muscle",
  #  "审美" = "Aesthetics",
  #  "身体" = "Body"
)

Stereotype_plot <- Stereotype_trend %>%
  filter(term %in% selected_terms) %>%
  mutate(term_en = term_labels_ST[term])

a2 <- ggplot(Stereotype_plot, aes(x = month, y = prop, color = term_en, group = term_en)) +
  geom_smooth(se = FALSE, linewidth = 0.5, method = "loess", span = 0.3) +
  geom_point(size = 0.5) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Top Terms Trend in Stereotype", x = NULL, y = NULL, color = "Top Terms") +
  theme_minimal(base_size = 8) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## 3. Class ======

topic_terms <- terms_by_topic[, "Class"]
Class_trend <- data.frame()
for (m in months) {
  label <- m
  
  sub_df <- doc_topics_sub %>% filter(Post_Month == m, Class > 0.4)
  ids <- as.character(sub_df$doc_id)
  if (length(ids) < 5) next
  
  sub_dfm <- dfm_sub[ids, ]
  matched_terms <- intersect(topic_terms, colnames(sub_dfm))
  freq_vec <- colSums(sub_dfm[, matched_terms, drop = FALSE])
  total_tokens <- sum(sub_dfm)
  
  df_m <- data.frame(
    term = matched_terms,
    freq = as.numeric(freq_vec),
    prop = as.numeric(freq_vec) / total_tokens,
    month = label  # ← 字符型，直接可用于图表
  )
  
  Class_trend <- rbind(Class_trend, df_m)
}

selected_terms <- c("阶级", "钱", "资源", "教育", "平等","利益")
term_labels_CL <- c(
  "阶级" = "Class",
  #  "压迫" = "Oppression",
  "钱" = "Money",
  "资源" = "Resources",
  "教育" = "Education",
  "平等" = "Equality",
  #  "地位" = "Status",
  "利益" = "Interests"
)

Class_plot <- Class_trend %>%
  filter(term %in% selected_terms) %>%
  mutate(term_en = term_labels_CL[term])

a3 <- ggplot(Class_plot, aes(x = month, y = prop, color = term_en, group = term_en)) +
  geom_smooth(se = FALSE, linewidth = 0.5, method = "loess", span = 0.3) +
  geom_point(size = 0.5) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Top Terms Trend in Class", x = NULL, y = NULL, color = "Top Terms") +
  theme_minimal(base_size = 8) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## 4. statism =====

topic_terms <- terms_by_topic[, "Statism"]
Statism_trend <- data.frame()
for (m in months) {
  label <- m
  
  sub_df <- doc_topics_sub %>% filter(Post_Month == m, Statism > 0.4)
  ids <- as.character(sub_df$doc_id)
  if (length(ids) < 5) next
  
  sub_dfm <- dfm_sub[ids, ]
  matched_terms <- intersect(topic_terms, colnames(sub_dfm))
  freq_vec <- colSums(sub_dfm[, matched_terms, drop = FALSE])
  total_tokens <- sum(sub_dfm)
  
  df_m <- data.frame(
    term = matched_terms,
    freq = as.numeric(freq_vec),
    prop = as.numeric(freq_vec) / total_tokens,
    month = label  # ← 字符型，直接可用于图表
  )
  
  Statism_trend <- rbind(Statism_trend, df_m)
}

selected_terms <- c("父权", "历史", "社会", "权力", "思想")
term_labels_ST <- c(
  "父权" = "Patriarchy",
  "历史" = "History",
  "社会" = "Society",
  "权力" = "Power",
  #  "现实" = "Reality",
  #  "爱情" = "Love",
  #  "世界" = "World",
  "思想" = "Ideology"
)

Statism_plot <- Statism_trend %>%
  filter(term %in% selected_terms) %>%
  mutate(term_en = term_labels_ST[term])

a4 <- ggplot(Statism_plot, aes(x = month, y = prop, color = term_en, group = term_en)) +
  geom_smooth(se = FALSE, linewidth = 0.5, method = "loess", span = 0.3) +
  geom_point(size = 0.5) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Top Terms Trend in Statism", x = NULL, y = NULL, color = "Top Terms") +
  theme_minimal(base_size = 8) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## 5. genderviolence =====

topic_terms <- terms_by_topic[, "GenderViolence"]
GenderViolence_trend <- data.frame()
for (m in months) {
  label <- m
  
  sub_df <- doc_topics_sub %>% filter(Post_Month == m, GenderViolence > 0.4)
  ids <- as.character(sub_df$doc_id)
  if (length(ids) < 5) next
  
  sub_dfm <- dfm_sub[ids, ]
  matched_terms <- intersect(topic_terms, colnames(sub_dfm))
  freq_vec <- colSums(sub_dfm[, matched_terms, drop = FALSE])
  total_tokens <- sum(sub_dfm)
  
  df_m <- data.frame(
    term = matched_terms,
    freq = as.numeric(freq_vec),
    prop = as.numeric(freq_vec) / total_tokens,
    month = label  # ← 字符型，直接可用于图表
  )
  
  GenderViolence_trend <- rbind(GenderViolence_trend, df_m)
}

selected_terms <- c("受害者", "家暴", "娇妻", "蝻","恶心")
term_labels_GV <- c(
  "受害者" = "Victim",
  "家暴" = "Domestic Violence",
  #  "案件" = "Case",
  #  "杀" = "Kill",
  "娇妻" = "Doting Wife (Misogyny)",
  #  "死" = "Death",
  #  "暴力" = "Violence",
  "蝻" = "Scumbag (Misandry)",
  #  "牠" = "It (Dehumanizing Male)",
  "恶心" = "Disgusting"
)

GenderViolence_plot <- GenderViolence_trend %>%
  filter(term %in% selected_terms) %>%
  mutate(term_en = term_labels_GV[term])

a5 <- ggplot(GenderViolence_plot, aes(x = month, y = prop, color = term_en, group = term_en)) +
  geom_smooth(se = FALSE, linewidth = 0.5, method = "loess", span = 0.3) +
  geom_point(size = 0.5) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Top Terms Trend in Gender Violence", x = NULL, y = NULL, color = "Top Terms") +
  theme_minimal(base_size = 8) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_grid(
  a1, a2, a3, a4, a5, 
  ncol = 2, align = "hv"
)

# kable种子词表===========================================
# 转化成英文展示

# seed_word_map <- list(
#   TraditionalNorms = c("生育" = "Childbearing", "结婚" = "Marriage", "丈夫" = "Husband", "孩子" = "Children", "妈妈" = "Mom", 
#                        "母亲" = "Mother", "婚姻" = "Marriage institution", "妻子" = "Wife", "思想" = "Ideology", 
#                        "家务" = "Housework", "家庭" = "Family"),
#   Stereotype = c("美" = "Beauty", "温柔" = "Gentleness", "贤惠" = "Virtuous", "化妆" = "Makeup", "取悦" = "To please",
#                  "美丽" = "Beautiful", "漂亮" = "Pretty", "审美" = "Aesthetics", "打扮" = "Dress up", "裙子" = "Skirt", 
#                  "服美役" = "Performing femininity", "高跟鞋" = "High heels"),
#   WomenRights = c("歧视" = "Discrimination", "尊重" = "Respect", "平等" = "Equality", "权利" = "Rights", 
#                   "权力" = "Power", "男女平等" = "Gender equality", "平权" = "Equal rights", "公平" = "Fairness"),
#   Class = c("底层" = "Underclass", "剥削" = "Exploitation", "阶级矛盾" = "Class conflict", "特权" = "Privilege", 
#             "利益" = "Interests", "资源" = "Resources", "打工" = "Wage labor", "压迫" = "Oppression", 
#             "阶级" = "Class", "经济" = "Economy", "钱" = "Money", "收入" = "Income", "职业" = "Occupation"),
#   GenderCrimes = c("骚扰" = "Harassment", "性侵" = "Sexual assault", "家暴" = "Domestic violence", "拐卖" = "Trafficking", 
#              "受害者" = "Victim", "拐骗" = "Luring", "清白" = "Purity", "打" = "To beat", 
#              "暴力.*事件" = "Violent incident", "网暴" = "Cyberbullying", "施暴者" = "Perpetrator", 
#              "伤害" = "Harm", "侵犯" = "Violate", "罪犯" = "Criminal", "新闻" = "News", 
#              "案件" = "Case", "案" = "Case", "qj*" = "Rape (pinyin)", "辱骂" = "Abuse", 
#              "被" = "Passive marker", "经历" = "Experience"),
#   # Homosexuality = c("耽美" = "Boys' Love", "女同" = "Lesbian", "同性恋" = "Homosexuality", "LGBT" = "LGBT", 
#   #                "男同" = "Gay", "百合" = "Girls' Love", "铜" = "Fujoshi slang", "姛" = "BL fan", 
#   #                "耽" = "Short for BL", "腐女" = "Fujoshi", "性取向" = "Sexual orientation"),
#   HateSpeech = c("蝻" = "Slur for men", "蝈蝻" = "Insult for men", "蛆" = "Maggot", "牠" = "Dehumanizing 'it'", 
#                  "该杀" = "Should be killed", "阉" = "Castrate", "该死" = "Should die", 
#                  "杀*" = "Kill (wildcard)", "杀男" = "Kill men", "精神男人" = "Mentally male", 
#                  "辱女" = "Misogynistic", "男人.*死" = "Men should die", "诅咒" = "Curse", 
#                  "男.*去死" = "Men go die")
# )

seed_word_map <- list(
  TraditionalNorms = c(
    "生育" = "Childbearing",
    "结婚" = "Marriage",
    "丈夫" = "Husband",
    "孩子" = "Children",
    "妈妈" = "Mom",
    "母亲" = "Mother",
    "婚姻" = "Marriage institution",
    "妻子" = "Wife",
    "家务" = "Housework",
    "家庭" = "Family"
  ),
  
  Stereotype = c(
    "美" = "Beauty",
    "温柔" = "Gentleness",
    "贤惠" = "Virtuous",
    "化妆" = "Makeup",
    "取悦" = "To please",
    "美丽" = "Beautiful",
    "漂亮" = "Pretty",
    "审美" = "Aesthetics",
    "打扮" = "Dress up",
    "裙子" = "Skirt",
    "服美役" = "Performing femininity",
    "高跟鞋" = "High heels"
  ),
  
  Class = c(
    "底层" = "Underclass",
    "剥削" = "Exploitation",
    "阶级矛盾" = "Class conflict",
    "利益" = "Interests",
    "资源" = "Resources",
    "打工" = "Wage labor",
    "资本主义" = "Capitalism",
    "资产" = "Assets",
    "教育" = "Education",
    "压迫" = "Oppression",
    "地位" = "Social status",
    "平等" = "Equality",
    "男女平等" = "Gender equality",
    "平权" = "Equal rights",
    "阶级" = "Class",
    "经济" = "Economy",
    "钱" = "Money",
    "收入" = "Income",
    "职业" = "Occupation",
    "赚钱" = "Make money"
  ),
  
  GenderViolence = c(
    "性骚扰" = "Sexual harassment",
    "性侵" = "Sexual assault",
    "家暴" = "Domestic violence",
    "拐卖" = "Trafficking",
    "受害者" = "Victim",
    "事件" = "Incident",
    "网暴" = "Cyberbullying",
    "暴力" = "Violence",
    "犯罪" = "Crime",
    "施暴者" = "Perpetrator",
    "侵犯" = "Violation",
    "罪犯" = "Criminal",
    "案件" = "Case",
    "强奸" = "Rape",
    "qj" = "Rape (pinyin abbreviation)",
    "法律" = "Law",
    "杀" = "Kill",
    "奸夫" = "Rapist",
    "死" = "Death"
  ),
  
  Statism = c(
    "社会" = "Society",
    "权力" = "Power",
    "政策" = "Policy",
    "父权" = "Patriarchy",
    "历史" = "History",
    "中国" = "China",
    "国家" = "State",
    "政府" = "Government",
    "民族主义" = "Nationalism",
    "国家利益" = "National interests",
    "自上而下" = "Top-down",
    "国家政策" = "State policy",
    "意识形态" = "Ideology",
    "政治" = "Politics",
    "制度" = "System"
  ),
  
  Novel = c(
    "莉莉丝" = "Lilith",
    "公主" = "Princess",
    "女主" = "Female lead",
    "小说" = "Novel",
    "多琳" = "Doreen",
    "男主" = "Male lead",
    "辛西娅" = "Cynthia",
    "玛利亚" = "Maria",
    "角色" = "Character",
    "虫" = "Insect (metaphor)",
    "捉个" = "Capture (in novel context)"
  )
)

# 把它们转为 tibble 表格
seed_words_df <- tibble(
  Topics = names(seed_word_map),
  `Seeded Words` = sapply(seed_word_map, function(pairs) {
    paste0(names(pairs), " (", pairs, ")", collapse = ", ")
  })
)

write.csv(seed_words_df, file = "seeds.csv", row.names = FALSE)


kable(seed_words_df, 
      caption = "Figure 2. Seeded Word Dictionary") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE, 
                position = "center")

# library(webshot)
# 
# # 先保存表格为 HTML 再转为 PNG
# save_kable(
#   kable(seed_words_df, 
#         caption = "Figure 2. Seeded Word Dictionary") %>%
#     kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
#                   full_width = FALSE, 
#                   position = "center"),
#   file = "Output_pic/seedwords.png"
# )

# 种子词稳定性测试    =====================================================

# 在高频词中的命中率
for (topic in names(seed_words)) {
  hits <- seed_words[[topic]] %in% terms_by_topic
  cat(topic, "命中率:", sum(hits), "/", length(hits), "\n")
}

# 评估种子词的表现

# 1. 计算种子词覆盖率
doc_seed_coverage <- function(dfm, seed_words) {
  coverage <- map_dbl(seed_words, function(words) {
    word_match <- dfm_select(dfm, pattern = words, selection = "keep")
    doc_covered <- rowSums(word_match) > 0
    mean(doc_covered)  # 覆盖率：命中文档数 / 总文档数
  })
  return(coverage)
}

# 2. 计算平均特征熵 (AFE)
afe_calculation <- function(dfm, seed_words) {
  afe_values <- map_dbl(seed_words, function(words) {
    word_match <- dfm_select(dfm, pattern = words, selection = "keep")
    co_occur_matrix <- as.matrix(word_match)
    co_occur_freq <- colSums(co_occur_matrix) / sum(co_occur_matrix)
    entropy <- -sum(co_occur_freq * log2(co_occur_freq + 1e-10))
    return(entropy)
  })
  return(afe_values)
}

# 定义不同的比例
proportions <- c(0.3, 0.5, 0.8, 1.0)

# 用于存储结果的数据框
results <- data.frame(
  proportion = proportions,
  perplexity = NA,
  divergence = NA
)

# 用列表保存每个比例下的种子词覆盖率和 AFE（可以进一步汇总，比如求均值）
coverage_list <- list()
afe_list <- list()

# 对于每个比例
for (p in proportions) {
  # 对每个主题的种子词随机抽取指定比例
  seed_words_sub <- lapply(seed_words, function(words) {
    # 计算抽样数量（四舍五入保证至少抽取1个）
    n <- ceiling(length(words) * p)
    sample(words, size = n)
  })
  
  # 构造新的种子词典
  seed_dict_test <- dictionary(seed_words_sub)
  
  # 固定随机种子，保证每次结果可重复
  set.seed(1234)
  
  # 运行 Seeded LDA 模型
  lda_model <- textmodel_seededlda(
    x = dfm_sub,              # 文档-词矩阵
    dictionary = seed_dict_test,  # 使用抽样后的种子词典
    residual = TRUE,      # 建立 "其他" 主题捕捉未归类词语
    valuetype = "regex",           # 模糊匹配
    batch_size = 0.1,
    auto_iter = TRUE,
    verbose = TRUE
  )
  
  # 记录指标
  results$perplexity[results$proportion == p] <- perplexity(lda_model_sub)
  results$divergence[results$proportion == p] <- divergence(lda_model_sub)
  
  # 保存种子词覆盖率和 AFE 结果
  coverage_list[[as.character(p)]] <- doc_seed_coverage(dfm_sub, seed_words_sub)
  afe_list[[as.character(p)]] <- afe_calculation(dfm_sub, seed_words_sub)
}

# 查看不同种子词比例下的量化指标
print(results)

# 将 coverage_list 转换为数据框
coverage_df <- do.call(rbind, lapply(names(coverage_list), function(p) {
  data.frame(
    proportion = as.numeric(p),
    topic = names(coverage_list[[p]]),
    coverage = as.numeric(coverage_list[[p]])
  )
}))

afe_df <- do.call(rbind, lapply(names(afe_list), function(p) {
  data.frame(
    proportion = as.numeric(p),
    topic = names(afe_list[[p]]),
    AFE = as.numeric(afe_list[[p]])
  )
}))


# 绘制折线图
p1 <- ggplot(coverage_df, aes(x = proportion, y = coverage, color = topic)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = c(0.3, 0.5, 0.8, 1.0)) +
  labs(title = "Seed Word Coverage vs Seed Word Proportion",
       x = "Proportion of Seed Words",
       y = "Seed Word Coverage",
       color = "Topic") +
  theme_minimal()

# 绘制折线图
p2 <- ggplot(afe_df, aes(x = proportion, y = AFE, color = topic)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = c(0.3, 0.5, 0.8, 1.0)) +
  labs(title = "AFE vs Seed Word Proportion",
       x = "Proportion of Seed Words",
       y = "Average Feature Entropy (AFE)",
       color = "Topic") +
  theme_minimal()

# 去掉图例的图
p1_nolegend <- p1 + theme(legend.position = "none")
p2_nolegend <- p2 + theme(legend.position = "none")

# 提取一个图的图例
legend <- get_legend(p1 + theme(legend.position = "right"))

# 拼图 + 加共享图例
plot_grid(p1_nolegend, p2_nolegend, legend,
          ncol = 3,
          rel_widths = c(1, 1, 0.3))  # 控制图例宽度占比

# # 查看话题共现  =====================================
# 
# #查看我关注的7个话题的共现情况 
# doc_topics_co_occur <- doc_topics_all[, 1:5]
# 
# # 设置阈值，认为某主题在文档中显著（例如大于 0.2）
# theta_threshold <- 0.2
# 
# # 对每条文档判断是否属于某主题（TRUE/FALSE）
# binary_matrix <- doc_topics_co_occur > theta_threshold
# 
# # 共现矩阵：计算任意两个主题在同一文档中同时为 TRUE 的次数
# co_occur_mat <- t(binary_matrix) %*% as.matrix(binary_matrix)
# 
# # 转为比例共现（占总文档数比例）
# co_occur_prop <- co_occur_mat / nrow(doc_topics_co_occur)
# 
# # 查看
# print(co_occur_prop)
# 
# 
# rowSums(binary_matrix) |> table()
# 
# # 用你已有的共现矩阵（用比例或次数都可以）
# # 这里假设你已有 co_occur_mat 或 co_occur_prop
# mat <- co_occur_prop  # 共现比例矩阵（也可以用 co_occur_mat 计数版本）
# 
# # 将对角线（自己和自己）设为 0，不参与分析
# diag(mat) <- 0
# 
# # threshold <- 0.02
# # mat_filtered <- mat
# # mat_filtered[mat_filtered < threshold] <- 0
# # g_filtered <- graph_from_adjacency_matrix(as.matrix(mat_filtered), mode = "undirected", weighted = TRUE, diag = FALSE)
# # g_tbl <- as_tbl_graph(g_filtered)
# 
# g <- graph_from_adjacency_matrix(as.matrix(mat), mode = "undirected", weighted = TRUE, diag = FALSE)
# g_tbl <- as_tbl_graph(g)
# 
# 
# set.seed(42)  # 固定布局
# 
# ggraph(g_tbl, layout = 'fr') +
#   geom_edge_link(aes(width = weight, alpha = weight), color = "gray30") +
#   geom_node_point(size = 3, color = 'steelblue') +
#   geom_node_text(aes(label = name), repel = TRUE, size = 4) +
#   scale_edge_width(range = c(0.5, 2)) +
#   theme_void() +
#   labs(title = "Topic Co-occurrence Network")

# # 读取数据-转词矩阵 ==============================================================================
# 
# cleaned_comments <- fread("data/cleaned_comments.csv")
# 
# # 处理包含所有评论的数据集（转化为词矩阵）
# corpus_all <- Corpus(DataframeSource(cleaned_comments[, c("doc_id", "text")]))
# 
# dtm_all <- DocumentTermMatrix(corpus_all)
# # 删除全是零的矩阵 # 目前还没搞明白为什么会出现全是零的矩阵，有两个doc都是
# # row_totals <- slam::row_sums(dtm_all)
# # dtm_all <- dtm_all[row_totals > 0, ]
# 
# # 转化成dfm
# dfm_all <- as.dfm(dtm_all)
# 
# # corpus_all <- VCorpus(DataframeSource(cleaned_comments[, c("doc_id", "text")]))
# # 
# # # 自定义空格分词 tokenizer
# # space_tokenizer <- function(x) unlist(strsplit(as.character(x), " "))
# # 
# # # 构建 DTM：此时 tokenizer 才会被正确调用
# # dtm_all <- DocumentTermMatrix(corpus_all, control = list(tokenize = space_tokenizer))
# 
# # 设置种子词典 ========================================================
# 
# # 获取前500高频词，用以参考
# term_freq_all <- slam::col_sums(dfm_all)  # 获取词汇的总频次
# term_freq_sorted_all <- sort(term_freq_all, decreasing = TRUE)  # 对词频进行降序排序
# 
# topwords_all <- head(names(term_freq_sorted_all), 500)
# 
# seed_words <- list(
#   "TraditionalNorms" = c("生育", "结婚", "丈夫", "孩子", "妈妈", "母亲","婚姻","妻子", "家务","家庭"),
#   "Stereotype" = c("美","温柔","贤惠", "化妆","取悦","美丽","漂亮","审美","打扮","裙子","服美役","高跟鞋"),
#   # "WomenRights" = c("歧视","尊重","平等", "权利", "权力", "男女平等","平权","公平"),
#   "Class" = c("底层", "剥削","阶级矛盾","利益","资源","打工","资本主义","资产","教育","压迫","地位","平等",
#               "男女平等","平权","阶级","经济","钱","收入","职业","赚钱","职业"),
#   "GenderViolence" = c("性骚扰", "性侵", "家暴", "拐卖", "受害者","暴力","犯罪",
#                "施暴者","侵犯","罪犯","案件","强奸", "qj","法律","杀","奸夫","死"),
# #  "Homosexuality" = c("耽美", "女同", "同性恋", "LGBT", "男同", "百合","铜","姛","耽", "腐女", "性取向"),
#   "Statism" = c("社会", "权力", "政策","父权","历史","中国","国家","政府",
#                 "民族主义", "国家利益", "自上而下", "国家政策", "意识形态","政治","制度"),
#   # "HateSpeech" = c("蝻", "蝈蝻", "男.*去死","蛆","牠","该杀","阉", "该死","奸夫",
#   #                  "精神男人","辱女","男人死", "诅咒","杀","骂"),
#   "Novel" = c("莉莉丝", "公主","女主", "小说", "多琳", "男主", "辛西娅","玛利亚","角色","虫","捉个")
# )
# 
# seed_words_morecata  <- list(
#   "TraditionalNorms" = c("生育", "结婚", "丈夫", "孩子", "妈妈", "母亲","婚姻","妻子", "家务","家庭"),
#   "Stereotype" = c("美","温柔","贤惠", "化妆","取悦","美丽","漂亮","审美","打扮","裙子","服美役","高跟鞋"),
#   # "WomenRights" = c("歧视","尊重","平等", "权利", "权力", "男女平等","平权","公平"),
#   "Class" = c("底层", "剥削","阶级矛盾","利益","资源","打工","资本主义","资产","教育","压迫","地位","平等",
#               "男女平等","平权","阶级","经济","钱","收入","职业","赚钱","职业"),
#   "GenderViolence" = c("性骚扰", "性侵", "家暴", "拐卖", "受害者","暴力","犯罪",
#                        "施暴者","侵犯","罪犯","案件","强奸", "qj","法律","杀","奸夫","死"),
#    "Homosexuality" = c("耽美", "女同", "同性恋", "LGBT", "男同", "百合","铜","姛","耽", "腐女", "性取向"),
#   "Statism" = c("社会", "权力", "政策","父权","历史","中国","国家","政府",
#                 "民族主义", "国家利益", "自上而下", "国家政策", "意识形态","政治","制度"),
#   "HateSpeech" = c("蝻", "蝈蝻", "男.*去死","蛆","牠","该杀","阉", "该死","奸夫",
#                    "精神男人","辱女","男人死", "诅咒","杀","骂"),
#   "Novel" = c("莉莉丝", "公主","女主", "小说", "多琳", "男主", "辛西娅","玛利亚","角色","虫","捉个")
# )
# 
# seed_dict <- dictionary(seed_words)
# 
# # 开始建模seeded LDA 全部数据===============================================
# 
# set.seed(1234)
# lda_model_all <- textmodel_seededlda(
#   x = dfm_all,         # 文档-词矩阵
# #  valuetype = "regex",           # 模糊匹配
#   dictionary = seed_dict,  # 种子词典
#   residual = T,     # 建立一个"剩余"主题，捕捉未分类词语
#   batch_size = 0.1,      # 参与迭代的数据库
#   auto_iter = TRUE,  # 自动迭代至收敛或达到最大迭代次数（默认max_iter=2000）
#   verbose = TRUE # 输出每轮迭代情况，以便观察
# )
# 
# doc_topics_all <- as.data.frame(lda_model_all$theta)
# 
# # 高频词共现情况（废弃） ==================================================
# 
# # comments_topic <- bind_cols(doc_topics_all, cleaned_comments)
# # 
# # # 添加季度列（你也可以改成按月）
# # comments_topic <- comments_topic %>%
# #   mutate(quarter = lubridate::floor_date(Post_Time, unit = "quarter"))
# # 
# # # 筛选高相关性文本
# # gv_docs <- comments_topic %>%
# #   filter(Statism > 0.4)
# # 
# # # 获取所有季度（确保顺序）
# # unique_quarters <- sort(unique(gv_docs$quarter))
# # quarter_labels <- paste0(year(unique_quarters), "Q", quarter(unique_quarters))
# # 
# # word_freq_list <- list()
# # 
# # for (i in seq_along(unique_quarters)) {
# #   q <- unique_quarters[i]
# #   label <- paste0(year(q), "Q", quarter(q))  # ← 提前创建字符型 label
# #   
# #   df_q <- gv_docs %>% filter(quarter == q)
# #   doc_ids <- df_q$doc_id
# #   valid_doc_ids <- intersect(as.character(doc_ids), rownames(dfm_all))
# #   sub_dfm <- dfm_all[valid_doc_ids, ]
# #   
# #   top_terms <- topfeatures(sub_dfm, 50)
# #   word_freq_q <- data.frame(
# #     term = names(top_terms),
# #     freq = as.numeric(top_terms),
# #     quarter_label = label  # 直接保存字符格式
# #   )
# #   
# #   word_freq_list[[label]] <- word_freq_q
# # }
# # 
# # all_word_freqs <- bind_rows(word_freq_list)
# # 
# # quarter_total_tokens <- list()
# # 
# # for (i in seq_along(unique_quarters)) {
# #   q <- unique_quarters[i]
# #   label <- paste0(year(q), "Q", quarter(q))
# #   df_q <- gv_docs %>% filter(quarter == q)
# #   doc_ids <- df_q$doc_id
# #   valid_doc_ids <- intersect(as.character(doc_ids), rownames(dfm_all))
# #   sub_dfm <- dfm_all[valid_doc_ids, ]
# #   
# #   total_tokens <- sum(colSums(sub_dfm))
# #   quarter_total_tokens[[label]] <- total_tokens
# # }
# # 
# # # 2. 获取所有季度 top10 词（你已有）
# # top_terms_all <- all_word_freqs %>%
# #   group_by(quarter_label) %>%
# #   slice_max(order_by = freq, n = 30, with_ties = FALSE) %>%
# #   ungroup() %>%
# #   distinct(term) %>%
# #   pull(term)
# # 
# # # 3. 构建词比例数据框（加入标准化）
# # norm_df <- all_word_freqs %>%
# #   filter(term %in% top_terms_all) %>%
# #   rowwise() %>%
# #   mutate(prop = freq / quarter_total_tokens[[quarter_label]]) %>%
# #   ungroup() %>%
# #   complete(term, quarter_label, fill = list(prop = 0)) %>%
# #   mutate(term = fct_reorder(term, -prop, .fun = max))  # 按最大值排序词
# # 
# # top_terms_ranked <- norm_df %>%
# #   group_by(term) %>%
# #   summarise(max_prop = max(prop)) %>%
# #   arrange(desc(max_prop))
# # 
# # selected_violence <- c("杀","恶心","现实","蝻","反抗","受害者")
# # plot_violence <- norm_df %>% filter(term %in% selected_violence)
# # 
# # selected_class <- c("阶级","利益","发展","资源","平权")
# # plot_class <- norm_df %>% filter(term %in% selecte_class)
# # 
# # selected_statism <- c("社会","环境","现实","爱情","世界")
# # plot_statism <- norm_df %>% filter(term %in% selected_statism)
# # 
# # selected_TN <- c("孩子","妈妈","结婚","工作","钱","社会")
# # plot_TN <- norm_df %>% filter(term %in% selected_TN)
# # 
# # selected_stereo <- c("自由","化妆","肌肉","社会","美","服美役")
# # plot_stereo <- norm_df %>% filter(term %in% selected_stereo)
# # 
# # 
# # # 4. 绘图
# # 
# # ggplot(plot_violence, aes(x = quarter_label, y = prop, color = term, group = term)) +
# #   geom_smooth(se = FALSE, linewidth = 0.8, method = "loess", span = 0.5) +  # 平滑线
# #   geom_point(size = 2) +
# #   scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.1))) +
# #   labs(
# #     title = "GenderViolence主题关键词占比随时间变化（折线图）",
# #     x = "季度",
# #     y = "词占比",
# #     color = "关键词"
# #   ) +
# #   theme_minimal(base_size = 14) +
# #   theme(
# #     axis.text.x = element_text(angle = 45, hjust = 1),
# #     panel.grid.minor = element_blank()
# #   )
# # 
# # ggplot(plot_class, aes(x = quarter_label, y = prop, color = term, group = term)) +
# #   geom_smooth(se = FALSE, linewidth = 0.8, method = "loess", span = 0.5) +  # 平滑线
# #   geom_point(size = 2) +
# #   scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.1))) +
# #   labs(
# #     title = "Class主题关键词占比随时间变化（折线图）",
# #     x = "季度",
# #     y = "词占比",
# #     color = "关键词"
# #   ) +
# #   theme_minimal(base_size = 14) +
# #   theme(
# #     axis.text.x = element_text(angle = 45, hjust = 1),
# #     panel.grid.minor = element_blank()
# #   )
# # 
# # ggplot(plot_statism, aes(x = quarter_label, y = prop, color = term, group = term)) +
# #   geom_smooth(se = FALSE, linewidth = 0.8, method = "loess", span = 0.5) +  # 平滑线
# #   geom_point(size = 2) +
# #   scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.1))) +
# #   labs(
# #     title = "Statism主题关键词占比随时间变化（折线图）",
# #     x = "季度",
# #     y = "词占比",
# #     color = "关键词"
# #   ) +
# #   theme_minimal(base_size = 14) +
# #   theme(
# #     axis.text.x = element_text(angle = 45, hjust = 1),
# #     panel.grid.minor = element_blank()
# #   )
# # 
# # ggplot(plot_TN, aes(x = quarter_label, y = prop, color = term, group = term)) +
# #   geom_smooth(se = FALSE, linewidth = 0.8, method = "loess", span = 0.5) +  # 平滑线
# #   geom_point(size = 2) +
# #   scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.1))) +
# #   labs(
# #     title = "TraditionalNorms主题关键词占比随时间变化（折线图）",
# #     x = "季度",
# #     y = "词占比",
# #     color = "关键词"
# #   ) +
# #   theme_minimal(base_size = 14) +
# #   theme(
# #     axis.text.x = element_text(angle = 45, hjust = 1),
# #     panel.grid.minor = element_blank()
# #   )
# # 
# # ggplot(plot_stereo, aes(x = quarter_label, y = prop, color = term, group = term)) +
# #   geom_smooth(se = FALSE, linewidth = 0.8, method = "loess", span = 0.5) +  # 平滑线
# #   geom_point(size = 2) +
# #   scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.1))) +
# #   labs(
# #     title = "Stereotype主题关键词占比随时间变化（折线图）",
# #     x = "季度",
# #     y = "词占比",
# #     color = "关键词"
# #   ) +
# #   theme_minimal(base_size = 14) +
# #   theme(
# #     axis.text.x = element_text(angle = 45, hjust = 1),
# #     panel.grid.minor = element_blank()
# #   )
# 
# # 调整模型 =====================================================
# 
# # 检查每个类别的前30个词和共现情况
# terms(lda_model_all, 30)
# 
# # 检查doc具体内容 ===============================
# # 检查每个topic概率最高的前10个doc
# # 假设你的文本在 cleaned_comments$Content
# text_data <- cleaned_comments$Content
# 
# # 创建空列表存储每个主题的Top文本
# top_docs_by_topic <- list()
# 
# # 遍历所有主题列
# for (topic_name in colnames(doc_topics_all)) {
#   top_indices <- order(doc_topics_all[[topic_name]], decreasing = TRUE)[1:10]
#   top_texts <- text_data[top_indices]
#   
#   top_docs_by_topic[[topic_name]] <- data.frame(
#     Rank = 1:10,
#     Theta = doc_topics_all[[topic_name]][top_indices],
#     Text = top_texts
#   )
# }
# 
# # 检查种子词典的表现   ==========================================================
# 
# # perplexity(lda_model_all)
# # divergence(lda_model_all)
# # # 1. 计算种子词覆盖率
# # doc_seed_coverage <- function(dfm, seed_words) {
# #   coverage <- map_dbl(seed_words, function(words) {
# #     word_match <- dfm_select(dfm, pattern = words, selection = "keep")
# #     doc_covered <- rowSums(word_match) > 0
# #     mean(doc_covered)  # 覆盖率：命中文档数 / 总文档数
# #   })
# #   return(coverage)
# # }
# # 
# # # 2. 计算平均特征熵 (AFE)
# # afe_calculation <- function(dfm, seed_words) {
# #   afe_values <- map_dbl(seed_words, function(words) {
# #     word_match <- dfm_select(dfm, pattern = words, selection = "keep")
# #     co_occur_matrix <- as.matrix(word_match)
# #     co_occur_freq <- colSums(co_occur_matrix) / sum(co_occur_matrix)
# #     entropy <- -sum(co_occur_freq * log2(co_occur_freq + 1e-10))
# #     return(entropy)
# #   })
# #   return(afe_values)
# # }
# # doc_seed_coverage(dfm_all, seed_words)
# # afe_calculation(dfm_all, seed_words)
# 
# 
# # 可视化结果  =============================================
# 
# # 按照时间进行可视化，看话题变化趋势
# 
# # doc_topics_all$Post_Month <- cleaned_comments$Post_Month
# # doc_topics_all$Likes <- cleaned_comments$Likes
# # doc_topics_all_long <- doc_topics_all[, !names(doc_topics_all) %in% c("Novel", "other")] %>%
# #   pivot_longer(
# #     cols = -c(Post_Month, Likes),
# #     names_to = "Topic",
# #     values_to = "Prob"
# #   )
# # 
# # # 不加权平均
# # topic_raw <- doc_topics_all_long %>%
# #   group_by(Post_Month, Topic) %>%
# #   summarise(Avg_Prob = mean(Prob, na.rm = TRUE), .groups = "drop") %>%
# #   mutate(Weight = "Unweighted")
# # 
# # # 加权平均
# # topic_weighted <- doc_topics_all_long %>%
# #   group_by(Post_Month, Topic) %>%
# #   summarise(Avg_Prob = weighted.mean(Prob, w = Likes, na.rm = TRUE), .groups = "drop") %>%
# #   mutate(Weight = "Weighted")
# # 
# # # 合并两组
# # topic_compare <- bind_rows(topic_raw, topic_weighted)
# # 
# # ggplot(topic_compare, aes(x = Post_Month, y = Avg_Prob, color = Topic, group = Topic)) +
# #   geom_smooth(se = FALSE, linewidth = 0.8, method = "loess", span = 0.5) +
# #   geom_point(size = 1) +
# #   facet_wrap(~ Weight, ncol = 1) +
# #   labs(
# #     title = "Topic Attention Over Time (Unweighted vs. Weighted by Likes)",
# #     x = "Post Month",
# #     y = "Average Topic Probability",
# #     color = "Topic"
# #   ) +
# #   theme_minimal(base_size = 10) +
# #   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# 
# topic_by_month <- doc_topics_all_long %>%
#   group_by(Post_Month, Topic) %>%
#   summarise(Avg_Prob = mean(Prob, na.rm = TRUE), .groups = "drop")
# 
# # 按时间顺序排序：
# topic_by_month <- topic_by_month %>%
#   arrange(Post_Month)
# 
# # 可视化
# ggplot(topic_by_month, aes(x = Post_Month, y = Avg_Prob, color = Topic, group = Topic)) +
#   geom_smooth(se = FALSE, linewidth = 0.8, method = "loess", span = 0.5) +  # 平滑线
#   geom_point(size = 1) +
#   labs(title = "Comments' Topic Attention Over Time",
#        x = "Post Month",
#        y = "Average Topic Probability",
#        color = "Topic") +
#   theme_minimal(base_size = 8) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))




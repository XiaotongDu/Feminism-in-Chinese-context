library(tm)
library(topicmodels)
library(tidytext)
library(dplyr)
library(tibble)
library(readr)
library(ggplot2)
library(data.table)
library(jiebaR)
library(slam)

# =============================================================================================
# 加载中文字体

library(showtext)
font_add(family = "my_font", regular = "/System/Library/Fonts/STHeiti Light.ttc")

# 启用 showtext 自动加载字体
showtext_auto()

# =============================================================================================

# 建立停用词词库
# 读取两个停用词文档
cn_stopwords <- readLines("cn_stopwords.txt", encoding = "UTF-8")
scu_stopwords <- readLines("scu_stopwords.txt", encoding = "UTF-8")

# 合并去重
combined_stopwords <- unique(c(cn_stopwords, scu_stopwords))

# 去除非汉字字符（保留汉字）
combined_stopwords <- gsub("[^\\p{Han}]", "", combined_stopwords, perl = TRUE)

# 去除空行（如果有的话）
combined_stopwords <- combined_stopwords[combined_stopwords != ""]

# 保存合并后的文件
writeLines(combined_stopwords, "merged_stopwords.txt", useBytes = TRUE)

# =============================================================================================

all_comments <- fread("all_comments.csv")

# 数据清洗
# 定义需要移除的关键词
keywords_to_remove <- c("营养液", "浇浇水", "捉个虫","更新", "催更", "元旦快乐",  "新年快乐")

# 清洗数据：删除包含关键词的行
cleaned_comments <- all_comments %>%
  filter(!is.na(Comment_ID)) %>%
  mutate(
    Likes = ifelse(is.na(Likes), 0, Likes),
    cleaned_comments = trimws(Content),
    cleaned_comments = gsub("[^\\p{Han}]", " ", cleaned_comments, perl = TRUE),  # 非汉字替换为空格
    cleaned_comments = gsub("[[:punct:]]+", " ", cleaned_comments),  # 标点符号替换为空格
    cleaned_comments = gsub("\\s+", " ", cleaned_comments)  # 多个空格合并成一个空格
  ) |>  
  filter(!(grepl(paste(keywords_to_remove, collapse = "|"), cleaned_comments, ignore.case = TRUE) &
             nchar(cleaned_comments) < 150)) |> 
  filter(!grepl("实名认证", cleaned_comments)) |> 
  distinct(cleaned_comments, .keep_all = TRUE) %>%
  filter(nchar(cleaned_comments) >= 2)

# 对评论进行分词
jieba <- worker(bylines = TRUE)

cleaned_comments$Content_cut <- sapply(cleaned_comments$cleaned_comments, function(text) {
  segment(text, jieba)
})

custom_stopwords <- readLines("merged_stopwords.txt", encoding = "UTF-8")

cleaned_comments$Content_cut <- lapply(cleaned_comments$Content_cut, function(word_list) {
  # 过滤掉停用词
  cleaned_words <- word_list[!word_list %in% custom_stopwords]
  return(cleaned_words)
})

cleaned_comments$Content_cut_str <- sapply(cleaned_comments$Content_cut, function(words) {
  paste(words, collapse = " ")
})

cleaned_comments <- cleaned_comments[nchar(Content_cut_str) > 0]

corpus <- Corpus(VectorSource(cleaned_comments$Content_cut_str))

dtm <- DocumentTermMatrix(corpus)

# # 查看DTM摘要
# inspect(dtm[1:20, 1:100])  # 查看前5条评论和前10个词的矩阵
# #运行结果发现矩阵非常稀疏：Sparsity 78%
# 
# lda_model <- LDA(dtm, k = 10, control = list(seed = 1234))

# =============================================================================================
# 不删除高频词，因为在LDA建模的时候，我们不知道删去的高频词是否和我们所需要的主题有关联（共现关系）

# # 过滤到top10高频词
# # 查看高频词
# # 获取词频
# term_freq <- slam::col_sums(dtm)  # 获取词汇的总频次
# term_freq_sorted <- sort(term_freq, decreasing = TRUE)  # 对词频进行降序排序
# head(term_freq_sorted, 10)  # 查看前10个高频词
# 
# top10words <- c(head(names(term_freq_sorted), 10), "莉莉丝","多琳","辛西娅","玛丽亚","女主","女仆","作者","呜呜", "啊啊啊", "哈哈哈")
# 
# # 过滤高频词
# 
# cleaned_comments$Content_cut <- lapply(cleaned_comments$Content_cut, function(detop_word) {
#   # 过滤掉停用词
#   cleaned_words <- detop_word[!detop_word %in% top10words]
#   return(cleaned_words)
# })
# 
# cleaned_comments$Content_cut_str <- sapply(cleaned_comments$Content_cut, function(words) {
#   paste(words, collapse = " ")
# })
# 
# cleaned_comments <- cleaned_comments[nchar(Content_cut_str) > 0]
# 
# corpus_new <- Corpus(VectorSource(cleaned_comments$Content_cut_str))
# 
# dtm_new <- DocumentTermMatrix(corpus_new)

# =============================================================================================

# 定义不同的 K 值范围
k_values <- seq(5, 30, by = 5)

# 存储困惑度和对数似然
perplexity_values <- c()
log_likelihood_values <- c()

# 循环建模，计算指标
for (k in k_values) {
  model <- LDA(dtm, k = k, control = list(seed = 1234))
  perplexity_values <- c(perplexity_values, perplexity(model))
  log_likelihood_values <- c(log_likelihood_values, logLik(model))
}

# 绘制困惑度图（越低越好）
plot(k_values, perplexity_values, type = "b", pch = 19, col = "dodgerblue",
     xlab = "Number of Topics (K)", ylab = "Perplexity", main = "Perplexity vs K")

# 绘制对数似然图（越高越好）
plot(k_values, log_likelihood_values, type = "b", pch = 19, col = "firebrick",
     xlab = "Number of Topics (K)", ylab = "Log-Likelihood", main = "Log-Likelihood vs K")

# =============================================================================================

# 重新用最优 k 值建模
best_k <- 25  # 根据你的评估结果填写
lda_best <- LDA(dtm, k = best_k, control = list(seed = 1234))

# 提取主题词
topics <- tidy(lda_best, matrix = "beta")

# 查看每个主题的高频词
top_terms <- topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "result", x = NULL, y = "Beta")

comment_topics <- tidy(lda_best, matrix = "gamma")

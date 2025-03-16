library(tm)
library(topicmodels)
library(tidytext)
library(dplyr)
library(tibble)
library(readr)
library(ggplot2)
library(data.table)

all_comments <- fread("all_comments.csv")

# 数据清洗

# 定义需要移除的关键词
keywords_to_remove <- c("营养液", "浇浇水", "捉个虫","更新", "催更", "元旦快乐",  "新年快乐")

# 清洗数据：删除包含关键词的行
cleaned_comments <- all_comments %>%
  filter(!is.na(Comment_ID)) %>%                # 删除 Comment_ID 为 NA 的行
  mutate(
    Likes = ifelse(is.na(Likes), 0, Likes),     # 将 Likes 列的 NA 替换为 0
    Content = trimws(Content)                  # 去除 Content 列中首尾多余空格
  ) |> 
  filter(!(grepl(paste(keywords_to_remove, collapse = "|"), Content, ignore.case = TRUE) &
             nchar(Content) < 150)) |> 
  filter(!grepl("实名认证", Content)) |> # 保留不包含“实名认证”的行
  distinct(Content, .keep_all = TRUE) %>%
  filter(nchar(Content) >= 2)

remove_chinese_punctuation <- function(text) {
  # 中文标点符号的正则表达式
  punctuation_pattern <- "[，。？！、；：“”‘’（）《》【】『』「」—…·~￥]"
  # 替换为 ""
  cleaned_text <- gsub(punctuation_pattern, "", text)
  return(cleaned_text)
}

# 应用到所有评论
cleaned_comments$Content <- sapply(cleaned_comments$Content, remove_chinese_punctuation)

comments <- cleaned_comments$Content

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

stopword_path <- "merged_stopwords.txt" 

cutter <- worker(stop_word = stopword_path)

segment_text <- function(text) {
  segments <- cutter[text]  # 分词
  return(segments)
}

cleaned_comments$segmented_content <- sapply(comments, function(x) {
  # 确保文本不为空
  if (nchar(x) == 0 || is.na(x)) {
    return("")  # 对空文本或 NA 返回空字符串
  }
  
  # 打印调试信息，查看每条评论的内容
  print(paste("Processing comment:", x))
  
  # 获取分词
  segments <- tryCatch({
    segment_text(x)  # 使用分词函数
  }, error = function(e) {
    # 捕获分词错误并返回空字符串
    message(paste("Error processing comment:", e$message))
    return("")
  })
  
  # 返回合并后的分词结果
  paste(segments, collapse = " ")
})

corpus <- Corpus(VectorSource(cleaned_comments$segmented_content))

# 文本清洗
# 自定义清洗停用词的函数
stopwords_cn <- readLines(stopword_path)

corpus_clean <- corpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  tm_map(removeWords, stopwords_cn)  # 使用自定义中文停用词

# 创建DTM矩阵
dtm <- DocumentTermMatrix(corpus_clean)
dtm <- removeSparseTerms(dtm, 0.95)  # 移除稀疏词

# 计算词频
term_freq <- colSums(as.matrix(dtm))

# 查看频率最高的前 10 个词（检查一下）
head(sort(term_freq, decreasing = TRUE), 10)

# 找出高频词的阈值（比如出现超过 5% 文档的词）
high_freq_threshold <- 0.05 * nrow(dtm)
high_freq_terms <- names(term_freq[term_freq > high_freq_threshold])

# 从 DTM 中移除高频词
dtm <- dtm[, !(colnames(dtm) %in% high_freq_terms)]

# 确认过滤后的维度
dim(dtm)

row_sums <- rowSums(as.matrix(dtm))

# 查看有多少行是空的
sum(row_sums == 0)

dtm_clean <- dtm[row_sums > 0, ]

k <- 10  # 设置主题数量
lda_model <- LDA(dtm_clean, k = k, control = list(seed = 1234))

# 查看主题建模结果
print(lda_model)


# 提取主题词
topics <- tidy(lda_model, matrix = "beta")

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
  labs(title = "主题建模结果", x = NULL, y = "Beta值")


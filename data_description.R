
# descriptive statistics
# library(readr)
library(data.table)
library(dplyr)
library(lubridate)
library(knitr)
library(kableExtra)
library(stringr)
library(ggplot2)
library(lubridate)
library(scales)

cleaned_comments <- fread(file = "data/cleaned_comments.csv")

china_provinces <- c("北京", "天津", "上海", "重庆", "河北", "山西", "辽宁", "吉林", "黑龙江", 
                     "江苏", "浙江", "安徽", "福建", "江西", "山东", "河南", "湖北", "湖南", 
                     "广东", "海南", "四川", "贵州", "云南", "陕西", "甘肃", "青海", "宁夏", 
                     "新疆", "西藏", "中国澳门", "中国香港", "中国台湾", "内蒙古", "广西")

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

colnames(cleaned_comments)

cleaned_comments$Post_Time <- as.Date(cleaned_comments$Post_Time)

# 找出“之后带 Location 的用户名和地址”
user_latest_location <- cleaned_comments %>%
  filter(!is.na(Location) & Post_Time >= as.Date("2022-08-01")) %>%
  group_by(Username) %>%
  summarise(Latest_Location = first(Location), .groups = "drop")

# 找出需要被填补的记录（Location 是 NA，时间早于 2022-08）
to_fill <- cleaned_comments %>%
  filter(is.na(Location) & Post_Time < as.Date("2022-08-01")) %>%
  semi_join(user_latest_location, by = "Username")  # 只保留那些后期有 IP 的用户

filled_comments <- cleaned_comments %>%
  left_join(user_latest_location, by = "Username") %>%
  mutate(Location = ifelse(is.na(Location) & Post_Time < as.Date("2022-08-01"),
                           Latest_Location, Location)) %>%
  select(-Latest_Location)

# 可视化柱状图（每月评论数量）

filled_comments <- filled_comments %>%
  mutate(
    Location_Category = case_when(
      is.na(Location) ~ "Unknown IP",
      Location %in% china_provinces ~ Location,
      TRUE ~ "Overseas"
    ),
    Likes = ifelse(is.na(Likes), 0, Likes),
    text = ifelse(is.na(text), "", text),
    Token_Count = str_count(text, "\\S+")
  )

# 添加 IP 状态分类列
filled_comments <- filled_comments %>%
  mutate(IP_Status = case_when(
    Location_Category == "Unknown IP" ~ "Unknown IP",
    Location_Category == "Overseas" ~ "Overseas",
    TRUE ~ "Chinese Provinces"
  )) %>%
  mutate(IP_Status = factor(IP_Status, levels = c("Chinese Provinces", "Overseas", "Unknown IP")))
# 统计每月评论数量
monthly_counts <- filled_comments %>%
  group_by(Post_Month, IP_Status) %>%
  summarise(Comments = n(), .groups = "drop")

# 转换时间格式为顺序型月份（如果是字符型）
monthly_counts$Post_Month <- as.factor(monthly_counts$Post_Month)
monthly_counts$Post_Month <- factor(monthly_counts$Post_Month, levels = sort(unique(monthly_counts$Post_Month)))

# 画图：柱状图（分组）
ggplot(monthly_counts, aes(x = Post_Month, y = Comments, fill = IP_Status)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Unknown IP" = "grey","Overseas" = "#1f77b4", "Chinese Provinces" = "#ff7f0e")) +
  labs(
    title = "Figure 1. Monthly Number of Comments by Locations",
    x = "Post Month",
    y = "Number of Comments",
    fill = "Locations"
  ) +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

filled_comments$doc_id <- as.character(filled_comments$doc_id)

write.table(filled_comments, "data/filled_comments.csv", row.names = FALSE, sep = "\t")

# 清洗 Location 和处理 Token 数
cleaned_comments_provinces <- filled_comments %>%
  filter(!is.na(Location),
         Location_Category != "Overseas")

location_counts <- cleaned_comments_provinces %>%
  count(Location, name = "Comment_Count") %>%
  filter(Comment_Count < 100)

cleaned_comments_provinces <- cleaned_comments_provinces %>%
  group_by(Location) %>%
  filter(n() >= 100) |> 
  ungroup()

# 主体表格
location_stats <- cleaned_comments_provinces %>%
  group_by(Location_Category) %>%
  summarise(
    Unique_Users = n_distinct(Username),
    Total_Comments = n(),
    Total_Tokens = sum(Token_Count),
    Avg_Tokens = mean(Token_Count, na.rm = TRUE),
    SD_Tokens = sd(Token_Count, na.rm = TRUE),
    Avg_Likes = mean(Likes, na.rm = TRUE),
    SD_Likes = sd(Likes, na.rm = TRUE),
    .groups = "drop"
  )

# # 增加 "All" 行
row_all <- cleaned_comments_provinces %>%
  summarise(
    Location_Category = "Total",
    Unique_Users = n_distinct(Username),
    Total_Comments = n(),
    Total_Tokens = sum(Token_Count),
    Avg_Tokens = mean(Token_Count, na.rm = TRUE),
    SD_Tokens = sd(Token_Count, na.rm = TRUE),
    Avg_Likes = mean(Likes, na.rm = TRUE),
    SD_Likes = sd(Likes, na.rm = TRUE)
  )

# # 增加 "All with IP" 行（排除没有IP）
# row_all_with_ip <- cleaned_comments_provinces %>%
#   filter(Location_Category != "Unknown IP") %>%
#   summarise(
#     Location_Category = "All with IP",
#     Unique_Users = n_distinct(Username),
#     Total_Comments = n(),
#     Total_Tokens = sum(Token_Count),
#     Avg_Tokens = mean(Token_Count, na.rm = TRUE),
#     SD_Tokens = sd(Token_Count, na.rm = TRUE),
#     Avg_Likes = mean(Likes, na.rm = TRUE)
#   )

# # 合并表格并排序（"All" 和 "All with IP" 后面接表格）
# location_stats$sort_order <- 3
# row_all$sort_order <- 1
# row_all_with_ip$sort_order <- 2
# 
# # 合并并按 sort_order 排序
# final_table <- bind_rows(row_all, row_all_with_ip, location_stats) %>%
#   arrange(sort_order, desc(Total_Comments)) %>%
#   select(-sort_order)  # 如果你不想展示这个排序列

final_table <- bind_rows(location_stats, row_all)

# Translate location labels to English
final_table$Location_Category <- ifelse(
  final_table$Location_Category %in% names(province_translation),
  province_translation[final_table$Location_Category],
  final_table$Location_Category
)

final_table <- final_table %>%
  mutate(sort_order = ifelse(Location_Category == "Total", Inf, -Unique_Users)) %>%
  arrange(sort_order) %>%
  select(-sort_order)

# 输出 kable 表格
descriptive <- kable(final_table,
        caption = "Descriptive Statistics of Comments by Provinces",
        col.names = c("Provinces", "Unique Users", "Total Comments", 
                      "Total Tokens", "Avg. Tokens", "SD Tokens",
                      "Avg. Likes", "SD Likes"),
        digits = 2, align = "c") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))



write.csv(final_table, file = "descriptive.csv", row.names = FALSE)

library(webshot)

# 先保存表格为 HTML 再转为 PNG
save_kable(
  kable(final_table,
        caption = "Descriptive Statistics of Comments by Provinces",
        col.names = c("Provinces", "Unique Users", "Total Comments", 
                      "Total Tokens", "Avg. Tokens", "SD Tokens",
                      "Avg. Likes"),
        digits = 2, align = "c") %>%
    kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed")),
  file = "Output_pic/table_provinces.png"
)

# ===================================================
# 处理同一个用户多个Location的问题
# 查看是否有同一 Username 对应多个 Location
username_location_morethan1 <- cleaned_comments_provinces %>%
  filter(!is.na(Username) & !is.na(Location)) %>%
  distinct(Username, Location) %>%
  count(Username) %>%
  filter(n > 1)

# 查看这些用户的具体 Location 信息
multiple_location_users <- cleaned_comments_provinces %>%
  filter(Username %in% username_location_morethan1$Username) %>%
  select(Username, Location, Content) %>%
  distinct() %>%
  arrange(Username)

# 计算用户的location数量（NA除外）
user_location_counts <- cleaned_comments_provinces %>%
  filter(!is.na(Username) & !is.na(Location)) %>%  # ✅ 只保留非 NA 的 Location
  group_by(Username, Location) %>%
  summarise(freq = n(), .groups = "drop")

# 找到每个用户中出现次数最多的地址（只保留唯一众数）
top_locations <- user_location_counts %>%
  group_by(Username) %>%
  filter(freq == max(freq)) %>%
  mutate(n_mode = n()) %>%
  filter(n_mode == 1) %>%  # ✅ 只保留唯一最多的情况
  select(Username, Location_MostFrequent = Location)

# 将用户的 Location 替换为众数（仅对拥有唯一众数的用户）
# （仅对 NA 以外 Location 多重用户）
cleaned_comments_provinces <- cleaned_comments_provinces %>%
  left_join(top_locations, by = "Username") %>%
  mutate(
    Location = ifelse(
      !is.na(Location) & !is.na(Location_MostFrequent),  # ✅ 原Location不为NA时才改
      Location_MostFrequent,
      Location
    )
  ) %>%
  select(-Location_MostFrequent)

# 未被更改者 = 仍然存在多个地址但没有唯一众数
unchanged_users <- setdiff(multiple_location_users$Username, top_locations$Username)

unchanged_user_locations <- cleaned_comments_provinces %>%
  filter(Username %in% unchanged_users) %>%
  filter(!is.na(Location)) %>%
  select(Username, Location, Content)

write.table(cleaned_comments_provinces, "data/cleaned_comments_provinces.csv", row.names = FALSE, sep = "\t")

# library(scales)
# 
# # 添加 IP 状态分类列
# cleaned_comments <- cleaned_comments %>%
#   mutate(IP_Status = case_when(
#     Location_Category == "Unknown IP" ~ "Unknown IP",
#     Location_Category == "Overseas" ~ "Overseas",
#     TRUE ~ "Chinese Provinces"
#   )) %>%
#   mutate(IP_Status = factor(IP_Status, levels = c("Chinese Provinces", "Overseas", "Unknown IP")))
# # 统计每月评论数量
# monthly_counts <- cleaned_comments %>%
#   group_by(Post_Month, IP_Status) %>%
#   summarise(Comments = n(), .groups = "drop")
# 
# # 转换时间格式为顺序型月份（如果是字符型）
# monthly_counts$Post_Month <- as.factor(monthly_counts$Post_Month)
# monthly_counts$Post_Month <- factor(monthly_counts$Post_Month, levels = sort(unique(monthly_counts$Post_Month)))
# 
# # 画图：柱状图（分组）
# ggplot(monthly_counts, aes(x = Post_Month, y = Comments, fill = IP_Status)) +
#   geom_bar(stat = "identity", position = "stack") +
#   scale_fill_manual(values = c("Unknown IP" = "grey","Overseas" = "#1f77b4", "Chinese Provinces" = "#ff7f0e")) +
#   labs(
#     title = "Monthly Number of Comments by Locations",
#     x = "Post Month",
#     y = "Number of Comments",
#     fill = "Locations"
#   ) +
#   theme_minimal(base_size = 8) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))




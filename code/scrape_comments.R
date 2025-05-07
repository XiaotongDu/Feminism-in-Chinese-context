# 加载必要的库
# load libraries
library(httr)
library(XML)
library(rvest)
library(dplyr)
library(stringr)
library(purrr)

# 基础 URL
base_url <- "https://www.jjwxc.net/comment.php?novelid=6272118&chapterid="

# 爬取章节的页数 the pages I need to scrape
chapter_pages <- map_df(1:155, ~{
  # 构建章节的 URL
  url <- paste0(base_url, .x, "&page=1")
  
  # 发起请求并解析 HTML 
  page <- tryCatch({
    read_html(GET(url))
  }, error = function(e) {
    message(paste("Error fetching chapter:", .x))
    return(NULL)
  })
  
  if (!is.null(page)) {
    # 提取总页数的文本
    total_pages <- page %>%
      html_node("div.pagebar span.num") %>%  # 定位总计页数的 <span> check the page number
      
      ### 这里有一个问题，如果总页数小于等于5页，就不会有总计页数，就会抓到NA
      ###### 暂时先这样，之后有时间再来解决
      
      html_text(trim = TRUE) %>%            # 提取文本并去除空格 extract text and delete empty spaces
      as.numeric()                         # 转换为数值
  } else {
    total_pages <- NA
  }
  
  # 返回数据框
  data.frame(
    Chapter_ID = .x,
    Total_Pages = ifelse(is.na(total_pages), 0, total_pages) # 缺失设为 0 set missing value as 0
  )
})

chapter_pages[chapter_pages$Total_Pages == 0, ]

# 手动修改评论页少于等于5页的章节，其中两章被屏蔽，可删除，暂时先这样
# there's some chapters being blocked and some chapters have comment pages less than 6(return NA from above)
# so I adjust them by hand

chapter_pages$Total_Pages[chapter_pages$Chapter_ID == 51] <- 5
chapter_pages$Total_Pages[chapter_pages$Chapter_ID == 87] <- 4
cleaned_chapter_pages <- chapter_pages %>% filter(Total_Pages != 0)

# 开始爬数据

all_comments <- data.frame()

# 遍历 cleaned_chapter_pages 中的每个章节
for (chapter_id in cleaned_chapter_pages$Chapter_ID[cleaned_chapter_pages$Chapter_ID >= 0]) {
  
  # 获取当前章节的总页数
  total_pages <- cleaned_chapter_pages$Total_Pages[cleaned_chapter_pages$Chapter_ID == chapter_id]
  message(paste("Fetching chapter", chapter_id, "with", total_pages, "pages"))
  
  # 设定最大重试次数
  max_retries <- 3
  
  for (page_num in 1:total_pages) {
    
    url <- paste0(base_url, chapter_id, "&page=", page_num)
    retry_count <- 0
    page <- NULL  # 初始化page变量
    
    # 每次请求之前生成一个 0 到 2 秒之间的随机秒数
    random_sleep <- runif(1, min = 0, max = 2)
    Sys.sleep(random_sleep)  # 暂停随机秒数
    
    # 使用tryCatch来处理读取页面的错误
    while (is.null(page) && retry_count < max_retries) {
      page <- tryCatch({
        message(paste("Fetching URL:", url))  # 打印URL，确认正在请求正确的页面
        read_html(url)
      }, error = function(e) {
        message(paste("Error fetching chapter", chapter_id, "page", page_num, "- Retry", retry_count + 1))
        return(NULL)
      })
      
      if (is.null(page)) {
        retry_count <- retry_count + 1
        message(paste("Retrying page", page_num, "attempt", retry_count))
        Sys.sleep(5)  # 暂停5秒再尝试
      } else {
        message(paste("Successfully fetched chapter", chapter_id, "page", page_num))
      }
    }
    
    if (!is.null(page)) {
      # 处理页面数据
      # --- 提取主楼评论数据 ---
      main_comments <- page %>%
        html_nodes(".readtd") %>%
        map_df(~{
          comment_floor <- html_text(.x %>% html_node(".tdtitle span"), trim = TRUE) %>%
            str_extract("№\\d+") %>%
            str_replace("№", "") %>%
            as.numeric()
          username <- html_text(.x %>% html_node(".redcommentreaderid"))
          post_time <- html_text(.x %>% html_node(".tdtitle"), trim = TRUE) %>%
            str_extract("\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}")
          chapter <- html_text(.x %>% html_node(".redcommentchapter"))
          content <- html_text(.x %>% html_node(".readbody span[id^='mormalcomment']"))
          likes <- html_text(.x %>% html_node(".agree_block span"), trim = TRUE) %>%
            as.numeric()
          location <- html_text(.x %>% html_node(".agree_block ~ span"), trim = TRUE) %>%
            str_extract("来自\\S+")
          
          data.frame(
            Comment_ID = comment_floor,
            Floor = 0,  # 主楼的 Floor 为 0
            Username = username,
            Post_Time = post_time,
            Chapter = chapter,
            Content = content,
            Likes = likes,
            Location = location
          )
        })
      
      # --- 提取回复楼数据 ---
      replies <- page %>%
        html_nodes(".readtd") %>%
        map_df(~{
          comment_floor <- html_text(.x %>% html_node(".tdtitle span"), trim = TRUE) %>%
            str_extract("№\\d+") %>%
            str_replace("№", "") %>%
            as.numeric()
          chapter <- html_text(.x %>% html_node(".redcommentchapter"))
          
          reply_bodies <- .x %>%
            html_nodes(".replybody") %>%
            map_df(~{
              reply_floor <- html_text(.x, trim = TRUE) %>%
                str_extract("\\[(\\d+)楼\\]") %>%
                gsub("\\[|楼\\]", "", .) %>%
                as.numeric()
              username <- html_text(.x %>% html_node("span[data-foldlingreplyauthor]"))
              post_time <- html_text(.x, trim = TRUE) %>%
                str_extract("\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}")
              content <- html_text(.x %>% html_node("span[id^='mormalreply']"))
              likes <- html_text(.x %>% html_node(".agree_block span"), trim = TRUE) %>%
                as.numeric()
              location <- html_text(.x %>% html_node(".agree_block ~ span"), trim = TRUE) %>%
                str_extract("来自\\S+")
              
              data.frame(
                Comment_ID = comment_floor,  # 回复楼对应主楼的 Comment_ID
                Floor = reply_floor,
                Username = username,
                Post_Time = post_time,
                Chapter = chapter,
                Content = content,
                Likes = likes,
                Location = location
              )
            })
          
          reply_bodies
        })
      
      # 将当前页面的评论数据合并到总数据框
      all_comments <- bind_rows(all_comments, main_comments, replies)
      gc()  # 清理内存
    } else {
      message(paste("Failed to fetch chapter", chapter_id, "page", page_num, "after", max_retries, "retries"))
    }
  }
}

all_comments$Chapter <- as.integer(all_comments$Chapter)

# save comments data
write.table(all_comments, "data/comments_update.csv", row.names = FALSE, sep = "\t")




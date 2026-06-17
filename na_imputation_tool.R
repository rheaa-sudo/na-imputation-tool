library(shiny)
library(dplyr)
library(DT)
library(VIM)
library(mice)

# ----------------------------------------------------
# Helper Functions
# ----------------------------------------------------
analyze_outliers <- function(df) {
  num_cols <- sapply(df, is.numeric)
  if (sum(num_cols) == 0) return(list(has_outliers = FALSE, pct = 0))
  total_outliers <- 0
  total_numeric_points <- 0
  for (col in names(df)[num_cols]) {
    vec <- df[[col]]
    clean_vec <- vec[!is.na(vec)]
    if (length(clean_vec) < 5) next
    q <- quantile(clean_vec, probs = c(0.25, 0.75))
    iqr <- q[2] - q[1]
    if (iqr == 0) next
    lower_bound <- q[1] - 1.5 * iqr
    upper_bound <- q[2] + 1.5 * iqr
    total_outliers <- total_outliers + sum(clean_vec < lower_bound | clean_vec > upper_bound)
    total_numeric_points <- total_numeric_points + length(clean_vec)
  }
  if (total_numeric_points == 0) return(list(has_outliers = FALSE, pct = 0))
  pct <- (total_outliers / total_numeric_points) * 100
  return(list(has_outliers = pct > 5, pct = round(pct, 1)))
}

get_rule_recommendations <- function(df, var_desc) {
  recs <- list()
  desc_lower <- tolower(var_desc)

  for (col in names(df)) {
    na_count <- sum(is.na(df[[col]]))
    if (na_count == 0) next

    col_type <- class(df[[col]])
    na_pct   <- (na_count / nrow(df)) * 100

    user_hint <- ""
    if (nchar(var_desc) > 0 && grepl(tolower(col), desc_lower)) {
      lines <- strsplit(desc_lower, "\n")[[1]]
      matching_line <- lines[grep(tolower(col), lines)]
      if (length(matching_line) > 0) user_hint <- matching_line[1]
    }

    if (col_type %in% c("numeric", "integer")) {
      if (grepl("mean|평균", user_hint)) {
        rec    <- "Mean (평균값 대치)"
        reason <- "변수 설명에서 평균값 대치 힌트가 감지되었습니다."
      } else if (grepl("median|중앙|중위", user_hint)) {
        rec    <- "Median (중앙값 대치)"
        reason <- "변수 설명에서 중앙값 대치 힌트가 감지되었습니다."
      } else if (grepl("zero|0|상수", user_hint)) {
        rec    <- "Constant 0 (0으로 채우기)"
        reason <- "변수 설명에서 0 대치 힌트가 감지되었습니다."
      } else if (na_pct > 30) {
        rec    <- "Drop Column (변수 삭제)"
        reason <- paste0("결측치 비율이 ", round(na_pct, 1), "%로 매우 높아 컬럼 삭제를 권장합니다.")
      } else {
        clean_vec <- df[[col]][!is.na(df[[col]])]
        v_mean    <- mean(clean_vec)
        v_median  <- median(clean_vec)
        v_sd      <- sd(clean_vec)
        is_skewed <- if (v_sd > 0) (abs(v_mean - v_median) / v_sd) > 0.15 else FALSE
        if (is_skewed) {
          rec    <- "Median (중앙값 대치)"
          reason <- "분포 왜도가 감지되어 이상치 영향이 적은 중앙값을 권장합니다."
        } else {
          rec    <- "Mean (평균값 대치)"
          reason <- "정규 분포에 가까워 평균값 대치를 권장합니다."
        }
      }
    } else {
      if (grepl("mode|최빈", user_hint)) {
        rec    <- "Mode (최빈값 대치)"
        reason <- "변수 설명에서 최빈값 대치 힌트가 감지되었습니다."
      } else if (grepl("missing|미상|결측|none|없음", user_hint)) {
        rec    <- "Constant 'Missing' ('Missing'으로 채우기)"
        reason <- "변수 설명에서 결측을 별도 범주로 처리하라는 힌트가 감지되었습니다."
      } else if (na_pct > 30) {
        rec    <- "Drop Column (변수 삭제)"
        reason <- paste0("결측치 비율이 ", round(na_pct, 1), "%로 매우 높아 컬럼 삭제를 권장합니다.")
      } else {
        rec    <- "Mode (최빈값 대치)"
        reason <- "범주형 변수의 기본 추천: 최빈값 대치."
      }
    }
    recs[[col]] <- list(rec = rec, reason = reason, status = "Pending")
  }
  return(recs)
}

# ----------------------------------------------------
# CSS
# ----------------------------------------------------
app_css <- "
  @import url('https://fonts.googleapis.com/css2?family=Gowun+Dodum&display=swap');

  body {
    background-color: #e8f0ef;
    color: #1e3a3a;
    font-family: 'Gowun Dodum', sans-serif;
    font-size: 14.5px;
    font-weight: 700;
  }
  h1, h2, h3, h4, h5 {
    color: #3a5f7a;
    letter-spacing: 0.04em;
    font-family: 'Gowun Dodum', sans-serif;
    font-weight: 700;
  }
  p, label, .control-label, .shiny-input-container, small {
    color: #3a5a5a !important;
    font-family: 'Gowun Dodum', sans-serif;
    font-weight: 700;
  }
  .top-panel {
    background: linear-gradient(135deg, #dde8e8 0%, #e4ecf0 100%);
    border: 1px solid #a8c4cc55;
    border-radius: 14px;
    padding: 20px 24px;
    margin-bottom: 24px;
    box-shadow: 0 2px 16px #a8c4cc33;
  }
  .bottom-panel {
    background: linear-gradient(135deg, #dde8e8, #e4ecf0);
    border: 1px solid #c4b8e033;
    border-radius: 14px;
    padding: 20px 24px;
    margin-top: 24px;
    box-shadow: 0 2px 16px #a8c4cc33;
  }
  .rec-card {
    background: #ffffff;
    border-radius: 10px;
    padding: 14px;
    margin-bottom: 10px;
    box-shadow: 0 1px 8px #a8c4cc28;
    border: 1px solid #d4e4e8;
  }
  .rec-card:hover { box-shadow: 0 3px 16px #8ab0cc33; }
  .manual-panel {
    background: #ffffff;
    border-radius: 10px;
    padding: 18px;
    margin-top: 16px;
    border-left: 4px solid #8ab0cc;
    box-shadow: 0 1px 8px #a8c4cc28;
  }
  .btn-warning {
    background: linear-gradient(135deg, #9b8fc4, #b8aee0) !important;
    border-color: #b8aee0 !important;
    color: #fff !important;
    font-weight: 700;
    font-family: 'Gowun Dodum', sans-serif;
    border-radius: 8px !important;
    box-shadow: 0 2px 8px #b8aee044;
  }
  .btn-success {
    background: linear-gradient(135deg, #4a9aaa, #6abccc) !important;
    border-color: #4a9aaa !important;
    color: #fff !important;
    font-weight: 700;
    font-family: 'Gowun Dodum', sans-serif;
    border-radius: 8px !important;
    box-shadow: 0 2px 8px #4a9aaa44;
  }
  .btn-primary {
    background: linear-gradient(135deg, #d4897a, #f2bfb0) !important;
    border-color: #d4897a !important;
    color: #fff !important;
    font-weight: 700;
    font-family: 'Gowun Dodum', sans-serif;
    border-radius: 8px !important;
    box-shadow: 0 2px 8px #d4897a44;
  }
  .btn-xs.btn-success {
    background: #4a9aaa !important;
    border-color: #4a9aaa !important;
    color: #fff !important;
    border-radius: 6px !important;
    font-weight: 700;
  }
  .btn-xs.btn-danger {
    background: #d4897a !important;
    border-color: #d4897a !important;
    color: #fff !important;
    border-radius: 6px !important;
    font-weight: 700;
  }
  table.dataTable {
    background-color: #ffffff !important;
    color: #1e3a3a !important;
    font-family: 'Gowun Dodum', sans-serif !important;
    font-weight: 700 !important;
    border-radius: 8px;
    box-shadow: 0 1px 8px #a8c4cc22;
  }
  table.dataTable thead th {
    background-color: #dde8e8 !important;
    color: #3a5f7a !important;
    border-bottom: 1px solid #a8c4cc44 !important;
    font-weight: 700 !important;
    letter-spacing: 0.03em;
  }
  table.dataTable tbody tr { background-color: #ffffff !important; color: #1e3a3a !important; }
  table.dataTable tbody tr:hover { background-color: #edf4f6 !important; }
  table.dataTable tbody tr.selected { background-color: #d4eaf0 !important; color: #3a5f7a !important; }
  .dataTables_wrapper .dataTables_paginate .paginate_button {
    color: #3a5f7a !important;
    font-family: 'Gowun Dodum', sans-serif;
    font-weight: 700;
  }
  .dataTables_wrapper .dataTables_paginate .paginate_button.current {
    background: #dde8e8 !important;
    border-color: #8ab0cc55 !important;
    color: #3a5f7a !important;
    border-radius: 4px !important;
  }
  .dataTables_wrapper .dataTables_info {
    color: #5a8a8a !important;
    font-family: 'Gowun Dodum', sans-serif;
    font-weight: 700;
  }
  select, input[type='text'], textarea {
    background-color: #ffffff !important;
    color: #1e3a3a !important;
    border: 1px solid #a8c4cc88 !important;
    border-radius: 6px !important;
    font-family: 'Gowun Dodum', sans-serif !important;
    font-weight: 700 !important;
  }
  .label-warning { background-color: #9b8fc4; color: #fff; font-weight: 700; }
  .label-success { background-color: #4a9aaa; color: #fff; font-weight: 700; }
  .label-danger  { background-color: #d4897a; color: #fff; font-weight: 700; }
  hr { border-color: #a8c4cc44; }
  .shiny-notification {
    background-color: #ffffff;
    color: #1e3a3a;
    border-left: 4px solid #8ab0cc;
    font-family: 'Gowun Dodum', sans-serif;
    font-weight: 700;
    border-radius: 8px;
    box-shadow: 0 2px 12px #a8c4cc44;
  }
  .well { background-color: #edf4f6 !important; border-color: #a8c4cc44 !important; }
  pre, code {
    background-color: #edf4f6 !important;
    color: #3a5a5a !important;
    border: none !important;
    font-size: 12px;
    font-weight: 700;
  }
  .btn-default {
    background-color: #ffffff !important;
    border-color: #a8c4cc88 !important;
    color: #3a5f7a !important;
    font-family: 'Gowun Dodum', sans-serif !important;
    font-weight: 700 !important;
    border-radius: 6px !important;
  }
  .radio label { color: #3a5a5a !important; font-weight: 700 !important; }
  .shiny-input-container .form-control {
    background-color: #ffffff !important;
    color: #1e3a3a !important;
    border-color: #a8c4cc88 !important;
    font-weight: 700 !important;
  }
"

# ----------------------------------------------------
# UI
# ----------------------------------------------------
ui <- fluidPage(
  tags$head(tags$style(HTML(app_css))),

  div(style = "padding: 24px 0 8px 0;",
    h1("✦ NA Imputation Tool",
       style = "margin:0; font-size:2rem; letter-spacing:0.08em;"),
    p("Data Quality & Smart Imputation",
      style = "color:#8ab0cc; margin:4px 0 0 2px; font-size:0.95rem;")
  ),

  # 상단 패널
  div(class = "top-panel",
    fluidRow(
      column(3, fileInput("file", "① Upload CSV File", accept = ".csv", width = "100%")),
      column(6,
        textAreaInput("var_desc", "② Variable Description (Optional)",
          placeholder = "예: LotFrontage: 도로 연결 선형 피트. NA는 도로 미접속 가능성.\nAlley: 골목 접근 유형. NA는 골목 없음을 의미할 수 있음.",
          rows = 2, width = "100%")
      ),
      column(3, style = "margin-top:25px;",
        actionButton("ai_analyze_btn", "⚡ Run Analysis",
          class = "btn-warning btn-block", style = "height:40px; font-size:1rem;")
      )
    )
  ),

  # 메인 영역
  fluidRow(
    # 왼쪽: 테이블 + 수동 설정 패널
    column(7,
      h3("③ Column Summary"),
      p("행을 클릭하면 아래에 수동 설정 및 분포 시각화가 나타납니다.",
        style = "font-size:0.88rem;"),
      DTOutput("na_summary"),
      uiOutput("individual_panel_ui")
    ),
    # 오른쪽: 추천 패널
    column(5,
      h3("④ Recommendations"),
      p("분석 후 제안을 검토하고 수락/거절하세요.", style = "font-size:0.88rem;"),
      uiOutput("ai_recommendations_ui")
    )
  ),

  # 하단 패널
  div(class = "bottom-panel", style = "margin-top:30px;",
    h4("⑤ Execute & Download"),
    fluidRow(
      column(5,
        radioButtons("global_mode", "Remaining NAs — Global Mode:",
          choices = c(
            "Individual Settings Only (개별 수동 처리만 실행)",
            "KNN Imputation for remaining NAs (남은 결측치 KNN 대치)",
            "MICE Imputation for remaining NAs (남은 결측치 MICE 대치)"
          ))
      ),
      column(3, style = "margin-top:25px;",
        actionButton("apply_btn", "Apply Treatments",
          class = "btn-success btn-lg btn-block")
      ),
      column(4, style = "margin-top:25px;",
        downloadButton("downloadData", "Download Cleaned Data",
          class = "btn-primary btn-lg btn-block")
      )
    ),
    p("⚠️ 다운로드가 막히는 경우 RStudio 상단의 'Open in Browser'를 이용해 주세요.",
      style = "color:#a07060; font-size:11px; margin-top:8px;")
  )
)

# ----------------------------------------------------
# Server
# ----------------------------------------------------
server <- function(input, output, session) {

  user_settings <- reactiveValues(treatments = list())
  ai_recs       <- reactiveValues(list = list())

  raw_data <- reactive({
    req(input$file)
    read.csv(input$file$datapath, stringsAsFactors = TRUE)
  })

  observe({
    req(raw_data())
    df <- raw_data()
    tr <- list()
    for (col in names(df)) tr[[col]] <- "Do Nothing"
    user_settings$treatments <- tr
    ai_recs$list <- list()
  })

  # 분석 버튼
  observeEvent(input$ai_analyze_btn, {
    req(raw_data())
    withProgress(message = 'Running Analysis...', value = 0.5, {
      recs <- get_rule_recommendations(raw_data(), input$var_desc)
      ai_recs$list <- recs
    })
    for (col in names(ai_recs$list)) {
      local({
        local_col <- col
        observeEvent(input[[paste0("accept_", local_col)]], {
          req(ai_recs$list[[local_col]])
          user_settings$treatments[[local_col]] <- ai_recs$list[[local_col]]$rec
          ai_recs$list[[local_col]]$status <- "Accepted"
          showNotification(paste0(local_col, ": 수락했습니다."), type = "message")
        }, ignoreInit = TRUE)
        observeEvent(input[[paste0("decline_", local_col)]], {
          req(ai_recs$list[[local_col]])
          ai_recs$list[[local_col]]$status <- "Declined"
          showNotification(paste0(local_col, ": 거절했습니다."), type = "warning")
        }, ignoreInit = TRUE)
      })
    }
  })

  observeEvent(input$impute_method, {
    req(selected_col())
    user_settings$treatments[[selected_col()]] <- input$impute_method
  })

  # 컬럼 요약 테이블
  output$na_summary <- renderDT({
    req(raw_data())
    df         <- raw_data()
    total_rows <- nrow(df)

    summary_df <- data.frame(
      Column = names(df),
      Type   = sapply(df, class),
      NAs    = sapply(df, function(x) sum(is.na(x)))
    )
    summary_df$NA_Pct <- paste0(round((summary_df$NAs / total_rows) * 100, 1), "%")

    summary_df$Recommendation <- sapply(names(df), function(col_name) {
      p        <- (sum(is.na(df[[col_name]])) / total_rows) * 100
      vec      <- df[[col_name]]
      col_type <- class(vec)
      if (p == 0)  return("처리 불필요")
      if (p >= 30) return("1순위: 변수삭제 / 2순위: MICE")
      if (col_type %in% c("numeric", "integer")) {
        clean_vec <- vec[!is.na(vec)]
        v_mean    <- mean(clean_vec); v_median <- median(clean_vec); v_sd <- sd(clean_vec)
        is_skewed <- if (v_sd > 0) (abs(v_mean - v_median) / v_sd) > 0.15 else FALSE
        if (p >= 10)   return("1순위: MICE / 2순위: 중앙값")
        if (is_skewed) return("1순위: 중앙값 / 2순위: 0 대치")
        return("1순위: 평균값 / 2순위: 중앙값")
      } else {
        if (p >= 10) return("1순위: MICE / 2순위: 'Missing'")
        return("1순위: 최빈값 / 2순위: 'Missing'")
      }
    })

    summary_df$Selected <- sapply(names(df), function(col) {
      val <- user_settings$treatments[[col]]
      if (is.null(val)) "Do Nothing" else val
    })

    display_df <- summary_df[, c("Column", "Type", "NAs", "NA_Pct", "Recommendation", "Selected")]
    datatable(display_df, selection = 'single',
              options = list(pageLength = 50, dom = 'tip'),
              rownames = FALSE)
  })

  selected_col <- reactive({
    req(input$na_summary_rows_selected)
    names(raw_data())[input$na_summary_rows_selected]
  })

  # 수동 설정 + 분포 시각화 패널
  output$individual_panel_ui <- renderUI({
    req(selected_col())
    col      <- selected_col()
    df       <- raw_data()
    col_type <- class(df[[col]])

    div(class = "manual-panel",
      h4(paste("⚙️", col)),
      fluidRow(
        column(6, uiOutput("treatment_selector")),
        column(6,
          tags$h5("🔍 NA Row Indices", style = "color:#3a5f7a;"),
          verbatimTextOutput("na_rows")
        )
      ),
      hr(),
      # 분포 시각화
      if (col_type %in% c("numeric", "integer")) {
        tagList(
          tags$h5("📊 Distribution & Outliers", style = "color:#3a5f7a;"),
          fluidRow(
            column(8, plotOutput("dist_plot", height = "200px")),
            column(4, tableOutput("numeric_summary"))
          )
        )
      } else {
        tagList(
          tags$h5("📊 Category Distribution", style = "color:#3a5f7a;"),
          fluidRow(
            column(8, plotOutput("dist_plot", height = "200px")),
            column(4, tableOutput("factor_summary"))
          )
        )
      }
    )
  })

  # 분포 시각화 플롯
  output$dist_plot <- renderPlot({
    req(selected_col())
    col      <- selected_col()
    df       <- raw_data()
    vec      <- df[[col]]
    col_type <- class(vec)

    par(bg = "#ffffff", col.axis = "#3a5a5a", col.lab = "#3a5f7a",
        col.main = "#3a5f7a", family = "sans", mar = c(3, 3, 2, 1))

    if (col_type %in% c("numeric", "integer")) {
      clean_vec <- vec[!is.na(vec)]
      q         <- quantile(clean_vec, probs = c(0.25, 0.75))
      iqr       <- q[2] - q[1]
      lower_b   <- q[1] - 1.5 * iqr
      upper_b   <- q[2] + 1.5 * iqr
      is_outlier <- clean_vec < lower_b | clean_vec > upper_b

      hist(clean_vec,
           col     = "#c8dfe8",
           border  = "#8ab0cc",
           main    = paste0("Histogram: ", col),
           xlab    = "",
           ylab    = "Count",
           breaks  = 30,
           font.main = 1)

      # 이상치 경계선 표시
      abline(v = lower_b, col = "#d4897a", lty = 2, lwd = 2)
      abline(v = upper_b, col = "#d4897a", lty = 2, lwd = 2)
      abline(v = mean(clean_vec),   col = "#4a9aaa", lty = 1, lwd = 2)
      abline(v = median(clean_vec), col = "#9b8fc4", lty = 1, lwd = 2)

      legend("topright",
             legend = c("Mean", "Median", "IQR Bound (outlier)"),
             col    = c("#4a9aaa", "#9b8fc4", "#d4897a"),
             lty    = c(1, 1, 2),
             lwd    = 2,
             bty    = "n",
             cex    = 0.75)

      # 이상치 비율 텍스트
      outlier_pct <- round(sum(is_outlier) / length(clean_vec) * 100, 1)
      mtext(paste0("이상치: ", sum(is_outlier), "개 (", outlier_pct, "%)"),
            side = 3, line = 0.2, cex = 0.75, col = "#d4897a")

    } else {
      # 범주형: 막대그래프
      counts    <- sort(table(vec[!is.na(vec)]), decreasing = TRUE)
      top_counts <- head(counts, 10)
      barplot(top_counts,
              col    = "#c8dfe8",
              border = "#8ab0cc",
              main   = paste0("Top Categories: ", col),
              xlab   = "",
              ylab   = "Count",
              las    = 2,
              cex.names = 0.7,
              font.main = 1)
    }
  }, bg = "#ffffff")

  output$treatment_selector <- renderUI({
    req(selected_col())
    col      <- selected_col()
    df       <- raw_data()
    col_type <- class(df[[col]])
    na_count <- sum(is.na(df[[col]]))
    choices  <- "Do Nothing"
    if (na_count > 0) {
      if (col_type %in% c("numeric", "integer")) {
        choices <- c(choices, "Mean (평균값 대치)", "Median (중앙값 대치)",
                     "Constant 0 (0으로 채우기)",
                     "Drop Rows with NAs (결측치 행 삭제)", "Drop Column (변수 삭제)")
      } else {
        choices <- c(choices, "Mode (최빈값 대치)",
                     "Constant 'Missing' ('Missing'으로 채우기)",
                     "Drop Rows with NAs (결측치 행 삭제)", "Drop Column (변수 삭제)")
      }
    }
    current_val <- user_settings$treatments[[col]]
    if (is.null(current_val)) current_val <- "Do Nothing"
    selectInput("impute_method", "Choose Action:", choices = choices, selected = current_val)
  })

  output$numeric_summary <- renderTable({
    req(selected_col())
    vec    <- raw_data()[[selected_col()]]
    clean  <- vec[!is.na(vec)]
    q      <- quantile(clean, probs = c(0.25, 0.75))
    iqr    <- q[2] - q[1]
    out_n  <- sum(clean < (q[1] - 1.5*iqr) | clean > (q[2] + 1.5*iqr))
    data.frame(
      Metric = c("Mean", "Median", "SD", "Min", "Max", "25%", "75%", "Outliers"),
      Value  = c(round(mean(clean), 2), round(median(clean), 2),
                 round(sd(clean), 2),   round(min(clean), 2),
                 round(max(clean), 2),  round(q[1], 2),
                 round(q[2], 2),        out_n)
    )
  }, striped = FALSE, bordered = FALSE, hover = TRUE)

  output$factor_summary <- renderTable({
    req(selected_col())
    vec       <- raw_data()[[selected_col()]]
    counts    <- table(vec, useNA = "no")
    df_counts <- as.data.frame(counts)
    names(df_counts) <- c("Category", "Count")
    df_counts %>% arrange(desc(Count)) %>% head(8)
  }, striped = FALSE, bordered = FALSE, hover = TRUE)

  output$na_rows <- renderPrint({
    req(selected_col())
    vec        <- raw_data()[[selected_col()]]
    na_indices <- which(is.na(vec))
    if (length(na_indices) == 0) cat("No NA values found.")
    else cat(paste(na_indices, collapse = ", "))
  })

  # 추천 패널 UI
  output$ai_recommendations_ui <- renderUI({
    req(raw_data())
    recs <- ai_recs$list
    if (length(recs) == 0) {
      return(div(class = "rec-card",
        p("변수 설명을 입력(선택사항)하고\n'Run Analysis' 버튼을 눌러보세요.\n각 컬럼에 맞는 처리 방법을 제안합니다.",
          style = "color:#5a8a8a; white-space:pre-line; margin:0;")
      ))
    }
    card_uis <- lapply(names(recs), function(col) {
      rec_info   <- recs[[col]]
      border_col <- switch(rec_info$status,
        "Pending"  = "#9b8fc4",
        "Accepted" = "#4a9aaa",
        "Declined" = "#d4897a"
      )
      status_badge <- switch(rec_info$status,
        "Pending"  = span("검토 중",  class = "label label-warning"),
        "Accepted" = span("수락",     class = "label label-success"),
        "Declined" = span("거절",     class = "label label-danger")
      )
      div(class = "rec-card",
          style = paste0("border-left: 4px solid ", border_col, ";"),
        fluidRow(
          column(8,
            tags$h5(tags$strong(col), " ", status_badge,
                    style = "margin-top:0; color:#3a5f7a;"),
            p(tags$strong("추천: "), rec_info$rec, style = "margin-bottom:4px;"),
            p(tags$small(rec_info$reason), style = "color:#5a8a8a; margin:0; font-size:0.82rem;")
          ),
          column(4, style = "text-align:right; margin-top:8px;",
            if (rec_info$status == "Pending") {
              tagList(
                actionButton(paste0("accept_", col), "수락",
                  class = "btn-success btn-xs", style = "width:52px; margin-bottom:4px;"),
                br(),
                actionButton(paste0("decline_", col), "거절",
                  class = "btn-danger btn-xs", style = "width:52px;")
              )
            }
          )
        )
      )
    })
    do.call(tagList, card_uis)
  })

  # ----------------------------------------------------
  # Apply & Imputation Pipeline
  # ----------------------------------------------------
  processed_data <- eventReactive(input$apply_btn, {
    df   <- raw_data()
    mode <- input$global_mode
    withProgress(message = 'Processing...', value = 0, {
      incProgress(0.3, detail = "Applying manual settings...")
      tryCatch({
        for (col in names(df)) {
          method <- user_settings$treatments[[col]]
          if (is.null(method) || method == "Do Nothing") next
          vec        <- df[[col]]
          na_indices <- is.na(vec)
          if (!any(na_indices) && method != "Drop Column (변수 삭제)") next
          if (method == "Mean (평균값 대치)") {
            df[[col]][na_indices] <- mean(vec, na.rm = TRUE)
          } else if (method == "Median (중앙값 대치)") {
            df[[col]][na_indices] <- median(vec, na.rm = TRUE)
          } else if (method == "Mode (최빈값 대치)") {
            ux <- unique(vec[!na_indices])
            df[[col]][na_indices] <- ux[which.max(tabulate(match(vec[!na_indices], ux)))]
          } else if (method == "Constant 0 (0으로 채우기)") {
            df[[col]][na_indices] <- 0
          } else if (method == "Constant 'Missing' ('Missing'으로 채우기)") {
            if (is.factor(df[[col]])) levels(df[[col]]) <- c(levels(df[[col]]), "Missing")
            df[[col]][na_indices] <- "Missing"
          } else if (method == "Drop Rows with NAs (결측치 행 삭제)") {
            df <- df[!is.na(df[[col]]), ]
          } else if (method == "Drop Column (변수 삭제)") {
            df[[col]] <- NULL
          }
        }
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })

      remaining_nas <- sum(is.na(df))

      if (remaining_nas > 0) {
        if (mode == "KNN Imputation for remaining NAs (남은 결측치 KNN 대치)") {
          incProgress(0.5, detail = "Running KNN...")
          df <- tryCatch({
            VIM::kNN(df, imp_var = FALSE)
          }, error = function(e) {
            showNotification("KNN failed. Falling back to Median.", type = "warning")
            for (col in names(df)) {
              if (!any(is.na(df[[col]]))) next
              if (is.numeric(df[[col]])) {
                df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
              } else {
                ux <- unique(df[[col]][!is.na(df[[col]])])
                df[[col]][is.na(df[[col]])] <- ux[which.max(tabulate(match(df[[col]][!is.na(df[[col]])], ux)))]
              }
            }
            df
          })
        } else if (mode == "MICE Imputation for remaining NAs (남은 결측치 MICE 대치)") {
          incProgress(0.5, detail = "Running MICE...")
          df <- tryCatch({
            cols_to_char <- sapply(df, function(x) is.factor(x) && length(levels(x)) > 15)
            df_mice <- df
            if (any(cols_to_char))
              for (col in names(df_mice)[cols_to_char])
                df_mice[[col]] <- as.character(df_mice[[col]])
            pred_matrix <- quickpred(df_mice, mincor = 0.3)
            imp_object  <- mice::mice(df_mice, m = 1, maxit = 2,
                                      predictorMatrix = pred_matrix,
                                      ridge = 0.1, printFlag = FALSE)
            df_res <- mice::complete(imp_object, 1)
            if (any(cols_to_char))
              for (col in names(df_res)[cols_to_char])
                df_res[[col]] <- as.factor(df_res[[col]])
            df_res
          }, error = function(e) {
            showNotification("MICE failed. Trying CART...", type = "warning")
            tryCatch({
              imp_object <- mice::mice(df, m = 1, maxit = 2, method = "cart", printFlag = FALSE)
              mice::complete(imp_object, 1)
            }, error = function(e2) {
              showNotification("MICE entirely failed. Falling back to Median.", type = "error")
              for (col in names(df)) {
                if (!any(is.na(df[[col]]))) next
                if (is.numeric(df[[col]])) {
                  df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
                } else {
                  ux <- unique(df[[col]][!is.na(df[[col]])])
                  df[[col]][is.na(df[[col]])] <- ux[which.max(tabulate(match(df[[col]][!is.na(df[[col]])], ux)))]
                }
              }
              df
            })
          })
        }
      }

      incProgress(0.2, detail = "Done!")
      return(df)
    })
  })

  output$downloadData <- downloadHandler(
    filename = function() paste0("cleaned_data_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(processed_data(), file, row.names = FALSE)
  )
}

shinyApp(ui, server)

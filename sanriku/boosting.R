## =========================================================
## Species-wise XGBoost + LOYO CV + SHAP + Sanriku plot
##
## 出力先：
## /Users/Yuki/Library/CloudStorage/Dropbox/isodata/sanriku/07/知事賞
##
## 図1：
## - 種別の年別三陸プロット
## - 10種まとめ（5列×2行、五十音順）
## - x軸「資源水準（0は調査期間の平均値）」
## - まとめ図でも全年ラベル表示
## - x/y軸タイトルは図全体で1つ
## - 種名・軸・凡例を大きめに表示
##
## 図2：
## - 直近5年間の10種サマリー
## - x軸「資源水準（0は調査期間の平均値）」
##
## 図3：
## - 種別のSHAP年変化
## - 10種まとめ（5列×2行、五十音順）
## - x軸の年表示は3年程度
## - x/y軸タイトルは図全体で1つ
## - 種名・軸・凡例を大きめに表示
## =========================================================


## =========================================================
## 0) packages
## =========================================================

library(tidyverse)
library(xgboost)
library(Matrix)
library(ggrepel)
library(patchwork)
library(ragg)


## =========================================================
## 1) directory
## =========================================================

dir_iso <- "/Users/Yuki/Library/CloudStorage/Dropbox/isodata/sanriku/07"

setwd(dir_iso)

out_dir <- file.path(
  dir_iso,
  "知事賞"
)

dir.create(
  out_dir,
  showWarnings = FALSE,
  recursive = TRUE
)


## 図1
fig1_dir <- file.path(
  out_dir,
  "図1"
)

fig1_species_dir <- file.path(
  fig1_dir,
  "種別"
)


## 図2
fig2_dir <- file.path(
  out_dir,
  "図2"
)


## 図3
fig3_dir <- file.path(
  out_dir,
  "図3"
)

fig3_species_dir <- file.path(
  fig3_dir,
  "種別"
)


dir.create(
  fig1_dir,
  showWarnings = FALSE,
  recursive = TRUE
)

dir.create(
  fig1_species_dir,
  showWarnings = FALSE,
  recursive = TRUE
)

dir.create(
  fig2_dir,
  showWarnings = FALSE,
  recursive = TRUE
)

dir.create(
  fig3_dir,
  showWarnings = FALSE,
  recursive = TRUE
)

dir.create(
  fig3_species_dir,
  showWarnings = FALSE,
  recursive = TRUE
)


## =========================================================
## 2) data
## =========================================================

df2 <- read.csv(
  "df_boosting_sanriku2.csv",
  fileEncoding = "CP932"
)

summary(df2)


## =========================================================
## 3) settings
## =========================================================

pred_vars <- c(
  "m_sst",
  "icumu_MHW",
  "icumu_MCS"
)


xgb_params <- list(
  booster = "gbtree",
  objective = "reg:squarederror",
  eta = 0.05,
  max_depth = 2,
  min_child_weight = 10,
  subsample = 0.8,
  colsample_bytree = 0.8,
  gamma = 0,
  lambda = 1,
  alpha = 0
)


nrounds_max <- 2000

early_stopping_rounds <- 30


expand_x <- 0.50
expand_y <- 0.50

inner_margin_x <- 0.02
inner_margin_y <- 0.02


jp_font <- "HiraKakuPro-W3"


## =========================================================
## 4) 種の五十音順
## =========================================================

species_order <- c(
  "イガイ類",
  "クボガイ",
  "コンブ類",
  "ツノマタ類",
  "テングサ類",
  "ヒジキ",
  "フクロフノリ",
  "マツモ",
  "マボヤ",
  "ワカメ"
)


## =========================================================
## 5) helper functions
## =========================================================


## ---------------------------------------------------------
## 学習データ基準で標準化
## ---------------------------------------------------------

scale_with_train <- function(
    train_df,
    test_df,
    vars
){
  
  mu <- sapply(
    train_df[, vars, drop = FALSE],
    mean,
    na.rm = TRUE
  )
  
  
  sd <- sapply(
    train_df[, vars, drop = FALSE],
    sd,
    na.rm = TRUE
  )
  
  
  sd[
    sd == 0 |
      is.na(sd)
  ] <- 1
  
  
  train_s <- train_df
  test_s <- test_df
  
  
  for(v in vars){
    
    train_s[[v]] <-
      (train_df[[v]] - mu[[v]]) /
      sd[[v]]
    
    
    test_s[[v]] <-
      (test_df[[v]] - mu[[v]]) /
      sd[[v]]
  }
  
  
  list(
    train = train_s,
    test = test_s
  )
}


## ---------------------------------------------------------
## XGBoost + LOYO CV
## ---------------------------------------------------------

fit_xgb_loyo <- function(
    df_sp,
    pred_vars,
    params,
    nrounds_max = 2000,
    early_stopping_rounds = 30,
    seed = 1
){
  
  set.seed(seed)
  
  
  df_sp <- df_sp %>%
    mutate(
      year = as.integer(year)
    ) %>%
    drop_na(
      any_of(
        c(
          "freq",
          "shore",
          "plot",
          "year",
          pred_vars
        )
      )
    )
  
  
  years <- sort(
    unique(
      df_sp$year
    )
  )
  
  
  best_iters <- c()
  
  
  ## -------------------------------------------------------
  ## LOYO
  ## -------------------------------------------------------
  
  for(y in years){
    
    tr <- df_sp %>%
      filter(
        year != y
      )
    
    
    te <- df_sp %>%
      filter(
        year == y
      )
    
    
    if(
      nrow(te) < 1 ||
      nrow(tr) < 30
    ){
      next
    }
    
    
    sc <- scale_with_train(
      tr,
      te,
      pred_vars
    )
    
    
    dtr <- xgb.DMatrix(
      as.matrix(
        sc$train[
          ,
          pred_vars,
          drop = FALSE
        ]
      ),
      label =
        sc$train$freq
    )
    
    
    dte <- xgb.DMatrix(
      as.matrix(
        sc$test[
          ,
          pred_vars,
          drop = FALSE
        ]
      ),
      label =
        sc$test$freq
    )
    
    
    m <- xgb.train(
      params = params,
      data = dtr,
      nrounds = nrounds_max,
      watchlist = list(
        train = dtr,
        eval = dte
      ),
      early_stopping_rounds =
        early_stopping_rounds,
      verbose = 0
    )
    
    
    best_iters <- c(
      best_iters,
      m$best_iteration
    )
  }
  
  
  ## -------------------------------------------------------
  ## best iterationの中央値
  ## -------------------------------------------------------
  
  best_n <- if(
    length(best_iters) == 0
  ){
    
    200
    
  } else {
    
    as.integer(
      median(
        best_iters,
        na.rm = TRUE
      )
    )
  }
  
  
  ## -------------------------------------------------------
  ## 全データで最終モデル
  ## -------------------------------------------------------
  
  mu_full <- sapply(
    df_sp[
      ,
      pred_vars,
      drop = FALSE
    ],
    mean,
    na.rm = TRUE
  )
  
  
  sd_full <- sapply(
    df_sp[
      ,
      pred_vars,
      drop = FALSE
    ],
    sd,
    na.rm = TRUE
  )
  
  
  sd_full[
    sd_full == 0 |
      is.na(sd_full)
  ] <- 1
  
  
  df_scaled <- df_sp
  
  
  for(v in pred_vars){
    
    df_scaled[[v]] <-
      (df_sp[[v]] - mu_full[[v]]) /
      sd_full[[v]]
  }
  
  
  dfull <- xgb.DMatrix(
    as.matrix(
      df_scaled[
        ,
        pred_vars,
        drop = FALSE
      ]
    ),
    label =
      df_scaled$freq
  )
  
  
  model <- xgb.train(
    params = params,
    data = dfull,
    nrounds = best_n,
    verbose = 0
  )
  
  
  list(
    model = model,
    best_nrounds = best_n,
    df_raw = df_sp,
    df_scaled = df_scaled
  )
}


## ---------------------------------------------------------
## 環境総合SHAP
##
## env_score = prediction - bias
## ---------------------------------------------------------

compute_env_score_interactions <- function(
    model,
    X_scaled
){
  
  p <- ncol(
    X_scaled
  )
  
  n <- nrow(
    X_scaled
  )
  
  
  pi_raw <- predict(
    model,
    X_scaled,
    predinteraction = TRUE
  )
  
  
  p1 <- p + 1
  
  
  pi_arr <- array(
    pi_raw,
    dim = c(
      n,
      p1,
      p1
    )
  )
  
  
  bias <- pi_arr[
    ,
    p1,
    p1
  ]
  
  
  pred <- predict(
    model,
    X_scaled
  )
  
  
  tibble(
    env_score =
      pred - bias,
    pred = pred,
    bias = bias
  )
}


## ---------------------------------------------------------
## plot-level data
## ---------------------------------------------------------

build_df_plot <- function(
    fit,
    pred_vars
){
  
  X_scaled <- as.matrix(
    fit$df_scaled[
      ,
      pred_vars,
      drop = FALSE
    ]
  )
  
  
  add <- compute_env_score_interactions(
    fit$model,
    X_scaled
  )
  
  
  fit$df_raw %>%
    select(
      plot,
      shore,
      year,
      species,
      freq,
      all_of(
        pred_vars
      )
    ) %>%
    bind_cols(
      add
    ) %>%
    mutate(
      year =
        as.integer(year),
      
      ## 種ごとの調査期間平均からの偏差
      freq_centered =
        freq -
        mean(
          freq,
          na.rm = TRUE
        )
    )
}


## ---------------------------------------------------------
## 年平均 ± SE
## ---------------------------------------------------------

summarise_year_mean_se <- function(
    df_plot
){
  
  df_plot %>%
    group_by(
      year
    ) %>%
    summarise(
      
      ## x軸：資源水準
      mean2 =
        mean(
          freq_centered,
          na.rm = TRUE
        ),
      
      sd2 =
        sd(
          freq_centered,
          na.rm = TRUE
        ),
      
      n2 =
        sum(
          is.finite(
            freq_centered
          )
        ),
      
      se2 =
        ifelse(
          n2 > 1,
          sd2 / sqrt(n2),
          0
        ),
      
      
      ## y軸：環境影響
      mean =
        mean(
          env_score,
          na.rm = TRUE
        ),
      
      sd1 =
        sd(
          env_score,
          na.rm = TRUE
        ),
      
      n1 =
        sum(
          is.finite(
            env_score
          )
        ),
      
      se =
        ifelse(
          n1 > 1,
          sd1 / sqrt(n1),
          0
        ),
      
      n =
        pmin(
          n1,
          n2
        ),
      
      .groups = "drop"
    ) %>%
    arrange(
      year
    )
}


## =========================================================
## 6) 図1作成関数
## =========================================================

plot_sanriku_yearmean_cross <- function(
    df_year,
    title = NULL,
    expand_x = 0.50,
    expand_y = 0.50,
    inner_margin_x = 0.02,
    inner_margin_y = 0.02,
    combined = FALSE
){
  
  ## -------------------------------------------------------
  ## 最新5年
  ## -------------------------------------------------------
  
  latest_year <- max(
    df_year$year,
    na.rm = TRUE
  )
  
  
  df_year <- df_year %>%
    mutate(
      is_recent5 =
        year >=
        (
          latest_year - 4
        )
    )
  
  
  ## -------------------------------------------------------
  ## 軸範囲
  ## -------------------------------------------------------
  
  x_rng <- range(
    df_year$mean2,
    na.rm = TRUE
  )
  
  
  y_rng <- range(
    df_year$mean,
    na.rm = TRUE
  )
  
  
  x_pad <-
    diff(x_rng) *
    expand_x
  
  
  y_pad <-
    diff(y_rng) *
    expand_y
  
  
  if(
    !is.finite(x_pad) ||
    x_pad == 0
  ){
    x_pad <- 0.05
  }
  
  
  if(
    !is.finite(y_pad) ||
    y_pad == 0
  ){
    y_pad <- 0.05
  }
  
  
  x_min <-
    x_rng[1] -
    x_pad
  
  x_max <-
    x_rng[2] +
    x_pad
  
  
  y_min <-
    y_rng[1] -
    y_pad
  
  y_max <-
    y_rng[2] +
    y_pad
  
  
  ## -------------------------------------------------------
  ## SEを図内に収める
  ## -------------------------------------------------------
  
  x_in_pad <-
    (
      x_max -
        x_min
    ) *
    inner_margin_x
  
  
  y_in_pad <-
    (
      y_max -
        y_min
    ) *
    inner_margin_y
  
  
  x_min_in <-
    x_min +
    x_in_pad
  
  
  x_max_in <-
    x_max -
    x_in_pad
  
  
  y_min_in <-
    y_min +
    y_in_pad
  
  
  y_max_in <-
    y_max -
    y_in_pad
  
  
  df_plot <- df_year %>%
    mutate(
      
      xmin_clip =
        pmax(
          mean2 - se2,
          x_min_in
        ),
      
      xmax_clip =
        pmin(
          mean2 + se2,
          x_max_in
        ),
      
      ymin_clip =
        pmax(
          mean - se,
          y_min_in
        ),
      
      ymax_clip =
        pmin(
          mean + se,
          y_max_in
        )
    )
  
  
  ## -------------------------------------------------------
  ## サイズ
  ## -------------------------------------------------------
  
  point_size_normal <-
    ifelse(
      combined,
      1.8,
      3
    )
  
  
  point_size_recent <-
    ifelse(
      combined,
      3.2,
      5
    )
  
  
  cross_width <-
    ifelse(
      combined,
      0.35,
      0.6
    )
  
  
  path_width <-
    ifelse(
      combined,
      0.4,
      0.5
    )
  
  
  ## ★まとめ図でも年ラベルを大きめに
  year_text_size <-
    ifelse(
      combined,
      2.7,
      3
    )
  
  
  ## =======================================================
  ## Plot
  ## =======================================================
  
  p <-
    ggplot(
      df_plot,
      aes(
        x = mean2,
        y = mean
      )
    ) +
    
    ## -----------------------------------------------------
  ## 背景四象限
  ## -----------------------------------------------------
  
  geom_rect(
    xmin = 0,
    xmax = x_max,
    ymin = y_min,
    ymax = 0,
    fill = "orange",
    alpha = 0.06
  ) +
    
    geom_rect(
      xmin = x_min,
      xmax = 0,
      ymin = y_min,
      ymax = 0,
      fill = "red",
      alpha = 0.06
    ) +
    
    geom_rect(
      xmin = x_min,
      xmax = 0,
      ymin = 0,
      ymax = y_max,
      fill = "yellow",
      alpha = 0.06
    ) +
    
    geom_rect(
      xmin = 0,
      xmax = x_max,
      ymin = 0,
      ymax = y_max,
      fill = "green",
      alpha = 0.06
    ) +
    
    ## -----------------------------------------------------
  ## 0線
  ## -----------------------------------------------------
  
  geom_hline(
    yintercept = 0,
    linewidth = 0.3
  ) +
    
    geom_vline(
      xintercept = 0,
      linewidth = 0.3
    ) +
    
    ## -----------------------------------------------------
  ## SE
  ## -----------------------------------------------------
  
  geom_segment(
    aes(
      x = mean2,
      xend = mean2,
      y = ymin_clip,
      yend = ymax_clip
    ),
    linewidth =
      cross_width,
    color = "black"
  ) +
    
    geom_segment(
      aes(
        x = xmin_clip,
        xend = xmax_clip,
        y = mean,
        yend = mean
      ),
      linewidth =
        cross_width,
      color = "black"
    ) +
    
    ## -----------------------------------------------------
  ## 年軌跡
  ## -----------------------------------------------------
  
  geom_path(
    alpha = 0.8,
    linewidth =
      path_width
  ) +
    
    ## 通常年
    geom_point(
      data =
        df_plot %>%
        filter(
          !is_recent5
        ),
      aes(
        color = year
      ),
      size =
        point_size_normal,
      alpha = 0.9
    ) +
    
    ## 直近5年
    geom_point(
      data =
        df_plot %>%
        filter(
          is_recent5
        ),
      aes(
        color = year
      ),
      shape = 18,
      size =
        point_size_recent
    ) +
    
    ## -----------------------------------------------------
  ## ★全年ラベル
  ## -----------------------------------------------------
  
  ggrepel::geom_text_repel(
    aes(
      label = year
    ),
    size =
      year_text_size,
    family =
      jp_font,
    color = "black",
    segment.color = NA,
    max.overlaps = Inf,
    force = 1.5,
    box.padding =
      ifelse(
        combined,
        0.10,
        0.25
      ),
    point.padding =
      ifelse(
        combined,
        0.05,
        0.15
      ),
    seed = 1
  ) +
    
    ## -----------------------------------------------------
  ## 年の濃淡
  ## -----------------------------------------------------
  
  scale_color_gradient(
    low = "white",
    high = "black"
  ) +
    
    ## -----------------------------------------------------
  ## 描画範囲
  ## -----------------------------------------------------
  
  coord_cartesian(
    xlim = c(
      x_min,
      x_max
    ),
    ylim = c(
      y_min,
      y_max
    ),
    clip = "off"
  ) +
    
    ## -----------------------------------------------------
  ## 軸・凡例
  ## -----------------------------------------------------
  
  labs(
    x =
      "資源水準（0は調査期間の平均値）",
    y =
      "環境からの影響",
    color =
      "年",
    title =
      title
  ) +
    
    theme_bw(
      base_family =
        jp_font
    ) +
    
    theme(
      
      ## ★種名
      plot.title =
        element_text(
          size =
            ifelse(
              combined,
              18,
              18
            ),
          hjust = 0.5
        ),
      
      ## ★x/y軸タイトル
      axis.title =
        element_text(
          size =
            ifelse(
              combined,
              16,
              14
            )
        ),
      
      ## ★軸目盛り
      axis.text =
        element_text(
          size =
            ifelse(
              combined,
              11,
              11
            )
        ),
      
      ## ★凡例タイトル
      legend.title =
        element_text(
          size =
            ifelse(
              combined,
              18,
              11
            )
        ),
      
      ## ★凡例文字
      legend.text =
        element_text(
          size =
            ifelse(
              combined,
              16,
              10
            )
        ),
      
      plot.margin =
        margin(
          7,
          7,
          7,
          7
        )
    )
  
  
  return(
    p
  )
}


## =========================================================
## 7) species-wise XGBoost
## =========================================================

species_list <-
  species_order[
    species_order %in%
      unique(
        df2$species
      )
  ]


fits <- list()

sanriku_yearmean <- list()

plots <- list()


for(sp in species_list){
  
  message(
    "Fitting: ",
    sp
  )
  
  
  df_sp <-
    df2 %>%
    filter(
      species == sp
    )
  
  
  if(
    nrow(df_sp) <
    100
  ){
    
    message(
      "  skipped (too few rows): ",
      sp
    )
    
    next
  }
  
  
  fit <- fit_xgb_loyo(
    df_sp,
    pred_vars,
    xgb_params,
    nrounds_max =
      nrounds_max,
    early_stopping_rounds =
      early_stopping_rounds,
    seed = 1
  )
  
  
  df_plot <- build_df_plot(
    fit,
    pred_vars
  )
  
  
  df_year <-
    summarise_year_mean_se(
      df_plot
    )
  
  
  fits[[sp]] <-
    fit
  
  
  sanriku_yearmean[[sp]] <-
    df_year
  
  
  plots[[sp]] <-
    plot_sanriku_yearmean_cross(
      df_year,
      title = sp,
      expand_x = expand_x,
      expand_y = expand_y,
      inner_margin_x =
        inner_margin_x,
      inner_margin_y =
        inner_margin_y,
      combined = FALSE
    )
}


## =========================================================
## 8) 図1：種別図を保存
## =========================================================

for(sp in names(plots)){
  
  message(
    "Saving Fig.1: ",
    sp
  )
  
  
  p <- plots[[sp]]
  
  
  if(
    is.null(p)
  ){
    next
  }
  
  
  sp_safe <- gsub(
    "[^[:alnum:]一-龥ぁ-んァ-ン]",
    "_",
    sp
  )
  
  
  ggsave(
    filename =
      file.path(
        fig1_species_dir,
        paste0(
          "図1_三陸プロット_",
          sp_safe,
          ".png"
        )
      ),
    plot = p,
    width = 11.69,
    height = 8.27,
    units = "in",
    dpi = 300,
    device =
      ragg::agg_png
  )
}


## =========================================================
## 9) 図1：10種まとめ（5列×2行）
## =========================================================

plots_combined <- list()


for(sp in species_order){
  
  if(
    !sp %in%
    names(
      sanriku_yearmean
    )
  ){
    next
  }
  
  
  df_year <-
    sanriku_yearmean[[sp]]
  
  
  plots_combined[[sp]] <-
    plot_sanriku_yearmean_cross(
      df_year,
      title = sp,
      expand_x = expand_x,
      expand_y = expand_y,
      inner_margin_x =
        inner_margin_x,
      inner_margin_y =
        inner_margin_y,
      combined = TRUE
    )
}


plots_combined_ordered <-
  plots_combined[
    species_order[
      species_order %in%
        names(
          plots_combined
        )
    ]
  ]


fig1_all <-
  patchwork::wrap_plots(
    plots_combined_ordered,
    ncol = 5,
    nrow = 2
  ) +
  patchwork::plot_layout(
    guides = "collect",
    axis_titles = "collect"
  ) &
  theme(
    legend.position =
      "right"
  )


print(
  fig1_all
)


ggsave(
  filename =
    file.path(
      fig1_dir,
      "図1_三陸プロット_10種_5列2行.png"
    ),
  plot =
    fig1_all,
  width = 20,
  height = 9,
  units = "in",
  dpi = 300,
  device =
    ragg::agg_png
)


## =========================================================
## 10) 図2：直近5年間の10種サマリー
## =========================================================

df_recent5_all <-
  map_dfr(
    names(
      sanriku_yearmean
    ),
    function(sp){
      
      df <-
        sanriku_yearmean[[sp]]
      
      
      if(
        is.null(df) ||
        nrow(df) == 0
      ){
        return(
          NULL
        )
      }
      
      
      latest_year <- max(
        df$year,
        na.rm = TRUE
      )
      
      
      df_recent <-
        df %>%
        filter(
          year >=
            latest_year - 4
        )
      
      
      if(
        nrow(df_recent) == 0
      ){
        return(
          NULL
        )
      }
      
      
      ## x軸
      mean_x <- mean(
        df_recent$mean2,
        na.rm = TRUE
      )
      
      
      sd_x <- sd(
        df_recent$mean2,
        na.rm = TRUE
      )
      
      
      n_x <- sum(
        is.finite(
          df_recent$mean2
        )
      )
      
      
      se_x <- ifelse(
        n_x > 1,
        sd_x / sqrt(n_x),
        0
      )
      
      
      ## y軸
      mean_y <- mean(
        df_recent$mean,
        na.rm = TRUE
      )
      
      
      sd_y <- sd(
        df_recent$mean,
        na.rm = TRUE
      )
      
      
      n_y <- sum(
        is.finite(
          df_recent$mean
        )
      )
      
      
      se_y <- ifelse(
        n_y > 1,
        sd_y / sqrt(n_y),
        0
      )
      
      
      tibble(
        species = sp,
        mean_x = mean_x,
        mean_y = mean_y,
        se_x = se_x,
        se_y = se_y
      )
    }
  )


df_recent5_all <-
  df_recent5_all %>%
  mutate(
    species =
      factor(
        species,
        levels =
          species_order
      )
  ) %>%
  arrange(
    species
  )


## ---------------------------------------------------------
## 図2の軸範囲
## ---------------------------------------------------------

x_rng <- range(
  df_recent5_all$mean_x,
  na.rm = TRUE
)


y_rng <- range(
  df_recent5_all$mean_y,
  na.rm = TRUE
)


x_pad <-
  diff(x_rng) *
  0.4


y_pad <-
  diff(y_rng) *
  0.4


if(
  !is.finite(x_pad) ||
  x_pad == 0
){
  x_pad <- 0.05
}


if(
  !is.finite(y_pad) ||
  y_pad == 0
){
  y_pad <- 0.05
}


x_min <-
  x_rng[1] -
  x_pad


x_max <-
  x_rng[2] +
  x_pad


y_min <-
  y_rng[1] -
  y_pad


y_max <-
  y_rng[2] +
  y_pad


## ---------------------------------------------------------
## 図2作成
## ---------------------------------------------------------

fig_summary <-
  ggplot(
    df_recent5_all,
    aes(
      x = mean_x,
      y = mean_y
    )
  ) +
  
  geom_rect(
    xmin = 0,
    xmax = x_max,
    ymin = y_min,
    ymax = 0,
    fill = "orange",
    alpha = 0.1
  ) +
  
  geom_rect(
    xmin = x_min,
    xmax = 0,
    ymin = y_min,
    ymax = 0,
    fill = "red",
    alpha = 0.1
  ) +
  
  geom_rect(
    xmin = x_min,
    xmax = 0,
    ymin = 0,
    ymax = y_max,
    fill = "yellow",
    alpha = 0.1
  ) +
  
  geom_rect(
    xmin = 0,
    xmax = x_max,
    ymin = 0,
    ymax = y_max,
    fill = "green",
    alpha = 0.1
  ) +
  
  geom_hline(
    yintercept = 0,
    linewidth = 0.3
  ) +
  
  geom_vline(
    xintercept = 0,
    linewidth = 0.3
  ) +
  
  geom_point(
    size = 6,
    color = "black"
  ) +
  
  ggrepel::geom_text_repel(
    aes(
      label = species
    ),
    size = 5,
    family =
      jp_font,
    segment.color = NA,
    box.padding = 0.4,
    point.padding = 0.3,
    max.overlaps = Inf
  ) +
  
  coord_cartesian(
    xlim = c(
      x_min,
      x_max
    ),
    ylim = c(
      y_min,
      y_max
    )
  ) +
  
  labs(
    x =
      "資源水準（0は調査期間の平均値）",
    y =
      "環境からの影響",
    title = ""
  ) +
  
  theme_bw(
    base_family =
      jp_font
  ) +
  
  theme(
    axis.title =
      element_text(
        size = 16
      ),
    axis.text =
      element_text(
        size = 14
      )
  )


print(
  fig_summary
)


ggsave(
  filename =
    file.path(
      fig2_dir,
      "図2_直近5年_10種三陸プロット.png"
    ),
  plot =
    fig_summary,
  width = 13,
  height = 9,
  units = "in",
  dpi = 300,
  device =
    ragg::agg_png
)


## =========================================================
## 11) 図3：各環境要因のSHAPを年別に計算
## =========================================================

shap_yearly <- list()


for(sp in names(fits)){
  
  fit <- fits[[sp]]
  
  
  X_scaled <- as.matrix(
    fit$df_scaled[
      ,
      pred_vars,
      drop = FALSE
    ]
  )
  
  
  shap_vals <- predict(
    fit$model,
    X_scaled,
    predcontrib = TRUE
  )
  
  
  shap_df <- as.data.frame(
    shap_vals
  )
  
  
  ## BIASを除外
  shap_df <- shap_df[
    ,
    colnames(shap_df) !=
      "BIAS",
    drop = FALSE
  ]
  
  
  colnames(
    shap_df
  ) <-
    pred_vars
  
  
  df_year <-
    fit$df_raw %>%
    select(
      year
    ) %>%
    bind_cols(
      shap_df
    ) %>%
    pivot_longer(
      -year,
      names_to =
        "variable",
      values_to =
        "shap"
    ) %>%
    group_by(
      year,
      variable
    ) %>%
    summarise(
      
      importance =
        mean(
          abs(shap),
          na.rm = TRUE
        ),
      
      direction =
        mean(
          shap,
          na.rm = TRUE
        ),
      
      .groups =
        "drop"
    )
  
  
  shap_yearly[[sp]] <-
    df_year
}


## =========================================================
## 12) 図3作成関数
## =========================================================

plot_shap_stack <- function(
    df_plot,
    title = NULL,
    combined = FALSE
){
  
  yrs <- sort(
    unique(
      df_plot$year
    )
  )
  
  
  ## 最初・中央・最後
  year_breaks <- unique(
    round(
      seq(
        min(yrs),
        max(yrs),
        length.out = 3
      )
    )
  )
  
  
  p <-
    ggplot(
      df_plot,
      aes(
        x = year,
        y = direction,
        fill = variable
      )
    ) +
    
    geom_col(
      color = "black",
      linewidth =
        ifelse(
          combined,
          0.15,
          0.3
        )
    ) +
    
    geom_hline(
      yintercept = 0,
      color = "black",
      linewidth = 0.5
    ) +
    
    scale_fill_manual(
      values = c(
        "m_sst" =
          "gold",
        "icumu_MHW" =
          "tomato",
        "icumu_MCS" =
          "skyblue"
      ),
      breaks = c(
        "m_sst",
        "icumu_MHW",
        "icumu_MCS"
      ),
      labels = c(
        "m_sst" =
          "平均水温",
        "icumu_MHW" =
          "熱波",
        "icumu_MCS" =
          "寒波"
      )
    ) +
    
    scale_x_continuous(
      breaks =
        year_breaks
    ) +
    
    labs(
      x =
        "年",
      y =
        "環境からの影響（SHAP平均）",
      fill =
        "環境要因",
      title =
        title
    ) +
    
    theme_bw(
      base_family =
        jp_font
    ) +
    
    theme(
      
      ## ★種名
      plot.title =
        element_text(
          size =
            ifelse(
              combined,
              18,
              18
            ),
          hjust = 0.5
        ),
      
      ## ★x/y軸タイトル
      axis.title =
        element_text(
          size =
            ifelse(
              combined,
              16,
              14
            )
        ),
      
      ## ★軸の数値・年
      axis.text =
        element_text(
          size =
            ifelse(
              combined,
              11,
              11
            )
        ),
      
      ## ★凡例タイトル
      legend.title =
        element_text(
          size =
            ifelse(
              combined,
              18,
              11
            )
        ),
      
      ## ★凡例文字
      legend.text =
        element_text(
          size =
            ifelse(
              combined,
              16,
              10
            )
        ),
      
      plot.margin =
        margin(
          7,
          7,
          7,
          7
        )
    )
  
  
  return(
    p
  )
}


## =========================================================
## 13) 図3：種別図
## =========================================================

shap_plots <- list()


for(sp in species_order){
  
  if(
    !sp %in%
    names(
      shap_yearly
    )
  ){
    next
  }
  
  
  message(
    "Creating SHAP plot: ",
    sp
  )
  
  
  df_plot <-
    shap_yearly[[sp]]
  
  
  if(
    is.null(df_plot) ||
    nrow(df_plot) == 0
  ){
    next
  }
  
  
  p_stack <-
    plot_shap_stack(
      df_plot,
      title = sp,
      combined = FALSE
    )
  
  
  shap_plots[[sp]] <-
    p_stack
  
  
  print(
    p_stack
  )
  
  
  sp_safe <- gsub(
    "[^[:alnum:]一-龥ぁ-んァ-ン]",
    "_",
    sp
  )
  
  
  ggsave(
    filename =
      file.path(
        fig3_species_dir,
        paste0(
          "図3_SHAP_",
          sp_safe,
          ".png"
        )
      ),
    plot =
      p_stack,
    width = 11,
    height = 8,
    units = "in",
    dpi = 300,
    device =
      ragg::agg_png
  )
}


## =========================================================
## 14) 図3：10種まとめ（5列×2行）
## =========================================================

shap_plots_combined <- list()


for(sp in species_order){
  
  if(
    !sp %in%
    names(
      shap_yearly
    )
  ){
    next
  }
  
  
  df_plot <-
    shap_yearly[[sp]]
  
  
  shap_plots_combined[[sp]] <-
    plot_shap_stack(
      df_plot,
      title = sp,
      combined = TRUE
    )
}


shap_plots_combined_ordered <-
  shap_plots_combined[
    species_order[
      species_order %in%
        names(
          shap_plots_combined
        )
    ]
  ]


fig3_all <-
  patchwork::wrap_plots(
    shap_plots_combined_ordered,
    ncol = 5,
    nrow = 2
  ) +
  patchwork::plot_layout(
    guides = "collect",
    axis_titles = "collect"
  ) &
  theme(
    legend.position =
      "right"
  )


print(
  fig3_all
)


ggsave(
  filename =
    file.path(
      fig3_dir,
      "図3_SHAP_10種_5列2行.png"
    ),
  plot =
    fig3_all,
  width = 20,
  height = 9,
  units = "in",
  dpi = 300,
  device =
    ragg::agg_png
)


## =========================================================
## 15) CSV保存
## =========================================================


## ---------------------------------------------------------
## 図2サマリー
## ---------------------------------------------------------

write.csv(
  df_recent5_all,
  file =
    file.path(
      fig2_dir,
      "図2_直近5年_10種サマリー.csv"
    ),
  row.names = FALSE,
  fileEncoding =
    "CP932"
)


## ---------------------------------------------------------
## 図1 年別データ
## ---------------------------------------------------------

sanriku_yearmean_all <-
  map_dfr(
    names(
      sanriku_yearmean
    ),
    function(sp){
      
      sanriku_yearmean[[sp]] %>%
        mutate(
          species = sp
        )
    }
  ) %>%
  mutate(
    species =
      factor(
        species,
        levels =
          species_order
      )
  ) %>%
  arrange(
    species,
    year
  )


write.csv(
  sanriku_yearmean_all,
  file =
    file.path(
      fig1_dir,
      "図1_年別三陸プロットデータ.csv"
    ),
  row.names = FALSE,
  fileEncoding =
    "CP932"
)


## ---------------------------------------------------------
## 図3 年別SHAP
## ---------------------------------------------------------

shap_yearly_all <-
  map_dfr(
    names(
      shap_yearly
    ),
    function(sp){
      
      shap_yearly[[sp]] %>%
        mutate(
          species = sp
        )
    }
  ) %>%
  mutate(
    species =
      factor(
        species,
        levels =
          species_order
      )
  ) %>%
  arrange(
    species,
    year,
    variable
  )


write.csv(
  shap_yearly_all,
  file =
    file.path(
      fig3_dir,
      "図3_年別SHAPデータ.csv"
    ),
  row.names = FALSE,
  fileEncoding =
    "CP932"
)


## =========================================================
## 16) 保存確認
## =========================================================

message("")
message(
  "=============================================="
)

message(
  "解析完了"
)

message(
  "=============================================="
)

message(
  "図1保存先：",
  fig1_dir
)

message(
  "図2保存先：",
  fig2_dir
)

message(
  "図3保存先：",
  fig3_dir
)

message(
  "=============================================="
)


print(
  list.files(
    out_dir,
    recursive = TRUE
  )
)
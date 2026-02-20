## =========================================================
## Species-wise XGBoost + LOYO CV + SHAP(interactions) + Sanriku plot (annual mean ± SE)
## - x-axis: freq_centered (freq - species mean)
## - y-axis: env_score = total SHAP incl. interactions (= pred - bias)
## - Visualize ONLY annual Sanriku mean (across all plots/shores)
## - Cross bars: mean ± SE for both x and y (SE=0 if n==1 -> always drawable)
## - Year labels: NO leader lines (no segments)
## =========================================================

require(tidyverse)
require(xgboost)
require(Matrix)
require(ggrepel)

dir_iso = "/Users/Yuki/Dropbox/isodata/sanriku/07/"
setwd(dir_iso)
df2 = read.csv("df_boosting_sanriku2.csv", fileEncoding = "CP932")
summary(df2)


## -------------------------
## 0) settings
## -------------------------
pred_vars <- c("m_sst", "icumu_MHW", "icumu_MCS")

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

# Expand plotting/background area (make it "wide")
expand_x <- 0.50
expand_y <- 0.50

# Keep cross bars away from edges a bit (optional)
inner_margin_x <- 0.02
inner_margin_y <- 0.02

## -------------------------
## 1) helper functions
## -------------------------

scale_with_train <- function(train_df, test_df, vars){
  mu <- sapply(train_df[, vars, drop=FALSE], mean, na.rm=TRUE)
  sd <- sapply(train_df[, vars, drop=FALSE], sd,   na.rm=TRUE)
  sd[sd == 0 | is.na(sd)] <- 1
  
  train_s <- train_df
  test_s  <- test_df
  for(v in vars){
    train_s[[v]] <- (train_df[[v]] - mu[[v]]) / sd[[v]]
    test_s[[v]]  <- (test_df[[v]]  - mu[[v]]) / sd[[v]]
  }
  list(train=train_s, test=test_s)
}

fit_xgb_loyo <- function(df_sp, pred_vars, params,
                         nrounds_max=2000, early_stopping_rounds=30, seed=1){
  
  set.seed(seed)
  
  df_sp <- df_sp %>%
    mutate(year = as.integer(year)) %>%
    drop_na(any_of(c("freq", "shore", "plot", "year", pred_vars)))
  
  years <- sort(unique(df_sp$year))
  best_iters <- c()
  
  for(y in years){
    tr <- df_sp %>% filter(year != y)
    te <- df_sp %>% filter(year == y)
    if(nrow(te) < 1 || nrow(tr) < 30) next
    
    sc <- scale_with_train(tr, te, pred_vars)
    
    dtr <- xgb.DMatrix(as.matrix(sc$train[, pred_vars, drop=FALSE]), label = sc$train$freq)
    dte <- xgb.DMatrix(as.matrix(sc$test[,  pred_vars, drop=FALSE]), label = sc$test$freq)
    
    m <- xgb.train(
      params = params,
      data = dtr,
      nrounds = nrounds_max,
      watchlist = list(train=dtr, eval=dte),
      early_stopping_rounds = early_stopping_rounds,
      verbose = 0
    )
    best_iters <- c(best_iters, m$best_iteration)
  }
  
  best_n <- if(length(best_iters)==0) 200 else as.integer(median(best_iters, na.rm=TRUE))
  
  # final fit on full data (scale on full for SHAP)
  mu_full <- sapply(df_sp[, pred_vars, drop=FALSE], mean, na.rm=TRUE)
  sd_full <- sapply(df_sp[, pred_vars, drop=FALSE], sd,   na.rm=TRUE)
  sd_full[sd_full == 0 | is.na(sd_full)] <- 1
  
  df_scaled <- df_sp
  for(v in pred_vars){
    df_scaled[[v]] <- (df_sp[[v]] - mu_full[[v]]) / sd_full[[v]]
  }
  
  dfull <- xgb.DMatrix(as.matrix(df_scaled[, pred_vars, drop=FALSE]), label = df_scaled$freq)
  
  model <- xgb.train(
    params = params,
    data = dfull,
    nrounds = best_n,
    verbose = 0
  )
  
  list(model=model, best_nrounds=best_n, df_raw=df_sp, df_scaled=df_scaled)
}

# env_score including interactions:
# env_score = prediction - bias, where bias from predinteraction tensor.
compute_env_score_interactions <- function(model, X_scaled){
  p <- ncol(X_scaled)
  n <- nrow(X_scaled)
  
  pi_raw <- predict(model, X_scaled, predinteraction = TRUE)
  p1 <- p + 1
  pi_arr <- array(pi_raw, dim = c(n, p1, p1))
  
  bias <- pi_arr[, p1, p1]
  pred <- predict(model, X_scaled)
  
  tibble(env_score = pred - bias, pred = pred, bias = bias)
}

# build plot-level df with centered freq + env_score
build_df_plot <- function(fit, pred_vars){
  X_scaled <- as.matrix(fit$df_scaled[, pred_vars, drop=FALSE])
  add <- compute_env_score_interactions(fit$model, X_scaled)
  
  fit$df_raw %>%
    select(plot, shore, year, species, freq, all_of(pred_vars)) %>%
    bind_cols(add) %>%
    mutate(
      year = as.integer(year),
      freq_centered = freq - mean(freq, na.rm = TRUE)  # species mean
    )
}

# (推奨) annual Sanriku mean ± SE (SE=0 if n==1)
summarise_year_mean_se <- function(df_plot){
  df_plot %>%
    group_by(year) %>%
    summarise(
      mean2 = mean(freq_centered, na.rm = TRUE),
      sd2   = sd(freq_centered,  na.rm = TRUE),
      n2    = sum(is.finite(freq_centered)),
      se2   = ifelse(n2 > 1, sd2 / sqrt(n2), 0),
      
      mean  = mean(env_score, na.rm = TRUE),
      sd1   = sd(env_score,  na.rm = TRUE),
      n1    = sum(is.finite(env_score)),
      se    = ifelse(n1 > 1, sd1 / sqrt(n1), 0),
      
      n     = pmin(n1, n2),
      .groups = "drop"
    ) %>%
    arrange(as.integer(year))
}

plot_sanriku_yearmean_cross <- function(df_year, title=NULL,
                                        expand_x=0.50, expand_y=0.50,
                                        inner_margin_x=0.02, inner_margin_y=0.02,
                                        label_years=NULL,
                                        label_size=3){
  
  # -------------------------
  # 最新5年フラグ
  # -------------------------
  latest_year <- max(df_year$year, na.rm = TRUE)
  df_year <- df_year %>%
    mutate(
      is_recent5 = year >= (latest_year - 4)
    )
  
  # axis range
  x_rng <- range(df_year$mean2, na.rm=TRUE)
  y_rng <- range(df_year$mean,  na.rm=TRUE)
  
  x_pad <- diff(x_rng) * expand_x
  y_pad <- diff(y_rng) * expand_y
  if(!is.finite(x_pad) || x_pad == 0) x_pad <- 0.05
  if(!is.finite(y_pad) || y_pad == 0) y_pad <- 0.05
  
  x_min <- x_rng[1] - x_pad
  x_max <- x_rng[2] + x_pad
  y_min <- y_rng[1] - y_pad
  y_max <- y_rng[2] + y_pad
  
  x_in_pad <- (x_max - x_min) * inner_margin_x
  y_in_pad <- (y_max - y_min) * inner_margin_y
  
  x_min_in <- x_min + x_in_pad
  x_max_in <- x_max - x_in_pad
  y_min_in <- y_min + y_in_pad
  y_max_in <- y_max - y_in_pad
  
  df_plot <- df_year %>%
    mutate(
      xmin_clip = pmax(mean2 - se2, x_min_in),
      xmax_clip = pmin(mean2 + se2, x_max_in),
      ymin_clip = pmax(mean  - se,  y_min_in),
      ymax_clip = pmin(mean  + se,  y_max_in)
    )
  
  df_lab <- if(is.null(label_years)) df_plot else df_plot %>% filter(year %in% label_years)
  
  ggplot(df_plot, aes(x = mean2, y = mean)) +
    
    # 背景
    geom_rect(xmin = 0, xmax = x_max, ymin = y_min, ymax = 0,
              fill = "orange", alpha = 0.06) +
    geom_rect(xmin = x_min, xmax = 0, ymin = y_min, ymax = 0,
              fill = "red", alpha = 0.06) +
    geom_rect(xmin = x_min, xmax = 0, ymin = 0, ymax = y_max,
              fill = "yellow", alpha = 0.06) +
    geom_rect(xmin = 0, xmax = x_max, ymin = 0, ymax = y_max,
              fill = "green", alpha = 0.06) +
    
    geom_hline(yintercept = 0, linewidth = 0.3) +
    geom_vline(xintercept = 0, linewidth = 0.3) +
    
    # cross bars
    geom_segment(aes(x = mean2, xend = mean2, y = ymin_clip, yend = ymax_clip),
                 linewidth = 0.6, color = "black") +
    geom_segment(aes(x = xmin_clip, xend = xmax_clip, y = mean, yend = mean),
                 linewidth = 0.6, color = "black") +
    
    # trajectory
    geom_path(alpha = 0.8) +
    
    # ---- 通常年（丸）----
  geom_point(
    data = df_plot %>% filter(!is_recent5),
    aes(color = year),
    size = 3,
    alpha = 0.9
  ) +
    
    # ---- 直近5年（星）----
  geom_point(
    data = df_plot %>% filter(is_recent5),
    aes(color = year),
    shape = 18,        # ★
    size = 5,
    stroke = 1.2
  ) +
    
    # ggrepel::geom_text_repel(
    #   data = df_lab,
    #   aes(label = year),
    #   size = label_size,
    #   segment.color = NA,
    #   min.segment.length = Inf,
    #   box.padding = 0.25,
    #   point.padding = 0.15,
    #   max.overlaps = Inf
    # ) +
    
    # scale_color_viridis_c() +
    scale_color_gradient(low = "white", high = "black")+
    coord_cartesian(xlim = c(x_min, x_max), ylim = c(y_min, y_max)) +
    
    # labs(
    #   x = "Resource level (freq centered at species mean): annual mean ± SE",
    #   y = "Environmental axis (total SHAP incl. interactions): annual mean ± SE",
    #   color = "Year",
    #   title = title
    # ) +
    labs(
      x = "資源レベル（0が平均）",
      y = "環境からの影響",
      color = "年",
      title = title
    ) +
    theme_bw(base_family = "HiraKakuPro-W3")
}

## -------------------------
## 2) run species-wise
## -------------------------
species_list <- sort(unique(df2$species))

fits <- list()
sanriku_yearmean <- list()
plots <- list()

for(sp in species_list){
  message("Fitting: ", sp)
  
  df_sp <- df2 %>% filter(species == sp)
  
  if(nrow(df_sp) < 100){
    message("  skipped (too few rows): ", sp)
    next
  }
  
  fit <- fit_xgb_loyo(df_sp, pred_vars, xgb_params,
                      nrounds_max = nrounds_max,
                      early_stopping_rounds = early_stopping_rounds,
                      seed = 1)
  
  df_plot <- build_df_plot(fit, pred_vars)
  df_year <- summarise_year_mean_se(df_plot)
  
  fits[[sp]] <- fit
  sanriku_yearmean[[sp]] <- df_year
  
  # 年ラベル：全てだと混む場合は label_years を指定してください
  plots[[sp]] <- plot_sanriku_yearmean_cross(
    df_year,
    title = paste0(sp),
    expand_x = expand_x, expand_y = expand_y,
    inner_margin_x = inner_margin_x, inner_margin_y = inner_margin_y,
    label_years = NULL,   # 例: c(2003, 2011, 2018, 2023)
    label_size = 3
  )
}

## -------------------------
## 3) example
## -------------------------
print(plots[["テングサ類"]])
print(plots[["フクロフノリ"]])
print(plots[["マツモ"]])
print(plots[["ワカメ"]])
print(plots[["ヒジキ"]])
print(plots[["コンブ類"]])
print(plots[["イガイ類"]])



# 保存 ----------------------------------------------------------------------
## 出力フォルダ
dir.create("sanriku_plot_png", showWarnings = FALSE)

for(sp in names(plots)){
  
  message("Saving: ", sp)
  
  p <- plots[[sp]]
  
  # 空対策
  if(is.null(p)) next
  
  # ファイル名安全化
  sp_safe <- gsub("[^[:alnum:]_一-龥ぁ-んァ-ン]", "_", sp)
  
  ggsave(
    filename = file.path("sanriku_plot_png",
                         paste0("Sanriku_plot_", sp_safe, ".png")),
    plot = p,
    units = "in",
    width = 11.69,   # A4横
    height = 8.27,
    dpi = 300
  )
}



# 発表スライド用 -----------------------------------------------------------------
## =========================================================
## 各種の直近5年平均 ± SE を1枚にまとめる（三陸サマリー）
## =========================================================
jp_font <- "HiraKakuPro-W3"

## -------------------------
## 1) 各種の直近5年平均 ± SE を計算
## -------------------------

df_recent5_all <- map_dfr(names(sanriku_yearmean), function(sp){
  
  df <- sanriku_yearmean[[sp]]
  if(is.null(df) || nrow(df) == 0) return(NULL)
  
  latest_year <- max(df$year, na.rm = TRUE)
  
  df_recent <- df %>%
    filter(year >= latest_year - 4)
  
  if(nrow(df_recent) == 0) return(NULL)
  
  # x軸（資源）
  mean_x <- mean(df_recent$mean2, na.rm = TRUE)
  sd_x   <- sd(df_recent$mean2,  na.rm = TRUE)
  n_x    <- sum(is.finite(df_recent$mean2))
  se_x   <- ifelse(n_x > 1, sd_x / sqrt(n_x), 0)
  
  # y軸（環境）
  mean_y <- mean(df_recent$mean, na.rm = TRUE)
  sd_y   <- sd(df_recent$mean,  na.rm = TRUE)
  n_y    <- sum(is.finite(df_recent$mean))
  se_y   <- ifelse(n_y > 1, sd_y / sqrt(n_y), 0)
  
  tibble(
    species = sp,
    mean_x = mean_x,
    mean_y = mean_y,
    se_x = se_x,
    se_y = se_y
  )
})

## -------------------------
## 2) 軸範囲
## -------------------------

x_rng <- range(df_recent5_all$mean_x, na.rm=TRUE)
y_rng <- range(df_recent5_all$mean_y, na.rm=TRUE)

x_pad <- diff(x_rng) * 0.4
y_pad <- diff(y_rng) * 0.4

if(diff(x_rng) == 0) x_pad <- 0.05
if(diff(y_rng) == 0) y_pad <- 0.05

x_min <- x_rng[1] - x_pad
x_max <- x_rng[2] + x_pad
y_min <- y_rng[1] - y_pad
y_max <- y_rng[2] + y_pad

## -------------------------
## 3) プロット
## -------------------------

fig_summary <- ggplot(df_recent5_all,
                      aes(x = mean_x, y = mean_y)) +
  
  # 背景四象限
  geom_rect(xmin = 0, xmax = x_max, ymin = y_min, ymax = 0,
            fill = "orange", alpha = 0.1) +
  geom_rect(xmin = x_min, xmax = 0, ymin = y_min, ymax = 0,
            fill = "red", alpha = 0.1) +
  geom_rect(xmin = x_min, xmax = 0, ymin = 0, ymax = y_max,
            fill = "yellow", alpha = 0.1) +
  geom_rect(xmin = 0, xmax = x_max, ymin = 0, ymax = y_max,
            fill = "green", alpha = 0.1) +
  
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_vline(xintercept = 0, linewidth = 0.3) +
  
  # # --- 十字バー ---
  # geom_segment(aes(x = mean_x - se_x,
  #                  xend = mean_x + se_x,
  #                  y = mean_y,
  #                  yend = mean_y),
  #              linewidth = 1) +
  # 
  # geom_segment(aes(x = mean_x,
  #                  xend = mean_x,
  #                  y = mean_y - se_y,
  #                  yend = mean_y + se_y),
  #              linewidth = 1) +
  
  # 点
  geom_point(size = 6, color = "black") +
  
  ggrepel::geom_text_repel(
    aes(label = species),
    size = 5,
    family = jp_font,
    segment.color = NA,
    box.padding = 0.4,
    point.padding = 0.3,
    max.overlaps = Inf
  ) +
  
  coord_cartesian(xlim = c(x_min, x_max),
                  ylim = c(y_min, y_max)) +
  
  labs(
    x = "資源水準（0は24年間の平均値）",
    y = "環境からの影響",
    title = ""
  ) +
  
  theme_bw(base_family = "HiraKakuPro-W3") +
  theme(
    plot.title = element_text(size = 18),
    axis.title = element_text(size = 16),
    axis.text  = element_text(size = 14)
  )

print(fig_summary)

## -------------------------
## 4) 保存（スライド用）
## -------------------------

ggsave(
  filename = "Sanriku_recent5_species_summary.png",
  plot = fig_summary,
  width = 13,
  height = 9,
  units = "in",
  dpi = 300
)



# 要因解析 --------------------------------------------------------------------
shap_yearly <- list()

for(sp in names(fits)){
  
  fit <- fits[[sp]]
  
  X_scaled <- as.matrix(fit$df_scaled[, pred_vars, drop=FALSE])
  
  shap_vals <- predict(fit$model, X_scaled, predcontrib = TRUE)
  shap_df <- as.data.frame(shap_vals)
  
  shap_df <- shap_df[, colnames(shap_df) != "BIAS", drop=FALSE]
  colnames(shap_df) <- pred_vars
  
  df_year <- fit$df_raw %>%
    select(year) %>%
    bind_cols(shap_df) %>%
    pivot_longer(-year, names_to="variable", values_to="shap") %>%
    group_by(year, variable) %>%
    summarise(
      importance = mean(abs(shap), na.rm=TRUE),   # 強さ
      direction  = mean(shap,      na.rm=TRUE),  # 方向（＋/−）
      .groups="drop"
    )
  
  shap_yearly[[sp]] <- df_year
}



# =========================================================
# SHAP積み上げグラフ（種ごとに自動作成・保存）
# =========================================================

dir.create("shap_stack_png", showWarnings = FALSE)

for(sp in names(shap_yearly)){
  
  message("Creating stack plot: ", sp)
  
  df_plot <- shap_yearly[[sp]]
  
  if(is.null(df_plot) || nrow(df_plot) == 0) next
  
  p_stack_level <- ggplot(df_plot,
                          aes(x = year,
                              y = direction,
                              fill = variable)) +
    
    geom_col(color = "black", linewidth = 0.3) +
    
    geom_hline(yintercept = 0,
               color = "black",
               linewidth = 0.5) +
    
    scale_fill_manual(
      values = c(
        "icumu_MHW" = "tomato",    # 熱波
        "icumu_MCS" = "skyblue",   # 寒波
        "m_sst"     = "gold"       # 平均水温
      ),
      labels = c(
        "icumu_MHW" = "熱波",
        "icumu_MCS" = "寒波",
        "m_sst"     = "平均水温"
      )
    ) +
    
    scale_x_continuous(breaks = unique(df_plot$year)) +
    
    theme_bw(base_family = "HiraKakuPro-W3") +
    labs(
      y = "環境からの影響（SHAP平均）",
      x = "年",
      fill = "環境要因",
      title = sp
    )
  
  print(p_stack_level)
  
  # ファイル名安全化
  sp_safe <- gsub("[^[:alnum:]_一-龥ぁ-んァ-ン]", "_", sp)
  
  ggsave(
    filename = file.path("shap_stack_png",
                         paste0("SHAP_stack_", sp_safe, ".png")),
    plot = p_stack_level,
    width = 11,
    height = 8,
    units = "in",
    dpi = 300,
    device = ragg::agg_png
  )
}

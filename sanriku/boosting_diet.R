## =========================================================
## Species-wise XGBoost + LOYO CV + SHAP + Sanriku plot  (SAVE FIGURES)
## (i)  SHAP重要度（符号なし：mean|SHAP|）…全種を1枚にまとめて保存
## (ii) SHAP重要度（符号あり：mean SHAP）…全種を1枚にまとめて保存
## (iii) dependence plot …種ごとに保存
## (iv)  sanriku plot …種ごとに保存
##
## - 目的変数: freq
## - 説明変数: m_sst 以降の列をすべて（densなども自動で含む）
## - 日本語フォント: theme_bw(base_family=...)
## - 出力: PNG（日本語の豆腐化回避）
## =========================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(xgboost)
  library(Matrix)
  library(ggrepel)
  library(forcats)
})

## -------------------------
## 0) data & font
## -------------------------
df2 <- sessile2   # <- データ名が違うならここだけ変更

# 日本語フォント（macOS想定。Windowsなら "Yu Gothic" などに変更）
jp_font <- "Hiragino Sans"

## -------------------------
## 1) settings
## -------------------------
start_var <- "m_sst"
stopifnot(start_var %in% names(df2))

pred_vars <- names(df2)[match(start_var, names(df2)):ncol(df2)]
pred_vars <- setdiff(pred_vars, c("freq")) # 念のため

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

# SHAP dependence：各種で何変数まで描くか
depend_top_k <- 6

# 全種まとめ図：上位何変数まで出すか（各種）
global_top_k_per_species <- 10

# 出力フォルダ
out_dir <- "SHAP_Sanriku_png"
dir.create(out_dir, showWarnings = FALSE)

## -------------------------
## 2) helper functions
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
    drop_na(any_of(c("freq","shore","plot","year", pred_vars)))
  
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
  
  list(
    model=model,
    best_nrounds=best_n,
    df_raw=df_sp,
    df_scaled=df_scaled
  )
}

# total SHAP incl interactions: env_score = pred - bias
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

# SHAP main effects (with sign)
compute_shap_contrib <- function(model, X_scaled, pred_vars){
  shp <- predict(model, X_scaled, predcontrib = TRUE)
  shp <- as.data.frame(shp)
  colnames(shp) <- c(pred_vars, "BIAS")
  shp
}

# SHAP summary: mean(|SHAP|) and mean(SHAP)
summarise_shap_signed <- function(shap_df, pred_vars){
  tibble(
    variable   = pred_vars,
    mean_abs   = sapply(pred_vars, \(v) mean(abs(shap_df[[v]]), na.rm=TRUE)),
    mean_shap  = sapply(pred_vars, \(v) mean(shap_df[[v]],      na.rm=TRUE))
  ) %>%
    arrange(desc(mean_abs))
}

# dependence plot: feature value vs SHAP (top_k by mean|SHAP|)
plot_shap_dependence <- function(df_raw, shap_df, sp, pred_vars,
                                 top_k=6, base_family="Hiragino Sans"){
  
  sum_df <- summarise_shap_signed(shap_df, pred_vars)
  top_vars <- sum_df$variable[seq_len(min(top_k, nrow(sum_df)))]
  
  d_long <- purrr::map_dfr(top_vars, function(v){
    tibble(
      species  = sp,
      variable = v,
      x_raw    = df_raw[[v]],
      shap     = shap_df[[v]]
    )
  }) %>% drop_na()
  
  ggplot(d_long, aes(x = x_raw, y = shap)) +
    geom_hline(yintercept = 0, linewidth = 0.35) +
    geom_point(alpha = 0.25, size = 1.2) +
    geom_smooth(method = "loess", se = FALSE, linewidth = 0.8) +
    facet_wrap(~ variable, scales = "free_x") +
    labs(
      x = "共変量（元スケール）",
      y = "SHAP（主効果；+/- が効果方向）",
      title = paste0("SHAP dependence（どの値域で + / - か） - ", sp)
    ) +
    theme_bw(base_family = base_family)
}

# build plot-level df with centered freq + env_score
build_df_plot <- function(fit, pred_vars){
  X_scaled <- xgb.DMatrix(as.matrix(fit$df_scaled[, pred_vars, drop=FALSE]))
  add <- compute_env_score_interactions(fit$model, X_scaled)
  
  fit$df_raw %>%
    select(plot, shore, year, species, freq, all_of(pred_vars)) %>%
    bind_cols(add) %>%
    mutate(
      year = as.integer(year),
      freq_centered = freq - mean(freq, na.rm = TRUE)
    )
}

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
                                        label_size=3,
                                        base_family="Hiragino Sans"){
  
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
    geom_rect(xmin = 0,     xmax = x_max, ymin = y_min, ymax = 0,     fill = "orange", alpha = 0.06) +
    geom_rect(xmin = x_min, xmax = 0,     ymin = y_min, ymax = 0,     fill = "red",    alpha = 0.06) +
    geom_rect(xmin = x_min, xmax = 0,     ymin = 0,     ymax = y_max, fill = "yellow", alpha = 0.06) +
    geom_rect(xmin = 0,     xmax = x_max, ymin = 0,     ymax = y_max, fill = "green",  alpha = 0.06) +
    
    geom_hline(yintercept = 0, linewidth = 0.3) +
    geom_vline(xintercept = 0, linewidth = 0.3) +
    
    geom_segment(aes(x = mean2, xend = mean2, y = ymin_clip, yend = ymax_clip),
                 linewidth = 0.6, color = "black") +
    geom_segment(aes(x = xmin_clip, xend = xmax_clip, y = mean, yend = mean),
                 linewidth = 0.6, color = "black") +
    
    geom_path(alpha = 0.8) +
    geom_point(aes(color = year), size = 3, alpha = 0.95) +
    
    ggrepel::geom_text_repel(
      data = df_lab,
      aes(label = year),
      size = label_size,
      segment.color = NA,
      segment.alpha = 0,
      min.segment.length = Inf,
      box.padding = 0.25,
      point.padding = 0.15,
      max.overlaps = Inf
    ) +
    
    scale_color_viridis_c() +
    coord_cartesian(xlim = c(x_min, x_max), ylim = c(y_min, y_max)) +
    labs(
      x = "資源水準（freq：種平均からの偏差）年平均 ± SE",
      y = "環境軸（SHAP総効果：pred-bias）年平均 ± SE",
      color = "年",
      title = title
    ) +
    theme_bw(base_family = base_family)
}

# ファイル名を安全にする
safe_name <- function(x){
  gsub("[^[:alnum:]_一-龥ぁ-んァ-ン]", "_", x)
}

## -------------------------
## 3) run species-wise (fit + shap summaries + species plots)
## -------------------------
species_list <- sort(unique(df2$species))

fits <- list()
shap_summary_tbl <- list()
shap_depend_plots <- list()
sanriku_plots <- list()

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
  
  fits[[sp]] <- fit
  
  # SHAP
  X_scaled <- xgb.DMatrix(as.matrix(fit$df_scaled[, pred_vars, drop=FALSE]))
  shap_df  <- compute_shap_contrib(fit$model, X_scaled, pred_vars)
  
  sum_df <- summarise_shap_signed(shap_df, pred_vars) %>%
    mutate(species = sp)
  
  shap_summary_tbl[[sp]] <- sum_df
  
  # (iii) dependence plot (species-wise)
  shap_depend_plots[[sp]] <- plot_shap_dependence(
    df_raw   = fit$df_raw,
    shap_df  = shap_df,
    sp       = sp,
    pred_vars= pred_vars,
    top_k    = depend_top_k,
    base_family = jp_font
  )
  
  # (iv) sanriku plot (species-wise)
  df_plot <- build_df_plot(fit, pred_vars)
  df_year <- summarise_year_mean_se(df_plot)
  
  sanriku_plots[[sp]] <- plot_sanriku_yearmean_cross(
    df_year,
    title = paste0("Sanriku plot（年平均 ± SE, SHAP） - ", sp),
    expand_x = expand_x, expand_y = expand_y,
    inner_margin_x = inner_margin_x, inner_margin_y = inner_margin_y,
    label_years = NULL,
    label_size = 3,
    base_family = jp_font
  )
}

# 実際に回った種
species_done <- intersect(names(shap_summary_tbl), names(sanriku_plots))

## -------------------------
## 4) (i)(ii) 全種まとめ図の作成
## -------------------------
shap_all <- bind_rows(shap_summary_tbl) %>%
  filter(species %in% species_done) %>%
  group_by(species) %>%
  mutate(rank = row_number(desc(mean_abs))) %>%
  ungroup() %>%
  filter(rank <= global_top_k_per_species)

# (i) 符号なし：mean|SHAP|
p_imp_unsigned_all <- shap_all %>%
  mutate(variable = fct_reorder(variable, mean_abs)) %>%
  ggplot(aes(x = mean_abs, y = variable)) +
  geom_col() +
  facet_wrap(~ species, scales = "free_y") +
  labs(
    x = "mean(|SHAP|)（主効果；符号なし）",
    y = NULL,
    title = paste0("SHAP重要度（符号なし） top", global_top_k_per_species, " / 種")
  ) +
  theme_bw(base_family = jp_font)

# (ii) 符号あり：mean SHAP（方向）
p_imp_signed_all <- shap_all %>%
  mutate(variable = fct_reorder(variable, mean_abs)) %>%
  ggplot(aes(x = mean_shap, y = variable)) +
  geom_vline(xintercept = 0, linewidth = 0.35) +
  geom_col() +
  facet_wrap(~ species, scales = "free_y") +
  labs(
    x = "mean(SHAP)（主効果；符号あり：+ は freq を押し上げる方向）",
    y = NULL,
    title = paste0("SHAP重要度（符号あり：平均SHAP） top", global_top_k_per_species, " / 種")
  ) +
  theme_bw(base_family = jp_font)

## -------------------------
## 5) SAVE FIGURES
## -------------------------

# (i) 全種まとめ：符号なし
png(file.path(out_dir, "ALL_species_SHAP_importance_unsigned.png"),
    width = 3600, height = 2400, res = 300)
print(p_imp_unsigned_all)
dev.off()

# (ii) 全種まとめ：符号あり
png(file.path(out_dir, "ALL_species_SHAP_importance_signed.png"),
    width = 3600, height = 2400, res = 300)
print(p_imp_signed_all)
dev.off()

# (iii) dependence plot（種ごと）
dir.create(file.path(out_dir, "dependence"), showWarnings = FALSE)
for(sp in species_done){
  sp_safe <- safe_name(sp)
  png(file.path(out_dir, "dependence", paste0(sp_safe, "_SHAP_dependence.png")),
      width = 2400, height = 1800, res = 300)
  print(shap_depend_plots[[sp]])
  dev.off()
}

# (iv) sanriku plot（種ごと）
dir.create(file.path(out_dir, "sanriku"), showWarnings = FALSE)
for(sp in species_done){
  sp_safe <- safe_name(sp)
  png(file.path(out_dir, "sanriku", paste0(sp_safe, "_Sanriku.png")),
      width = 2200, height = 1800, res = 300)
  print(sanriku_plots[[sp]])
  dev.off()
}

message("DONE. Saved to: ", normalizePath(out_dir))
require(tidyverse)
require(xgboost)
require(Matrix)
require(ggrepel)


# directory ---------------------------------------------------------------
dir_iso = "/Users/Yuki/Library/CloudStorage/Dropbox/isodata/doto/distribution/"
setwd(dir_iso)



# data --------------------------------------------------------------------
df2 = read.csv("df_boosting_doto.csv", fileEncoding = "CP932")


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
  
  # ---- 年の新旧判定 ----
  df_year <- df_year %>%
    arrange(year) %>%
    mutate(
      year_rank = rank(year, ties.method = "first"),
      n_year = n(),
      recent5 = year >= max(year) - 4     # 0〜1に正規化
    )
  
  # ---- 軸範囲 ----
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
    
    # 十字バー
    geom_segment(aes(x = mean2, xend = mean2, y = ymin_clip, yend = ymax_clip),
                 linewidth = 0.6, color = "black") +
    geom_segment(aes(x = xmin_clip, xend = xmax_clip, y = mean, yend = mean),
                 linewidth = 0.6, color = "black") +
    
    # 軌跡
    geom_path(alpha = 0.6, color = "grey40") +
    
    # 点（昔→白、最近→黒）
    geom_point(aes(fill = year,
                   shape = recent5),
               size = 3,
               color = "black") +
    
    scale_fill_gradient(low = "white", high = "black") +
    scale_shape_manual(values = c(`FALSE` = 21, `TRUE` = 23)) +guides(shape = "none")+
    
    ggrepel::geom_text_repel(
      data = df_lab,
      aes(label = year),
      size = label_size,
      segment.color = NA,
      max.overlaps = Inf
    ) +
    
    coord_cartesian(xlim = c(x_min, x_max), ylim = c(y_min, y_max)) +
    
    labs(
      x = "資源量（0が平均値）",
      y = "環境の影響",
      title = title,
      fill = "年"
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
print(plots[["フクロフノリ"]])
print(plots[["クロバギンナンソウ"]])

ggsave(
  filename = "sanriku_フクロフノリ.png",
  plot = plots[["フクロフノリ"]],
  width = 6,
  height = 5,
  dpi = 300
)

ggsave(
  filename = "sanriku_クロバギンナンソウ.png",
  plot = plots[["クロバギンナンソウ"]],
  width = 6,
  height = 5,
  dpi = 300
)




# 平均値の図   ------------------------------------------------------------------
## -------------------------
## 日本語フォント設定
## -------------------------
jp_family <- "HiraKakuPro-W3"


## -------------------------
## 種平均データ作成
## -------------------------
df_species <- bind_rows(lapply(names(sanriku_yearmean), function(sp){
  df <- sanriku_yearmean[[sp]]
  
  df %>%
    summarise(
      species = sp,
      mean2 = mean(mean2, na.rm=TRUE),
      sd2   = sd(mean2, na.rm=TRUE),
      n2    = sum(is.finite(mean2)),
      se2   = ifelse(n2 > 1, sd2 / sqrt(n2), 0),
      
      mean  = mean(mean, na.rm=TRUE),
      sd1   = sd(mean, na.rm=TRUE),
      n1    = sum(is.finite(mean)),
      se    = ifelse(n1 > 1, sd1 / sqrt(n1), 0)
    )
}))

## -------------------------
## 対象2種を指定
## -------------------------
target_species <- c("フクロフノリ", "クロバギンナンソウ")

df2sp <- df_species %>%
  filter(species %in% target_species) %>%
  filter(is.finite(mean2), is.finite(mean))

## -------------------------
## 軸範囲計算
## -------------------------
x_rng <- range(df2sp$mean2 + c(-1,1)*df2sp$se2, na.rm=TRUE)
y_rng <- range(df2sp$mean  + c(-1,1)*df2sp$se,  na.rm=TRUE)

x_pad <- diff(x_rng) * 0.3
if(!is.finite(x_pad) || x_pad==0) x_pad <- 0.05

y_pad <- diff(y_rng) * 0.3
if(!is.finite(y_pad) || y_pad==0) y_pad <- 0.05

x_min <- x_rng[1]-x_pad
x_max <- x_rng[2]+x_pad
y_min <- y_rng[1]-y_pad
y_max <- y_rng[2]+y_pad

## -------------------------
## 作図
## -------------------------
p_species_mean <- ggplot(df2sp, aes(x = mean2, y = mean)) +
  
  # 背景四象限
  geom_rect(xmin = 0,     xmax = x_max, ymin = y_min, ymax = 0,     fill = "orange", alpha = 0.08) +
  geom_rect(xmin = x_min, xmax = 0,     ymin = y_min, ymax = 0,     fill = "red",    alpha = 0.08) +
  geom_rect(xmin = x_min, xmax = 0,     ymin = 0,     ymax = y_max, fill = "yellow", alpha = 0.08) +
  geom_rect(xmin = 0,     xmax = x_max, ymin = 0,     ymax = y_max, fill = "green",  alpha = 0.08) +
  
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_vline(xintercept = 0, linewidth = 0.3) +
  
  # 十字バー
  geom_segment(aes(x = mean2, xend = mean2,
                   y = mean - se, yend = mean + se),
               linewidth = 0.8, color = "black") +
  
  geom_segment(aes(x = mean2 - se2, xend = mean2 + se2,
                   y = mean, yend = mean),
               linewidth = 0.8, color = "black") +
  
  # 点
  geom_point(size = 4, color = "black") +
  
  # 種名ラベル
  ggrepel::geom_text_repel(
    aes(label = species),
    family = jp_family,
    size = 4,
    segment.color = NA,
    max.overlaps = Inf,
    na.rm = TRUE
  ) +
  
  coord_cartesian(xlim = c(x_min, x_max), ylim = c(y_min, y_max)) +
  
  labs(
    x = "資源量（0が平均値）",
    y = "環境の影響",
    title = "Sanriku plot（種平均 ± SE）"
  ) +
  
  theme_bw(base_family = jp_family)

## 表示
print(p_species_mean)


## -------------------------
## 保存（豆腐になりにくい）
## -------------------------
ggsave(
  filename = "sanriku_species_mean_2sp.png",
  plot = p_species_mean,
  width = 7,
  height = 5,
  dpi = 300,
  device = ragg::agg_png
)




base_family = "HiraKakuPro-W3"

# -------------------------------------------------------------------------
shap_importance_plots <- list()
shap_dependence_plots <- list()

for(sp in names(fits)){
  
  fit <- fits[[sp]]
  
  # scaled predictors
  X_scaled <- as.matrix(fit$df_scaled[, pred_vars, drop=FALSE])
  
  # SHAP values
  shap_vals <- predict(fit$model, X_scaled, predcontrib = TRUE)
  shap_df <- as.data.frame(shap_vals)
  
  # remove BIAS column
  shap_df <- shap_df[, colnames(shap_df) != "BIAS", drop=FALSE]
  colnames(shap_df) <- pred_vars
  
  # -------------------------
  # (i) importance plot
  # -------------------------
  
  imp <- data.frame(
    variable = pred_vars,
    mean_abs_shap = sapply(pred_vars, function(v)
      mean(abs(shap_df[[v]]), na.rm=TRUE)),
    mean_shap = sapply(pred_vars, function(v)
      mean(shap_df[[v]], na.rm=TRUE))
  )
  
  p_imp <- ggplot(imp, aes(x=reorder(variable, mean_abs_shap),
                           y=mean_abs_shap,
                           fill=mean_shap)) +
    geom_col() +
    coord_flip() +
    scale_fill_gradient2(low="gray60", mid="gray60", high="gray60") +
    labs(title=paste("SHAP importance -", sp),
         x="Variable",
         y="mean(|SHAP|)",
         fill="mean SHAP") +
    theme_bw(base_family = "HiraKakuPro-W3")
  
  shap_importance_plots[[sp]] <- p_imp
  
  # -------------------------
  # (ii) dependence plot（1枚にまとめる：修正版）
  # -------------------------
  
  df_list <- list()
  
  for(v in pred_vars){
    df_list[[v]] <- data.frame(
      variable = v,
      x = fit$df_raw[[v]],
      shap = shap_df[[v]]
    )
  }
  
  df_dep <- bind_rows(df_list)
  
  p_dep <- ggplot(df_dep, aes(x=x, y=shap)) +
    geom_point(alpha=0.4) +
    geom_smooth(method="loess", se=FALSE, color="red") +
    facet_wrap(~variable, scales="free_x") +
    labs(title=paste("SHAP dependence -", sp),
         x="Predictor value",
         y="SHAP value") +
    theme_bw(base_family = "HiraKakuPro-W3")
  
  shap_dependence_plots[[sp]] <- p_dep
}



# 作図 ----------------------------------------------------------------------
print(shap_importance_plots[["フクロフノリ"]])
print(shap_importance_plots[["クロバギンナンソウ"]])
print(shap_dependence_plots[["フクロフノリ"]])
print(shap_dependence_plots[["クロバギンナンソウ"]])



# 年ごとのshap ----------------------------------------------------------------
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
      importance = mean(abs(shap), na.rm=TRUE),
      .groups="drop"
    )
  
  shap_yearly[[sp]] <- df_year
}

fig = ggplot(shap_yearly[["フクロフノリ"]],
       aes(x=year, y=importance, color=variable)) +
  geom_line() +
  geom_point() +
  theme_bw(base_family="HiraKakuPro-W3") +
  labs(y="重要度",
       x = "Year",
       title="")
ggsave(filename = "trend_shap_funo.png", plot = fig, units = "in", width = 11.69, height = 8.27)

fig = ggplot(shap_yearly[["クロバギンナンソウ"]],
       aes(x=year, y=importance, color=variable)) +
  geom_line() +
  geom_point() +
  theme_bw(base_family="HiraKakuPro-W3") +
  labs(y="重要度",
       x = "Year",
       title="")
ggsave(filename = "trend_shap_kuro.png", plot = fig, units = "in", width = 11.69, height = 8.27)

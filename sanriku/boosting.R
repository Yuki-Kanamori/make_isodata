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
                                        label_years=NULL,  # NULLなら全ラベル、例: c(2003,2011,2023)
                                        label_size=3){
  
  # axis range based on annual means
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
  
  # inner clip bounds (avoid "touching the edge" look)
  x_in_pad <- (x_max - x_min) * inner_margin_x
  y_in_pad <- (y_max - y_min) * inner_margin_y
  
  x_min_in <- x_min + x_in_pad
  x_max_in <- x_max - x_in_pad
  y_min_in <- y_min + y_in_pad
  y_max_in <- y_max - y_in_pad
  
  # clip cross-bars inside inner bounds
  df_plot <- df_year %>%
    mutate(
      xmin_clip = pmax(mean2 - se2, x_min_in),
      xmax_clip = pmin(mean2 + se2, x_max_in),
      ymin_clip = pmax(mean  - se,  y_min_in),
      ymax_clip = pmin(mean  + se,  y_max_in)
    )
  
  # label subset
  df_lab <- if(is.null(label_years)) df_plot else df_plot %>% filter(year %in% label_years)
  
  ggplot(df_plot, aes(x = mean2, y = mean)) +
    
    # background quadrants (wide area)
    geom_rect(xmin = 0,     xmax = x_max, ymin = y_min, ymax = 0,     fill = "orange", alpha = 0.06) +
    geom_rect(xmin = x_min, xmax = 0,     ymin = y_min, ymax = 0,     fill = "red",    alpha = 0.06) +
    geom_rect(xmin = x_min, xmax = 0,     ymin = 0,     ymax = y_max, fill = "yellow", alpha = 0.06) +
    geom_rect(xmin = 0,     xmax = x_max, ymin = 0,     ymax = y_max, fill = "green",  alpha = 0.06) +
    
    geom_hline(yintercept = 0, linewidth = 0.3) +
    geom_vline(xintercept = 0, linewidth = 0.3) +
    
    # cross bars (always drawable; SE=0 -> zero-length segment)
    geom_segment(aes(x = mean2, xend = mean2, y = ymin_clip, yend = ymax_clip),
                 linewidth = 0.6, color = "black") +
    geom_segment(aes(x = xmin_clip, xend = xmax_clip, y = mean, yend = mean),
                 linewidth = 0.6, color = "black") +
    
    # trajectory + points
    geom_path(alpha = 0.8) +
    geom_point(aes(color = year), size = 3, alpha = 0.95) +
    
    # year labels WITHOUT leader lines
    ggrepel::geom_text_repel(
      data = df_lab,
      aes(label = year),
      size = label_size,
      segment.color = NA,      # ★ 線を消す
      segment.alpha = 0,       # 念のため
      min.segment.length = Inf,
      box.padding = 0.25,
      point.padding = 0.15,
      max.overlaps = Inf
    ) +
    
    scale_color_viridis_c() +
    coord_cartesian(xlim = c(x_min, x_max), ylim = c(y_min, y_max)) +
    
    labs(
      x = "Resource level (freq centered at species mean): annual mean ± SE",
      y = "Environmental axis (total SHAP incl. interactions): annual mean ± SE",
      color = "Year",
      title = title
    ) +
    theme_bw()
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
    title = paste0("Sanriku plot (annual mean ± SE, SHAP) - ", sp),
    expand_x = expand_x, expand_y = expand_y,
    inner_margin_x = inner_margin_x, inner_margin_y = inner_margin_y,
    label_years = NULL,   # 例: c(2003, 2011, 2018, 2023)
    label_size = 3
  )
}

## -------------------------
## 3) example
## -------------------------
# print(plots[["テングサ類"]])
print(plots[["テングサ類"]])
print(plots[["フクロフノリ"]])
print(plots[["マツモ"]])
print(plots[["ワカメ"]])
print(plots[["ヒジキ"]])
print(plots[["コンブ類"]])
print(plots[["イガイ類"]])

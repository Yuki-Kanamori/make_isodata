
# ============================================================
# Multivariate S-map：プロット別推定可能性・安定性診断
#
# 入力：
#   relative_coverage_timeseries.csv
#
# 5海岸、23プロット、Pre/Post = 46システム
# 各システム：9年間 × 5種
#
# モデル：
#   X(t+1) = f[X1(t), ..., X5(t)]
#
# 各応答種について局所線形回帰を実施
# θ：局所性
# λ：ridge正則化強度
#
# 診断：
#   1. 行列ランク・条件数
#   2. LOOCV予測性能
#   3. 持続予測との比較
#   4. 係数のジャックナイフ安定性
#
# 他プロットのデータは混ぜない
# 差分は取らない
# ============================================================


# ============================================================
# 1. ディレクトリ
# ============================================================

data_dir <- "/Users/Yuki/Dropbox/isodata/sanriku/07"

# 添付した時系列CSVの保存場所
input_file <- file.path(
  data_dir,
  "SMap_trend_check",
  "relative_coverage_timeseries.csv"
)

# 前回の出力が日時別フォルダにある場合は、
# ここを実際のCSVパスに変更してください。
if (!file.exists(input_file)) {
  
  candidates <- list.files(
    file.path(data_dir, "SMap_trend_check"),
    pattern = "^relative_coverage_timeseries\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  )
  
  if (length(candidates) == 0) {
    stop("relative_coverage_timeseries.csv が見つかりません。")
  }
  
  input_file <- candidates[
    which.max(file.info(candidates)$mtime)
  ]
}

out_dir <- file.path(
  data_dir,
  "SMap_plot_stability",
  paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S"))
)

dir.create(
  out_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# ============================================================
# 2. パッケージ
# ============================================================

packages <- c(
  "dplyr", "tidyr", "ggplot2",
  "readr", "showtext", "sysfonts"
)

missing <- packages[
  !vapply(
    packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing) > 0) {
  install.packages(missing)
}

library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(showtext)
library(sysfonts)


# ============================================================
# 3. 日本語フォント
# ============================================================

font_candidates <- c(
  "/System/Library/Fonts/ヒラギノ角ゴシック W3.ttc",
  "/System/Library/Fonts/ヒラギノ角ゴシック W4.ttc",
  "/System/Library/Fonts/ヒラギノ角ゴシック W5.ttc",
  "/System/Library/Fonts/Supplemental/Arial Unicode.ttf"
)

font_candidates <- font_candidates[
  file.exists(font_candidates)
]

if (length(font_candidates) == 0) {
  stop("日本語フォントが見つかりません。")
}

sysfonts::font_add(
  family = "jp",
  regular = font_candidates[1]
)

showtext::showtext_auto(TRUE)
showtext::showtext_opts(dpi = 300)


# ============================================================
# 4. 保存関数
# ============================================================

save_csv <- function(dat, filename) {
  
  readr::write_excel_csv(
    dat,
    file.path(out_dir, filename),
    na = ""
  )
}

save_pdf <- function(plots, filename, width, height) {
  
  grDevices::pdf(
    file.path(out_dir, filename),
    width = width,
    height = height,
    useDingbats = FALSE
  )
  
  tryCatch(
    {
      for (p in plots) print(p)
    },
    finally = {
      grDevices::dev.off()
    }
  )
}


# ============================================================
# 5. パラメータ
# ============================================================

shores <- c("MJ", "AG", "OR", "KG", "AK")

theta_grid <- c(
  0, 0.1, 0.5, 1, 2, 4
)

lambda_grid <- c(
  0, 0.001, 0.01, 0.1, 1
)

# 係数の符号安定性を判定する際の数値許容差
coef_tol <- 1e-8


# ============================================================
# 6. データ読み込み
# ============================================================

dat <- readr::read_csv(
  input_file,
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE
)

required_cols <- c(
  "shore", "plot", "period",
  "year", "species", "relative_coverage"
)

if (!all(required_cols %in% names(dat))) {
  stop("CSVの列名が想定と異なります。")
}

dat <- dat %>%
  mutate(
    shore = as.character(shore),
    plot = as.character(plot),
    period = as.character(period),
    species = as.character(species),
    year = as.integer(year),
    relative_coverage = as.numeric(relative_coverage)
  ) %>%
  filter(
    shore %in% shores,
    period %in% c("Pre", "Post")
  )

if (anyNA(dat[required_cols])) {
  stop("入力CSVに欠損値があります。")
}

if (anyDuplicated(
  dat[c("shore", "plot", "period", "year", "species")]
)) {
  stop("同じプロット・期間・年・種の重複行があります。")
}


# ============================================================
# 7. S-map用の局所加重ridge回帰
#
# 距離：標準化された5種の状態空間で計算
#
# 重み：
#   w_i = exp(-θ d_i / mean(d))
#
# λ=0：通常の局所線形回帰
# λ>0：ridge回帰
#
# 切片は正則化しない
# ============================================================

smap_fit <- function(
    X_lib,
    y_lib,
    x_query,
    theta,
    lambda
) {
  
  n <- nrow(X_lib)
  p <- ncol(X_lib)
  
  if (n < 2) {
    return(NULL)
  }
  
  distances <- sqrt(
    rowSums(
      sweep(
        X_lib,
        2,
        x_query,
        "-"
      )^2
    )
  )
  
  mean_distance <- mean(distances)
  
  if (
    theta == 0 ||
    mean_distance < 1e-12
  ) {
    
    weights <- rep(1, n)
    
  } else {
    
    weights <- exp(
      -theta * distances / mean_distance
    )
  }
  
  # 重みの和を1に正規化
  weights <- weights / sum(weights)
  
  design <- cbind(
    intercept = 1,
    X_lib
  )
  
  query_design <- c(1, x_query)
  
  weighted_design <- design * sqrt(weights)
  
  weighted_response <- y_lib * sqrt(weights)
  
  gram <- crossprod(weighted_design)
  
  rhs <- crossprod(
    weighted_design,
    weighted_response
  )
  
  penalty <- diag(
    c(0, rep(lambda, p))
  )
  
  A <- gram + penalty
  
  beta <- tryCatch(
    as.vector(solve(A, rhs)),
    error = function(e) NULL
  )
  
  if (is.null(beta)) {
    return(NULL)
  }
  
  prediction <- sum(
    query_design * beta
  )
  
  # Ridgeの有効自由度
  effective_df <- tryCatch(
    sum(diag(solve(A, gram))),
    error = function(e) NA_real_
  )
  
  list(
    prediction = prediction,
    coefficients = beta,
    effective_df = effective_df,
    n_effective = 1 / sum(weights^2)
  )
}


# ============================================================
# 8. 交差検証
#
# 8組の遷移のうち1組を予測用に除外
# 残り7組から推定
#
# 他プロットのデータは使用しない
# ============================================================

run_cv <- function(
    X,
    y,
    theta,
    lambda
) {
  
  n <- nrow(X)
  
  predictions <- rep(NA_real_, n)
  
  for (i in seq_len(n)) {
    
    train <- setdiff(seq_len(n), i)
    
    fit <- smap_fit(
      X_lib = X[train, , drop = FALSE],
      y_lib = y[train],
      x_query = X[i, ],
      theta = theta,
      lambda = lambda
    )
    
    if (!is.null(fit)) {
      predictions[i] <- fit$prediction
    }
  }
  
  if (anyNA(predictions)) {
    
    return(
      tibble(
        rmse = NA_real_,
        rho = NA_real_,
        n_predicted = sum(!is.na(predictions))
      )
    )
  }
  
  rmse <- sqrt(
    mean((y - predictions)^2)
  )
  
  rho <- if (
    sd(y) > 0 &&
    sd(predictions) > 0
  ) {
    cor(y, predictions)
  } else {
    NA_real_
  }
  
  tibble(
    rmse = rmse,
    rho = rho,
    n_predicted = n
  )
}


# ============================================================
# 9. 係数の安定性評価
#
# 全8組で係数を推定
# さらに1組ずつ除外して再推定
#
# 各状態における局所係数について、
# 除外前後の符号・変動幅を比較
# ============================================================

coefficient_stability <- function(
    X,
    y,
    theta,
    lambda,
    species_names,
    years
) {
  
  n <- nrow(X)
  p <- ncol(X)
  
  full_coefs <- matrix(
    NA_real_,
    nrow = n,
    ncol = p + 1
  )
  
  jack_coefs <- array(
    NA_real_,
    dim = c(n, n, p + 1)
  )
  
  eff_df <- rep(NA_real_, n)
  eff_n <- rep(NA_real_, n)
  
  for (i in seq_len(n)) {
    
    full_fit <- smap_fit(
      X,
      y,
      X[i, ],
      theta,
      lambda
    )
    
    if (!is.null(full_fit)) {
      
      full_coefs[i, ] <- full_fit$coefficients
      eff_df[i] <- full_fit$effective_df
      eff_n[i] <- full_fit$n_effective
    }
    
    for (j in seq_len(n)) {
      
      train <- setdiff(seq_len(n), j)
      
      jack_fit <- smap_fit(
        X[train, , drop = FALSE],
        y[train],
        X[i, ],
        theta,
        lambda
      )
      
      if (!is.null(jack_fit)) {
        jack_coefs[i, j, ] <- jack_fit$coefficients
      }
    }
  }
  
  names_coef <- c(
    "(Intercept)",
    species_names
  )
  
  details <- list()
  
  k <- 1
  
  for (i in seq_len(n)) {
    
    for (v in seq_len(p + 1)) {
      
      full_value <- full_coefs[i, v]
      
      jack_values <- jack_coefs[i, , v]
      jack_values <- jack_values[
        is.finite(jack_values)
      ]
      
      sign_agreement <- if (
        length(jack_values) > 0 &&
        is.finite(full_value) &&
        abs(full_value) > coef_tol
      ) {
        
        mean(
          sign(jack_values[
            abs(jack_values) > coef_tol
          ]) == sign(full_value)
        )
        
      } else {
        NA_real_
      }
      
      if (is.nan(sign_agreement)) {
        sign_agreement <- NA_real_
      }
      
      details[[k]] <- tibble(
        year = years[i],
        predictor = names_coef[v],
        coefficient = full_value,
        jack_mean = if (length(jack_values) > 0) {
          mean(jack_values)
        } else {
          NA_real_
        },
        jack_sd = if (length(jack_values) > 1) {
          sd(jack_values)
        } else {
          NA_real_
        },
        jack_min = if (length(jack_values) > 0) {
          min(jack_values)
        } else {
          NA_real_
        },
        jack_max = if (length(jack_values) > 0) {
          max(jack_values)
        } else {
          NA_real_
        },
        sign_agreement = sign_agreement,
        effective_df = eff_df[i],
        effective_n = eff_n[i]
      )
      
      k <- k + 1
    }
  }
  
  bind_rows(details)
}


# ============================================================
# 10. 解析用の標準化
#
# プロット×期間ごとに5種を標準化
#
# 定数系列はSD=1として扱う
# （標準化後はすべて0）
#
# 応答も種ごとに標準化する。
# 係数は標準化尺度で出力する。
# ============================================================

standardize_matrix <- function(M) {
  
  center <- colMeans(M)
  
  scale_value <- apply(M, 2, sd)
  
  constant <- (
    !is.finite(scale_value) |
      scale_value < 1e-12
  )
  
  scale_value[constant] <- 1
  
  Z <- sweep(M, 2, center, "-")
  Z <- sweep(Z, 2, scale_value, "/")
  
  list(
    Z = Z,
    center = center,
    scale = scale_value,
    constant = constant
  )
}


# ============================================================
# 11. メイン解析
# ============================================================

systems <- dat %>%
  distinct(shore, plot, period) %>%
  arrange(shore, plot, period)

all_cv <- list()
all_selected <- list()
all_coefs <- list()
all_diagnostics <- list()

counter <- 1


for (s in seq_len(nrow(systems))) {
  
  shore_id <- systems$shore[s]
  plot_id <- systems$plot[s]
  period_id <- systems$period[s]
  
  message(
    "解析中：",
    shore_id, " / ",
    plot_id, " / ",
    period_id
  )
  
  subset_dat <- dat %>%
    filter(
      shore == shore_id,
      plot == plot_id,
      period == period_id
    )
  
  wide <- subset_dat %>%
    select(year, species, relative_coverage) %>%
    pivot_wider(
      names_from = species,
      values_from = relative_coverage
    ) %>%
    arrange(year)
  
  years <- wide$year
  
  species_names <- setdiff(
    names(wide),
    "year"
  )
  
  if (
    length(years) != 9 ||
    length(species_names) != 5
  ) {
    stop(
      "9年×5種ではありません：",
      plot_id, " ", period_id
    )
  }
  
  if (any(diff(years) != 1)) {
    stop(
      "年が連続していません：",
      plot_id, " ", period_id
    )
  }
  
  M <- as.matrix(
    wide[, species_names]
  )
  
  storage.mode(M) <- "double"
  
  if (anyNA(M)) {
    stop(
      "欠損値があります：",
      plot_id, " ", period_id
    )
  }
  
  standardized <- standardize_matrix(M)
  
  Z <- standardized$Z
  
  # t年の5種の状態
  X <- Z[1:8, , drop = FALSE]
  
  # t+1年の5種の状態
  Y <- Z[2:9, , drop = FALSE]
  
  prediction_years <- years[1:8]
  
  # 行列ランク・条件数
  design <- cbind(1, X)
  
  matrix_rank <- qr(design)$rank
  
  matrix_condition <- tryCatch(
    kappa(design, exact = TRUE),
    error = function(e) Inf
  )
  
  all_diagnostics[[counter]] <- tibble(
    shore = shore_id,
    plot = plot_id,
    period = period_id,
    n_years = nrow(M),
    n_transitions = nrow(X),
    n_species = ncol(X),
    n_parameters = ncol(design),
    matrix_rank = matrix_rank,
    full_rank = matrix_rank == ncol(design),
    condition_number = matrix_condition,
    n_constant_species = sum(
      standardized$constant
    )
  )
  
  # 応答種ごとにS-map
  for (target_id in seq_along(species_names)) {
    
    target_name <- species_names[target_id]
    
    y <- Y[, target_id]
    
    # 持続予測：
    # 翌年の被度 = 当年の被度
    persistence <- X[, target_id]
    
    persistence_rmse <- sqrt(
      mean((y - persistence)^2)
    )
    
    cv_results <- list()
    z <- 1
    
    for (theta in theta_grid) {
      
      for (lambda in lambda_grid) {
        
        cv <- run_cv(
          X,
          y,
          theta,
          lambda
        )
        
        cv_results[[z]] <- tibble(
          shore = shore_id,
          plot = plot_id,
          period = period_id,
          target = target_name,
          theta = theta,
          lambda = lambda,
          rmse = cv$rmse,
          rho = cv$rho,
          n_predicted = cv$n_predicted
        )
        
        z <- z + 1
      }
    }
    
    cv_table <- bind_rows(cv_results)
    
    all_cv[[length(all_cv) + 1]] <- cv_table
    
    valid_cv <- cv_table %>%
      filter(
        is.finite(rmse),
        n_predicted == 8
      ) %>%
      arrange(
        rmse,
        lambda,
        theta
      )
    
    if (nrow(valid_cv) == 0) {
      
      all_selected[[length(all_selected) + 1]] <- tibble(
        shore = shore_id,
        plot = plot_id,
        period = period_id,
        target = target_name,
        theta = NA_real_,
        lambda = NA_real_,
        cv_rmse = NA_real_,
        cv_rho = NA_real_,
        persistence_rmse = persistence_rmse,
        rmse_ratio = NA_real_
      )
      
      next
    }
    
    best <- valid_cv[1, ]
    
    all_selected[[length(all_selected) + 1]] <- tibble(
      shore = shore_id,
      plot = plot_id,
      period = period_id,
      target = target_name,
      theta = best$theta,
      lambda = best$lambda,
      cv_rmse = best$rmse,
      cv_rho = best$rho,
      persistence_rmse = persistence_rmse,
      rmse_ratio = if (persistence_rmse > 0) {
        best$rmse / persistence_rmse
      } else {
        NA_real_
      }
    )
    
    coef_table <- coefficient_stability(
      X = X,
      y = y,
      theta = best$theta,
      lambda = best$lambda,
      species_names = species_names,
      years = prediction_years
    )
    
    coef_table <- coef_table %>%
      mutate(
        shore = shore_id,
        plot = plot_id,
        period = period_id,
        target = target_name,
        theta = best$theta,
        lambda = best$lambda,
        .before = 1
      )
    
    all_coefs[[length(all_coefs) + 1]] <- coef_table
  }
  
  counter <- counter + 1
}


# ============================================================
# 12. 結果を統合
# ============================================================

cv_results <- bind_rows(all_cv)

selected_models <- bind_rows(all_selected)

coefficient_details <- bind_rows(all_coefs)

system_diagnostics <- bind_rows(all_diagnostics)


# ============================================================
# 13. 係数安定性の要約
#
# 自己効果と種間効果を区別
# 切片は集計から除外
# ============================================================

coefficient_summary <- coefficient_details %>%
  filter(
    predictor != "(Intercept)"
  ) %>%
  mutate(
    interaction_type = if_else(
      predictor == target,
      "Self",
      "Cross"
    )
  ) %>%
  group_by(
    shore, plot, period,
    target, predictor,
    interaction_type,
    theta, lambda
  ) %>%
  summarise(
    mean_coefficient = mean(
      coefficient,
      na.rm = TRUE
    ),
    sd_coefficient = sd(
      coefficient,
      na.rm = TRUE
    ),
    mean_jack_sd = mean(
      jack_sd,
      na.rm = TRUE
    ),
    mean_sign_agreement = if (
      all(is.na(sign_agreement))
    ) {
      NA_real_
    } else {
      mean(
        sign_agreement,
        na.rm = TRUE
      )
    },
    mean_effective_df = mean(
      effective_df,
      na.rm = TRUE
    ),
    mean_effective_n = mean(
      effective_n,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# ============================================================
# 14. 応答種ごとの要約
# ============================================================

target_stability <- coefficient_summary %>%
  group_by(
    shore, plot, period, target
  ) %>%
  summarise(
    mean_sign_agreement = if (
      all(is.na(mean_sign_agreement))
    ) {
      NA_real_
    } else {
      mean(
        mean_sign_agreement,
        na.rm = TRUE
      )
    },
    mean_jack_sd = mean(
      mean_jack_sd,
      na.rm = TRUE
    ),
    mean_effective_df = mean(
      mean_effective_df,
      na.rm = TRUE
    ),
    mean_effective_n = mean(
      mean_effective_n,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  left_join(
    selected_models,
    by = c(
      "shore", "plot",
      "period", "target"
    )
  ) %>%
  left_join(
    system_diagnostics,
    by = c(
      "shore", "plot", "period"
    )
  )


# ============================================================
# 15. CSV出力
# ============================================================

save_csv(
  system_diagnostics,
  "system_diagnostics.csv"
)

save_csv(
  cv_results,
  "theta_lambda_cv.csv"
)

save_csv(
  selected_models,
  "selected_models.csv"
)

save_csv(
  coefficient_details,
  "coefficient_details.csv"
)

save_csv(
  coefficient_summary,
  "coefficient_stability.csv"
)

save_csv(
  target_stability,
  "target_stability.csv"
)


# ============================================================
# 16. 作図設定
# ============================================================

plot_theme <- theme_bw(
  base_size = 11,
  base_family = "jp"
) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

period_colors <- c(
  Pre = "#2876B9",
  Post = "#D55E00"
)


# ============================================================
# 17. S-mapと持続予測の比較
#
# RMSE比 < 1：S-mapの誤差が小さい
# RMSE比 > 1：持続予測の誤差が小さい
# ============================================================

p_rmse <- ggplot(
  target_stability,
  aes(
    x = plot,
    y = rmse_ratio,
    color = period
  )
) +
  geom_hline(
    yintercept = 1,
    linetype = "dashed",
    color = "grey50"
  ) +
  geom_point(
    position = position_dodge(width = 0.5),
    size = 2
  ) +
  facet_wrap(
    ~ shore,
    scales = "free_x",
    ncol = 2
  ) +
  scale_color_manual(
    values = period_colors
  ) +
  labs(
    title = "S-mapと持続予測の予測誤差比較",
    subtitle = "RMSE比が1未満ならS-mapの誤差が小さい",
    x = "プロット",
    y = "S-map RMSE / 持続予測RMSE",
    color = "期間"
  ) +
  plot_theme +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

save_pdf(
  list(p_rmse),
  "prediction_performance.pdf",
  width = 12,
  height = 9
)


# ============================================================
# 18. θ・λの選択結果
# ============================================================

p_theta <- ggplot(
  selected_models,
  aes(
    x = plot,
    y = theta,
    color = period
  )
) +
  geom_point(
    position = position_dodge(width = 0.5),
    size = 2
  ) +
  facet_wrap(
    ~ shore,
    scales = "free_x"
  ) +
  scale_color_manual(
    values = period_colors
  ) +
  labs(
    title = "選択されたθ",
    x = "プロット",
    y = "θ",
    color = "期間"
  ) +
  plot_theme +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )


p_lambda <- ggplot(
  selected_models,
  aes(
    x = plot,
    y = factor(lambda),
    color = period
  )
) +
  geom_point(
    position = position_dodge(width = 0.5),
    size = 2
  ) +
  facet_wrap(
    ~ shore,
    scales = "free_x"
  ) +
  scale_color_manual(
    values = period_colors
  ) +
  labs(
    title = "選択されたridge正則化強度",
    x = "プロット",
    y = "λ",
    color = "期間"
  ) +
  plot_theme +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

save_pdf(
  list(p_theta, p_lambda),
  "selected_parameters.pdf",
  width = 12,
  height = 9
)


# ============================================================
# 19. 係数の符号安定性
#
# 1：除外前後で符号が常に一致
# 0：一致しない
#
# ただし符号が安定しても、
# 推定値が正しいとは限らない
# ============================================================

p_stability <- ggplot(
  coefficient_summary,
  aes(
    x = plot,
    y = mean_sign_agreement,
    color = period
  )
) +
  geom_point(
    position = position_jitterdodge(
      jitter.width = 0.12,
      dodge.width = 0.6
    ),
    alpha = 0.7,
    size = 1.5
  ) +
  facet_wrap(
    ~ shore,
    scales = "free_x"
  ) +
  scale_color_manual(
    values = period_colors
  ) +
  coord_cartesian(
    ylim = c(0, 1)
  ) +
  labs(
    title = "局所係数の符号安定性",
    subtitle = "1時点除外によるジャックナイフ診断",
    x = "プロット",
    y = "符号一致率",
    color = "期間"
  ) +
  plot_theme +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

save_pdf(
  list(p_stability),
  "coefficient_sign_stability.pdf",
  width = 12,
  height = 9
)


# ============================================================
# 20. 行列の条件数
# ============================================================

p_condition <- ggplot(
  system_diagnostics,
  aes(
    x = plot,
    y = condition_number,
    color = period
  )
) +
  geom_point(
    position = position_dodge(width = 0.5),
    size = 2
  ) +
  scale_y_log10() +
  facet_wrap(
    ~ shore,
    scales = "free_x"
  ) +
  scale_color_manual(
    values = period_colors
  ) +
  labs(
    title = "説明変数行列の条件数",
    subtitle = "値が大きいほど係数推定が不安定になりやすい",
    x = "プロット",
    y = "条件数（対数目盛）",
    color = "期間"
  ) +
  plot_theme +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

save_pdf(
  list(p_condition),
  "matrix_condition.pdf",
  width = 12,
  height = 9
)


# ============================================================
# 21. 診断結果の集計
# ============================================================

diagnostic_summary <- target_stability %>%
  group_by(
    shore,
    period
  ) %>%
  summarise(
    n_models = n(),
    n_estimable = sum(
      is.finite(cv_rmse)
    ),
    n_better_than_persistence = sum(
      rmse_ratio < 1,
      na.rm = TRUE
    ),
    median_rmse_ratio = median(
      rmse_ratio,
      na.rm = TRUE
    ),
    median_sign_agreement = median(
      mean_sign_agreement,
      na.rm = TRUE
    ),
    median_effective_df = median(
      mean_effective_df,
      na.rm = TRUE
    ),
    n_ridge_selected = sum(
      lambda > 0,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

save_csv(
  diagnostic_summary,
  "diagnostic_summary.csv"
)

print(diagnostic_summary)


# ============================================================
# 22. 完了
# ============================================================

cat("\n解析完了\n")
cat("入力：", input_file, "\n")
cat("出力：", out_dir, "\n")

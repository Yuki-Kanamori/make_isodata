
# ============================================================
# 相対被度のトレンド確認
# 5海岸 × 各プロット × 優占5種 × Pre/Post
#
# Pre : 2002–2010
# Post: 2018–2026
#
# 線形回帰 + Mann-Kendall検定
# 差分なし
# ============================================================


# ============================================================
# 1. ディレクトリ
# ============================================================

data_dir <- "/Users/Yuki/Dropbox/isodata/sanriku/07"

out_dir <- file.path(
  data_dir,
  "SMap_trend_check",
  paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S"))
)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)


# ============================================================
# 2. パッケージ
# ============================================================

packages <- c(
  "dplyr", "tidyr", "ggplot2",
  "showtext", "sysfonts", "Kendall", "readr"
)

missing <- packages[
  !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing) > 0) {
  install.packages(missing)
}

library(dplyr)
library(tidyr)
library(ggplot2)
library(showtext)
library(sysfonts)
library(Kendall)
library(readr)


# ============================================================
# 3. 日本語フォント・保存関数
# ============================================================

font_candidates <- c(
  "/System/Library/Fonts/ヒラギノ角ゴシック W3.ttc",
  "/System/Library/Fonts/ヒラギノ角ゴシック W4.ttc",
  "/System/Library/Fonts/ヒラギノ角ゴシック W5.ttc",
  "/System/Library/Fonts/Supplemental/Arial Unicode.ttf"
)

font_candidates <- font_candidates[file.exists(font_candidates)]

if (length(font_candidates) == 0) {
  stop("日本語フォントが見つかりません。")
}

sysfonts::font_add(
  family = "jp",
  regular = font_candidates[1]
)

showtext::showtext_auto(TRUE)
showtext::showtext_opts(dpi = 300)


save_csv <- function(dat, filename) {
  readr::write_excel_csv(
    dat,
    file.path(out_dir, filename),
    na = ""
  )
}


save_pdf <- function(plot_list, filename, width, height) {
  
  grDevices::pdf(
    file.path(out_dir, filename),
    width = width,
    height = height,
    useDingbats = FALSE
  )
  
  tryCatch(
    {
      for (p in plot_list) print(p)
    },
    finally = grDevices::dev.off()
  )
}


# ============================================================
# 4. 調査地
# ============================================================

shores <- c("MJ", "AG", "OR", "KG", "AK")

shore_plots <- list(
  MJ = paste0("MJ", 1:5),
  AG = paste0("AG", 1:5),
  OR = paste0("OR", 1:5),
  KG = paste0("KG", 1:5),
  AK = paste0("AK", 1:3)
)

all_plots <- unlist(shore_plots, use.names = FALSE)


# ============================================================
# 5. データ読み込み
# ============================================================

raw_c <- read.csv(
  file.path(data_dir, "sanriku07C.csv"),
  fileEncoding = "CP932",
  stringsAsFactors = FALSE
)

raw_ext <- read.csv(
  file.path(data_dir, "sanriku07C_Ext.csv"),
  fileEncoding = "CP932",
  stringsAsFactors = FALSE
)


clean_records <- function(dat) {
  
  dat %>%
    mutate(
      across(
        all_of(c("plot", "row", "column", "species")),
        ~ trimws(as.character(.x))
      ),
      year = as.integer(.data$year),
      row = tolower(.data$row)
    ) %>%
    filter(
      .data$plot %in% all_plots,
      !is.na(.data$year),
      !is.na(.data$species),
      .data$species != "",
      tolower(.data$species) != "nodata"
    )
}

c_clean <- clean_records(raw_c)
ext_clean <- clean_records(raw_ext)


# ============================================================
# 6. Pre/Post抽出
# ============================================================

pre_data <- c_clean %>%
  filter(between(.data$year, 2002, 2010)) %>%
  mutate(period = "Pre")

post_data <- bind_rows(
  c_clean %>%
    filter(
      between(.data$year, 2018, 2026),
      .data$row %in% sprintf("r%02d", 1:10)
    ),
  ext_clean %>%
    filter(
      between(.data$year, 2018, 2026),
      .data$row %in% sprintf("r%02d", 11:20)
    )
) %>%
  mutate(period = "Post")

all_data <- bind_rows(pre_data, post_data) %>%
  mutate(
    shore = sub("[0-9]+$", "", .data$plot),
    period = factor(.data$period, levels = c("Pre", "Post"))
  )


# ============================================================
# 7. 相対被度
#
# 分子：対象種の出現格子記録数
# 分母：全種の出現格子記録数
#
# 相対被度（%）= 分子 / 分母 × 100
#
# 同じ格子に複数種が存在する場合も、
# それぞれの出現を1記録としてカウントする。
# ============================================================

valid_records <- all_data %>%
  filter(
    .data$species != "0",
    !grepl("^死", .data$species)
  )

total_counts <- valid_records %>%
  count(
    .data$shore, .data$plot,
    .data$period, .data$year,
    name = "n_total"
  )

species_counts <- valid_records %>%
  count(
    .data$shore, .data$plot,
    .data$period, .data$year,
    .data$species,
    name = "n_species"
  )

relative_coverage <- species_counts %>%
  left_join(
    total_counts,
    by = c("shore", "plot", "period", "year")
  ) %>%
  mutate(
    relative_coverage = 100 * n_species / n_total
  )


# ============================================================
# 8. 非出現年を0で補完
# ============================================================

relative_complete <- bind_rows(
  lapply(shores, function(shore_id) {
    
    years <- total_counts %>%
      filter(.data$shore == .env$shore_id) %>%
      select(all_of(c("shore", "plot", "period", "year")))
    
    species_set <- relative_coverage %>%
      filter(.data$shore == .env$shore_id) %>%
      distinct(.data$species)
    
    tidyr::crossing(years, species_set) %>%
      left_join(
        relative_coverage %>%
          select(all_of(c(
            "shore", "plot", "period", "year",
            "species", "relative_coverage"
          ))),
        by = c("shore", "plot", "period", "year", "species")
      ) %>%
      mutate(
        relative_coverage = replace_na(relative_coverage, 0)
      )
  })
)


# ============================================================
# 9. 各海岸の優占5種
#
# プロット×期間の平均相対被度を計算し、
# Pre/Post全体で順位付け
# ============================================================

species_ranking <- relative_complete %>%
  group_by(shore, period, plot, species) %>%
  summarise(
    plot_mean = mean(relative_coverage),
    .groups = "drop"
  ) %>%
  group_by(shore, species) %>%
  summarise(
    mean_relative_coverage = mean(plot_mean),
    .groups = "drop"
  ) %>%
  arrange(shore, desc(mean_relative_coverage), species) %>%
  group_by(shore) %>%
  mutate(rank = row_number()) %>%
  ungroup()

top_species <- species_ranking %>%
  filter(rank <= 5)

save_csv(species_ranking, "species_ranking.csv")


relative_top <- relative_complete %>%
  inner_join(
    top_species %>% select(shore, species),
    by = c("shore", "species")
  ) %>%
  arrange(shore, plot, period, species, year)

save_csv(relative_top, "relative_coverage_timeseries.csv")


# ============================================================
# 10. トレンド解析関数
# ============================================================

trend_test <- function(dat) {
  
  dat <- dat %>% arrange(year)
  
  x <- dat$year
  y <- dat$relative_coverage
  
  n <- length(y)
  
  empty <- tibble(
    n_years = n,
    slope = NA_real_,
    slope_ci_lower = NA_real_,
    slope_ci_upper = NA_real_,
    lm_p = NA_real_,
    r_squared = NA_real_,
    mk_tau = NA_real_,
    mk_p = NA_real_,
    mean_relative_coverage = mean(y),
    sd_relative_coverage = sd(y),
    net_change = tail(y, 1) - y[1]
  )
  
  if (n < 3 || length(unique(x)) < 3) {
    return(empty)
  }
  
  if (length(unique(y)) == 1) {
    empty$slope <- 0
    empty$slope_ci_lower <- 0
    empty$slope_ci_upper <- 0
    return(empty)
  }
  
  fit <- lm(y ~ I(x - min(x)))
  
  fit_sum <- summary(fit)
  ci <- confint(fit)
  
  mk <- Kendall::MannKendall(y)
  
  tibble(
    n_years = n,
    slope = unname(coef(fit)[2]),
    slope_ci_lower = ci[2, 1],
    slope_ci_upper = ci[2, 2],
    lm_p = fit_sum$coefficients[2, 4],
    r_squared = fit_sum$r.squared,
    mk_tau = as.numeric(mk$tau),
    mk_p = as.numeric(mk$sl),
    mean_relative_coverage = mean(y),
    sd_relative_coverage = sd(y),
    net_change = tail(y, 1) - y[1]
  )
}


# ============================================================
# 11. 各プロット×各種×期間の検定
# ============================================================

trend_summary <- relative_top %>%
  group_by(shore, plot, period, species) %>%
  group_modify(~ trend_test(.x)) %>%
  ungroup() %>%
  mutate(
    lm_significant = !is.na(lm_p) & lm_p < 0.05,
    mk_significant = !is.na(mk_p) & mk_p < 0.05,
    trend_direction = case_when(
      slope > 0 ~ "Increase",
      slope < 0 ~ "Decrease",
      TRUE ~ "Flat"
    )
  )

save_csv(trend_summary, "trend_summary.csv")


# ============================================================
# 12. 作図設定
# ============================================================

plot_theme <- theme_bw(
  base_size = 11,
  base_family = "jp"
) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 9),
    legend.position = "bottom"
  )

period_colors <- c(
  Pre = "#2876B9",
  Post = "#D55E00"
)


# ============================================================
# 13. 各プロット・各種の時系列
#
# 実線：相対被度
# 破線：線形トレンド
# ============================================================

plot_pages <- list()

for (shore_id in shores) {
  
  dat <- relative_top %>%
    filter(.data$shore == .env$shore_id)
  
  p <- ggplot(
    dat,
    aes(
      x = year,
      y = relative_coverage,
      color = period,
      group = period
    )
  ) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 1.6) +
    geom_smooth(
      method = "lm",
      formula = y ~ x,
      se = FALSE,
      linetype = "dashed",
      linewidth = 0.6
    ) +
    facet_grid(species ~ plot, scales = "free_y") +
    scale_color_manual(values = period_colors) +
    scale_x_continuous(
      breaks = c(2002, 2006, 2010, 2018, 2022, 2026)
    ) +
    labs(
      title = paste0(shore_id, "：プロット別の相対被度"),
      subtitle = "実線：観測値　破線：線形トレンド",
      x = "年",
      y = "相対被度（%）",
      color = "期間"
    ) +
    plot_theme +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  plot_pages[[shore_id]] <- p
}

save_pdf(
  plot_pages,
  "plot_relative_coverage_timeseries.pdf",
  width = 16,
  height = 12
)


# ============================================================
# 14. 海岸平均の時系列
# ============================================================

shore_mean <- relative_top %>%
  group_by(shore, period, year, species) %>%
  summarise(
    mean_relative_coverage = mean(relative_coverage),
    sd_relative_coverage = sd(relative_coverage),
    n_plots = n(),
    .groups = "drop"
  )

shore_pages <- list()

for (shore_id in shores) {
  
  dat <- shore_mean %>%
    filter(.data$shore == .env$shore_id)
  
  p <- ggplot(
    dat,
    aes(
      x = year,
      y = mean_relative_coverage,
      color = period,
      group = period
    )
  ) +
    geom_errorbar(
      aes(
        ymin = pmax(
          0,
          mean_relative_coverage - sd_relative_coverage
        ),
        ymax = mean_relative_coverage + sd_relative_coverage
      ),
      width = 0.25,
      alpha = 0.6
    ) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2) +
    geom_smooth(
      method = "lm",
      formula = y ~ x,
      se = FALSE,
      linetype = "dashed"
    ) +
    facet_wrap(~ species, ncol = 2, scales = "free_y") +
    scale_color_manual(values = period_colors) +
    labs(
      title = paste0(shore_id, "：海岸平均の相対被度"),
      subtitle = "平均 ± SD、破線：線形トレンド",
      x = "年",
      y = "平均相対被度（%）",
      color = "期間"
    ) +
    plot_theme
  
  shore_pages[[shore_id]] <- p
}

save_pdf(
  shore_pages,
  "shore_relative_coverage_timeseries.pdf",
  width = 12,
  height = 9
)


# ============================================================
# 15. 検定結果の集計
# ============================================================

trend_counts <- trend_summary %>%
  group_by(shore, period) %>%
  summarise(
    n_series = n(),
    lm_p_lt_005 = sum(lm_significant),
    mk_p_lt_005 = sum(mk_significant),
    both_p_lt_005 = sum(
      lm_significant & mk_significant
    ),
    .groups = "drop"
  )

save_csv(trend_counts, "trend_counts.csv")

print(trend_counts)


# ============================================================
# 16. 完了
# ============================================================

cat("\n解析完了\n")
cat("出力先：", out_dir, "\n")

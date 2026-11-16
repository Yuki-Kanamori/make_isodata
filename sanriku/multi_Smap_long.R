
library(tidyverse)

## ============================================================
## Regularized multivariate S-map
## Sanriku 2002–2026 / 23 plots / top 5 species
##
## - Original raw-data format
## - Coverage or relative frequency
## - No differencing
## - Hashimoto et al. (2024) theta/lambda grid
## - LOOCV parameter selection
## - Annual coefficients including 2026
## - Three-period summaries and plots
## ============================================================

INPUT <- "sanriku07C.csv"
OUT <- "Smap_Hashimoto"

dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

PLOTS <- c(
  paste0("MJ", 1:5),
  paste0("AG", 1:5),
  paste0("OR", 1:5),
  paste0("KG", 1:5),
  paste0("AK", 1:3)
)

YEARS <- 2002:2026
N_POINTS <- 200
N_SPECIES <- 5

THETA <- c(0, .1, .5, 1, 1.5, 2, 2.5, 3, 4, 6, 8)
LAMBDA <- c(0, .0001, .001, .01, .1, .5, 1, 2)

PERIOD_LEVELS <- c("Pre", "Middle", "Post")

period_of <- function(y) {
  case_when(
    y >= 2002 & y <= 2010 ~ "Pre",
    y >= 2011 & y <= 2017 ~ "Middle",
    y >= 2018 & y <= 2026 ~ "Post",
    TRUE ~ NA_character_
  )
}

save_csv <- function(x, filename) {
  write.csv(
    x,
    file.path(OUT, filename),
    row.names = FALSE,
    fileEncoding = "CP932",
    na = ""
  )
}

## ============================================================
## 1. Read original raw data
## ============================================================

raw <- read.csv(
  INPUT,
  fileEncoding = "CP932",
  stringsAsFactors = FALSE,
  na.strings = c("NA", "")
) %>%
  filter(
    plot %in% PLOTS,
    year %in% YEARS
  ) %>%
  mutate(
    species = trimws(species),
    species = na_if(species, ""),
    species = if_else(
      species == "座フクロフノリ",
      "フクロフノリ",
      species
    ),
    point_id = paste(column, row, sep = "_")
  )

required_columns <- c(
  "plot", "year", "column", "row", "species"
)

if (!all(required_columns %in% names(raw))) {
  stop("Required columns are missing.")
}

if (anyNA(raw$plot) ||
    anyNA(raw$year) ||
    anyNA(raw$column) ||
    anyNA(raw$row)) {
  stop("Missing plot/year/column/row identifiers.")
}

## ============================================================
## 2. Check sampling effort
## ============================================================

duplicates <- raw %>%
  count(plot, year, point_id, name = "n") %>%
  filter(n > 1)

save_csv(duplicates, "duplicate_points.csv")

if (nrow(duplicates) > 0) {
  stop(
    "Duplicated sampling points detected. ",
    "See duplicate_points.csv."
  )
}

effort_observed <- raw %>%
  group_by(plot, year) %>%
  summarise(
    n_rows = n(),
    n_points = n_distinct(point_id),
    n_species_recorded = sum(
      !is.na(species) & species != "0"
    ),
    n_zero = sum(species == "0", na.rm = TRUE),
    n_na = sum(is.na(species)),
    n_valid = sum(!is.na(species)),
    .groups = "drop"
  )

effort <- expand_grid(
  plot = PLOTS,
  year = YEARS
) %>%
  left_join(
    effort_observed,
    by = c("plot", "year")
  ) %>%
  mutate(
    across(
      c(
        n_rows, n_points,
        n_species_recorded,
        n_zero, n_na, n_valid
      ),
      ~replace_na(.x, 0L)
    ),
    status = case_when(
      n_points == 0 ~ "no_data",
      n_points > N_POINTS ~ "more_than_200",
      n_na > 0 ~ "contains_NA",
      n_points < N_POINTS ~ "fewer_than_200",
      n_points == N_POINTS ~ "complete_200",
      TRUE ~ "unknown"
    )
  )

save_csv(effort, "sampling_effort.csv")

print(effort %>% count(status))

if (any(effort$n_points > N_POINTS)) {
  stop("More than 200 sampling points detected.")
}

# If all plot-years contain exactly 200 observed points,
# use coverage. Otherwise use relative frequency.

all_200 <- all(
  effort$n_points == N_POINTS &
    effort$n_na == 0
)

VALUE_TYPE <- if (all_200) {
  "coverage"
} else {
  "relative_frequency"
}

message("Selected measurement: ", VALUE_TYPE)

# A plot-year with no valid observations cannot be analysed.
# NA is treated as missing, never as absence.

effort <- effort %>%
  mutate(
    usable = n_valid > 0 & n_points <= N_POINTS
  )

save_csv(effort, "sampling_effort.csv")

## ============================================================
## 3. Construct coverage / relative frequency
## ============================================================

counts <- raw %>%
  filter(
    !is.na(species),
    species != "0"
  ) %>%
  count(
    plot, year, species,
    name = "coverage"
  )

species_list <- sort(unique(counts$species))

dat <- expand_grid(
  plot = PLOTS,
  year = YEARS,
  species = species_list
) %>%
  left_join(
    counts,
    by = c("plot", "year", "species")
  ) %>%
  left_join(
    effort %>%
      select(
        plot, year, n_valid,
        n_na, usable
      ),
    by = c("plot", "year")
  ) %>%
  mutate(
    coverage = if_else(
      usable,
      replace_na(coverage, 0L),
      NA_integer_
    ),
    value = if (all_200) {
      as.numeric(coverage)
    } else {
      if_else(
        usable,
        as.numeric(coverage) / n_valid,
        NA_real_
      )
    },
    period = period_of(year)
  )

save_csv(dat, "species_timeseries.csv")

## ============================================================
## 4. Select top five species per plot
##    Based on Pre and Post
## ============================================================

top5 <- dat %>%
  filter(
    year <= 2010 | year >= 2018
  ) %>%
  group_by(plot, species) %>%
  summarise(
    mean_value = if (all(is.na(value))) {
      NA_real_
    } else {
      mean(value, na.rm = TRUE)
    },
    .groups = "drop"
  ) %>%
  filter(
    is.finite(mean_value),
    mean_value > 0
  ) %>%
  group_by(plot) %>%
  arrange(
    desc(mean_value),
    species,
    .by_group = TRUE
  ) %>%
  slice_head(n = N_SPECIES) %>%
  mutate(rank = row_number()) %>%
  ungroup()

save_csv(top5, "top5_species.csv")

if (any(
  top5 %>%
  count(plot) %>%
  pull(n) != N_SPECIES
)) {
  stop("Some plots have fewer than five recorded species.")
}

## ============================================================
## 5. Weighted ridge S-map
##
## x(t) -> x(t+1)
## No differencing
## Intercept is not penalized
## ============================================================

smap_fit <- function(X, y, xq, theta, lambda) {
  
  X <- as.matrix(X)
  xq <- as.numeric(xq)
  
  if (
    nrow(X) < 4 ||
    anyNA(X) ||
    anyNA(y) ||
    anyNA(xq)
  ) {
    return(NULL)
  }
  
  d <- sqrt(
    rowSums(
      sweep(X, 2, xq, "-")^2
    )
  )
  
  md <- mean(d)
  
  w <- if (
    theta == 0 || md < 1e-12
  ) {
    rep(1, length(d))
  } else {
    exp(-theta * d / md)
  }
  
  if (
    !all(is.finite(w)) ||
    sum(w) < 1e-12
  ) {
    return(NULL)
  }
  
  w <- w / mean(w)
  
  Z <- cbind(1, X)
  
  penalty <- diag(
    c(0, rep(lambda, ncol(X)))
  )
  
  A <- crossprod(Z, Z * w) + penalty
  b <- crossprod(Z, y * w)
  
  beta <- tryCatch(
    as.numeric(solve(A, b)),
    error = function(e) NULL
  )
  
  if (
    is.null(beta) ||
    any(!is.finite(beta))
  ) {
    return(NULL)
  }
  
  list(
    beta = beta,
    prediction = sum(c(1, xq) * beta)
  )
}

## ============================================================
## 6. Standardization
## ============================================================

standardize_train <- function(X, Xq, y) {
  
  X <- as.matrix(X)
  Xq <- as.matrix(Xq)
  
  mu <- colMeans(X)
  sdv <- apply(X, 2, sd)
  
  sdv[
    !is.finite(sdv) | sdv < 1e-10
  ] <- 1
  
  ym <- mean(y)
  ys <- sd(y)
  
  if (
    !is.finite(ys) ||
    ys < 1e-10
  ) {
    ys <- 1
  }
  
  list(
    X = sweep(
      sweep(X, 2, mu, "-"),
      2, sdv, "/"
    ),
    Xq = sweep(
      sweep(Xq, 2, mu, "-"),
      2, sdv, "/"
    ),
    y = (y - ym) / ys,
    mu = mu,
    sdv = sdv,
    ym = ym,
    ys = ys
  )
}

## ============================================================
## 7. LOOCV for theta/lambda
## ============================================================

cv_one <- function(X, y, theta, lambda) {
  
  pred <- rep(NA_real_, length(y))
  
  for (k in seq_along(y)) {
    
    train <- setdiff(
      seq_along(y), k
    )
    
    st <- standardize_train(
      X[train, , drop = FALSE],
      X[k, , drop = FALSE],
      y[train]
    )
    
    fit <- smap_fit(
      st$X,
      st$y,
      st$Xq[1, ],
      theta,
      lambda
    )
    
    if (!is.null(fit)) {
      pred[k] <-
        fit$prediction * st$ys + st$ym
    }
  }
  
  if (anyNA(pred)) {
    return(Inf)
  }
  
  sqrt(mean((y - pred)^2))
}

## ============================================================
## 8. Main S-map analysis
## ============================================================

all_coeff <- list()
all_params <- list()
all_cv <- list()
all_diagnostics <- list()

for (pl in PLOTS) {
  
  message("Processing: ", pl)
  
  spp <- top5 %>%
    filter(plot == pl) %>%
    arrange(rank) %>%
    pull(species)
  
  if (length(spp) != N_SPECIES) {
    warning("Skipping plot: ", pl)
    next
  }
  
  wide <- dat %>%
    filter(
      plot == pl,
      species %in% spp
    ) %>%
    select(year, species, value) %>%
    pivot_wider(
      names_from = species,
      values_from = value
    ) %>%
    arrange(year)
  
  Xall <- as.matrix(
    wide[, spp, drop = FALSE]
  )
  
  years <- wide$year
  
  # Valid consecutive transitions
  idx <- which(diff(years) == 1)
  
  X <- Xall[idx, , drop = FALSE]
  Y <- Xall[idx + 1, , drop = FALSE]
  
  transition_years <- years[idx]
  
  valid <- complete.cases(X) &
    complete.cases(Y)
  
  X <- X[valid, , drop = FALSE]
  Y <- Y[valid, , drop = FALSE]
  
  transition_years <-
    transition_years[valid]
  
  all_diagnostics[[length(all_diagnostics) + 1]] <-
    tibble(
      plot = pl,
      n_transitions = nrow(X),
      n_species = length(spp),
      first_year = if (nrow(X) > 0) {
        min(transition_years)
      } else {
        NA_integer_
      },
      last_year = if (nrow(X) > 0) {
        max(transition_years)
      } else {
        NA_integer_
      }
    )
  
  if (nrow(X) < 8) {
    warning("Insufficient transitions: ", pl)
    next
  }
  
  # Estimate each recipient separately
  for (j in seq_along(spp)) {
    
    target <- spp[j]
    y <- Y[, j]
    
    grid <- expand_grid(
      theta = THETA,
      lambda = LAMBDA
    )
    
    grid$rmse <- map2_dbl(
      grid$theta,
      grid$lambda,
      ~cv_one(X, y, .x, .y)
    )
    
    grid <- grid %>%
      mutate(
        plot = pl,
        target = target,
        .before = 1
      )
    
    all_cv[[length(all_cv) + 1]] <- grid
    
    best <- grid %>%
      filter(is.finite(rmse)) %>%
      arrange(rmse, lambda, theta) %>%
      slice_head(n = 1)
    
    if (nrow(best) == 0) {
      warning(
        "No valid parameter combination: ",
        pl, " / ", target
      )
      next
    }
    
    all_params[[length(all_params) + 1]] <-
      best
    
    # Standardize using all available transitions
    st <- standardize_train(X, X, y)
    
    # Evaluate coefficients at all observed years,
    # including 2026.
    #
    # A coefficient at 2026 is evaluated at the 2026
    # state; its 2027 prediction is not observed.
    
    valid_query <- which(
      complete.cases(Xall)
    )
    
    Xquery <- Xall[
      valid_query, ,
      drop = FALSE
    ]
    
    query_years <- years[valid_query]
    
    Xquery_std <- sweep(
      sweep(
        Xquery, 2, st$mu, "-"
      ),
      2, st$sdv, "/"
    )
    
    for (k in seq_len(nrow(Xquery_std))) {
      
      fit <- smap_fit(
        st$X,
        st$y,
        Xquery_std[k, ],
        best$theta,
        best$lambda
      )
      
      if (is.null(fit)) next
      
      # Return slopes to original coverage /
      # relative-frequency units.
      slopes <- fit$beta[-1] *
        st$ys / st$sdv
      
      all_coeff[[length(all_coeff) + 1]] <-
        tibble(
          plot = pl,
          year = query_years[k],
          period = period_of(query_years[k]),
          target = target,
          predictor = spp,
          coefficient = as.numeric(slopes),
          theta = best$theta,
          lambda = best$lambda,
          interaction_type = if_else(
            target == spp,
            "self",
            "cross"
          )
        )
    }
  }
}

coeff <- bind_rows(all_coeff)
params <- bind_rows(all_params)
cv_results <- bind_rows(all_cv)
diagnostics <- bind_rows(all_diagnostics)

if (nrow(coeff) == 0) {
  stop("No coefficients were estimated.")
}

save_csv(
  coeff,
  "coefficients_yearly.csv"
)

save_csv(
  params,
  "selected_parameters.csv"
)

save_csv(
  cv_results,
  "theta_lambda_cv.csv"
)

save_csv(
  diagnostics,
  "diagnostics.csv"
)

## ============================================================
## 9. Annual coefficient time-series plots
## ============================================================

pdf(
  file.path(
    OUT, "interaction_timeseries.pdf"
  ),
  width = 16,
  height = 12,
  onefile = TRUE
)

for (pl in PLOTS) {
  
  d <- coeff %>%
    filter(plot == pl)
  
  if (nrow(d) == 0) next
  
  g <- ggplot(
    d,
    aes(
      x = year,
      y = coefficient
    )
  ) +
    geom_hline(
      yintercept = 0,
      linetype = "dotted",
      color = "grey55"
    ) +
    geom_vline(
      xintercept = c(2010.5, 2017.5),
      linetype = "dashed",
      color = "grey70"
    ) +
    geom_line(linewidth = .45) +
    geom_point(size = .9) +
    facet_grid(
      target ~ predictor,
      scales = "free_y"
    ) +
    scale_x_continuous(
      breaks = seq(2002, 2026, 4),
      limits = c(2002, 2026)
    ) +
    labs(
      title = paste(
        "S-map interaction coefficients:", pl
      ),
      subtitle = paste(
        "Rows: recipients | Columns: predictors |",
        VALUE_TYPE
      ),
      x = "Year",
      y = "S-map coefficient"
    ) +
    theme_bw(base_size = 10) +
    theme(
      strip.text = element_text(size = 8),
      axis.text.x = element_text(
        angle = 45,
        hjust = 1
      )
    )
  
  print(g)
}

dev.off()

## ============================================================
## 10. Three-period mean coefficients
## ============================================================

coeff <- coeff %>%
  mutate(
    period = factor(
      period,
      levels = PERIOD_LEVELS
    )
  )

period_summary <- coeff %>%
  group_by(
    plot, period, target,
    predictor, interaction_type
  ) %>%
  summarise(
    n_years = n(),
    mean_coefficient = mean(coefficient),
    sd_coefficient = if (n() > 1) {
      sd(coefficient)
    } else {
      NA_real_
    },
    median_coefficient = median(coefficient),
    positive_fraction = mean(coefficient > 0),
    .groups = "drop"
  )

save_csv(
  period_summary,
  "period_mean_coefficients.csv"
)

period_comparison <- period_summary %>%
  select(
    plot, target, predictor,
    interaction_type,
    period, mean_coefficient
  ) %>%
  pivot_wider(
    names_from = period,
    values_from = mean_coefficient
  ) %>%
  mutate(
    Middle_minus_Pre = Middle - Pre,
    Post_minus_Middle = Post - Middle,
    Post_minus_Pre = Post - Pre
  )

save_csv(
  period_comparison,
  "period_comparison.csv"
)

## ============================================================
## 11. Period mean plots
## ============================================================

pdf(
  file.path(
    OUT, "interaction_period_means.pdf"
  ),
  width = 16,
  height = 12,
  onefile = TRUE
)

for (pl in PLOTS) {
  
  d <- period_summary %>%
    filter(plot == pl)
  
  if (nrow(d) == 0) next
  
  g <- ggplot(
    d,
    aes(
      x = period,
      y = mean_coefficient,
      group = 1
    )
  ) +
    geom_hline(
      yintercept = 0,
      linetype = "dotted"
    ) +
    geom_line(color = "grey50") +
    geom_point(size = 2) +
    facet_grid(
      target ~ predictor,
      scales = "free_y"
    ) +
    labs(
      title = paste(
        "Period mean coefficients:", pl
      ),
      x = "Period",
      y = "Mean S-map coefficient"
    ) +
    theme_bw(base_size = 10) +
    theme(
      strip.text = element_text(size = 8)
    )
  
  print(g)
}

dev.off()

## ============================================================
## 12. Boxplots of annual coefficients
## ============================================================

pdf(
  file.path(
    OUT, "interaction_period_boxplots.pdf"
  ),
  width = 16,
  height = 12,
  onefile = TRUE
)

for (pl in PLOTS) {
  
  d <- coeff %>%
    filter(plot == pl)
  
  if (nrow(d) == 0) next
  
  g <- ggplot(
    d,
    aes(
      x = period,
      y = coefficient
    )
  ) +
    geom_hline(
      yintercept = 0,
      linetype = "dotted",
      color = "grey55"
    ) +
    geom_boxplot(
      width = .6,
      outlier.shape = NA,
      fill = "grey85"
    ) +
    geom_jitter(
      width = .12,
      height = 0,
      size = .9,
      alpha = .6
    ) +
    stat_summary(
      fun = mean,
      geom = "point",
      shape = 18,
      size = 2.5
    ) +
    facet_grid(
      target ~ predictor,
      scales = "free_y"
    ) +
    labs(
      title = paste(
        "Period comparison:", pl
      ),
      subtitle = "Diamond = period mean",
      x = "Period",
      y = "S-map coefficient"
    ) +
    theme_bw(base_size = 10) +
    theme(
      strip.text = element_text(size = 8)
    )
  
  print(g)
}

dev.off()

## ============================================================
## 13. Final checks
## ============================================================

message("Measurement: ", VALUE_TYPE)

message("Number of plots analysed: ",
        n_distinct(coeff$plot))

message("Coefficient years:")
print(
  coeff %>%
    distinct(plot, year) %>%
    count(plot, name = "n_years")
)

message("Selected theta:")
print(
  params %>%
    count(theta)
)

message("Selected lambda:")
print(
  params %>%
    count(lambda)
)

message("Completed. Output directory: ", OUT)

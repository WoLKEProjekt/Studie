#' Report one contrast from an emmeans contrast object in APA style
#'
#' This function extracts the estimate, standard error, and p-value from a
#' given contrast (e.g., an emmeans contrast object), and returns them as a
#' character string formatted in APA style with inline math notation.
#'
#' @param contrast An emmeans contrast object or data.frame-like object
#'   containing at least the columns `estimate`, `SE`, and `p.value`.
#' @param idx Numeric index of the row (contrast) to report. Default = 1.
#'
#' @return A character string, e.g.
#'   `"$1.10$, $SE = 0.63$, $p = .086$"`,
#'   ready for inline reporting in Quarto/Markdown.
#'
#' @examples
#' # emms <- emmeans(model, ~ intervention * time)
#' # ctr <- contrast(emms, method = "revpairwise")
#' # report_contrast(ctr, 1)
report_contrast <- function(contrast, idx = 1) {
  d <- as.data.frame(contrast[idx])
  est <- d$estimate %>% apa_num
  se <- d$SE %>% apa_num
  t <- d$t.ratio %>% apa_num
  p_val <- d$p.value %>% apa_p(add_equals = TRUE)

  paste0("$b =", est, "$, $SE = ", se, "$, $t=", t, "$, $p ", p_val, "$")
}
#' Report ANOVA results for a fixed effect in an lmer model
#'
#' Runs `lmerTest::anova()` on a fitted mixed-effects model using the
#' specified type of sums of squares (I, II, or III) and denominator
#' degrees of freedom method (default: Kenward–Roger). The output is
#' formatted according to APA style via `papaja::apa_print()`.
#'
#' @param model A fitted lmer model (from lme4 or lmerTest).
#' @param term  Character string, the name of the fixed effect to report.
#' @param type  Type of sums of squares to use ("I", "II", or "III").
#' @param ddf   Method for denominator degrees of freedom (e.g., "Kenward-Roger").
#'
#' @return A character string with APA-style ANOVA results, e.g.,
#'   "F(1, 142) = 4.37, p = .038".
#'
#' @examples
#' # report_lmer_anova(fit, term = "intervention_time")
report_lmer_anova <- function(
  model,
  term = "intervention_time",
  type = "II",
  ddf = "Kenward-Roger"
) {
  if (!requireNamespace("lmerTest", quietly = TRUE)) {
    stop("Package 'lmerTest' is required.")
  }
  if (!inherits(model, "lmerModLmerTest")) {
    model <- lmerTest::as_lmerModLmerTest(model)
  }

  aov_tab <- anova(model, type = type, ddf = ddf)

  apa_print(aov_tab)$full_result[[term]]
}

#' Report one fixed effect from an (lmer/lmerTest/lm) model in APA style
#'
#' Method:
#'   Converts the model to an `lmerTest` object when possible to obtain
#'   per-term t-tests and denominator dfs (e.g., Kenward–Roger). Extracts the
#'   requested coefficient (by name or index) via `broom::tidy()`, then formats
#'   **b**, **SE**, **t**, **p** using `papaja::apa_num()` / `papaja::apa_p()`.
#'   Falls back to the residual df for plain `lm` (or when per-term df are
#'   unavailable). Output can be wrapped in math mode for Quarto.
#'
#' @param model A fitted model (`lmerMod`/`lmerModLmerTest`/`lm`).
#' @param term  Coefficient to report (name or 1-based index). Default: 1.
#' @param ddf   Denominator-df method passed to `summary()` when model is
#'              `lmerTest` (e.g., `"Kenward-Roger"` or `"Satterthwaite"`).
#' @param math  If TRUE, wrap the result in `$...$` for math rendering.
#'
#' @return A single character string, e.g.
#'   `$b = 0.23,\ SE = 0.07,\ t(28) = 3.45,\ p = .002$`
report_lmer <- function(model, term = 8, ddf = "Kenward-Roger", math = TRUE) {
  if (!requireNamespace("papaja", quietly = TRUE)) {
    stop(
      "Package 'papaja' is required. Install it via install.packages('papaja')."
    )
  }
  if (!requireNamespace("broom", quietly = TRUE)) {
    stop(
      "Package 'broom' is required. Install it via install.packages('broom')."
    )
  }

  # use broom::tidy to extract coefficient info
  tt <- broom::tidy(model, ddf = ddf)
  terms <- tt$term

  # resolve term (by name or index)
  i <- if (is.numeric(term)) as.integer(term) else match(term, terms)
  if (is.na(i) || i < 1 || i > nrow(tt)) {
    stop(
      "Requested `term` not found. Available terms: ",
      paste(terms, collapse = ", ")
    )
  }

  # pull and format values
  b <- papaja::apa_num(tt$estimate[i])
  se <- papaja::apa_num(tt$std.error[i])
  t <- papaja::apa_num(tt$statistic[i])
  df <- papaja::apa_num(tt$df[i])
  p <- apa_p(tt$p.value[i], add_equals = T)

  wrap_math <- function(x) if (math) paste0("$", x, "$") else x

  txt <- paste(
    wrap_math(paste0("b = ", b)),
    wrap_math(paste0("SE = ", se)),
    wrap_math(paste0("t(", df, ") = ", t)),
    wrap_math(paste0("p", p)),
    sep = ", "
  )
  txt
}

#' Report one fixed effect from an lmer model using papaja
#'
#' @param model lmer model (from lmerTest::lmer)
#' @param term  name of the fixed effect (character)
#' @return character string like "b = 0.23, t = 2.34, p = .021"
report_lmer_papaja <- function(
  model,
  term = "intervention_C_MINT_Sprache_timepost",
  ddf = "Kenward-Roger"
) {
  # if lmer model is not of type lmerTest, then refit it
  if (class(model)[1] != "lmerModLmerTest") {
    model <- lmerTest::as_lmerModLmerTest(model)
  }
  apa_print(
    model,
    standardized = FALSE
  )$full_result[[term]] # get apa_print output
}

plot_outcome <- function(
  data,
  groups = "intervention",
  ylab = "Rating",
  scales_facet = "free",
  y_limits = NULL,
  error_bars = FALSE
) {
  # set color scale/palette and plot theme
  wolke_color_scale <- scale_colour_manual(values = rev(pal_jco("default")(3)))
  wolke_theme <-
    jtools::theme_apa(
      legend.pos = "bottom",
      legend.use.title = T,
      facet.title.size = 12,
      x.font.size = 18,
      y.font.size = 18,
      legend.font.size = 12
    )
  #wolke_theme <- theme_bw(base_size = 18)

  data %>%
    ggplot(aes(
      x = time,
      y = score,
      color = get(groups),
      group = get(groups)
    )) +
    stat_summary(fun.data = mean_se, aes(shape = get(groups))) +
    stat_summary(
      fun.data = mean_se,
      geom = "line",
      aes(linetype = get(groups))
    ) +
    # add error bars
    {
      if (error_bars) {
        stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1)
      }
    } +
    facet_wrap(~outcome, scales = scales_facet, ncol = 2) +
    labs(
      x = "Time",
      y = ylab,
      color = "Treatment Groups",
      shape = "Treatment Groups",
      linetype = "Treatment Groups"
    ) +
    {
      if (!is.null(y_limits)) scale_y_continuous(limits = y_limits) else NULL
    } +
    wolke_color_scale +
    wolke_theme
}

plot_outcome_agg <- function(
  data,
  groups = "intervention",
  outcome_var = "outcome",
  ylab = "Score",
  scales_facet = "fixed",
  ncol = 3,
  y_limits = NULL
) {
  # set color scale/palette and plot theme (same as plot_outcome)
  wolke_color_scale <- scale_colour_manual(values = rev(pal_jco("default")(3)))
  wolke_theme <-
    jtools::theme_apa(
      legend.pos = "bottom",
      legend.use.title = T,
      facet.title.size = 12,
      x.font.size = 18,
      y.font.size = 18,
      legend.font.size = 12
    )

  data %>%
    ggplot(aes(
      x = time,
      y = score,
      color = get(groups),
      group = get(groups),
      shape = get(groups),
      linetype = get(groups)
    )) +
    geom_point(size = 2) +
    geom_line(size = 0.5) +
    facet_wrap(
      as.formula(paste0("~", outcome_var)),
      scales = scales_facet,
      ncol = ncol
    ) +
    labs(
      x = "Time",
      y = ylab,
      color = "Treatment Groups",
      shape = "Treatment Groups",
      linetype = "Treatment Groups"
    ) +
    {
      if (!is.null(y_limits)) scale_y_continuous(limits = y_limits) else NULL
    } +
    wolke_color_scale +
    wolke_theme
}

plot_outcome_over_time <- function(
  data = NULL,
  outcome = "TPACK",
  groups = "intervention",
  title = "title",
  subtitle = "subtitle",
  ylab = "Rating"
) {
  base_size = 18
  p <- ggplot(
    data %>% filter(!is.na(get(outcome))),
    aes(time, get(outcome), group = get(groups))
  ) +
    stat_summary(fun.data = mean_se, geom = "line", aes(linetype = group)) +
    stat_summary(fun.data = mean_se, geom = "errorbar", width = .1) +
    stat_summary(
      fun.data = mean_se,
      aes(shape = group, fill = group),
      size = 1
    ) +
    scale_shape_manual(values = c(21, 21)) +
    scale_fill_manual(values = c("black", "white")) +
    ylab(ylab) +
    xlab("Time") +
    labs(shape = "Group", fill = "Group", linetype = "Group") +
    theme_bw(base_size = base_size) +
    scale_linetype_manual(values = c("solid", "longdash")) +
    theme(
      legend.position = "none",
      legend.key.width = unit(5, "line"),
      legend.margin = margin(c(5, 5, 5, 5)),
      legend.background = element_rect(colour = "black"),
      plot.title = element_text(
        hjust = .5,
        face = "bold",
        size = base_size - 1
      ),
      plot.subtitle = element_text(hjust = .5, size = base_size - 3)
    ) +
    ggtitle(title, subtitle)

  return(p)
}

fit_ancova <- function(
  response_var,
  groups = NULL,
  controls = NULL,
  se_type = "HC3",
  data
) {
  data <- data %>%
    rename(
      !!"pretest" := paste0(response_var, "_pre"),
    )
  formula_string <- paste(
    paste0(response_var, "_post"),
    "~",
    groups,
    "+",
    "pretest",
    "+",
    paste(controls, collapse = "+")
  )
  if (!is.null(se_type)) {
    model <- lm_robust(
      as.formula(formula_string),
      data = data,
      se_type = se_type
    )
  } else {
    model <- lm(as.formula(formula_string), data = data)
  }
  return(model)
}

fit_lmer <- function(
  response_var,
  groups = NULL,
  time_var = NULL,
  controls = NULL,
  random_terms = NULL,
  lmer_control = NULL,
  data
) {
  for (idx in seq_along(random_terms)) {
    random_term <- random_terms[idx]

    formula_string <- paste(
      response_var,
      "~",
      ifelse(
        !is.null(groups),
        paste0("(", paste(groups, collapse = " + "), ")", " * "),
        ""
      ),
      time_var,
      "+",
      paste(controls, collapse = "+"),
      ifelse(!is.null(random_term), "+", ""),
      random_term
    )

    # print(formula_string)
    if (is.null(lmer_control) && !is.null(random_term)) {
      model <- lmer(as.formula(formula_string), data = data)
    } else if (!is.null(random_term)) {
      model <- lmer(
        as.formula(formula_string),
        data = data,
        control = lmer_control
      )
    } else {
      model <- lm(as.formula(formula_string), data = data)
    }
    if (!isSingular(model)) {
      return(model)
    }

    if (idx == length(random_terms)) {
      warning(
        paste(
          "Model for",
          response_var,
          "is singular. The simplest random structure lead to a singular model."
        )
      )
      return(model)
    } else {
      message(
        paste0(
          "Model for ",
          response_var,
          " is singular using the random structure ",
          random_term,
          ". Reducing random effects structure."
        )
      )
    }
  }
}

tab_models <- function(
  models,
  string.est = "b",
  p.val = "kr",
  p.style = "numeric_stars",
  title = NULL,
  vcov.fun = NULL,
  vcov.args = NULL,
  df.method = "kr"
) {
  tab_model(
    models,
    p.val = p.val,
    title = title,
    string.est = string.est,
    show.ci = F,
    show.stat = T,
    string.stat = "t",
    show.se = T,
    string.se = "SE",
    p.style = p.style,
    vcov.fun = vcov.fun,
    vcov.args = vcov.args,
    df.method = df.method
  )
}

# change class of the ANCOVA models to the given class name, defaulting to 'lmerMod'
change_class <- function(model, class = "lmerMod") {
  m <- model
  class(m) <- class
  return(m)
}

stargazer_models <- function(
  models,
  title = "Titel",
  column.labels = NULL,
  se = NULL,
  type = "html",
  df.method = "Kenward-Roger"
) {
  ddfs <- list(
    "kr" = "Kenward-Roger",
    "kenward-roger" = "Kenward-Roger",
    "satterthwaite" = "Satterthwaite",
    "Satterthwaite" = "Satterthwaite",
    "wald" = "Wald"
  )
  # extract p-values from lmer
  p_values <- lapply(models, function(x) {
    if (class(x)[1] == "lmerModLmerTest" && tolower(df.method) != "wald") {
      return(summary(x, ddf = ddfs[[df.method]])$coefficients[, 5])
    } else if (class(x)[1] == "lm") {
      return(summary(x)$coefficients[, 4])
    }
  })

  models_stargazer = models
  if (class(models[[1]])[1] == "lmerModLmerTest") {
    # change the class of the ANCOVAs to "lmerMod"
    models_stargazer <- lapply(models, change_class, "lmerMod")
  }

  stargazer(
    models_stargazer,
    #omit = c("Constant"),
    column.labels = column.labels,
    digits = 2,
    se = se,
    p = p_values,
    type = type,
    keep.stat = c("n", "rsq", "adj.rsq"),
    no.space = TRUE,
    float = TRUE,
    header = FALSE,
    font.size = "scriptsize",
    star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
    star.char = c("+", "*", "**", "***"),
    notes = c("+ p<.1; * p<.05; ** p<.01; *** p<.001"),
    notes.append = F,
    table.placement = "htpb!",
    intercept.bottom = F,
    report = ("vc*stp"),
    title = title
  )
}

modelsummary_models <- function(
  models,
  outcomes = NULL,
  title = NULL,
  output = "kableExtra",
  ddf = NULL,
  vcov = NULL,
  tidy = NULL,
  coef_rename = NULL
) {
  # create a new list based on the outcomes that contains names(list)=models
  if (!is.null(outcomes)) {
    list_models <- lapply(seq_along(outcomes), function(x) {
      models[[x]]
    })
    names(list_models) <- outcomes
  } else {
    list_models <- lapply(seq_along(models), function(x) {
      models[[x]]
    })
    names(list_models) <- sapply(models, function(x) {
      ifelse("mira" %in% class(x), formula(x[[4]][[1]])[[2]], formula(x)[[2]])
    })
  }
  ms <- modelsummary(
    list_models,
    output = output,
    # estimate = "{estimate}{stars} \n ({std.error}) p={p.value}",
    statistic = c("SE" = "std.error", "t" = "statistic", "p" = "p.value"),
    estimate = c("b" = "{estimate}{stars}"),
    #statistic=NULL,
    shape = term ~ model + statistic,
    title = title,
    fmt = fmt_decimal(digits = 2, pdigits = 3),
    ci_method = ddf,
    coef_rename = coef_rename,
    vcov = vcov,
    tidy = tidy,
    escape = switch(output, "latex" = T, "latex_tabular" = T, F)
  )
  if (output == "kableExtra") {
    return(
      ms %>%
        kable_styling(
          bootstrap_options = c("striped", "condensed", "hover"),
          full_width = T
        )
    )
  } else {
    return(ms)
  }
}

# Convert everything to character, then use type.convert to infer proper base types
clean_df <- function(df) {
  df[] <- lapply(df, function(col) {
    col <- as.character(col) # force avector etc. to become characters
    type.convert(col, as.is = TRUE)
  })
  return(df)
}

check_distances <- function(data, dist = 2) {
  # finding typos in code
  # get and save single instances of code
  single_codes <- data %>%
    count(code) %>%
    filter(n == 1) %>%
    pull(code)
  # Get only the rows where code is in single_codes
  data_single <- data %>%
    filter(code %in% single_codes)

  # Now split the codes based on group
  single_codes_pre <- data_single %>%
    filter(time == "pre") %>%
    pull(code)

  single_codes_post <- data_single %>%
    filter(time == "post") %>%
    pull(code)

  # 1. Compute the distance matrix
  dist_matrix <- stringdistmatrix(
    a = single_codes_pre,
    b = single_codes_post,
    method = "lv"
  )

  # 2. Convert to data frame with row and column names
  dist_df <- as.data.frame(dist_matrix)
  rownames(dist_df) <- single_codes_pre
  colnames(dist_df) <- single_codes_post

  # 3. Extract the index positions where distance == 2
  matches_x <- which(dist_df == dist, arr.ind = TRUE)

  # 4. Create a data frame of those matches
  matches_distance_x <- data.frame(
    pre_code = rownames(dist_df)[matches_x[, "row"]],
    post_code = colnames(dist_df)[matches_x[, "col"]],
    distance = dist
  )

  # 5. View result
  print(matches_distance_x)
}

check_demo_vars <- function(
  data,
  demo_vars = c("age", "semester", "gender", "students"),
  id = "code",
  time_col = "time",
  pre_label = "pre",
  post_label = "post",
  show_only_diff = FALSE
) {
  out <- data %>%
    select(all_of(c(id, time_col, demo_vars))) %>%
    # ensure a common type across demo vars
    mutate(across(all_of(demo_vars), as.character)) %>%
    pivot_longer(
      cols = all_of(demo_vars),
      names_to = "variable",
      values_to = "value"
      # alternatively: values_transform = as.character
      # or: values_ptypes = list(value = character())
    ) %>%
    group_by(.data[[id]], variable, .data[[time_col]]) %>%
    summarise(
      value = paste(unique(na.omit(value)), collapse = ", "),
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = all_of(time_col), values_from = value) %>%
    transmute(
      !!id := .data[[id]],
      variable,
      pre = .data[[pre_label]],
      post = .data[[post_label]],
      match = (pre == post) | (is.na(pre) & is.na(post))
    ) %>%
    arrange(.data[[id]], variable)

  if (show_only_diff) {
    out <- filter(out, is.na(match) | !match)
  }
  print(n = 100, na.omit(out))
}

adjust_table_for_word <- function(ft, width = 0) {
  ft <- flextable::autofit(ft)
  ft <- flextable::set_table_properties(ft, layout = "autofit", width = width)
  ft <- flextable::fontsize(ft, size = 10, part = "header")
  ft <- flextable::fontsize(ft, size = 8, part = "body")
  ft
}

modelsummary_output <- ifelse(
  knitr::is_latex_output(),
  "latex_tabular",
  ifelse(knitr::is_html_output(), "kableExtra", "flextable")
)

#' Prepare individual-level prediction data for comparison plots
#'
#' Transforms raw ANCOVA predictions and pretest data into long format suitable
#' for plotting raw vs. predicted comparisons.
#'
#' @param data_wide_unscaled Data frame with pre/post scores and covariates
#' @param scales Character vector of scale names (e.g., c("TK", "TPK", "TCK", ...))
#' @param labels Named character vector mapping scale names to display labels
#' @param ancovas_unscaled List of fitted ANCOVA models (one per scale)
#'
#' @return Long-format data frame with columns: code, intervention, outcome, time, score
#'
prepare_comparison_data <- function(
  data_wide_unscaled,
  scales,
  labels,
  ancovas_unscaled
) {
  # Step 1: Compute adjusted posttest predictions for all scales
  for (scale in scales) {
    pre_var <- paste0(scale, "_pre")
    adj_var <- paste0(scale, "_adj_post")

    data_wide_unscaled[[adj_var]] <- predict(
      ancovas_unscaled[[scale]],
      data.frame(
        intervention = data_wide_unscaled$intervention,
        pretest = mean(data_wide_unscaled[[pre_var]], na.rm = TRUE),
        gender = data_wide_unscaled$gender,
        semester = data_wide_unscaled$semester
      )
    )
  }

  # Step 2: Calculate pretest mean for each scale
  pretest_means <- tibble(
    scale = scales,
    pretest_mean = map_dbl(
      scales,
      ~ mean(data_wide_unscaled[[paste0(.x, "_pre")]], na.rm = TRUE)
    )
  ) %>%
    mutate(
      outcome = factor(scale, levels = scales, labels = labels)
    ) %>%
    select(outcome, pretest_mean)

  # Step 3: Transform individual subject data to long format
  data_wide_unscaled %>%
    as.data.frame() %>%
    select(
      code,
      intervention,
      all_of(c(
        paste0(scales, "_pre"),
        paste0(scales, "_adj_post")
      ))
    ) %>%
    pivot_longer(
      cols = -c(code, intervention),
      names_to = "name",
      values_to = "score"
    ) %>%
    mutate(
      scale = str_remove(name, "_pre$|_adj_post$"),
      outcome = factor(scale, levels = scales, labels = labels),
      time = factor(
        if_else(str_detect(name, "_pre$"), "pre_mean", "post_adj"),
        levels = c("pre_mean", "post_adj")
      )
    ) %>%
    left_join(pretest_means, by = "outcome") %>%
    mutate(
      score = if_else(time == "pre_mean", pretest_mean, score)
    ) %>%
    select(code, intervention, outcome, time, score)
}

#' Create a faceted outcome plot with standard styling
#'
#' Wraps plot_outcome() + facet_wrap() with consistent theme and coordinate settings.
#'
#' @param data Input data frame with outcome and score columns
#' @param ncol Number of facet columns
#' @param outcome_labels Named character vector for facet labels (if NULL, uses default)
#' @param y_title Y-axis title
#' @param y_zoom Y-axis zoom limits via coord_cartesian (e.g., c(2, 4.5))
#' @param show_legend Logical: show legend at bottom?
#' @param show_strip_labels Logical: show facet strip text?
#' @param show_n_per_facet Logical: append sample size to each facet label?
#' @param plot_title Main plot title
#' @param plot_subtitle Plot subtitle
#' @param error_bars Logical: include error bars from plot_outcome()?
#' @param ... Additional arguments passed to plot_outcome()
#'
#' @return A ggplot object
#'
plot_facet_outcome <- function(
  data,
  ncol = 6,
  outcome_labels = NULL,
  y_title = NULL,
  y_zoom = NULL,
  show_legend = TRUE,
  show_strip_labels = TRUE,
  show_n_per_facet = FALSE,
  plot_title = NULL,
  plot_subtitle = NULL,
  error_bars = FALSE,
  ylab = "Rating"
) {
  # Create base plot (without show_n_per_facet passed to plot_outcome)
  p <- plot_outcome(
    data,
    scales_facet = "free",
    error_bars = error_bars,
    ylab = ylab
  )

  # Calculate n per outcome if requested
  if (show_n_per_facet) {
    n_per_outcome <- data %>%
      filter(!is.na(score)) %>%
      group_by(outcome) %>%
      summarise(n = n_distinct(code), .groups = "drop")

    # Create labels with n on separate line
    if (!is.null(outcome_labels)) {
      # Match outcome levels to labels and add n below
      facet_labels <- set_names(
        paste0(
          outcome_labels[match(n_per_outcome$outcome, names(outcome_labels))],
          "\n(n=",
          n_per_outcome$n,
          ")"
        ),
        n_per_outcome$outcome
      )
    } else {
      facet_labels <- set_names(
        paste0(n_per_outcome$outcome, "\n(n=", n_per_outcome$n, ")"),
        n_per_outcome$outcome
      )
    }
  } else {
    facet_labels <- outcome_labels
  }

  # Build labeller
  if (!is.null(facet_labels)) {
    p <- p +
      facet_wrap(
        ~outcome,
        scales = "free",
        ncol = ncol,
        labeller = labeller(outcome = facet_labels)
      )
  } else {
    p <- p +
      facet_wrap(
        ~outcome,
        scales = "free",
        ncol = ncol
      )
  }

  # Add labs and theme
  p <- p +
    labs(
      title = plot_title,
      subtitle = plot_subtitle,
      y = y_title,
      x = NULL
    )

  # Theme settings
  legend_pos <- if (show_legend) "bottom" else "none"

  # Build theme list conditionally to avoid merging incompatible elements
  theme_list <- list(legend.position = legend_pos)

  if (!show_strip_labels) {
    theme_list$strip.text.x <- element_blank()
  }

  p <- p + do.call(theme, theme_list)

  # Add zoom if specified
  if (!is.null(y_zoom)) {
    p <- p + coord_cartesian(ylim = y_zoom)
  }

  p
}

#' Combine raw and predicted plots vertically with patchwork
#'
#' @param p_raw Raw data plot
#' @param p_pred Predicted/adjusted data plot
#'
#' @return Combined plot via patchwork /
#'
combine_raw_pred_plots <- function(p_raw, p_pred) {
  library(patchwork)
  p_raw / p_pred
}

packages <- c(
  "tidyverse",
  "openxlsx",
  "lavaan",
  "DiagrammeR",
  "htmlwidgets"
)

installed_packages <- rownames(installed.packages())

for (pkg in packages) {
  if (!(pkg %in% installed_packages)) {
    install.packages(pkg)
  }
}

library(tidyverse)
library(openxlsx)
library(lavaan)
library(DiagrammeR)
library(htmlwidgets)

options(timeout = 600)

DEFAULT_INPUT <- "Силосразн.xlsx"

if (file.exists(DEFAULT_INPUT)) {
  input_file <- DEFAULT_INPUT
} else if (interactive()) {
  input_file <- file.choose()
} else {
  stop("Файл Силосразн.xlsx не найден. Положите его в одну папку со скриптом.")
}

df_raw <- read.xlsx(input_file)

clean_names_ru <- function(x) {
  x %>%
    str_replace_all("ё", "е") %>%
    str_replace_all("Ё", "Е") %>%
    str_to_lower() %>%
    str_replace_all("[^a-zа-я0-9]+", "_") %>%
    str_replace_all("^_+|_+$", "")
}

clean_number <- function(x) {
  if (is.numeric(x)) {
    return(x)
  }

  x <- as.character(x)
  x <- str_replace_all(x, ",", ".")
  x <- str_replace_all(x, "\\s+", "")
  x <- str_replace_all(x, "[^0-9\\.\\-]", "")

  suppressWarnings(as.numeric(x))
}

find_col <- function(data_names, include_patterns, exclude_patterns = character(0), required = TRUE) {
  nm <- clean_names_ru(data_names)
  include_hit <- rep(FALSE, length(nm))

  for (p in include_patterns) {
    include_hit <- include_hit | str_detect(nm, p)
  }

  if (length(exclude_patterns) > 0) {
    exclude_hit <- rep(FALSE, length(nm))

    for (p in exclude_patterns) {
      exclude_hit <- exclude_hit | str_detect(nm, p)
    }

    include_hit <- include_hit & !exclude_hit
  }

  found <- data_names[include_hit]

  if (length(found) == 0) {
    if (required) {
      stop(
        paste0(
          "Не найден столбец по шаблонам: ",
          paste(include_patterns, collapse = " | "),
          "\nПроверь названия столбцов в Excel."
        )
      )
    }

    return(NA_character_)
  }

  found[1]
}

impute_median <- function(x) {
  ifelse(is.na(x), median(x, na.rm = TRUE), x)
}

names(df_raw) <- clean_names_ru(names(df_raw))

col_type <- find_col(
  names(df_raw),
  c("тип.*сил", "вид.*сил", "группа.*сил", "культура"),
  required = FALSE
)

col_moisture <- find_col(
  names(df_raw),
  c("влаж", "влага"),
  required = FALSE
)

col_sv_direct <- find_col(
  names(df_raw),
  c("сух.*веществ", "^св$", "св_"),
  required = FALSE
)

col_sugar <- find_col(
  names(df_raw),
  c("сахар"),
  required = TRUE
)

col_crude_protein <- find_col(
  names(df_raw),
  c("сыр.*протеин", "^сп$", "протеин"),
  exclude_patterns = c("перевар"),
  required = TRUE
)

col_digestible_protein <- find_col(
  names(df_raw),
  c("перевар.*протеин", "^пп$"),
  required = TRUE
)

col_crude_fiber <- find_col(
  names(df_raw),
  c("сыр.*клетчат", "^ск$", "клетчат"),
  required = TRUE
)

col_ph <- find_col(
  names(df_raw),
  c("ph", "p_h", "рн", "кислот"),
  required = TRUE
)

col_lactic <- find_col(
  names(df_raw),
  c("молоч.*кисл"),
  exclude_patterns = c("доля", "относ", "соотнош"),
  required = TRUE
)

col_acetic <- find_col(
  names(df_raw),
  c("уксус.*кисл"),
  exclude_patterns = c("доля", "относ", "соотнош"),
  required = TRUE
)

col_butyric <- find_col(
  names(df_raw),
  c("маслян.*кисл"),
  exclude_patterns = c("доля", "относ", "соотнош"),
  required = TRUE
)

col_oe <- find_col(
  names(df_raw),
  c("обмен.*энерг", "^оэ$", "оэ_", "oe"),
  required = TRUE
)

col_ke <- find_col(
  names(df_raw),
  c("корм.*ед", "^ке$", "ке_", "ke"),
  required = TRUE
)

col_quality <- find_col(
  names(df_raw),
  c("качество", "оценка"),
  required = TRUE
)

df <- df_raw %>%
  transmute(
    type_silos = if (!is.na(col_type)) as.character(.data[[col_type]]) else NA_character_,
    moisture = if (!is.na(col_moisture)) clean_number(.data[[col_moisture]]) else NA_real_,
    sv_direct = if (!is.na(col_sv_direct)) clean_number(.data[[col_sv_direct]]) else NA_real_,
    sugar = clean_number(.data[[col_sugar]]),
    crude_protein = clean_number(.data[[col_crude_protein]]),
    digestible_protein = clean_number(.data[[col_digestible_protein]]),
    crude_fiber = clean_number(.data[[col_crude_fiber]]),
    ph = clean_number(.data[[col_ph]]),
    lactic = clean_number(.data[[col_lactic]]),
    acetic = clean_number(.data[[col_acetic]]),
    butyric = clean_number(.data[[col_butyric]]),
    oe = clean_number(.data[[col_oe]]),
    ke = clean_number(.data[[col_ke]]),
    quality = clean_number(.data[[col_quality]])
  ) %>%
  mutate(
    sv = case_when(
      !is.na(sv_direct) ~ sv_direct,
      is.na(sv_direct) & !is.na(moisture) ~ 100 - moisture,
      TRUE ~ NA_real_
    )
  ) %>%
  select(
    type_silos,
    sv, sugar, crude_protein, crude_fiber,
    lactic, acetic, butyric, ph,
    digestible_protein, oe, ke,
    quality
  ) %>%
  filter(!is.na(quality))

model_vars <- c(
  "sv", "sugar", "crude_protein", "crude_fiber",
  "lactic", "acetic", "butyric", "ph",
  "digestible_protein", "oe", "ke",
  "quality"
)

df <- df %>%
  mutate(
    across(
      all_of(model_vars),
      impute_median
    )
  )

model_dag <- '
  lactic  ~ sugar
  acetic  ~ sugar
  butyric ~ sv
  ph      ~ sv + lactic + acetic

  digestible_protein ~ crude_protein + crude_fiber
  oe                 ~ crude_protein + crude_fiber + ph
  ke                 ~ oe

  quality ~ ph + butyric + digestible_protein + oe + ke

  sv ~~ sugar + crude_protein + crude_fiber
  sugar ~~ crude_protein + crude_fiber
  crude_protein ~~ crude_fiber
'

fit_dag <- sem(
  model_dag,
  data = df,
  meanstructure = TRUE,
  estimator = "MLR"
)

coef_sem <- parameterEstimates(
  fit_dag,
  standardized = TRUE
) %>%
  filter(op == "~") %>%
  mutate(std_label = sprintf("%.2f", std.all))

get_coef <- function(lhs_name, rhs_name) {
  x <- coef_sem %>%
    filter(lhs == lhs_name, rhs == rhs_name) %>%
    pull(std_label)

  if (length(x) == 0) {
    return("")
  }

  x[1]
}

c_sugar_lactic <- get_coef("lactic", "sugar")
c_sugar_acetic <- get_coef("acetic", "sugar")

c_sv_butyric <- get_coef("butyric", "sv")
c_sv_ph <- get_coef("ph", "sv")

c_lactic_ph <- get_coef("ph", "lactic")
c_acetic_ph <- get_coef("ph", "acetic")

c_protein_dp <- get_coef("digestible_protein", "crude_protein")
c_fiber_dp <- get_coef("digestible_protein", "crude_fiber")

c_protein_oe <- get_coef("oe", "crude_protein")
c_fiber_oe <- get_coef("oe", "crude_fiber")
c_ph_oe <- get_coef("oe", "ph")

c_oe_ke <- get_coef("ke", "oe")

c_ph_quality <- get_coef("quality", "ph")
c_butyric_quality <- get_coef("quality", "butyric")
c_dp_quality <- get_coef("quality", "digestible_protein")
c_oe_quality <- get_coef("quality", "oe")
c_ke_quality <- get_coef("quality", "ke")

dot_code <- paste0("
digraph sem_silage {

  graph [
    charset = 'UTF-8',
    layout = dot,
    rankdir = LR,
    splines = ortho,
    overlap = false,
    concentrate = false,
    nodesep = '0.65',
    ranksep = '1.00',
    bgcolor = white,
    compound = true,
    labelloc = t,
    label = 'Причинно-следственный граф факторов качества консервированных кормов\\nСтрелки отражают предполагаемое направление влияния между наблюдаемыми переменными',
    fontsize = 20,
    fontname = 'Times New Roman'
  ];

  node [
    shape = box,
    style = 'rounded',
    color = black,
    penwidth = '1.2',
    fontname = 'Times New Roman',
    fontsize = 14,
    margin = '0.18,0.10'
  ];

  edge [
    color = black,
    penwidth = '1.2',
    arrowsize = '0.8',
    fontname = 'Times New Roman',
    fontsize = 11
  ];

  subgraph cluster_composition {
    label = 'Состав корма';
    color = gray50;
    style = dotted;
    penwidth = '1.0';
    fontname = 'Times New Roman';
    fontsize = 15;

    sv      [label = 'СВ\\n(сухое вещество)'];
    sugar   [label = 'Сахар'];
    protein [label = 'Сырой\\nпротеин'];
    fiber   [label = 'Сырая\\nклетчатка'];
  }

  subgraph cluster_fermentation {
    label = 'Брожение';
    color = gray50;
    style = dotted;
    penwidth = '1.0';
    fontname = 'Times New Roman';
    fontsize = 15;

    acetic  [label = 'Уксусная\\nкислота'];
    lactic  [label = 'Молочная\\nкислота'];
    butyric [label = 'Масляная\\nкислота'];
    ph      [label = 'pH'];
  }

  subgraph cluster_nutrition {
    label = 'Питательность';
    color = gray50;
    style = dotted;
    penwidth = '1.0';
    fontname = 'Times New Roman';
    fontsize = 15;

    dp [label = 'Переваримый\\nпротеин'];
    oe [label = 'Обменная\\nэнергия'];
    ke [label = 'Кормовые\\nединицы'];
  }

  subgraph cluster_quality {
    label = 'Качество';
    color = gray50;
    style = dotted;
    penwidth = '1.0';
    fontname = 'Times New Roman';
    fontsize = 15;

    quality [label = 'Оценка\\nкачества'];
  }

  sv -> lactic [style = invis, weight = 10];
  lactic -> dp [style = invis, weight = 10];
  dp -> quality [style = invis, weight = 10];

  sugar -> lactic [xlabel = '", c_sugar_lactic, "'];
  sugar -> acetic [xlabel = '", c_sugar_acetic, "'];

  sv -> butyric [xlabel = '", c_sv_butyric, "'];
  sv -> ph [xlabel = '", c_sv_ph, "'];

  lactic -> ph [xlabel = '", c_lactic_ph, "'];
  acetic -> ph [xlabel = '", c_acetic_ph, "'];

  protein -> dp [xlabel = '", c_protein_dp, "'];
  fiber -> dp [xlabel = '", c_fiber_dp, "'];

  protein -> oe [xlabel = '", c_protein_oe, "'];
  fiber -> oe [xlabel = '", c_fiber_oe, "'];
  ph -> oe [xlabel = '", c_ph_oe, "'];

  oe -> ke [xlabel = '", c_oe_ke, "'];

  ph -> quality [xlabel = '", c_ph_quality, "'];
  butyric -> quality [xlabel = '", c_butyric_quality, "'];
  dp -> quality [xlabel = '", c_dp_quality, "'];
  oe -> quality [xlabel = '", c_oe_quality, "'];
  ke -> quality [xlabel = '", c_ke_quality, "'];
}
")

graph_sem_numbers <- grViz(dot_code)

output_dir <- "sem_graph_output"

if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

saveWidget(
  graph_sem_numbers,
  file = file.path(output_dir, "sem_graph_with_coefficients.html"),
  selfcontained = TRUE
)

print(summary(fit_dag, standardized = TRUE, rsquare = TRUE, fit.measures = TRUE))
print(coef_sem)

graph_sem_numbers

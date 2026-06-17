# Информационная система оценки качества консервированных кормов растительного происхождения
# © 2026 Болотова Анастасия
# Назначение: автоматизация анализа лабораторных данных о качестве консервированных кормов

packages <- c(
  "shiny", "openxlsx", "dplyr", "stringr", "tibble",
  "lavaan", "DiagrammeR", "DT", "httr", "jsonlite"
)

installed_packages <- rownames(installed.packages())
for (pkg in packages) {
  if (!(pkg %in% installed_packages)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

library(shiny)
library(openxlsx)
library(dplyr)
library(stringr)
library(tibble)
library(lavaan)
library(DiagrammeR)
library(DT)
library(httr)
library(jsonlite)

options(timeout = 600)

variable_labels <- c(
  sv = "Сухое вещество",
  sugar = "Сахар",
  crude_protein = "Сырой протеин",
  crude_fiber = "Сырая клетчатка",
  lactic = "Молочная кислота",
  acetic = "Уксусная кислота",
  butyric = "Масляная кислота",
  ph = "pH",
  digestible_protein = "Переваримый протеин",
  oe = "Обменная энергия",
  ke = "Кормовые единицы",
  quality = "Оценка качества"
)

model_vars <- names(variable_labels)

clean_names_ru <- function(x) {
  x %>%
    str_replace_all("ё", "е") %>%
    str_replace_all("Ё", "Е") %>%
    str_to_lower() %>%
    str_replace_all("[^a-zа-я0-9]+", "_") %>%
    str_replace_all("^_+|_+$", "")
}

clean_number <- function(x) {
  if (is.numeric(x)) return(x)
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
          "Не найден обязательный столбец по шаблонам: ",
          paste(include_patterns, collapse = " | "),
          ". Проверь названия столбцов в Excel."
        ),
        call. = FALSE
      )
    }
    return(NA_character_)
  }

  found[1]
}

impute_median <- function(x) {
  med <- median(x, na.rm = TRUE)
  if (is.na(med)) med <- 0
  x[is.na(x)] <- med
  x
}

prepare_data <- function(path) {
  df_raw <- read.xlsx(path)
  original_names <- names(df_raw)
  names(df_raw) <- clean_names_ru(names(df_raw))

  col_type <- find_col(names(df_raw), c("тип.*сил", "вид.*сил", "группа.*сил", "культура"), required = FALSE)
  col_moisture <- find_col(names(df_raw), c("влаж", "влага"), required = FALSE)
  col_sv_direct <- find_col(names(df_raw), c("сух.*веществ", "^св$", "^св_"), required = FALSE)
  col_sugar <- find_col(names(df_raw), c("сахар"), required = TRUE)
  col_crude_protein <- find_col(names(df_raw), c("сыр.*протеин", "^сп$", "протеин"), exclude_patterns = c("перевар"), required = TRUE)
  col_digestible_protein <- find_col(names(df_raw), c("перевар.*протеин", "^пп$"), required = TRUE)
  col_crude_fiber <- find_col(names(df_raw), c("сыр.*клетчат", "^ск$", "клетчат"), required = TRUE)
  col_ph <- find_col(names(df_raw), c("^ph$", "ph", "p_h", "рн"), required = TRUE)
  col_lactic <- find_col(names(df_raw), c("молоч.*кисл"), exclude_patterns = c("доля", "относ", "соотнош"), required = TRUE)
  col_acetic <- find_col(names(df_raw), c("уксус.*кисл"), exclude_patterns = c("доля", "относ", "соотнош"), required = TRUE)
  col_butyric <- find_col(names(df_raw), c("маслян.*кисл"), exclude_patterns = c("доля", "относ", "соотнош"), required = TRUE)
  col_oe <- find_col(names(df_raw), c("обмен.*энерг", "^оэ$", "оэ_", "oe"), required = TRUE)
  col_ke <- find_col(names(df_raw), c("корм.*ед", "^ке$", "ке_", "ke"), required = TRUE)
  col_quality <- find_col(names(df_raw), c("качество", "оценка"), required = TRUE)

  df <- df_raw %>%
    transmute(
      type_silos = if (!is.na(col_type)) as.character(.data[[col_type]]) else NA_character_,
      moisture = if (!is.na(col_moisture)) clean_number(.data[[col_moisture]]) else NA_real_,
      sv_direct = if (!is.na(col_sv_direct)) clean_number(.data[[col_sv_direct]]) else NA_real_,
      sugar = clean_number(.data[[col_sugar]]),
      crude_protein = clean_number(.data[[col_crude_protein]]),
      crude_fiber = clean_number(.data[[col_crude_fiber]]),
      lactic = clean_number(.data[[col_lactic]]),
      acetic = clean_number(.data[[col_acetic]]),
      butyric = clean_number(.data[[col_butyric]]),
      ph = clean_number(.data[[col_ph]]),
      digestible_protein = clean_number(.data[[col_digestible_protein]]),
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
    select(type_silos, all_of(model_vars))

  n_before <- nrow(df)
  missing_before <- colSums(is.na(df[model_vars]))

  df <- df %>% filter(!is.na(quality))
  n_after_quality_filter <- nrow(df)

  df <- df %>% mutate(across(all_of(model_vars), impute_median))
  missing_after <- colSums(is.na(df[model_vars]))

  sv_source_label <- if (!is.na(col_sv_direct)) {
    col_sv_direct
  } else if (!is.na(col_moisture)) {
    paste0("рассчитано как 100 - ", col_moisture)
  } else {
    "не найдено"
  }

  col_map <- tibble(
    Переменная = c(
      "Тип силоса", "Влажность", "Сухое вещество", "Сахар",
      "Сырой протеин", "Переваримый протеин", "Сырая клетчатка",
      "pH", "Молочная кислота", "Уксусная кислота", "Масляная кислота",
      "Обменная энергия", "Кормовые единицы", "Оценка качества"
    ),
    Найденный_столбец = c(
      col_type, col_moisture, sv_source_label, col_sugar,
      col_crude_protein, col_digestible_protein, col_crude_fiber,
      col_ph, col_lactic, col_acetic, col_butyric,
      col_oe, col_ke, col_quality
    )
  )

  list(
    raw_names = original_names,
    data = df,
    col_map = col_map,
    n_before = n_before,
    n_after_quality_filter = n_after_quality_filter,
    missing_before = missing_before,
    missing_after = missing_after
  )
}

desc_stats <- function(df) {
  tibble(
    Показатель = unname(variable_labels[model_vars]),
    Переменная = model_vars,
    Среднее = sapply(df[model_vars], mean),
    Медиана = sapply(df[model_vars], median),
    Ст_откл = sapply(df[model_vars], sd),
    Минимум = sapply(df[model_vars], min),
    Максимум = sapply(df[model_vars], max)
  ) %>% mutate(across(where(is.numeric), ~ round(.x, 3)))
}

corr_table <- function(df) {
  corr <- round(cor(df[model_vars], method = "spearman"), 3)
  out <- as.data.frame(corr)
  out <- tibble::rownames_to_column(out, "Показатель")
  out$Показатель <- unname(variable_labels[out$Показатель])
  names(out)[-1] <- unname(variable_labels[names(out)[-1]])
  out
}

fit_sem_model <- function(df) {
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

  sem(
    model_dag,
    data = df,
    meanstructure = TRUE,
    estimator = "MLR"
  )
}

sem_coefficients <- function(fit) {
  parameterEstimates(fit, standardized = TRUE) %>%
    filter(op == "~") %>%
    transmute(
      Зависимая = lhs,
      Предиктор = rhs,
      Коэффициент = round(est, 4),
      SE = round(se, 4),
      z = round(z, 4),
      p_value = signif(pvalue, 4),
      beta = round(std.all, 4)
    )
}

r2_table <- function(fit) {
  r2 <- inspect(fit, "r2")
  tibble(
    Переменная = names(r2),
    Показатель = unname(variable_labels[names(r2)]),
    R2 = round(as.numeric(r2), 4)
  )
}

fit_indices_table <- function(fit) {
  indices <- c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr", "aic", "bic")
  vals <- fitMeasures(fit, indices)
  tibble(
    Индекс = names(vals),
    Значение = round(as.numeric(vals), 4)
  )
}

make_conclusion <- function(fit) {
  coefs <- sem_coefficients(fit)
  qcoef <- coefs %>% filter(Зависимая == "quality")

  if (nrow(qcoef) == 0 || all(is.na(qcoef$beta))) {
    return("Не удалось определить непосредственные факторы экспертной оценки качества.")
  }

  idx <- which.max(abs(qcoef$beta))
  main_factor <- qcoef$Предиктор[idx]
  main_beta <- qcoef$beta[idx]
  main_p <- qcoef$p_value[idx]
  main_label <- ifelse(
    main_factor %in% names(variable_labels),
    variable_labels[[main_factor]],
    main_factor
  )

  direction <- ifelse(
    main_beta < 0,
    "связано со снижением числовой оценки качества, то есть с улучшением качества корма",
    "связано с увеличением числовой оценки качества, то есть с ухудшением качества корма"
  )

  sig_quality <- qcoef %>% filter(!is.na(p_value), p_value < 0.05)
  borderline_quality <- qcoef %>% filter(!is.na(p_value), p_value >= 0.05, p_value < 0.10)
  non_sig_quality <- qcoef %>% filter(is.na(p_value) | p_value >= 0.05)

  sig_text <- if (nrow(sig_quality) > 0) {
    paste(
      apply(sig_quality, 1, function(row) {
        pred <- row[["Предиктор"]]
        label <- ifelse(pred %in% names(variable_labels), variable_labels[[pred]], pred)
        paste0("«", label, "» (β = ", round(as.numeric(row[["beta"]]), 3), ", p = ", signif(as.numeric(row[["p_value"]]), 3), ")")
      }),
      collapse = "; "
    )
  } else {
    "устойчивые статистически значимые непосредственные предикторы не выявлены"
  }

  border_text <- if (nrow(borderline_quality) > 0) {
    paste(
      apply(borderline_quality, 1, function(row) {
        pred <- row[["Предиктор"]]
        label <- ifelse(pred %in% names(variable_labels), variable_labels[[pred]], pred)
        paste0("«", label, "» (β = ", round(as.numeric(row[["beta"]]), 3), ", p = ", signif(as.numeric(row[["p_value"]]), 3), ")")
      }),
      collapse = "; "
    )
  } else {
    "пограничные непосредственные связи не выявлены"
  }

  r2 <- inspect(fit, "r2")
  r2_quality <- if ("quality" %in% names(r2)) round(as.numeric(r2[["quality"]]), 4) else NA_real_
  fm <- fitMeasures(fit, c("cfi", "tli", "rmsea", "srmr"))

  paste0(
    "По результатам SEM/path-модели наибольшую непосредственную связь с экспертной оценкой качества имеет показатель «",
    main_label, "» (β = ", round(main_beta, 3), ", p = ", signif(main_p, 3), "). Увеличение данного показателя ",
    direction, ". Поскольку большее значение переменной «Оценка качества» соответствует худшему качеству, знак коэффициента интерпретируется в обратной логике.

",

    "К статистически значимым непосредственным предикторам оценки качества относятся: ", sig_text, ". Пограничные связи: ", border_text,
    ". Остальные непосредственные предикторы не достигли уровня статистической значимости p < 0,05, поэтому их следует рассматривать как направления связи, а не как устойчиво подтверждённые эффекты.

",

    "Коэффициент детерминации для переменной «Оценка качества» составляет R² = ", r2_quality,
    ", то есть модель объясняет около ", round(r2_quality * 100, 1), "% вариации экспертной оценки качества. Это можно рассматривать как умеренную объясняющую способность для наблюдательных лабораторных данных.

",

    "Индексы качества модели составили: CFI = ", round(fm[["cfi"]], 3),
    ", TLI = ", round(fm[["tli"]], 3),
    ", RMSEA = ", round(fm[["rmsea"]], 3),
    ", SRMR = ", round(fm[["srmr"]], 3),
    ". Эти значения показывают, что модель целесообразно использовать как разведочную структурную модель, предназначенную для интерпретации отдельных связей. Полученные результаты следует рассматривать как автоматизированную интерпретацию наблюдательных данных, а не как окончательное доказательство причинности."
  )
}


make_table_text <- function(x, max_rows = 12) {
  x <- as.data.frame(x)
  if (nrow(x) > max_rows) {
    x <- x[seq_len(max_rows), , drop = FALSE]
  }
  paste(capture.output(print(x, row.names = FALSE)), collapse = "\n")
}

make_llm_prompt <- function(analysis_result) {
  coef <- analysis_result$coef %>%
    mutate(
      Зависимая_показатель = ifelse(
        Зависимая %in% names(variable_labels),
        unname(variable_labels[Зависимая]),
        Зависимая
      ),
      Предиктор_показатель = ifelse(
        Предиктор %in% names(variable_labels),
        unname(variable_labels[Предиктор]),
        Предиктор
      ),
      Значимость = case_when(
        is.na(p_value) ~ "нет p-value",
        p_value < 0.001 ~ "статистически значимо, p < 0.001",
        p_value < 0.05 ~ "статистически значимо, p < 0.05",
        p_value < 0.10 ~ "тенденция, 0.05 <= p < 0.10",
        TRUE ~ "не статистически значимо, p >= 0.05"
      ),
      Интерпретация_для_качества = case_when(
        Зависимая == "quality" & beta < 0 ~ "отрицательная связь: при росте показателя числовая оценка качества снижается, то есть качество улучшается",
        Зависимая == "quality" & beta > 0 ~ "положительная связь: при росте показателя числовая оценка качества повышается, то есть качество ухудшается",
        TRUE ~ "промежуточный путь модели"
      )
    ) %>%
    select(
      Зависимая_показатель, Предиктор_показатель,
      Коэффициент, p_value, beta, Значимость, Интерпретация_для_качества
    )

  quality_coef <- coef %>%
    filter(Зависимая_показатель == "Оценка качества") %>%
    arrange(desc(abs(beta)))

  significant_quality <- quality_coef %>%
    filter(!is.na(p_value), p_value < 0.05)

  tendency_quality <- quality_coef %>%
    filter(!is.na(p_value), p_value >= 0.05, p_value < 0.10)

  not_significant_quality <- quality_coef %>%
    filter(is.na(p_value) | p_value >= 0.05)

  quality_r2 <- analysis_result$r2 %>%
    filter(Переменная == "quality") %>%
    pull(R2)

  quality_r2_text <- if (length(quality_r2) > 0) {
    as.character(quality_r2[1])
  } else {
    "нет данных"
  }

  prompt <- paste0(
    "Напиши аналитическое заключение на русском языке для информационной системы оценки качества консервированных кормов растительного происхождения.\n\n",

    "КРИТИЧЕСКИЕ ПРАВИЛА, которые нельзя нарушать:\n",
    "1. Не используй английские слова, служебные заголовки Draft, Outline, Explanatory power и Markdown-разметку.\n",
    "2. Не упоминай диплом, дипломную работу, студента, преподавателя или учебное исследование. Текст должен выглядеть как вывод промышленного/аналитического приложения.\n",
    "3. Используй корректные термины: SEM/path-модель, структурная модель или модель структурных уравнений. Не пиши 'структурно-уравнительный' и 'структурно-равностный'.\n",
    "4. Не называй показатель статистически значимым, если p_value >= 0.05. Если 0.05 <= p_value < 0.10, пиши только 'тенденция' или 'пограничная значимость'.\n",
    "5. Не используй слово 'коррелирует' для SEM-коэффициентов. Пиши 'имеет направление связи', 'коэффициент пути', 'стандартизированный коэффициент'.\n",
    "6. Не используй жёсткие причинные формулировки типа 'влияет', 'воздействует', 'приводит'. Данные наблюдательные, поэтому пиши осторожно: 'связано', 'может быть связано', 'указывает на тенденцию'.\n",
    "7. Не придумывай численные значения. Используй только коэффициенты, p_value, beta, R2 и индексы, переданные ниже.\n",
    "8. Обязательно учитывай шкалу качества: большее числовое значение переменной 'Оценка качества' означает худшее качество корма. Поэтому отрицательная связь с оценкой качества означает улучшение качества, а положительная связь означает ухудшение качества.\n\n",

    "Желаемый формат ответа:\n",
    "- 5–7 связных абзацев;\n",
    "- научный, но понятный стиль;\n",
    "- объём примерно 3500–5000 знаков;\n",
    "- последнее предложение должно быть полностью завершено;\n",
    "- без списков, без заголовков, без Markdown;\n",
    "- один раз в начале укажи, что интерпретация выполнена по уже рассчитанным коэффициентам и индексам модели; дальше не повторяй эту мысль.\n\n",

    "Логика заключения:\n",
    "1. Сначала кратко опиши, что выполнен автоматизированный SEM/path-анализ, а вывод основан на уже рассчитанных коэффициентах, p-value, R2 и индексах модели.\n",
    "2. Затем опиши только статистически значимые непосредственные предикторы экспертной оценки качества.\n",
    "3. Отдельно опиши показатели с пограничной значимостью как тенденции, если они есть.\n",
    "4. Отдельно упомяни показатели с сильными beta, но p_value >= 0.05: их нельзя называть значимыми, можно только сказать, что направление связи содержательно интерпретируемо, но не устойчиво статистически.\n",
    "5. Интерпретируй R2 целевой переменной.\n",
    "6. Укажи ограничения: наблюдательный характер данных, разведочная модель, отсутствие технологических факторов хранения и заготовки.\n",
    "7. Заверши сухим практическим выводом о том, какие показатели требуют внимания при оценке качества корма. Не упоминай диплом, исследование студента или учебную работу.\n\n",

    "Автоматическое заключение без LLM:\n",
    analysis_result$conclusion, "\n\n",

    "ВСЕ коэффициенты путей SEM/path-модели:\n",
    make_table_text(coef, max_rows = 40), "\n\n",

    "Непосредственные предикторы экспертной оценки качества, отсортированные по модулю beta:\n",
    make_table_text(quality_coef, max_rows = 10), "\n\n",

    "Статистически значимые непосредственные предикторы экспертной оценки качества, p < 0.05:\n",
    make_table_text(significant_quality, max_rows = 10), "\n\n",

    "Пограничные непосредственные предикторы экспертной оценки качества, 0.05 <= p < 0.10:\n",
    make_table_text(tendency_quality, max_rows = 10), "\n\n",

    "Незначимые непосредственные предикторы экспертной оценки качества, p >= 0.05:\n",
    make_table_text(not_significant_quality, max_rows = 10), "\n\n",

    "R2 целевой переменной 'Оценка качества': ", quality_r2_text, "\n\n",

    "Коэффициенты детерминации R2:\n",
    make_table_text(analysis_result$r2, max_rows = 20), "\n\n",

    "Индексы качества модели:\n",
    make_table_text(analysis_result$fit_indices, max_rows = 20), "\n\n",

    "Верни только готовый русский текст заключения."
  )

  prompt
}

call_openrouter_llm <- function(prompt, api_key, model = "openrouter/free") {
  if (is.null(api_key) || !nzchar(api_key)) {
    stop("Не указан OpenRouter API key.", call. = FALSE)
  }

  if (is.null(model) || !nzchar(model)) {
    model <- "openrouter/free"
  }

  model <- trimws(model)

  if (!(model == "openrouter/free" || grepl(":free$", model))) {
    stop(
      "Для демонстрации разрешены только бесплатные модели: openrouter/free или модель с окончанием :free.",
      call. = FALSE
    )
  }

  body <- list(
    model = model,
    messages = list(
      list(
        role = "system",
        content = paste(
          "Ты статистический интерпретатор для информационной системы оценки качества консервированных кормов.",
          "Пиши только по-русски, без английских служебных слов и без Markdown.",
          "Интерпретируй только уже рассчитанные таблицы и не добавляй внешние данные.",
          "Нельзя придумывать числа или делать выводы, не подтверждённые переданными результатами.",
          "Запрещено называть коэффициент статистически значимым при p_value >= 0.05.",
          "Для наблюдательных данных избегай жёстких причинных формулировок.",
          "Не упоминай диплом, учебную работу, студента или преподавателя."
        )
      ),
      list(
        role = "user",
        content = prompt
      )
    ),
    temperature = 0.1,
    max_tokens = 4500
  )

  response <- httr::POST(
    url = "https://openrouter.ai/api/v1/chat/completions",
    httr::add_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json",
      `HTTP-Referer` = "http://localhost",
      `X-Title` = "Silage quality Shiny app"
    ),
    body = body,
    encode = "json"
  )

  response_text <- httr::content(response, as = "text", encoding = "UTF-8")

  if (httr::status_code(response) >= 300) {
    stop(
      paste0(
        "Ошибка LLM-запроса. HTTP ", httr::status_code(response), ": ",
        response_text
      ),
      call. = FALSE
    )
  }

  parsed <- jsonlite::fromJSON(response_text, simplifyVector = FALSE)
  out <- parsed$choices[[1]]$message$content
  finish_reason <- parsed$choices[[1]]$finish_reason

  if (is.null(out) || !nzchar(out)) {
    stop("LLM не вернула текст заключения.", call. = FALSE)
  }

  if (!is.null(finish_reason) && finish_reason == "length") {
    out <- paste0(
      out,
      "\n\n[Предупреждение: ответ был обрезан моделью из-за ограничения длины. Выберите другую бесплатную модель с окончанием :free или увеличьте max_tokens в функции call_openrouter_llm().]"
    )
  }

  out
}

build_sem_graph <- function(fit) {
  coef_sem <- parameterEstimates(fit, standardized = TRUE) %>%
    filter(op == "~") %>%
    mutate(std_label = sprintf("%.2f", std.all))

  get_coef <- function(lhs_name, rhs_name) {
    x <- coef_sem %>% filter(lhs == lhs_name, rhs == rhs_name) %>% pull(std_label)
    if (length(x) == 0) return("")
    x[1]
  }

  DiagrammeR::grViz(paste0("
  digraph sem_silage_app {
    graph [
      charset = 'UTF-8', layout = dot, rankdir = LR,
      splines = ortho, overlap = false, concentrate = false,
      nodesep = '0.60', ranksep = '0.90', bgcolor = white,
      labelloc = t,
      label = 'SEM-модель факторов качества консервированных кормов\\nна стрелках указаны стандартизированные коэффициенты',
      fontsize = 18, fontname = 'Times New Roman'
    ];

    node [
      shape = box, style = 'rounded', color = black,
      penwidth = '1.2', fontname = 'Times New Roman',
      fontsize = 13, margin = '0.16,0.09'
    ];

    edge [
      color = black, penwidth = '1.2', arrowsize = '0.8',
      fontname = 'Times New Roman', fontsize = 11
    ];

    subgraph cluster_composition {
      label = 'Состав корма'; color = gray50; style = dotted;
      sv [label = 'СВ'];
      sugar [label = 'Сахар'];
      protein [label = 'Сырой\\nпротеин'];
      fiber [label = 'Сырая\\nклетчатка'];
    }

    subgraph cluster_fermentation {
      label = 'Брожение'; color = gray50; style = dotted;
      lactic [label = 'Молочная\\nкислота'];
      acetic [label = 'Уксусная\\nкислота'];
      butyric [label = 'Масляная\\nкислота'];
      ph [label = 'pH'];
    }

    subgraph cluster_nutrition {
      label = 'Питательность'; color = gray50; style = dotted;
      dp [label = 'Переваримый\\nпротеин'];
      oe [label = 'Обменная\\nэнергия'];
      ke [label = 'Кормовые\\nединицы'];
    }

    subgraph cluster_quality {
      label = 'Качество'; color = gray50; style = dotted;
      quality [label = 'Оценка\\nкачества'];
    }

    sv -> lactic [style = invis, weight = 10];
    lactic -> dp [style = invis, weight = 10];
    dp -> quality [style = invis, weight = 10];

    sugar -> lactic [xlabel = '", get_coef("lactic", "sugar"), "'];
    sugar -> acetic [xlabel = '", get_coef("acetic", "sugar"), "'];
    sv -> butyric [xlabel = '", get_coef("butyric", "sv"), "'];
    sv -> ph [xlabel = '", get_coef("ph", "sv"), "'];
    lactic -> ph [xlabel = '", get_coef("ph", "lactic"), "'];
    acetic -> ph [xlabel = '", get_coef("ph", "acetic"), "'];
    protein -> dp [xlabel = '", get_coef("digestible_protein", "crude_protein"), "'];
    fiber -> dp [xlabel = '", get_coef("digestible_protein", "crude_fiber"), "'];
    protein -> oe [xlabel = '", get_coef("oe", "crude_protein"), "'];
    fiber -> oe [xlabel = '", get_coef("oe", "crude_fiber"), "'];
    ph -> oe [xlabel = '", get_coef("oe", "ph"), "'];
    oe -> ke [xlabel = '", get_coef("ke", "oe"), "'];
    ph -> quality [xlabel = '", get_coef("quality", "ph"), "'];
    butyric -> quality [xlabel = '", get_coef("quality", "butyric"), "'];
    dp -> quality [xlabel = '", get_coef("quality", "digestible_protein"), "'];
    oe -> quality [xlabel = '", get_coef("quality", "oe"), "'];
    ke -> quality [xlabel = '", get_coef("quality", "ke"), "'];
  }
  "))
}

make_report_workbook <- function(analysis_result, file) {
  wb <- createWorkbook()

  addWorksheet(wb, "Проверка данных")
  writeData(wb, "Проверка данных", analysis_result$check)

  addWorksheet(wb, "Столбцы")
  writeData(wb, "Столбцы", analysis_result$col_map)

  addWorksheet(wb, "Описание")
  writeData(wb, "Описание", analysis_result$desc)

  addWorksheet(wb, "Корреляции")
  writeData(wb, "Корреляции", analysis_result$corr)

  addWorksheet(wb, "SEM коэффициенты")
  writeData(wb, "SEM коэффициенты", analysis_result$coef)

  addWorksheet(wb, "R2")
  writeData(wb, "R2", analysis_result$r2)

  addWorksheet(wb, "Индексы модели")
  writeData(wb, "Индексы модели", analysis_result$fit_indices)

  addWorksheet(wb, "Заключение")
  writeData(wb, "Заключение", data.frame(Заключение = analysis_result$conclusion))

  saveWorkbook(wb, file, overwrite = TRUE)
}

ui <- fluidPage(
  titlePanel("Информационная система оценки качества консервированных кормов"),

  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Загрузить Excel-файл с данными (.xlsx)", accept = ".xlsx"),
      actionButton("run", "Запустить анализ", class = "btn-primary"),
      br(), br(),
      tags$hr(),
      downloadButton("download_report", "Скачать Excel-отчёт"),
      br(), br(),
      helpText("Файл должен иметь структуру, близкую к исходной базе: показатели состава, брожения, питательности и экспертная оценка качества."),
      tags$hr(),
      tags$small("© 2026 Болотова Анастасия")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Проверка данных", br(), DTOutput("check"), br(), DTOutput("col_map")),
        tabPanel("Описание", br(), DTOutput("desc")),
        tabPanel("Корреляции", br(), DTOutput("corr")),
        tabPanel("SEM коэффициенты", br(), DTOutput("coef")),
        tabPanel("R2", br(), DTOutput("r2")),
        tabPanel("Индексы модели", br(), DTOutput("fit_indices")),
        tabPanel("SEM-граф", br(), grVizOutput("sem_graph", height = "650px")),
        tabPanel(
          "LLM-заключение",
          br(),
          p("Сначала запусти анализ, затем укажи API-ключ и нажми кнопку генерации. LLM не пересчитывает данные, а только пишет текст по готовым таблицам."),
          fluidRow(
            column(
              6,
              passwordInput(
                "llm_key",
                "OpenRouter API key",
                value = Sys.getenv("OPENROUTER_API_KEY")
              )
            ),
            column(
              6,
              textInput(
                "llm_model",
                "Модель OpenRouter",
                value = ifelse(
                  nzchar(Sys.getenv("OPENROUTER_MODEL")),
                  Sys.getenv("OPENROUTER_MODEL"),
                  "openrouter/free"
                )
              )
            )
          ),
          helpText("Для бесплатного тестирования используй openrouter/free или конкретную модель с окончанием :free. API-ключ не сохраняется в файле приложения."),
          actionButton("make_llm", "Сформировать LLM-заключение", class = "btn-success"),
          br(), br(),
          tags$div(
            style = "white-space: pre-wrap; overflow-wrap: anywhere; border: 1px solid #ddd; border-radius: 4px; padding: 12px; min-height: 120px; background: #fafafa;",
            textOutput("llm_conclusion", container = span)
          ),
          br(),
          downloadButton("download_llm", "Скачать LLM-заключение")
        ),
        tabPanel(
          "Заключение",
          br(),
          tags$div(
            style = "white-space: pre-wrap; overflow-wrap: anywhere; border: 1px solid #ddd; border-radius: 4px; padding: 12px; min-height: 120px; background: #fafafa; line-height: 1.45;",
            textOutput("conclusion", container = span)
          )
        ),
        tabPanel(
          "О программе",
          br(),
          h4("Информационная система оценки качества консервированных кормов растительного происхождения"),
          p("Приложение предназначено для автоматизации анализа лабораторных данных о качестве консервированных кормов, расчёта статистических показателей, построения SEM/path-модели, визуализации результатов и формирования аналитического заключения."),
          tags$div(
            style = "border: 1px solid #ddd; border-radius: 4px; padding: 12px; background: #fafafa; line-height: 1.45;",
            tags$strong("Знак охраны авторского права"),
            br(),
            tags$span("© 2026 Болотова Анастасия"),
            br(),
            tags$span("Программная реализация, структура анализа, интерфейс и материалы описания разработаны автором проекта.")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  analysis <- eventReactive(input$run, {
    req(input$file)

    withProgress(message = "Выполняется анализ данных", value = 0, {
      incProgress(0.15, detail = "Загрузка и проверка структуры")
      prepared <- prepare_data(input$file$datapath)
      df <- prepared$data

      shiny::validate(
        shiny::need(nrow(df) >= 20, "Для SEM-модели желательно не менее 20 наблюдений после удаления строк без оценки качества.")
      )

      incProgress(0.25, detail = "Расчёт описательной статистики и корреляций")
      desc <- desc_stats(df)
      corr <- corr_table(df)

      incProgress(0.25, detail = "Построение SEM/path-модели")
      fit <- fit_sem_model(df)

      incProgress(0.20, detail = "Формирование таблиц результатов")
      coef <- sem_coefficients(fit)
      r2 <- r2_table(fit)
      fit_indices <- fit_indices_table(fit)
      conclusion <- make_conclusion(fit)

      check <- tibble(
        Показатель = c(
          "Количество строк в загруженном файле",
          "Количество строк после удаления записей без оценки качества",
          "Количество переменных SEM-модели",
          "Пропуски после заполнения медианой"
        ),
        Значение = c(
          prepared$n_before,
          prepared$n_after_quality_filter,
          length(model_vars),
          sum(prepared$missing_after)
        )
      )

      incProgress(0.15, detail = "Готово")

      list(
        data = df,
        check = check,
        col_map = prepared$col_map,
        desc = desc,
        corr = corr,
        fit = fit,
        coef = coef,
        r2 = r2,
        fit_indices = fit_indices,
        conclusion = conclusion
      )
    })
  })


  llm_text <- eventReactive(input$make_llm, {
    req(analysis())

    shiny::validate(
      shiny::need(nzchar(input$llm_key), "Введите OpenRouter API key во вкладке LLM-заключение или задайте переменную окружения OPENROUTER_API_KEY.")
    )

    withProgress(message = "Формируется LLM-заключение", value = 0, {
      incProgress(0.35, detail = "Подготовка результатов для LLM")
      prompt <- make_llm_prompt(analysis())

      incProgress(0.45, detail = "Запрос к LLM")
      answer <- call_openrouter_llm(
        prompt = prompt,
        api_key = input$llm_key,
        model = input$llm_model
      )

      incProgress(0.20, detail = "Готово")
      answer
    })
  })

  output$check <- renderDT({
    res <- analysis()
    req(res)
    datatable(res$check, rownames = FALSE, options = list(pageLength = 10, dom = "t"))
  })

  output$col_map <- renderDT({
    res <- analysis()
    req(res)
    datatable(res$col_map, rownames = FALSE, options = list(pageLength = 20))
  })

  output$desc <- renderDT({
    res <- analysis()
    req(res)
    datatable(res$desc, rownames = FALSE, options = list(pageLength = 20))
  })

  output$corr <- renderDT({
    res <- analysis()
    req(res)
    datatable(res$corr, rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE))
  })

  output$coef <- renderDT({
    res <- analysis()
    req(res)
    datatable(res$coef, rownames = FALSE, options = list(pageLength = 20))
  })

  output$r2 <- renderDT({
    res <- analysis()
    req(res)
    datatable(res$r2, rownames = FALSE, options = list(pageLength = 20))
  })

  output$fit_indices <- renderDT({
    res <- analysis()
    req(res)
    datatable(res$fit_indices, rownames = FALSE, options = list(pageLength = 20))
  })

  output$sem_graph <- renderGrViz({
    res <- analysis()
    req(res)
    build_sem_graph(res$fit)
  })

  output$conclusion <- renderText({
    res <- analysis()
    req(res)
    res$conclusion
  })


  output$llm_conclusion <- renderText({
    llm_text()
  })

  output$download_llm <- downloadHandler(
    filename = function() {
      paste0("silage_quality_llm_conclusion_", Sys.Date(), ".txt")
    },
    content = function(file) {
      writeLines(llm_text(), con = file, useBytes = TRUE)
    }
  )

  output$download_report <- downloadHandler(
    filename = function() {
      paste0("silage_quality_analysis_report_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      make_report_workbook(analysis(), file)
    }
  )
}

shinyApp(ui, server)


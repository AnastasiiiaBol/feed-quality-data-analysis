from pathlib import Path
import argparse
import json
import math
import warnings
import numpy as np
import pandas as pd
import matplotlib
import matplotlib.pyplot as plt
from scipy.stats import kruskal, mannwhitneyu, f_oneway
from sklearn.compose import ColumnTransformer
from sklearn.ensemble import RandomForestClassifier
from sklearn.impute import SimpleImputer
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import accuracy_score, balanced_accuracy_score, classification_report, confusion_matrix, f1_score, mean_absolute_error, mean_squared_error, r2_score, cohen_kappa_score
from sklearn.model_selection import StratifiedKFold, KFold, cross_val_predict
from sklearn.neural_network import MLPClassifier
from sklearn.pipeline import Pipeline
from sklearn.base import clone
from sklearn.preprocessing import OneHotEncoder, StandardScaler
import statsmodels.formula.api as smf
from statsmodels.miscmodels.ordinal_model import OrderedModel
try:
    import networkx as nx
except Exception:
    nx = None
warnings.filterwarnings('ignore')
plt.rcParams['font.family'] = 'DejaVu Sans'
plt.rcParams['figure.dpi'] = 150

# Отключаем сохранение табличных файлов.
pd.DataFrame.to_csv = lambda self, *args, **kwargs: None
pd.Series.to_csv = lambda self, *args, **kwargs: None
DEFAULT_INPUT = "Silosrazn.xlsx"
DEFAULT_OUTPUT = "silage_full_project_outputs"
YEAR_COL = 'Год'
TYPE_COL = 'тип_силоса'
QUALITY_COL = 'качество_корма'
MOISTURE_COL = 'влага_проц_воздушно_сух'
SV_COL = 'св_проц'
SV_GKG_COL = 'св_г_кг'
SUGAR_COL = 'сахар_г_воздушно_сух'
CP_COL = 'протеин_проц_воздушно_сух'
CF_COL = 'клетчатка_проц_воздушно_сух'
ASH_COL = 'зола_проц_воздушно_сух'
PH_COL = 'ph_натуральная_влажность'
LAC_COL = 'молочная_кислота_проц_натуральная_влажность'
ACE_COL = 'уксусная_кислота_проц_натуральная_влажность'
BUT_COL = 'масляная_кислота_проц_натуральная_влажность'
TOTAL_ACID_COL = 'всего_кислот_проц_натуральная влажность'
LAC_SHARE_COL = 'доля_молочной_кислоты_проц_натуральная_влажность'
ACE_SHARE_COL = 'доля_уксусной_кислоты_проц_натуральная_влажность'
BUT_SHARE_COL = 'доля_масляной_кислоты_проц_натуральная_влажность'
PP_COL = 'переваримый_протеин_г_кг_воздушно_сух'
OE_COL = 'оэ_мдж_воздушно_сух'
KE_COL = 'корм_ед_кг_воздушно_сух'
BUT_DM_COL = 'масляная_кислота_проц_от_св'
ACE_DM_COL = 'уксусная_кислота_проц_от_св'
LAC_DM_COL = 'молочная_кислота_проц_от_св'
LAC_SHARE_CALC_COL = 'доля_молочной_расчет'
ACE_SHARE_PAIR_COL = 'доля_уксусной_в_паре_расчет'
LAC_SHARE_PAIR_COL = 'доля_молочной_в_паре_расчет'
YEAR_CAT_COL = 'Год_кат'
TYPE_GOST_COL = 'группа_гост'
BASE_COLS = [SV_COL, SUGAR_COL, CP_COL, CF_COL]
FERM_COLS = [PH_COL, LAC_COL, ACE_COL, BUT_COL, TOTAL_ACID_COL, LAC_SHARE_COL, ACE_SHARE_COL, BUT_SHARE_COL]
OUTPUT_COLS = [PP_COL, OE_COL, KE_COL]
CORE_NUMERIC_COLS = BASE_COLS + FERM_COLS + OUTPUT_COLS
DISPLAY_NAMES = {
    SV_COL: 'СВ, %',
    SUGAR_COL: 'Сахар',
    CP_COL: 'Сырой протеин, %',
    CF_COL: 'Сырая клетчатка, %',
    PH_COL: 'pH',
    LAC_COL: 'Молочная кислота, %',
    ACE_COL: 'Уксусная кислота, %',
    BUT_COL: 'Масляная кислота, %',
    TOTAL_ACID_COL: 'Всего кислот, %',
    LAC_SHARE_COL: 'Доля молочной, %',
    ACE_SHARE_COL: 'Доля уксусной, %',
    BUT_SHARE_COL: 'Доля масляной, %',
    PP_COL: 'Переваримый протеин',
    OE_COL: 'ОЭ',
    KE_COL: 'КЕ',
}

CORR_LABELS = {
    YEAR_COL: 'Год',
    MOISTURE_COL: 'Влага',
    CP_COL: 'Протеин',
    'калий_г_кг_воздушно_сух': 'Калий',
    CF_COL: 'Клетчатка',
    ASH_COL: 'Зола',
    'кальций_г_кг_воздушно_сух': 'Кальций',
    'фосфор_г_кг_воздушно_сух': 'Фосфор',
    'каротин_мг_кг_воздушно_сух': 'Каротин',
    SUGAR_COL: 'Сахар',
    'нитраты_мг_кг_воздушно_сух': 'Нитраты',
    'жир_проц_воздушно_сух': 'Жир',
    OE_COL: 'ОЭ',
    KE_COL: 'КЕ',
    PP_COL: 'Перев. протеин',
    QUALITY_COL: 'Качество',
    'крахмал_проц_воздуш_сух': 'Крахмал',
    PH_COL: 'pH',
    LAC_COL: 'Молочная к-та',
    ACE_COL: 'Уксусная к-та',
    BUT_COL: 'Масляная к-та',
    TOTAL_ACID_COL: 'Всего кислот',
    LAC_SHARE_COL: 'Доля молочной',
    ACE_SHARE_COL: 'Доля уксусной',
    BUT_SHARE_COL: 'Доля масляной',
}

def make_dirs(root: Path):
    return {
        'root': root,
        'data': root / 'data',
        'tables': root / 'tables',
        'figures': root / 'figures',
        'reports': root / 'reports',
    }

def print_section(title: str):
    print('\n' + '=' * 80)
    print(title)
    print('=' * 80)

def print_dataframe(df: pd.DataFrame, max_rows: int = 30):
    if df is None or df.empty:
        print('Нет данных.')
        return
    with pd.option_context(
        'display.max_rows', max_rows,
        'display.max_columns', None,
        'display.width', 220,
        'display.max_colwidth', 80,
    ):
        print(df.to_string())

def clean_dataframe(df: pd.DataFrame) -> pd.DataFrame:
    out = df.copy()
    for col in out.columns:
        if out[col].dtype == object:
            s = out[col].astype(str).str.strip()
            s = s.replace({'': np.nan, 'nan': np.nan, 'None': np.nan, 'NA': np.nan, 'NA ': np.nan, 'NaN': np.nan})
            num = pd.to_numeric(s.str.replace(',', '.', regex=False), errors='coerce')
            if num.notna().sum() >= max(5, int(0.6 * s.notna().sum())):
                out[col] = num
            else:
                out[col] = s
    out[QUALITY_COL] = pd.to_numeric(out[QUALITY_COL], errors='coerce').astype('Int64')
    out[TYPE_COL] = out[TYPE_COL].astype(str).str.strip().replace({'nan': np.nan})
    out[SV_COL] = 100 - out[MOISTURE_COL]
    out[SV_GKG_COL] = out[SV_COL] * 10
    out['сырой_протеин_г_кг_св'] = out[CP_COL] * 10
    out['сырая_клетчатка_г_кг_св'] = out[CF_COL] * 10
    if ASH_COL in out.columns:
        out['сырая_зола_г_кг_св'] = out[ASH_COL] * 10
    out[LAC_DM_COL] = out[LAC_COL] * 100 / out[SV_COL]
    out[ACE_DM_COL] = out[ACE_COL] * 100 / out[SV_COL]
    out[BUT_DM_COL] = out[BUT_COL] * 100 / out[SV_COL]
    acid_sum = out[[LAC_COL, ACE_COL, BUT_COL]].sum(axis=1, min_count=1)
    out[LAC_SHARE_CALC_COL] = np.where(acid_sum > 0, 100 * out[LAC_COL] / acid_sum, np.nan)
    pair_sum = out[[LAC_COL, ACE_COL]].sum(axis=1, min_count=1)
    out[LAC_SHARE_PAIR_COL] = np.where(pair_sum > 0, 100 * out[LAC_COL] / pair_sum, np.nan)
    out[ACE_SHARE_PAIR_COL] = np.where(pair_sum > 0, 100 * out[ACE_COL] / pair_sum, np.nan)
    out[YEAR_CAT_COL] = out[YEAR_COL].astype('Int64').astype(str)
    return out
def prepare_analysis_dataframe(df: pd.DataFrame):
    filtered = df.dropna(subset=[QUALITY_COL]).copy()
    removed = int(len(df) - len(filtered))
    rows = []
    for col in filtered.columns:
        before = int(filtered[col].isna().sum())
        strategy = 'без изменений'
        fill_value = np.nan
        if before > 0:
            if pd.api.types.is_numeric_dtype(filtered[col]):
                med = filtered[col].median(skipna=True)
                if pd.notna(med):
                    filtered[col] = filtered[col].fillna(med)
                    strategy = 'медиана'
                    fill_value = med
            else:
                mode = filtered[col].mode(dropna=True)
                if not mode.empty:
                    filtered[col] = filtered[col].fillna(mode.iloc[0])
                    strategy = 'мода'
                    fill_value = mode.iloc[0]
        after = int(filtered[col].isna().sum())
        rows.append([col, before, after, strategy, fill_value])
    filtered[QUALITY_COL] = filtered[QUALITY_COL].astype(int)
    filtered[YEAR_CAT_COL] = filtered[YEAR_COL].astype('Int64').astype(str)
    prep_detail = pd.DataFrame(rows, columns=['Переменная', 'Пропуски_до', 'Пропуски_после', 'Способ_обработки', 'Заполнитель'])
    prep_summary = pd.DataFrame([
        ['Исходное число строк', len(df)],
        ['Удалено строк без экспертной оценки', removed],
        ['Осталось строк для анализа', len(filtered)],
    ], columns=['Показатель', 'Значение'])
    return filtered, prep_detail, prep_summary

def build_eda_tables(df: pd.DataFrame):
    type_counts = df[TYPE_COL].value_counts(dropna=False).rename_axis('тип_силоса').reset_index(name='n')
    type_counts['доля_%'] = (100 * type_counts['n'] / len(df)).round(2)

    year_counts = df[YEAR_COL].value_counts(dropna=False).sort_index().rename_axis('Год').reset_index(name='n')
    year_counts['доля_%'] = (100 * year_counts['n'] / len(df)).round(2)

    desc_cols = [
        SV_COL,
        MOISTURE_COL,
        SUGAR_COL,
        CP_COL,
        CF_COL,
        PH_COL,
        LAC_COL,
        ACE_COL,
        BUT_COL,
        PP_COL,
        OE_COL,
        KE_COL,
        QUALITY_COL,
    ]
    desc_cols = [c for c in desc_cols if c in df.columns]

    desc = df[desc_cols].describe().T.reset_index()
    desc = desc.rename(columns={
        'index': 'Показатель',
        'count': 'n',
        'mean': 'Среднее',
        'std': 'Ст.откл.',
        'min': 'Мин',
        '25%': 'Q1',
        '50%': 'Медиана',
        '75%': 'Q3',
        'max': 'Макс'
    })

    for col in ['n', 'Среднее', 'Ст.откл.', 'Мин', 'Q1', 'Медиана', 'Q3', 'Макс']:
        if col in desc.columns:
            desc[col] = desc[col].round(4)

    return type_counts, year_counts, desc
def build_variable_roles() -> pd.DataFrame:
    rows = [
        [SV_COL, 'Вход', 'Основные показатели по СВ', 'Отдельный входной показатель'],
        [SUGAR_COL, 'Вход', 'Основные показатели по СВ', 'Основной показатель состава'],
        [CP_COL, 'Вход', 'Основные показатели по СВ', 'Основной показатель состава'],
        [CF_COL, 'Вход', 'Основные показатели по СВ', 'Основной показатель состава'],
        [PH_COL, 'Промежуточная', 'Ферментация', 'Индикатор хода брожения'],
        [LAC_COL, 'Промежуточная', 'Ферментация', 'Органическая кислота'],
        [ACE_COL, 'Промежуточная', 'Ферментация', 'Органическая кислота'],
        [BUT_COL, 'Промежуточная', 'Ферментация', 'Органическая кислота'],
        [PP_COL, 'Выход', 'Питательность', 'Показатель питательности'],
        [OE_COL, 'Выход', 'Питательность', 'Показатель питательности'],
        [KE_COL, 'Выход', 'Питательность', 'Показатель питательности'],
        [QUALITY_COL, 'Итог', 'Качество', 'Целевая переменная'],
        [TYPE_COL, 'Внешний фактор', 'Группа', 'Группирующий фактор'],
        [YEAR_COL, 'Внешний фактор', 'Год', 'Фактор наблюдения'],
    ]
    return pd.DataFrame(rows, columns=['Переменная', 'Роль', 'Блок', 'Описание'])
def build_dag_edges() -> pd.DataFrame:
    rows = [
        [TYPE_COL, SV_COL, '+'], [TYPE_COL, SUGAR_COL, '+'], [TYPE_COL, CP_COL, '+'], [TYPE_COL, CF_COL, '+'], [TYPE_COL, PH_COL, '+'], [TYPE_COL, QUALITY_COL, '+'],
        [YEAR_COL, SV_COL, '+'], [YEAR_COL, SUGAR_COL, '+'], [YEAR_COL, PH_COL, '+'], [YEAR_COL, QUALITY_COL, '+'],
        [SV_COL, LAC_COL, '+'], [SV_COL, BUT_COL, '-'], [SUGAR_COL, LAC_COL, '+'], [SUGAR_COL, ACE_COL, '+'], [CP_COL, PH_COL, '+'],
        [LAC_COL, PH_COL, '-'], [ACE_COL, PH_COL, '-'], [BUT_COL, PH_COL, '-'],
        [CP_COL, PP_COL, '+'], [CF_COL, PP_COL, '-'], [CP_COL, OE_COL, '+'], [CF_COL, OE_COL, '-'], [OE_COL, KE_COL, '+'],
        [PH_COL, QUALITY_COL, '+'], [LAC_COL, QUALITY_COL, '+'], [BUT_COL, QUALITY_COL, '-'], [OE_COL, QUALITY_COL, '+'], [KE_COL, QUALITY_COL, '+'],
    ]
    return pd.DataFrame(rows, columns=['Источник', 'Приемник', 'Знак связи'])
def build_task_spec() -> pd.DataFrame:
    rows = [
        ['1. Скрытые факторы', 'Оценка возможности определения молочной и масляной кислот по основным показателям', f'{SV_COL}; {PH_COL}', f'{LAC_COL}; {BUT_COL}', f'{TYPE_COL}; {YEAR_COL}', 'Промежуточные downstream-переменные не контролируются', 'OLS simple/full'],
        ['2. Диагностика качества', 'Автоматическая классификация качества корма', f'{SV_COL}; {SUGAR_COL}; {CP_COL}; {CF_COL}; {PH_COL}; {LAC_COL}; {ACE_COL}; {BUT_COL}', QUALITY_COL, f'{TYPE_COL}; {YEAR_COL}', 'Расчётные выходы не обязательны', 'Logistic/RF/MLP'],
        ['3. Сравнение групп силоса', 'Сравнение трёх групп силоса по всем блокам показателей', TYPE_COL, 'Показатели состава, ферментации, питательности', YEAR_COL, 'Нет отдельной злаковой группы', 'Kruskal-Wallis + pairwise MWU'],
        ['4. Повышение питательности', 'Факторы, влияющие на ОЭ и КЕ', f'{CP_COL}; {CF_COL}; {SV_COL}; {PH_COL}; {LAC_COL}; {ACE_COL}; {BUT_COL}', f'{OE_COL}; {KE_COL}', f'{TYPE_COL}; {YEAR_COL}', f'Для {KE_COL} {OE_COL} — медиатор', 'OLS'],
        ['5. Итоговая модель качества', 'Итоговая модель определения качества силоса', 'Состав + ферментация + группа + год', QUALITY_COL, 'Сравнение с ГОСТ/DLG/Flieg', 'ПП/ОЭ/КЕ отдельно в практической модели', 'RF как лучшая итоговая модель'],
        ['6. Причинно-следственный блок', 'Система регрессионных моделей по DAG', 'Узлы DAG', 'Кислоты, pH, ОЭ, КЕ, плохое качество', f'{TYPE_COL}; {YEAR_COL}', 'Модели специфицированы по экспертному DAG', 'OLS + logit'],
    ]
    return pd.DataFrame(rows, columns=['Задача', 'Смысл', 'Воздействие/вход', 'Исход', 'Корректировка', 'Что не включаем', 'Тип модели'])
def save_text(path: Path, text: str):
    # Сохранение отключено.
    return None
def pretty_corr_label(name: str) -> str:
    return CORR_LABELS.get(name, name)

def corr_heatmap(corr: pd.DataFrame, out: Path | None = None):
    n = len(corr.columns)
    labels = [pretty_corr_label(c) for c in corr.columns]

    mask = np.triu(np.ones_like(corr, dtype=bool), k=1)
    data = np.ma.masked_where(mask, corr.values)

    fig_w = max(18, min(30, 0.62 * n + 8))
    fig_h = max(16, min(28, 0.60 * n + 7))
    fig, ax = plt.subplots(figsize=(fig_w, fig_h))

    im = ax.imshow(data, vmin=-1, vmax=1, aspect="equal")

    cbar = fig.colorbar(im, ax=ax, fraction=0.028, pad=0.02)
    cbar.set_label("Коэффициент корреляции Спирмена")

    ax.set_xticks(np.arange(n))
    ax.set_yticks(np.arange(n))

    if n > 24:
        fs = 6
        num_fs = 3
    elif n > 18:
        fs = 7
        num_fs = 4
    else:
        fs = 8
        num_fs = 5

    ax.set_xticklabels(labels, rotation=60, ha='right', fontsize=fs)
    ax.set_yticklabels(labels, fontsize=fs)

    ax.tick_params(axis='x', pad=6)
    ax.tick_params(axis='y', pad=4)

    ax.set_title("Корреляционная матрица")

    ax.set_xticks(np.arange(-0.5, n, 1), minor=True)
    ax.set_yticks(np.arange(-0.5, n, 1), minor=True)
    ax.grid(which='minor', linestyle='-', linewidth=0.35)
    ax.tick_params(which='minor', bottom=False, left=False)

    for i in range(n):
        for j in range(i + 1):
            val = corr.iloc[i, j]
            if pd.isna(val):
                continue
            ax.text(
                j,
                i,
                f"{val:.2f}",
                ha='center',
                va='center',
                fontsize=num_fs,
            )

    fig.subplots_adjust(
        left=0.18,
        bottom=0.34,
        right=0.90,
        top=0.93,
    )

    plt.show()
    plt.close(fig)
def normalize_quality_for_compare(s: pd.Series) -> pd.Series:
    arr = pd.to_numeric(s, errors='coerce')
    return arr.where(arr.isin([1, 2, 3]))
def compare_metrics(y_true: pd.Series, y_pred: pd.Series) -> dict:
    mask = y_true.notna() & y_pred.notna()
    yt = y_true[mask].astype(int)
    yp = y_pred[mask].astype(int)
    if len(yt) == 0:
        return {'n': 0, 'accuracy': np.nan, 'balanced_accuracy': np.nan, 'macro_f1': np.nan, 'weighted_f1': np.nan, 'kappa': np.nan}
    return {
        'n': int(len(yt)),
        'accuracy': float(accuracy_score(yt, yp)),
        'balanced_accuracy': float(balanced_accuracy_score(yt, yp)),
        'macro_f1': float(f1_score(yt, yp, average='macro')),
        'weighted_f1': float(f1_score(yt, yp, average='weighted')),
        'kappa': float(cohen_kappa_score(yt, yp)),
    }
def holm_adjust(pvals):
    pvals = np.asarray(pvals, dtype=float)
    n = len(pvals)
    order = np.argsort(pvals)
    adjusted = np.empty(n)
    running = 0.0
    for rank, idx in enumerate(order):
        value = (n - rank) * pvals[idx]
        running = max(running, value)
        adjusted[idx] = min(running, 1.0)
    return adjusted
def run_group_analysis(df: pd.DataFrame, out_dirs):
    cols = [SV_COL, SUGAR_COL, CP_COL, CF_COL, PH_COL, LAC_COL, ACE_COL, BUT_COL, PP_COL, OE_COL, KE_COL]
    summary = df.groupby(TYPE_COL)[cols].agg(['mean', 'median', 'std'])
    summary.to_csv(out_dirs['tables'] / 'group_summary.csv', encoding='utf-8-sig')

    anova_rows = []
    kr_rows = []
    pair_rows = []
    groups = list(df[TYPE_COL].dropna().unique())

    for col in cols:
        vals = [g[col].dropna().values for _, g in df.groupby(TYPE_COL)]

        if all(len(v) > 0 for v in vals) and len(vals) >= 2:
            try:
                f_stat, p_anova = f_oneway(*vals)
            except Exception:
                f_stat, p_anova = np.nan, np.nan

            try:
                stat, p = kruskal(*vals)
            except Exception:
                stat, p = np.nan, np.nan
        else:
            f_stat, p_anova = np.nan, np.nan
            stat, p = np.nan, np.nan

        anova_rows.append({'variable': col, 'F': f_stat, 'p_value': p_anova})
        kr_rows.append({'variable': col, 'H': stat, 'p_value': p})

        local_pairs = []
        for i in range(len(groups)):
            for j in range(i + 1, len(groups)):
                a = df.loc[df[TYPE_COL] == groups[i], col].dropna()
                b = df.loc[df[TYPE_COL] == groups[j], col].dropna()
                if len(a) == 0 or len(b) == 0:
                    u, p2, effect = np.nan, np.nan, np.nan
                else:
                    u, p2 = mannwhitneyu(a, b, alternative='two-sided')
                    effect = abs(2 * u / (len(a) * len(b)) - 1)
                local_pairs.append({'variable': col, 'group_a': groups[i], 'group_b': groups[j], 'u_stat': u, 'p_value': p2, 'effect_size_rank_biserial_abs': effect})

        if local_pairs:
            adj = holm_adjust([r['p_value'] if pd.notna(r['p_value']) else 1.0 for r in local_pairs])
            for r, adjp in zip(local_pairs, adj):
                r['p_value_holm'] = adjp
                pair_rows.append(r)

    anova_df = pd.DataFrame(anova_rows).sort_values('p_value')
    kr_df = pd.DataFrame(kr_rows).sort_values('p_value')
    pair_df = pd.DataFrame(pair_rows).sort_values(['variable', 'p_value'])

    anova_df.to_csv(out_dirs['tables'] / 'group_anova.csv', index=False, encoding='utf-8-sig')
    kr_df.to_csv(out_dirs['tables'] / 'group_kruskal.csv', index=False, encoding='utf-8-sig')
    pair_df.to_csv(out_dirs['tables'] / 'group_pairwise_holm.csv', index=False, encoding='utf-8-sig')

    for col in [OE_COL, KE_COL, CP_COL, PP_COL, CF_COL, PH_COL]:
        groups_plot = []
        labels_plot = []
        for g, part in df[[TYPE_COL, col]].dropna().groupby(TYPE_COL):
            groups_plot.append(part[col].values)
            labels_plot.append(str(g))
        plt.figure(figsize=(8, 5))
        plt.boxplot(groups_plot, tick_labels=labels_plot)
        plt.title(f"{DISPLAY_NAMES.get(col, col)} по типам силоса")
        plt.xlabel("Тип силоса")
        plt.ylabel(DISPLAY_NAMES.get(col, col))
        plt.xticks(rotation=20, ha="right")
        plt.tight_layout()
        plt.show()
        plt.close()

    return anova_df, kr_df, pair_df
def regression_metrics(y_true, y_pred):
    mask = pd.Series(y_true).notna() & pd.Series(y_pred).notna()
    yt = np.asarray(pd.Series(y_true)[mask], dtype=float)
    yp = np.asarray(pd.Series(y_pred)[mask], dtype=float)
    return {
        'r2': float(r2_score(yt, yp)),
        'mae': float(mean_absolute_error(yt, yp)),
        'rmse': float(math.sqrt(mean_squared_error(yt, yp))),
    }
def build_design_for_models(df: pd.DataFrame, features):
    X = df[features].copy()
    y = df[QUALITY_COL].astype(int)
    numeric = [c for c in features if c not in [TYPE_COL, YEAR_CAT_COL]]
    categoric = [c for c in features if c in [TYPE_COL, YEAR_CAT_COL]]
    pre = ColumnTransformer([
        ('num', Pipeline([('imp', SimpleImputer(strategy='median')), ('sc', StandardScaler())]), numeric),
        ('cat', Pipeline([('imp', SimpleImputer(strategy='most_frequent')), ('oh', OneHotEncoder(handle_unknown='ignore'))]), categoric),
    ])
    return X, y, pre
def evaluate_classifiers(df: pd.DataFrame, features, prefix, out_dirs):
    X, y, pre = build_design_for_models(df, features)
    models = {
        'logistic_regression': LogisticRegression(max_iter=1000, class_weight='balanced'),
        'random_forest': RandomForestClassifier(n_estimators=500, random_state=42, class_weight='balanced_subsample'),
        'mlp': MLPClassifier(hidden_layer_sizes=(64, 32), max_iter=1500, random_state=42),
    }
    cv = StratifiedKFold(n_splits=5, shuffle=True, random_state=42)
    rows = []
    per_class_rows = []
    for name, model in models.items():
        pipe = Pipeline([('pre', pre), ('model', model)])
        pred = cross_val_predict(pipe, X, y, cv=cv)
        rows.append({
            'scenario': prefix,
            'model': name,
            'accuracy': accuracy_score(y, pred),
            'balanced_accuracy': balanced_accuracy_score(y, pred),
            'macro_f1': f1_score(y, pred, average='macro'),
            'weighted_f1': f1_score(y, pred, average='weighted'),
        })
        rep = classification_report(y, pred, output_dict=True, zero_division=0)
        for cls in ['1', '2', '3']:
            if cls in rep:
                per_class_rows.append({
                    'scenario': prefix,
                    'model': name,
                    'class': cls,
                    'precision': rep[cls]['precision'],
                    'recall': rep[cls]['recall'],
                    'f1': rep[cls]['f1-score'],
                    'support': rep[cls]['support'],
                })
        cm = pd.DataFrame(confusion_matrix(y, pred, labels=[1, 2, 3]), index=['true_1', 'true_2', 'true_3'], columns=['pred_1', 'pred_2', 'pred_3'])
        cm.to_csv(out_dirs['tables'] / f'{prefix}_{name}_confusion_matrix.csv', encoding='utf-8-sig')
    summary = pd.DataFrame(rows).sort_values(['weighted_f1', 'accuracy'], ascending=False)
    per_class = pd.DataFrame(per_class_rows)
    summary.to_csv(out_dirs['tables'] / f'{prefix}_classification_summary.csv', index=False, encoding='utf-8-sig')
    per_class.to_csv(out_dirs['tables'] / f'{prefix}_classification_per_class.csv', index=False, encoding='utf-8-sig')
    return summary, per_class
def acid_models(df: pd.DataFrame, out_dirs):
    formulas = {
        'lactic_simple': f'Q("{LAC_COL}") ~ Q("{SV_COL}") + Q("{PH_COL}")',
        'lactic_full': f'Q("{LAC_COL}") ~ Q("{SV_COL}") + Q("{PH_COL}") + Q("{SUGAR_COL}") + Q("{CP_COL}") + Q("{CF_COL}") + C(Q("{TYPE_COL}")) + C(Q("{YEAR_CAT_COL}"))',
        'butyric_simple': f'Q("{BUT_COL}") ~ Q("{SV_COL}") + Q("{PH_COL}")',
        'butyric_full': f'Q("{BUT_COL}") ~ Q("{SV_COL}") + Q("{PH_COL}") + Q("{SUGAR_COL}") + Q("{CP_COL}") + Q("{CF_COL}") + C(Q("{TYPE_COL}")) + C(Q("{YEAR_CAT_COL}"))',
    }
    rows = []
    for name, formula in formulas.items():
        model = smf.ols(formula, data=df).fit()
        pred = model.predict(df)
        target = LAC_COL if 'lactic' in name else BUT_COL
        m = regression_metrics(df[target], pred)
        rows.append({'target': target, 'model': name.split('_')[-1], **m, 'adj_r2': model.rsquared_adj, 'aic': model.aic})
        ci = model.conf_int()
        coef = pd.DataFrame({'term': model.params.index, 'coef': model.params.values, 'std_err': model.bse.values, 'p_value': model.pvalues.values, 'ci_low': ci[0].values, 'ci_high': ci[1].values})
        coef.to_csv(out_dirs['tables'] / f'{name}_coefficients.csv', index=False, encoding='utf-8-sig')
    res = pd.DataFrame(rows)
    res.to_csv(out_dirs['tables'] / 'acid_models_summary.csv', index=False, encoding='utf-8-sig')
    return res
def oe_ke_models(df: pd.DataFrame, out_dirs):
    formulas = {
        'oe_model': f'Q("{OE_COL}") ~ Q("{SV_COL}") + Q("{CP_COL}") + Q("{CF_COL}") + Q("{PH_COL}") + Q("{LAC_COL}") + Q("{ACE_COL}") + Q("{BUT_COL}") + C(Q("{TYPE_COL}")) + C(Q("{YEAR_CAT_COL}"))',
        'ke_model': f'Q("{KE_COL}") ~ Q("{SV_COL}") + Q("{CP_COL}") + Q("{CF_COL}") + Q("{PH_COL}") + Q("{LAC_COL}") + Q("{ACE_COL}") + Q("{BUT_COL}") + C(Q("{TYPE_COL}")) + C(Q("{YEAR_CAT_COL}"))',
    }
    rows = []
    for name, formula in formulas.items():
        model = smf.ols(formula, data=df).fit()
        target = OE_COL if 'oe' in name else KE_COL
        pred = model.predict(df)
        m = regression_metrics(df[target], pred)
        rows.append({'target': target, **m, 'adj_r2': model.rsquared_adj, 'aic': model.aic})
        ci = model.conf_int()
        coef = pd.DataFrame({'term': model.params.index, 'coef': model.params.values, 'std_err': model.bse.values, 'p_value': model.pvalues.values, 'ci_low': ci[0].values, 'ci_high': ci[1].values})
        coef.to_csv(out_dirs['tables'] / f'{name}_coefficients.csv', index=False, encoding='utf-8-sig')
    res = pd.DataFrame(rows)
    res.to_csv(out_dirs['tables'] / 'oe_ke_models_summary.csv', index=False, encoding='utf-8-sig')
    return res
def assign_fermentation_rules(df: pd.DataFrame) -> pd.DataFrame:
    out = df.copy()
    out['правило_масляная_>0_3_от_св'] = out[BUT_DM_COL] > 0.3
    out['правило_70_30_без_масляной'] = (out[LAC_SHARE_PAIR_COL].between(65, 75, inclusive='both')) & (out[BUT_DM_COL] <= 0.01)
    out['правило_55_60_без_масляной'] = (out[LAC_SHARE_CALC_COL].between(55, 60, inclusive='both')) & (out[BUT_DM_COL] <= 0.01)
    out['правило_строгое_оба'] = out['правило_70_30_без_масляной'] & out['правило_55_60_без_масляной']
    conds = [out['правило_масляная_>0_3_от_св'], out['правило_70_30_без_масляной']]
    choices = ['неблагоприятное/некачественный', 'благоприятное молочнокислое']
    out['тип_брожения_правила'] = np.select(conds, choices, default='прочее/неопределенное')
    return out
def fermentation_summary(df: pd.DataFrame, out_dirs):
    cols = ['правило_масляная_>0_3_от_св', 'правило_70_30_без_масляной', 'правило_55_60_без_масляной', 'правило_строгое_оба']
    counts = pd.DataFrame({'criterion': cols, 'count_true': [int(df[c].sum()) for c in cols], 'share_true': [float(df[c].mean()) for c in cols]})
    counts.to_csv(out_dirs['tables'] / 'fermentation_rule_counts.csv', index=False, encoding='utf-8-sig')
    pd.crosstab(df['тип_брожения_правила'], df[QUALITY_COL]).to_csv(out_dirs['tables'] / 'fermentation_vs_quality.csv', encoding='utf-8-sig')
    return counts
def map_gost_group(x):
    x = str(x)
    if 'Кукуруз' in x:
        return 'кукуруза'
    if 'Люцернов' in x:
        return 'бобовый'
    if 'Смеси' in x or 'боб-злак' in x:
        return 'бобово-злаковый'
    return np.nan
def class_min(v, t1, t2, t3):
    if pd.isna(v):
        return np.nan
    if v >= t1:
        return 1
    if v >= t2:
        return 2
    if v >= t3:
        return 3
    return 4
def class_max(v, t1, t2, t3):
    if pd.isna(v):
        return np.nan
    if v <= t1:
        return 1
    if v <= t2:
        return 2
    if v <= t3:
        return 3
    return 4
def class_range(v, r1, r2, r3):
    if pd.isna(v):
        return np.nan
    if r1[0] <= v <= r1[1]:
        return 1
    if r2[0] <= v <= r2[1]:
        return 2
    if r3[0] <= v <= r3[1]:
        return 3
    return 4
def gost_dlg_flieg(df: pd.DataFrame, out_dirs):
    out = df.copy()
    out[TYPE_GOST_COL] = out[TYPE_COL].map(map_gost_group)
    DM_THRESH = {'кукуруза': (300, 250, 200), 'бобовый': (280, 260, 240), 'бобово-злаковый': (280, 260, 240)}
    CP_THRESH = {'кукуруза': (80, 75, 75), 'бобовый': (160, 140, 120), 'бобово-злаковый': (140, 130, 110)}
    CF_THRESH = {'кукуруза': (220, 240, 260), 'бобовый': (280, 300, 320), 'бобово-злаковый': (280, 300, 320)}
    ASH_THRESH = (100, 110, 130)
    LACTIC_THRESH = {'кукуруза': (70, 65, 60), 'бобовый': (65, 60, 55), 'бобово-злаковый': (65, 60, 55)}
    BUT_THRESH = (0.1, 0.2, 0.3)
    PH_R1 = (3.9, 4.3)
    PH_R2 = (3.9, 4.3)
    PH_R3 = (3.8, 4.5)
    def gost_row(r):
        g = r[TYPE_GOST_COL]
        if pd.isna(g):
            return pd.Series({'gost_dm_class': np.nan, 'gost_cp_class': np.nan, 'gost_cf_class': np.nan, 'gost_ash_class': np.nan, 'gost_lactic_share_class': np.nan, 'gost_butyric_dm_class': np.nan, 'gost_ph_class': np.nan, 'gost_class_final': np.nan, 'gost_rule': np.nan, 'gost_class_compare': np.nan})
        c = {}
        c['dm'] = class_min(r[SV_GKG_COL], *DM_THRESH[g])
        c['cp'] = class_min(r['сырой_протеин_г_кг_св'], *CP_THRESH[g])
        c['cf'] = class_max(r['сырая_клетчатка_г_кг_св'], *CF_THRESH[g])
        c['ash'] = class_max(r.get('сырая_зола_г_кг_св', np.nan), *ASH_THRESH)
        c['lactic_share'] = class_min(r[LAC_SHARE_COL] if pd.notna(r[LAC_SHARE_COL]) else r[LAC_SHARE_CALC_COL], *LACTIC_THRESH[g])
        c['butyric_dm'] = class_max(r[BUT_DM_COL], *BUT_THRESH)
        c['ph'] = class_range(r[PH_COL], PH_R1, PH_R2, PH_R3)
        strict_vals = [v for v in c.values() if not pd.isna(v)]
        strict = max(strict_vals) if strict_vals else np.nan
        key_ok = all(pd.notna(c[k]) and c[k] <= 2 for k in ['dm', 'cp', 'butyric_dm'])
        if key_ok:
            final = max(c[k] for k in ['dm', 'cp', 'butyric_dm'])
            rule = '4.7'
        else:
            final = strict
            rule = 'strict'
        compare = min(int(final), 3) if pd.notna(final) else np.nan
        return pd.Series({
            'gost_dm_class': c['dm'], 'gost_cp_class': c['cp'], 'gost_cf_class': c['cf'], 'gost_ash_class': c['ash'],
            'gost_lactic_share_class': c['lactic_share'], 'gost_butyric_dm_class': c['butyric_dm'], 'gost_ph_class': c['ph'],
            'gost_class_final': final, 'gost_rule': rule, 'gost_class_compare': compare,
        })
    gost = out.apply(gost_row, axis=1)
    out = pd.concat([out, gost], axis=1)
    def dlg_ba_score(x):
        if pd.isna(x):
            return np.nan
        if x <= 0.3: return 90
        if x <= 0.4: return 81
        if x <= 0.7: return 72
        if x <= 1.0: return 63
        if x <= 1.3: return 54
        if x <= 1.6: return 45
        if x <= 1.9: return 36
        if x <= 2.6: return 27
        if x <= 3.6: return 18
        if x <= 5.0: return 9
        return 0
    def dlg_aa_score(x):
        if pd.isna(x): return np.nan
        if x <= 3.0: return 0
        if x <= 3.5: return -10
        if x <= 4.5: return -20
        if x <= 5.5: return -30
        if x <= 6.5: return -40
        if x <= 7.5: return -50
        if x <= 8.5: return -60
        return -70
    def dlg_ph_score(dm, ph):
        if pd.isna(dm) or pd.isna(ph): return np.nan
        if dm <= 30:
            if ph <= 4.0: return 10
            if ph <= 4.3: return 5
            if ph <= 4.6: return 0
            return -5
        if dm <= 45:
            if ph <= 4.5: return 10
            if ph <= 4.8: return 5
            return 0
        if ph <= 5.0: return 10
        if ph <= 5.3: return 5
        return 0
    def dlg_grade(total):
        if pd.isna(total): return np.nan
        if total >= 90: return 1
        if total >= 72: return 2
        if total >= 52: return 3
        if total >= 30: return 4
        return 5
    out['dlg_ba_score'] = out[BUT_DM_COL].map(dlg_ba_score)
    out['dlg_aa_score'] = out[ACE_DM_COL].map(dlg_aa_score)
    out['dlg_ph_score'] = out.apply(lambda r: dlg_ph_score(r[SV_COL], r[PH_COL]), axis=1)
    out['dlg_total_score'] = out[['dlg_ba_score', 'dlg_aa_score', 'dlg_ph_score']].sum(axis=1, min_count=3)
    out['dlg_grade'] = out['dlg_total_score'].map(dlg_grade)
    out['dlg_class_compare'] = out['dlg_grade'].map({1: 1, 2: 1, 3: 2, 4: 3, 5: 3})
    out['flieg_score'] = 220 + (2 * out[SV_COL] - 15) - 40 * out[PH_COL]
    def flieg_class(x):
        if pd.isna(x): return np.nan
        if x > 100: return 1
        if x >= 81: return 1
        if x >= 61: return 2
        if x >= 41: return 3
        return 3
    out['flieg_class_compare'] = out['flieg_score'].map(flieg_class)
    method_rows = []
    expert = normalize_quality_for_compare(out[QUALITY_COL])
    for name, col in [('ГОСТ', 'gost_class_compare'), ('DLG', 'dlg_class_compare'), ('индекс_Флига', 'flieg_class_compare')]:
        m = compare_metrics(expert, normalize_quality_for_compare(out[col]))
        method_rows.append({'method': name, **m})
    methods = pd.DataFrame(method_rows).sort_values(['accuracy', 'weighted_f1'], ascending=False)
    out.to_csv(out_dirs['data'] / 'normative_benchmark_data.csv', index=False, encoding='utf-8-sig')
    methods.to_csv(out_dirs['tables'] / 'normative_method_comparison.csv', index=False, encoding='utf-8-sig')
    pd.DataFrame({'gost_class_final': out['gost_class_final'].value_counts(dropna=False).sort_index()}).to_csv(out_dirs['tables'] / 'gost_distribution.csv', encoding='utf-8-sig')
    return out, methods
def causal_models(df: pd.DataFrame, out_dirs):
    work = df.copy()
    work['bad_quality'] = (work[QUALITY_COL].astype(int) == 3).astype(int)
    formulas = {
        'causal_lactic': f'Q("{LAC_COL}") ~ Q("{SV_COL}") + Q("{SUGAR_COL}") + Q("{CP_COL}") + Q("{CF_COL}") + C(Q("{TYPE_COL}")) + C(Q("{YEAR_CAT_COL}"))',
        'causal_butyric_dm': f'Q("{BUT_DM_COL}") ~ Q("{SV_COL}") + Q("{SUGAR_COL}") + Q("{CP_COL}") + Q("{CF_COL}") + C(Q("{TYPE_COL}")) + C(Q("{YEAR_CAT_COL}"))',
        'causal_ph': f'Q("{PH_COL}") ~ Q("{LAC_COL}") + Q("{ACE_COL}") + Q("{BUT_COL}") + Q("{SV_COL}") + C(Q("{TYPE_COL}")) + C(Q("{YEAR_CAT_COL}"))',
        'causal_oe': f'Q("{OE_COL}") ~ Q("{CP_COL}") + Q("{CF_COL}") + Q("{PH_COL}") + Q("{BUT_DM_COL}") + C(Q("{TYPE_COL}")) + C(Q("{YEAR_CAT_COL}"))',
        'causal_ke': f'Q("{KE_COL}") ~ Q("{OE_COL}") + Q("{CF_COL}") + Q("{PH_COL}") + C(Q("{TYPE_COL}")) + C(Q("{YEAR_CAT_COL}"))',
        'causal_bad_quality': f'bad_quality ~ Q("{PH_COL}") + Q("{LAC_SHARE_CALC_COL}") + Q("{BUT_DM_COL}") + C(Q("{TYPE_COL}")) + C(Q("{YEAR_CAT_COL}"))',
    }
    rows = []
    for name, formula in formulas.items():
        if name == 'causal_bad_quality':
            model = smf.logit(formula, data=work).fit(disp=0)
            ci = model.conf_int()
            coef = pd.DataFrame({'term': model.params.index, 'coef': model.params.values, 'std_err': model.bse.values, 'p_value': model.pvalues.values, 'ci_low': ci[0].values, 'ci_high': ci[1].values})
            coef['odds_ratio'] = np.exp(coef['coef'])
            coef['or_ci_low'] = np.exp(coef['ci_low'])
            coef['or_ci_high'] = np.exp(coef['ci_high'])
            rows.append({'model': name, 'metric': 'pseudo_r2_mcfadden', 'value': 1 - model.llf / model.llnull})
            rows.append({'model': name, 'metric': 'aic', 'value': model.aic})
        else:
            model = smf.ols(formula, data=work).fit()
            ci = model.conf_int()
            coef = pd.DataFrame({'term': model.params.index, 'coef': model.params.values, 'std_err': model.bse.values, 'p_value': model.pvalues.values, 'ci_low': ci[0].values, 'ci_high': ci[1].values})
            rows.append({'model': name, 'metric': 'r2', 'value': model.rsquared})
            rows.append({'model': name, 'metric': 'adj_r2', 'value': model.rsquared_adj})
            rows.append({'model': name, 'metric': 'aic', 'value': model.aic})
        coef.to_csv(out_dirs['tables'] / f'{name}_coefficients.csv', index=False, encoding='utf-8-sig')
    summary = pd.DataFrame(rows)
    summary.to_csv(out_dirs['tables'] / 'causal_models_summary.csv', index=False, encoding='utf-8-sig')
    return summary
def draw_dag(edges: pd.DataFrame, out_dirs=None):
    if nx is None or edges is None or edges.empty:
        return

    import matplotlib.patches as mpatches
    from matplotlib.patches import FancyArrowPatch

    G = nx.DiGraph()
    for _, r in edges.iterrows():
        G.add_edge(r['Источник'], r['Приемник'], sign=r['Знак связи'])

    display_names = {
        SV_COL: 'СВ',
        SUGAR_COL: 'Сахар',
        CP_COL: 'Протеин',
        CF_COL: 'Клетчатка',
        PH_COL: 'pH',
        LAC_COL: 'Молочная\nкислота',
        ACE_COL: 'Уксусная\nкислота',
        BUT_COL: 'Масляная\nкислота',
        PP_COL: 'Переваримый\nпротеин',
        OE_COL: 'ОЭ',
        KE_COL: 'КЕ',
    }

    pos = {
        SV_COL: (0.0, 3.0),
        SUGAR_COL: (0.0, 2.0),
        CP_COL: (0.0, 1.0),
        CF_COL: (0.0, 0.0),

        LAC_COL: (2.6, 3.0),
        ACE_COL: (2.6, 2.0),
        BUT_COL: (2.6, 1.0),
        PH_COL: (2.6, 0.0),

        PP_COL: (5.4, 1.8),
        OE_COL: (5.4, 0.8),
        KE_COL: (7.2, 0.8),
    }

    present_nodes = set(G.nodes())
    pos = {k: v for k, v in pos.items() if k in present_nodes}

    fig, ax = plt.subplots(figsize=(12, 7))

    group_boxes = [
        (-0.7, -0.8, 1.5, 4.2, 'Состав корма'),
        (1.9, -0.8, 1.5, 4.2, 'Брожение'),
        (4.6, -0.2, 3.2, 2.7, 'Результаты'),
    ]
    for x, y, w, h, title in group_boxes:
        rect = mpatches.FancyBboxPatch(
            (x, y), w, h,
            boxstyle='round,pad=0.03,rounding_size=0.08',
            linewidth=1.2,
            facecolor='none',
            edgecolor='0.5',
            linestyle='dotted'
        )
        ax.add_patch(rect)
        ax.text(x + w / 2, y + h + 0.08, title, ha='center', va='bottom', fontsize=11)

    def add_arrow(start, end, sign='+', rad=0.0):
        style = '-' if sign == '+' else '--'
        arrow = FancyArrowPatch(
            start, end,
            arrowstyle='-|>',
            mutation_scale=18,
            lw=1.7,
            linestyle=style,
            color='black',
            shrinkA=28,
            shrinkB=28,
            connectionstyle=f'arc3,rad={rad}',
            zorder=1
        )
        ax.add_patch(arrow)

    rad_map = {
        (LAC_COL, PH_COL): 0.05,
        (ACE_COL, PH_COL): 0.00,
        (BUT_COL, PH_COL): -0.05,
        (CF_COL, OE_COL): -0.05,
        (PH_COL, OE_COL): 0.05,
    }

    for u, v, d in G.edges(data=True):
        if u not in pos or v not in pos:
            continue
        add_arrow(pos[u], pos[v], sign=d['sign'], rad=rad_map.get((u, v), 0.0))

    nx.draw_networkx_nodes(
        G, pos,
        node_size=2600,
        node_color='white',
        edgecolors='black',
        linewidths=1.3,
        node_shape='s',
        ax=ax
    )
    nx.draw_networkx_labels(
        G, pos,
        labels={node: display_names.get(node, node) for node in G.nodes},
        font_size=10,
        ax=ax
    )

    plus_line = plt.Line2D([0], [0], color='black', lw=1.7, linestyle='-', marker='>', markersize=8)
    minus_line = plt.Line2D([0], [0], color='black', lw=1.7, linestyle='--', marker='>', markersize=8)

    ax.legend(
        [plus_line, minus_line],
        ['Сплошная стрелка — положительная связь', 'Пунктирная стрелка — отрицательная связь'],
        loc='lower center',
        bbox_to_anchor=(0.5, -0.08),
        ncol=2,
        frameon=False,
        fontsize=10
    )

    ax.set_title(
        'Причинно-следственный граф факторов, влияющих\n'
        'на качество консервированных кормов растительного происхождения',
        fontsize=14,
        pad=16
    )
    ax.set_axis_off()
    plt.tight_layout()
    plt.show()

def causal_models_extended(df: pd.DataFrame, out_dirs):
    work = df.copy()
    work['high_quality'] = (work[QUALITY_COL].astype(int) == 1).astype(int)
    work['bad_quality'] = (work[QUALITY_COL].astype(int) == 3).astype(int)
    work['quality_ord'] = work[QUALITY_COL].astype(int) - 1
    work['доля_молочной_расчет2'] = work[LAC_SHARE_CALC_COL]
    formulas = {
        'causal_lactic': f'Q("{LAC_COL}") ~ Q("{SV_COL}") + Q("{SUGAR_COL}") + Q("{CP_COL}") + Q("{CF_COL}") + C(Q("{TYPE_COL}")) + C(Q("{YEAR_CAT_COL}"))',
        'causal_butyric_dm': f'Q("{BUT_DM_COL}") ~ Q("{SV_COL}") + Q("{SUGAR_COL}") + Q("{CP_COL}") + Q("{CF_COL}") + C(Q("{TYPE_COL}")) + C(Q("{YEAR_CAT_COL}"))',
        'causal_ph': f'Q("{PH_COL}") ~ Q("{LAC_COL}") + Q("{ACE_COL}") + Q("{BUT_COL}") + Q("{SV_COL}") + C(Q("{TYPE_COL}")) + C(Q("{YEAR_CAT_COL}"))',
        'causal_oe': f'Q("{OE_COL}") ~ Q("{CP_COL}") + Q("{CF_COL}") + Q("{PH_COL}") + Q("{BUT_DM_COL}") + C(Q("{TYPE_COL}")) + C(Q("{YEAR_CAT_COL}"))',
        'causal_ke': f'Q("{KE_COL}") ~ Q("{OE_COL}") + Q("{CF_COL}") + Q("{PH_COL}") + C(Q("{TYPE_COL}")) + C(Q("{YEAR_CAT_COL}"))',
        'causal_high_quality_logit': f'high_quality ~ Q("{PH_COL}") + Q("{LAC_SHARE_CALC_COL}") + Q("{BUT_DM_COL}") + C(Q("{TYPE_COL}")) + C(Q("{YEAR_CAT_COL}"))',
        'causal_bad_quality_logit': f'bad_quality ~ Q("{PH_COL}") + Q("{LAC_SHARE_CALC_COL}") + Q("{BUT_DM_COL}") + C(Q("{TYPE_COL}")) + C(Q("{YEAR_CAT_COL}"))',
    }
    rows = []
    for name, formula in formulas.items():
        if 'logit' in name:
            model = smf.logit(formula, data=work).fit(disp=0)
            ci = model.conf_int()
            coef = pd.DataFrame({
                'term': model.params.index,
                'coef': model.params.values,
                'std_err': model.bse.values,
                'p_value': model.pvalues.values,
                'ci_low': ci[0].values,
                'ci_high': ci[1].values,
            })
            coef['odds_ratio'] = np.exp(coef['coef'])
            coef['or_ci_low'] = np.exp(coef['ci_low'])
            coef['or_ci_high'] = np.exp(coef['ci_high'])
            rows.extend([
                {'model': name, 'metric': 'pseudo_r2_mcfadden', 'value': 1 - model.llf / model.llnull},
                {'model': name, 'metric': 'aic', 'value': model.aic},
            ])
        else:
            model = smf.ols(formula, data=work).fit()
            ci = model.conf_int()
            coef = pd.DataFrame({
                'term': model.params.index,
                'coef': model.params.values,
                'std_err': model.bse.values,
                'p_value': model.pvalues.values,
                'ci_low': ci[0].values,
                'ci_high': ci[1].values,
            })
            rows.extend([
                {'model': name, 'metric': 'r2', 'value': model.rsquared},
                {'model': name, 'metric': 'adj_r2', 'value': model.rsquared_adj},
                {'model': name, 'metric': 'aic', 'value': model.aic},
            ])
        coef.to_csv(out_dirs['tables'] / f'{name}_coefficients.csv', index=False, encoding='utf-8-sig')
    ord_exog = pd.get_dummies(
        work[[PH_COL, LAC_SHARE_CALC_COL, BUT_DM_COL, OE_COL, TYPE_COL, YEAR_CAT_COL]],
        drop_first=True,
    ).astype(float)
    ordered_model = OrderedModel(work['quality_ord'], ord_exog, distr='logit').fit(method='bfgs', disp=False)
    ci = ordered_model.conf_int()
    coef = pd.DataFrame({
        'term': ordered_model.params.index,
        'coef': ordered_model.params.values,
        'std_err': ordered_model.bse.values,
        'p_value': ordered_model.pvalues.values,
        'ci_low': ci[0].values,
        'ci_high': ci[1].values,
    })
    coef.to_csv(out_dirs['tables'] / 'causal_quality_ordered_logit_coefficients.csv', index=False, encoding='utf-8-sig')
    rows.extend([
        {'model': 'causal_quality_ordered_logit', 'metric': 'aic', 'value': ordered_model.aic},
        {'model': 'causal_quality_ordered_logit', 'metric': 'bic', 'value': ordered_model.bic},
    ])
    summary = pd.DataFrame(rows)
    summary.to_csv(out_dirs['tables'] / 'causal_models_summary.csv', index=False, encoding='utf-8-sig')
    return summary
def benchmark_vs_ml(df: pd.DataFrame, out_dirs):
    interp_features = [SV_COL, SUGAR_COL, CP_COL, CF_COL, PH_COL, LAC_COL, ACE_COL, BUT_COL, TYPE_COL, YEAR_CAT_COL]
    practical_features = interp_features + [PP_COL, OE_COL, KE_COL]
    interp_summary, _ = evaluate_classifiers(df, interp_features, 'interpretable_quality', out_dirs)
    practical_summary, _ = evaluate_classifiers(df, practical_features, 'practical_quality', out_dirs)
    rf_best = practical_summary.loc[practical_summary['model'] == 'random_forest'].copy()
    rf_best.insert(0, 'method', 'Random Forest')
    rf_best = rf_best.rename(columns={'scenario': 'scenario_name'})
    return interp_summary, practical_summary, rf_best
def build_summary_text(df, acid_df, group_kr, oe_ke_df, methods_df, practical_summary, causal_summary, fermentation_counts):
    rf_row = practical_summary.sort_values(['weighted_f1', 'accuracy'], ascending=False).iloc[0]
    flieg_row = methods_df.loc[methods_df['method'] == 'индекс_Флига'].iloc[0] if 'индекс_Флига' in methods_df['method'].values else None
    dlg_row = methods_df.loc[methods_df['method'] == 'DLG'].iloc[0] if 'DLG' in methods_df['method'].values else None
    gost_row = methods_df.loc[methods_df['method'] == 'ГОСТ'].iloc[0] if 'ГОСТ' in methods_df['method'].values else None
    f_map = {r['criterion']: (r['count_true'], r['share_true']) for _, r in fermentation_counts.iterrows()} if fermentation_counts is not None and not fermentation_counts.empty else {}
    lines = []
    lines.append('Краткое резюме по всем задачам')
    lines.append('=============================')
    lines.append(f'В анализ вошло {len(df)} проб после удаления строк без экспертной оценки и медианной обработки пропусков.')
    lines.append('По задаче скрытых факторов были построены две линейные регрессионные модели для молочной и масляной кислоты: простая по СВ и pH и расширенная по СВ, pH, сахару, сырому протеину, сырой клетчатке, типу силоса и году.')
    for _, r in acid_df.iterrows():
        label = 'простая' if r['model'] == 'simple' else 'расширенная'
        target_name = 'молочной кислоты' if r['target'] == LAC_COL else 'масляной кислоты'
        lines.append(f'Модель для {target_name} ({label}): R2={r["r2"]:.3f}, MAE={r["mae"]:.3f}, RMSE={r["rmse"]:.3f}.')
    lines.append('Полученные значения качества аппроксимации показывают, что кислоты по этим показателям предсказываются слабо, то есть на брожение, вероятно, влияют дополнительные неучтённые факторы.')
    if f_map:
        c1, s1 = f_map.get('правило_масляная_>0_3_от_св', (0, 0))
        c2, s2 = f_map.get('правило_70_30_без_масляной', (0, 0))
        c3, s3 = f_map.get('правило_55_60_без_масляной', (0, 0))
        c4, s4 = f_map.get('правило_строгое_оба', (0, 0))
        lines.append(f'По блоку брожения и порчи правило по масляной кислоте > 0,3% от СВ выполняется для {c1} проб ({s1:.3f}), правило соотношения молочной и уксусной кислот 70:30 при отсутствии масляной кислоты — для {c2} проб ({s2:.3f}), условие по молочной кислоте 55–60% при отсутствии масляной — для {c3} проб ({s3:.3f}), а одновременное выполнение обоих условий — для {c4} проб ({s4:.3f}).')
    top_group = group_kr.nsmallest(6, 'p_value')[['variable', 'p_value']]
    lines.append('По задаче сравнения групп силоса наиболее выраженные различия получены по следующим показателям: ' + ', '.join(top_group['variable'].tolist()) + '.')
    for _, r in oe_ke_df.iterrows():
        lines.append(f'Модель для {r["target"]}: R2={r["r2"]:.3f}, MAE={r["mae"]:.3f}, RMSE={r["rmse"]:.3f}.')
    lines.append('По задаче повышения питательности регрессионные модели показывают, что для роста ОЭ и КЕ наибольшее значение имеют снижение клетчатки, поддержание оптимального СВ и предотвращение неблагоприятного брожения.')
    lines.append(f'По задаче диагностики качества лучшая модель — {rf_row["model"]} ({rf_row["scenario"]}): accuracy={rf_row["accuracy"]:.3f}, weighted F1={rf_row["weighted_f1"]:.3f}.')
    if flieg_row is not None:
        lines.append(f'Из формальных методов лучшим оказался индекс Флига: accuracy={flieg_row["accuracy"]:.3f}, weighted F1={flieg_row["weighted_f1"]:.3f}.')
    if dlg_row is not None:
        lines.append(f'DLG: accuracy={dlg_row["accuracy"]:.3f}, weighted F1={dlg_row["weighted_f1"]:.3f}.')
    if gost_row is not None:
        lines.append(f'ГОСТ: accuracy={gost_row["accuracy"]:.3f}, weighted F1={gost_row["weighted_f1"]:.3f}.')
    if not causal_summary.empty:
        lines.append('Дополнительно были построены причинно-следственные модели по DAG, включая OLS-модели для кислот, pH, ОЭ и КЕ, а также logit/ordered logit модели для качества.')
    return '\n'.join(lines)

import statsmodels.api as sm
from sklearn.model_selection import train_test_split
from sklearn.metrics import (
    r2_score, mean_absolute_error, mean_squared_error,
    accuracy_score, precision_score, recall_score, f1_score
)
from sklearn.ensemble import RandomForestRegressor, RandomForestClassifier, HistGradientBoostingRegressor
from sklearn.linear_model import LogisticRegression


def _optional_year_col(df: pd.DataFrame):
    candidates = []
    if 'YEAR_CAT_COL' in globals():
        try:
            candidates.append(globals()['YEAR_CAT_COL'])
        except Exception:
            pass
    candidates += ['Год', 'год', 'year', 'Year']
    for c in candidates:
        if c in df.columns:
            return c
    return None


def add_hidden_factor_features(df: pd.DataFrame) -> pd.DataFrame:
    out = df.copy()

    # Флаг кукурузного силоса
    out['is_corn_silage'] = (
        out[TYPE_COL].astype(str).str.contains('кукуруз', case=False, na=False)
    ).astype(int)

    # Если нет масляной кислоты в % от СВ — считаем
    but_dm_col = 'масляная_кислота_проц_от_св'
    if but_dm_col not in out.columns and BUT_COL in out.columns and SV_COL in out.columns:
        sv_safe = out[SV_COL].replace(0, np.nan)
        out[but_dm_col] = out[BUT_COL] * 100.0 / sv_safe

    # Порог по СВ < 30%
    out['sv_lt_30'] = (out[SV_COL] < 30).astype(int)

    # Квадраты
    out['sv_sq'] = out[SV_COL] ** 2
    out['ph_sq'] = out[PH_COL] ** 2
    out['protein_sq'] = out[CP_COL] ** 2
    out['sugar_sq'] = out[SUGAR_COL] ** 2
    out['fiber_sq'] = out[CF_COL] ** 2

    # Взаимодействия
    out['sv_x_ph'] = out[SV_COL] * out[PH_COL]
    out['ph_x_protein'] = out[PH_COL] * out[CP_COL]
    out['sv_x_sugar'] = out[SV_COL] * out[SUGAR_COL]
    out['protein_x_sugar'] = out[CP_COL] * out[SUGAR_COL]
    out['protein_x_fiber'] = out[CP_COL] * out[CF_COL]
    out['ph_x_lactic'] = out[PH_COL] * out[LAC_COL]
    out['ph_x_butyric'] = out[PH_COL] * out[BUT_COL]

    # Лог-преобразования для более устойчивых моделей
    out['log_sugar'] = np.log1p(out[SUGAR_COL].clip(lower=0))
    out['log_butyric'] = np.log1p(out[BUT_COL].clip(lower=0))
    out['log_lactic'] = np.log1p(out[LAC_COL].clip(lower=0))

    # Бинарная цель для неблагоприятного брожения
    if but_dm_col in out.columns:
        out['bad_fermentation_flag'] = (out[but_dm_col] > 0.3).astype(int)
    else:
        out['bad_fermentation_flag'] = np.nan

    return out


def _build_design_matrix(
    df: pd.DataFrame,
    numeric_features: list[str],
    include_type: bool = True,
    include_year: bool = True
) -> pd.DataFrame:
    X = df[numeric_features].copy()

    if include_type and TYPE_COL in df.columns:
        type_dummies = pd.get_dummies(
            df[TYPE_COL].astype(str),
            prefix='type',
            drop_first=True,
            dtype=float
        )
        X = pd.concat([X, type_dummies], axis=1)

    year_col = _optional_year_col(df)
    if include_year and year_col is not None:
        if pd.api.types.is_numeric_dtype(df[year_col]):
            X[year_col] = pd.to_numeric(df[year_col], errors='coerce')
        else:
            year_dummies = pd.get_dummies(
                df[year_col].astype(str),
                prefix='year',
                drop_first=True,
                dtype=float
            )
            X = pd.concat([X, year_dummies], axis=1)

    return X


def _reg_metrics(y_true, y_pred) -> dict:
    return {
        'r2': float(r2_score(y_true, y_pred)),
        'mae': float(mean_absolute_error(y_true, y_pred)),
        'rmse': float(np.sqrt(mean_squared_error(y_true, y_pred))),
    }


def _clf_metrics(y_true, y_pred) -> dict:
    return {
        'accuracy': float(accuracy_score(y_true, y_pred)),
        'precision': float(precision_score(y_true, y_pred, zero_division=0)),
        'recall': float(recall_score(y_true, y_pred, zero_division=0)),
        'f1': float(f1_score(y_true, y_pred, zero_division=0)),
    }


def fit_ols_logtarget_holdout(
    df: pd.DataFrame,
    feature_cols: list[str],
    target_col: str,
    model_name: str,
    subgroup_name: str = 'all',
    cv_splits: int = 5,
    random_state: int = 42,
) -> dict:
    work = df[feature_cols + [target_col]].dropna().copy()

    if len(work) < 40:
        return {
            'target': target_col,
            'model': model_name,
            'subgroup': subgroup_name,
            'n': len(work),
            'r2': np.nan,
            'mae': np.nan,
            'rmse': np.nan,
            'adj_r2': np.nan,
            'aic': np.nan,
        }

    X = work[feature_cols].astype(float).reset_index(drop=True)
    y = work[target_col].astype(float).reset_index(drop=True)

    y_values = y.to_numpy(dtype=float)
    y_log = np.log1p(np.clip(y_values, a_min=0.0, a_max=None))

    n_splits = min(cv_splits, len(work))
    if n_splits < 3:
        return {
            'target': target_col,
            'model': model_name,
            'subgroup': subgroup_name,
            'n': len(work),
            'r2': np.nan,
            'mae': np.nan,
            'rmse': np.nan,
            'adj_r2': np.nan,
            'aic': np.nan,
        }

    kf = KFold(n_splits=n_splits, shuffle=True, random_state=random_state)
    oof_pred = np.full(len(work), np.nan, dtype=float)

    for train_idx, test_idx in kf.split(X):
        X_train = sm.add_constant(X.iloc[train_idx], has_constant='add')
        X_test = sm.add_constant(X.iloc[test_idx], has_constant='add')
        X_test = X_test.reindex(columns=X_train.columns, fill_value=0.0)

        model = sm.OLS(y_log[train_idx], X_train).fit()
        pred_log = model.predict(X_test)
        pred = np.clip(np.expm1(np.asarray(pred_log, dtype=float)), a_min=0.0, a_max=None)

        oof_pred[test_idx] = pred

    m = _reg_metrics(y_values, oof_pred)

    return {
        'target': target_col,
        'model': model_name,
        'subgroup': subgroup_name,
        'n': len(work),
        'r2': m['r2'],
        'mae': m['mae'],
        'rmse': m['rmse'],
        'adj_r2': np.nan,
        'aic': np.nan,
    }


def fit_ml_regressor_holdout_logtarget(
    df: pd.DataFrame,
    feature_cols: list[str],
    target_col: str,
    model_name: str,
    estimator,
    subgroup_name: str = 'all',
    cv_splits: int = 5,
    random_state: int = 42,
) -> dict:
    work = df[feature_cols + [target_col]].dropna().copy()

    if len(work) < 40:
        return {
            'target': target_col,
            'model': model_name,
            'subgroup': subgroup_name,
            'n': len(work),
            'r2': np.nan,
            'mae': np.nan,
            'rmse': np.nan,
            'adj_r2': np.nan,
            'aic': np.nan,
        }

    X = work[feature_cols].astype(float).reset_index(drop=True)
    y = work[target_col].astype(float).reset_index(drop=True)

    y_values = y.to_numpy(dtype=float)
    y_log = np.log1p(np.clip(y_values, a_min=0.0, a_max=None))

    n_splits = min(cv_splits, len(work))
    if n_splits < 3:
        return {
            'target': target_col,
            'model': model_name,
            'subgroup': subgroup_name,
            'n': len(work),
            'r2': np.nan,
            'mae': np.nan,
            'rmse': np.nan,
            'adj_r2': np.nan,
            'aic': np.nan,
        }

    kf = KFold(n_splits=n_splits, shuffle=True, random_state=random_state)
    oof_pred = np.full(len(work), np.nan, dtype=float)

    for train_idx, test_idx in kf.split(X):
        est = clone(estimator)
        est.fit(X.iloc[train_idx], y_log[train_idx])

        pred_log = est.predict(X.iloc[test_idx])
        pred = np.clip(np.expm1(np.asarray(pred_log, dtype=float)), a_min=0.0, a_max=None)

        oof_pred[test_idx] = pred

    m = _reg_metrics(y_values, oof_pred)

    return {
        'target': target_col,
        'model': model_name,
        'subgroup': subgroup_name,
        'n': len(work),
        'r2': m['r2'],
        'mae': m['mae'],
        'rmse': m['rmse'],
        'adj_r2': np.nan,
        'aic': np.nan,
    }


def fit_binary_classifier_holdout(
    df: pd.DataFrame,
    feature_cols: list[str],
    target_col: str,
    model_name: str,
    estimator,
    subgroup_name: str = 'all',
    cv_splits: int = 5,
    random_state: int = 42,
) -> dict:
    work = df[feature_cols + [target_col]].dropna().copy()

    if len(work) < 30 or work[target_col].nunique() < 2:
        return {
            'target': target_col,
            'model': model_name,
            'subgroup': subgroup_name,
            'n': len(work),
            'positive_rate': np.nan,
            'accuracy': np.nan,
            'precision': np.nan,
            'recall': np.nan,
            'f1': np.nan,
        }

    X = work[feature_cols].astype(float).reset_index(drop=True)
    y = work[target_col].astype(int).reset_index(drop=True)

    min_class_count = int(y.value_counts().min())
    n_splits = min(cv_splits, min_class_count)

    if n_splits < 2:
        return {
            'target': target_col,
            'model': model_name,
            'subgroup': subgroup_name,
            'n': len(work),
            'positive_rate': float(y.mean()),
            'accuracy': np.nan,
            'precision': np.nan,
            'recall': np.nan,
            'f1': np.nan,
        }

    skf = StratifiedKFold(n_splits=n_splits, shuffle=True, random_state=random_state)
    oof_pred = np.full(len(work), -1, dtype=int)

    for train_idx, test_idx in skf.split(X, y):
        est = clone(estimator)
        est.fit(X.iloc[train_idx], y.iloc[train_idx])
        pred = est.predict(X.iloc[test_idx])
        oof_pred[test_idx] = pred

    m = _clf_metrics(y.to_numpy(dtype=int), oof_pred)

    return {
        'target': target_col,
        'model': model_name,
        'subgroup': subgroup_name,
        'n': len(work),
        'positive_rate': float(y.mean()),
        'accuracy': m['accuracy'],
        'precision': m['precision'],
        'recall': m['recall'],
        'f1': m['f1'],
    }

def fit_ols_fullsample(
    df: pd.DataFrame,
    feature_cols: list[str],
    target_col: str,
    model_name: str,
    subgroup_name: str = 'all'
) -> dict:
    work = df[feature_cols + [target_col]].dropna().copy()

    if len(work) < 20:
        return {
            'target': target_col,
            'model': model_name,
            'subgroup': subgroup_name,
            'n': len(work),
            'r2': np.nan,
            'mae': np.nan,
            'rmse': np.nan,
            'adj_r2': np.nan,
            'aic': np.nan,
        }

    X = sm.add_constant(work[feature_cols].astype(float), has_constant='add')
    y = work[target_col].astype(float)

    model = sm.OLS(y, X).fit()
    pred = model.predict(X)
    m = _reg_metrics(y, pred)

    return {
        'target': target_col,
        'model': model_name,
        'subgroup': subgroup_name,
        'n': len(work),
        'r2': m['r2'],
        'mae': m['mae'],
        'rmse': m['rmse'],
        'adj_r2': float(model.rsquared_adj),
        'aic': float(model.aic),
    }


def fit_ml_regressor_holdout(
    df: pd.DataFrame,
    feature_cols: list[str],
    target_col: str,
    model_name: str,
    estimator,
    subgroup_name: str = 'all',
    cv_splits: int = 5,
    random_state: int = 42,
) -> dict:
    work = df[feature_cols + [target_col]].dropna().copy()

    if len(work) < 30:
        return {
            'target': target_col,
            'model': model_name,
            'subgroup': subgroup_name,
            'n': len(work),
            'r2': np.nan,
            'mae': np.nan,
            'rmse': np.nan,
            'adj_r2': np.nan,
            'aic': np.nan,
        }

    X = work[feature_cols].astype(float).reset_index(drop=True)
    y = work[target_col].astype(float).reset_index(drop=True)

    n_splits = min(cv_splits, len(work))
    if n_splits < 3:
        return {
            'target': target_col,
            'model': model_name,
            'subgroup': subgroup_name,
            'n': len(work),
            'r2': np.nan,
            'mae': np.nan,
            'rmse': np.nan,
            'adj_r2': np.nan,
            'aic': np.nan,
        }

    kf = KFold(n_splits=n_splits, shuffle=True, random_state=random_state)
    oof_pred = np.full(len(work), np.nan, dtype=float)

    for train_idx, test_idx in kf.split(X):
        est = clone(estimator)
        est.fit(X.iloc[train_idx], y.iloc[train_idx])
        pred = est.predict(X.iloc[test_idx])
        oof_pred[test_idx] = np.asarray(pred, dtype=float)

    m = _reg_metrics(y.to_numpy(dtype=float), oof_pred)

    return {
        'target': target_col,
        'model': model_name,
        'subgroup': subgroup_name,
        'n': len(work),
        'r2': m['r2'],
        'mae': m['mae'],
        'rmse': m['rmse'],
        'adj_r2': np.nan,
        'aic': np.nan,
    }

def run_extended_hidden_factor_models(df: pd.DataFrame) -> pd.DataFrame:
    """
    Новые модели для молочной и масляной кислот:
    - OLS с взаимодействиями
    - OLS с нелинейностями
    - RF / HistGB
    - отдельно corn / non-corn
    """
    out_rows = []

    # Базовые признаки для новых моделей
    base = [SV_COL, PH_COL, SUGAR_COL, CP_COL, CF_COL]
    interactions = base + [
        'sv_x_ph', 'ph_x_protein', 'sv_x_sugar',
        'protein_x_sugar', 'protein_x_fiber'
    ]
    nonlinear = interactions + [
        'sv_lt_30', 'sv_sq', 'ph_sq', 'protein_sq', 'sugar_sq', 'fiber_sq'
    ]

    X_inter = _build_design_matrix(df, interactions, include_type=True, include_year=True)
    X_nonlin = _build_design_matrix(df, nonlinear, include_type=True, include_year=True)

    df_inter = pd.concat([df.reset_index(drop=True), X_inter.reset_index(drop=True)], axis=1)
    df_nonlin = pd.concat([df.reset_index(drop=True), X_nonlin.reset_index(drop=True)], axis=1)

    inter_cols = X_inter.columns.tolist()
    nonlin_cols = X_nonlin.columns.tolist()

    targets = [LAC_COL, BUT_COL]

    for target in targets:
        # Все данные
        out_rows.append(fit_ols_fullsample(df_inter, inter_cols, target, f'ols_interactions_{target}', 'all'))
        out_rows.append(fit_ols_fullsample(df_nonlin, nonlin_cols, target, f'ols_nonlinear_{target}', 'all'))

        out_rows.append(
            fit_ml_regressor_holdout(
                df_nonlin, nonlin_cols, target,
                f'random_forest_{target}',
                RandomForestRegressor(
                    n_estimators=400,
                    min_samples_leaf=2,
                    random_state=42,
                    n_jobs=-1
                ),
                'all'
            )
        )
        out_rows.append(
            fit_ml_regressor_holdout(
                df_nonlin, nonlin_cols, target,
                f'hist_gb_{target}',
                HistGradientBoostingRegressor(
                    max_depth=4,
                    learning_rate=0.05,
                    max_iter=400,
                    random_state=42
                ),
                'all'
            )
        )

        # Отдельно кукурузный и некукурузный силос
        for subgroup_name, mask in {
            'corn_only': df_nonlin['is_corn_silage'] == 1,
            'non_corn_only': df_nonlin['is_corn_silage'] == 0,
        }.items():
            sub = df_nonlin.loc[mask].copy()
            out_rows.append(fit_ols_fullsample(sub, nonlin_cols, target, f'ols_nonlinear_{target}', subgroup_name))
            out_rows.append(
                fit_ml_regressor_holdout(
                    sub, nonlin_cols, target,
                    f'random_forest_{target}',
                    RandomForestRegressor(
                        n_estimators=300,
                        min_samples_leaf=2,
                        random_state=42,
                        n_jobs=-1
                    ),
                    subgroup_name
                )
            )

    return pd.DataFrame(out_rows)


def run_bad_fermentation_models(df: pd.DataFrame) -> pd.DataFrame:
    """
    Отдельная бинарная постановка:
    bad_fermentation_flag = 1, если масляная кислота > 0.3% от СВ
    """
    base = [SV_COL, PH_COL, SUGAR_COL, CP_COL, CF_COL, LAC_COL, ACE_COL, BUT_COL]
    ext = base + [
        'sv_lt_30', 'sv_sq', 'ph_sq', 'protein_sq', 'sugar_sq',
        'sv_x_ph', 'ph_x_protein', 'sv_x_sugar', 'protein_x_sugar', 'protein_x_fiber'
    ]

    X = _build_design_matrix(df, ext, include_type=True, include_year=True)
    work = pd.concat([df.reset_index(drop=True), X.reset_index(drop=True)], axis=1)
    feature_cols = X.columns.tolist()

    out_rows = []
    out_rows.append(
        fit_binary_classifier_holdout(
            work, feature_cols, 'bad_fermentation_flag',
            'logistic_bad_fermentation',
            LogisticRegression(
                max_iter=5000,
                class_weight='balanced',
                random_state=42
            ),
            'all'
        )
    )
    out_rows.append(
        fit_binary_classifier_holdout(
            work, feature_cols, 'bad_fermentation_flag',
            'rf_bad_fermentation',
            RandomForestClassifier(
                n_estimators=400,
                min_samples_leaf=2,
                class_weight='balanced',
                random_state=42,
                n_jobs=-1
            ),
            'all'
        )
    )

    for subgroup_name, mask in {
        'corn_only': work['is_corn_silage'] == 1,
        'non_corn_only': work['is_corn_silage'] == 0,
    }.items():
        sub = work.loc[mask].copy()
        out_rows.append(
            fit_binary_classifier_holdout(
                sub, feature_cols, 'bad_fermentation_flag',
                'rf_bad_fermentation',
                RandomForestClassifier(
                    n_estimators=300,
                    min_samples_leaf=2,
                    class_weight='balanced',
                    random_state=42,
                    n_jobs=-1
                ),
                subgroup_name
            )
        )

    return pd.DataFrame(out_rows)


def build_dag_edges_verified() -> pd.DataFrame:
    rows = [
        {'Источник': SV_COL, 'Приемник': PH_COL, 'Знак связи': '-'},
        {'Источник': SV_COL, 'Приемник': BUT_COL, 'Знак связи': '-'},
        {'Источник': SUGAR_COL, 'Приемник': LAC_COL, 'Знак связи': '+'},
        {'Источник': SUGAR_COL, 'Приемник': ACE_COL, 'Знак связи': '+'},
        {'Источник': CP_COL, 'Приемник': PP_COL, 'Знак связи': '+'},
        {'Источник': CF_COL, 'Приемник': PP_COL, 'Знак связи': '-'},
        {'Источник': CF_COL, 'Приемник': OE_COL, 'Знак связи': '-'},
        {'Источник': CP_COL, 'Приемник': OE_COL, 'Знак связи': '+'},
        {'Источник': PH_COL, 'Приемник': OE_COL, 'Знак связи': '-'},
        {'Источник': LAC_COL, 'Приемник': PH_COL, 'Знак связи': '-'},
        {'Источник': ACE_COL, 'Приемник': PH_COL, 'Знак связи': '-'},
        {'Источник': BUT_COL, 'Приемник': PH_COL, 'Знак связи': '+'},
        {'Источник': OE_COL, 'Приемник': KE_COL, 'Знак связи': '+'},
    ]
    return pd.DataFrame(rows)


def append_hidden_factor_summary(
    summary_text: str,
    hidden_ext_df: pd.DataFrame,
    bad_ferm_df: pd.DataFrame
) -> str:
    lines = [summary_text, '', 'Дополнительная проверка по замечанию научного руководителя']

    if not hidden_ext_df.empty:
        best_lac = hidden_ext_df.loc[hidden_ext_df['target'] == LAC_COL].sort_values(
            ['r2', 'mae'], ascending=[False, True]
        ).head(1)
        best_but = hidden_ext_df.loc[hidden_ext_df['target'] == BUT_COL].sort_values(
            ['r2', 'mae'], ascending=[False, True]
        ).head(1)

        if not best_lac.empty:
            r = best_lac.iloc[0]
            lines.append(
                f"Лучшая новая модель для молочной кислоты: {r['model']} / {r['subgroup']} "
                f"(R2={r['r2']:.3f}, MAE={r['mae']:.3f}, RMSE={r['rmse']:.3f})."
            )
        if not best_but.empty:
            r = best_but.iloc[0]
            lines.append(
                f"Лучшая новая модель для масляной кислоты: {r['model']} / {r['subgroup']} "
                f"(R2={r['r2']:.3f}, MAE={r['mae']:.3f}, RMSE={r['rmse']:.3f})."
            )

    if not bad_ferm_df.empty:
        best_bad = bad_ferm_df.sort_values(['f1', 'recall', 'precision'], ascending=False).head(1)
        if not best_bad.empty:
            r = best_bad.iloc[0]
            lines.append(
                f"Лучшая модель для выявления неблагоприятного брожения "
                f"(масляная кислота > 0.3% от СВ): {r['model']} / {r['subgroup']} "
                f"(accuracy={r['accuracy']:.3f}, precision={r['precision']:.3f}, "
                f"recall={r['recall']:.3f}, F1={r['f1']:.3f})."
            )

    return '\n'.join(lines)

def _hidden_feature_columns() -> list[str]:
    return [
        SV_COL, PH_COL, SUGAR_COL, CP_COL, CF_COL,
        'sv_lt_30',
        'sv_sq', 'ph_sq', 'protein_sq', 'sugar_sq', 'fiber_sq',
        'sv_x_ph', 'ph_x_protein', 'sv_x_sugar',
        'protein_x_sugar', 'protein_x_fiber',
    ]


def _subgroup_masks(df: pd.DataFrame) -> dict[str, pd.Series]:
    masks = {
        'all': pd.Series(True, index=df.index),
        'corn_only': df['is_corn_silage'] == 1,
        'non_corn_only': df['is_corn_silage'] == 0,
    }

    # Дополнительно — по каждому типу силоса
    for silage_type in df[TYPE_COL].dropna().astype(str).unique():
        key = f"type::{silage_type}"
        masks[key] = df[TYPE_COL].astype(str) == silage_type

    return masks


def run_hidden_factor_models_plus(df: pd.DataFrame) -> pd.DataFrame:
    """
    Ещё более сильный блок скрытых факторов:
    - молочная кислота при натуральной влажности
    - молочная кислота в % от СВ
    - доля молочной кислоты
    - масляная кислота при натуральной влажности
    - масляная кислота в % от СВ
    - по всем данным, corn/non-corn, и по каждому типу силоса
    """
    feature_base = _hidden_feature_columns()
    X = _build_design_matrix(df, feature_base, include_type=True, include_year=True)
    work = pd.concat([df.reset_index(drop=True), X.reset_index(drop=True)], axis=1)
    feature_cols = X.columns.tolist()

    target_specs = [
        (LAC_COL, 'lactic_nat'),
        (LAC_DM_COL, 'lactic_dm'),
        (LAC_SHARE_CALC_COL, 'lactic_share'),
        (BUT_COL, 'butyric_nat'),
        (BUT_DM_COL, 'butyric_dm'),
    ]

    subgroup_masks = _subgroup_masks(work)
    out_rows = []

    for target_col, target_alias in target_specs:
        if target_col not in work.columns:
            continue

        for subgroup_name, mask in subgroup_masks.items():
            sub = work.loc[mask].copy()

            # маленькие группы пропускаем
            if sub[target_col].dropna().shape[0] < 40:
                continue

            out_rows.append(
                fit_ols_logtarget_holdout(
                    sub, feature_cols, target_col,
                    f'ols_log_{target_alias}',
                    subgroup_name
                )
            )

            out_rows.append(
                fit_ml_regressor_holdout_logtarget(
                    sub, feature_cols, target_col,
                    f'hist_gb_log_{target_alias}',
                    HistGradientBoostingRegressor(
                        max_depth=4,
                        learning_rate=0.05,
                        max_iter=400,
                        random_state=42
                    ),
                    subgroup_name
                )
            )

            out_rows.append(
                fit_ml_regressor_holdout_logtarget(
                    sub, feature_cols, target_col,
                    f'rf_log_{target_alias}',
                    RandomForestRegressor(
                        n_estimators=400,
                        min_samples_leaf=2,
                        random_state=42,
                        n_jobs=-1
                    ),
                    subgroup_name
                )
            )

    return pd.DataFrame(out_rows)


def run_butyric_two_stage_models(df: pd.DataFrame) -> tuple[pd.DataFrame, pd.DataFrame]:
    """
    Двухэтапная схема для масляной кислоты:
    1) классификация bad_fermentation_flag
    2) регрессия BUT_DM_COL только на положительных случаях
    """
    feature_base = _hidden_feature_columns()
    X = _build_design_matrix(df, feature_base, include_type=True, include_year=True)
    work = pd.concat([df.reset_index(drop=True), X.reset_index(drop=True)], axis=1)
    feature_cols = X.columns.tolist()

    subgroup_masks = _subgroup_masks(work)

    stage1_rows = []
    stage2_rows = []

    for subgroup_name, mask in subgroup_masks.items():
        sub = work.loc[mask].copy()

        # классификация риска неблагоприятного брожения 
        stage1_rows.append(
            fit_binary_classifier_holdout(
                sub,
                feature_cols,
                'bad_fermentation_flag',
                'logistic_two_stage_bad_fermentation',
                LogisticRegression(
                    max_iter=5000,
                    class_weight='balanced',
                    random_state=42
                ),
                subgroup_name
            )
        )

        stage1_rows.append(
            fit_binary_classifier_holdout(
                sub,
                feature_cols,
                'bad_fermentation_flag',
                'rf_two_stage_bad_fermentation',
                RandomForestClassifier(
                    n_estimators=400,
                    min_samples_leaf=2,
                    class_weight='balanced',
                    random_state=42,
                    n_jobs=-1
                ),
                subgroup_name
            )
        )

        #регрессия уровня масляной кислоты % от СВ только на "плохих" случаях
        positive_sub = sub.loc[sub['bad_fermentation_flag'] == 1].copy()

        if BUT_DM_COL in positive_sub.columns and positive_sub[BUT_DM_COL].dropna().shape[0] >= 25:
            stage2_rows.append(
                fit_ols_logtarget_holdout(
                    positive_sub,
                    feature_cols,
                    BUT_DM_COL,
                    'ols_log_butyric_dm_positive_only',
                    subgroup_name
                )
            )

            stage2_rows.append(
                fit_ml_regressor_holdout_logtarget(
                    positive_sub,
                    feature_cols,
                    BUT_DM_COL,
                    'hist_gb_log_butyric_dm_positive_only',
                    HistGradientBoostingRegressor(
                        max_depth=4,
                        learning_rate=0.05,
                        max_iter=300,
                        random_state=42
                    ),
                    subgroup_name
                )
            )

            stage2_rows.append(
                fit_ml_regressor_holdout_logtarget(
                    positive_sub,
                    feature_cols,
                    BUT_DM_COL,
                    'rf_log_butyric_dm_positive_only',
                    RandomForestRegressor(
                        n_estimators=300,
                        min_samples_leaf=2,
                        random_state=42,
                        n_jobs=-1
                    ),
                    subgroup_name
                )
            )

    return pd.DataFrame(stage1_rows), pd.DataFrame(stage2_rows)

def winsorize_series(s: pd.Series, lower_q: float = 0.02, upper_q: float = 0.98) -> pd.Series:
    x = pd.to_numeric(s, errors='coerce')
    if x.notna().sum() < 10:
        return x
    lo = x.quantile(lower_q)
    hi = x.quantile(upper_q)
    return x.clip(lower=lo, upper=hi)


def winsorize_hidden_factor_columns(df: pd.DataFrame) -> pd.DataFrame:
    out = df.copy()

    candidate_cols = [
        SV_COL, PH_COL, SUGAR_COL, CP_COL, CF_COL,
        LAC_COL, ACE_COL, BUT_COL,
        LAC_DM_COL, ACE_DM_COL, BUT_DM_COL,
        ASH_COL,
        'жир_проц_воздушно_сух',
        'крахмал_проц_воздуш_сух',
        'калий_г_кг_воздушно_сух',
        'кальций_г_кг_воздушно_сух',
        'фосфор_г_кг_воздушно_сух',
        'каротин_мг_кг_воздушно_сух',
        'нитраты_мг_кг_воздушно_сух',
    ]

    for col in candidate_cols:
        if col in out.columns:
            out[col] = winsorize_series(out[col])

    return out


def build_hidden_factor_experiment_df(df: pd.DataFrame) -> pd.DataFrame:
    out = winsorize_hidden_factor_columns(df)

    # дополнительные признаки состава
    if 'крахмал_проц_воздуш_сух' in out.columns:
        out['starch_x_ph'] = out['крахмал_проц_воздуш_сух'] * out[PH_COL]
        out['starch_x_sv'] = out['крахмал_проц_воздуш_сух'] * out[SV_COL]

    if ASH_COL in out.columns:
        out['ash_x_ph'] = out[ASH_COL] * out[PH_COL]
        out['ash_x_sv'] = out[ASH_COL] * out[SV_COL]

    if 'жир_проц_воздушно_сух' in out.columns:
        out['fat_x_sv'] = out['жир_проц_воздушно_сух'] * out[SV_COL]

    # отношения
    out['protein_to_fiber'] = np.where(out[CF_COL] > 0, out[CP_COL] / out[CF_COL], np.nan)
    out['sugar_to_fiber'] = np.where(out[CF_COL] > 0, out[SUGAR_COL] / out[CF_COL], np.nan)

    # логарифмы сильно скошенных показателей
    if 'нитраты_мг_кг_воздушно_сух' in out.columns:
        out['log_nitrates'] = np.log1p(out['нитраты_мг_кг_воздушно_сух'].clip(lower=0))
    if 'каротин_мг_кг_воздушно_сух' in out.columns:
        out['log_carotene'] = np.log1p(out['каротин_мг_кг_воздушно_сух'].clip(lower=0))

    return out


def run_hidden_factor_experiment(df: pd.DataFrame) -> pd.DataFrame:
    exp = build_hidden_factor_experiment_df(df)

    feature_cols = [
        SV_COL, PH_COL, SUGAR_COL, CP_COL, CF_COL,
        'sv_lt_30',
        'sv_sq', 'ph_sq', 'protein_sq', 'sugar_sq', 'fiber_sq',
        'sv_x_ph', 'ph_x_protein', 'sv_x_sugar',
        'protein_x_sugar', 'protein_x_fiber',
        'protein_to_fiber', 'sugar_to_fiber'
    ]

    optional_cols = [
        ASH_COL,
        'жир_проц_воздушно_сух',
        'крахмал_проц_воздуш_сух',
        'калий_г_кг_воздушно_сух',
        'кальций_г_кг_воздушно_сух',
        'фосфор_г_кг_воздушно_сух',
        'каротин_мг_кг_воздушно_сух',
        'нитраты_мг_кг_воздушно_сух',
        'starch_x_ph', 'starch_x_sv',
        'ash_x_ph', 'ash_x_sv',
        'fat_x_sv',
        'log_nitrates', 'log_carotene',
    ]

    feature_cols += [c for c in optional_cols if c in exp.columns]

    X = _build_design_matrix(exp, feature_cols, include_type=True, include_year=True)
    work = pd.concat([exp.reset_index(drop=True), X.reset_index(drop=True)], axis=1)
    model_cols = X.columns.tolist()

    subgroup_masks = {
        'all': pd.Series(True, index=work.index),
        'corn_only': work['is_corn_silage'] == 1,
        'non_corn_only': work['is_corn_silage'] == 0,
    }

    out_rows = []

    for target_col in [LAC_COL, BUT_COL]:
        for subgroup_name, mask in subgroup_masks.items():
            sub = work.loc[mask].copy()

            if sub[target_col].dropna().shape[0] < 40:
                continue

            out_rows.append(
                fit_ols_fullsample(
                    sub,
                    model_cols,
                    target_col,
                    f'ols_experiment_{target_col}',
                    subgroup_name
                )
            )

            out_rows.append(
                fit_ml_regressor_holdout(
                    sub,
                    model_cols,
                    target_col,
                    f'hist_gb_experiment_{target_col}',
                    HistGradientBoostingRegressor(
                        max_depth=3,
                        learning_rate=0.05,
                        max_iter=180,
                        random_state=42
                    ),
                    subgroup_name,
                    cv_splits=3
                )
            )

            out_rows.append(
                fit_ml_regressor_holdout(
                    sub,
                    model_cols,
                    target_col,
                    f'rf_experiment_{target_col}',
                    RandomForestRegressor(
                        n_estimators=120,
                        min_samples_leaf=3,
                        random_state=42,
                        n_jobs=-1
                    ),
                    subgroup_name,
                    cv_splits=3
                )
            )

    return pd.DataFrame(out_rows)


def append_hidden_factor_experiment_summary(summary_text: str, hidden_exp_df: pd.DataFrame) -> str:
    if hidden_exp_df is None or hidden_exp_df.empty:
        return summary_text

    lines = [summary_text, '', 'Экспериментальный блок улучшения скрытых факторов']

    for target_col, target_label in [
        (LAC_COL, 'молочной кислоты'),
        (BUT_COL, 'масляной кислоты'),
    ]:
        part = hidden_exp_df.loc[hidden_exp_df['target'] == target_col].sort_values(
            ['r2', 'mae'], ascending=[False, True]
        ).head(1)

        if not part.empty:
            r = part.iloc[0]
            lines.append(
                f"Лучшая экспериментальная модель для {target_label}: "
                f"{r['model']} / {r['subgroup']} "
                f"(R2={r['r2']:.3f}, MAE={r['mae']:.3f}, RMSE={r['rmse']:.3f})."
            )

    return '\n'.join(lines)
def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--input', type=str, default=DEFAULT_INPUT)
    parser.add_argument('--output', type=str, default=DEFAULT_OUTPUT)
    args = parser.parse_args()

    input_path = Path(args.input)
    out_dirs = make_dirs(Path(args.output))

    df_raw = pd.read_excel(input_path)
    df_clean = clean_dataframe(df_raw)
    df, prep_detail, prep_summary = prepare_analysis_dataframe(df_clean)

    type_counts, year_counts, desc_stats = build_eda_tables(df)
    df = assign_fermentation_rules(df)

    df = add_hidden_factor_features(df)
    hidden_ext_df = run_extended_hidden_factor_models(df)
    bad_ferm_df = run_bad_fermentation_models(df)

    hidden_exp_df = run_hidden_factor_experiment(df)

    hidden_plus_df = run_hidden_factor_models_plus(df)
    but2_clf_df, but2_reg_df = run_butyric_two_stage_models(df)

    variable_roles = build_variable_roles()
    dag_edges = build_dag_edges_verified()
    task_spec = build_task_spec()

    df.to_csv(out_dirs['data'] / 'analysis_data.csv', index=False, encoding='utf-8-sig')
    prep_detail.to_csv(out_dirs['tables'] / 'preprocessing_detail.csv', index=False, encoding='utf-8-sig')
    prep_summary.to_csv(out_dirs['tables'] / 'preprocessing_summary.csv', index=False, encoding='utf-8-sig')
    variable_roles.to_csv(out_dirs['tables'] / 'variable_roles.csv', index=False, encoding='utf-8-sig')
    dag_edges.to_csv(out_dirs['tables'] / 'dag_edges_verified.csv', index=False, encoding='utf-8-sig')
    task_spec.to_csv(out_dirs['tables'] / 'task_specification.csv', index=False, encoding='utf-8-sig')

    hidden_ext_df.to_csv(out_dirs['tables'] / 'hidden_factor_models_extended.csv', index=False, encoding='utf-8-sig')
    bad_ferm_df.to_csv(out_dirs['tables'] / 'bad_fermentation_models.csv', index=False, encoding='utf-8-sig')

    # корреляции
    CORR_COLS_FULL = [
        YEAR_COL,
        MOISTURE_COL,
        CP_COL,
        'калий_г_кг_воздушно_сух',
        CF_COL,
        ASH_COL,
        'кальций_г_кг_воздушно_сух',
        'фосфор_г_кг_воздушно_сух',
        'каротин_мг_кг_воздушно_сух',
        SUGAR_COL,
        'нитраты_мг_кг_воздушно_сух',
        'жир_проц_воздушно_сух',
        OE_COL,
        KE_COL,
        PP_COL,
        QUALITY_COL,
        'крахмал_проц_воздуш_сух',
        PH_COL,
        LAC_COL,
        ACE_COL,
        BUT_COL,
        TOTAL_ACID_COL,
        LAC_SHARE_COL,
        ACE_SHARE_COL,
        BUT_SHARE_COL,
    ]
    corr_cols = [c for c in CORR_COLS_FULL if c in df.columns]
    corr = df[corr_cols].corr(method='spearman')
    corr.to_csv(out_dirs['tables'] / 'correlation_matrix_spearman.csv', encoding='utf-8-sig')
    corr_heatmap(corr, out_dirs['figures'] / 'correlation_heatmap.png')

    # --- основные аналитические блоки ---
    group_anova, group_kr, group_pair = run_group_analysis(df, out_dirs)
    acid_df = acid_models(df, out_dirs)
    oe_ke_df = oe_ke_models(df, out_dirs)
    fermentation_counts = fermentation_summary(df, out_dirs)
    norm_df, methods_df = gost_dlg_flieg(df, out_dirs)
    causal_summary = causal_models_extended(norm_df, out_dirs)
    interp_summary, practical_summary, rf_best = benchmark_vs_ml(df, out_dirs)

    # --- DAG ---
    draw_dag(dag_edges, out_dirs)

    # --- сравнение методов ---
    method_comparison = methods_df.copy()
    if not rf_best.empty:
        rf_metrics = rf_best.iloc[0]
        method_comparison = pd.concat([
            pd.DataFrame([{
                'method': 'Random Forest',
                'n': len(df),
                'accuracy': rf_metrics['accuracy'],
                'balanced_accuracy': rf_metrics['balanced_accuracy'],
                'macro_f1': rf_metrics['macro_f1'],
                'weighted_f1': rf_metrics['weighted_f1'],
                'kappa': np.nan
            }]),
            method_comparison,
        ], ignore_index=True)

    method_comparison.to_csv(
        out_dirs['tables'] / 'method_comparison_all.csv',
        index=False,
        encoding='utf-8-sig'
    )

    summary_text = build_summary_text(
        df, acid_df, group_kr, oe_ke_df,
        methods_df, practical_summary, causal_summary,
        fermentation_counts
    )
    summary_text = append_hidden_factor_summary(summary_text, hidden_ext_df, bad_ferm_df)
    summary_text = append_hidden_factor_experiment_summary(summary_text, hidden_exp_df)
    if hidden_plus_df is not None and not hidden_plus_df.empty:
        best_plus = hidden_plus_df.sort_values(['r2', 'mae'], ascending=[False, True]).iloc[0]
        summary_text += (
            f"\nДополнительно при переходе к target-переменным в % от СВ и долевым показателям "
            f"лучшая модель была получена для {best_plus['target']} "
            f"({best_plus['model']} / {best_plus['subgroup']}): "
            f"R2={best_plus['r2']:.3f}, MAE={best_plus['mae']:.3f}, RMSE={best_plus['rmse']:.3f}."
        )

    if but2_reg_df is not None and not but2_reg_df.empty:
        best_two_stage = but2_reg_df.sort_values(['r2', 'mae'], ascending=[False, True]).iloc[0]
        summary_text += (
            f"\nВ двухэтапной схеме для масляной кислоты лучшая регрессия уровня "
            f"масляной кислоты (% от СВ) на положительных случаях: "
            f"{best_two_stage['model']} / {best_two_stage['subgroup']} "
            f"(R2={best_two_stage['r2']:.3f}, MAE={best_two_stage['mae']:.3f}, RMSE={best_two_stage['rmse']:.3f})."
        )
    save_text(out_dirs['reports'] / 'summary_ru.txt', summary_text)

    key_metrics = {
        'n_rows_final': int(len(df)),
        'best_rf_accuracy': float(
            practical_summary.loc[
                practical_summary['model'] == 'random_forest', 'accuracy'
            ].max()
        ),
        'best_rf_weighted_f1': float(
            practical_summary.loc[
                practical_summary['model'] == 'random_forest', 'weighted_f1'
            ].max()
        ),
        'flieg_accuracy': float(
            methods_df.loc[methods_df['method'] == 'индекс_Флига', 'accuracy'].iloc[0]
        ) if 'индекс_Флига' in methods_df['method'].values else None,
        'dlg_accuracy': float(
            methods_df.loc[methods_df['method'] == 'DLG', 'accuracy'].iloc[0]
        ) if 'DLG' in methods_df['method'].values else None,
        'gost_accuracy': float(
            methods_df.loc[methods_df['method'] == 'ГОСТ', 'accuracy'].iloc[0]
        ) if 'ГОСТ' in methods_df['method'].values else None,
        'fermentation_rule_counts': fermentation_counts.to_dict(orient='records'),
    }
    save_text(
        out_dirs['reports'] / 'key_metrics.json',
        json.dumps(key_metrics, ensure_ascii=False, indent=2)
    )

    print_section('ГЛАВА 2.1. РАЗВЕДОЧНЫЙ АНАЛИЗ ДАННЫХ О КАЧЕСТВЕ КОНСЕРВИРОВАННЫХ КОРМОВ')
    print('\n[Подготовка данных]')
    print_dataframe(prep_summary, max_rows=20)

    print_section('ОБРАБОТКА ПРОПУСКОВ')
    print_dataframe(prep_detail.loc[prep_detail['Пропуски_до'] > 0], max_rows=100)

    print_section('СОСТАВ ВЫБОРКИ ПО ТИПАМ СИЛОСА')
    print_dataframe(type_counts, max_rows=20)

    print_section('СОСТАВ ВЫБОРКИ ПО ГОДАМ')
    print_dataframe(year_counts, max_rows=20)

    print_section('ОПИСАТЕЛЬНАЯ СТАТИСТИКА ПО ОСНОВНЫМ ПОКАЗАТЕЛЯМ')
    print_dataframe(desc_stats, max_rows=30)

    print_section('КОРРЕЛЯЦИОННАЯ МАТРИЦА (SPEARMAN)')
    print_dataframe(corr.round(4), max_rows=50)

    print_section('СРАВНЕНИЕ ГРУПП: ANOVA')
    print_dataframe(group_anova, max_rows=50)

    print_section('СРАВНЕНИЕ ГРУПП: KRUSKAL-WALLIS')
    print_dataframe(group_kr, max_rows=50)

    print_section('ПОПАРНЫЕ СРАВНЕНИЯ (первые строки)')
    print_dataframe(group_pair.head(30), max_rows=30)

    print_section('ГЛАВА 2.2. ПОСТАНОВКА ЗАДАЧ ПРИЧИННОГО АНАЛИЗА')
    print('\n[Роли переменных]')
    print_dataframe(variable_roles, max_rows=30)

    print_section('СПЕЦИФИКАЦИЯ ЗАДАЧ')
    print_dataframe(task_spec, max_rows=20)

    print_section('СВЯЗИ DAG')
    print_dataframe(dag_edges, max_rows=100)

    print_section('ЗАДАЧА 1. СКРЫТЫЕ ФАКТОРЫ')
    print_dataframe(acid_df, max_rows=20)

    print_section('ЗАДАЧА 1. СКРЫТЫЕ ФАКТОРЫ — ЭКСПЕРИМЕНТАЛЬНЫЙ БЛОК')
    print_dataframe(
        hidden_exp_df.sort_values(['target', 'r2'], ascending=[True, False]),
        max_rows=100
    )

    print_section('ЗАДАЧА 1. СКРЫТЫЕ ФАКТОРЫ — ДОПОЛНИТЕЛЬНЫЕ МОДЕЛИ')
    print_dataframe(
        hidden_ext_df.sort_values(['target', 'r2'], ascending=[True, False]),
        max_rows=100
    )

    print_section('ДОПОЛНИТЕЛЬНО: НЕБЛАГОПРИЯТНОЕ БРОЖЕНИЕ (масляная кислота > 0.3% от СВ)')
    print_dataframe(
        bad_ferm_df.sort_values(['f1', 'recall'], ascending=False),
        max_rows=50
    )

    print_section('ЗАДАЧА 1. СКРЫТЫЕ ФАКТОРЫ — РАСШИРЕННЫЕ TARGET-ПЕРЕМЕННЫЕ')
    print_dataframe(
        hidden_plus_df.sort_values(['target', 'r2'], ascending=[True, False]),
        max_rows=200
    )

    print_section('ДВУХЭТАПНАЯ МОДЕЛЬ: ЭТАП 1 — КЛАССИФИКАЦИЯ НЕБЛАГОПРИЯТНОГО БРОЖЕНИЯ')
    print_dataframe(
        but2_clf_df.sort_values(['f1', 'recall'], ascending=False),
        max_rows=100
    )

    print_section('ДВУХЭТАПНАЯ МОДЕЛЬ: ЭТАП 2 — РЕГРЕССИЯ УРОВНЯ МАСЛЯНОЙ КИСЛОТЫ (% ОТ СВ) ДЛЯ ПЛОХИХ СЛУЧАЕВ')
    print_dataframe(
        but2_reg_df.sort_values(['r2', 'mae'], ascending=[False, True]),
        max_rows=100
    )

    print_section('ЗАДАЧА 2. ДИАГНОСТИКА БРОЖЕНИЯ И ПОРЧИ')
    print_dataframe(fermentation_counts, max_rows=20)

    print_section('ЗАДАЧА 2. ДИАГНОСТИКА КАЧЕСТВА: ИНТЕРПРЕТИРУЕМЫЙ НАБОР ПРИЗНАКОВ')
    print_dataframe(interp_summary, max_rows=20)

    print_section('ЗАДАЧА 2. ДИАГНОСТИКА КАЧЕСТВА: ПРАКТИЧЕСКИЙ НАБОР ПРИЗНАКОВ')
    print_dataframe(practical_summary, max_rows=20)

    print_section('ЗАДАЧА 3. СРАВНЕНИЕ ГРУПП СИЛОСА: ANOVA')
    print_dataframe(group_anova, max_rows=50)

    print_section('ЗАДАЧА 3. СРАВНЕНИЕ ГРУПП СИЛОСА: KRUSKAL-WALLIS')
    print_dataframe(group_kr, max_rows=50)

    print_section('ЗАДАЧА 4. ПОВЫШЕНИЕ ПИТАТЕЛЬНОСТИ: МОДЕЛИ ДЛЯ ОЭ И КЕ')
    print_dataframe(oe_ke_df, max_rows=20)

    print_section('ДОПОЛНИТЕЛЬНО: СРАВНЕНИЕ ГОСТ / DLG / ИНДЕКСА ФЛИГА')
    print_dataframe(methods_df, max_rows=20)

    print_section('ДОПОЛНИТЕЛЬНО: ПРИЧИННЫЕ МОДЕЛИ ПО DAG')
    print_dataframe(causal_summary, max_rows=50)

    print_section('СВОДНОЕ СРАВНЕНИЕ МЕТОДОВ')
    print_dataframe(method_comparison, max_rows=20)

    print_section('ИТОГОВОЕ РЕЗЮМЕ')
    print(summary_text)

if __name__ == '__main__':
    main()

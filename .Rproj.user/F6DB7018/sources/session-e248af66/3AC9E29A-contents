# ===================================================================
# INTERAKTIVNÍ SHINY APLIKACE: BIOSTATISTIKA A MATICOVÁ ALGEBRA
# (Popisná statistika, Korelace, Lineární regrese, Maticové operace, rovnic)
# ===================================================================

if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")

library(shiny)
library(ggplot2)

# Vzhled aplikace
app_theme <- if (requireNamespace("shinythemes", quietly = TRUE)) {
  shinythemes::shinytheme("flatly")
} else {
  NULL
}

# -------------------------------------------------------------------
# LOGO V PRAVÉM HORNÍM ROHU (s odkazem na MENDELU)
# -------------------------------------------------------------------
logo_soubor <- "logo.png"                # název souboru ve složce www
logo_odkaz  <- "https://umfgz.af.mendelu.cz"  # adresa, na kterou logo odkazuje
logo_text   <- "MENDELU"                 # záložní text + popisek při najetí myší
logo_vyska  <- 36                        # výška loga v pixelech

vytvor_logo_html <- function() {
  obsah <- if (file.exists(file.path("www", logo_soubor))) {
    tags$img(src = logo_soubor, alt = logo_text, style = paste0("height: ", logo_vyska, "px;"))
  } else {
    logo_text
  }
  as.character(tags$a(
    href = logo_odkaz, target = "_blank", rel = "noopener noreferrer", class = "navbar-logo", title = logo_text, obsah
  ))
}

logo_css <- "
.navbar-logo {
  position: absolute;
  right: 15px;
  top: 0;
  height: 56px;
  display: flex;
  align-items: center;
  z-index: 1100;
  color: #ffffff;
  font-weight: bold;
  text-decoration: none;
}
.navbar-logo:hover, .navbar-logo:focus {
  opacity: 0.85;
  color: #ffffff;
  text-decoration: none;
}
@media (max-width: 767px) {
  .navbar-logo { right: 60px; }
}
"

logo_js <- paste0(
  "var logoHtml = ", jsonlite::toJSON(vytvor_logo_html(), auto_unbox = TRUE), ";\n",
  "$(function() { $('.navbar > .container-fluid').first().append(logoHtml); });"
)

# -------------------------------------------------------------------
# ČISTÉ HTML/CSS PRO VZORCE
# -------------------------------------------------------------------
formula_css <- "
.formula-box {
  background-color: #f8f9fa;
  border: 1px solid #0072B2;
  border-radius: 6px;
  padding: 12px;
  margin-top: 15px;
  font-family: 'Segoe UI', Arial, sans-serif;
}
.eq-line {
  margin: 8px 0;
  font-size: 1.02em;
  display: flex;
  align-items: center;
  flex-wrap: wrap;
  gap: 4px;
  line-height: 1.5;
}
.frac {
  display: inline-flex;
  flex-direction: column;
  vertical-align: middle;
  text-align: center;
  padding: 0 4px;
  font-size: 0.95em;
}
.frac .num {
  border-bottom: 1.5px solid #222;
  padding-bottom: 2px;
}
.frac .den {
  padding-top: 2px;
}
.mat-brackets {
  display: inline-flex;
  align-items: center;
  border-left: 2px solid #222;
  border-right: 2px solid #222;
  border-radius: 4px;
  padding: 2px 6px;
  margin: 0 4px;
  vertical-align: middle;
}
.mat-table {
  border-collapse: collapse;
  margin: 0;
}
.mat-table td {
  padding: 2px 6px;
  text-align: center;
  font-family: monospace;
  font-size: 0.95em;
}

.r-code-box {
  background-color: #272822;
  color: #f8f8f2;
  border-radius: 6px;
  padding: 12px;
  font-family: 'Consolas', 'Courier New', monospace;
  font-size: 0.9em;
  overflow-x: auto;
  white-space: pre;
}
.status-badge {
  padding: 6px 12px;
  border-radius: 4px;
  font-weight: bold;
  display: inline-block;
  margin-bottom: 10px;
}
.status-ok {
  background-color: #d4edda;
  color: #155724;
  border: 1px solid #c3e6cb;
}
.status-err {
  background-color: #f8d7da;
  color: #721c24;
  border: 1px solid #f5c6cb;
}

.math-sym {
  font-family: 'Times New Roman', Times, serif;
  font-style: italic;
  font-weight: bold;
}
"

# -------------------------------------------------------------------
# UŽIVATELSKÉ ROZHRANÍ (UI - NAVBAR)
# -------------------------------------------------------------------
ui <- navbarPage(
  theme = app_theme,
  title = "Biostatistika a maticová algebra",
  header = tags$head(
    tags$style(HTML(formula_css)),
    tags$style(HTML(logo_css)),
    tags$script(HTML(logo_js))
  ),
  footer = tags$div(
    class = "text-muted",
    style = "text-align: center; padding: 12px; margin-top: 20px; border-top: 1px solid #ddd; font-size: 0.9em;",
    HTML(paste0("Autor: <b>Tomáš Urban</b>, UMFGZ, AF MENDELU © ", format(Sys.Date(), "%Y"), " (verze 0.6) | ")),
    actionLink("btn_cite_modal", "Jak citovat tuto aplikaci", icon = icon("quote-right"), style = "font-weight: bold; color: #2c7be5;")
  ),
  
  # =================================================================
  # ZÁLOŽKA 1: POPISNÁ STATISTIKA A NORMÁLNÍ ROZDĚLENÍ
  # =================================================================
  tabPanel(
    "1. Popisná statistika",
    tabsetPanel(
      id = "subtab_desc",
      
      # -------------------------------------------------------------
      # POD-ZÁLOŽKA 1A: VÝBĚROVÁ STATISTIKA & ROZDĚLENÍ
      # -------------------------------------------------------------
      tabPanel(
        "1A. Výběrová statistika & data",
        br(),
        sidebarLayout(
          sidebarPanel(
        width = 4,
        h3("Parametry spojitého znaku"),
        helpText("Simulace dat nebo analýza vlivu průměru (μ) a směrodatné odchylky (σ) na tvar rozdělení a intervaly spolehlivosti."),
        hr(),
        radioButtons("dist_mode", "Zdroj dat:",
                     choices = c("Generovaná data (Normální rozdělení)" = "gen",
                                 "Vlastní řada čísel" = "manual")),
        
        conditionalPanel(
          condition = "input.dist_mode == 'gen'",
          sliderInput("pop_mean", "Průměr (μ):", min = 0, max = 100, value = 10, step = 0.5),
          sliderInput("pop_sd", "Směrodatná odchylka (σ):", min = 0.5, max = 20, value = 5, step = 0.5),
          numericInput("pop_N", "Velikost vzorku (N):", value = 100, min = 10, max = 5000, step = 10),
          actionButton("btn_reseed", "Vygenerovat nový vzorek", class = "btn-primary")
        ),
        
        conditionalPanel(
          condition = "input.dist_mode == 'manual'",
          textAreaInput("manual_data", "Zadejte čísla oddělená čárkou:", 
                        value = "11, 4, 9, 8, 12, 17, 3, 5, 16, 15", rows = 3),
          helpText("Výchozí data: Denní výdoj mléka 10 krav ze cvičení")
        ),
        
        hr(),
        wellPanel(
          h4("Vypočtené statistické míry:"),
          htmlOutput("desc_stats_html")
        ),
        
        tags$details(
          class = "formula-box",
          tags$summary(tags$b(style = "color: #0072B2; font-size: 1.05em;", "Použité vzorce a teorie")),
          tags$div(
            style = "margin-top: 10px;",
            HTML("
            <div class='eq-line'>
              <b>Aritmetický průměr:</b> &nbsp;
              <span class='math-sym'>ȳ</span> = 
              <span class='frac'><span class='num'>∑ y<sub>i</sub></span><span class='den'>n</span></span>
            </div>
            <div class='eq-line'>
              <b>Výběrový rozptyl:</b> &nbsp;
              <span class='math-sym'>s<sup>2</sup></span> = 
              <span class='frac'><span class='num'>∑ (y<sub>i</sub> − ȳ)<sup>2</sup></span><span class='den'>n − 1</span></span>
            </div>
            <div class='eq-line'>
              <b>Směrodatná odchylka:</b> &nbsp; <span class='math-sym'>s</span> = √<span style='text-decoration: overline;'>s<sup>2</sup></span>
            </div>
            <div class='eq-line'>
              <b>Střední chyba průměru:</b> &nbsp; <span class='math-sym'>SE</span> = 
              <span class='frac'><span class='num'>s</span><span class='den'>√<span style='text-decoration: overline;'>n</span></span></span>
            </div>
            <div class='eq-line'>
              <b>Variační koeficient:</b> &nbsp; <span class='math-sym'>CV</span> = 
              <span class='frac'><span class='num'>s</span><span class='den'>ȳ</span></span> · 100%
            </div>
            <div style='margin-top: 8px;'>
              <b>Pravidlo 3σ:</b>
              <ul>
                <li>68,3 % dat leží v intervalu ȳ ± 1s</li>
                <li>95,4 % dat leží v intervalu ȳ ± 2s</li>
                <li>99,7 % dat leží v intervalu ȳ ± 3s</li>
              </ul>
            </div>
            ")
          )
        )
      ),
      
      mainPanel(
        width = 8,
        plotOutput("plot_histogram", height = "400px"),
        br(),
        plotOutput("plot_boxplot", height = "220px"),
        br(),
        wellPanel(
          style = "background-color: #f8f9fa; border-left: 4px solid #0072B2; padding: 15px; margin-top: 10px;",
          h4(style = "color: #0072B2; margin-top: 0;", "💡 Biologické zdůvodnění volby rozsahů parametrů (μ a σ)"),
          HTML("
          <p style='margin-bottom: 8px; font-size: 0.95em;'>
            <b>1. Rozsah průměru (μ ∈ 0 až 100):</b> Pokrývá běžná biologická měřítka kvantitativních znaků v plemenitbě a genetice 
            (např. denní výdoj mléka 10–40 kg, hřbetní tuk prasat 10–30 mm, jatečná výtěžnost 50–80 %, obsahy složek v %).
          </p>
          <p style='margin-bottom: 0px; font-size: 0.95em;'>
            <b>2. Rozsah směrodatné odchylky (σ ∈ 0,5 až 20):</b> Odpovídá přirozené variabilitě u hospodářských zvířat 
            (variační koeficient <i>CV</i> = 5 % až 50 %). Horní limit σ zabraňuje vzniku biologicky nereálných záporných fenotypů 
            a spodní limit σ = 0,5 zamezuje nulové variabilitě (dělení nulou při výpočtu <i>CV</i>, <i>SE</i> a <i>z</i>-skóre).
          </p>
          ")
        )
      )
    )
  ),
  
  # -------------------------------------------------------------
  # POD-ZÁLOŽKA 1B: SROVNÁNÍ PRŮMĚRŮ A VARIANCÍ NORMÁLNÍHO ROZDĚLENÍ
  # -------------------------------------------------------------
  tabPanel(
    "1B. Srovnání efektu průměru (μ) a rozptylu (σ²)",
    br(),
    sidebarLayout(
      sidebarPanel(
        width = 4,
        h3("Porovnání křivek N(μ, σ²)"),
        helpText("Porovnání vlivu změny průměru (μ = posun polohy) a rozptylu (σ² = měřítko/variabilita) na tvar hustoty pravděpodobnosti."),
        hr(),
        h4(style = "color: #0072B2;", "Křivka 1 (Referenční - Modrá)"),
        sliderInput("norm_mean1", "Průměr μ₁:", min = -50, max = 50, value = 10, step = 1),
        sliderInput("norm_sd1", "Směrodatná odchylka σ₁ (σ₁² = rozptyl):", min = 1, max = 25, value = 5, step = 0.5),
        hr(),
        h4(style = "color: #D55E00;", "Křivka 2 (Srovnávací - Oranžová)"),
        sliderInput("norm_mean2", "Průměr μ₂:", min = -50, max = 50, value = 25, step = 1),
        sliderInput("norm_sd2", "Směrodatná odchylka σ₂:", min = 1, max = 25, value = 5, step = 0.5),
        hr(),
        h4(style = "color: #009E73;", "Křivka 3 (Volitelná - Zelená)"),
        checkboxInput("show_curve3", "Aktivovat 3. křivku", value = TRUE),
        conditionalPanel(
          condition = "input.show_curve3 == true",
          sliderInput("norm_mean3", "Průměr μ₃:", min = -50, max = 50, value = 10, step = 1),
          sliderInput("norm_sd3", "Směrodatná odchylka σ₃:", min = 1, max = 25, value = 2, step = 0.5)
        ),
        hr(),
        tags$details(
          class = "formula-box",
          tags$summary(tags$b(style = "color: #0072B2; font-size: 1.05em;", "Teorie: Poloha vs. Variabilita")),
          tags$div(
            style = "margin-top: 10px;",
            HTML("
            <div class='eq-line'>
              <b>Hustota pravděpodobnosti:</b> &nbsp;
              <span class='math-sym'>f(x)</span> = 
              <span class='frac'><span class='num'>1</span><span class='den'>σ · √<span style='text-decoration: overline;'>2π</span></span></span> · e<sup>−<span class='frac'><span class='num'>(x − μ)<sup>2</sup></span><span class='den'>2σ<sup>2</sup></span></span></sup>
            </div>
            <div class='eq-line' style='margin-top: 8px;'>
              <b>1. Průměr (μ) = Parametr polohy:</b><br/>
              Určuje pozici vrcholu křivky na ose X. Změna μ posouvá křivku doprava/doleva bez jakékoliv změny jejího tvaru nebo výšky.
            </div>
            <div class='eq-line' style='margin-top: 8px;'>
              <b>2. Rozptyl (σ²) = Parametr měřítka:</b><br/>
              Určuje rozptýlenost dat a tvar křivky. Vyšší rozptyl σ² znamená širší a plošší křivku. Nižší rozptyl σ² vytváří strmou a vysokou křivku s daty shluknutými těsně kolem průměru.
            </div>
            <div class='eq-line' style='margin-top: 8px;'>
              <b>3. Celková plocha:</b><br/>
              Plocha pod každou křivkou je vždy přesně rovna 1 (100 % pravděpodobnosti).
            </div>
            ")
          )
        )
      ),
      
      mainPanel(
        width = 8,
        plotOutput("plot_norm_compare", height = "460px"),
        br(),
        wellPanel(
          h4("Přehled parametrů a vrcholů aktivních křivek:"),
          htmlOutput("norm_compare_summary_html")
        )
      )
    )
  )
)
),

  # =================================================================
  # ZÁLOŽKA 2: ANALÝZA VARIANCE (ANOVA)
  # =================================================================
  tabPanel(
    "2. Analýza variance (ANOVA)",
    tabsetPanel(
      id = "subtab_anova",
      
      # -------------------------------------------------------------
      # -------------------------------------------------------------
      # POD-ZÁLOŽKA 2A: JEDNOFAKTOROVÁ ANOVA (RUČNÍ VÝPOČET & VZORCE)
      # -------------------------------------------------------------
      tabPanel(
        "2A. Jednofaktorová ANOVA (Ruční výpočet & Vzorce)",
        br(),
        sidebarLayout(
          sidebarPanel(
            width = 4,
            h3("Jednofaktorová ANOVA"),
            helpText("Testování vlivu genetického faktoru (např. otec/kohout) na užitkovost potomstva (snáška slepic)."),
            hr(),
            wellPanel(
              style = "background-color: #eef6fc; border-left: 4px solid #0072B2; padding: 12px;",
              h4(style = "margin-top: 0; color: #0072B2;", "Formulace hypotéz ANOVA:"),
              HTML("
              <b>Nulová hypotéza (H₀):</b> μ₁ = μ₂ = μ₃ = μ₄<br/>
              <small><i>(Efekt otce je nulový; průměry všech skupin potomstva jsou shodné.)</i></small><br/><br/>
              <b>Alternativní hypotéza (H₁):</b> ∃ (i, j): μᵢ ≠ μⱼ<br/>
              <small><i>(Efekt otce je statisticky významný; alespoň jeden otec se liší od ostatních.)</i></small>
              ")
            ),
            hr(),
            radioButtons("anova_preset", "Výchozí datový soubor / Scénář:",
                         choices = c(
                           "Příklad z přednášky: Snáška slepic (Průkazný efekt, p < 0.001)" = "poultry_sig",
                           "Srovnávací soubor: Vyrovnaná snáška (Neprůkazný efekt, p = 0.79)" = "poultry_nonsig",
                           "Simulovaná data: Výrazný efekt otce (Průkazný, p < 0.05)" = "sim_sig",
                           "Simulovaná data: Nulový efekt otce (Neprůkazný, p >= 0.05)" = "sim_nonsig",
                           "Vlastní nastavení simulace (posuvníky)" = "sim_custom"
                         )),
            conditionalPanel(
              condition = "input.anova_preset == 'sim_custom'",
              sliderInput("anova_sim_diff", "Rozdíl mezi skupinami (efekt):", min = 0, max = 50, value = 20, step = 5),
              sliderInput("anova_sim_sd", "Uvnitř-skupinový rozptyl (σ):", min = 5, max = 30, value = 15, step = 1),
              actionButton("btn_anova_reseed", "Vygenerovat nová data", class = "btn-primary")
            ),
            hr(),
            wellPanel(
              h4("Souhrn F-testu a rozhodnutí:"),
              htmlOutput("anova_f_summary_html")
            ),
            tags$details(
              class = "formula-box",
              tags$summary(tags$b(style = "color: #0072B2; font-size: 1.05em;", "Vzorce pro jednofaktorovou ANOVA")),
              tags$div(
                style = "margin-top: 10px;",
                HTML("
                <div class='eq-line'>
                  <b>Model:</b> &nbsp; <span class='math-sym'>y<sub>i,j</sub></span> = μ + a<sub>i</sub> + e<sub>i,j</sub>
                </div>
                <div class='eq-line'>
                  <b>Celkový součet čtverců:</b> &nbsp;
                  <span class='math-sym'>SS<sub>celkový</sub></span> = ∑ y<sup>2</sup> − <span class='frac'><span class='num'>(∑ y)<sup>2</sup></span><span class='den'>n</span></span>
                </div>
                <div class='eq-line'>
                  <b>Součet čtverců mezi skupinami:</b> &nbsp;
                  <span class='math-sym'>SS<sub>mezi</sub></span> = ∑ <span class='frac'><span class='num'>Y<sub>i·</sub><sup>2</sup></span><span class='den'>n<sub>i</sub></span></span> − <span class='frac'><span class='num'>Y<sub>··</sub><sup>2</sup></span><span class='den'>n</span></span>
                </div>
                <div class='eq-line'>
                  <b>Reziduální součet čtverců:</b> &nbsp;
                  <span class='math-sym'>SS<sub>reziduální</sub></span> = SS<sub>celkový</sub> − SS<sub>mezi</sub>
                </div>
                <div class='eq-line'>
                  <b>Průměrné čtverce (Variance):</b> &nbsp; <span class='math-sym'>MS</span> = <span class='frac'><span class='num'>SS</span><span class='den'>df</span></span>
                </div>
                <div class='eq-line'>
                  <b>F-test:</b> &nbsp; <span class='math-sym'>F</span> = <span class='frac'><span class='num'>MS<sub>mezi</sub></span><span class='den'>MS<sub>reziduální</sub></span></span>
                </div>
                ")
              )
            )
          ),
          
          mainPanel(
            width = 8,
            wellPanel(
              h4("Tabulka analýzy variance (ANOVA Table):"),
              tableOutput("anova_manual_table")
            ),
            plotOutput("plot_anova_boxplot", height = "380px"),
            br(),
            wellPanel(
              h4("Generovaný R kód pro ruční výpočet ANOVA:"),
              verbatimTextOutput("out_r_script_anova_manual")
            )
          )
        )
      ),
      
      # -------------------------------------------------------------
      # POD-ZÁLOŽKA 2B: ANOVA V R & POST-HOC TESTY (aov, lm, Tukey)
      # -------------------------------------------------------------
      tabPanel(
        "2B. ANOVA v R & Post-hoc testy (aov, lm, Tukey)",
        br(),
        sidebarLayout(
          sidebarPanel(
            width = 4,
            h3("Post-hoc analýza v R"),
            helpText("Identifikace konkrétních dvojic skupin, které se statisticky významně liší (Tukey HSD test)."),
            hr(),
            wellPanel(
              h4("Výsledky R funkce aov():"),
              verbatimTextOutput("out_anova_aov_summary")
            ),
            wellPanel(
              h4("Výsledky Tukey HSD testu:"),
              verbatimTextOutput("out_anova_tukey_summary")
            )
          ),
          
          mainPanel(
            width = 8,
            plotOutput("plot_anova_tukey_ci", height = "380px"),
            br(),
            wellPanel(
              h4("Generovaný R kód (aov, TukeyHSD, emmeans):"),
              verbatimTextOutput("out_r_script_anova_func")
            )
          )
        )
      ),
      
      # -------------------------------------------------------------
      # POD-ZÁLOŽKA 2C: DVOUFAKTOROVÁ HIERARCHICKÁ ANOVA (Otec / Matka)
      # -------------------------------------------------------------
      tabPanel(
        "2C. Dvoufaktorová hierarchická ANOVA (Otec / Matka)",
        br(),
        sidebarLayout(
          sidebarPanel(
            width = 4,
            h3("Hierarchická ANOVA"),
            helpText("Analýza vnořených faktorů: Vliv otce a matky (uvnitř otce) na přírůstek selat."),
            hr(),
            wellPanel(
              style = "background-color: #eef6fc; border-left: 4px solid #0072B2; padding: 12px;",
              h4(style = "margin-top: 0; color: #0072B2;", "Hypotézy hierarchické ANOVA:"),
              HTML("
              <b>H₀(Otec):</b> a₁ = a₂ = 0 &nbsp;<i>(Efekt otce je nulový)</i><br/>
              <b>H₀(Matka/Otec):</b> b₁₁ = b₁₂ = ... = 0 &nbsp;<i>(Efekt matky vnořené v otci je nulový)</i>
              ")
            ),
            hr(),
            radioButtons("nested_preset", "Datový soubor / Scénář (Otec / Matka):",
                         choices = c(
                           "Příklad z cvičení 6: Přírůstky selat (Průkazný Otce i Matky, p < 0.01)" = "piglets_sig",
                           "Srovnávací soubor: Vyrovnané přírůstky (Neprůkazný Otce i Matky, p > 0.05)" = "piglets_nonsig",
                           "Simulace: Výrazné efekty Otce i Matky (Průkazné, p < 0.05)" = "nested_sim_sig",
                           "Simulace: Nulové efekty Otce i Matky (Neprůkazné, p > 0.05)" = "nested_sim_nonsig",
                           "Vlastní simulovaný pokus (posuvníky)" = "nested_sim_custom"
                         )),
            conditionalPanel(
              condition = "input.nested_preset == 'nested_sim_custom'",
              sliderInput("nested_eff_father", "Efekt otce (rozdíl průměrů):", min = 0, max = 30, value = 15, step = 1),
              sliderInput("nested_eff_mother", "Efekt matky v otci (rozdíl):", min = 0, max = 15, value = 8, step = 1),
              sliderInput("nested_sim_sd", "Uvnitř-skupinová variabilita (σ):", min = 1, max = 10, value = 3, step = 0.5),
              actionButton("btn_nested_reseed", "Vygenerovat novou simulaci", class = "btn-primary")
            ),
            hr(),
            wellPanel(
              h4("Hierarchická struktura:"),
              p("Otec 1 → Matky 1.1, 1.2, 1.3 → Selata"),
              p("Otec 2 → Matky 2.1, 2.2, 2.3 → Selata")
            ),
            tags$details(
              class = "formula-box",
              tags$summary(tags$b(style = "color: #0072B2; font-size: 1.05em;", "Vzorce pro dvoufaktorovou hierarchickou ANOVA")),
              tags$div(
                style = "margin-top: 10px;",
                HTML("
                <div class='eq-line'>
                  <b>Lineární model:</b> &nbsp; <span class='math-sym'>y<sub>ijk</sub></span> = μ + a<sub>i</sub> + b<sub>ij</sub> + e<sub>ijk</sub>
                </div>
                <div class='eq-line' style='margin-top: 6px;'>
                  <b>SS Otci (mezi otci):</b> &nbsp;
                  <span class='math-sym'>SS<sub>otci</sub></span> = ∑ <span class='frac'><span class='num'>(∑ y<sub>i··</sub>)<sup>2</sup></span><span class='den'>n<sub>i</sub></span></span> − <span class='frac'><span class='num'>(∑ y)<sup>2</sup></span><span class='den'>n</span></span>
                </div>
                <div class='eq-line'>
                  <b>SS Matka(Otec) (mezi matkami v otci):</b> &nbsp;
                  <span class='math-sym'>SS<sub>matka(otec)</sub></span> = ∑ <span class='frac'><span class='num'>(∑ y<sub>ij·</sub>)<sup>2</sup></span><span class='den'>n<sub>ij</sub></span></span> − ∑ <span class='frac'><span class='num'>(∑ y<sub>i··</sub>)<sup>2</sup></span><span class='den'>n<sub>i</sub></span></span>
                </div>
                <div class='eq-line'>
                  <b>SS Reziduální (uvnitř matek):</b> &nbsp;
                  <span class='math-sym'>SS<sub>reziduální</sub></span> = ∑ y<sup>2</sup> − ∑ <span class='frac'><span class='num'>(∑ y<sub>ij·</sub>)<sup>2</sup></span><span class='den'>n<sub>ij</sub></span></span>
                </div>
                <div class='eq-line'>
                  <b>SS Celkový:</b> &nbsp;
                  <span class='math-sym'>SS<sub>celkový</sub></span> = ∑ y<sup>2</sup> − <span class='frac'><span class='num'>(∑ y)<sup>2</sup></span><span class='den'>n</span></span>
                </div>
                <div class='eq-line' style='margin-top: 6px;'>
                  <b>Stupně volnosti:</b> &nbsp;
                  <span class='math-sym'>df<sub>otci</sub></span> = k − 1, &nbsp;
                  <span class='math-sym'>df<sub>matka(otec)</sub></span> = p − k, &nbsp;
                  <span class='math-sym'>df<sub>reziduální</sub></span> = n − p
                </div>
                <div class='eq-line'>
                  <b>Průměrné čtverce:</b> &nbsp; <span class='math-sym'>MS</span> = <span class='frac'><span class='num'>SS</span><span class='den'>df</span></span>
                </div>
                <div class='eq-line' style='margin-top: 6px;'>
                  <b>F-testy:</b> &nbsp;
                  <span class='math-sym'>F<sub>otci</sub></span> = <span class='frac'><span class='num'>MS<sub>otci</sub></span><span class='den'>MS<sub>reziduální</sub></span></span>, &nbsp;&nbsp;
                  <span class='math-sym'>F<sub>matky</sub></span> = <span class='frac'><span class='num'>MS<sub>matka(otec)</sub></span><span class='den'>MS<sub>reziduální</sub></span></span>
                </div>
                ")
              )
            )
          ),
          
          mainPanel(
            width = 8,
            wellPanel(
              h4("Tabulka hierarchické ANOVA:"),
              tableOutput("anova_nested_table")
            ),
            plotOutput("plot_anova_nested_boxplot", height = "380px"),
            br(),
            wellPanel(
              h4("Generovaný R kód pro hierarchickou ANOVA (lm):"),
              verbatimTextOutput("out_r_script_anova_nested")
            )
          )
        )
      )
    )
  ),

  
# =================================================================
# ZÁLOŽKA 3: KORELACE A LINEÁRNÍ REGRESE
  # =================================================================
  tabPanel(
    "3. Korelace a Regrese",
    tabsetPanel(
      id = "subtab_cor_reg",
      
      # -------------------------------------------------------------
      # POD-ZÁLOŽKA 2A: KORELACE
      # -------------------------------------------------------------
      tabPanel(
        "3A. Korelace (Asociace znaků)",
        br(),
        sidebarLayout(
          sidebarPanel(
            width = 4,
            h3("Korelační analýza (r)"),
            helpText("Korelace měří těsnost oboustranného (symetrického) vztahu mezi znaky X a Y bez předpokladu příčinnosti."),
            hr(),
            sliderInput("cor_r", "Požadovaný korelační koeficient (r):", min = -0.99, max = 0.99, value = 0.75, step = 0.05),
            numericInput("cor_sd_x", "Směrodatná odchylka X (sₓ):", value = 10, min = 1, max = 50, step = 1),
            numericInput("cor_sd_y", "Směrodatná odchylka Y (sᵧ):", value = 5, min = 1, max = 50, step = 1),
            numericInput("cor_N", "Počet jedinců (N):", value = 60, min = 10, max = 500, step = 10),
            actionButton("btn_cor_gen", "Generovat nová data", class = "btn-primary"),
            hr(),
            wellPanel(
              h4("Výsledky korelace:"),
              htmlOutput("cor_stats_html")
            ),
            
            tags$details(
              class = "formula-box",
              tags$summary(tags$b(style = "color: #0072B2; font-size: 1.05em;", "Vzorce pro korelační analýzu")),
              tags$div(
                style = "margin-top: 10px;",
                HTML("
                <div class='eq-line'>
                  <b>Výběrová kovariance:</b> &nbsp;
                  <span class='math-sym'>Cov(x,y)</span> = 
                  <span class='frac'><span class='num'>∑ (x<sub>i</sub> − x̄)(y<sub>i</sub> − ȳ)</span><span class='den'>n − 1</span></span>
                </div>
                <div class='eq-line'>
                  <b>Pearsonův korelační koeficient:</b> &nbsp;
                  <span class='math-sym'>r</span> = 
                  <span class='frac'><span class='num'>Cov(x,y)</span><span class='den'>s<sub>x</sub> · s<sub>y</sub></span></span> ∈ ⟨−1, 1⟩
                </div>
                <div class='eq-line'>
                  <b>Test významnosti (t-test):</b> &nbsp;
                  <span class='math-sym'>t</span> = 
                  <span class='frac'><span class='num'>r · √<span style='text-decoration: overline;'>n − 2</span></span><span class='den'>√<span style='text-decoration: overline;'>1 − r<sup>2</sup></span></span></span>, &nbsp; <span class='math-sym'>df</span> = n − 2
                </div>
                ")
              )
            )
          ),
          
          mainPanel(
            width = 8,
            plotOutput("plot_cor_scatter", height = "420px"),
            br(),
            wellPanel(
              h4("Kovariační a Korelační matice:"),
              fluidRow(
                column(6, h5("Variančně-kovariační matice:"), verbatimTextOutput("out_cov_matrix")),
                column(6, h5("Korelační matice:"), verbatimTextOutput("out_cor_matrix"))
              )
            ),
            wellPanel(
              h4("Generovaný R kód pro korelační analýzu:"),
              verbatimTextOutput("out_r_script_cor")
            )
          )
        )
      ),
      
      # -------------------------------------------------------------
      # POD-ZÁLOŽKA 2B: LINEÁRNÍ REGRESE
      # -------------------------------------------------------------
      tabPanel(
        "3B. Lineární regrese (Příčinný model)",
        br(),
        sidebarLayout(
          sidebarPanel(
            width = 4,
            h3("Regresní model (y = a + b·x)"),
            helpText("Lineární regrese popisuje jednostrannou kauzální závislost závisle proměnné Y na nezávisle proměnné X."),
            hr(),
            sliderInput("reg_slope", "Regresní koeficient / Sklon (b):", min = -4, max = 4, value = 0.45, step = 0.05),
            numericInput("reg_intercept", "Úsek na ose Y (a):", value = 12, min = -50, max = 50, step = 1),
            sliderInput("reg_noise", "Chybový rozptyl (Šum σₑ):", min = 1, max = 20, value = 4, step = 0.5),
            numericInput("reg_N", "Počet jedinců (N):", value = 50, min = 10, max = 500, step = 10),
            actionButton("btn_reg_gen", "Vygenerovat novou regresi", class = "btn-primary"),
            hr(),
            wellPanel(
              h4("Výsledky regresního modelu:"),
              htmlOutput("reg_stats_html")
            ),
            
            tags$details(
              class = "formula-box",
              tags$summary(tags$b(style = "color: #0072B2; font-size: 1.05em;", "Vzorce pro lineární regresi")),
              tags$div(
                style = "margin-top: 10px;",
                HTML("
                <div class='eq-line'>
                  <b>Model regrese:</b> &nbsp; <span class='math-sym'>ŷ<sub>i</sub></span> = a + b<sub>y|x</sub> · x<sub>i</sub>
                </div>
                <div class='eq-line'>
                  <b>Regresní koeficient (Sklon b<sub>y|x</sub>):</b> &nbsp;
                  <span class='math-sym'>b<sub>y|x</sub></span> = 
                  <span class='frac'><span class='num'>Cov(x,y)</span><span class='den'>s<sub>x</sub><sup>2</sup></span></span> = 
                  r · <span class='frac'><span class='num'>s<sub>y</sub></span><span class='den'>s<sub>x</sub></span></span>
                </div>
                <div class='eq-line'>
                  <b>Úsek na ose Y (a):</b> &nbsp; <span class='math-sym'>a</span> = ȳ − b<sub>y|x</sub> · x̄
                </div>
                <div class='eq-line'>
                  <b>Koeficient determinace (R<sup>2</sup>):</b> &nbsp;
                  <span class='math-sym'>R<sup>2</sup></span> = r<sup>2</sup> = 
                  <span class='frac'><span class='num'>SS<sub>reg</sub></span><span class='den'>SS<sub>tot</sub></span></span> · 100%
                </div>
                <div class='eq-line'>
                  <b>Reziduum (e<sub>i</sub>):</b> &nbsp; <span class='math-sym'>e<sub>i</sub></span> = y<sub>i</sub> − ŷ<sub>i</sub>
                </div>
                ")
              )
            )
          ),
          
          mainPanel(
            width = 8,
            plotOutput("plot_reg_scatter", height = "400px"),
            br(),
            plotOutput("plot_residuals", height = "220px"),
            br(),
            wellPanel(
              h4("Generovaný R kód pro lineární regresi:"),
              verbatimTextOutput("out_r_script_reg")
            )
          )
        )
      )
    )
  ),
  
    # =================================================================
  # ZÁLOŽKA 3: KALKULÁTOR MATICOVÝCH OPERACÍ A KONFORMITY
  # =================================================================
  tabPanel(
    "4. Maticový kalkulátor a Konformita",
    tabsetPanel(
      id = "subtab_matrix",
      
      # -------------------------------------------------------------
      # POD-ZÁLOŽKA 3A: OPERACE S JEDNOU MATICÍ (A)
      # -------------------------------------------------------------
      tabPanel(
        "4A. Jedna matice (Transpozice, Determinant, Inverze)",
        br(),
        sidebarLayout(
          sidebarPanel(
            width = 4,
            h3("Zadání matice A"),
            helpText("Zadejte prvky čtvercové matice 2x2 nebo 3x3 pro výpočet transpozice, determinantu a inverzní matice."),
            hr(),
            radioButtons("mat_size", "Rozměr matice A:", choices = c("2x2" = "2", "3x3" = "3"), selected = "3"),
            
            conditionalPanel(
              condition = "input.mat_size == '2'",
              h4("Prvky matice A (2x2):"),
              fluidRow(
                column(6, numericInput("m2_11", "A[1,1]", value = 9)),
                column(6, numericInput("m2_12", "A[1,2]", value = 4))
              ),
              fluidRow(
                column(6, numericInput("m2_21", "A[2,1]", value = 5)),
                column(6, numericInput("m2_22", "A[2,2]", value = 6))
              )
            ),
            
            conditionalPanel(
              condition = "input.mat_size == '3'",
              h4("Prvky matice A (3x3):"),
              fluidRow(
                column(4, numericInput("m3_11", "A[1,1]", value = 5)),
                column(4, numericInput("m3_12", "A[1,2]", value = 3)),
                column(4, numericInput("m3_13", "A[1,3]", value = -4))
              ),
              fluidRow(
                column(4, numericInput("m3_21", "A[2,1]", value = 3)),
                column(4, numericInput("m3_22", "A[2,2]", value = -8)),
                column(4, numericInput("m3_23", "A[2,3]", value = 5))
              ),
              fluidRow(
                column(4, numericInput("m3_31", "A[3,1]", value = 8)),
                column(4, numericInput("m3_32", "A[3,2]", value = 10)),
                column(4, numericInput("m3_33", "A[3,3]", value = -3))
              )
            ),
            
            hr(),
            tags$details(
              class = "formula-box",
              tags$summary(tags$b(style = "color: #0072B2; font-size: 1.05em;", "Vzorce pro maticovou algebru")),
              tags$div(
                style = "margin-top: 10px;",
                HTML("
                <div class='eq-line'>
                  <b>Transpozice:</b> &nbsp; <span class='math-sym'>A<sup>T</sup></span> : a<sub>ij</sub><sup>T</sup> = a<sub>ji</sub>
                </div>
                <div class='eq-line'>
                  <b>Stopa matice:</b> &nbsp; <span class='math-sym'>tr(A)</span> = ∑ a<sub>ii</sub>
                </div>
                <div class='eq-line'>
                  <b>Inverzní matice:</b> &nbsp; <span class='math-sym'>A · A<sup>−1</sup></span> = <span class='math-sym'>I</span>
                </div>
                <div class='eq-line'>
                  <b>Inverze matice 2x2:</b> &nbsp; 
                  <span class='math-sym'>A<sup>−1</sup></span> = 
                  <span class='frac'><span class='num'>1</span><span class='den'>|A|</span></span>
                  <span class='mat-brackets'>
                    <table class='mat-table'>
                      <tr><td>a<sub>22</sub></td><td>−a<sub>12</sub></td></tr>
                      <tr><td>−a<sub>21</sub></td><td>a<sub>11</sub></td></tr>
                    </table>
                  </span>
                </div>
                ")
              )
            )
          ),
          
          mainPanel(
            width = 8,
            fluidRow(
              column(6, wellPanel(h4("Původní matice A:"), verbatimTextOutput("out_mat_A"))),
              column(6, wellPanel(h4("Transponovaná matice Aᵀ:"), verbatimTextOutput("out_mat_T")))
            ),
            fluidRow(
              column(6, wellPanel(h4("Determinant |A| a vlastnosti:"), htmlOutput("out_mat_det_html"))),
              column(6, wellPanel(h4("Inverzní matice A⁻¹:"), verbatimTextOutput("out_mat_inv")))
            ),
            fluidRow(
              column(12, wellPanel(h4("Kontrola: A %*% A⁻¹ (Jednotková matice I):"), verbatimTextOutput("out_mat_check")))
            ),
            wellPanel(
              h4("Generovaný R kód (pro samostatný výpočet v RStudiu):"),
              verbatimTextOutput("out_r_script_single")
            )
          )
        )
      ),
      
      # -------------------------------------------------------------
      # POD-ZÁLOŽKA 3B: OPERACE SE DVĚMA MATICEMI (A a B) & KONFORMITA
      # -------------------------------------------------------------
      tabPanel(
        "4B. Operace se dvěma maticemi (A ± B, A %*% B) & Konformita",
        br(),
        sidebarLayout(
          sidebarPanel(
            width = 4,
            h3("Volba rozměrů matic A a B"),
            helpText("Testování podmínek konformity pro sčítání, odečítání a násobení matic různého typu."),
            hr(),
            fluidRow(
              column(6, 
                     h4("Matice A (m × n):"),
                     selectInput("dim_A_r", "Řádky A (m):", choices = c(1, 2, 3), selected = 2),
                     selectInput("dim_A_c", "Sloupce A (n):", choices = c(1, 2, 3), selected = 3)
              ),
              column(6, 
                     h4("Matice B (p × k):"),
                     selectInput("dim_B_r", "Řádky B (p):", choices = c(1, 2, 3), selected = 3),
                     selectInput("dim_B_c", "Sloupce B (k):", choices = c(1, 2, 3), selected = 2)
              )
            ),
            actionButton("btn_random_mats", "Vygenerovat náhodné matice", class = "btn-primary", style = "margin-top: 10px; width: 100%;"),
            
            hr(),
            tags$details(
              class = "formula-box",
              tags$summary(tags$b(style = "color: #0072B2; font-size: 1.05em;", "Teorie konformity matic")),
              tags$div(
                style = "margin-top: 10px;",
                HTML("
                <div class='eq-line'>
                  <b>Sčítání a Odečítání (A ± B):</b><br/>
                  Vyžadují <b>shodný rozměr</b> obou matic: <span class='math-sym'>m<sub>A</sub> = m<sub>B</sub></span> a <span class='math-sym'>n<sub>A</sub> = n<sub>B</sub></span>.
                </div>
                <div class='eq-line' style='margin-top: 8px;'>
                  <b>Násobení matic (A %*% B):</b><br/>
                  Vyžaduje konformitu: <b>počet sloupců A = počet řádků B</b> (<span class='math-sym'>n<sub>A</sub> = p<sub>B</sub></span>).
                </div>
                <div class='eq-line' style='margin-top: 8px;'>
                  <b>Výsledný rozměr C = A · B:</b> &nbsp; <span class='math-sym'>m<sub>A</sub> × k<sub>B</sub></span>.
                </div>
                <div class='eq-line' style='margin-top: 8px; color: #D55E00;'>
                  <b>Nekomutativnost:</b> Obecně <span class='math-sym'>A · B ≠ B · A</span>!
                </div>
                ")
              )
            )
          ),
          
          mainPanel(
            width = 8,
            fluidRow(
              column(6, wellPanel(h4("Matice A:"), verbatimTextOutput("out_two_A"))),
              column(6, wellPanel(h4("Matice B:"), verbatimTextOutput("out_two_B")))
            ),
            
            wellPanel(
              h3("1. Sčítání a Odečítání matic (A ± B)"),
              uiOutput("ui_add_sub_status"),
              fluidRow(
                column(6, h4("Součet A + B:"), verbatimTextOutput("out_mat_add")),
                column(6, h4("Rozdíl A - B:"), verbatimTextOutput("out_mat_sub"))
              )
            ),
            
            wellPanel(
              h3("2. Násobení matic (A %*% B)"),
              uiOutput("ui_mult_status"),
              fluidRow(
                column(6, h4("Součin A %*% B:"), verbatimTextOutput("out_mat_mult")),
                column(6, h4("Součin B %*% A (Test komutativity):"), verbatimTextOutput("out_mat_mult_ba"))
              )
            ),
            
            wellPanel(
              h4("Generovaný R kód pro sčítání, odečítání a násobení:"),
              verbatimTextOutput("out_r_script_two")
            )
          )
        )
      )
    )
  ),
  # =================================================================
  # ZÁLOŽKA 4: ŘEŠENÍ SOUSTAVY ROVNIC A INVERZE MATICE
  # =================================================================
  tabPanel(
    "5. Řešení soustavy rovnic",
    tabsetPanel(
      id = "subtab_equations",
      
      # -------------------------------------------------------------
      # POD-ZÁLOŽKA 4A: PODROBNÝ POSTUP INVERZE MATICE (KROK ZA KROKEM)
      # -------------------------------------------------------------
      tabPanel(
        "5A. Podrobný postup inverze matice A",
        br(),
        sidebarLayout(
          sidebarPanel(
            width = 4,
            h3("Algebraická inverze matice A"),
            helpText("Krok za krokem: Výpočet determinantu |A|, matice kofaktorů C, adjungované matice adj(A) a finální inverzní matice A⁻¹."),
            hr(),
            radioButtons("inv_step_size", "Rozměr matice A:",
                         choices = c("2x2" = "2", "3x3" = "3"),
                         selected = "3"),
            hr(),
            conditionalPanel(
              condition = "input.inv_step_size == '2'",
              h4("Prvky matice A (2x2):"),
              fluidRow(
                column(6, numericInput("inv2_11", "A[1,1]", value = 9)),
                column(6, numericInput("inv2_12", "A[1,2]", value = 4))
              ),
              fluidRow(
                column(6, numericInput("inv2_21", "A[2,1]", value = 5)),
                column(6, numericInput("inv2_22", "A[2,2]", value = 6))
              )
            ),
            conditionalPanel(
              condition = "input.inv_step_size == '3'",
              h4("Prvky matice A (3x3):"),
              fluidRow(
                column(4, numericInput("inv3_11", "A[1,1]", value = 5)),
                column(4, numericInput("inv3_12", "A[1,2]", value = 3)),
                column(4, numericInput("inv3_13", "A[1,3]", value = -4))
              ),
              fluidRow(
                column(4, numericInput("inv3_21", "A[2,1]", value = 3)),
                column(4, numericInput("inv3_22", "A[2,2]", value = -8)),
                column(4, numericInput("inv3_23", "A[2,3]", value = 5))
              ),
              fluidRow(
                column(4, numericInput("inv3_31", "A[3,1]", value = 8)),
                column(4, numericInput("inv3_32", "A[3,2]", value = 10)),
                column(4, numericInput("inv3_33", "A[3,3]", value = -3))
              )
            ),
            hr(),
            tags$details(
              class = "formula-box",
              tags$summary(tags$b(style = "color: #0072B2; font-size: 1.05em;", "Teoretický vzorec inverze")),
              tags$div(
                style = "margin-top: 10px;",
                HTML("
                <div class='eq-line'>
                  <b>Vzorec pro inverzní matici:</b>
                </div>
                <div class='eq-line'>
                  <span class='math-sym'>A<sup>−1</sup></span> = 
                  <span class='frac'><span class='num'>1</span><span class='den'>|A|</span></span> · <span class='math-sym'>adj(A)</span> = 
                  <span class='frac'><span class='num'>1</span><span class='den'>|A|</span></span> · <span class='math-sym'>C<sup>T</sup></span>
                </div>
                <div class='eq-line' style='margin-top: 6px;'>
                  <b>Podmínka existence:</b> &nbsp; <span class='math-sym'>|A| ≠ 0</span>
                </div>
                <div class='eq-line' style='margin-top: 6px;'>
                  <b>Definice kofaktoru:</b> &nbsp; <span class='math-sym'>C<sub>ij</sub></span> = (−1)<sup>i+j</sup> · <span class='math-sym'>M<sub>ij</sub></span>
                </div>
                ")
              )
            )
          ),
          
          mainPanel(
            width = 8,
            wellPanel(
              h4("Krok 1: Výpočet determinantu |A|"),
              htmlOutput("out_step_det_html")
            ),
            wellPanel(
              h4("Krok 2: Matice algebraických doplňků / kofaktorů C"),
              htmlOutput("out_step_cofactors_html"),
              br(),
              h5("Matice kofaktorů C:"),
              verbatimTextOutput("out_mat_C_step")
            ),
            wellPanel(
              h4("Krok 3: Transponovaná matice doplňků / Adjungovaná matice adj(A) = Cᵀ"),
              verbatimTextOutput("out_mat_adj_step")
            ),
            wellPanel(
              h4("Krok 4: Finální inverzní matice A⁻¹ = (1 / |A|) · adj(A)"),
              htmlOutput("out_step_inv_explanation_html"),
              br(),
              h5("Vypočtená inverzní matice A⁻¹:"),
              verbatimTextOutput("out_mat_inv_step"),
              hr(),
              h5("Kontrola vynásobením A %*% A⁻¹ (Jednotková matice I):"),
              verbatimTextOutput("out_mat_check_step")
            ),
            wellPanel(
              h4("Generovaný R kód pro krok za krokem výpočet inverze:"),
              verbatimTextOutput("out_r_script_inv_step")
            )
          )
        )
      ),
      
      # -------------------------------------------------------------
      # POD-ZÁLOŽKA 4B: PŘÍKLADY SOUSTAVY ROVNIC
      # -------------------------------------------------------------
      tabPanel(
        "5B. Příklady soustavy rovnic",
        br(),
        sidebarLayout(
          sidebarPanel(
            width = 4,
            h3("Soustava A · b = y"),
            helpText("Řešení soustavy lineárních rovnic maticovou algebrou: b = A⁻¹ · y."),
            hr(),
            radioButtons("eq_preset", "Výchozí příklady z přednášky:",
                         choices = c("Příklad 1: Zelinářství (2 rovnice)" = "p1",
                                     "Příklad 2: Soustava 3 rovnic" = "p2")),
            hr(),
            uiOutput("ui_eq_inputs"),
            
            tags$details(
              class = "formula-box",
              tags$summary(tags$b(style = "color: #0072B2; font-size: 1.05em;", "Teoretický postup")),
              tags$div(
                style = "margin-top: 10px;",
                HTML("
                <div class='eq-line'>
                  <b>Soustava rovnic:</b> &nbsp; <span class='math-sym'>A · b = y</span>
                </div>
                <div class='eq-line'>
                  Vynásobením zleva inverzí <span class='math-sym'>A<sup>−1</sup></span>:
                </div>
                <div class='eq-line'>
                  <span class='math-sym'>A<sup>−1</sup> A b = A<sup>−1</sup> y</span>
                </div>
                <div class='eq-line'>
                  Jelikož <span class='math-sym'>A<sup>−1</sup> A = I</span>:
                </div>
                <div class='eq-line'>
                  <b>Řešení:</b> &nbsp; <span class='math-sym'>b = A<sup>−1</sup> y</span>
                </div>
                <div class='eq-line' style='margin-top: 6px;'>
                  <b>Podmínka:</b> &nbsp; <span class='math-sym'>det(A) ≠ 0</span>
                </div>
                ")
              )
            )
          ),
          
          mainPanel(
            width = 8,
            wellPanel(
              h3("Matice soustavy a pravá strana:"),
              fluidRow(
                column(6, h4("Matice koeficientů A:"), verbatimTextOutput("out_eq_A")),
                column(6, h4("Vektor pravých stran y:"), verbatimTextOutput("out_eq_y"))
              )
            ),
            wellPanel(
              h3("Výpočet a výsledky:"),
              htmlOutput("out_eq_solution_html"),
              hr(),
              h4("Vektor neznámých b:"),
              verbatimTextOutput("out_eq_b_vec")
            ),
            wellPanel(
              h4("Grafická vizualizace řešení:"),
              plotOutput("plot_eq_lines", height = "320px")
            ),
            wellPanel(
              h4("Generovaný R kód pro řešení soustavy:"),
              verbatimTextOutput("out_r_script_eq")
            )
          )
        )
      )
    )
  ),
  
  # =================================================================
  # ZÁLOŽKA 6: MATICOVÉ ŘEŠENÍ 1-FAKTOROVÉ ANOVA
  # =================================================================
  tabPanel(
    "6. Maticové řešení ANOVA",
    tabsetPanel(
      id = "subtab_anova_matrix",
      
      # -------------------------------------------------------------
      # POD-ZÁLOŽKA 6A: SOUSTAVA NORMÁLNÍCH ROVNIC (X, XᵀX, Xᵀy, b)
      # -------------------------------------------------------------
      tabPanel(
        "6A. Soustava normálních rovnic (X, XᵀX, Xᵀy, b)",
        br(),
        sidebarLayout(
          sidebarPanel(
            width = 4,
            h3("Soustava normálních rovnic"),
            helpText("Jednofaktorová ANOVA vyjádřená jako lineární model y = X·b + e. Odhad parametrů se získá řešením soustavy (XᵀX)b = Xᵀy."),
            hr(),
            radioButtons("anova_mat_preset", "Výchozí datový soubor:",
                         choices = c("Příklad: 3 otci (n = 9 potomků)" = "small_3",
                                     "Příklad: Snáška (4 otci, n = 48)" = "poultry")),
            radioButtons("anova_mat_model_type", "Reprezentace modelu:",
                         choices = c("Model skupinových průměrů (X = indikátory skupin)" = "cell_means",
                                     "Model s referenční skupinou (Intercept + efekty)" = "ref_group")),
            hr(),
            wellPanel(
              h4("Souhrn odhadnutých parametrů b:"),
              htmlOutput("out_anova_mat_b_summary")
            ),
            
            tags$details(
              class = "formula-box",
              tags$summary(tags$b(style = "color: #0072B2; font-size: 1.05em;", "Teorie a maticové vzorce")),
              tags$div(
                style = "margin-top: 10px;",
                HTML("
                <div class='eq-line'>
                  <b>Lineární model:</b> &nbsp; <span class='math-sym'>y</span> = <span class='math-sym'>X · b + e</span>
                </div>
                <div class='eq-line'>
                  <b>Soustava normálních rovnic:</b> &nbsp;
                  <span class='math-sym'>X<sup>T</sup> X · b</span> = <span class='math-sym'>X<sup>T</sup> y</span>
                </div>
                <div class='eq-line'>
                  <b>Řešení odhadů b:</b> &nbsp;
                  <span class='math-sym'>b̂</span> = (X<sup>T</sup> X)<sup>−1</sup> X<sup>T</sup> y
                </div>
                <div class='eq-line' style='margin-top: 6px;'>
                  <b>Vlastnosti X<sup>T</sup>X:</b> V modelu skupinových průměrů je X<sup>T</sup>X diagonální matice obsahující počty prvků ve skupinách <i>n<sub>i</sub></i>.
                </div>
                <div class='eq-line' style='margin-top: 6px;'>
                  <b>Vlastnosti X<sup>T</sup>y:</b> Vektor X<sup>T</sup>y obsahuje součty naměřených hodnot v jednotlivých skupinách <i>Y<sub>i·</sub></i>.
                </div>
                ")
              )
            )
          ),
          
          mainPanel(
            width = 8,
            wellPanel(
              h3("Vektor pozorování y a matice plánu X:"),
              fluidRow(
                column(5, h4("Vektor y (prvních 12 prvků):"), verbatimTextOutput("out_anova_mat_y")),
                column(7, h4("Design matice X (prvních 12 řádků):"), verbatimTextOutput("out_anova_mat_X"))
              )
            ),
            wellPanel(
              h3("Soustava normálních rovnic: (XᵀX) b = Xᵀy"),
              fluidRow(
                column(6, h4("Matice soustavy XᵀX:"), verbatimTextOutput("out_anova_mat_XtX")),
                column(6, h4("Vektor pravých stran Xᵀy:"), verbatimTextOutput("out_anova_mat_Xty"))
              )
            ),
            wellPanel(
              h3("Odhadnutý vektor parametrů b = (XᵀX)⁻¹ Xᵀy:"),
              verbatimTextOutput("out_anova_mat_b_vec")
            ),
            wellPanel(
              h4("Generovaný R kód pro maticové řešení ANOVA:"),
              verbatimTextOutput("out_r_script_anova_matrix")
            )
          )
        )
      ),
      
      # -------------------------------------------------------------
      # POD-ZÁLOŽKA 6B: ROZKLAD VARIABILITY & PROJEKČNÍ MATICE (P_X, e, SS)
      # -------------------------------------------------------------
      tabPanel(
        "6B. Rozklad variability & Projekční matice (P_X, e, SS)",
        br(),
        sidebarLayout(
          sidebarPanel(
            width = 4,
            h3("Projekční matice a rezidua"),
            helpText("Maticový rozklad celkového součtu čtverců na meziskupinový a reziduální pomocí projekční matice P_X (Hat matrix)."),
            hr(),
            wellPanel(
              h4("Shrnutí maticového rozkladu SS:"),
              htmlOutput("out_anova_mat_ss_html")
            ),
            
            tags$details(
              class = "formula-box",
              tags$summary(tags$b(style = "color: #0072B2; font-size: 1.05em;", "Vzorce projekčních matic")),
              tags$div(
                style = "margin-top: 10px;",
                HTML("
                <div class='eq-line'>
                  <b>Projekční matice (Hat matrix):</b><br/>
                  <span class='math-sym'>P<sub>X</sub></span> = X (X<sup>T</sup> X)<sup>−1</sup> X<sup>T</sup>
                </div>
                <div class='eq-line'>
                  <b>Vyrovnané hodnoty ŷ:</b> &nbsp; <span class='math-sym'>ŷ</span> = P<sub>X</sub> y
                </div>
                <div class='eq-line'>
                  <b>Vektor reziduí e:</b> &nbsp; <span class='math-sym'>e</span> = y − ŷ = (I − P<sub>X</sub>) y
                </div>
                <div class='eq-line' style='margin-top: 6px;'>
                  <b>SS celkový:</b> &nbsp; <span class='math-sym'>SS<sub>tot</sub></span> = y<sup>T</sup> y − <span class='frac'><span class='num'>1</span><span class='den'>n</span></span> y<sup>T</sup> J y
                </div>
                <div class='eq-line'>
                  <b>SS mezi:</b> &nbsp; <span class='math-sym'>SS<sub>mezi</sub></span> = y<sup>T</sup> P<sub>X</sub> y − <span class='frac'><span class='num'>1</span><span class='den'>n</span></span> y<sup>T</sup> J y
                </div>
                <div class='eq-line'>
                  <b>SS reziduální:</b> &nbsp; <span class='math-sym'>SS<sub>rez</sub></span> = e<sup>T</sup> e = y<sup>T</sup> (I − P<sub>X</sub>) y
                </div>
                ")
              )
            )
          ),
          
          mainPanel(
            width = 8,
            wellPanel(
              h3("Projekční matice P_X a vyrovnané hodnoty ŷ:"),
              fluidRow(
                column(7, h4("Projekční matice P_X = X(XᵀX)⁻¹Xᵀ (výsek):"), verbatimTextOutput("out_anova_mat_PX")),
                column(5, h4("Vyrovnané hodnoty ŷ = P_X y:"), verbatimTextOutput("out_anova_mat_yhat"))
              )
            ),
            wellPanel(
              h3("Vektor reziduí e = y − ŷ:"),
              verbatimTextOutput("out_anova_mat_e")
            ),
            wellPanel(
              h4("Generovaný R kód pro projekční matici a rozklad SS:"),
              verbatimTextOutput("out_r_script_anova_proj")
            )
          )
        )
      )
    )
  )
)
# SERVEROVÁ LOGIKA
# -------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Vyskakovací okno pro citace
  observeEvent(input$btn_cite_modal, {
    showModal(modalDialog(
      title = "Jak citovat tuto aplikaci",
      size = "m",
      easyClose = TRUE,
      footer = modalButton("Zavřít"),
      p("Pokud aplikaci využijete ve své práci nebo výuce, citujte ji prosím následovně:"),
      
      tags$b("ČSN ISO 690:2022:"),
      div(
        style = "background: #f4f6f8; border-left: 3px solid #2c7be5; padding: 8px 12px; margin: 5px 0 15px 0; font-family: monospace; font-size: 0.9em;",
        "URBAN, Tomáš, 2026. Biostatistika a maticová algebra [interaktivní webová aplikace]. Brno: Mendelova univerzita v Brně [cit. 2026-10-16]. Dostupné z: https://tgu-gen.github.io/QG1/"
      ),
      
      tags$b("APA 7:"),
      div(
        style = "background: #f4f6f8; border-left: 3px solid #28a745; padding: 8px 12px; margin: 5px 0 10px 0; font-family: monospace; font-size: 0.9em;",
        "Urban, T. (2026). Biostatistika a maticová algebra [Interaktivní webová aplikace]. Mendelova univerzita v Brně. https://tgu-gen.github.io/QG1/"
      )
    ))
  })
  
  # -----------------------------------------------------------------
  # LOGIKA ZÁLOŽKY 1: POPISNÁ STATISTIKA
  # -----------------------------------------------------------------
  get_desc_data <- reactive({
    if (input$dist_mode == "gen") {
      input$btn_reseed
      isolate({
        rnorm(input$pop_N, mean = input$pop_mean, sd = input$pop_sd)
      })
    } else {
      raw_txt <- input$manual_data
      vals <- as.numeric(trimws(unlist(strsplit(raw_txt, "[,;\\s]+"))))
      vals <- vals[!is.na(vals)]
      if (length(vals) < 2) vals <- c(11, 4, 9, 8, 12, 17, 3, 5, 16, 15)
      vals
    }
  })
  
  output$desc_stats_html <- renderUI({
    y <- get_desc_data()
    n <- length(y)
    m <- mean(y)
    v <- var(y)
    s <- sd(y)
    se <- s / sqrt(n)
    cv <- (s / m) * 100
    
    HTML(paste0(
      "<b>Počet pozorování (n):</b> ", n, "<br/>",
      "<b>Průměr (ȳ):</b> <span style='color: #0072B2; font-weight: bold;'>", round(m, 3), "</span><br/>",
      "<b>Rozptyl (s²):</b> ", round(v, 3), "<br/>",
      "<b>Směrodatná odchylka (s):</b> <span style='color: #D55E00; font-weight: bold;'>", round(s, 3), "</span><br/>",
      "<b>Střední chyba průměru (SE):</b> ", round(se, 3), "<br/>",
      "<b>Variační koeficient (CV):</b> ", round(cv, 2), " %"
    ))
  })
  
  output$plot_histogram <- renderPlot({
    y <- get_desc_data()
    df <- data.frame(y = y)
    m <- mean(y)
    s <- sd(y)
    
    ggplot(df, aes(x = y)) +
      geom_histogram(aes(y = after_stat(density)), bins = max(8, floor(length(y)/4)), 
                     fill = "#56B4E9", color = "white", alpha = 0.7) +
      stat_function(fun = dnorm, args = list(mean = m, sd = s), color = "#D55E00", linewidth = 1.3) +
      geom_vline(xintercept = m, color = "#0072B2", linewidth = 1.2, linetype = "solid") +
      geom_vline(xintercept = c(m - s, m + s), color = "#E69F00", linewidth = 1, linetype = "dashed") +
      geom_vline(xintercept = c(m - 2*s, m + 2*s), color = "#CC79A7", linewidth = 0.8, linetype = "dotted") +
      labs(title = "Histogram naměřených dat s proloženou křivkou normálního rozdělení N(μ, σ²)",
           subtitle = "Modrá plná čára = Průměr | Oranžová čárkovaná = ±1 s (68.3 %) | Fialová tečkovaná = ±2 s (95.4 %)",
           x = "Hodnota znaku (y)", y = "Hustota pravděpodobnosti") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold"),
            panel.background = element_rect(fill = "white", color = NA),
            plot.background = element_rect(fill = "white", color = NA))
  })
  
  output$plot_boxplot <- renderPlot({
    y <- get_desc_data()
    df <- data.frame(y = y)
    
    ggplot(df, aes(x = "Znak", y = y)) +
      geom_boxplot(fill = "#E69F00", alpha = 0.6, outlier.color = "#D55E00", outlier.size = 3) +
      geom_jitter(width = 0.15, color = "#0072B2", size = 2.5, alpha = 0.7) +
      coord_flip() +
      labs(title = "Krabičkový graf (Boxplot) s rozptylem jednotlivých pozorování", x = "", y = "Hodnota znaku") +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"),
            panel.background = element_rect(fill = "white", color = NA),
            plot.background = element_rect(fill = "white", color = NA))
  })
  
  # -----------------------------------------------------------------
  # LOGIKA POD-ZÁLOŽKY 1B: SROVNÁNÍ NORMÁLNÍHO ROZDĚLENÍ
  # -----------------------------------------------------------------
  output$plot_norm_compare <- renderPlot({
    m1 <- input$norm_mean1
    sd1 <- input$norm_sd1
    var1 <- sd1^2
    
    m2 <- input$norm_mean2
    sd2 <- input$norm_sd2
    var2 <- sd2^2
    
    c3_active <- isTRUE(input$show_curve3)
    
    all_means <- c(m1, m2)
    all_sds <- c(sd1, sd2)
    if (c3_active) {
      m3 <- input$norm_mean3
      sd3 <- input$norm_sd3
      var3 <- sd3^2
      all_means <- c(all_means, m3)
      all_sds <- c(all_sds, sd3)
    }
    
    min_x <- min(all_means - 3.8 * all_sds)
    max_x <- max(all_means + 3.8 * all_sds)
    x_grid <- seq(min_x, max_x, length.out = 600)
    
    df1 <- data.frame(x = x_grid, y = dnorm(x_grid, mean = m1, sd = sd1), Kurv = "Křivka 1")
    df2 <- data.frame(x = x_grid, y = dnorm(x_grid, mean = m2, sd = sd2), Kurv = "Křivka 2")
    
    df_all <- rbind(df1, df2)
    if (c3_active) {
      df3 <- data.frame(x = x_grid, y = dnorm(x_grid, mean = m3, sd = sd3), Kurv = "Křivka 3")
      df_all <- rbind(df_all, df3)
    }
    
    peak1_y <- dnorm(m1, mean = m1, sd = sd1)
    peak2_y <- dnorm(m2, mean = m2, sd = sd2)
    
    colors_map <- c("Křivka 1" = "#0072B2", "Křivka 2" = "#D55E00")
    if (c3_active) {
      colors_map["Křivka 3"] <- "#009E73"
    }
    
    p <- ggplot(df_all, aes(x = x, y = y, color = Kurv, fill = Kurv)) +
      geom_polygon(alpha = 0.15, linewidth = 0.2) +
      geom_line(linewidth = 1.4) +
      geom_vline(xintercept = m1, color = "#0072B2", linetype = "dashed", linewidth = 0.9) +
      geom_vline(xintercept = m2, color = "#D55E00", linetype = "dashed", linewidth = 0.9) +
      scale_color_manual(values = colors_map) +
      scale_fill_manual(values = colors_map) +
      labs(
        title = "Porovnání hustot pravděpodobnosti normálního rozdělení N(μ, σ²)",
        subtitle = "Sledování vlivu posunu průměru (μ) a strmosti/šířky rozptylu (σ²)",
        x = "Hodnota znaku (X)",
        y = "Hustota pravděpodobnosti f(x)",
        color = "Křivka", fill = "Křivka"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold"),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.position = "top"
      )
    
    if (c3_active) {
      p <- p + geom_vline(xintercept = m3, color = "#009E73", linetype = "dashed", linewidth = 0.9)
    }
    
    lbl1 <- paste0("Křivka 1:
μ₁ = ", m1, "
σ₁² = ", round(var1, 1), "
(σ₁ = ", sd1, ")")
    p <- p + annotate("label", x = m1, y = peak1_y * 1.05, label = lbl1,
                      color = "#0072B2", fill = "white", fontface = "bold", size = 3.8, alpha = 0.85)
    
    lbl2 <- paste0("Křivka 2:
μ₂ = ", m2, "
σ₂² = ", round(var2, 1), "
(σ₂ = ", sd2, ")")
    p <- p + annotate("label", x = m2, y = peak2_y * 1.05, label = lbl2,
                      color = "#D55E00", fill = "white", fontface = "bold", size = 3.8, alpha = 0.85)
    
    if (c3_active) {
      peak3_y <- dnorm(m3, mean = m3, sd = sd3)
      lbl3 <- paste0("Křivka 3:
μ₃ = ", m3, "
σ₃² = ", round(var3, 1), "
(σ₃ = ", sd3, ")")
      p <- p + annotate("label", x = m3, y = peak3_y * 1.05, label = lbl3,
                        color = "#009E73", fill = "white", fontface = "bold", size = 3.8, alpha = 0.85)
    }
    
    p
  })

  output$norm_compare_summary_html <- renderUI({
    m1 <- input$norm_mean1
    sd1 <- input$norm_sd1
    var1 <- sd1^2
    peak1 <- dnorm(m1, mean = m1, sd = sd1)
    
    m2 <- input$norm_mean2
    sd2 <- input$norm_sd2
    var2 <- sd2^2
    peak2 <- dnorm(m2, mean = m2, sd = sd2)
    
    c3_active <- isTRUE(input$show_curve3)
    
    txt1 <- paste0("<span style='color: #0072B2; font-weight: bold;'>Křivka 1 (Modrá):</span> Průměr μ₁ = <b>", m1, "</b> | Rozptyl σ₁² = <b>", round(var1, 2), "</b> (σ₁ = ", sd1, ") | Max. hustota f(μ₁) = <b>", round(peak1, 4), "</b>")
    txt2 <- paste0("<span style='color: #D55E00; font-weight: bold;'>Křivka 2 (Oranžová):</span> Průměr μ₂ = <b>", m2, "</b> | Rozptyl σ₂² = <b>", round(var2, 2), "</b> (σ₂ = ", sd2, ") | Max. hustota f(μ₂) = <b>", round(peak2, 4), "</b>")
    
    txt3 <- ""
    if (c3_active) {
      m3 <- input$norm_mean3
      sd3 <- input$norm_sd3
      var3 <- sd3^2
      peak3 <- dnorm(m3, mean = m3, sd = sd3)
      txt3 <- paste0("<br/><span style='color: #009E73; font-weight: bold;'>Křivka 3 (Zelená):</span> Průměr μ₃ = <b>", m3, "</b> | Rozptyl σ₃² = <b>", round(var3, 2), "</b> (σ₃ = ", sd3, ") | Max. hustota f(μ₃) = <b>", round(peak3, 4), "</b>")
    }
    
    HTML(paste0(txt1, "<br/>", txt2, txt3))
  })

  # -----------------------------------------------------------------
    # -----------------------------------------------------------------
  # -----------------------------------------------------------------
  # LOGIKA ZÁLOŽKY 2: ANALÝZA VARIANCE (ANOVA)
  # -----------------------------------------------------------------
  get_anova_data <- reactive({
    preset <- input$anova_preset
    
    if (preset == "poultry_sig") {
      # Dataset z přednášky / cvičení 6: 48 slepic po 4 otcích (PRŮKAZNÝ EFEKT, p < 0.001)
      data.frame(
        otec = factor(rep(c("k1", "k2", "k3", "k4"), times = c(6, 6, 12, 24))),
        snaska = c(
          172, 253, 239, 203, 247, 256,                 # k1 (n = 6)
          137, 197, 200, 198, 131, 145,                 # k2 (n = 6)
          209, 187, 183, 215, 215, 134, 185, 218, 155, 214, 184, 176, # k3 (n = 12)
          172, 166, 121, 115, 134, 134, 127, 115, 138, 116, 124, 197,
          118, 129, 113, 138, 133, 116, 112, 114, 110, 124, 118, 138  # k4 (n = 24)
        )
      )
    } else if (preset == "poultry_nonsig") {
      # Srovnávací dataset: Snáška vyrovnaná bez efektu otce (NEPRŮKAZNÝ EFEKT, p = 0.79)
      data.frame(
        otec = factor(rep(c("k1", "k2", "k3", "k4"), times = c(6, 6, 12, 24))),
        snaska = c(
          162, 153, 159, 163, 157, 156,
          157, 167, 160, 158, 161, 155,
          159, 157, 163, 165, 155, 164, 158, 161, 155, 162, 157, 159,
          158, 162, 161, 159, 160, 157, 163, 158, 160, 159, 162, 157, 158, 161, 159, 160, 158, 162, 157, 161, 159, 160, 158, 161
        )
      )
    } else if (preset == "sim_sig") {
      # Simulovaná data s výrazným průkazným efektem otce
      set.seed(123)
      g1 <- rnorm(10, mean = 175, sd = 10)
      g2 <- rnorm(10, mean = 140, sd = 10)
      g3 <- rnorm(12, mean = 160, sd = 10)
      g4 <- rnorm(15, mean = 125, sd = 10)
      data.frame(
        otec = factor(rep(c("Otec_A", "Otec_B", "Otec_C", "Otec_D"), times = c(10, 10, 12, 15))),
        snaska = round(c(g1, g2, g3, g4), 1)
      )
    } else if (preset == "sim_nonsig") {
      # Simulovaná data s nulovým neprůkazným efektem otce (garantovaný neprůkazný výsledek, p >= 0.05)
      set.seed(101)
      g1 <- rnorm(10, mean = 150, sd = 15)
      g2 <- rnorm(10, mean = 150, sd = 15)
      g3 <- rnorm(12, mean = 150, sd = 15)
      g4 <- rnorm(15, mean = 150, sd = 15)
      # Vyrovnání skupinových průměrů pro garantovanou neprůkaznost (H0 nezamítnuta, p >= 0.05)
      g1 <- g1 - mean(g1) + 150.1
      g2 <- g2 - mean(g2) + 149.9
      g3 <- g3 - mean(g3) + 150.2
      g4 <- g4 - mean(g4) + 149.8
      data.frame(
        otec = factor(rep(c("Otec_A", "Otec_B", "Otec_C", "Otec_D"), times = c(10, 10, 12, 15))),
        snaska = round(c(g1, g2, g3, g4), 1)
      )
    } else {
      # Vlastní simulace přes posuvníky
      input$btn_anova_reseed
      isolate({
        eff <- input$anova_sim_diff
        sd_val <- input$anova_sim_sd
        g1 <- rnorm(10, mean = 150 + eff, sd = sd_val)
        g2 <- rnorm(10, mean = 150, sd = sd_val)
        g3 <- rnorm(12, mean = 150 + eff/2, sd = sd_val)
        g4 <- rnorm(15, mean = 150 - eff/2, sd = sd_val)
        data.frame(
          otec = factor(rep(c("Otec_A", "Otec_B", "Otec_C", "Otec_D"), times = c(10, 10, 12, 15))),
          snaska = round(c(g1, g2, g3, g4), 1)
        )
      })
    }
  })

  output$anova_f_summary_html <- renderUI({
    df <- get_anova_data()
    fit <- aov(snaska ~ otec, data = df)
    s_fit <- summary(fit)[[1]]
    f_val <- s_fit["otec", "F value"]
    p_val <- s_fit["otec", "Pr(>F)"]
    df_between <- s_fit["otec", "Df"]
    df_within <- s_fit["Residuals", "Df"]
    f_crit <- qf(0.95, df_between, df_within)
    
    status_txt <- if (p_val < 0.05) {
      "<span style='color: green; font-weight: bold;'>F > F_krit (p < 0.05) → ZAMÍTÁME H₀ ve prospěch H₁</span><br/><b>Závěr:</b> Efekt otce/genetického faktoru je <span style='color: green; font-weight: bold;'>STATISTICKY VÝZNAMNÝ (PRŮKAZNÝ)</span>."
    } else {
      "<span style='color: orange; font-weight: bold;'>F ≤ F_krit (p ≥ 0.05) → NEZAMÍTÁME H₀</span><br/><b>Závěr:</b> Efekt otce/genetického faktoru <span style='color: orange; font-weight: bold;'>NENÍ STATISTICKY VÝZNAMNÝ (NEPRŮKAZNÝ)</span>."
    }
    
    HTML(paste0(
      "<b>Vypočtená F-hodnota:</b> <span style='color: #0072B2; font-weight: bold; font-size: 1.1em;'>", round(f_val, 3), "</span><br/>",
      "<b>Kritická hodnota F (α = 0.05):</b> ", round(f_crit, 3), " (df₁ = ", df_between, ", df₂ = ", df_within, ")<br/>",
      "<b>p-hodnota testu:</b> ", format.pval(p_val, digits = 4), "<br/>",
      "<b>Rozhodnutí:</b> ", status_txt
    ))
  })

  output$anova_manual_table <- renderTable({
    df <- get_anova_data()
    y <- df$snaska
    group <- df$otec
    n <- length(y)
    groups <- levels(group)
    i <- length(groups)
    
    Y_dotdot <- sum(y)
    SS_total <- sum(y^2) - (Y_dotdot^2 / n)
    
    group_totals <- tapply(y, group, sum)
    group_counts <- tapply(y, group, length)
    
    SS_between <- sum((group_totals^2) / group_counts) - (Y_dotdot^2 / n)
    SS_within <- SS_total - SS_between
    
    df_between <- i - 1
    df_within <- n - i
    df_total <- n - 1
    
    MS_between <- SS_between / df_between
    MS_within <- SS_within / df_within
    
    F_val <- MS_between / MS_within
    p_val <- 1 - pf(F_val, df_between, df_within)
    
    res_df <- data.frame(
      "Zdroj variability" = c("Mezi skupinami (Otci)", "Uvnitř skupin (Rezidua)", "Celková"),
      "Součet čtverců (SS)" = c(round(SS_between, 2), round(SS_within, 2), round(SS_total, 2)),
      "Stupně volnosti (df)" = c(df_between, df_within, df_total),
      "Průměrný čtverec (MS)" = c(round(MS_between, 2), round(MS_within, 2), ""),
      "F-hodnota" = c(round(F_val, 3), "", ""),
      "p-hodnota" = c(format.pval(p_val, digits = 4), "", ""),
      check.names = FALSE
    )
    res_df
  })

  output$plot_anova_boxplot <- renderPlot({
    df <- get_anova_data()
    
    ggplot(df, aes(x = otec, y = snaska, fill = otec)) +
      geom_boxplot(alpha = 0.6, outlier.color = "#D55E00", outlier.size = 3) +
      geom_jitter(width = 0.15, color = "#222222", size = 2, alpha = 0.6) +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 4, fill = "white", color = "black") +
      labs(title = "Rozložení hodnot snášky slepic podle otců (s vyznačenými průměry ◇)",
           x = "Otec / Kohout (Skupina)", y = "Snáška (počet vajec / rok)", fill = "Otec") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold"),
            legend.position = "none",
            panel.background = element_rect(fill = "white", color = NA),
            plot.background = element_rect(fill = "white", color = NA))
  })

  output$out_r_script_anova_manual <- renderText({
    df <- get_anova_data()
    y_vals <- paste(df$snaska, collapse = ", ")
    g_vals <- paste(paste0('"', df$otec, '"'), collapse = ", ")
    
    paste0(
      "# --- Jednofaktorová ANOVA podle matematických vzorců v R ---
",
      "y <- c(", y_vals, ")
",
      "g <- factor(c(", g_vals, "))
",
      "n <- length(y)

",
      "# 1. Součty čtverců (SS)
",
      "Y_dotdot  <- sum(y)
",
      "SS_total  <- sum(y^2) - (Y_dotdot^2 / n)
",
      "SS_between<- sum((tapply(y, g, sum)^2) / tapply(y, g, length)) - (Y_dotdot^2 / n)
",
      "SS_within <- SS_total - SS_between

",
      "# 2. Stupně volnosti a Průměrné čtverce (MS)
",
      "df_between <- length(levels(g)) - 1
",
      "df_within  <- n - length(levels(g))
",
      "MS_between <- SS_between / df_between
",
      "MS_within  <- SS_within / df_within

",
      "# 3. F-test
",
      "F_stat <- MS_between / MS_within
",
      "p_val  <- 1 - pf(F_stat, df_between, df_within)
",
      "cat('F =', round(F_stat, 3), '| p-val =', format.pval(p_val), '\n')
"
    )
  })

  output$out_anova_aov_summary <- renderPrint({
    df <- get_anova_data()
    fit <- aov(snaska ~ otec, data = df)
    summary(fit)
  })

  output$out_anova_tukey_summary <- renderPrint({
    df <- get_anova_data()
    fit <- aov(snaska ~ otec, data = df)
    TukeyHSD(fit)
  })

  output$plot_anova_tukey_ci <- renderPlot({
    df <- get_anova_data()
    fit <- aov(snaska ~ otec, data = df)
    tuk <- TukeyHSD(fit)
    
    par(mar = c(4, 8, 3, 2))
    plot(tuk, las = 1, col = "#0072B2")
    title(main = "95% konfidenční intervaly rozdílů průměrů (Tukey HSD)")
    abline(v = 0, lty = 2, col = "red", lwd = 1.5)
    par(mar = c(5, 4, 4, 2) + 0.1)
  })

  output$out_r_script_anova_func <- renderText({
    paste0(
      "# --- Jednofaktorová ANOVA a Post-hoc testy v R ---
",
      "library(emmeans)
",
      "library(multcomp)

",
      "# 1. Model ANOVA pomocí aov() i lm()
",
      "fit_aov <- aov(snaska ~ otec, data = data)
",
      "summary(fit_aov)

",
      "# 2. Tukeyho HSD test
",
      "tukey_res <- TukeyHSD(fit_aov)
",
      "print(tukey_res)
",
      "plot(tukey_res)

",
      "# 3. Estimated Marginal Means a písmenové zobrazení (cld)
",
      "fit_lm <- lm(snaska ~ otec, data = data)
",
      "emm    <- emmeans(fit_lm, ~ otec)
",
      "cld_res<- cld(emm, Letters = letters, adjust = 'tukey')
",
      "print(cld_res)
"
    )
  })

  # --- Hierarchická ANOVA ---
  get_nested_data <- reactive({
    preset <- input$nested_preset
    
    if (preset == "piglets_sig") {
      # Příklad z R-cvičení 6: 21 selat od 6 matek vnořených ve 2 otcích (PRŮKAZNÉ EFEKTY)
      data.frame(
        OTEC = factor(rep(c("Otec_1", "Otec_2"), times = c(10, 11))),
        MATKA = factor(rep(c("M1.1", "M1.2", "M1.3", "M2.1", "M2.2", "M2.3"), times = c(3, 4, 3, 4, 3, 4))),
        PRIR = c(
          85, 90, 88,  92, 95, 91, 94,  87, 89, 86,       # Otec 1
          105, 102, 108, 104,  98, 100, 97,  110, 112, 109, 107 # Otec 2
        )
      )
    } else if (preset == "piglets_nonsig") {
      # Srovnávací soubor: Vyrovnané přírůstky (NEPRŮKAZNÉ EFEKTY)
      data.frame(
        OTEC = factor(rep(c("Otec_1", "Otec_2"), times = c(10, 11))),
        MATKA = factor(rep(c("M1.1", "M1.2", "M1.3", "M2.1", "M2.2", "M2.3"), times = c(3, 4, 3, 4, 3, 4))),
        PRIR = c(
          96, 95, 97,  97, 96, 95, 96,  95, 97, 96,       # Otec 1
          97, 96, 95, 96,  95, 97, 96,  96, 95, 97, 96   # Otec 2
        )
      )
    } else if (preset == "nested_sim_sig") {
      set.seed(123)
      o1_m1 <- rnorm(4, mean = 85, sd = 2)
      o1_m2 <- rnorm(4, mean = 92, sd = 2)
      o2_m1 <- rnorm(4, mean = 105, sd = 2)
      o2_m2 <- rnorm(4, mean = 112, sd = 2)
      data.frame(
        OTEC = factor(rep(c("Otec_1", "Otec_2"), each = 8)),
        MATKA = factor(rep(c("M1.1", "M1.2", "M2.1", "M2.2"), each = 4)),
        PRIR = round(c(o1_m1, o1_m2, o2_m1, o2_m2), 1)
      )
    } else if (preset == "nested_sim_nonsig") {
      # Simulovaná data s nulovými efekty Otce i Matky (garantovaný neprůkazný výsledek, p >= 0.05)
      set.seed(101)
      o1_m1 <- rnorm(4, mean = 100, sd = 5)
      o1_m2 <- rnorm(4, mean = 100, sd = 5)
      o2_m1 <- rnorm(4, mean = 100, sd = 5)
      o2_m2 <- rnorm(4, mean = 100, sd = 5)
      
      # Vyrovnání průměrů skupin pro garantovanou neprůkaznost (p >= 0.05)
      o1_m1 <- o1_m1 - mean(o1_m1) + 100.1
      o1_m2 <- o1_m2 - mean(o1_m2) + 99.9
      o2_m1 <- o2_m1 - mean(o2_m1) + 100.2
      o2_m2 <- o2_m2 - mean(o2_m2) + 99.8
      
      data.frame(
        OTEC = factor(rep(c("Otec_1", "Otec_2"), each = 8)),
        MATKA = factor(rep(c("M1.1", "M1.2", "M2.1", "M2.2"), each = 4)),
        PRIR = round(c(o1_m1, o1_m2, o2_m1, o2_m2), 1)
      )
    } else {
      input$btn_nested_reseed
      isolate({
        eff_o <- input$nested_eff_father
        eff_m <- input$nested_eff_mother
        sd_v <- input$nested_sim_sd
        
        o1_m1 <- rnorm(4, mean = 90 - eff_o/2 - eff_m/2, sd = sd_v)
        o1_m2 <- rnorm(4, mean = 90 - eff_o/2 + eff_m/2, sd = sd_v)
        o2_m1 <- rnorm(4, mean = 90 + eff_o/2 - eff_m/2, sd = sd_v)
        o2_m2 <- rnorm(4, mean = 90 + eff_o/2 + eff_m/2, sd = sd_v)
        
        data.frame(
          OTEC = factor(rep(c("Otec_1", "Otec_2"), each = 8)),
          MATKA = factor(rep(c("M1.1", "M1.2", "M2.1", "M2.2"), each = 4)),
          PRIR = round(c(o1_m1, o1_m2, o2_m1, o2_m2), 1)
        )
      })
    }
  })

  output$anova_nested_table <- renderTable({
    df <- get_nested_data()
    fit <- lm(PRIR ~ OTEC / MATKA, data = df)
    an_tbl <- anova(fit)
    
    res_df <- data.frame(
      "Zdroj variability" = c("Otec", "Matka (uvnitř Otce)", "Reziduální (Uvnitř matek)"),
      "Součet čtverců (SS)" = round(an_tbl$`Sum Sq`, 2),
      "Stupně volnosti (df)" = an_tbl$Df,
      "Průměrný čtverec (MS)" = round(an_tbl$`Mean Sq`, 2),
      "F-hodnota" = ifelse(is.na(an_tbl$`F value`), "", as.character(round(an_tbl$`F value`, 3))),
      "p-hodnota" = ifelse(is.na(an_tbl$`Pr(>F)`), "", format.pval(an_tbl$`Pr(>F)`, digits = 4)),
      check.names = FALSE
    )
    res_df
  })

  output$plot_anova_nested_boxplot <- renderPlot({
    df <- get_nested_data()
    ggplot(df, aes(x = MATKA, y = PRIR, fill = OTEC)) +
      geom_boxplot(alpha = 0.7) +
      geom_jitter(width = 0.1, color = "black", size = 2) +
      facet_wrap(~ OTEC, scales = "free_x") +
      scale_fill_manual(values = c("#0072B2", "#D55E00")) +
      labs(title = "Přírůstek selat podle otce a matky (vnořený design)",
           x = "Matka (vnořená v otci)", y = "Přírůstek (kg)", fill = "Otec") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold"),
            legend.position = "bottom",
            panel.background = element_rect(fill = "white", color = NA),
            plot.background = element_rect(fill = "white", color = NA))
  })

  output$out_r_script_anova_nested <- renderText({
    paste0(
      "# --- Dvoufaktorová hierarchická ANOVA (Nested) v R ---
",
      "# Model: PRIR ~ OTEC / MATKA  (Matka je vnořená v Otci)

",
      "fit_nested <- lm(PRIR ~ OTEC / MATKA, data = data_piglets)
",
      "anova_table<- anova(fit_nested)
",
      "print(anova_table)

",
      "# Post-hoc analýza matek v rámci otce:
",
      "library(emmeans)
",
      "emm_matka <- emmeans(fit_nested, ~ MATKA | OTEC)
",
      "pairs(emm_matka, adjust = 'tukey')
"
    )
  })

  
  # -----------------------------------------------------------------
  # LOGIKA POD-ZÁLOŽKY 3A: KORELACE
  # -----------------------------------------------------------------
  get_cor_data <- reactive({
    input$btn_cor_gen
    isolate({
      N <- input$cor_N
      r_target <- input$cor_r
      sx <- input$cor_sd_x
      sy <- input$cor_sd_y
      
      x_raw <- rnorm(N, mean = 100, sd = sx)
      z_raw <- rnorm(N, mean = 50, sd = sy)
      
      y_raw <- r_target * (x_raw - mean(x_raw)) / sd(x_raw) + 
               sqrt(1 - r_target^2) * (z_raw - mean(z_raw)) / sd(z_raw)
      y_vals <- 50 + y_raw * sy
      
      data.frame(x = x_raw, y = y_vals)
    })
  })
  
  output$cor_stats_html <- renderUI({
    df <- get_cor_data()
    cov_xy <- cov(df$x, df$y)
    r_xy <- cor(df$x, df$y)
    ct <- cor.test(df$x, df$y)
    p_val <- ct$p.value
    
    status_txt <- if (p_val < 0.05) {
      "<span style='color: green; font-weight: bold;'>Statisticky významná korelace (p < 0.05)</span>"
    } else {
      "<span style='color: orange; font-weight: bold;'>Korelace není statisticky významná (p ≥ 0.05)</span>"
    }
    
    HTML(paste0(
      "<b>Kovariance Cov(X,Y):</b> ", round(cov_xy, 3), "<br/>",
      "<b>Pearsonův korelační koeficient (r):</b> <span style='color: #0072B2; font-weight: bold; font-size: 1.1em;'>", round(r_xy, 4), "</span><br/>",
      "<b>Test významnosti t-statistika:</b> ", round(ct$statistic, 3), " (df = ", ct$parameter, ")<br/>",
      "<b>p-hodnota testu:</b> ", format.pval(p_val, digits = 4), "<br/>",
      "<b>Stav:</b> ", status_txt
    ))
  })
  
  output$plot_cor_scatter <- renderPlot({
    df <- get_cor_data()
    r_xy <- cor(df$x, df$y)
    
    # 95% elipsa spočtená v základním R (bez závislosti na balíčku MASS,
    # který stat_ellipse() používá a který nemusí být v nasazeném prostředí dostupný)
    ctr <- colMeans(df[, c("x", "y")])
    S   <- cov(df[, c("x", "y")])
    eg  <- eigen(S, symmetric = TRUE)
    tt  <- seq(0, 2 * pi, length.out = 200)
    circ <- rbind(cos(tt), sin(tt)) * sqrt(qchisq(0.95, df = 2))
    ell <- t(ctr + eg$vectors %*% (sqrt(pmax(eg$values, 0)) * circ))
    ell <- data.frame(x = ell[, 1], y = ell[, 2])
    
    ggplot(df, aes(x = x, y = y)) +
      geom_polygon(data = ell, aes(x = x, y = y), inherit.aes = FALSE,
                   alpha = 0.1, fill = "#0072B2", color = "#0072B2") +
      geom_point(color = "#0072B2", size = 3, alpha = 0.8) +
      labs(title = paste0("Korelační diagram s konfidenční elipsou (r = ", round(r_xy, 3), ")"),
           subtitle = "Dvourozměrné rozdělení znaků X a Y bez předpokladu kauzality",
           x = "Znak X", y = "Znak Y") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold"),
            panel.background = element_rect(fill = "white", color = NA),
            plot.background = element_rect(fill = "white", color = NA))
  })  
  output$out_cov_matrix <- renderPrint({
    df <- get_cor_data()
    cov(df)
  })
  
  output$out_cor_matrix <- renderPrint({
    df <- get_cor_data()
    cor(df)
  })

  output$out_r_script_cor <- renderText({
    df <- get_cor_data()
    r_val <- input$cor_r
    sx <- input$cor_sd_x
    sy <- input$cor_sd_y
    N <- input$cor_N
    
    paste0(
      "# --- Korelační analýza v R ---\n",
      "# Požadované parametry: r = ", r_val, ", s_x = ", sx, ", s_y = ", sy, ", N = ", N, "\n\n",
      "# 1. Výpočet kovariance a korelace pro vektory x a y:\n",
      "cov_val <- cov(x, y)       # Výběrová kovariance Cov(x,y)\n",
      "cor_val <- cor(x, y)       # Pearsonův korelační koeficient r\n",
      "print(paste('Kovariance Cov(X,Y):', round(cov_val, 3)))\n",
      "print(paste('Korelace r:', round(cor_val, 4)))\n\n",
      "# 2. Test statistické významnosti korelace (H0: r = 0):\n",
      "test_res <- cor.test(x, y)\n",
      "print(test_res)\n\n",
      "# 3. Variančně-kovariační a Korelační matice pro data.frame:\n",
      "data <- data.frame(Znak_X = x, Znak_Y = y)\n",
      "cov_mat <- cov(data)       # Variančně-kovariační matice\n",
      "cor_mat <- cor(data)       # Korelační matice (na diagonále 1.0)\n",
      "print(cov_mat)\n",
      "print(cor_mat)\n"
    )
  })
  
  # -----------------------------------------------------------------
  # LOGIKA POD-ZÁLOŽKY 3B: LINEÁRNÍ REGRESE
  # -----------------------------------------------------------------
  get_reg_data <- reactive({
    input$btn_reg_gen
    isolate({
      N <- input$reg_N
      a_target <- input$reg_intercept
      b_target <- input$reg_slope
      noise_sd <- input$reg_noise
      
      x_vals <- rnorm(N, mean = 50, sd = 15)
      e_vals <- rnorm(N, mean = 0, sd = noise_sd)
      y_vals <- a_target + b_target * x_vals + e_vals
      
      data.frame(x = x_vals, y = y_vals)
    })
  })
  
  output$reg_stats_html <- renderUI({
    df <- get_reg_data()
    fit <- lm(y ~ x, data = df)
    a <- coef(fit)[1]
    b <- coef(fit)[2]
    s_fit <- summary(fit)
    r2 <- s_fit$r.squared
    p_b <- s_fit$coefficients[2, 4]
    
    HTML(paste0(
      "<b>Odhadnutá regresní rovnice:</b><br/>",
      "<span style='color: #D55E00; font-weight: bold; font-size: 1.1em;'>ŷ = ", round(a, 2), " + ", round(b, 3), " · x</span><br/><br/>",
      "<b>Sklon (Regresní koeficient b<sub>y|x</sub>):</b> ", round(b, 4), " (p = ", format.pval(p_b, digits = 3), ")<br/>",
      "<b>Úsek na ose Y (a):</b> ", round(a, 3), "<br/>",
      "<b>Koeficient determinace (R²):</b> <span style='color: #0072B2; font-weight: bold;'>", round(r2 * 100, 2), " %</span>"
    ))
  })
  
  output$plot_reg_scatter <- renderPlot({
    df <- get_reg_data()
    fit <- lm(y ~ x, data = df)
    
    ggplot(df, aes(x = x, y = y)) +
      geom_point(color = "#0072B2", size = 3, alpha = 0.7) +
      geom_smooth(method = "lm", color = "#D55E00", linewidth = 1.3, se = TRUE, fill = "#D55E00", alpha = 0.15) +
      labs(title = "Lineární regrese Y = a + b·X s pásem spolehlivosti",
           x = "Vysvětlující proměnná (X)", y = "Vysvětlovaná proměnná (Y)") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold"),
            panel.background = element_rect(fill = "white", color = NA),
            plot.background = element_rect(fill = "white", color = NA))
  })
  
  output$plot_residuals <- renderPlot({
    df <- get_reg_data()
    fit <- lm(y ~ x, data = df)
    df$res <- residuals(fit)
    
    ggplot(df, aes(x = x, y = res)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 1) +
      geom_point(color = "#E69F00", size = 2.8) +
      labs(title = "Graf reziduí (e_i = y_i - ŷ_i)", x = "X", y = "Reziduum (e)") +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"),
            panel.background = element_rect(fill = "white", color = NA),
            plot.background = element_rect(fill = "white", color = NA))
  })

  output$out_r_script_reg <- renderText({
    b_val <- input$reg_slope
    a_val <- input$reg_intercept
    sd_err <- input$reg_noise
    N <- input$reg_N
    
    paste0(
      "# --- Lineární regrese y = a + b*x v R ---\n",
      "# Požadované parametry: Sklon b = ", b_val, ", Úsek a = ", a_val, ", Šum sd = ", sd_err, ", N = ", N, "\n\n",
      "# 1. Odhad lineárního regresního modelu:\n",
      "model <- lm(y ~ x, data = df)    # Nezávislá proměnná x, závislá y\n\n",
      "# 2. Detailní statistické výsledky (p-hodnoty, SE, R^2):\n",
      "summary(model)\n\n",
      "# 3. Extrakce regresních koeficientů (a, b):\n",
      "coefs <- coef(model)\n",
      "a_est <- coefs[1]               # Úsek na ose Y (Intercept)\n",
      "b_est <- coefs[2]               # Sklon regresní přímky (b_y|x)\n",
      "print(paste('Regresní rovnice: y =', round(a_est, 2), '+', round(b_est, 3), '* x'))\n\n",
      "# 4. Výpočet reziduí (e_i = y_i - y_hat_i) a koeficientu determinace R^2:\n",
      "resids <- residuals(model)      # Vektor reziduí\n",
      "r2_val <- summary(model)$r.squared  # Koeficient determinace\n",
      "print(paste('R-squared:', round(r2_val * 100, 2), '%'))\n\n",
      "# 5. Vykreslení regresní přímky v základním grafu:\n",
      "plot(df$x, df$y, pch = 19, col = 'darkblue', xlab = 'X', ylab = 'Y')\n",
      "abline(model, col = 'red', lwd = 2)\n"
    )
  })
  
  # -----------------------------------------------------------------
  # LOGIKA POD-ZÁLOŽKY 4A: JEDNA MATICE
  # -----------------------------------------------------------------
  get_matrix_A <- reactive({
    if (input$mat_size == "2") {
      matrix(c(input$m2_11, input$m2_12,
               input$m2_21, input$m2_22), nrow = 2, ncol = 2, byrow = TRUE)
    } else {
      matrix(c(input$m3_11, input$m3_12, input$m3_13,
               input$m3_21, input$m3_22, input$m3_23,
               input$m3_31, input$m3_32, input$m3_33), nrow = 3, ncol = 3, byrow = TRUE)
    }
  })
  
  output$out_mat_A <- renderPrint({ get_matrix_A() })
  output$out_mat_T <- renderPrint({ t(get_matrix_A()) })
  
  output$out_mat_det_html <- renderUI({
    A <- get_matrix_A()
    d <- det(A)
    tr <- sum(diag(A))
    status <- if (abs(d) < 1e-8) {
      "<span style='color: red; font-weight: bold;'>Singulární matice (|A| = 0) - nemá inverzi!</span>"
    } else {
      "<span style='color: green; font-weight: bold;'>Regulární matice (|A| ≠ 0) - inverze existuje.</span>"
    }
    
    HTML(paste0(
      "<b>Determinant |A|:</b> ", round(d, 4), "<br/>",
      "<b>Stopa matice tr(A):</b> ", round(tr, 4), "<br/>",
      "<b>Stav matice:</b> ", status
    ))
  })
  
  output$out_mat_inv <- renderPrint({
    A <- get_matrix_A()
    if (abs(det(A)) < 1e-8) {
      "Inverzní matice neexistuje (determinant je 0)."
    } else {
      solve(A)
    }
  })
  
  output$out_mat_check <- renderPrint({
    A <- get_matrix_A()
    if (abs(det(A)) < 1e-8) {
      "Není k dispozici."
    } else {
      round(A %*% solve(A), 10)
    }
  })
  
  output$out_r_script_single <- renderText({
    A <- get_matrix_A()
    nr <- nrow(A)
    nc <- ncol(A)
    vals_str <- paste(as.vector(t(A)), collapse = ", ")
    
    paste0(
      "# --- Vytvoření čtvercové matice A v R ---\n",
      "A <- matrix(c(", vals_str, "), nrow = ", nr, ", ncol = ", nc, ", byrow = TRUE)\n\n",
      "# --- Operace s maticí A ---\n",
      "A_trans <- t(A)           # Transpozice matice\n",
      "det_A   <- det(A)         # Determinant |A|\n",
      "tr_A    <- sum(diag(A))   # Stopa matice (součet prvků na diagonále)\n",
      "A_inv   <- solve(A)       # Inverzní matice A^-1\n\n",
      "# Ověření: A %*% A_inv (Jednotková matice I)\n",
      "round(A %*% solve(A), 10)"
    )
  })

  # -----------------------------------------------------------------
  # LOGIKA POD-ZÁLOŽKY 4B: OPERACE SE DVĚMA MATICEMI & KONFORMITA
  # -----------------------------------------------------------------
  two_mats <- reactiveValues(A = NULL, B = NULL)
  
  observe({
    mA_r <- as.numeric(input$dim_A_r)
    mA_c <- as.numeric(input$dim_A_c)
    mB_r <- as.numeric(input$dim_B_r)
    mB_c <- as.numeric(input$dim_B_c)
    
    input$btn_random_mats
    
    isolate({
      two_mats$A <- matrix(sample(-5:9, mA_r * mA_c, replace = TRUE), nrow = mA_r, ncol = mA_c)
      two_mats$B <- matrix(sample(-5:9, mB_r * mB_c, replace = TRUE), nrow = mB_r, ncol = mB_c)
    })
  })
  
  output$out_two_A <- renderPrint({
    A <- two_mats$A
    cat("Matice A (", nrow(A), "x", ncol(A), "):
", sep = "")
    print(A)
  })
  
  output$out_two_B <- renderPrint({
    B <- two_mats$B
    cat("Matice B (", nrow(B), "x", ncol(B), "):
", sep = "")
    print(B)
  })
  
  output$ui_add_sub_status <- renderUI({
    A <- two_mats$A
    B <- two_mats$B
    if (nrow(A) == nrow(B) && ncol(A) == ncol(B)) {
      div(class = "status-badge status-ok",
          paste0("✔ KONFORMITA PRO SČÍTÁNÍ A ODEČÍTÁNÍ: ANO (obě matice mají shodný rozměr ", nrow(A), "x", ncol(A), ")"))
    } else {
      div(class = "status-badge status-err",
          paste0("❌ KONFORMITA PRO SČÍTÁNÍ A ODEČÍTÁNÍ: NE (matice A je ", nrow(A), "x", ncol(A), ", ale matice B je ", nrow(B), "x", ncol(B), "). Sčítání ani odečítání není definováno!"))
    }
  })
  
  output$out_mat_add <- renderPrint({
    A <- two_mats$A
    B <- two_mats$B
    if (nrow(A) == nrow(B) && ncol(A) == ncol(B)) {
      A + B
    } else {
      cat("Není konformní (nelze sečíst).")
    }
  })
  
  output$out_mat_sub <- renderPrint({
    A <- two_mats$A
    B <- two_mats$B
    if (nrow(A) == nrow(B) && ncol(A) == ncol(B)) {
      A - B
    } else {
      cat("Není konformní (nelze odečíst).")
    }
  })
  
  output$ui_mult_status <- renderUI({
    A <- two_mats$A
    B <- two_mats$B
    if (ncol(A) == nrow(B)) {
      div(class = "status-badge status-ok",
          paste0("✔ KONFORMITA PRO NÁSOBENÍ A %*% B: ANO (počet sloupců A = ", ncol(A), " odpovídá počtu řádků B = ", nrow(B), "). Výsledná matice C má rozměr ", nrow(A), "x", ncol(B), "."))
    } else {
      div(class = "status-badge status-err",
          paste0("❌ KONFORMITA PRO NÁSOBENÍ A %*% B: NE (počet sloupců A = ", ncol(A), " se NEROVNÁ počtu řádků B = ", nrow(B), "). Násobení A %*% B nelze provést!"))
    }
  })
  
  output$out_mat_mult <- renderPrint({
    A <- two_mats$A
    B <- two_mats$B
    if (ncol(A) == nrow(B)) {
      A %*% B
    } else {
      cat("Chyba v R: non-conformable arguments (A %*% B nelze spočítat).")
    }
  })
  
  output$out_mat_mult_ba <- renderPrint({
    A <- two_mats$A
    B <- two_mats$B
    if (ncol(B) == nrow(A)) {
      res_ba <- B %*% A
      cat("Výsledek B %*% A (rozměr ", nrow(B), "x", ncol(A), "):
", sep = "")
      print(res_ba)
      if (ncol(A) == nrow(B) && nrow(A) == ncol(B)) {
        if (identical(A %*% B, B %*% A)) {
          cat("\nV tomto speciálním případě A %*% B == B %*% A.")
        } else {
          cat("\nPozor: A %*% B != B %*% A (Důkaz nekomutativity!).")
        }
      }
    } else {
      cat("B %*% A NENÍ KONFORMNÍ (počet sloupců B = ", ncol(B), " != počet řádků A = ", nrow(A), ").", sep = "")
    }
  })
  
  output$out_r_script_two <- renderText({
    A <- two_mats$A
    B <- two_mats$B
    
    vals_A <- paste(as.vector(t(A)), collapse = ", ")
    vals_B <- paste(as.vector(t(B)), collapse = ", ")
    
    add_code <- if (nrow(A) == nrow(B) && ncol(A) == ncol(B)) {
      "A + B  # Součet matic\nA - B  # Rozdíl matic\n"
    } else {
      "# A + B  # CHYBA: non-conformable arrays (stejné rozměry jsou vyžadovány)\n"
    }
    
    mult_code <- if (ncol(A) == nrow(B)) {
      "C <- A %*% B  # Násobení matic (konformní)\nprint(C)\n"
    } else {
      "# C <- A %*% B  # CHYBA: non-conformable arguments (sloupce A != řádky B)\n"
    }
    
    paste0(
      "# --- Definice matic v R ---\n",
      "A <- matrix(c(", vals_A, "), nrow = ", nrow(A), ", ncol = ", ncol(A), ", byrow = TRUE)\n",
      "B <- matrix(c(", vals_B, "), nrow = ", nrow(B), ", ncol = ", ncol(B), ", byrow = TRUE)\n\n",
      "# --- 1. Sčítání a Odečítání (Vyžaduje m_A == m_B a n_A == n_B) ---\n",
      add_code, "\n",
      "# --- 2. Násobení matic (Vyžaduje počet sloupců A == počet řádků B) ---\n",
      mult_code
    )
  })
  
  # -----------------------------------------------------------------
  # LOGIKA POD-ZÁLOŽKY 5A: PODROBNÝ POSTUP INVERZE MATICE A
  # -----------------------------------------------------------------
  get_step_inv_matrix_A <- reactive({
    if (input$inv_step_size == "2") {
      matrix(c(input$inv2_11, input$inv2_12,
               input$inv2_21, input$inv2_22), nrow = 2, ncol = 2, byrow = TRUE)
    } else {
      matrix(c(input$inv3_11, input$inv3_12, input$inv3_13,
               input$inv3_21, input$inv3_22, input$inv3_23,
               input$inv3_31, input$inv3_32, input$inv3_33), nrow = 3, ncol = 3, byrow = TRUE)
    }
  })

  output$out_step_det_html <- renderUI({
    A <- get_step_inv_matrix_A()
    d <- det(A)
    n <- nrow(A)
    
    if (n == 2) {
      calc_str <- paste0("|A| = (", A[1,1], " · ", A[2,2], ") − (", A[1,2], " · ", A[2,1], ") = ", 
                         (A[1,1]*A[2,2]), " − ", (A[1,2]*A[2,1]), " = <b>", round(d, 4), "</b>")
    } else {
      p1 <- A[1,1]*A[2,2]*A[3,3]
      p2 <- A[1,2]*A[2,3]*A[3,1]
      p3 <- A[1,3]*A[2,1]*A[3,2]
      m1 <- A[1,3]*A[2,2]*A[3,1]
      m2 <- A[1,1]*A[2,3]*A[3,2]
      m3 <- A[1,2]*A[2,1]*A[3,3]
      
      calc_str <- paste0(
        "<b>Sarrusovo pravidlo pro matici 3x3:</b><br/>",
        "|A| = (a₁₁a₂₂a₃₃ + a₁₂a₂₃a₃₁ + a₁₃a₂₁a₃₂) − (a₁₃a₂₂a₃₁ + a₁₁a₂₃a₃₂ + a₁₂a₂₁a₃₃)<br/>",
        "|A| = (", p1, " + ", p2, " + ", p3, ") − (", m1, " + ", m2, " + ", m3, ") = ",
        (p1+p2+p3), " − (", (m1+m2+m3), ") = <b>", round(d, 4), "</b>"
      )
    }
    
    status <- if (abs(d) < 1e-8) {
      "<br/><span style='color: red; font-weight: bold;'>Singulární matice (|A| = 0) − nemá inverzi!</span>"
    } else {
      "<br/><span style='color: green; font-weight: bold;'>Regulární matice (|A| ≠ 0) − inverzní matice existuje.</span>"
    }
    
    HTML(paste0(calc_str, status))
  })

  get_step_inv_data <- reactive({
    A <- get_step_inv_matrix_A()
    n <- nrow(A)
    d <- det(A)
    
    C <- matrix(0, nrow = n, ncol = n)
    if (n == 2) {
      C[1,1] <- A[2,2]
      C[1,2] <- -A[2,1]
      C[2,1] <- -A[1,2]
      C[2,2] <- A[1,1]
    } else {
      for (i in 1:3) {
        for (j in 1:3) {
          sub_mat <- A[-i, -j]
          sub_det <- sub_mat[1,1]*sub_mat[2,2] - sub_mat[1,2]*sub_mat[2,1]
          C[i,j] <- ((-1)^(i+j)) * sub_det
        }
      }
    }
    adj <- t(C)
    
    inv <- if (abs(d) < 1e-8) NULL else solve(A)
    
    list(A = A, det = d, C = C, adj = adj, inv = inv)
  })

  output$out_step_cofactors_html <- renderUI({
    dat <- get_step_inv_data()
    A <- dat$A
    n <- nrow(A)
    
    if (n == 2) {
      txt <- paste0(
        "C₁₁ = +a₂₂ = <b>", dat$C[1,1], "</b> | ",
        "C₁₂ = −a₂₁ = <b>", dat$C[1,2], "</b><br/>",
        "C₂₁ = −a₁₂ = <b>", dat$C[2,1], "</b> | ",
        "C₂₂ = +a₁₁ = <b>", dat$C[2,2], "</b>"
      )
    } else {
      txt <- paste0(
        "<b>Prvky kofaktorové matice C<sub>ij</sub> = (−1)<sup>i+j</sup> · det(M<sub>ij</sub>):</b><br/>",
        "C₁₁ = +(", A[2,2], "·", A[3,3], " − ", A[2,3], "·", A[3,2], ") = <b>", dat$C[1,1], "</b> | ",
        "C₁₂ = −(", A[2,1], "·", A[3,3], " − ", A[2,3], "·", A[3,1], ") = <b>", dat$C[1,2], "</b> | ",
        "C₁₃ = +(", A[2,1], "·", A[3,2], " − ", A[2,2], "·", A[3,1], ") = <b>", dat$C[1,3], "</b><br/>",
        "C₂₁ = −(", A[1,2], "·", A[3,3], " − ", A[1,3], "·", A[3,2], ") = <b>", dat$C[2,1], "</b> | ",
        "C₂₂ = +(", A[1,1], "·", A[3,3], " − ", A[1,3], "·", A[3,1], ") = <b>", dat$C[2,2], "</b> | ",
        "C₂₃ = −(", A[1,1], "·", A[3,2], " − ", A[1,2], "·", A[3,1], ") = <b>", dat$C[2,3], "</b><br/>",
        "C₃₁ = +(", A[1,2], "·", A[2,3], " − ", A[1,3], "·", A[2,2], ") = <b>", dat$C[3,1], "</b> | ",
        "C₃₂ = −(", A[1,1], "·", A[2,3], " − ", A[1,3], "·", A[2,1], ") = <b>", dat$C[3,2], "</b> | ",
        "C₃₃ = +(", A[1,1], "·", A[2,2], " − ", A[1,2], "·", A[2,1], ") = <b>", dat$C[3,3], "</b>"
      )
    }
    HTML(txt)
  })

  output$out_mat_C_step <- renderPrint({ get_step_inv_data()$C })
  output$out_mat_adj_step <- renderPrint({ get_step_inv_data()$adj })

  output$out_step_inv_explanation_html <- renderUI({
    dat <- get_step_inv_data()
    if (abs(dat$det) < 1e-8) {
      HTML("<span style='color: red; font-weight: bold;'>Inverzní matice neexistuje.</span>")
    } else {
      HTML(paste0(
        "A⁻¹ = (1 / <b>", round(dat$det, 2), "</b>) · adj(A)<br/>",
        "Každý prvek adjungované matice adj(A) vydělíme determinantem |A| = <b>", round(dat$det, 2), "</b>."
      ))
    }
  })

  output$out_mat_inv_step <- renderPrint({
    dat <- get_step_inv_data()
    if (is.null(dat$inv)) "Matice nemá inverzi." else round(dat$inv, 4)
  })

  output$out_mat_check_step <- renderPrint({
    dat <- get_step_inv_data()
    if (is.null(dat$inv)) "Není k dispozici." else round(dat$A %*% dat$inv, 10)
  })

  output$out_r_script_inv_step <- renderText({
    dat <- get_step_inv_data()
    A <- dat$A
    vals_A <- paste(as.vector(t(A)), collapse = ", ")
    
    paste0(
      "# --- Zadání matice A v R ---\n",
      "A <- matrix(c(", vals_A, "), nrow = ", nrow(A), ", ncol = ", ncol(A), ", byrow = TRUE)\n\n",
      "# --- Krok 1: Výpočet determinantu |A| ---\n",
      "det_A <- det(A)\n",
      "cat('Determinant |A| =', det_A, '\\n')\n\n",
      "# --- Krok 2: Matice algebraických doplňků (Cofactor matrix) ---\n",
      "n <- nrow(A)\n",
      "C <- matrix(0, nrow = n, ncol = n)\n",
      "for (i in 1:n) {\n",
      "  for (j in 1:n) {\n",
      "    C[i, j] <- ((-1)^(i + j)) * det(A[-i, -j, drop = FALSE])\n",
      "  }\n",
      "}\n",
      "cat('Matice kofaktorů C:\\n'); print(C)\n\n",
      "# --- Krok 3: Adjungovaná matice adj(A) = t(C) ---\n",
      "adj_A <- t(C)\n",
      "cat('Adjungovaná matice adj(A):\\n'); print(adj_A)\n\n",
      "# --- Krok 4: Výpočet inverzní matice A^-1 ---\n",
      "A_inv_step <- (1 / det_A) * adj_A\n",
      "cat('Vypočtená inverze A^-1:\\n'); print(round(A_inv_step, 4))\n\n",
      "# --- Přímý výpočet v R přes solve(A) ---\n",
      "A_inv_direct <- solve(A)\n",
      "cat('Přímý výpočet v R přes solve(A):\\n'); print(round(A_inv_direct, 4))\n"
    )
  })

  # -----------------------------------------------------------------
  # LOGIKA POD-ZÁLOŽKY 5B: PŘÍKLADY SOUSTAVY ROVNIC
  # -----------------------------------------------------------------
  output$ui_eq_inputs <- renderUI({
    if (input$eq_preset == "p1") {
      tagList(
        h4("Soustava 2x2 (Zelinářství):"),
        p("9x + 4y = 422"),
        p("5x + 6y = 344")
      )
    } else {
      tagList(
        h4("Soustava 3x3:"),
        p("5x₁ + 3x₂ - 4x₃ = 21"),
        p("3x₁ - 8x₂ + 5x₃ = 10"),
        p("8x₁ + 10x₂ - 3x₃ = -5")
      )
    }
  })
  
  get_eq_data <- reactive({
    if (input$eq_preset == "p1") {
      A <- matrix(c(9, 4, 5, 6), nrow = 2, ncol = 2, byrow = TRUE)
      y <- c(422, 344)
      names_b <- c("x (Jablka)", "y (Hrušky)")
    } else {
      A <- matrix(c(5, 3, -4, 3, -8, 5, 8, 10, -3), nrow = 3, ncol = 3, byrow = TRUE)
      y <- c(21, 10, -5)
      names_b <- c("x₁", "x₂", "x₃")
    }
    list(A = A, y = y, names = names_b)
  })
  
  output$out_eq_A <- renderPrint({ get_eq_data()$A })
  output$out_eq_y <- renderPrint({ get_eq_data()$y })
  
  output$out_eq_solution_html <- renderUI({
    dat <- get_eq_data()
    A <- dat$A
    y <- dat$y
    d <- det(A)
    
    if (abs(d) < 1e-8) {
      HTML("<span style='color: red; font-weight: bold;'>Soustavu nelze jednoznačně vyřešit (|A| = 0).</span>")
    } else {
      b <- solve(A) %*% y
      res_txt <- paste(sapply(1:length(b), function(i) {
        paste0("<b>", dat$names[i], ":</b> <span style='color: #0072B2; font-weight: bold;'>", round(b[i], 4), "</span>")
      }), collapse = " | ")
      
      HTML(paste0(
        "<b>Determinant matice soustavy |A|:</b> ", round(d, 2), "<br/>",
        "<b>Vypočtené řešení b = A⁻¹ · y:</b><br/>", res_txt
      ))
    }
  })
  
  output$out_eq_b_vec <- renderPrint({
    dat <- get_eq_data()
    if (abs(det(dat$A)) < 1e-8) return("Chyba")
    b <- solve(dat$A, dat$y)
    names(b) <- dat$names
    b
  })
  
  output$plot_eq_lines <- renderPlot({
    dat <- get_eq_data()
    if (input$eq_preset == "p1") {
      x_vals <- seq(10, 50, length.out = 100)
      y1 <- (422 - 9 * x_vals) / 4
      y2 <- (344 - 5 * x_vals) / 6
      
      df_lines <- data.frame(
        x = rep(x_vals, 2),
        y = c(y1, y2),
        Rovnice = rep(c("9x + 4y = 422", "5x + 6y = 344"), each = 100)
      )
      
      b_sol <- solve(dat$A, dat$y)
      
      ggplot(df_lines, aes(x = x, y = y, color = Rovnice)) +
        geom_line(linewidth = 1.3) +
        annotate("point", x = b_sol[1], y = b_sol[2], color = "red", size = 5) +
        annotate("text", x = b_sol[1], y = b_sol[2] + 3, label = paste0("Průsečík [", round(b_sol[1]), ", ", round(b_sol[2]), "]"), 
                 color = "black", fontface = "bold", size = 4.5) +
        labs(title = "Geometrické řešení soustavy 2 rovnic (Průsečík přímek)",
             x = "Cena x (Jablka)", y = "Cena y (Hrušky)") +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(face = "bold"),
              panel.background = element_rect(fill = "white", color = NA),
              plot.background = element_rect(fill = "white", color = NA))
    } else {
      b_sol <- solve(dat$A, dat$y)
      df_b <- data.frame(Neznama = dat$names, Hodnota = as.numeric(b_sol))
      
      ggplot(df_b, aes(x = Neznama, y = Hodnota, fill = Neznama)) +
        geom_bar(stat = "identity", width = 0.5) +
        geom_text(aes(label = round(Hodnota, 4)), vjust = ifelse(df_b$Hodnota >= 0, -0.4, 1.2), fontface = "bold", size = 4.5) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 1) +
        scale_fill_manual(values = c("x₁" = "#E69F00", "x₂" = "#56B4E9", "x₃" = "#009E73")) +
        labs(title = "Hodnoty vypočtených neznámých (x₁, x₂, x₃) soustavy 3x3",
             x = "Neznámé proměnné", y = "Hodnota") +
        theme_minimal(base_size = 14) +
        theme(legend.position = "none",
              plot.title = element_text(face = "bold"),
              panel.background = element_rect(fill = "white", color = NA),
              plot.background = element_rect(fill = "white", color = NA))
    }
  })
  output$out_r_script_eq <- renderText({
    dat <- get_eq_data()
    A <- dat$A
    y <- dat$y
    vals_A <- paste(as.vector(t(A)), collapse = ", ")
    vals_y <- paste(y, collapse = ", ")
    
    paste0(
      "# --- Zadání soustavy lineárních rovnic A · b = y v R ---
",
      "A <- matrix(c(", vals_A, "), nrow = ", nrow(A), ", ncol = ", ncol(A), ", byrow = TRUE)
",
      "y <- c(", vals_y, ")

",
      "# --- Řešení soustavy b = A^-1 · y ---
",
      "b <- solve(A, y)  # R funkce pro řešení A * b = y
",
      "print(b)

",
      "# Ověření zpětným vynásobením:
",
      "A %*% b  # Musí se rovnat vektoru y"
    )
  })

    # -----------------------------------------------------------------
  # LOGIKA ZÁLOŽKY 6: MATICOVÉ ŘEŠENÍ 1-FAKTOROVÉ ANOVA
  # -----------------------------------------------------------------
  get_anova_matrix_data <- reactive({
    preset <- input$anova_mat_preset
    model_type <- input$anova_mat_model_type
    
    if (preset == "small_3") {
      df <- data.frame(
        otec = factor(rep(c("Otec_1", "Otec_2", "Otec_3"), each = 3)),
        snaska = c(18, 20, 22,   12, 14, 16,   24, 26, 28)
      )
    } else {
      # Dataset ze cvičení (48 slepic po 4 otcích)
      df <- data.frame(
        otec = factor(rep(c("k1", "k2", "k3", "k4"), times = c(6, 6, 12, 24))),
        snaska = c(
          172, 253, 239, 203, 247, 256,
          137, 197, 200, 198, 131, 145,
          209, 187, 183, 215, 215, 134, 185, 218, 155, 214, 184, 176,
          172, 166, 121, 115, 134, 134, 127, 115, 138, 116, 124, 197,
          118, 129, 113, 138, 133, 116, 112, 114, 110, 124, 118, 138
        )
      )
    }
    
    y <- df$snaska
    if (model_type == "cell_means") {
      X <- model.matrix(~ 0 + otec, data = df)
      colnames(X) <- levels(df$otec)
    } else {
      X <- model.matrix(~ otec, data = df)
    }
    
    XtX <- t(X) %*% X
    Xty <- t(X) %*% y
    b <- solve(XtX, Xty)
    
    PX <- X %*% solve(XtX) %*% t(X)
    yhat <- PX %*% y
    e <- y - yhat
    
    n <- length(y)
    J <- matrix(1, nrow = n, ncol = n)
    SS_total <- as.numeric(t(y) %*% y - (1/n) * t(y) %*% J %*% y)
    SS_between <- as.numeric(t(y) %*% PX %*% y - (1/n) * t(y) %*% J %*% y)
    SS_within <- as.numeric(t(e) %*% e)
    
    list(
      df = df, y = y, X = X, XtX = XtX, Xty = Xty, b = b,
      PX = PX, yhat = yhat, e = e,
      SS_total = SS_total, SS_between = SS_between, SS_within = SS_within
    )
  })

  output$out_anova_mat_y <- renderPrint({
    dat <- get_anova_matrix_data()
    head(dat$y, 12)
  })

  output$out_anova_mat_X <- renderPrint({
    dat <- get_anova_matrix_data()
    head(dat$X, 12)
  })

  output$out_anova_mat_XtX <- renderPrint({
    dat <- get_anova_matrix_data()
    dat$XtX
  })

  output$out_anova_mat_Xty <- renderPrint({
    dat <- get_anova_matrix_data()
    dat$Xty
  })

  output$out_anova_mat_b_vec <- renderPrint({
    dat <- get_anova_matrix_data()
    dat$b
  })

  output$out_anova_mat_b_summary <- renderUI({
    dat <- get_anova_matrix_data()
    m_type <- input$anova_mat_model_type
    b_vals <- dat$b
    names_b <- rownames(b_vals)
    
    lines_txt <- sapply(1:length(b_vals), function(i) {
      paste0("<b>", names_b[i], ":</b> <span style='color: #0072B2; font-weight: bold; font-size: 1.05em;'>", round(b_vals[i], 3), "</span>")
    })
    txt_num <- paste(lines_txt, collapse = "<br/>")
    
    if (m_type == "cell_means") {
      interp_txt <- "<div style='margin-top: 10px; padding: 8px; background: #eef6fc; border-left: 3px solid #0072B2; font-size: 0.9em;'><b>💡 Interpretace (Model skupinových průměrů):</b><br/>Koeficienty <i>b</i> přímo udávají <b>přímé výběrové průměry</b> jednotlivých otců/skupin (ȳ₁, ȳ₂, ...).</div>"
    } else {
      interp_txt <- "<div style='margin-top: 10px; padding: 8px; background: #fdf6e3; border-left: 3px solid #D55E00; font-size: 0.9em;'><b>💡 Interpretace (Referenční model):</b><br/><code>(Intercept)</code> = průměr 1. referenční skupiny (ȳ₁).<br/>Ostatní koeficienty = <b>odchylky (kontrasty)</b> od referenční skupiny (ȳᵢ − ȳ₁).</div>"
    }
    
    HTML(paste0(txt_num, "<br/>", interp_txt))
  })

  output$out_r_script_anova_matrix <- renderText({
    dat <- get_anova_matrix_data()
    m_type <- input$anova_mat_model_type
    formula_str <- if (m_type == "cell_means") "~ 0 + otec" else "~ otec"
    
    comment_type <- if (m_type == "cell_means") {
      "# Model skupinových průměrů (~ 0 + otec):\n# X^T * X je DIAGONÁLNÍ matice s počty prvků ve skupinách (n_1, n_2, ...)\n# b obsahuje přímé průměry skupin (y_1_bar, y_2_bar, ...)\n"
    } else {
      "# Referenční model (~ otec):\n# První sloupec X tvoří jednotkový Intercept.\n# b[1] = průměr referenční skupiny, b[i] = odchylka (kontrast) i-té skupiny od referenční.\n"
    }
    
    paste0(
      "# ===================================================================\n",
      "# MATICOVÉ ŘEŠENÍ 1-FAKTOROVÉ ANOVA SOUSTAVOU NORMÁLNÍCH ROVNIC\n",
      "# ===================================================================\n\n",
      comment_type, "\n",
      "# 1. Vektor pozorování y a matice plánu X:\n",
      "y <- data$snaska\n",
      "X <- model.matrix(", formula_str, ", data = data)\n\n",
      "# 2. Matice soustavy normálních rovnic X^T * X:\n",
      "XtX <- t(X) %*% X\n",
      "print(XtX)\n\n",
      "# 3. Vektor pravých stran X^T * y:\n",
      "Xty <- t(X) %*% y\n",
      "print(Xty)\n\n",
      "# 4. Odhad parametrů b = (X^T * X)^-1 * X^T * y:\n",
      "b <- solve(XtX, Xty)\n",
      "print(b)\n"
    )
  })

  output$out_anova_mat_PX <- renderPrint({
    dat <- get_anova_matrix_data()
    n_show <- min(9, nrow(dat$PX))
    round(dat$PX[1:n_show, 1:n_show], 3)
  })

  output$out_anova_mat_yhat <- renderPrint({
    dat <- get_anova_matrix_data()
    head(round(dat$yhat, 3), 12)
  })

  output$out_anova_mat_e <- renderPrint({
    dat <- get_anova_matrix_data()
    head(round(dat$e, 3), 12)
  })

  output$out_anova_mat_ss_html <- renderUI({
    dat <- get_anova_matrix_data()
    HTML(paste0(
      "<b>SS celkový (yᵀy − C):</b> <span style='color: #222; font-weight: bold;'>", round(dat$SS_total, 2), "</span><br/>",
      "<b>SS mezi skupinami (yᵀP_X y − C):</b> <span style='color: #0072B2; font-weight: bold;'>", round(dat$SS_between, 2), "</span><br/>",
      "<b>SS reziduální (eᵀe = yᵀ(I − P_X)y):</b> <span style='color: #D55E00; font-weight: bold;'>", round(dat$SS_within, 2), "</span>"
    ))
  })

  output$out_r_script_anova_proj <- renderText({
    paste0(
      "# ===================================================================
",
      "# MATICOVÝ ROZKLAD VARIABILITY ANOVA POMOCÍ PROJEKČNÍ MATICE P_X
",
      "# ===================================================================

",
      "# 1. Projekční matice P_X (Hat matrix):
",
      "PX <- X %*% solve(t(X) %*% X) %*% t(X)

",
      "# 2. Vyrovnané hodnoty y_hat a rezidua e:
",
      "yhat <- PX %*% y
",
      "e    <- y - yhat

",
      "# 3. Maticové součty čtverců (SS):
",
      "n <- length(y)
",
      "J <- matrix(1, nrow = n, ncol = n)
",
      "C_factor <- (1/n) * as.numeric(t(y) %*% J %*% y)

",
      "SS_total   <- as.numeric(t(y) %*% y) - C_factor
",
      "SS_between <- as.numeric(t(y) %*% PX %*% y) - C_factor
",
      "SS_within  <- as.numeric(t(e) %*% e)

",
      "cat('SS_total:', round(SS_total, 2), '\n')
",
      "cat('SS_between:', round(SS_between, 2), '\n')
",
      "cat('SS_within:', round(SS_within, 2), '\n')
"
    )
  })
}

# SPUŠTĚNÍ APLIKACE
# -------------------------------------------------------------------
shinyApp(ui = ui, server = server)

# =============================================================================
# T3 — Regresión económica: CASEN y ENE
# Fundamentos de Programación para Análisis Económico · UdeC-EAN
#
# Autor: Sebastián Navarrete Poblete
# Fecha: 13-10-2026
#
# Qué hace este script:
#   PARTE A: ecuación de ingresos con la CASEN reducida (flujo broom + dplyr).
#   PARTE B: brecha de desocupación entre hombres y mujeres con la ENE
#            (mayo-julio 2026), a partir de mi base limpia de la A3.
#   PARTE C: tabla con modelsummary() y exportación de resultados a CSV.
#
# Cómo correrlo: abrir el .Rproj y correr el script completo (no usa setwd()).
# =============================================================================

library(dplyr)
library(broom)
library(modelsummary)

## Parte A — CASEN (45 puntos)

### A1. Dos modelos

casen <- read.csv("data/raw/casen_reducido.csv") |>
  mutate(experiencia = pmax(edad - educ - 6, 0))

m1 <- lm(ingreso ~ educ, data = casen)
m2 <- lm(ingreso ~ educ + experiencia + genero, data = casen)

### A2. La tabla de coeficientes, con `broom` + `dplyr`

tidy(m2, conf.int = TRUE) |>
  filter(term != "(Intercept)") |>                        # sin intercepto
  mutate(across(c(estimate, conf.low, conf.high), round),
         significativo = p.value < 0.05) |>               # TRUE si p < 5 %
  select(term, estimate, conf.low, conf.high, p.value, significativo) |>
  arrange(p.value)                                        # de más a menos claro
# Resultado: educ 42246 (int.conf 29487 a 55004, significativo).
#            generoM 55686 y experiencia -517: NO significativos.

### A3. Comparar modelos

# Mi tercer modelo: Mincer, con experiencia al cuadrado (perfil cóncavo).
m3 <- lm(ingreso ~ educ + experiencia + I(experiencia^2) + genero, data = casen)

# Guarda los tres en una lista con nombre y apila sus `glance()`:

modelos <- list(simple = m1, controles = m2, mio = m3)
lapply(modelos, glance) |>
  bind_rows(.id = "modelo") |>
  select(modelo, r.squared, adj.r.squared, AIC, nobs)
# Comenta en 2–3 líneas: ¿qué pasa con el R² y con el R² **ajustado**? ¿Tu modelo
# mejora algo?

#  el R² sube de un modelo al siguiente (0.458 -> 0.475 -> 0.481)
# porque el R² siempre sube al agregar variables. El R² AJUSTADO, que castiga
# agregar variables, BAJA (0.448 -> 0.444 -> 0.440). Mi modelo no mejora: la
# experiencia (y su cuadrado) no aportan información útil en esta muestra.


### A4. ¿Cambió el coeficiente de educación? 

bind_rows(simple    = tidy(m1),
          controles = tidy(m2),
          mio       = tidy(m3), .id = "modelo") |>
  filter(term == "educ") |>
  select(modelo, estimate, std.error, p.value)

cor(casen$educ, casen$experiencia)

# el coeficiente casi no cambia (42080 -> 42246 -> 42690). Un
# control mueve un coeficiente solo si se relaciona con la educación
# y con el ingreso. La experiencia casi no se relaciona con la educación
# (correlación -0.156) ni con el ingreso, así que no lo mueve.


### A5. Diagnóstico 
# 1. Con `augment()` + `arrange()`, las tres personas peor predichas. ¿Qué tienen
# en común?

augment(m2) |>
  select(.rownames, ingreso, educ, experiencia, genero, .fitted, .resid) |>
  mutate(across(c(.fitted, .resid), round)) |>
  arrange(desc(abs(.resid))) |>
  head(3)
# son tres hombres con residuo NEGATIVO (ganan 250.000 a 300.000
# menos de lo que predice el modelo).

# 2. Con `augment()` + `summarise()`, la máxima distancia de Cook (`.cooksd`) y el
# máximo apalancamiento (`.hat`).

augment(m2) |>
  summarise(max_cook = max(.cooksd),
            max_hat  = max(.hat))
# 3. Re-estima `m2` **sin** la observación de mayor distancia de Cook y reporta
# cuánto cambia el coeficiente de educación.

augment(m2) |>
  slice_max(.cooksd, n = 1) |>
  select(.rownames, ingreso, educ, genero, .cooksd)       # es la fila 43

m2_sin <- casen |>
  filter(row_number() != 43) |>
  lm(ingreso ~ educ + experiencia + genero, data = _)

bind_rows(todos = tidy(m2), sin_43 = tidy(m2_sin), .id = "muestra") |>
  filter(term == "educ") |>
  select(muestra, estimate, std.error, p.value)

# sin la fila 43  el coeficiente de educ sube de 42246 a 45218 (+2972
# pesos, +7 %). Sigue siendo significativo

### A6. Interpretación de la Parte A

# «En el modelo con controles, cada año adicional de educación se asocia con
# $42.246 más de ingreso mensual, en promedio, manteniendo experiencia y
# género constantes (IC 95 %: 29.487 a 55.004).»
#
# «El coeficiente de generoM es 55.686. Esto significa que, a igual educación
# y experiencia, los hombres ganan en promedio $55.686 más que las mujeres,
# pero la diferencia no es significativa (p = 0.204; el IC incluye el 0).»
#
# Significancia: generoM(55.686 pesos, más que un
# año de educación) pero no significativo. Con n = 55 hay muy pocos datos para
# distinguir esa diferencia

# Limitación: la base tiene solo 55 personas con ingreso y es didáctica, y
# faltan variables (horas trabajadas, ocupación, región) que también se
# asocian con el ingreso


# PARTE B — ENE: mi pregunta

# B1. Pregunta
# PREGUNTA: en la fuerza de trabajo, ¿en cuántos puntos porcentuales
# es mayor la probabilidad de estar desocupada para una mujer que para un
# hombre, a igual tramo de edad y región, y cambia esa brecha según el tramo de
# edad? (trimestre mayo-julio 2026)

### B2. Prepara la base

# mi base limpia de la A3. Ya trae: sexo, edad, grupo_edad, region,
# activ, meses_busqueda (calculada en la A3 desde e6_mes y e6_ano) y fact_cal.
ene <- read.csv("data/processed/ene_a3.csv")

regiones <- c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso",
              "O'Higgins", "Maule", "Biobío", "La Araucanía", "Los Lagos",
              "Aysén", "Magallanes", "Metropolitana", "Los Ríos",
              "Arica y Parinacota", "Ñuble")
# Auditoría de las variables que uso
table(ene$activ_lbl)       # 41850 ocupados · 4412 desocupados · 36379 fuera de la FT
table(ene$sexo_lbl)        # solo Hombre / Mujer
table(ene$grupo_edad)      # 4 tramos, sin NA
table(ene$region)          # códigos 1 a 16

summary(ene$meses_busqueda[ene$activ == 2])
# Entre los desocupados: mediana 3 meses, máximo 244, y 258 NA.
# Los 258 NA son los centinelas de e6_mes (88, 99) y e6_ano (8888, 9999) que
# ya convertí a NA en la A3. El máximo de 244 meses no es un
# centinela
#
# Selector por patrón: todas las variables de la pregunta E6 (inicio de la
# búsqueda) de una vez, para revisar que no queden centinelas 88/99/8888/9999.
ene |>
  filter(activ == 2) |>
  select(starts_with("e6_")) |>
  summary()
# e6_mes llega hasta 12 y e6_ano hasta 2026: no quedan centinelas.
# e6_mes tiene 258 NA y e6_ano 92: basta que falte UNO de los dos para que
# meses_busqueda quede NA

# Fuerza de trabajo = ocupados (1) + desocupados (2)
ft <- ene |>
  filter(activ %in% 1:2) |>
  mutate(desocupado = activ == 2,              # LÓGICA: TRUE / FALSE
         mujer      = sexo == 2,
         region_txt = regiones[region]) |>     # región como texto
  select(desocupado, mujer, grupo_edad, region_txt, meses_busqueda, fact_cal)

nrow(ft)                                       # 46262 personas
ft |> summarise(sin_ponderar = mean(desocupado),
                ponderada    = weighted.mean(desocupado, fact_cal))

# Tasa de desocupación: 9,5 % sin ponderar · 9,5 % ponderada.
# (En la A3: 8,9 % hombres y 10,3 % mujeres, ponderadas.)
#
# 💡 MODELO DE PROBABILIDAD LINEAL: desocupado es TRUE/FALSE, así que cada
#    coeficiente se lee en puntos porcentuales (0,01 = 1 pp).

# Agregación 1: tasa de desocupación por sexo, con cuántas personas hay (n()).
ft |>
  group_by(mujer) |>
  summarise(n    = n(),
            tasa = round(100 * weighted.mean(desocupado, fact_cal), 1)) |>
  ungroup()
# Hombres 8,9 % · mujeres 10,3 %: 1,4 pp de diferencia, igual que el modelo base.

# Agregación 2: tasa por tramo de edad y sexo.
tasas_tramo <- ft |>
  group_by(grupo_edad, mujer) |>
  summarise(n    = n(),
            tasa = 100 * weighted.mean(desocupado, fact_cal)) |>
  ungroup()

# Comparación con group_by() + mutate(): dentro de cada tramo, cuánto más alta
# es la tasa de las mujeres que la de los hombres del MISMO tramo.
# (summarise() junta cada grupo en una fila; mutate() mantiene todas las filas
#  y le agrega a cada una la comparación con su grupo.)
tasas_tramo |>
  group_by(grupo_edad) |>
  mutate(brecha_pp = round(tasa - tasa[mujer == FALSE], 1)) |>
  ungroup() |>
  filter(mujer) |>
  select(grupo_edad, n, brecha_pp)
# Brecha: 15-24: 1,8 pp · 25-34: 0,9 · 35-54: 1,3 · 55+: 0,8.
# La brecha es mayor entre los jóvenes, pero la tasa también: entre 15 y 24
# años la desocupación es de 23-25 %

# Tramos de búsqueda con case_when(). Los NA se tratan EXPLÍCITAMENTE con su
# propia categoría (si no, case_when() los dejaría como NA sin avisar).
desocupados_tramos <- ft |>
  filter(desocupado) |>
  mutate(tramo_busqueda = case_when(
    is.na(meses_busqueda)  ~ "Sin dato",
    meses_busqueda <= 3    ~ "Hasta 3 meses",
    meses_busqueda <= 12   ~ "4 a 12 meses",
    meses_busqueda > 12    ~ "Más de 12 meses"))

desocupados_tramos |> count(tramo_busqueda)    # ninguna fila queda en NA

desocupados_tramos |>
  filter((grupo_edad == "15-24" | grupo_edad == "25-34") &
           tramo_busqueda == "Más de 12 meses") |>
  group_by(mujer) |>
  summarise(n = n())
# 132 mujeres y 91 hombres jóvenes llevan más de un año buscando trabajo.

### B3. Al menos dos modelos

# Modelo base: solo el sexo: la brecha bruta
d1 <- lm(desocupado ~ mujer, data = ft, weights = fact_cal)

# Modelo con controles: tramo de edad y región (los dos como texto).
d2 <- lm(desocupado ~ mujer + grupo_edad + region_txt, data = ft,
         weights = fact_cal)

tidy(d2) |>
  filter(term != "(Intercept)") |>               
  mutate(pp = round(100 * estimate, 1)) |>
  select(term, pp, p.value)
# En esta tabla se comparan con la primera region "base" en orden alfabetico
# lo que seria en este caso comparativo con grupo edad: "15-24" y 
# region txt: antofagasta los cuales no aparecen en la tabla 



### B4. Extrae y compara con `broom` + `dplyr`
# 1. El coeficiente de mujer en ambos modelos, en pp
bind_rows(base = tidy(d1), controles = tidy(d2), .id = "modelo") |>
  filter(term == "mujerTRUE") |>
  mutate(across(c(estimate, std.error), ~ round(100 * .x, 2))) |>   # a pp
  select(modelo, estimate, std.error, p.value)

# Resultado: 1,35 pp (base) -> 1,16 pp (con controles). Ambos significativos.

# 2. glance() de ambos modelos apilados
modelos_ene <- list(base = d1, controles = d2)
lapply(modelos_ene, glance) |>
  bind_rows(.id = "modelo") |>
  select(modelo, r.squared, adj.r.squared, nobs)

# Resultado: R² 0.0005 -> 0.0245, nobs 46262. El sexo no explica realmente el
# porque alguien esta desocupado, en realidad el tramo explica más esto.

# 3. ¿Cambia la brecha según el tramo de edad? Una regresión por tramo,
#    con intervalo de confianza.
brecha_tramo <- ft |>
  group_by(grupo_edad) |>
  group_modify(~ tidy(lm(desocupado ~ mujer + region_txt,
                         data = .x, weights = fact_cal),
                      conf.int = TRUE)) |>
  filter(term == "mujerTRUE") |>
  ungroup() |>
  mutate(across(c(estimate, conf.low, conf.high), ~ round(100 * .x, 1)),
         distinta_de_cero = conf.low > 0) |>
  select(grupo_edad, estimate, conf.low, conf.high, distinta_de_cero)

brecha_tramo

# Resultado: 15-24: 1,9 pp · 25-34: 1,0 pp · 35-54: 1,3 pp · 55+: 0,8 pp.
# Solo en 35-54 el intervalo no incluye el cero.

# Y una interacción (*) para probar si la brecha DIFIERE entre tramos.
# cuánto cambia la brecha respecto al tramo base (15-24).
d_int <- lm(desocupado ~ mujer * grupo_edad + region_txt, data = ft,
            weights = fact_cal)
tidy(d_int) |>
  filter(term %in% c("mujerTRUE", "mujerTRUE:grupo_edad25-34",
                     "mujerTRUE:grupo_edad35-54", "mujerTRUE:grupo_edad55+")) |>
  mutate(pp = round(100 * estimate, 1)) |>
  select(term, pp, p.value)

# Resultado: las interacciones son -0,6 a -1,0 pp, con p-values 0,39 a 0,60.
# No se puede afirmar que la brecha sea distinta entre tramos de edad.

### B5. Sensibilidad

d2_sin_ponderar <- lm(desocupado ~ mujer + grupo_edad + region_txt, data = ft)

bind_rows(ponderado = tidy(d2), sin_ponderar = tidy(d2_sin_ponderar),
          .id = "modelo") |>
  filter(term == "mujerTRUE") |>
  mutate(pp = round(100 * estimate, 2)) |>
  select(modelo, pp, p.value)

# Resultado: sin ponderar la brecha es 1,47 pp; ponderada, 1,16 pp. Ponderar
# la baja en 0,31 pp (un 21 %): la muestra sin ponderar exagera la brecha

### B6. Interpretación de la Parte B (esto es lo que más pesa)

# En mayo-julio 2026, ser mujer se asocia con una probabilidad de estar
# desocupada 1,35 pp mayor que la de un hombre (modelo base: responde
# "¿cuánto difieren hombres y mujeres?"). Al comparar personas del mismo tramo
# de edad y región (modelo con controles: "¿cuánto difieren hombres y mujeres
# de la misma edad y región?") la brecha baja levemente a 1,16 pp: casi
# nada de la brecha se explica por edad o región.
# Limitaciones: 
# (1)  los errores estándar de lm() con weights no consideran el diseño muestral
# de la ENE (estratos, conglomerados) 
# (2) faltan variables como educación o hijos en el hogar, que podrían
# explicar parte de la brecha.

## Parte C — Comunicar (10 puntos)

# 1. Los modelos de la Parte B lado a lado
modelsummary(modelos_ene,
             stars   = TRUE,
             gof_map = c("nobs", "r.squared"))

# 2. Exportar a CSV
dir.create("data/processed", showWarnings = FALSE)
modelsummary(modelos_ene, output = "data/processed/t3_tabla_ene.csv")
tidy(m2, conf.int = TRUE) |>
  write.csv("data/processed/t3_coeficientes_casen.csv", row.names = FALSE)






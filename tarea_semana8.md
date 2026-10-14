# T3 — Regresión económica: CASEN y ENE
### Módulo II · Semana 8 · **Tarea calificada — 14 % de la nota final**
#### Fundamentos de Programación para Análisis Económico · UdeC-EAN

**Tipo:** calificada.
**Entrega:** script `.R` comentado + `README.md` + CSV de resultados, en tu
repositorio de GitHub.
**Vienes preparado si hiciste:** [A3 (S6, limpieza de la ENE)](tarea_semana6.md),
el [laboratorio de la Semana 7](../labs/semana7_lab_esqueleto.R) (tus funciones) y
el [laboratorio de la Semana 8](../labs/semana8_lab_esqueleto.R).
**Uso de IA:** restringido (ver el final de este documento).

---

## Objetivo

Estimar, extraer, comparar, diagnosticar e **interpretar** modelos de regresión
en R, con dos bases:

- **Parte A — CASEN.** La ecuación de ingresos de la Semana 8, con todo el flujo de
  `broom` + `dplyr`. Es igual para todo el curso: se evalúa que domines el flujo.
- **Parte B — ENE.** Tu propia pregunta sobre el mercado laboral chileno, con la
  base que limpiaste en la A3. Se evalúa tu juicio: qué modelo, qué controles,
  cómo se lee y qué **no** se puede concluir.

> Datos:
> - `data/raw/casen_reducido.csv` (60 filas; **5 `NA` en `ingreso`**).
> - Tu `data/processed/ene_a3.csv` (A3). Si tu pregunta necesita variables que no
>   conservaste, vuelve a `data/raw/ene-2026-06-mjj.csv`, agrégalas **y repite la
>   limpieza que corresponda** (centinelas, `NA` estructurales). Documéntalo.
> - Manual: `data/raw/codigos-ene-2020.pdf`.
>
> Recordatorio permanente: **esto no es un análisis causal.** Se dice «se asocia
> con», nunca «causa» ni «provoca».

### Esto NO es repetir el laboratorio

El laboratorio estimó un modelo fijo: `informal ~ edad + I(edad^2) + mujer +
categoria`. En la Parte B ese modelo, tal cual, **no se acepta**. Puedes usar la
misma variable dependiente, pero tu pregunta, tus controles y tu interpretación
deben ser tuyos.

---

## Qué debes hacer

Crea un script `scripts/tarea_t3.R` con encabezado (autor, fecha, qué hace). Una
sola llamada a librerías al inicio:

```r
library(dplyr)
library(broom)
library(modelsummary)
```

## Parte A — CASEN (45 puntos)

### A1. Dos modelos

```r
casen <- read.csv("data/raw/casen_reducido.csv") |>
  mutate(experiencia = pmax(edad - educ - 6, 0))

m1 <- lm(ingreso ~ educ, data = casen)
m2 <- lm(ingreso ~ educ + experiencia + genero, data = casen)
```

### A2. La tabla de coeficientes, con `broom` + `dplyr`

**No uses `summary()` para reportar.** Construye con un solo pipe una tabla de `m2`
que tenga, por cada coeficiente salvo el intercepto: estimación redondeada,
intervalo de confianza al 95 %, p-value y una columna lógica que diga si es
significativo al 5 %. Ordénala por p-value.

> Pista: `tidy(m2, conf.int = TRUE)` y los verbos de siempre (Sesión 2).

### A3. Comparar modelos

Agrega **un tercer modelo de tu elección** sobre CASEN. Algunas opciones:

| Opción | Qué agrega |
|---|---|
| `I(experiencia^2)` | perfil cóncavo de Mincer |
| `log(ingreso)` como variable dependiente | coeficientes en porcentaje |
| `educ * genero` | retornos distintos por género (interacción) |
| una variable creada con `mutate()` (ej. `superior = educ >= 13`) | un umbral |

Guarda los tres en una lista con nombre y apila sus `glance()`:

```r
modelos <- list(simple = m1, controles = m2, mio = m3)
lapply(modelos, glance) |> bind_rows(.id = "modelo")
```

Comenta en 2–3 líneas: ¿qué pasa con el R² y con el R² **ajustado**? ¿Tu modelo
mejora algo?

### A4. ¿Cambió el coeficiente de educación?

Con `bind_rows()` de los `tidy()` y `filter(term == "educ")`, muestra el
coeficiente de educación en los tres modelos. Explica en un comentario por qué
cambia o no cambia (pista: ¿qué se tiene que cumplir para que un control mueva
un coeficiente?).

> Si tu tercer modelo usa `log(ingreso)`, el coeficiente de `educ` está en otra
> escala. Dilo y no lo compares directamente con los otros dos.

### A5. Diagnóstico

1. Con `augment()` + `arrange()`, las tres personas peor predichas. ¿Qué tienen
   en común?
2. Con `augment()` + `summarise()`, la máxima distancia de Cook (`.cooksd`) y el
   máximo apalancamiento (`.hat`).
3. Re-estima `m2` **sin** la observación de mayor distancia de Cook y reporta
   cuánto cambia el coeficiente de educación.

### A6. Interpretación de la Parte A

Completa, con tus números:

> «En el modelo con controles, cada año adicional de educación se asocia con
> $\_\_\_\_\_\_ más de ingreso mensual, en promedio, manteniendo experiencia y
> género constantes (IC 95 %: \_\_\_\_\_\_ a \_\_\_\_\_\_).»
>
> «El coeficiente de `generoM` es \_\_\_\_\_\_. Esto significa que \_\_\_\_\_\_
> \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_.»

Y una línea sobre significancia **e** importancia económica: ¿qué coeficiente es
grande pero no significativo? ¿Qué significa eso con n = 55?

---

## Parte B — ENE: tu pregunta (45 puntos)

### B1. Declara tu pregunta

En un comentario, antes del código. Debe poder responderse con una regresión sobre
la ENE:

- ❌ «Voy a ver qué variables afectan el empleo.» → no es una pregunta, y «afectan»
  es lenguaje causal.
- ✅ «¿Cuántas horas menos trabajan habitualmente las mujeres ocupadas que los
  hombres, a igual edad y nivel educacional, y cambia esa brecha entre regiones?»
- ✅ «¿Se asocia el nivel educacional con la probabilidad de ser informal, y cuánto
  de esa asociación se mantiene al comparar personas de la misma categoría
  ocupacional?»

### B2. Prepara la base

Parte de tu `ene_a3.csv`. Si agregas variables desde el archivo crudo, **audítalas
igual que en la A3**: distribución completa, centinelas, `NA` estructurales.

> ⚠️ Una trampa concreta: `cine11_1d` (nivel educacional, CINE 2011) tiene el
> código **`9 = Sin clasificación`**. Si lo usas como número, para `lm()` es un
> nivel educacional *más alto que el doctorado* (6). Y `habituales` trae `888` y
> `999` — el laboratorio te mostró qué le hacen a un coeficiente.

### B3. Al menos dos modelos

| Requisito | Detalle |
|---|---|
| Variable dependiente | una sola, numérica (`habituales` limpias) o lógica (`informal`, `desocupado`…). Si es lógica, el modelo es de **probabilidad lineal** y se lee en puntos porcentuales |
| Modelo base | tu variable explicativa de interés, más lo mínimo |
| Modelo con controles | agrega **al menos dos** controles, y **al menos uno que el laboratorio no usó** (educación, región, tramo de edad, sector…) |
| Ponderación | ambos modelos con `weights = fact_cal` |
| Variables categóricas | como texto o `factor()`, nunca como número (región 1 a 16 **no** es una escala) |

### B4. Extrae y compara con `broom` + `dplyr`

1. Una tabla con el coeficiente de tu variable de interés en ambos modelos
   (`bind_rows()` de los `tidy()`), en la unidad que corresponde (horas, pp).
2. `glance()` de ambos modelos apilados.
3. **Uno** de estos dos:
   - una regresión por grupo con `group_by()` + `group_modify()` (por región,
     sexo, tramo…), con intervalo de confianza; o
   - una interacción (`*`) que ponga a prueba si tu relación difiere entre grupos.

### B5. Sensibilidad

Muestra **una** decisión que mueva tu resultado y cuánto lo mueve. Opciones:

- con y sin ponderar;
- con y sin limpiar un centinela;
- con y sin un control clave;
- con y sin un grupo extremo (ej. mayores de 65).

### B6. Interpretación de la Parte B (esto es lo que más pesa)

De **6 a 10 líneas**, responde tu pregunta. Exigencias:

- Magnitudes **en la unidad correcta** (horas, puntos porcentuales), no solo signos.
- Di «se asocia», nunca «causa».
- Explica qué cambió al agregar controles y **qué pregunta responde** cada modelo.
- Distingue significancia de importancia: con más de 40.000 observaciones casi todo
  es significativo; ¿tu coeficiente es **grande**?
- Nombra **dos** limitaciones. Una de ellas debe ser que los errores estándar de
  `lm()` con `weights` no consideran el diseño muestral de la ENE.

---

## Parte C — Comunicar (10 puntos)

1. Una tabla con `modelsummary()` que muestre los modelos de la Parte B lado a lado
   (estrellas, `nobs`, R²).
2. Exporta a CSV:

```r
dir.create("data/processed", showWarnings = FALSE)
modelsummary(modelos_ene, output = "data/processed/t3_tabla_ene.csv")
tidy(m2, conf.int = TRUE) |>
  write.csv("data/processed/t3_coeficientes_casen.csv", row.names = FALSE)
```

3. En el `README.md`: tu pregunta de la Parte B, tu respuesta en tres líneas y cómo
   correr el script.

---

## Rúbrica (14 % de la nota final)

| Criterio | Puntos |
|---|---:|
| **Parte A** | |
| A1–A2. Modelos y tabla de coeficientes con `tidy()` + `dplyr` en un pipe | 10 |
| A3–A4. Tercer modelo, comparación con `lapply()` + `bind_rows()`, R² ajustado comentado, coeficiente de educación explicado | 12 |
| A5. Diagnóstico con `augment()`: peor predichos, influencia y re-estimación sin el caso más influyente | 10 |
| A6. Interpretación con IC, ceteris paribus y significancia ≠ importancia | 13 |
| **Parte B** | |
| B1–B2. Pregunta propia y respondible; variables auditadas (centinelas, `NA`, códigos) | 8 |
| B3. Modelos bien especificados: ponderados, categóricas como texto, un control nuevo | 10 |
| B4. Extracción y comparación con `broom` + `dplyr`; `group_modify()` **o** interacción | 8 |
| B5. Sensibilidad cuantificada | 5 |
| B6. **Interpretación**: unidades correctas, lenguaje asociativo, qué responde cada modelo, dos limitaciones | 14 |
| **Parte C** | |
| `modelsummary()`, CSV exportados y README | 5 |
| Script reproducible (corre de principio a fin sin editar) + commits claros | 5 |
| **Total** | **100** |

> **Descuento por reproducibilidad:** si el script no corre de cero en otra máquina
> (rutas absolutas, objetos que no existen, `setwd()`), se descuentan 15 puntos.
> Pruébalo reiniciando R y corriéndolo completo antes de entregar.
>
> **Descuento por lenguaje causal:** cada «causa», «provoca», «el efecto de X es»
> sin calificar descuenta 2 puntos de la interpretación correspondiente.

---

## Entrega

1. `scripts/tarea_t3.R` en tu repositorio.
2. Los dos CSV en `data/processed/`.
3. `README.md` actualizado.
4. **Avanza por commits** (no subas todo en uno): el historial es tu evidencia de
   trabajo.
5. Pega el enlace del repositorio en Canvas.

---

## Declaración de autoría y uso de IA

Copia este bloque al final de tu `README.md` y complétalo:

```markdown
## Declaración de autoría y uso de IA
- Herramienta utilizada: ____ (o «ninguna»)
- Para qué la usé: ____ (ej.: entender por qué un coeficiente salía NA)
- Qué hice yo: ____ (ej.: elegí la pregunta, los controles y escribí la interpretación)
- Verificación: confirmo que entiendo y puedo explicar todo lo que entrego.
```

**Uso permitido:** pedir que te expliquen un error, preguntar qué hace una función
o un argumento (`weights`, `conf.int`, `group_modify`), pedir alternativas a un
código que ya escribiste, revisar la redacción.

**Uso no permitido:** pegar este enunciado y entregar lo que devuelva; entregar
código que no puedes explicar línea por línea; pedirle que **interprete** los
coeficientes por ti — esa es justamente la habilidad que se evalúa.

Un aviso concreto: si le pides a un chatbot que «haga una regresión con la ENE», te
va a inventar variables (`ingreso`, `salario`, `años_educ`) que **no existen** en
este archivo. El manual manda sobre el chatbot.

Declarar el uso **no baja la nota**; ocultarlo constituye falta a la ética
académica (Guía de uso ético de tecnologías digitales, UdeC).

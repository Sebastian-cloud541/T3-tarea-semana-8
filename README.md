# T3-tarea semana 8
T3 — Regresión económica: CASEN y ENE

Autor: Sebastián Navarrete Poblete · 
Fundamentos de Programación para Análisis Económico · UdeC-EAN

Pregunta (Parte B)

En la fuerza de trabajo, ¿en cuántos puntos porcentuales es mayor la 
probabilidad de estar desocupada para una mujer que para un hombre, 
a igual tramo de edad y región, y cambia esa brecha según el tramo de edad?
(ENE, mayo–julio 2026)

Respuesta:
De cada 100 hombres en la fuerza de trabajo, unos 9 están desocupados
(8,9 %); de cada 100 mujeres, unas 10 (10,3 %). La diferencia es de 1,4 
puntos porcentuales (pp), y baja a 1,2 pp al comparar hombres y mujeres
de la misma edad y región: es significativa, pero no mucho. La diferencia 
aparece en todos los tramos de edad (entre 0,8 y 1,9 pp) y no cambia de 
forma significativa entre ellos. Entre los desocupados, las mujeres llevan
unas 3 semanas (0,7 meses) más buscando trabajo, pero esa diferencia no
es significativa.

Cómo correr el script
1. Abrir el proyecto desde el .Rproj.
2.Verificar que existan data/raw/casen_reducido.csv y data/processed/ene_a3.csv 
(base limpia de la A3).
3.Correr completo scripts/tarea_t3.R. Requiere dplyr, broom y modelsummary.
4.El script genera data/processed/t3_tabla_ene.csv y 
data/processed/t3_coeficientes_casen.csv.

## Declaración de autoría y uso de IA
- Herramienta utilizada: Claude
- Para qué la usé: Correcion de errores en el codigo, guia de codigos de R para 
las acciones que se necesitan para responder mi pregunta.
- Qué hice yo: Escribir el codigo,elegir la preguntar,escribir
e interpretar los resultados 
- Verificación: confirmo que entiendo y puedo explicar todo lo que entrego.




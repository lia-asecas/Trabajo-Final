# Trabajo-Final

Para resolver las consignas del **Trabajo Práctico Final del curso Curso ASET - "Programación y visualización en estadísticas laborales"**
éste trabajo integra y mejora el cálculo sobre el Índice de Precariedad laboral que realizamos desde el Observatorio de les Trabajadores en La Izquierda Diario.

Este indicador trabaja sobre los **microdatos de la EPH-Indec**, tomando la población ocupada (ESTADO=1). Según sus condiciones laborales
categoriza a cada trabajador/a según su precariedad laboral en tres dimensiones: según sus **Ingresos**, el tipo de **Jornada** y su **relación Contractual**.
Y a la vez calcula los niveles de precariedad según las combinaciones de la dimensiones que alcance: ninguna (no presenta precariedad en ninguna de las 3 dimensiones), baja (al menos 1 dimensión),
Media (combina dos aspectos de precariedad) y Alta (cuando sus condiciones laborales refleja los tres criterios de precariedad a la vez).

Con este trabajo transformamos el proceso de cálculo en un Repositorio para el trabajo colaborativo del equipo, que incluye y adapta el script de cálculo,
presenta listados de control, expide informes htlm embebidos con los resultados del cálculo para su publicación, y dispone permanentemente un
Dashboard (interactivo) con los últimos resultados calculados.

# Consignas:

### 1. Selección de encuesta: 
EPH-Indec, microdatos de población por año y trimestre

### 2. Informe de análisis exploratorio: 
En los scripts de cálculo e Informes se vuelcan los totales absolutos del período y los resultados del Indicador, tanto del período como de la serie 2017 en adelante) 
y se desagregan según variables sensibles como Sexo (P02), categoria ocupacion (CAT_OCUP), Edad (P03), nivel educativo, Region, ocupación y Jerarquía en la misma.

### 3. Script en R:
- **/scripts/app.R**: una shiny que muestra los resultados del último procesamiento (en tarjetas), la fecha y el período guardados en */log/ejecucion.log*, y habilita botones de acción (*Nuevo Cálculo / Ver Informe / Consultar*)
- **/scripts/02_actualizar_indice.Rmd**: es un Markdown que saca un html y un docx con los resultados del procesamiento tomando el período (año y trimestre ingresado por pantalla (el botón *Nuevo Cálculo* habilita un popup */apps/nuevo_calculo/app.R* que permite ingresar año y trimestre). Deja 3 resultados: */resultados/Ind_aaaa_Tt.html .docx y .xlsx*.
- **/scripts/02_calcular_indice.R**: descaga los microdatos de la EPH, toma lo necesario para el cálculo de */bases/umbrales.xlsx*, calcula los indicadores, guarda las bases resultantes reducidas (*/bases/individual_procesada_aaaa_Tt.csv*) para luego poder consultar sobre el .RData guardado, y agrega una línea en */log/ejecucion.log*).
- **/scripts/Formatos.R** y **funciones_indice.R**: contiene los formatos generales y las funciones necesarias en el script de cálculo, respectivamente.
- **/apps/nuevo_calculo/app.R**: es una shiny que habilita el ingreso de los parametros para hacer ejecutar el cálculo (año y trimestre - por el momento solo va a funcionar para **2026-1** o **2025-4**).
- **/apps/consultar/app.R**: una shiny que habilita una pantalla al usuario con un gráfico de líneas (los indicadores a través de los años en el mismo trimestre) y una tabla con los valores absolutos y % que varían según los filtros (sobre CAT_OCUP, JErarquía y Ocupación PP04D_COD) que puede activar el usuario para ver los cambios en 
los valores del indicador en la serie.

### 4. Dashboard: 
Una shiny para ver los resultados de la última ejecución y a la vez consultar, filtrar y ver cómo cambian los indicadores en forma interactiva.
Está programada en */apps/consultar/app.R*. 
Se puede filtrar la categoría ocupacional, la jerarquía en la cual se desempeñan los ocupados, y la ocupación.
   

<br><br>

## Anexo metodológico
Índice de Precarización Laboral, elaboración propia en base a Microdatos EPH-INDEC

### 1. Introducción y marco conceptual
Tomando como referencia informes nacionales e internacionales basados en estadísticas laborales, el índice de precarización laboral se organiza a partir de tres dimensiones centrales:

*	Ingresos
*	Jornada de trabajo
*	Condiciones de contratación


### 2. Universo de análisis
El universo de análisis del índice está compuesto por los ocupados relevados por la EPH-INDEC de los 31 aglomerados urbanos.

*Nota: el Indicador que publicamos en La Izquierda Diario excluye de la muestra a los patrones (CAT_OCUP=1) y quienes se desempeñan en*
*las fuerzas represivas (PP04D_COD que comiencen con 48 o 49), pero para este ejercicio vamos a utilizar a todos los ocupados,*
*justamente para ver en la pantalla de consulta con filtros, cómo cambian los indicadores excluyendo a éstos ocupados*
*como así también otros utilizando el criterio de la jerarquía en la que desempeña la tarea (el 3 dígito del PP04D_COD).*


### 3. Dimensiones


#### **Dimensión 1**: Precarización por ingresos

Definición: se considera precario por ingresos a todo trabajador cuyo ingreso en la ocupación principal es inferior al valor de la Canasta Básica Total (CBT) para un adulto equivalente.

**Umbral utilizado**: se utiliza el valor mensual de la CBT para un adulto equivalente publicado por la Junta Interna ATE-INDEC, correspondiente al período de referencia de cada trimestre analizado.
(el excel con los datos de los umbrales se encuentra en /bases/umbrales.xlsx y por el momento solo tiene cargado el trimestre 4 para los años 2017-2025 y el trimestre 1 para 2017-2026)

Se utiliza este umbral pues el cálculo de las canastas del INDEC están basadas en Encuestas de Gastos muy antiguas.

*Nota: el 25% de los ocupados encuestados elige no contestar sobre su ingreso (P21, ingreso de la ocupación principal), por lo cual,*
*la EPH recalcula en un ponderador distinto (PONDIIO) utilizado para los cálculos sobre ingresos. Por lo cual, hay un 25% de lo ocupados*
*sobre los cuales no se podrá calcular el Nivel de precariedad pues no tiene indicador de precariedad por ingreso.*


#### **Dimensión 2**: Precarización por jornada laboral
Definición: se considera precario por jornada laboral a quien trabaja fuera de los parámetros de la jornada laboral legal máxima vigente en Argentina o quien, trabajando a tiempo parcial, demanda más horas de trabajo.

Se toman las siguientes variables de la EPH-INDEC:

* Subocupados demandantes (< 35 hs. sem. y buscan más horas) — INTENSI=1 o INTENSI=3, o (INTENSI=2 & PP03I=1)
* Sobreocupados (> 45 hs. sem.) — PP03C=2
* Pluriempleados (más de un empleo) — PP03C=2 sobre ocup.


#### **Dimensión 3**: Precarización por condiciones de contratación
Definición: se considera precario por contrato a quien no accede al trabajo registrado con derechos plenos (lo que la OIT denomina 'trabajo típico'). 
Se incluyen las siguientes situaciones:

* Asalariados sin descuento jubilatorio  (PP07H=2)
* Asalariados registrados sin al menos un derecho laboral: vacaciones, aguinaldo, licencia por enfermedad u obra social (PP07G1=2, PP07G2=2, PP07G3=2 o PP07G4=2)
* Asalariados con contrato a plazo fijo (PP07H=1 & PP07C=1)
* Trabajadores familiares sin remuneración (CAT_OCUP=4)
* Cuentapropistas no profesionales (CAT_OCUP=2 & CALIFICACION ≠ 'Profesionales')


### 4.- Construcción del índice compuesto
Cada trabajador del universo recibe un valor 0 o 1 en cada dimensión. La suma de las tres dimensiones compone el índice, con valores posibles de 0 a 3.

Estos serían los cuatro niveles:

* 0 -> No alcanzados por el índice
* 1 -> Baja
* 2 -> Media
* 3 -> Alta

*Nota: Hay que tener en cuenta que cuantificar según su nivel es utilizando el ponderador PONDERA que para los ocupados que no informan su ingreso está*
*vacío y su ingreso registra P21=-9. En esos casos, el nivel de las dimensiones está dado por dimensión jornada (0 o 1) + dimensión contrato (0 o 1) y su rango* 
*será entonces entre 0 y 2. Por lo cual ese 25% de los ocupados nunca pueden alcanzar la dimensión Alta como así tampoco Ninguna (pues no hay información sobre ingresos).*


### 5.- Fuente de datos
Los microdatos utilizados provienen de la Encuesta Permanente de Hogares (EPH) del Instituto Nacional de Estadística y Censos (INDEC).
Los umbrales de la Canasta Básica Total para adulto equivalente corresponden a los valores publicados por la Junta Interna ATE-INDEC.


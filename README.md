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

### 4. Dashboard: 
Una shiny para consultar, filtrar y ver cómo cambian los indicadores en forma interactiva está programada en */apps/consultar/app.R*. En la pantalla se puede filtrar la categoría ocupacional, la ocupación y la jerarquía en la cual se emplean los ocupados.
   

<br><br>

## Anexo metodológico
Índice de Precarización Laboral, elaboración propia en base a Microdatos EPH-INDEC

### 1. Introducción y marco conceptual
Tomando como referencia informes nacionales e internacionales basados en estadísticas laborales, el índice de precarización laboral se organiza a partir de tres dimensiones centrales:

*	Ingresos
*	Jornada de trabajo
*	Condiciones de contratación

### 2. Universo de análisis
El universo de análisis del índice está compuesto por los ocupados relevados por la EPH-INDEC, con las siguientes exclusiones:

*	Patrones (CAT_OCUP = 1).
*	Fuerzas de seguridad y defensa (PP04D_COD con prefijo 48 o 49)

El universo final comprende, entonces, a los asalariados, cuentapropistas y trabajadores familiares sin remuneración de los 31 aglomerados urbanos excluyendo a los sectores mencionados.


### 3. Dimensiones

#### **Dimensión 1**: Precarización por ingresos

Definición: se considera precario por ingresos a todo trabajador cuyo ingreso en la ocupación principal es inferior al valor de la Canasta Básica Total (CBT) para un adulto equivalente.
**Umbral utilizado**: se utiliza el valor mensual de la CBT para un adulto equivalente publicado por la Junta Interna ATE-INDEC, correspondiente al período de referencia de cada trimestre analizado.

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
Cada trabajador del universo recibe un valor 0 o 1 en cada dimensión. La suma de las tres dimensiones compone el índice, con valores posibles de 0 a 3, que se traducen en cuatro niveles:

* 0 -> No alcanzados por el índice
* 1 -> Baja
* 2 -> Media
* 3 -> Alta

### 5.- Fuente de datos
Los microdatos utilizados provienen de la Encuesta Permanente de Hogares (EPH) del Instituto Nacional de Estadística y Censos (INDEC).
Los umbrales de la Canasta Básica Total para adulto equivalente corresponden a los valores publicados por la Junta Interna ATE-INDEC.


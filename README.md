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
- Script01: página HTML que presente al Oservatorio y al Indicador, con el valor (XX%) del indicador y el período calculado (sólo desagregado por sexo) y permita al usuario Ver/Bajar el último informe (en HTML) e ir a una pantalla interactiva que le permita visualizar (filtrar/combinar) el indicador por variables sensibles definidas.
- Script02: es el que descarga los datos (\bases), realiza el cálculo, registra el log (\log), y almacena las bases resultantes para el Dashboard.
- Script03: es el que genera el informe en HTML con los resultados del período y la serie (\resultados).
- Script04: es el que genera el panel interactivo en R-Shiny con los datos procesados.

### 4. Dashboard: 
El Script01 es un tablero de control (informa el indicador general y por sexo en el período al que hace referencia) 
y habilita el llamado al Script04, una app en R Shiny que dispone el conjunto de bases resultantes del último procesamiento para que el ususrio pueda filtrar y combinar variables observando los cambios en el indicador.
   

<br><br>

La metodología del cálculo del Indicador implica asignar a cada OCupado/a si es precario en cada una de las siguientes **diensiones**:
-Precariedad por Ingresos: Sobre los Ocupados (ESTADO=1) que hayan declarado su Ingreso de la Ocupación principal (P21>=0) y éste sea menor al valor 
del Adulto Equivalente calculado por la Junta Interna de ATE-INDEC en la Canasta Familiar de Ingresos Mínimos.
Hay que tener cuenta que aproximadamente el 20-25% de los ocupados no declara su ingreso. La EPH le asigna -9 a la variable P21, y arma un PONDERADOR específico, raclaculando el ponderador de las personas (PONDERA) distribuyendolo entre aquellos ocupados que sí declaran su ingreso, 
por lo cual se utiliza el ponderador PONDIIO para calcular los niveles de ocupados precarios por ingreso en relacion al total de ocupados.
-Precariedad por Jornada: mmmmmm
-Precariedad por Contrato: mmmm

<br><br>

Para calcular los niveles o dimensiones de precariedad de cada ocupado/a se presenta la dificultad del 20-25% de quellos que no informan su ingreso,
por lo cual, ese porcentaje no puede evaluarse en sus 3 dimensiones, no sabremos si su nivel es 0, 1, 2.


# Modelo Base Refinado

Se selecciona como modelo base refinado al modelo **102_modeltwoCptmDiagProp_Results** obtenido mediante estimación bayesiana. Se copian las carpetas desde 05_MINIMIZACION/04_modeloBayesiano/

Este modelo consiste en un modelo de dos compartimentos con matriz de *variabilidad interindividual diagonal* y error *residual proporcional*.

Este modelo cuenta con los siguientes parámetros poblacionales estimados:

|  |Parámetro|Media|DE|Mediana|P<sub>2.5%</sub>|P<sub>97.5%</sub>|
|--|---------|--------|--------|--------|---------|-------|
|1 |CL|10.6|0.897|10.5|8.81|12.4|
|2 |Q|11.7|2.75|11.7|6.24|17.2|
|3 |V<sub>1</sub>|46.8|2.80|46.8|41.1|52.3|
|4 |V<sub>2</sub>|49.9|3.03|49.9|43.9|55.8|
|5 |omega<sub>CL</sub>|0.0911|0.0559|0.0774|0.0287|0.236
|6 |omega<sub>Q</sub>|2.22|4.28|1.03|0.0283|10.6|
|7 |omega<sub>V1</sub>|0.121|0.111|0.0914|0.00741|0.422
|8|omega<sub>V2</sub>|1.07|1.08|0.808|0.0246|3.96|
|9|b|0.0922|0.00814|0.0918|0.0777|0.110

<hr><br>
Para ejecutar los **diagnósticos de prueba** del modelo se realizan los siguientes pasos:

1. Primero obtener un dataFrame con los parámetros del modelo:
    - `Parámetros poblacionales`
    - `Parámetros` estimados para cada `individuo`
    - `Predicciones poblacionales e individuales`

    Los parametros se extraen del archivo fit donde están las muestras obtenidas mediante el algoritmo NUTS.  
2. Realizar simulaciones de MonteCarlo con el diseño del experimento.
3. Obtener archivos con residuales PWRES, IWRES, y NPDE.
4. Obtener un pcVPC y NPC
5. 
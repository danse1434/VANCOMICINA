# Estimación de parámetros bayesiana

## Modelo Final

En esta sección se muestran los resultados de un análisis de los datos de vancomicina (VAN) mediante el uso de modelos longitudinales (efectos mixtos no lineales) implementados en Stan.

Para el análisis de datos de vancomicina se utilizó el lenguaje de programación probabilístico Stan, y su paquete para utilización mediante R. Bajo este paradigma, se utilizó el algoritmo NUTS (No-U-Turn Sampler) que cuenta con ventajas en problemas multidimensionales complejos y suele generar una proporción alta de muestreos efectivos en comparación con el algoritmo de Metropolis-Hastings. La estimación de los parámetros se realizó teniendo en cuenta las mismas especificaciones del modelo base, se utilizaron 4 cadenas con diferentes conjuntos de valores iniciales, 500 muestras de *burn-in*, 2000 muestreos posteriores, y la selección de muestras o *thinning* generadas cada 10 muestreos (tal como se determinó por diagnósticos de autocorrelación).

## 1. Organización de modelos

A continuación se muestra una tabla con la explicación de los modelos explorados en la carpeta `"./src"`. En este carpeta se encuentran los modelos con la secuencia `100` y `102` con formato \*.stan.

En las secuencias `000` a `009` se encuentran scripts de procesamiento de datos en el formato requerido para la aplicación de cada modelo. Con las secuencias `010` a `019` se encuentran scripts para la ejecución del modelo y los datos, así como un análisis general de diagnósticos de modelos.

Entre las secuencias `040` a `070` se encuentran scripts con funciones auxiliares.

+------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------+
| Modelo                 | Descripción                                                                                                                                     |
+========================+=================================================================================================================================================+
| `101_modeloFinal`      | Modelo de dos compartimentos con matriz de variabilidad interindividual completa, modelo de error residual aditivo.                             |
+------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------+
| `102_modeloFinal`      | Modelo de dos compartimentos con matriz de variabilidad interindividual diagonal y correlación entre V1 y V2, modelo de error residual aditivo. |
+------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------+

## 2. Organización del proyecto

En la carpeta `./docs/` se encuentran una serie de prototipos para el desarrollo de la función de dos compartimentos aplicada en cada modelo, así como su comparación con implementaciones en otros programas. En la carpeta `./figures/` se encuentran gráficos diagnósticos y de rendimiento predictivo para los modelos desarrollados. En la carpeta `./models/` se encuentran los muestreos de la función *a posteriori* obtenidos para cada modelo, como método de permanencia de los mismos. En la carpeta `./reports/` se encuentran tablas con los resúmenes de los estadísticos obtenidos para cada modelo. En la carpeta `./src/` se encuentran los modelos discutidos en la sección anterior.

## 3. Resultados principales

Se consideran al modelo 102 como el resultado principales obtenidos en estos procedimientos, la diferencia entre los dos radica en la utilización de una BSV con bloqueo en V1-V2. Ambos modelos son de dos compartimentos, con matriz de variabilidad interindividual diagonal, modelo de error proporcional y distribución log normal para las observaciones.

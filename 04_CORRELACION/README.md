# Exploración de Bloqueo en Matriz de Variabilidad Omega
## Análisis de farmacocinética de vancomicina

A continuación, se describen las correspondencias entre 

| Proyecto | Bloqueo |
|------|------|
|M2CPTM_nobs_2_aditv| Matriz Omega Diagonal (Base) |
|M2CPTM_nobs_2_full | Matriz Omega Completa |
|M2CPTM_nobs_2_aditv_corr1| Matriz con correlación entre Cl, V1, y V2|
|M2CPTM_nobs_2_aditv_corr2| Matriz con correlación entre V1 y V2|
|M2CPTM_nobs_2_aditv_corr3| Matriz con correlación entre Cl y V1|

<br><br>
El modelo con la mejor bondad de ajuste fue M2CPTM_nobs_2_aditv_corr2 con una reducción en 1.41 para BICc frente al `Modelo de Error Base`. Este modelo indica que la correlación entre V1 y V2 es 1 de manera significativa, y los SE por FIM muestran estabilidad. 

El modelo con la correlación entre Cl y V1 no mejora bondad de ajuste, y también tiene problemas en la estabilidad de matriz FIM.

La matriz BSV completa tiene el peor desempeño indicando que el modelo está sobreparametrizado.


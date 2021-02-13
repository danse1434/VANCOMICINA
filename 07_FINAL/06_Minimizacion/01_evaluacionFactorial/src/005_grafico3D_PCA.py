# Carga de Paquetes
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import sys

sys.path.append('./src')
functions_PCA = __import__('011_funciones_PCA')

############################
data_PCA = pd.read_csv('results/02_PCA.csv')                         # Lectura de archivos de PCA
df_estandarizado = pd.read_csv('results/03_datosEstandarizados.csv') # Lectura de datos estandarizados

fig = plt.figure( figsize = (7,5), dpi = 1000 )
fig.add_subplot(221)
functions_PCA.generarBiplot(df_estandarizado, data_PCA.loc[:,'label'], which=(1,2))
fig.add_subplot(222)
functions_PCA.generarBiplot(df_estandarizado, data_PCA.loc[:,'label'], which=(1,3))
fig.add_subplot(223)
functions_PCA.generarBiplot(df_estandarizado, data_PCA.loc[:,'label'], which=(2,3))
fig.add_subplot(224)
x = functions_PCA.generarTriplot(df_estandarizado, data_PCA.loc[:,'label'], which=(1,2,3), pos=224, fig=fig)

fig.set_tight_layout(True)

plt.savefig('figures/04_resultadosPCA.pdf', format = 'pdf', dpi = 1000)
fig.savefig('figures/04_resultadosPCA.png', format = 'png', dpi = 1000)

############################
# Generar triplot en Plotly
fig_plotly = functions_PCA.generarTriplot_Plotly(x)
fig_plotly.write_html("./figures/05_resultadosPCA.html")

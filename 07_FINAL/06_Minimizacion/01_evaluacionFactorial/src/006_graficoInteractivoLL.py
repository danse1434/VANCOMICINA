# Carga de Paquetes
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import sys
import plotly.express as px
import plotly.graph_objects as go # or plotly.express as px


# Lectura de archivo csv con datos
data_list = []

rutaSAEM = '/modeloFinal/ChartsData/Saem/CvParam.txt'

for i in range(81):
  df = pd.read_csv(f'evaluacion/E{i+1}' + rutaSAEM)
  df['iteracion'] = i+1
  data_list.append(df)

data_list[1]

data = pd.concat(data_list, axis=0)


df_indice = pd.read_csv('results/001_arregloOrigen.csv')
df_indice['iteracion'] = np.arange(1, 82, 1)

def mezclarIndices(lista = [], nround = 2):
  """Mezcla Indices
    args:
      lista: lista con pd.Series indicando columnas que se deben unir mediante 
      esta función.
      nround: número de cifras significativas para unir las series.
    return: 
      Retornar una lista con los valores y el **id** de datos.
  """
  # Convierte todos los elementos en string con redondeo
  lista1 = [[f"{round(j, nround)}" for j in i] for i in lista]
  # Convierte a los elementos en letras indicando el nivel (tres posibles valores)
  lista2 = [['L' if i == min(K) else 'H' if i == max(K) else 'M' for i in K] for K in lista]
  
  lista1a = ['-'.join(i) for i in np.transpose(lista1)]
  lista2a = ['-'.join(i) for i in np.transpose(lista2)]
  
  return [lista1a, lista2a]


df_indice = df_indice.assign(
    espec1=lambda df: mezclarIndices(
        [df.Cl_pop, df.Q_pop, df.V1_pop, df.V2_pop], 1)[0],
    espec2=lambda df: mezclarIndices([df.Cl_pop, df.Q_pop, df.V1_pop, df.V2_pop], 1)[1]
)

# Unir iteraciones con los datos 
data1 = pd.merge(data, df_indice, how = 'left', on = 'iteracion')
del([data, df_indice])

# Crear gráfico Plotly
data1.columns

# fig = go.Figure()

# fig.add_trace(go.Scatter(
#     x=data1.loc[data1['iteracion'] == 1, ].loc[:, 'iteration'],
#     y=data1.loc[data1['iteracion'] == 1, ].loc[:, 'convergenceIndicator'],
#     text=data1.loc[data1['iteracion'] == 1, ].loc[:, 'espec2'],
#     hovertemplate='Iteración: %{x} <br> Indicador: %{y:.1f} <br> Ini (Cl, Q, V1, V2): %{text}',
#     showlegend=False
# ))

linePlot1 = px.line(data_frame = data1, x = 'iteration', y = 'convergenceIndicator', line_group="iteracion", 
  custom_data = ['espec2']) 

for serie in linePlot1['data']:
  serie['hovertemplate'] = 'Iteración: %{x} <br> Indicador: %{y:.1f} <br> Ini (Cl, Q, V1, V2): %{customdata[0]}'

param_list = ['Cl_pop_x', 'Q_pop_x', 'V1_pop_x', 'V2_pop_x', 'a1', 'a2', 'beta_Cl_logtWTKG',
  'beta_Cl_tCLCRMLMIN', 'corr_V2_V1', 'omega_Cl', 'omega_Q', 'omega_V1',
  'omega_V2', 'convergenceIndicator']

button_dict = []

for i in param_list:
  button_dict.append(
      dict(
          # args=[{"y": [round(i, 3) for i in data1.loc[:, i].values.tolist()]}],
          args=[{
              "x": [data1['iteration']],
              "y": [data1[i]],
              "linegroup": [data1['iteracion']],
              "customdata": [data1['espec2']]
          }],
          # args=[{"y":i}],
          label=i,
          method="update"))

updateMenu = [{
  'buttons': button_dict,
  'direction': "down",
  'pad': {"r": 10, "t": 10},
  'showactive': True,
  'x': 0.1,
  'xanchor': "left",
  'y': 1.1,
  'yanchor': "top"
}]

  
linePlot1 = linePlot1.update_layout(xaxis={'title': 'Iteración'}, updatemenus=updateMenu)

# linePlot1.show()
linePlot1.write_html("./figures/08_indicadoresConvergencia.html")

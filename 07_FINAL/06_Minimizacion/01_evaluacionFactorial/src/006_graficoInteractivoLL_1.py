# Carga de Paquetes
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import sys
import plotly.express as px
import plotly.graph_objects as go  # or plotly.express as px

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


def mezclarIndices(lista=[], nround=2):
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
  lista2 = [['L' if i == min(K) else 'H' if i == max(
      K) else 'M' for i in K] for K in lista]

  lista1a = ['-'.join(i) for i in np.transpose(lista1)]
  lista2a = ['-'.join(i) for i in np.transpose(lista2)]

  return [lista1a, lista2a]


df_indice = df_indice.assign(
    espec1=lambda df: mezclarIndices(
        [df.Cl_pop, df.Q_pop, df.V1_pop, df.V2_pop], 1)[0],
    espec2=lambda df: mezclarIndices(
        [df.Cl_pop, df.Q_pop, df.V1_pop, df.V2_pop], 1)[1]
)

# Unir iteraciones con los datos
data1 = pd.merge(data, df_indice, how='left', on='iteracion')
del([data, df_indice])

# Convertir a formato largo
data2 = pd.melt(data1, id_vars=[
                'iteration', 'iteracion', 'phase', 'espec1', 'espec2'], var_name='parameter')
data3 = data2.loc[data2['iteration'] %
                  5 == 0, ]  # Conservar cada 100 iteraciones

df = data3

fig = go.Figure()

param_list = ['Cl_pop_x', 'Q_pop_x', 'V1_pop_x', 'V2_pop_x', 'a1', 'a2',
              'beta_Cl_logtCLCRMLMIN', 'corr_V2_V1', 'omega_Cl', 'omega_Q', 'omega_V1',
              'omega_V2', 'convergenceIndicator']
param_name = ['Cl (L/h)', 'Q (L/h)', 'V1 (L)', 'V2 (L)', 'a1', 'a2', 
              u"\u03B2" + ' Cl-logtClCr', u"\u03C1" + ' V1-V2', 
              u"\u03C9" + ' Cl', u"\u03C9" + ' Q', u"\u03C9" + ' V1', u"\u03C9" + ' V2', 
              'Indicador de Convergencia']

n_lines = np.unique(df.iteracion)

trace = []

for k, param in enumerate(param_list):
  df1 = df.loc[df.parameter == param, :]
  traceList = []

  for i, var in enumerate(n_lines):
    df2 = df1.loc[df['iteracion'] == var, :]

    trace1 = fig.add_trace(go.Scatter(
        x=df2.loc[:, 'iteration'],
        y=df2.loc[:, 'value'],
        mode='lines',
        line_color=px.colors.qualitative.Dark24[k],
        customdata=df2.loc[:, 'espec2'],
        name='ID{}'.format(i),
        hovertemplate='Iteración: %{x} <br> Indicador: %{y:.1f} <br> Ini (Cl, Q, V1, V2): %{customdata}',
        connectgaps=True,
        showlegend=False,
        visible=True if param == 'Cl_pop_x' else False,
        opacity=0.2
    ))

    traceList.append(trace1)
  trace.append(traceList)

# Botón


def elegirLineas(parametro):
  return [True if(param == parametro) else False for k, param in enumerate(
      param_list) for i, _ in enumerate(n_lines)]


button_dict = []

for i, var in enumerate(param_list):
  button_dict.append(
      dict(
          args=[{"visible": elegirLineas(var)},
                {"yaxis": {'title': param_name[i]}}],
          label = param_name[i],
          method="update"))

updateMenu = [
    dict(
        buttons=button_dict,
        direction="down",
        pad={"r": 10, "t": 10, "b": 20},
        showactive=True,
        x=0.1,
        xanchor="left",
        y=1.1,
        yanchor="top"
    ),
    dict(
        buttons=list([
            {'args': [
                {'template': 'plotly_dark'}
                # {'plot_bgcolor': 'rgb(10,10,10)',
                #  'paper_bgcolor': 'rgb(0,0,0)', 
                #  'xaxis': {'color': 'white'},
                #  'yaxis': {'color': 'white'}},
            ],
                'label': 'Modo oscuro', 'method': 'relayout'},
            {'args': [
                {'template': 'simple_white'},
                # {'plot_bgcolor': 'rgb(255,255,255)',
                #  'paper_bgcolor': 'rgb(255,255,255)', 
                #  'xaxis': {'color': 'black'},
                #  'yaxis': {'color': 'black'}},
            ],
                'label': 'Modo claro', 'method': 'relayout'}
        ]),
        direction="down",
        pad={"r": 10, "t": 10, "b": 20},
        showactive=True,
        x=0.4,
        xanchor="left",
        y=1.1,
        yanchor="top"
    ),
]

# Menu de actualización 2

fig.update_layout(
    xaxis = {'title': 'Iteración'},
    yaxis = {'title': 'Cl (L/h)'},
    template = 'plotly_dark',
    hovermode = 'closest',
    updatemenus = updateMenu)

# fig.show()

fig.write_html("./figures/08_indicadoresConvergencia.html")

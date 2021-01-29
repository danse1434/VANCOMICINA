import random
import matplotlib.pyplot   as plt
from matplotlib.patches    import FancyArrowPatch
from mpl_toolkits.mplot3d  import proj3d

import pandas              as pd
import numpy               as np
from sklearn.decomposition import PCA
from adjustText            import adjust_text
import plotly.graph_objects as go

# Función Dodge
def puntoDodge(x, li, ls, sd = 0.5):
  """

  """
  y = li - 10
  while (y < li) or (y > ls):
    y = random.normalvariate(x, sd)
  return y

def generarBiplot(data, labels, which=(1,2)):
  """
  
  """
  pca = PCA(n_components=np.max(which)).fit(data)
  
  # datos transformados
  x_s0 = pca.fit_transform(data)[:, which[0]-1]
  x_s1 = pca.fit_transform(data)[:, which[1]-1]
  # determinación de ratio de varianza y vectores de componentes
  var_ratio  = pca.explained_variance_ratio_
  components = pca.components_ * 3

  # determinación de valores atípicos
  d = np.sqrt((x_s0 - np.mean(x_s0))**2 + (x_s1 - np.mean(x_s1))**2)
  d_cor = d - np.mean(d) > 3 * np.std(d) # Indicador de influencia
  cList = list( map(lambda x: 'red' if x else 'blue', d_cor)) # Color de acuerdo a indicador
    
  plt.scatter(x=x_s0, y=x_s1, c=cList, marker = '.', alpha = 0.5)
  ncomp = components.shape[1]
  
  comp_arrow = [plt.arrow(0,0, components[0,i], components[1,i], color='r', alpha=0.5) for i in range(ncomp)]
  comp_text  = [plt.text(components[0,i]*1.15, 
                         components[1,i]*1.15, 
                         list(data.columns)[i], 
                         color='g', ha='center', va='center', size=8) for i in range(ncomp)]

  textos = [ plt.text(x_s0[i], x_s1[i], txt, size=7) for i, txt in enumerate(labels) if d_cor[i]]
  adjust_text(textos,    force_text=(0.2,1.0))
  adjust_text(comp_text, force_text=(0.2,1.0))

  plt.xlabel('PC{m} ({n:.2f}%)'.format(m= which[0], n=var_ratio[which[0]-1]*100) )
  plt.ylabel('PC{m} ({n:.2f}%)'.format(m= which[1], n=var_ratio[which[1]-1]*100) )
   
  return(0)



class Arrow3D(FancyArrowPatch):
  def __init__(self, xs, ys, zs, *args, **kwargs):
    FancyArrowPatch.__init__(self, (0,0), (0,0), *args, **kwargs)
    self._verts3d = xs, ys, zs

  def draw(self, renderer):
    xs3d, ys3d, zs3d = self._verts3d
    xs, ys, zs = proj3d.proj_transform(xs3d, ys3d, zs3d, renderer.M)
    self.set_positions((xs[0],ys[0]),(xs[1],ys[1]))
    FancyArrowPatch.draw(self, renderer)



def generarTriplot(data, labels, which=(1,2,3), view_pos=(+17, -61), pos=111, fig=None):
  """
  
  """
  pca = PCA(n_components=np.max(which)).fit(data)
  
  # datos transformados
  x_s0 = pca.fit_transform(data)[:, which[0]-1]
  x_s1 = pca.fit_transform(data)[:, which[1]-1]
  x_s2 = pca.fit_transform(data)[:, which[2]-1]
  # determinación de ratio de varianza y vectores de componentes
  var_ratio  = pca.explained_variance_ratio_
  components = pca.components_ * 3

  # determinación de valores atípicos
  d = np.sqrt((x_s0 - np.mean(x_s0))**2 + (x_s1 - np.mean(x_s1))**2 + (x_s2 - np.mean(x_s2))**2)
  d_cor = d - np.mean(d) > 3 * np.std(d) # Indicador de influencia
  cList = list( map(lambda x: 'red' if x else 'blue', d_cor)) # Color de acuerdo a indicador
  
  if fig == None:
    fig = plt.figure()
  
  ax = fig.add_subplot(pos, projection='3d')
  ax.scatter(x_s0, x_s1, x_s2, marker = 'o', c = cList, alpha = 0.5)

  comp_vec = []

  for i in range( components.shape[1] ):
    # ax.plot([0, components[0,i]], [0, components[1,i]], [0, components[2,i]], color='r', alpha=0.5)
    a = Arrow3D([0, components[0,i]], [0, components[1,i]], [0, components[2,i]], mutation_scale=20, lw=0.1, arrowstyle="-|>", color="r")
    ax.add_artist(a)
    
    comp_vec.append(ax.text(
      components[0,i], components[1,i], components[2,i], 
      list(data.columns)[i], color='g', ha='center', va='center', size=6))
  
  adjust_text(comp_vec, force_text=(0.2,1.0))
  # textos = [ plt.text(x_s0[i], x_s1[i], txt, size=7) for i, txt in enumerate(labels) if d_cor[i]]
  # adjust_text(textos, force_text=(0.2,1.0))

  ax_labels = [ 'PC{m} ({n:.2f}%)'.format(m= i, n=var_ratio[i-1]*100) for i in which]
  ax.set(xlabel = ax_labels[0], ylabel = ax_labels[1], zlabel = ax_labels[2])
  ax.view_init(elev=view_pos[0], azim=view_pos[1])

  res_dict = {'x_scatter':x_s0, 'y_scatter':x_s1, 'z_scatter':x_s2, 
  'colorList':cList, 'compVec':comp_vec, 'components':components, 'labels': ax_labels, 'specs': labels}
  return(res_dict)




def generarTriplot_Plotly(x):
  
  # Formato de etiquetas (valores iniciales)
  format1 = ["Configuracion Inicial {0} <br>x: {1:.3f}, y: {2:.3f}, z: {3:.3f}".format(i,j,k,n) 
  for i,j,k,n in zip(x['specs'], x['x_scatter'], x['y_scatter'], x['z_scatter'])]

  # Gráfico de dispersión
  fig = go.Figure(data=[go.Scatter3d(x=x['x_scatter'], y=x['y_scatter'], z=x['z_scatter'],
                        mode='markers', 
                        marker=dict(size=12, color=x['colorList'], opacity=0.8),
                        hovertemplate = format1
                        )],
                layout = go.Layout(scene=go.Scene(
                  xaxis=go.XAxis(title=x['labels'][0]),
                  yaxis=go.YAxis(title=x['labels'][1]),
                  zaxis=go.ZAxis(title=x['labels'][2])
                  )))

  # Vectores de parámetros
  for i in range(9):
    fig.add_trace(
      go.Scatter3d(
        x = [0, x['components'][0][i]*5], y = [0, x['components'][1][i]*5], z = [0, x['components'][2][i]*5],
        mode = 'lines', line={'color':'green', 'width':5}, 
        name=x['compVec'][i].get_text(),
        text=[x['compVec'][i].get_text()],
        hovertemplate = "{}".format( x['compVec'][i].get_text() )
      )
    )
  ax_lim = {'nticks':11, 'range':(-11,+11)}
  fig.update_layout(
    title_text='Análisis de Covergencia - PCA',
    scene = {'xaxis':ax_lim, 'yaxis':ax_lim, 'zaxis':ax_lim, 
    'aspectmode':'cube', 'aspectratio':{'x':1, 'y':1.5, 'z':1.5}}
    # aspectratio = dict( x=1, y=1, z=1 )
  )
  return(fig)

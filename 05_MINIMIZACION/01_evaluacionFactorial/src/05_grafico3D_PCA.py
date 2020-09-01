import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import random

data = pd.read_csv('results/02_PCA.csv')

x,y,z = [ data.iloc[:, i] for i in [1,2,3] ]

cList = ['red' if x[i] < -4 else 'blue' for i in range(len(x))]

fig = plt.figure( figsize = (12*2, 12*2), dpi = 1000 )

# función de dodge
def puntoDodge(x, li, ls, sd = 0.5):
  y = li - 10
  while (y < li) or (y > ls):
    y = random.normalvariate(x, sd)
  return y

# Posicionamiento de etiquetas
xmap = x.map(lambda i: i + 5)
ymap = y.map(lambda i: puntoDodge(i, -5, 4))

# Asignar el subplot
def asignarGraf(pos, x, y, xlab, ylab):
  ax = fig.add_subplot(2, 2, pos)
  ax.scatter(x = x, y = y, c = cList, marker = '.', alpha = 0.5)
  ax.set(xlabel = xlab, ylabel = ylab)
  ax.set(xlim = (-12, 12), ylim = (-6, +6))
  
  for i, txt in enumerate(data.loc[:,'label']):
    if x[i] < -4:
      ax.annotate( txt, (x[i], y[i]), xytext = (xmap[i], ymap[i]), size=7,
      arrowprops=dict(arrowstyle="->",
                            connectionstyle="angle3,angleA=0,angleB=-90"))
  return ax



asignarGraf(1, x, y, 'PC 1', 'PC 2')
asignarGraf(2, x, z, 'PC 1', 'PC 3')
asignarGraf(3, y, z, 'PC 2', 'PC 3')

# Gráfico 3D
ax = fig.add_subplot(2, 2, 4, projection='3d')
ax.scatter(xs = x, ys = y, zs = z, marker = 'o', c = cList, alpha = 0.5)
ax.set(xlabel = 'PCA1', ylabel = 'PCA2', zlabel = 'PCA3')
ax.view_init(elev=20., azim=+50)

fig.set_tight_layout(True) # plt.tight_layout() # evita la aglomeración de labels

# plt.rcParams["figure.figsize"] = [12*2,12*2]

plt.show()

fig.savefig('figures/04_resultadosPCA.pdf', format = 'pdf', dpi = 1000)
fig.savefig('figures/04_resultadosPCA.png', format = 'png', dpi = 1000)

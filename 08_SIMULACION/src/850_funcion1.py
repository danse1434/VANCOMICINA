import os
from pickle import dump
import matplotlib.pyplot as plt

def guardarMatplotlib(fig, fname, path=os.path.join('..', 'figures'), incluirPNG=False, bbox_inches=None, pad_inches=0.1,
                      facecolor='w', edgecolor='w', orientation='portrait'):
    """
    Guarda una figura de matplotlib como imágen PNG y PDF, y como JSON

    Parámetros:
    ----------- 
    fig: matplotlib.figure
        Figura a guardar
    file: str
        Nombre del archivo
    path: str
        Ruta donde guardarlo
    incluirPNG: bool
        Indica si incluir el formato PNG al guardar, por defecto sólo se imprime PDF
    bbox_inches: str
        Parámetro de bbox_inches para el PDF
    pad_inches: float
        Espacio extra para alinear los ejes en el PDF
    facecolor: str
        Color de fondo de la imagen
    edgecolor: str
        Color de borde de la imagen
    orientation: str
        Orientación de la imagen
    """
    # Almacenamiento pdf
    plt.savefig(os.path.join(path, fname + '.pdf'),
                facecolor=facecolor, edgecolor=edgecolor, orientation=orientation,
                bbox_inches=bbox_inches, pad_inches=pad_inches)
    if incluirPNG:
        plt.savefig(os.path.join(path, fname + '.png'))

    # Almacenamiento serializado
    file = open(os.path.join(path, fname), 'wb')
    dump(fig, file)
    file.close()
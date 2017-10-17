import sys
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation

from bauer import kleene_tree

fig = plt.figure()
ax = fig.add_subplot(111)
ax.axis('off')

def get_height(length):
    if length < 0:
        return 0
    return - (length) / (length + 1) ** 0.5

def plot_segment(seg, px):
    if seg[-1] == 0:
        color = 'blue'
        qx = px - 1 / (2 ** len(seg))
    else:
        color = 'red'
        qx = px + 1 / (2 ** len(seg))
    py = get_height(len(seg) - 1)
    qy = get_height(len(seg))
    ax.plot((px, qx), (py, qy), color=color, linewidth=(-1/qy)**0.5)
    return qx


tree = kleene_tree()

x_positions = {() : 0}

def draw():
    write_layer = 10
    while True:
        node = next(tree)

        if len(node) >= write_layer:
            filename = 'bauer{}.svg'.format(len(node))
            fig.savefig(filename)
            print("Saved", filename)
            write_layer = len(node) + 1

        x_positions[node] = plot_segment(node, x_positions[node[:-1]])

draw()

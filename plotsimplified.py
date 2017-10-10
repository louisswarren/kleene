import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation

from kleene import kleene_cuts as kleene

def plot_line(px, py, qx, qy, color='black', linestyle='solid'):
    ax.plot((px, qx), (py, qy), color=color,
            linewidth=(-1/qy)**0.5, linestyle=linestyle)

plotted = set()
def plot_cut_args(cut, subpath=False):
    if cut in plotted:
        return
    plotted.add(cut)
    py = (1 - len(cut)) / len(cut) ** 0.5
    px = 0
    for i in range(1, len(cut)):
        yield from plot_cut_args(cut[:i], subpath=True)
        if cut[i-1] == '0':
            px -= 1 / (2 ** i)
        else:
            px += 1 / (2 ** i)
    qy = - len(cut) / (len(cut) + 1) ** 0.5
    if cut[-1] == '0':
        qx = px - 1 / (2 ** len(cut))
        if subpath:
            color = 'blue'
            linestyle = 'solid'
        else:
            color = 'darkblue'
            linestyle = 'dotted'
    else:
        qx = px + 1 / (2 ** len(cut))
        if subpath:
            color = 'red'
            linestyle = 'solid'
        else:
            color = 'brown'
            linestyle = 'dotted'
    yield px, py, qx, qy, color, linestyle


tree = kleene()

def update(framenum):
    cut = next(tree)
    for args in plot_cut_args(cut):
        plot_line(*args)

fig = plt.figure()
ax = fig.add_subplot(111)
ax.axis('off')

_ = FuncAnimation(fig, update, interval=1)
plt.show()


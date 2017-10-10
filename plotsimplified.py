import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation

from kleene import kleene_cuts as kleene

def plot_line(px, py, qx, qy, color):
    ax.plot((px, qx), (py, qy), color=color, linewidth=(-1/qy)**0.5)

plotted = set()
def plot_path(cut):
    if cut in plotted:
        return
    plotted.add(cut)
    py = (1 - len(cut)) / len(cut) ** 0.5
    px = 0
    for i in range(1, len(cut)):
        yield from plot_path(cut[:i])
        if cut[i-1] == '0':
            px -= 1 / (2 ** i)
        else:
            px += 1 / (2 ** i)
    qy = - len(cut) / (len(cut) + 1) ** 0.5
    if cut[-1] == '0':
        qx = px - 1 / (2 ** len(cut))
        color = 'blue'
    else:
        qx = px + 1 / (2 ** len(cut))
        color = 'red'
    yield px, py, qx, qy, color


tree = kleene()

def update(framenum):
    cut = next(tree)
    for args in plot_path(cut[:-1]):
        plot_line(*args)

def update_cached(framenum):
    for args in plot_path(cached_cuts[framenum][:-1]):
        plot_line(*args)

fig = plt.figure()
ax = fig.add_subplot(111)
ax.axis('off')

_ = FuncAnimation(fig, update, interval=1)
plt.show()


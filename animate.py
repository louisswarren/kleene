import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation

from cuts import cuts as cached_cuts
from kleene import kleene

def plot_line(px, py, qx, qy, color):
    ax.plot((px, qx), (py, qy), color=color, linewidth=1)

plotted = set()
def plot_path(cut):
    if cut in plotted:
        return
    plotted.add(cut)
    py = 1 - len(cut)
    px = 0
    for i in range(1, len(cut)):
        yield from plot_path(cut[:i])
        if cut[i-1] == '0':
            px -= 1 / (2 ** i)
        else:
            px += 1 / (2 ** i)
    qy = py - 1
    if cut[-1] == '0':
        qx = px - 1 / (2 ** len(cut))
        color = 'blue'
    else:
        qx = px + 1 / (2 ** len(cut))
        color = 'red'
    yield px, py, qx, qy, color


tree = kleene()

def update(framenum):
    cut, _, _ = next(tree)
    for args in plot_path(cut[:-1]):
        plot_line(*args)

def update_cached(framenum):
    for args in plot_path(cached_cuts[framenum][:-1]):
        plot_line(*args)

fig = plt.figure()
ax = fig.add_subplot(111)
_ = FuncAnimation(fig, update_cached, interval=1, frames=len(cached_cuts))
#_ = FuncAnimation(fig, update, interval=1)
plt.show()


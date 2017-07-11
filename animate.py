import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation

from cuts import cuts

def plot_line(px, py, qx, qy, color, linewidth=2):
    ax.plot((px, qx), (py, qy), color=color, linewidth=linewidth)

plotted = set()
def plot_path(cut, recursed=False):
    if cut in plotted:
        return
    plotted.add(cut)
    py = 1 - len(cut)
    px = 0
    for i in range(1, len(cut)):
        yield from plot_path(cut[:i], True)
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
    if recursed:
        linewidth = 1
    else:
        color = 'black'
        linewidth = 0.5
    yield px, py, qx, qy, color, linewidth

def plot_path_no_tip(cut, recursed=False):
    if cut in plotted:
        return
    plotted.add(cut)
    py = 1 - len(cut)
    px = 0
    for i in range(1, len(cut)):
        yield from plot_path_no_tip(cut[:i], True)
        if cut[i-1] == '0':
            px -= 1 / (2 ** i)
        else:
            px += 1 / (2 ** i)
    if not recursed:
        return
    qy = py - 1
    if cut[-1] == '0':
        qx = px - 1 / (2 ** len(cut))
        color = 'blue'
    else:
        qx = px + 1 / (2 ** len(cut))
        color = 'red'
    yield px, py, qx, qy, color, 1

def update(framenum):
    for args in plot_path_no_tip(cuts[framenum]):
        plot_line(*args)


fig = plt.figure()
ax = fig.add_subplot(111)
animation = FuncAnimation(fig, update, interval=1, frames=100)
plt.show()


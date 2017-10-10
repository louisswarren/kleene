import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation

from kleene import kleene_cuts, CutContainer

fig = plt.figure()
ax = fig.add_subplot(111)
ax.axis('off')

def get_height(length):
    if length < 0:
        return 0
    return - (length) / (length + 1) ** 0.5

def plot_segment(seg, px):
    if seg[-1] == '0':
        color = 'blue'
        qx = px - 1 / (2 ** len(seg))
    else:
        color = 'red'
        qx = px + 1 / (2 ** len(seg))
    py = get_height(len(seg) - 1)
    qy = get_height(len(seg))
    ax.plot((px, qx), (py, qy), color=color, linewidth=(-1/qy)**0.5)
    return qx


cuts = CutContainer()


last_layer = [('0', plot_segment('0', 0)), ('1', plot_segment('1', 0))]

def update(framenum):
    layer = []
    initial, px = last_layer.pop(0)
    for end in '0', '1':
        seg = initial + end
        if seg in cuts:
            continue
        last_layer.append((seg, plot_segment(seg, px)))



_ = FuncAnimation(fig, update, interval=1)
plt.show()


import matplotlib.pyplot as plt
import matplotlib.lines as lines

from cuts import cuts

compose = lambda f: lambda g: lambda *a, **k: f(g(*a, **k))


fig = plt.figure()
ax = fig.add_subplot(111)

plotopts = {'linewidth':0.5}

@compose(tuple)
def trimcuts(t, cuts):
    for cut in cuts:
        if cut[0] == t:
            if len(cut) > 1:
                yield cut[1:]


def plotnext(px, py, plevel, cuts=(), max_level=5, removed=False):
    if plevel > max_level:
        return
    sx = 1/(2**plevel)
    sy = 1/plevel
    qy = py - sy
    for val, shift in (('0', -sx), ('1', sx)):
        qx = px + shift
        if not removed and val not in cuts:
            ax.plot((px, qx), (py, qy), color='black', **plotopts)
            plotnext(qx, qy, plevel + 1, trimcuts(val, cuts), max_level)
        else:
            ax.plot((px, qx), (py, qy), color='red', **plotopts)
            plotnext(qx, qy, plevel + 1, trimcuts(val, cuts), max_level, True)



plotnext(0, 0, 1, cuts, 16)
plt.show()


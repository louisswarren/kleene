import matplotlib.pyplot as plt

from cuts import cuts

compose = lambda f: lambda g: lambda *a, **k: f(g(*a, **k))


fig = plt.figure()
ax = fig.add_subplot(111)

@compose(tuple)
def trimcuts(t, cuts):
    for cut in cuts:
        if cut[0] == t:
            if len(cut) > 1:
                yield cut[1:]

plotopts = {'linewidth':0.5}

def plotnext(px, py, plevel, cuts=(), max_level=5, removed=False):
    if plevel > max_level:
        return
    sx = 1/(2**plevel)
    qy = py - 1
    if plevel < 10:
        print('\t' * plevel + 'Level' + str(plevel) + '...')
    for val, shift in (('0', -sx), ('1', sx)):
        qx = px + shift
        if not removed and val not in cuts:
            ax.plot((px, qx), (py, qy), color='black', **plotopts)
            plotnext(qx, qy, plevel + 1, trimcuts(val, cuts), max_level)
        else:
            ax.plot((px, qx), (py, qy), color='red', **plotopts)
            plotnext(qx, qy, plevel + 1, trimcuts(val, cuts), max_level, True)
    if plevel < 10:
        print('\t' * plevel + 'Level' + str(plevel) + 'COMPLETE')

def plotcuts(px, py, plevel, cuts=(), max_level=5):
    if plevel > max_level:
        return
    sx = 1/(2**plevel)
    qy = py - 1
    r = False
    if plevel < 16:
        print('\t' * plevel + 'Level', str(plevel))
    for val, shift, color in (('0', -sx, 'blue'), ('1', sx, 'red')):
        qx = px + shift
        if val not in cuts:
            c = plotcuts(qx, qy, plevel + 1, trimcuts(val, cuts), max_level)
            if c:
                ax.plot((px, qx), (py, qy), color=color, linewidth=2)
                r = True
        else:
            ax.plot((px, qx), (py, qy), color='grey', **plotopts)
            r = True
    return r


#plotnext(0, 0, 1, cuts, 10)
plotcuts(0, 0, 1, cuts, 6)
plt.show()

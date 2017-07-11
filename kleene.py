from collections import defaultdict

from tm import *

def halting_tm(instructions, max_runtime=100):
    tape_history = set()
    tape, p, t = Tape(), 0, 0
    jumps = jump_table(instructions)
    prognum = int('0' + instructions, 6)
    while p < len(instructions):
        x = instructions[p]
        if   x == I.OutputVal: yield tape[t]
        elif x == I.ToggleVal: tape[t] = 1 - tape[t]
        elif x == I.MoveFward: t += 1
        elif x == I.MoveBward: t = t - 1 if t else 0
        elif x == I.JmpFIfNot: p = jumps[p] if tape[t] == 0 else p
        elif x == I.JmpBIfSet: p = jumps[p] if tape[t] == 1 else p
        else: raise NotImplementedError(x)
        if x == I.OutputVal:
            tape_history = set()
        else:
            yield
        p += 1
        if (p, t, str(tape)) in tape_history or \
                len(tape_history) > prognum * max_runtime or \
                len(tape) > max_runtime:
            break
        else:
            tape_history.add((p, t, str(tape)))


def simulate():
    machines, outputs = {}, defaultdict(lambda: '')
    try:
        n = 0
        while True:
            machines[n] = tm(int_to_base6(n))
            completed = []
            for m in list(machines.keys()):
                try:
                    v = next(machines[m])
                    if v is not None:
                        outputs[m] += str(v)
                except StopIteration:
                    yield m, outputs[m]
                    del machines[m]
                    if m in outputs:
                        del outputs[m]
            n += 1
    except KeyboardInterrupt:
        pass
    finally:
        for m in machines.keys():
            yield m, outputs[m] + '-'


import sys
f = open('log.txt', 'w')

def kleene():
    machines, outputs = {}, defaultdict(lambda: '')
    cuts = set()
    cut_depth = 2
    n = 0
    fmt = "Iter number: {:<16} Number of machines: {:<16}"
    while True:
        machines[n] = halting_tm(int_to_base6(n))
        completed = []
        for m in list(machines.keys()):
            if any(outputs[m].startswith(cut) for cut in cuts):
                del machines[m]
                if m in outputs:
                    del outputs[m]
                continue
            try:
                v = next(machines[m])
                if v is not None:
                    outputs[m] += str(v)
                if len(outputs[m]) >= cut_depth:
                    cut = outputs[m][:cut_depth]
                    cuts.add(cut)
                    yield cut, m, n
                    del machines[m]
                    del outputs[m]
                    cut_depth += 1
            except StopIteration:
                del machines[m]
                if m in outputs:
                    del outputs[m]
        n += 1
        if not n % 100:
            print(fmt.format(n, len(machines)), file=sys.stderr)
            print(fmt.format(n, len(machines)), file=f)
            f.flush()

if __name__ == '__main__':
    for cut in kleene():
        print("Cut {:<16} from machine {:<32} after {:<16} iterations".format(*cut))
        sys.stdout.flush()

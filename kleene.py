# Function composition

from collections import defaultdict

compose = lambda f: lambda g: lambda *a, **k: f(g(*a, **k))

class I:
    '''Instruction set for turing machine.'''
    OutputVal = '0'            # Output current cell
    ToggleVal = '1'            # Toggle current cell
    MoveFward = '2'            # Move tape pointer forward
    MoveBward = '3'            # Move tape pointer backward
    JmpFIfNot = '4'            # Jump forward to matching '5' if cell is 0
    JmpBIfOne = '5'            # Jump backward to matching '4' if cell is 1
    # Optimally, each JmpFIfNot would match a JmpBIfOne (allowing for nesting).
    # On a mismatch, the jump target is the beginning of the program instead.


class Tape(list):
    '''Automatically expanding tape.'''
    def _expand(self, n):
        self += [0] * (n - len(self))

    def __getitem__(self, i):
        self._expand(i + 1)
        return super().__getitem__(i)

    def __setitem__(self, i, x):
        self._expand(i + 1)
        return super().__setitem__(i, x)


@compose(dict)
def build_jump_table(instrs):
    '''Precompute jump targets.'''
    jump_stack = []
    for ctr, instr in enumerate(instrs):
        if instr == I.JmpFIfNot:
            jump_stack.append(ctr)
        elif instr == I.JmpBIfOne:
            if jump_stack:
                s = jump_stack.pop()
                yield s, ctr
                yield ctr, s
            else:
                yield ctr, 0
    for ctr in jump_stack:
        yield ctr, 0


def transition(instr, jumps, tape, ptr, ctr):
    '''Perform a single state turing machine state transition.'''
    output = None
    if   instr == I.OutputVal: output    = tape[ptr]
    elif instr == I.ToggleVal: tape[ptr] = 1 - tape[ptr]
    elif instr == I.MoveFward: ptr       = ptr + 1
    elif instr == I.MoveBward: ptr       = ptr - 1 if ptr else 0
    elif instr == I.JmpFIfNot: ctr       = jumps[ctr] if tape[ptr] == 0 else ctr
    elif instr == I.JmpBIfOne: ctr       = jumps[ctr] if tape[ptr] == 1 else ctr
    else: raise NotImplementedError(repr(instr))
    ctr += 1
    return output, tape, ptr, ctr


def htm(instrs, max_runfactor=1):
    '''Co-routine for an automatically halting machine.

    Not a real turing machine, as it halts after a too-long runtime or infinite
    non-outputting loop. The number of transitions between outputs is bounded
    proportionally to the program length. Higher max_runfactor makes
    diagonalisation slower.  Lower max_runfactor could limit machine
    complexity, preventing the search for new cuts. In practice, a value of at
    least one appears to work.'''
    state_history = set()
    max_runtime = int(len(instrs) * max_runfactor)
    tape, ptr, ctr = Tape(), 0, 0
    jumps = build_jump_table(instrs)
    while ctr < len(instrs):
        output, tape, ptr, ctr = transition(instrs[ctr], jumps, tape, ptr, ctr)
        yield output
        if output is not None:
            state_history = set()
        state = (str(tape), ptr, ctr)
        if state in state_history or len(state_history) > max_runtime:
            return
        else:
            state_history.add(state)

def get_prog(index):
    '''Get the program for a given index.'''
    prog = ''
    while index:
        prog = str(index % 6) + prog
        index //= 6
    return prog

def diagonal_run():
    '''Run all machines (diagonalised) giving their outputs.

    Stops displaying outputs of halted machines.'''
    n = 0
    machines = {}
    outputs = defaultdict(lambda: '')
    while True:
        for i in reversed(range(n)):
            if i in machines:
                try:
                    output = next(machines[i])
                    if output is not None:
                        outputs[i] += str(output)
                except StopIteration:
                    del machines[i]
                if i in outputs:
                    print('{:>8}: {}'.format(i, outputs[i]))
        machines[n] = htm(get_prog(n))
        n += 1
        print()

def kleene_cuts():
    n = 0
    cut_depth = 1
    cuts = {}
    machines = {}
    outputs = defaultdict(lambda: '')
    while True:
        for i in list(machines.keys()):
            try:
                output = next(machines[i])
            except StopIteration:
                del machines[i]
                if i in outputs:
                    del outputs[i]
                continue
            if output is not None:
                outputs[i] += str(output)
                # Check if this machine has reached a cut
                if outputs[i] == cuts.get(len(outputs[i])):
                    del machines[i]
                    del outputs[i]
                    continue
            if i in outputs and len(outputs[i]) == cut_depth + 1:
                cut_depth += 1
                cuts[cut_depth] = outputs[i]
                yield cuts[cut_depth]
                # Cut all running machines that start with this cut
                for j in list(machines):
                    if outputs[j].startswith(cuts[cut_depth]):
                        del machines[j]
                        del outputs[i]
        machines[n] = htm(get_prog(n))
        n += 1

class CutContainer:
    '''Container for deciding if a given segment is in the cut enumeration.'''
    def __init__(self):
        self.cuts = kleene_cuts()
        self.found = set()

    def __contains__(self, seg):
        while len(self.found) + 1 < len(seg):
            self.found.add(next(self.cuts))
        return seg in self.found

def is_in_tree(a):
    '''Determine if a given finite sequence is in the Kleene tree.

    This function is computable, so the tree is computable.'''
    cuts = kleene_cuts()
    for cut, _ in cuts:
        if a.startswith(cut):
            return False
        if len(cut) > len(a):
            return True

if __name__ == '__main__':
    for k in kleene_cuts():
        print(k)



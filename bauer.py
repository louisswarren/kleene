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

@compose(reversed)
@compose(tuple)
def to_base(n, b):
    while n:
        yield n % b
        n //= b

def get_prog(index):
    '''Get the program for a given index.'''
    return ''.join(map(str, to_base(index, 5)))


def computable_function(n, m):
    '''Co-routine for the nth computable function, on imput m.'''
    instrs = get_prog(n)
    if I.OutputVal not in instrs: return
    tape, ptr, ctr = Tape(to_base(m, 2)), 0, 0
    jumps = build_jump_table(instrs)
    while ctr < len(instrs):
        output, tape, ptr, ctr = transition(instrs[ctr], jumps, tape, ptr, ctr)
        if output is not None:
            yield int(output)
            return
        else:
            yield output

def nontotal_function(n):
    '''Computable partial function which differs from every total function.'''
    for step in computable_function(n, n):
        if step is None:
            yield
        else:
            yield 1 - step
            return

def approximate_nontotal_function(n, k):
    '''Total bounded function which approximates the nontotal function.'''
    for _, output in zip(range(k), nontotal_function(n)):
        if output is not None:
            return output
    return 'abort'

mem = {}
def memoized_approx(n, k):
    if n not in mem:
        r = approximate_nontotal_function(n, k)
        if r == 'abort':
            return r
        mem[n] = r
    return mem[n]


def kleene_tree():
    last_layer = [()]
    while True:
        layer = []
        for parent in last_layer:
            for node in (parent + (0,), parent + (1,)):
                if all(memoized_approx(k + 1, len(node)) in ('abort', node[k]) for k in range(len(node))):
                    layer.append(node)
                    yield node
        last_layer = layer

if __name__ == '__main__':
    for node in kleene_tree():
        print(''.join(map(str, node)))


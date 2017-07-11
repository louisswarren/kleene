'''A simple turing machine model.'''

class I:
    '''Instruction set for turing machine.'''
    OutputVal = '0'            # Output current cell
    ToggleVal = '1'            # Toggle current cell
    MoveFward = '2'            # Move tape pointer forward
    MoveBward = '3'            # Move tape pointer backward
    JmpFIfNot = '4'            # Jump forward to matching '5' if cell is 0
    JmpBIfSet = '5'            # Jump backward to matching '4' if cell is 1
    # Optimally, each JmpFIfNot would match a JmpBIfSet (allowing for nesting).
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

    def print_state(self, t):
        print(''.join(str(v) for v in self))
        print(' ' * t + '^')
        print()


def jump_table(instructions):
    '''Precompute jumps.'''
    jumps, jump_stack = {}, []
    for ptr, instr in enumerate(instructions):
        if instr == I.JmpFIfNot:
            jump_stack.append(ptr)
        elif instr == I.JmpBIfSet:
            if jump_stack:
                s = jump_stack.pop()
                jumps[s], jumps[ptr] = ptr, s
            else:
                jumps[ptr] = 0
    jumps.update({ptr: 0 for ptr in jump_stack})
    return jumps

def tm(instructions):
    '''Co-routine for the turing machine. Yields after each instruction.'''
    tape, p, t = Tape(), 0, 0
    jumps = jump_table(instructions)
    while p < len(instructions):
        x = instructions[p]
        if   x == I.OutputVal: yield tape[t]
        elif x == I.ToggleVal: tape[t] = 1 - tape[t]
        elif x == I.MoveFward: t += 1
        elif x == I.MoveBward: t = t - 1 if t else 0
        elif x == I.JmpFIfNot: p = jumps[p] if tape[t] == 0 else p
        elif x == I.JmpBIfSet: p = jumps[p] if tape[t] == 1 else p
        else: raise NotImplementedError(x)
        if x != I.OutputVal: yield
        p += 1

def run_prog(prog):
    '''Print out the result of running a given turing machine.'''
    for iteration in tm(prog):
        if iteration is not None:
            print(iteration, end='')
    print()

def int_to_base6(n):
    '''Convert a natural number into a valid program for the turing machine.'''
    s = ''
    while n:
        s = str(n % 6) + s
        n //= 6
    return s

def run(n):
    return run_prog(int_to_base6(n))

def example():
    '''Print the members of {0^(4-n) 1^(n + 1) | n = 0..4}'''

    prog = ( '12222213333'    # Put 00001 on the tape, with 1 at the beginning
           + '141'            # End of loop while there are still zeros
             + '1410215'      # Output initial zeros
             + '14025'        # Output remaining ones
             + '343513'       # Go back, set last zero to one
             + '14131512'     # Go back, skipping over zeros
           + '151'            # End of loop while there are still zeros
           + '4025'           # Output remaining ones
           )

    run_prog(prog)

def example2():
    '''Same as example().'''
    run(873132658028094286652476778967260321)

if __name__ == '__main__':
    example()
    example2()


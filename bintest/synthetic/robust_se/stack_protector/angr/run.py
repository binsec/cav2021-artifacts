#!/usr/bin/env python3
import angr
import claripy

project = angr.Project("../a.out", auto_load_libs=False)
initial_state = project.factory.blank_state(addr=0x08049225, add_options={angr.options.SYMBOL_FILL_UNCONSTRAINED_MEMORY, angr.options.SYMBOL_FILL_UNCONSTRAINED_REGISTERS})
sym_input = claripy.BVS("sym_input", 7)
initial_state.mem[0x0804c010].uint32_t = sym_input.zero_extend(25)
simgr = project.factory.simgr(initial_state)


def find(state) -> bool:
    # print(hex(state.addr))
    if state.addr != 0x08049223:
        return False
    print("n=", state.solver.eval(sym_input, cast_to=int))
    print("return address=", state.mem[state.regs.esp+4].uint32_t.resolved)
    # I am unable to find the proper way to check that adding the constraint
    # @[esp+4]==0x61616161 is still satisfiable so let's under approximate in
    # the ugliest possible way
    if "<BV32 0x61616161>" == str(state.mem[state.regs.esp+4].uint32_t.resolved):
        # how gs_base is encoded, I don't know
        print("reached with")
        print("gs=", state.regs.gs)
        print("canary=", state.mem[state.regs.gs + 20].uint32_t.resolved)
        exit(0)
        return True
    print("not yet reached")
    return False


def avoid(state) -> bool:
    return state.addr in {0x804921e, 0x0804923b}


simgr.explore(find=find, avoid=avoid)
print(f"found {len(simgr.found)} states")
# found = simgr.found[0]
# print(found.solver.eval(sym_input, cast_to=int))

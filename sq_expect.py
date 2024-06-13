from pathlib import Path
import subprocess
import sys

from tqdm import tqdm

SQUIRREL_BIN = Path('squirrel/bin/sq')
SCRIPTS = Path('resources/scripts')
OUTPUTS = Path('resources/outputs')
MACRO_FILE = Path('squirrel_lang/src/test_macro.rs')

class Module:
    def __init__(self):
        self.scripts = []
        self.submodules = {}

    def get_submod(self, name):
        if name not in self.submodules:
            self.submodules[name] = Module()
        return self.submodules[name]

    def print_mod(self, mod_name, hier, handle, indent):
        print(f'{pad(indent)}mod {mod_name} {{', file=handle)
        print(f'{pad(indent+1)}use super::*;', file=handle)
        for script in self.scripts:
            script_name = script.split('.', 1)[0]
            script_upper = script_name.upper()
            print(f'{pad(indent+1)}const {script_upper}_PATH: &str = "{hier}/{script}";', file=handle)
            print(f'{pad(indent+1)}const {script_upper}_CONTENTS: &str = include_str!("../../../resources/scripts/{hier}/{script}");', file=handle)
            print(f'{pad(indent+1)}#[test]', file=handle)
            print(f'{pad(indent+1)}fn test_{script_name}() {{', file=handle)
            print(f'{pad(indent+2)}$func({script_upper}_PATH, {script_upper}_CONTENTS);', file=handle)
            print(f'{pad(indent+1)}}}', file=handle)

        for mod_name, mod in self.submodules.items():
            mod.print_mod(mod_name, hier / mod_name, handle, indent+1)

        print(f'{pad(indent)}}}', file=handle)

root_mod = Module()

for script in SCRIPTS.glob('**/*.nut'):
    hier = script.relative_to(SCRIPTS).parent.parts
    output_dir = OUTPUTS
    module = root_mod
    for part in hier:
        module = module.get_submod(part)
        output_dir = output_dir / part
    output_dir.mkdir(parents=True, exist_ok=True)
    module.scripts.append(script.name)
    output = output_dir / f'{script.name}-expect.txt'
    print(script.relative_to(SCRIPTS))
    with output.open('w') as handle:
        res = subprocess.run([SQUIRREL_BIN, str(script)], stdout=handle, stderr=subprocess.PIPE)
        if len(res.stderr) > 0:
            print("Error:")
            print(res.stderr.decode())

def pad(amt):
    return ' ' * amt * 4

with open(MACRO_FILE, 'wt') as handle:
    print('#[macro_export]', file=handle)
    print('macro_rules! test_foreach {', file=handle)
    print('    ($func:tt) => {', file=handle)
    root_mod.print_mod('test', Path(''), handle, 1)
    print('    };', file=handle)
    print('}', file=handle)
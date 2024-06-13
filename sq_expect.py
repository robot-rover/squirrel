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
        print(f'{pad(indent)}#[rustfmt::skip]', file=handle)
        print(f'{pad(indent)}mod {mod_name} {{', file=handle)
        print(f'{pad(indent+1)}use super::*;', file=handle)
        if len(self.scripts) > 0:
            print(f'{pad(indent+1)}use std::fs;', file=handle)
        for script in self.scripts:
            script_name = script.split('.', 1)[0]
            script_upper = script_name.upper()
            print(f'{pad(indent+1)}const {script_upper}_PATH: &str = "{hier}/{script}";', file=handle)
            print(f'{pad(indent+1)}#[test]', file=handle)
            print(f'{pad(indent+1)}fn test_{script_name}() {{', file=handle)
            print(f'{pad(indent+2)}let contents = fs::read_to_string("../resources/scripts/{hier}/{script}").expect("Unable to read script source");', file=handle)
            print(f'{pad(indent+2)}$func({script_upper}_PATH, &contents);', file=handle)
            print(f'{pad(indent+1)}}}', file=handle)

        for mod_name, mod in self.submodules.items():
            mod.print_mod(mod_name, hier / mod_name, handle, indent+1)

        print(f'{pad(indent)}}}', file=handle)

root_mod = Module()

def register_script(script):
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

for dirpath, dirnames, filenames in SCRIPTS.walk(follow_symlinks=True):
    for file in filenames:
        if file.endswith('.nut'):
            register_script(dirpath / file)

def pad(amt):
    return ' ' * amt * 4

with open(MACRO_FILE, 'wt') as handle:
    print('#[macro_export]', file=handle)
    print('macro_rules! test_foreach {', file=handle)
    print('    ($func:tt) => {', file=handle)
    assert len(root_mod.scripts) == 0
    for name, sub_mod in root_mod.submodules.items():
        sub_mod.print_mod(name, Path(name), handle, 2)
    print('    };', file=handle)
    print('}', file=handle)
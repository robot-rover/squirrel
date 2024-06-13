from pathlib import Path
import subprocess

from tqdm import tqdm

SQUIRREL_BIN = Path('squirrel/bin/sq')
SCRIPTS = Path('resources/scripts')
OUTPUTS = Path('resources/outputs')
MACRO_FILE = Path('squirrel_lang/src/test_macro.rs')

test_util_mods = {}


for category in SCRIPTS.iterdir():
    output_dir = OUTPUTS / category.name
    output_dir.mkdir(parents=True, exist_ok=True)
    module = []
    for script in category.iterdir():
        module.append(script.name)
        output = output_dir / f'{script.name}-expect.txt'
        print(script.relative_to(SCRIPTS))
        with output.open('w') as handle:
            res = subprocess.run([SQUIRREL_BIN, str(script)], stdout=handle, stderr=subprocess.PIPE)
            if len(res.stderr) > 0:
                print("Error:")
                print(res.stderr.decode())
    test_util_mods[category.name] = module

with open(MACRO_FILE, 'wt') as handle:
    print('#[macro_export]', file=handle)
    print('macro_rules! test_foreach {', file=handle)
    print('    ($func:tt) => {', file=handle)
    for mod_name, mod in test_util_mods.items():
        print(f'{" " * 8}mod {mod_name} {{', file=handle)
        print(f'{" " * 12}use super::*;', file=handle)
        for script in mod:
            script_name = script.split('.', 1)[0]
            script_upper = script_name.upper()
            print(f'{" " * 12}const {script_upper}_PATH: &str = "{mod_name}/{script}";', file=handle)
            print(f'{" " * 12}const {script_upper}_CONTENTS: &str = include_str!("../../../resources/scripts/{mod_name}/{script}");', file=handle)
            print(f'{" " * 12}#[test]', file=handle)
            print(f'{" " * 12}fn test_{script_name}() {{', file=handle)
            print(f'{" " * 16}$func({script_upper}_PATH, {script_upper}_CONTENTS);', file=handle)
            print(f'{" " * 12}}}', file=handle)
        print(f'{" " * 8}}}', file=handle)
    print('    };', file=handle)
    print('}', file=handle)
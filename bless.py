import os
import re
from pathlib import Path
import shutil
import argparse

root = Path(__file__).parent
resources = root / 'resources'
target = root / 'target' / 'tmp'

parse = argparse.ArgumentParser()
parse.add_argument('test_type', type=str)
parse.add_argument('mod_glob', type=str)
parse.add_argument('tag_glob', type=str)

args = parse.parse_args()

actual_postfix = "-actual.json"
expect_postfix = "-expect.json"
test_type = args.test_type
mod_glob = args.mod_glob
tag_glob = args.tag_glob + actual_postfix

print(f'Test Type: "{test_type}", Mod Glob: "{mod_glob}", Tag Glob: "{tag_glob}"')

to_bless = []
for mod in (target / test_type).glob(mod_glob):
    if not mod.is_dir:
        continue
    for tag in mod.glob(tag_glob):
        to_bless.append(tag)

print('Bless?')
print('\n'.join(str(path.relative_to(root)) for path in to_bless))

ans = input('[y/N]')
if ans in ('y', 'Y'):
    for path in to_bless:
        move_to = resources / path.relative_to(target).parent / path.name.replace(actual_postfix, expect_postfix)
        move_to.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy(path, move_to)
else:
    print("Aborted")



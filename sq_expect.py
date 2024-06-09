from pathlib import Path
import subprocess

from tqdm import tqdm

SQUIRREL_BIN = Path('squirrel/bin/sq')
SAMPLES = Path('squirrel/samples')
OUTPUTS = Path('resources/squirrel/samples')

OUTPUTS.mkdir(parents=True, exist_ok=True)

for example in tqdm(SAMPLES.glob('*.nut')):
    output = OUTPUTS / f'{example.stem}.txt'
    with output.open('w') as handle:
        subprocess.run([SQUIRREL_BIN, example], stdout=handle)
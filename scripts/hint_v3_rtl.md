# RTL Hint v3 workload pipeline

Build `build/emu` from this checkout. The runner reads
`../manifest/test.json` and the five slices in `../workload/cpython-heavy.json`.
It uses the checkpoint's `_memory_.zstd` as `-i`, the manifest's `gcpt_restore`
as `-r`, the audited six-instruction boot flash as `-F`, and the kmhv3-matched
NEMU interpreter shared object as `--diff`. Their SHA256 values and the NEMU
source commit/configuration/build command are in `../results/rtl-v3-nemu/audit.json`.
The NEMU source configuration enables `CONFIG_RV_SDTRIG` and
`CONFIG_DIFFTEST_CHECK_SDTRIG`, giving the 1400-byte difftest state required by
this RTL. The explicit flash is required because this NEMU exports the older
flash-loading API and its built-in three-instruction image differs from the
RTL's six-instruction default.

```sh
python3 scripts/hint_v3_rtl.py --all --jobs 5 --output /absolute/output/rtl-hint --through build
python3 scripts/hint_v3_rtl.py --all --jobs 5 --output /absolute/output/rtl-hint --through hint
```

The first command runs each slice's baseline and independent RTL discovery,
then profiles all top-20 target-miss PCs in disjoint CAM groups of at most 16.
It evaluates masks 0–15 with the shared finite builder and writes a v3 JSON
metadata file, packed image, excluded-PC report, and fixed-confidence estimate.
The second command revalidates every existing run's command, log, source and
binary hashes, then performs actual RTL Hint replay. The default run length is
1M warmup plus 1M measure. `--warmup` and `--measure` permit shorter smoke
runs; such runs are outside the formal report. `--jobs` runs independent slices
in parallel while keeping the required pass order inside each slice.

Each run records NEMU and GCPT helper hashes, the emulator and checkpoint
hashes, actual instruction/cycle counts, IPC, measurement-window Hint counters,
and a difftest log. `replay-report.json` contains observed per-PC corrected and
harmed target predictions from committed RTL events. Its counts are distinct
from `fixed-conf3-estimate.json`, which is only an offline image estimate.

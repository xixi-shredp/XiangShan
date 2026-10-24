# RTL Hint workload smoke pipeline

Run on ps1/ps2. The runner reads `manifest/smoke-test.json` and
`workload/cpython-norvv-uart16550-100m200m-heavy.json` from the project root.
Supply `--deps` with audited `emu`, `checkpoint_root`, `gcpt_restore`,
`boot_flash`, `ref_so`, and `dramsim3_ini` paths. The emulator must already be
built for `BackendV2NoRVVNoLLCConfig` with the matching NEMU difftest library.

```sh
python3 rtl/scripts/hint_v3_rtl.py --execution-server ps2 --deps /absolute/deps.json --all --jobs 10 --output /absolute/new-output-directory --through hint
```

Each slice runs baseline → Discovery → public CAM selection/load → Detail →
public table construction/load → Hint, in that order. Independent slices run
in parallel, capped by available CPU and memory. Output stage directories must
be new; this runner does not resume or overwrite an existing run.

`warmup_insts` and `measure_insts` in the smoke manifest determine all four
emulator runs, the profiling warmup boundary, and the final counter checks.
The current smoke default is **10,000 warmup + 10,000 measure instructions**:
`-W 10000 -I 20000 --measure-instr=10000`, with
`H2P_HINT_WARMUP_INSTR=10000`. The runner also accepts other positive integer
windows from an explicitly supplied `--manifest`. It records a manifest
snapshot and both requested and actual counts; whole-cycle instruction
overshoot is retained. Performance comparisons must identify their window.

Each run records input hashes, commands, environment, exit status, actual
instructions/cycles, IPC, and Hint counters. The pipeline checks the public
trace format and tool/runner identity before completing. Preserve existing
1M/1M runs under their original frozen runner and output directory; start
10K/10K runs in a separate directory so their results remain distinguishable.

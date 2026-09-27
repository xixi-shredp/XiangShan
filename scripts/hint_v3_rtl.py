#!/usr/bin/env python3
"""Independent RTL Hint v3 discovery, profile, finite build and replay.

The only shared input is the finite-v3 format/scoring code. Every branch trace
in this pipeline is emitted by this RTL emulator's Verilator DPI observer.
"""
from __future__ import annotations

import argparse
from collections import Counter
from concurrent.futures import ThreadPoolExecutor, as_completed
import gzip
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[2]
RTL = ROOT / "rtl"
sys.path.insert(0, str(ROOT / "xs-gem5" / "util"))
from h2p_hint import finite_v3 as finite  # noqa: E402

CONTRACT = finite.contract
MARKER = re.compile(
    r"H2P_RUN_INSTR=(\d+) H2P_WARMUP_INSTR=(\d+) "
    r"H2P_MEASURE_INSTR=(\d+) H2P_MEASURE_CYCLES=(\d+)"
)


def sha(path: Path) -> str:
    return finite.digest(path)


def write_json(path: Path, value: object) -> None:
    path.write_text(json.dumps(value, indent=2, sort_keys=True) + "\n")


def config() -> dict:
    cfg = json.loads((ROOT / "manifest" / "test.json").read_text())
    if cfg["warmup_insts"] != 1_000_000 or cfg["measure_insts"] != 1_000_000:
        raise ValueError("formal manifest must specify 1M warmup + 1M measure")
    return cfg


def checkpoint(workload_id: str) -> Path:
    cfg = config()
    workloads = json.loads((ROOT / "manifest" / cfg["workload_manifest"]).resolve().read_text())
    choices = {
        f"{row['bench']}_test/{point}"
        for row in workloads["cpts"] for point in row["cpt_list"]
    }
    if workload_id not in choices:
        raise ValueError(f"not one of the five declared slices: {workload_id}")
    directory = Path(workloads["workload_root"]) / "cpts" / "xsgem5" / workload_id
    files = list(directory.glob("*_memory_.zstd"))
    if len(files) != 1:
        raise ValueError(f"expected one GCPT memory image in {directory}")
    return files[0]


def build_identity(emu: Path, ref: Path, restore: Path, flash: Path, cpt: Path) -> dict:
    head = subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=RTL, text=True).strip()
    if head != config()["rtl"]["commit"]:
        raise ValueError(f"RTL HEAD mismatch: {head}")
    audit_path = (ROOT / "manifest" / config()["rtl"]["nemu_build_audit"]).resolve()
    audit = json.loads(audit_path.read_text())
    if (audit["so_sha256"] != sha(ref) or
            audit["boot_flash_sha256"] != sha(flash) or
            audit["boot_flash_hex"] != flash.read_bytes().hex() or
            audit["state_bytes_observed"] != 1400):
        raise ValueError("NEMU SO or boot flash differs from audited kmhv3 build")
    changed = subprocess.check_output(["git", "status", "--porcelain=v1"], cwd=RTL, text=True)
    source_files = [
        RTL / "src/main/scala/xiangshan/frontend/Frontend.scala",
        RTL / "src/main/scala/xiangshan/frontend/Bundles.scala",
        RTL / "src/main/scala/xiangshan/frontend/bpu/Bpu.scala",
        RTL / "src/main/scala/xiangshan/frontend/bpu/Bundles.scala",
        RTL / "src/main/scala/xiangshan/frontend/ftq/Ftq.scala",
        RTL / "src/main/scala/xiangshan/frontend/ftq/Bundles.scala",
        RTL / "src/main/scala/xiangshan/frontend/simfrontend/SimFrontend.scala",
        RTL / "src/main/scala/xiangshan/frontend/bpu/hint/Hint.scala",
        RTL / "src/main/scala/xiangshan/frontend/bpu/hint/HintDpi.scala",
        RTL / "src/main/scala/xiangshan/frontend/bpu/hint/HintProfileDpi.scala",
        RTL / "src/main/scala/xiangshan/frontend/bpu/hint/HintReplayDpi.scala",
        RTL / "src/main/scala/xiangshan/backend/CtrlBlock.scala",
        RTL / "src/main/scala/xiangshan/backend/fu/wrapper/BranchUnit.scala",
        RTL / "src/main/scala/xiangshan/backend/fu/wrapper/JumpUnit.scala",
        RTL / "difftest/src/test/csrc/common/hint_dpi.cpp",
        RTL / "difftest/src/test/csrc/emu/emu.cpp",
        RTL / "difftest/src/test/csrc/plugin/simfrontend/ftq.cpp",
        RTL / "difftest/src/test/csrc/plugin/simfrontend/ftq.h",
        RTL / "difftest/src/test/csrc/plugin/simfrontend/simfrontend.cpp",
        RTL / "difftest/src/test/csrc/plugin/simfrontend/simfrontend.h",
        Path(__file__),
        Path(finite.__file__), ROOT / "manifest/hint_v3.py",
        ROOT / "manifest/test.json", ROOT / "workload/cpython-heavy.json",
        audit_path,
    ]
    return {
        "rtl_head": head, "rtl_status": changed,
        "source_sha256": {str(path.relative_to(ROOT)): sha(path) for path in source_files},
        "emulator_sha256": sha(emu), "nemu_sha256": sha(ref),
        "gcpt_restore_sha256": sha(restore), "boot_flash_sha256": sha(flash),
        "checkpoint_sha256": sha(cpt),
    }


def parse_marker(log: Path, warmup: int, measure: int) -> dict:
    data = log.read_text(errors="replace")
    found = MARKER.findall(data)
    if len(found) != 1:
        raise ValueError(f"missing or repeated H2P instruction audit marker: {log}")
    total, used_warmup, actual_measure, cycles = map(int, found[0])
    if used_warmup != warmup or total < warmup + measure or actual_measure < measure:
        raise ValueError(f"short or mismatched RTL instruction count: {found[0]}")
    if cycles <= 0 or "Warmup finished" not in data:
        raise ValueError(f"missing measurement cycle/warmup marker in {log}")
    if "ABORT at pc" in data or "HIT BAD TRAP" in data or "difftest mismatch" in data.lower():
        raise ValueError(f"RTL/NEMU mismatch or abort in {log}")
    stat_lines = re.findall(r"^H2P_HINT_STATS (.+)$", data, re.MULTILINE)
    if len(stat_lines) != 1:
        raise ValueError(f"missing or repeated Hint counter marker: {log}")
    counters = dict((name, int(value)) for name, value in
                    re.findall(r"([a-z0-9_]+)=(\d+)", stat_lines[0]))
    required = {"cam_miss", "context_miss", "conf0_fallback", "overrides",
                "meta_stall", "lhr_stall", "conf_up", "conf_down", "conf_dropped"}
    if set(counters) != required:
        raise ValueError(f"incomplete Hint counters in {log}")
    return {"total_instructions": total, "warmup_instructions": used_warmup,
            "measure_instructions": actual_measure, "measure_cycles": cycles,
            "ipc": actual_measure / cycles, "hint_counters": counters}


def run_emu(name: str, out: Path, mode: str, cpt: Path, restore: Path, flash: Path, ref: Path,
            warmup: int, measure: int, image: Path | None = None,
            profile: Path | None = None, replay: Path | None = None,
            dry_run: bool = False) -> dict:
    emu = RTL / "build/emu"
    command = [str(emu), "-i", str(cpt), "-r", str(restore), "-F", str(flash),
               "--diff", str(ref),
               "-W", str(warmup), "-I", str(warmup + measure)]
    env = os.environ.copy()
    env.update({"H2P_HINT_MODE": mode, "H2P_HINT_WARMUP_INSTR": str(warmup)})
    if image is not None:
        env["H2P_HINT_IMAGE"] = str(image)
    else:
        env.pop("H2P_HINT_IMAGE", None)
    if profile is not None:
        env["H2P_HINT_PROFILE"] = str(profile)
    else:
        env.pop("H2P_HINT_PROFILE", None)
    if replay is not None:
        env["H2P_HINT_REPLAY"] = str(replay)
    else:
        env.pop("H2P_HINT_REPLAY", None)
    info = {"name": name, "mode": mode, "command": command,
            "image_sha256": sha(image) if image and not dry_run else None,
            "profile": str(profile) if profile else None,
            "replay": str(replay) if replay else None}
    if dry_run:
        return info
    before = {str(path): sha(path) for path in (emu, cpt, restore, flash, ref)}
    if image is not None:
        before[str(image)] = sha(image)
    status_path = out / "status.json"
    if status_path.is_file():
        prior = json.loads(status_path.read_text())
        for key in ("name", "mode", "command", "image_sha256", "profile", "replay"):
            if prior.get(key) != info.get(key):
                raise ValueError(f"resumed RTL run differs in {key}: {out}")
        if prior.get("exit_code") != 0 or prior.get("inputs_sha256") != before:
            raise ValueError(f"cannot resume changed or failed RTL run: {out}")
        actual = parse_marker(Path(prior["log"]), warmup, measure)
        if image is not None and f"Hint DPI: validated {image}" not in Path(prior["log"]).read_text(errors="replace"):
            raise ValueError(f"resumed RTL image load marker missing: {out}")
        for key, value in actual.items():
            if prior.get(key) != value:
                raise ValueError(f"resumed RTL run metrics changed: {out}")
        for key, path in (("profile_sha256", profile), ("replay_sha256", replay)):
            if path is not None and prior.get(key) != sha(path):
                raise ValueError(f"resumed RTL run trace changed: {out}")
        return prior
    out.mkdir(parents=True, exist_ok=False)
    log = out / "emu.log"
    with log.open("w") as stream:
        completed = subprocess.run(command, cwd=RTL, env=env, stdout=stream,
                                   stderr=subprocess.STDOUT, check=False)
    info["exit_code"] = completed.returncode
    info["log"] = str(log)
    info["inputs_sha256"] = before
    after = {path: sha(Path(path)) for path in before}
    if before != after:
        raise ValueError(f"RTL run input changed during execution: {name}")
    if completed.returncode:
        write_json(out / "status.json", info)
        raise RuntimeError(f"RTL run {name} failed: {completed.returncode}; {log}")
    info.update(parse_marker(log, warmup, measure))
    if image is not None and f"Hint DPI: validated {image}" not in log.read_text(errors="replace"):
        raise ValueError(f"RTL image validation/load marker missing: {log}")
    if profile is not None:
        if not profile.is_file():
            raise ValueError(f"RTL did not produce profile: {profile}")
        info["profile_sha256"] = sha(profile)
    if replay is not None:
        if not replay.is_file():
            raise ValueError(f"RTL did not produce replay events: {replay}")
        info["replay_sha256"] = sha(replay)
    write_json(out / "status.json", info)
    return info


def discovery_top20(path: Path) -> tuple[list[int], dict]:
    misses, counts = Counter(), Counter()
    last_seq = -1
    with gzip.open(path, "rt") as stream:
        header = json.loads(next(stream))
        if header != {"kind": "discovery_header", "format": "h2p-hint-discovery", "version": 3}:
            raise ValueError("bad RTL discovery header")
        for line in stream:
            row = json.loads(line)
            if row["kind"] != "branch" or row["seq"] <= last_seq:
                raise ValueError("discovery sequence is not committed order")
            last_seq = row["seq"]
            pc = int(row["pc"], 16)
            CONTRACT.address_word(pc)
            counts[pc] += 1
            misses[pc] += not row["baseline_valid"] or row["baseline_target"] != row["actual"]
    top = sorted((pc for pc in counts if misses[pc] > 0),
                 key=lambda pc: (-misses[pc], -counts[pc], pc))[:20]
    return top, {"top20": [{"pc": hex(pc), "misses": misses[pc],
                            "executions": counts[pc]} for pc in top],
                 "observed_indirect_pcs": len(counts)}


def candidate_image(pcs: list[int], source: dict, path: Path) -> None:
    metadata = {"format": "h2p-hint", "version": 3, "storage": "finite",
                "hash_version": 3, "mask_order": CONTRACT.MASK_ORDER,
                "history": CONTRACT.HISTORY, "layout": CONTRACT.LAYOUT,
                "cam": [{"pc": hex(pc), "mask": 0} for pc in sorted(pcs)],
                "slots": [], "entries": [], "source": source}
    path.write_bytes(CONTRACT.pack_image(metadata))


def replay_report(path: Path) -> dict:
    per_pc: dict[int, Counter] = {}
    total = Counter()
    last_seq = -1
    with gzip.open(path, "rt") as stream:
        header = json.loads(next(stream))
        if header != {"kind": "header", "format": "h2p-hint-replay", "version": 3}:
            raise ValueError("bad RTL Hint replay header")
        for line in stream:
            row = json.loads(line)
            if row["kind"] != "branch" or row["seq"] <= last_seq:
                raise ValueError("replay events not in committed order")
            last_seq = row["seq"]
            pc = int(row["pc"], 16)
            counts = per_pc.setdefault(pc, Counter())
            before = row["baseline_target"] != row["actual"]
            after = row["final_target"] != row["actual"]
            for counter in (counts, total):
                counter["executions"] += 1
                counter["baseline_target_misses"] += before
                counter["hint_target_misses"] += after
                counter["corrected"] += before and not after
                counter["harmed"] += not before and after
                counter["used_hint"] += row["used_hint"]
                counter["conf0"] += row["context_hit"] and row["conf_at_prediction"] == 0
    return {"total": dict(total),
            "pcs": [{"pc": hex(pc), **dict(counts)}
                    for pc, counts in sorted(per_pc.items())]}


def run_slice(workload_id: str, output: Path, warmup: int, measure: int,
              through: str, dry_run: bool) -> dict:
    cfg = config()["rtl"]
    cpt = checkpoint(workload_id)
    restore = Path(cfg["gcpt_restore"])
    flash = (ROOT / "manifest" / cfg["boot_flash"]).resolve()
    ref = (ROOT / "manifest" / cfg["nemu_so"]).resolve()
    emu = RTL / "build/emu"
    for required in (cpt, restore, flash, ref, emu):
        if not required.is_file():
            raise FileNotFoundError(required)
    identity = build_identity(emu, ref, restore, flash, cpt)
    output.mkdir(parents=True, exist_ok=True)
    identity_path = output / "identity.json"
    if identity_path.exists() and json.loads(identity_path.read_text()) != identity:
        raise ValueError("RTL source/executable identity differs from existing output")
    write_json(identity_path, identity)
    runs = []
    for name, mode, profile in (("baseline", "off", None),
                                ("discovery", "discover", output / "discovery" / "branches.jsonl.gz")):
        runs.append(run_emu(name, output / name, mode, cpt, restore, flash, ref,
                            warmup, measure, profile=profile, dry_run=dry_run))
    if dry_run or through == "discovery":
        write_json(output / "runs.json", runs)
        return {"runs": runs}
    if runs[0]["measure_cycles"] != runs[1]["measure_cycles"]:
        raise ValueError("observer changed baseline cycle count")
    top, discovery = discovery_top20(output / "discovery" / "branches.jsonl.gz")
    write_json(output / "discovery.json", discovery)
    source = {"workload_id": workload_id,
              "checkpoint_sha256": identity["checkpoint_sha256"],
              "profile_sha256": sha(output / "discovery" / "branches.jsonl.gz"),
              "builder_sha256": sha(Path(finite.__file__))}
    profiles = []
    for group_id, first in enumerate(range(0, len(top), 16)):
        pcs = top[first:first + 16]
        group = output / f"profile-{group_id}"
        group.mkdir(exist_ok=True)
        candidate = group / "candidates.bin"
        expected_candidate = group / "candidates.new.bin"
        candidate_image(pcs, source, expected_candidate)
        if candidate.exists() and candidate.read_bytes() != expected_candidate.read_bytes():
            raise ValueError("resumed candidate image changed")
        expected_candidate.replace(candidate)
        profile = group / "branches.jsonl.gz"
        runs.append(run_emu(f"profile-{group_id}", group / "run", "profile",
                            cpt, restore, flash, ref, warmup, measure,
                            image=candidate, profile=profile))
        if runs[-1]["measure_cycles"] != runs[0]["measure_cycles"]:
            raise ValueError("profile observer changed baseline cycle count")
        header, rows = finite.read_profile(profile)
        if set(int(pc, 16) for pc in header["pcs"]) != set(pcs):
            raise ValueError("profile group CAM identity mismatch")
        profiles.append(profile)
    if through == "profile":
        write_json(output / "runs.json", runs)
        return {"runs": runs, "discovery": discovery}
    if profiles:
        _, rows = finite.merge_profiles(profiles, top)
    else:
        rows = []
    source["profile_sha256"] = hashlib.sha256(
        "\n".join(sha(path) for path in profiles).encode()).hexdigest()
    scores = []
    for profile in profiles:
        header, group_rows = finite.read_profile(profile)
        group_pcs = [int(pc, 16) for pc in header["pcs"]]
        for mask in range(16):
            one, _, _ = finite.build_image(
                group_rows, {pc: mask for pc in group_pcs}, source)
            one_rows = {int(row["pc"], 16): row
                        for row in finite.score_image(group_rows, one)["pcs"]}
            for pc in group_pcs:
                scores.append((pc, mask, one_rows.get(pc, {"corrected": 0, "harmed": 0})))
    selected, selection = finite.choose_best(scores)
    metadata, image, builder = finite.build_image(rows, selected, source)
    write_json(output / "metadata.json", metadata)
    (output / "hint-v3.bin").write_bytes(image)
    if (CONTRACT.pack_image(metadata) != image or
            sha(output / "hint-v3.bin") != builder["image_sha256"] or
            source["checkpoint_sha256"] != identity["checkpoint_sha256"] or
            source["builder_sha256"] != sha(Path(finite.__file__))):
        raise ValueError("finite image/provenance audit failed before RTL replay")
    write_json(output / "builder.json", builder)
    write_json(output / "selection.json", selection)
    write_json(output / "fixed-conf3-estimate.json", finite.score_image(rows, metadata))
    write_json(output / "source.json", source)
    if through == "build":
        write_json(output / "runs.json", runs)
        return {"runs": runs, "selected": len(selected)}
    replay = output / "hint" / "replay.jsonl.gz"
    runs.append(run_emu("hint", output / "hint", "hint", cpt, restore, flash, ref,
                        warmup, measure, image=output / "hint-v3.bin",
                        replay=replay))
    write_json(output / "replay-report.json", replay_report(replay))
    if build_identity(emu, ref, restore, flash, cpt) != identity:
        raise ValueError("RTL source or executable identity changed during pipeline")
    write_json(output / "runs.json", runs)
    return {"runs": runs, "selected": len(selected)}


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    selection = parser.add_mutually_exclusive_group(required=True)
    selection.add_argument("--slice", help="e.g. scimark_fft_test/26")
    selection.add_argument("--all", action="store_true", help="run all five manifest slices")
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--through", choices=("discovery", "profile", "build", "hint"),
                        default="hint")
    parser.add_argument("--warmup", type=int, default=1_000_000)
    parser.add_argument("--measure", type=int, default=1_000_000)
    parser.add_argument("--dry-run", action="store_true")
    parser.add_argument("--jobs", type=int, default=1,
                        help="parallel slices; each slice retains baseline/discovery/profile/build order")
    args = parser.parse_args()
    if args.warmup <= 0 or args.measure <= 0 or args.jobs <= 0:
        parser.error("warmup, measure and jobs must be positive")
    if args.all:
        workloads = json.loads(
            (ROOT / "manifest" / config()["workload_manifest"]).resolve().read_text())
        ids = [f"{row['bench']}_test/{point}"
               for row in workloads["cpts"] for point in row["cpt_list"]]
        if len(ids) != 5 or len(set(ids)) != 5:
            raise ValueError("formal RTL report requires exactly five unique slices")
    else:
        ids = [args.slice]
    results = {}
    with ThreadPoolExecutor(max_workers=min(args.jobs, len(ids))) as pool:
        futures = {
            pool.submit(run_slice, workload_id,
                        args.output.resolve() / workload_id.replace("/", "_"),
                        args.warmup, args.measure, args.through, args.dry_run): workload_id
            for workload_id in ids
        }
        for future in as_completed(futures):
            workload_id = futures[future]
            results[workload_id] = future.result()
            print(json.dumps({workload_id: results[workload_id]}, indent=2), flush=True)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

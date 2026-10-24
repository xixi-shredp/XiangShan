#!/usr/bin/env python3
"""Remote RTL smoke pipeline using the standalone public SPEC trace CLI.

Execute on ps1/ps2. --deps supplies ref_so, gcpt_restore, boot_flash and optionally
emu/checkpoint_root. No guest mutation or private simulator format is required.
"""
from __future__ import annotations
import argparse
from concurrent.futures import ThreadPoolExecutor, as_completed
from datetime import datetime, timezone
import hashlib
import json
import math
import os
from pathlib import Path
import re
import socket
import subprocess
import sys
import hint_cam58

ROOT = Path(__file__).resolve().parents[2]
RTL = ROOT / "rtl"
TOOL = ROOT / "xs-gem5/util/h2p_hint/spec_trace.py"
MARKER = re.compile(r"H2P_RUN_INSTR=(\d+) H2P_WARMUP_INSTR=(\d+) H2P_MEASURE_INSTR=(\d+) H2P_MEASURE_CYCLES=(\d+)")


def sha(path):
    h = hashlib.sha256()
    with Path(path).open("rb") as stream:
        for chunk in iter(lambda: stream.read(1 << 20), b""):
            h.update(chunk)
    return h.hexdigest()


def write_json(path, value):
    Path(path).write_text(json.dumps(value, indent=2, sort_keys=True) + "\n")


def path_value(value):
    path = Path(value).expanduser()
    return (ROOT / path).resolve() if not path.is_absolute() else path.resolve()


def public_tool(action, out, trace=None, meta=None, cam=None, image=None, kind=None):
    command = [sys.executable, str(TOOL), action]
    for flag, value in (("trace", trace), ("meta", meta), ("cam", cam),
                        ("image", image), ("kind", kind)):
        if value is not None:
            command += ["--" + flag, str(value)]
    if out is not None:
        command += ["--output", str(out)]
    result = subprocess.run(command, text=True, capture_output=True, check=False)
    if result.returncode:
        raise RuntimeError(f"public builder failed: {command}\n{result.stderr}")
    return json.loads(result.stdout)


def trace_meta(trace, kind, cam=None):
    width = 7 if kind == "discovery" else 22
    size = trace.stat().st_size
    if size % width:
        raise ValueError("truncated RTL raw record")
    audit = [json.loads(line) for line in Path(str(trace) + ".audit.jsonl").read_text().splitlines()]
    footer = audit[-1]
    if not footer.get("complete"):
        raise ValueError("RTL trace audit is incomplete")
    meta = {"schema": "hint-spec-trace", "version": 1, "hash_version": 7,
            "mask_order": ["ITH", "Call", "Ret", "LHR"], "kind": kind,
            "record_bytes": width, "termination": "final", "capacity_records": size // width,
            "data_sha256": sha(trace), "p_error": footer.get("p_error", 0),
            "audit_sha256": sha(Path(str(trace) + ".audit.jsonl")),
            "stop_reason": "instruction-limit", "fetch_block_bytes": 64, "fetch_align_bytes": 32,
            "capture": footer}
    if cam:
        meta["cam_sha256"] = json.loads(cam.read_text())["image_sha256"]
    path = trace.with_suffix(".json")
    write_json(path, meta)
    public_tool("validate", trace.with_suffix(".validation.json"), trace, path, cam)
    return path


def parse_marker(log, warmup_insts, measure_insts):
    data = log.read_text(errors="replace")
    markers = MARKER.findall(data)
    if len(markers) != 1:
        raise ValueError(f"missing/repeated measure marker: {log}")
    total, warmup, measure, cycles = map(int, markers[0])
    if warmup < warmup_insts or measure < measure_insts or total != warmup + measure or cycles <= 0:
        raise ValueError(f"invalid {warmup_insts} warmup + {measure_insts} measure boundary: {markers[0]}")
    if any(text in data.lower() for text in ("abort at pc", "hit bad trap", "difftest mismatch")):
        raise ValueError(f"RTL/NEMU failure: {log}")
    stats = re.findall(r"^H2P_HINT_STATS (.+)$", data, re.MULTILINE)
    if len(stats) != 1:
        raise ValueError("missing Hint statistics")
    return {"total_instructions": total, "warmup_instructions": warmup,
            "measure_instructions": measure, "measure_cycles": cycles, "ipc": measure / cycles,
            "hint_counters": {key: int(value) for key, value in re.findall(r"([a-z0-9_]+)=(\d+)", stats[0])}}


def run_emu(output, mode, checkpoint, deps, warmup_insts, measure_insts, image=None):
    output.mkdir(parents=True, exist_ok=False)
    emu = path_value(deps.get("emu", deps.get("emulator", "rtl/build/emu")))
    inputs = [emu, checkpoint] + [path_value(deps[key]) for key in ("gcpt_restore", "boot_flash", "ref_so")]
    if image:
        image_manifest = image.with_suffix(".adapter.json")
        adapted = json.loads(image_manifest.read_text())
        if adapted.get("image_sha256") != sha(image) or adapted.get("adapter_sha256") != sha(Path(hint_cam58.__file__)):
            raise ValueError("CAM58 loaded image/adapter identity differs from adapter manifest")
        for key in ("public_image", "selection", "public_cam"):
            path = Path(adapted[key])
            if sha(path) != adapted[key + "_sha256"]:
                raise ValueError(f"CAM58 {key} changed before loading")
            inputs.append(path)
        inputs.append(image_manifest)
    for path in inputs + ([image] if image else []):
        if not path.is_file():
            raise FileNotFoundError(path)
    command = [str(emu), "-i", str(checkpoint), "-r", str(inputs[2]), "-F", str(inputs[3]),
               "--diff", str(inputs[4]), "-W", str(warmup_insts), "-I", str(warmup_insts + measure_insts),
               f"--measure-instr={measure_insts}"]
    dram_ini = deps.get("dramsim3_ini", "deps/DRAMsim3/configs/XiangShan.ini")
    if dram_ini is not None:
        ini = path_value(dram_ini)
        if not ini.is_file():
            raise FileNotFoundError(ini)
        command += ["--dramsim3-ini", str(ini), "--dramsim3-outdir", str(output)]
        inputs.append(ini)
    env = os.environ.copy()
    for key in list(env):
        if key.startswith("H2P_"):
            del env[key]
    env.update(H2P_HINT_MODE=mode, H2P_HINT_WARMUP_INSTR=str(warmup_insts), H2P_RUNTIME_CONTRACT="1",
               H2P_TARGET_SUMMARY=str(output / "targets.json"))
    trace = None
    if image:
        env["H2P_HINT_IMAGE"] = str(image)
    if mode in ("discover", "profile"):
        trace = output / "trace.bin"
        env["H2P_HINT_PROFILE"] = str(trace)
        env["H2P_HINT_TRACE_COMMIT"] = str(trace)
    if mode == "hint":
        env["H2P_HINT_REPLAY"] = str(output / "replay.jsonl.gz")
    hashes = {str(path): sha(path) for path in inputs + ([image] if image else [])}
    log = output / "emu.log"
    with log.open("w") as stream:
        result = subprocess.run(command, cwd=RTL, env=env, stdout=stream, stderr=subprocess.STDOUT, check=False)
    status = {"mode": mode, "command": command, "inputs_sha256": hashes, "exit_code": result.returncode,
              "requested_warmup_instructions": warmup_insts, "requested_measure_instructions": measure_insts,
              "environment": {key: value for key, value in env.items() if key.startswith("H2P_")}}
    write_json(output / "status.json", status)
    if result.returncode:
        raise RuntimeError(f"RTL run failed ({result.returncode}): {log}")
    if hashes != {str(path): sha(Path(path)) for path in hashes}:
        raise ValueError("run input changed during execution")
    if image and "H2P_HINT_LOAD_COMPLETE" not in log.read_text():
        raise ValueError("image table-write handshake completion missing")
    status.update(parse_marker(log, warmup_insts, measure_insts))
    if trace:
        status["trace_sha256"] = sha(trace)
    write_json(output / "status.json", status)
    return status, trace


def resource_budget(requested_jobs, slice_count):
    """Conservative startup admission; the outer dispatcher chooses each server."""
    info = {}
    for line in Path("/proc/meminfo").read_text().splitlines():
        fields = line.split()
        if len(fields) >= 2:
            info[fields[0].rstrip(":")] = int(fields[1]) * 1024
    available = info.get("MemAvailable", 0)
    affinity = sorted(os.sched_getaffinity(0))
    load1, load5, load15 = os.getloadavg()
    cpu_remaining = max(0, len(affinity) - math.ceil(load1))
    per_emu = 12 * 1024 ** 3
    memory_slots = available // per_emu
    effective = min(requested_jobs, slice_count, memory_slots, cpu_remaining)
    audit = {"timestamp_utc": datetime.now(timezone.utc).isoformat(),
             "hostname": socket.gethostname(), "mem_available_bytes": available,
             "cpu_affinity": affinity, "affinity_cpu_count": len(affinity),
             "load_average": [load1, load5, load15], "cpu_remaining": cpu_remaining,
             "per_emu_memory_bytes": per_emu, "memory_job_limit": memory_slots,
             "requested_jobs": requested_jobs, "slice_count": slice_count,
             "effective_jobs": effective,
             "policy": "min(requested,slices,MemAvailable/12GiB,affinity-ceil(load1)); startup admission only"}
    return audit


def run_slice(slice_id, checkpoint, output, deps, through, execution_server, budget, warmup_insts, measure_insts):
    output.mkdir(parents=True, exist_ok=True)
    identity = {"checkpoint_sha256": sha(checkpoint), "builder_sha256": sha(TOOL),
                "cam58_adapter_sha256": sha(Path(hint_cam58.__file__)),
                "runner_sha256": sha(Path(__file__)), "host": socket.gethostname(),
                "execution_server": execution_server, "resource_budget": budget,
                "requested_warmup_instructions": warmup_insts, "requested_measure_instructions": measure_insts}
    write_json(output / "identity.json", identity)
    runs = []
    baseline, _ = run_emu(output / "baseline", "off", checkpoint, deps, warmup_insts, measure_insts)
    runs.append(baseline)
    discovery, trace = run_emu(output / "discovery", "discover", checkpoint, deps, warmup_insts, measure_insts)
    runs.append(discovery)
    discovery["baseline_cycle_difference"] = discovery["measure_cycles"] - baseline["measure_cycles"]
    meta = trace_meta(trace, "discovery")
    selected_discovery = output / "discovery-selected.bin"
    choices = hint_cam58.select_discovery(trace, meta, selected_discovery)
    selected_meta = trace_meta(selected_discovery, "discovery")
    cam, cam_image = output / "cam.json", output / "cam.bin"
    public_cam_image = output / "cam.public.bin"
    # Public selection ranks each window's total baseline misses, not just its winning CFI.
    public_tool("select-cam", cam, trace, meta, image=public_cam_image)
    selection = hint_cam58.freeze_selection(cam, choices, trace, output / "cam58-selection.json")
    hint_cam58.adapt_image(public_cam_image, cam, selection, cam_image, "cam")
    if through != "discovery":
        detail, trace = run_emu(output / "detail", "profile", checkpoint, deps, warmup_insts, measure_insts, cam_image)
        runs.append(detail)
        detail["baseline_cycle_difference"] = detail["measure_cycles"] - baseline["measure_cycles"]
        meta = trace_meta(trace, "detail", cam)
        selected_detail = output / "detail-selected.bin"
        hint_cam58.filter_detail(trace, meta, cam, selection, selected_detail)
        selected_meta = trace_meta(selected_detail, "detail", cam)
        if through != "profile":
            image = output / "hint.bin"
            public_image = output / "hint.public.bin"
            public_tool("build", output / "image.json", selected_detail, selected_meta, cam, public_image)
            hint_cam58.adapt_image(public_image, cam, selection, image, "load")
            if through == "hint":
                hint, _ = run_emu(output / "hint", "hint", checkpoint, deps, warmup_insts, measure_insts, image)
                runs.append(hint)
    if (sha(TOOL) != identity["builder_sha256"] or sha(Path(__file__)) != identity["runner_sha256"] or
            sha(Path(hint_cam58.__file__)) != identity["cam58_adapter_sha256"]):
        raise ValueError(f"builder/runner changed during the slice pipeline: {slice_id}")
    identity["source_identity_verified_after_pipeline"] = True
    write_json(output / "identity.json", identity)
    write_json(output / "runs.json", {"slice": slice_id, "runs": runs,
               "selection_basis": "public unordered static estimate; actual IPC measured separately"})
    return runs


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--manifest", type=Path, default=ROOT / "manifest/smoke-test.json")
    parser.add_argument("--workloads", type=Path, default=ROOT / "workload/cpython-norvv-uart16550-100m200m-heavy.json")
    parser.add_argument("--deps", required=True, type=Path)
    parser.add_argument("--execution-server", required=True, choices=("ps1", "ps2"),
                        help="trusted outer SSH dispatcher label; actual hostname is also recorded")
    selection = parser.add_mutually_exclusive_group(required=True)
    selection.add_argument("--slice", help="bench/point, e.g. scimark_fft/51")
    selection.add_argument("--all", action="store_true")
    parser.add_argument("--checkpoint", type=Path, help="explicit checkpoint file for a single slice")
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--jobs", type=int, default=1, help="parallel independent slices, capped by startup memory/CPU budget")
    parser.add_argument("--through", choices=("discovery", "profile", "build", "hint"), default="hint")
    args = parser.parse_args()
    config = json.loads(args.manifest.read_text())
    warmup_insts, measure_insts = config["warmup_insts"], config["measure_insts"]
    if any(type(value) is not int or value <= 0 for value in (warmup_insts, measure_insts)):
        parser.error("manifest warmup_insts and measure_insts must be positive integers")
    if args.jobs < 1:
        parser.error("--jobs must be positive")
    if args.all and args.checkpoint:
        parser.error("--checkpoint requires --slice")
    workloads = json.loads(args.workloads.read_text())
    declared = [f"{row['bench']}/{point}" for row in workloads["cpts"] for point in row["cpt_list"]]
    ids = declared if args.all else [args.slice.replace("_test/", "/")]
    if any(slice_id not in declared for slice_id in ids):
        parser.error("slice not declared by the current workload manifest")
    deps = json.loads(args.deps.read_text())
    root = deps.get("checkpoint_root", workloads["workload_root"])
    if root.startswith("ps1:"):
        root = root[4:]
    root = Path(root).expanduser()
    args.output = args.output.resolve()
    args.output.mkdir(parents=True, exist_ok=True)
    write_json(args.output / "manifest-snapshot.json", config)
    budget = resource_budget(args.jobs, len(ids))
    budget["execution_server"] = args.execution_server
    write_json(args.output / "resource-budget.json", budget)
    if budget["effective_jobs"] < 1:
        parser.error("insufficient startup memory/CPU budget; see resource-budget.json")
    checkpoints = {}
    for slice_id in ids:
        bench, point = slice_id.split("/")
        if args.checkpoint:
            checkpoint = args.checkpoint.resolve()
        else:
            choices = set()
            for directory in (root / f"{bench}_test" / point, root / bench / point):
                if directory.is_dir():
                    choices.update(path.resolve() for path in directory.rglob("*_memory_.zstd"))
            if len(choices) != 1:
                parser.error(f"expected exactly one checkpoint for {slice_id}; supply --checkpoint")
            checkpoint = choices.pop()
        checkpoints[slice_id] = checkpoint
    results = {}
    with ThreadPoolExecutor(max_workers=budget["effective_jobs"]) as pool:
        pending = {pool.submit(run_slice, slice_id, checkpoint,
                               args.output / slice_id.replace("/", "_"), deps,
                               args.through, args.execution_server, budget, warmup_insts, measure_insts): slice_id
                   for slice_id, checkpoint in checkpoints.items()}
        for future in as_completed(pending):
            slice_id = pending[future]
            results[slice_id] = future.result()
            write_json(args.output / "summary.json", results)
    print(json.dumps(results, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

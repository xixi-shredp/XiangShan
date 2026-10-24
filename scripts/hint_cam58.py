"""RTL-only CAM58 adapter; public 7/22B records and public builder stay unchanged."""
from collections import Counter, defaultdict
import hashlib
import json
from pathlib import Path
import struct

CAM_MAGIC = b"H2PCAM58"
LOAD_MAGIC = b"H2PLD058"


def digest(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def require(ok, message):
    if not ok:
        raise ValueError(message)


def address(word):
    return (word << 1) | (0xffff000000000000 if word >> 46 else 0)


def canonical(pc):
    return 0 <= pc < 1 << 64 and (pc >> 48) == (0xffff if pc & (1 << 47) else 0)


def indirect_identity(instr):
    require(type(instr) is int and 0 <= instr < 1 << 32, "instruction field invalid")
    if instr & 3 == 3:
        require(instr & 0x707f == 0x67, "sidecar is not indirect")
        rd, rs = (instr >> 7) & 31, (instr >> 15) & 31
    else:
        require(instr & 0xe07f == 0x8002, "sidecar is not compressed indirect")
        rd, rs = (instr >> 12) & 1, (instr >> 7) & 31
        require(rs != 0, "sidecar is EBREAK")
    require(not (rs in (1, 5) and rd != rs), "return/PopAndPush is not eligible")
    return rd in (1, 5)


def checked_identity(row):
    require(type(row.get("is_rvc")) is bool and type(row.get("is_call")) is bool,
            "CAM58 requires explicit checked width/call sidecar")
    q, pc = int(row["start_pc"], 16), int(row["pc"], 16)
    require(not ((q | pc) & 1), "odd CAM58 PC")
    require(0 <= q < 1 << 64 and 0 <= pc < 1 << 64, "CAM58 PC out of range")
    require(canonical(q) and canonical(pc), "CAM58 Q/P must be Sv48 canonical")
    require(q <= (1 << 64) - 65, "CAM58 window address calculation would wrap")
    end = pc + (0 if row["is_rvc"] else 2)
    require(canonical(end) and end <= (1 << 64) - 3, "CAM58 end-halfword/C+2 invalid or wraps")
    effective_end = min((q + 64) & ~31, (q & ~4095) + 4096)
    require(q <= end < effective_end < 1 << 64 and canonical(effective_end - 2), "CAM58 CFI outside original window")
    require(pc >= q or (not row["is_rvc"] and pc == q - 2), "invalid negative DELTA")
    position = (end - (q & ~31)) // 2
    require(0 <= position < 32, "CAM58 position width overflow")
    require(row.get("cfi_end_pc") == hex(end) and type(row.get("cfi_position")) is int and row["cfi_position"] == position,
            "CAM58 sidecar position differs from checked width/PC")
    require(type(row.get("ftq_offset")) is int and q + row["ftq_offset"] * 2 == end,
            "CAM58 sidecar offset differs from checked width/PC")
    is_call = indirect_identity(row.get("instr"))
    require(row["is_call"] == is_call, "CAM58 call classification differs from instruction")
    return q, pc, position, is_call


def checked_rows(trace, meta, kind, cam=None):
    trace, meta = Path(trace), Path(meta)
    data = trace.read_bytes()
    metadata = json.loads(meta.read_text())
    require(metadata.get("schema") == "hint-spec-trace" and metadata.get("version") == 1 and metadata.get("kind") == kind,
            "CAM58 source metadata schema/kind mismatch")
    audit_path = Path(str(trace) + ".audit.jsonl")
    require(metadata.get("data_sha256") == digest(trace), "raw digest mismatch")
    require(metadata.get("audit_sha256") == digest(audit_path), "checked sidecar digest mismatch")
    audit = [json.loads(line) for line in audit_path.read_text().splitlines()]
    require(audit[0].get("version") == 3 and audit[0].get("cam_identity") == "checked_pc_width_instruction" and
            audit[0].get("producer") == "rtl-ftq-checked-retirement",
            "CAM58 requires checked identity sidecar version3")
    footer = audit[-1]
    require(footer.get("complete") is True and not footer.get("overflow") and footer.get("p_error") == 0,
            "incomplete/failed CAM58 source capture")
    width, final_bit = (7, 54) if kind == "discovery" else (22, 172)
    require(len(data) % width == 0 and len(data) >= width, "truncated CAM58 source raw")
    records = audit[1:-1]
    require(footer.get("records") == len(records) and len(data) == width * (len(records) + 1),
            "CAM58 sidecar/raw count mismatch")
    require(int.from_bytes(data[-width:], "little") == 1 << final_bit, "missing exact final record")
    origins = Counter(row.get("origin") for row in records)
    require(set(origins) <= {"hardware_trace_fifo", "checked_rob_tail_host_completion"} and
            origins["hardware_trace_fifo"] == footer.get("hardware_fifo_records") and
            origins["checked_rob_tail_host_completion"] == footer.get("checked_tail_host_completed") and
            origins["checked_rob_tail_host_completion"] <= 80, "CAM58 source origin accounting mismatch")
    cam_entries = json.loads(Path(cam).read_text())["entries"] if cam else None
    seen = set()
    result = []
    for index, row in enumerate(records):
        require(row.get("kind") == "record" and row.get("index") == index, "CAM58 sidecar row index mismatch")
        key = (row.get("generation_id"), row.get("retired_ordinal"), row.get("ftq_tag"), row.get("query_cycle"))
        require(all(type(v) is int and v >= 0 for v in key) and key[0] > 0 and key[1] > 0 and key not in seen,
                "duplicate/invalid checked CAM58 identity")
        seen.add(key)
        require(type(row.get("baseline_miss")) is bool and type(row.get("query_valid")) is bool,
                "CAM58 source flags are not Boolean")
        q, pc, position, is_call = checked_identity(row)
        raw = data[index * width:(index + 1) * width]
        value = int.from_bytes(raw, "little")
        require(value >> (final_bit + 1) == 0 and not (value & (1 << final_bit)), "CAM58 raw padding/final invalid")
        delta = (value >> (47 if kind == "discovery" else 163)) & 127
        delta = delta - 128 if delta & 64 else delta
        require(pc == q + delta * 2, "CAM58 raw DELTA differs from checked identity")
        if kind == "discovery":
            require(address(value & ((1 << 47) - 1)) == pc and row["baseline_miss"] is True,
                    "Discovery raw PC/miss differs from checked sidecar")
        else:
            pc_id = value & 15
            require(cam_entries is not None and cam_entries[pc_id]["valid"] and int(cam_entries[pc_id]["start_pc"], 16) == q,
                    "Detail raw CAM mapping differs from checked sidecar")
            require(address((value >> 4) & ((1 << 47) - 1)) == int(row["target"], 16), "Detail target differs from checked sidecar")
            require(bool((value >> 170) & 1) == row["baseline_miss"] and bool((value >> 171) & 1) == row["query_valid"],
                    "Detail flags differ from checked sidecar")
        result.append({"row": row, "raw": raw, "q": q, "pc": pc, "position": position, "is_call": is_call})
    return result


def write_subset(source, output, kind, rows, reason):
    source, output = Path(source), Path(output)
    width, final_bit = (7, 54) if kind == "discovery" else (22, 172)
    output.write_bytes(b"".join(item["raw"] for item in rows) + (1 << final_bit).to_bytes(width, "little"))
    audit = [{"kind": "header", "format": "h2p-ftq-commit-trace", "version": 3,
              "producer": "rtl-ftq-checked-retirement",
              "cam_identity": "checked_pc_width_instruction", "record_bytes": width,
              "derived_from_sha256": digest(source), "derived_audit_sha256": digest(str(source) + ".audit.jsonl"),
              "selection": reason}]
    for index, item in enumerate(rows):
        row = dict(item["row"])
        row["source_index"], row["index"] = row["index"], index
        audit.append(row)
    hardware = sum(item["row"]["origin"] == "hardware_trace_fifo" for item in rows)
    audit.append({"kind": "footer", "complete": True, "records": len(rows),
                  "hardware_fifo_records": hardware, "checked_tail_host_completed": len(rows) - hardware,
                  "overflow": False, "p_error": 0, "final_transport_appended": True,
                  "source_sha256": digest(source), "selection": reason})
    Path(str(output) + ".audit.jsonl").write_text("".join(json.dumps(row, sort_keys=True) + "\n" for row in audit))


def discovery_choices(rows):
    by_window = defaultdict(Counter)
    identities = {}
    for item in rows:
        key = (item["q"], item["pc"])
        identity = (item["position"], item["is_call"], item["row"]["is_rvc"], item["row"]["instr"])
        require(key not in identities or identities[key] == identity, "selected PC has unstable width/call/instruction")
        identities[key] = identity
        by_window[item["q"]][item["pc"]] += 1
    choices = {q: min(counts, key=lambda pc: (-counts[pc], pc)) for q, counts in by_window.items()}
    return {q: {"start_pc": hex(q), "pc": hex(pc), "position": identities[q, pc][0],
                "is_call": identities[q, pc][1], "is_rvc": identities[q, pc][2], "instr": identities[q, pc][3],
                "misses": by_window[q][pc]} for q, pc in choices.items()}


def select_discovery(trace, meta, output):
    rows = checked_rows(trace, meta, "discovery")
    choices = discovery_choices(rows)
    selected = [item for item in rows if item["pc"] == int(choices[item["q"]]["pc"], 16)]
    write_subset(trace, output, "discovery", selected, "one actual CFI per window: most baseline misses, tie lower PC")
    return choices


def freeze_selection(cam, choices, trace, output):
    cam, output = Path(cam), Path(output)
    entries = json.loads(cam.read_text())["entries"]
    selection = {"schema": "rtl-hint-cam58-selection", "version": 1,
                 "public_cam_sha256": digest(cam), "source_raw_sha256": digest(trace),
                 "source_audit_sha256": digest(str(trace) + ".audit.jsonl"),
                 "source_raw": str(Path(trace).resolve()), "source_meta": str(Path(trace).with_suffix(".json").resolve()),
                 "entries": [{"id": entry["id"], **choices[int(entry["start_pc"], 16)]}
                             for entry in entries if entry["valid"]]}
    output.write_text(json.dumps(selection, indent=2, sort_keys=True) + "\n")
    return output


def selection_entries(selection, cam):
    value = json.loads(Path(selection).read_text())
    require(value.get("schema") == "rtl-hint-cam58-selection" and value.get("version") == 1,
            "invalid CAM58 selection version")
    require(value.get("public_cam_sha256") == digest(cam), "CAM58 frozen public CAM digest mismatch")
    source = Path(value["source_raw"])
    require(digest(source) == value["source_raw_sha256"] and digest(str(source) + ".audit.jsonl") == value["source_audit_sha256"],
            "CAM58 source evidence changed after identity selection")
    choices = discovery_choices(checked_rows(source, value["source_meta"], "discovery"))
    public = json.loads(Path(cam).read_text())["entries"]
    entries = {}
    for item in value["entries"]:
        index = item["id"]
        require(type(index) is int and 0 <= index < 16 and index not in entries, "duplicate/invalid CAM58 selection id")
        q = int(item["start_pc"], 16)
        require(public[index]["id"] == index and public[index]["valid"] and int(public[index]["start_pc"], 16) == q,
                "CAM58 selection mapping differs from public CAM")
        pc = int(item["pc"], 16)
        row = dict(item, cfi_position=item["position"], cfi_end_pc=hex(pc + (0 if item["is_rvc"] else 2)),
                   ftq_offset=(pc + (0 if item["is_rvc"] else 2) - q) // 2)
        checked_identity(row)
        require(q in choices and all(item.get(field) == expected for field, expected in choices[q].items()),
                "CAM58 selection differs from highest-miss actual CFI evidence")
        entries[index] = item
    require(set(entries) == {entry["id"] for entry in public if entry["valid"]}, "missing CAM58 selection identity")
    # Public selection independently defines the top16 windows by all-CFI miss counts.
    rows = checked_rows(source, value["source_meta"], "discovery")
    totals = Counter(item["q"] for item in rows)
    ranked = sorted(totals, key=lambda q: (-totals[q], q))[:16]
    require([int(public[index]["start_pc"], 16) for index in sorted(entries)] == ranked,
            "public CAM ids differ from all-branch window miss ranking")
    return entries


def filter_detail(trace, meta, cam, selection, output):
    entries = selection_entries(selection, cam)
    chosen = {int(row["start_pc"], 16): row for row in entries.values()}
    rows = checked_rows(trace, meta, "detail", cam)
    for item in rows:
        expected = chosen.get(item["q"])
        if expected and item["pc"] == int(expected["pc"], 16):
            require(all(item["row"][field] == expected[field] for field in ("is_rvc", "instr", "is_call")),
                    "selected CFI instruction/width changed between Discovery and Detail")
    selected = [item for item in rows if item["q"] in chosen and
                item["pc"] == int(chosen[item["q"]]["pc"], 16) and
                item["position"] == chosen[item["q"]]["position"] and item["is_call"] == chosen[item["q"]]["is_call"]]
    write_subset(trace, output, "detail", selected, "only the frozen CAM58 actual CFI identity")


def adapt_image(public_image, cam, selection, output, kind):
    public_image, output = Path(public_image), Path(output)
    data = bytearray(public_image.read_bytes())
    expected = 144 if kind == "cam" else 0x4190
    require(len(data) == expected and data[:8] == (b"H2PHINTP" if kind == "cam" else b"H2PHINTL") and not any(data[8:16]),
            "CAM58 adapter requires valid public52 intermediate image")
    entries = selection_entries(selection, cam)
    for index in range(16):
        word = struct.unpack_from("<Q", data, 16 + index * 8)[0]
        require(word >> 52 == 0, "public intermediate CAM has reserved bits")
        valid = bool(word & (1 << 51))
        require(valid == (index in entries), "public image CAM valid mapping changed")
        if valid:
            entry = entries[index]
            require(address(word & ((1 << 47) - 1)) == int(entry["start_pc"], 16), "public image CAM PC changed")
            word |= entry["position"] << 52 | int(entry["is_call"]) << 57
        else:
            require(word == 0, "invalid public CAM word nonzero")
        struct.pack_into("<Q", data, 16 + index * 8, word)
    data[:8] = CAM_MAGIC if kind == "cam" else LOAD_MAGIC
    output.write_bytes(data)
    report = {"schema": "rtl-hint-cam58-image", "version": 1, "kind": kind,
              "magic": bytes(data[:8]).decode(), "public_image_sha256": digest(public_image),
              "public_image": str(public_image.resolve()), "selection": str(Path(selection).resolve()),
              "public_cam": str(Path(cam).resolve()),
              "selection_sha256": digest(selection), "public_cam_sha256": digest(cam),
              "image_sha256": digest(output), "adapter_sha256": digest(__file__),
              "context_slot_bytes_unchanged": bytes(data[144:]) == public_image.read_bytes()[144:]}
    output.with_suffix(".adapter.json").write_text(json.dumps(report, indent=2, sort_keys=True) + "\n")
    return report

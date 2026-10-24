"""Run on ps1/ps2; test real adapter with unchanged public CLI intermediates."""
import hashlib
import json
import os
from pathlib import Path
import struct
import subprocess
import sys
import tempfile
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
import hint_cam58 as adapter

TOOL = Path(os.environ["HINT_PUBLIC_TOOL"])


class Cam58Test(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.root = Path(self.temp.name)
        self.sequence = 0

    def tearDown(self):
        self.temp.cleanup()

    def fixture(self, specifications, kind="discovery", cam=None):
        self.sequence += 1
        trace = self.root / f"trace-{self.sequence}.bin"
        width, final_bit = (7, 54) if kind == "discovery" else (22, 172)
        audit = [{"kind": "header", "version": 3, "producer": "rtl-ftq-checked-retirement",
                  "cam_identity": "checked_pc_width_instruction"}]
        data = bytearray()
        entries = json.loads(cam.read_text())["entries"] if cam else None
        for index, source in enumerate(specifications):
            q, pc = source.get("q", 0x1006), source.get("pc", 0x1008)
            rvc, instr = source.get("rvc", True), source.get("instr", 0x00078067)
            end = pc + (0 if rvc else 2)
            miss, valid = source.get("miss", True), source.get("valid", True)
            call = source.get("call", adapter.indirect_identity(instr))
            target = source.get("target", 0x2000)
            if kind == "discovery":
                value = ((pc >> 1) & ((1 << 47) - 1)) | (((pc-q)//2 & 127) << 47)
            else:
                pc_id = next(row["id"] for row in entries if row["valid"] and int(row["start_pc"], 16) == q)
                value = pc_id | ((target >> 1) << 4) | (((pc-q)//2 & 127) << 163) | (int(miss) << 170) | (int(valid) << 171)
            data += value.to_bytes(width, "little")
            audit.append({"kind": "record", "index": index, "retired_ordinal": index + 1,
                          "generation_id": index + 1, "ftq_tag": index % 128, "query_cycle": index + 100,
                          "start_pc": hex(q), "pc": hex(pc), "target": hex(target),
                          "baseline_miss": miss, "query_valid": valid, "origin": "hardware_trace_fifo",
                          "instr": instr, "is_rvc": rvc, "cfi_end_pc": hex(end),
                          "cfi_position": (end-(q & ~31))//2, "ftq_offset": (end-q)//2, "is_call": call})
        data += (1 << final_bit).to_bytes(width, "little")
        trace.write_bytes(data)
        audit.append({"kind": "footer", "complete": True, "records": len(specifications),
                      "hardware_fifo_records": len(specifications), "checked_tail_host_completed": 0,
                      "overflow": False, "p_error": 0})
        Path(str(trace) + ".audit.jsonl").write_text("".join(json.dumps(row) + "\n" for row in audit))
        meta = trace.with_suffix(".json")
        self.metadata(trace, meta, kind, cam)
        return trace, meta

    def metadata(self, trace, meta, kind, cam=None):
        data = {"schema": "hint-spec-trace", "version": 1, "hash_version": 7,
                "mask_order": ["ITH", "Call", "Ret", "LHR"], "kind": kind,
                "record_bytes": 7 if kind == "discovery" else 22, "termination": "final",
                "capacity_records": trace.stat().st_size // (7 if kind == "discovery" else 22),
                "data_sha256": adapter.digest(trace), "audit_sha256": adapter.digest(str(trace) + ".audit.jsonl"),
                "p_error": 0, "fetch_block_bytes": 64, "fetch_align_bytes": 32}
        if cam:
            data["cam_sha256"] = json.loads(cam.read_text())["image_sha256"]
        meta.write_text(json.dumps(data))

    def public(self, action, trace, meta, cam=None):
        output = self.root / (action + f"-{self.sequence}.json")
        image = output.with_suffix(".bin")
        command = [sys.executable, str(TOOL), action, "--trace", str(trace), "--meta", str(meta), "--output", str(output), "--image", str(image)]
        if cam:
            command += ["--cam", str(cam)]
        subprocess.run(command, capture_output=True, text=True, check=True)
        return output, image

    def prepare(self, specifications):
        trace, meta = self.fixture(specifications)
        choices = adapter.select_discovery(trace, meta, self.root / "selected.bin")
        cam, image = self.public("select-cam", trace, meta)
        selection = adapter.freeze_selection(cam, choices, trace, self.root / "selection.json")
        return trace, meta, cam, image, selection

    def mutate_audit(self, trace, meta, mutate):
        audit_path = Path(str(trace) + ".audit.jsonl")
        rows = [json.loads(line) for line in audit_path.read_text().splitlines()]
        mutate(rows)
        audit_path.write_text("".join(json.dumps(row) + "\n" for row in rows))
        value = json.loads(meta.read_text())
        value["audit_sha256"] = adapter.digest(audit_path)
        meta.write_text(json.dumps(value))

    def test_public_pipeline_and_fixed_detail(self):
        rows = [{"pc": 0x1008}, {"pc": 0x100a}, {"pc": 0x1008}]
        trace, meta, cam, image, selection = self.prepare(rows)
        adapted = self.root / "cam58.bin"
        adapter.adapt_image(image, cam, selection, adapted, "cam")
        word = struct.unpack_from("<Q", adapted.read_bytes(), 16)[0]
        self.assertEqual(adapted.read_bytes()[:8], b"H2PCAM58")
        self.assertEqual((word >> 52) & 31, 4)
        detail, dmeta = self.fixture(rows, "detail", cam)
        filtered = self.root / "filtered.bin"
        adapter.filter_detail(detail, dmeta, cam, selection, filtered)
        self.metadata(filtered, filtered.with_suffix(".json"), "detail", cam)
        self.assertEqual(len(adapter.checked_rows(filtered, filtered.with_suffix(".json"), "detail", cam)), 2)
        report, public_image = self.public("build", filtered, filtered.with_suffix(".json"), cam)
        final = self.root / "hint58.bin"
        binding = adapter.adapt_image(public_image, cam, selection, final, "load")
        self.assertTrue(binding["context_slot_bytes_unchanged"])
        self.assertEqual(final.read_bytes()[:8], b"H2PLD058")
        self.assertEqual(final.read_bytes()[144:], public_image.read_bytes()[144:])

    def test_rank_all_window_misses_and_tie_low_pc(self):
        rows = [{"q": 0x1000, "pc": 0x1002}] * 4 + [{"q": 0x1000, "pc": 0x1000}] * 4
        rows += [{"q": 0x1000+i*128, "pc": 0x1000+i*128} for i in range(1, 17) for _ in range(5)]
        trace, meta, cam, image, selection = self.prepare(rows)
        entries = adapter.selection_entries(selection, cam)
        self.assertEqual(entries[0]["pc"], "0x1000")
        self.assertEqual(len(entries), 16)
        self.assertNotIn(hex(0x1000+16*128), [row["start_pc"] for row in entries.values()])

    def test_cross_boundary_and_call(self):
        trace, meta, cam, image, selection = self.prepare([{"q": 0x1000, "pc": 0xffe, "rvc": False, "instr": 0x000780e7}])
        result = self.root / "call58.bin"
        adapter.adapt_image(image, cam, selection, result, "cam")
        word = struct.unpack_from("<Q", result.read_bytes(), 16)[0]
        self.assertEqual(word >> 52, 1 << 5)  # position0, isCall1

    def test_rvc_vs_decompressed_width(self):
        for rvc, position in ((True, 4), (False, 5)):
            trace, meta = self.fixture([{"rvc": rvc}])
            self.assertEqual(adapter.checked_rows(trace, meta, "discovery")[0]["position"], position)

    def test_raw_record_order_is_not_execution_order(self):
        trace, meta = self.fixture([{"pc": 0x1008}, {"pc": 0x100a}])
        self.mutate_audit(trace, meta, lambda rows: rows[1].update(retired_ordinal=100))
        self.assertEqual(len(adapter.checked_rows(trace, meta, "discovery")), 2)

    def test_sidecar_corruption_rejected(self):
        mutations = {
            "missing-width": lambda a: a[1].pop("is_rvc"),
            "bad-width": lambda a: a[1].update(is_rvc=False),
            "bad-position": lambda a: a[1].update(cfi_position=5),
            "bad-call": lambda a: a[1].update(is_call=True),
            "bad-offset": lambda a: a[1].update(ftq_offset=8),
            "bad-pc": lambda a: a[1].update(pc="0x100a"),
            "old-version": lambda a: a[0].update(version=2),
            "bad-producer": lambda a: a[0].update(producer="guessed"),
            "missing-row": lambda a: a.pop(1),
            "duplicate-identity": lambda a: a[2].update({k: a[1][k] for k in ("retired_ordinal", "generation_id", "ftq_tag", "query_cycle")}),
            "coroutine": lambda a: a[1].update(instr=0x000280e7, is_call=True),
        }
        for name, mutate in mutations.items():
            with self.subTest(name=name):
                trace, meta = self.fixture([{}, {}])
                self.mutate_audit(trace, meta, mutate)
                with self.assertRaises(ValueError):
                    adapter.checked_rows(trace, meta, "discovery")

    def test_raw_or_audit_sha_rejected(self):
        trace, meta = self.fixture([{}])
        trace.write_bytes(bytes([trace.read_bytes()[0] ^ 1]) + trace.read_bytes()[1:])
        with self.assertRaisesRegex(ValueError, "raw digest"):
            adapter.checked_rows(trace, meta, "discovery")
        trace, meta = self.fixture([{}])
        Path(str(trace) + ".audit.jsonl").write_text(Path(str(trace) + ".audit.jsonl").read_text() + "\n")
        with self.assertRaisesRegex(ValueError, "sidecar digest"):
            adapter.checked_rows(trace, meta, "discovery")

    def test_missing_or_modified_final_rejected(self):
        trace, meta = self.fixture([{}])
        trace.write_bytes(trace.read_bytes()[:-7] + bytes(7))
        self.metadata(trace, meta, "discovery")
        with self.assertRaisesRegex(ValueError, "final"):
            adapter.checked_rows(trace, meta, "discovery")

    def test_selected_source_changes_rejected(self):
        trace, meta, cam, image, selection = self.prepare([{}])
        source = json.loads(selection.read_text())
        source["entries"][0]["is_call"] = True
        selection.write_text(json.dumps(source))
        with self.assertRaises(ValueError):
            adapter.adapt_image(image, cam, selection, self.root / "bad.bin", "cam")

    def test_code_change_rejected(self):
        trace, meta = self.fixture([{}, {"instr": 0x000780e7}])
        with self.assertRaisesRegex(ValueError, "unstable"):
            adapter.select_discovery(trace, meta, self.root / "bad.bin")
        trace, meta, cam, image, selection = self.prepare([{}])
        detail, dmeta = self.fixture([{"instr": 0x000780e7}], "detail", cam)
        with self.assertRaisesRegex(ValueError, "changed between"):
            adapter.filter_detail(detail, dmeta, cam, selection, self.root / "bad-detail.bin")

    def test_call_pop_classification(self):
        for rd, rs, expected in ((1, 10, True), (5, 10, True), (1, 1, True), (5, 5, True), (0, 10, False)):
            self.assertEqual(adapter.indirect_identity(0x67 | (rd << 7) | (rs << 15)), expected)
        for rd, rs in ((1, 5), (5, 1), (0, 1), (0, 5)):
            with self.assertRaisesRegex(ValueError, "PopAndPush"):
                adapter.indirect_identity(0x67 | (rd << 7) | (rs << 15))
        self.assertTrue(adapter.indirect_identity(0x9082))  # C.JALR x1, no pop
        with self.assertRaisesRegex(ValueError, "PopAndPush"):
            adapter.indirect_identity(0x9282)  # C.JALR x5, pop and push

    def test_canonical_and_wrap_boundaries(self):
        for q in (0xffff800000001006, 0x7fffffffffc0):
            trace, meta = self.fixture([{"q": q, "pc": q}])
            self.assertEqual(adapter.checked_rows(trace, meta, "discovery")[0]["q"], q)
        for q, pc, rvc in ((0x800000000000, 0x7ffffffffffe, False),
                           (0xfffffffffffffff0, 0xfffffffffffffff0, True),
                           (0xffffffffffffffe0, 0xfffffffffffffffe, False)):
            with self.subTest(q=hex(q)):
                end = pc + (0 if rvc else 2)
                row = {"start_pc": hex(q), "pc": hex(pc), "is_rvc": rvc, "is_call": False,
                       "instr": 0x78067, "cfi_end_pc": hex(end), "cfi_position": (end-(q & ~31))//2,
                       "ftq_offset": (end-q)//2}
                with self.assertRaisesRegex(ValueError, "canonical|wrap|invalid"):
                    adapter.checked_identity(row)


if __name__ == "__main__":
    unittest.main(verbosity=2)

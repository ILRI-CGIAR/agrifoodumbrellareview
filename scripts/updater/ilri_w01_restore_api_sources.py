#!/usr/bin/env python3
"""Restore and verify ILRI W01 API source inputs from restricted Zenodo archives."""
import json, os, hashlib, io, tarfile, urllib.request
from pathlib import Path

cfg=json.loads(Path("config/ilri_w01_source_archives.json").read_text())
token=os.getenv("ZENODO_ACCESS_TOKEN","")
if not token: raise RuntimeError("Missing ZENODO_ACCESS_TOKEN")
root=Path("outputs/w01_api_restored")
root.mkdir(parents=True,exist_ok=True)
results={}
def request_json(url):
    req=urllib.request.Request(url,headers={"Authorization":f"Bearer {token}"})
    with urllib.request.urlopen(req,timeout=120) as r:return json.load(r)
for source, spec in cfg["api_sources"].items():
    dep_id=spec["zenodo_record_id"]
    dep=request_json(f"https://zenodo.org/api/deposit/depositions/{dep_id}")
    if not dep.get("submitted") or dep.get("metadata",{}).get("access_right")!="restricted":
        raise RuntimeError(f"{source}: expected a published restricted archive")
    matching=[f for f in dep.get("files",[]) if f.get("filename",f.get("key"))==spec["archive"]]
    if len(matching)!=1:raise RuntimeError(f"{source}: missing or duplicate {spec['archive']}")
    entry=matching[0]
    bucket=dep.get("links",{}).get("bucket","")
    if not bucket:raise RuntimeError(f"{source}: no authenticated bucket URL")
    from urllib.parse import quote
    url=bucket.rstrip("/")+"/"+quote(spec["archive"],safe="")
    req=urllib.request.Request(url,headers={"Authorization":f"Bearer {token}"})
    with urllib.request.urlopen(req,timeout=1800) as response:data=response.read()
    checksum=entry.get("checksum","")
    if checksum.startswith("md5:") and hashlib.md5(data).hexdigest()!=checksum[4:]:
        raise RuntimeError(f"{source}: Zenodo MD5 mismatch")
    with tarfile.open(fileobj=io.BytesIO(data),mode="r:gz") as archive:
        members=[m for m in archive.getmembers() if m.isfile()]
        targets=[m for m in members if Path(m.name).name=="unique_records.jsonl"]
        if len(targets)!=1:raise RuntimeError(f"{source}: missing/ambiguous unique_records.jsonl")
        fp=archive.extractfile(targets[0])
        raw=fp.read()
    lines=[line for line in raw.splitlines() if line.strip()]
    expected=spec["expected_records"]
    if len(lines)!=expected:
        raise RuntimeError(f"{source}: expected {expected} records, found {len(lines)}")
    first=json.loads(lines[0])
    outdir=root/source
    outdir.mkdir(parents=True,exist_ok=True)
    (outdir/"unique_records.jsonl").write_bytes(raw)
    results[source]={"record_count":len(lines),"sha256":hashlib.sha256(raw).hexdigest(),
        "first_record_fields":list(first.keys()),"zenodo_record_id":dep_id,
        "path":str(outdir/"unique_records.jsonl")}
    print(f"PASS W01 source archive restore: {source}: {len(lines)} records; fields {list(first.keys())}",flush=True)
Path("outputs/w01_api_restore_inventory.json").write_text(json.dumps(results,indent=2))

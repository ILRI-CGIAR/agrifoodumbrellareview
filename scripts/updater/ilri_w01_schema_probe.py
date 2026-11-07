#!/usr/bin/env python3
import json
from pathlib import Path
def fields(x,depth=0):
 if depth>2:return type(x).__name__
 if isinstance(x,dict):return {k:fields(v,depth+1) for k,v in list(x.items())[:70]}
 if isinstance(x,list):return {"list_length":len(x),"element":fields(x[0],depth+1) if x else None}
 return type(x).__name__
for src in ("scopus","lens","openalex","agricola","wos"):
 p=Path("w01_archive_inputs")/src/"unique_records.jsonl"
 if not p.exists():raise RuntimeError(f"Missing {p}")
 with p.open() as f:row=json.loads(f.readline())
 print(f"SCHEMA {src}: {json.dumps(fields(row))}",flush=True)

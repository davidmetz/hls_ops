#!/usr/bin/python3
import glob

SPLITTER = "// ----- 8< ----- FILE"
for path in glob.glob("*.sv"):
    with open(path, "r") as f:
        verilog = f.read()
    if SPLITTER in verilog:
        print(path)
        verilog = verilog.split(SPLITTER)[0]
        with open(path, "w") as f:
            f.write(verilog)
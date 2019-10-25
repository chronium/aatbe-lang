#!/usr/bin/env python3

import os
from itertools import chain
from collections import namedtuple
from subprocess import Popen, PIPE, DEVNULL
import datetime

SYM_BOX = '☐'
SYM_TICK = '✓'
SYM_FAIL = '✗'

TestCase = namedtuple('TestCase', 'name suite path src')
TestStatistics = namedtuple('TestStatistics', 'name status')

def buildCompiler():
    print("%s Building compiler..." % SYM_BOX)
    proc = Popen(
        ["cargo", "build"],
        stdout=DEVNULL,
        stderr=PIPE,
        close_fds=True
    )
    (output, err) = proc.communicate()
    exitCode = proc.wait()
    if exitCode != 0:
        print("%s Compiler build failed:" % SYM_FAIL)
        print(err)
        exit(1)
    print("%s Compiler build succeeded." % SYM_TICK)

def gatherTestCases():
    def getFileNames(tpl):
        dirname, _, filenames = tpl
        return [os.path.join(dirname, file) for file in filenames]
    return list(chain(*map(getFileNames, os.walk("spec"))))

def parseTest(file):
    with open(file, "r") as f:
        content = f.read()
    def isSpectorComment(line):
        line = line.strip()
        if not line.startswith('//'): return False
        line = line.split('//', 1)[1].strip()
        return True if line.startswith('spector:') else False
    props = dict()
    for line in list(filter(isSpectorComment, content.splitlines())):
        values = line.split('spector:', 1)[1].split(' ', 1)
        if len(values) != 2: continue
        key, val = values
        props[key] = val
    name = props.get('name', os.path.basename(file))
    suite = os.path.dirname(file).split('spec/', 1)[1]
    return TestCase(name, suite, file, content)

def runTest(file):
    test = parseTest(file)
    print("[%s] %s %s" % (test.suite, SYM_BOX, test.name))
    proc = Popen(
        ["cargo", "run", "--", test.path],
        stdout=DEVNULL,
        stderr=PIPE
    )
    (output, err) = proc.communicate()
    exitCode = proc.wait()
    if exitCode == 0:
        print("[%s] %s %s" % (test.suite, SYM_TICK, test.name))
    else:
        print("[%s] %s %s (code %s)" % (test.suite, SYM_FAIL, test.name, exitCode))
        print(err)
    return TestStatistics(test.name, exitCode == 0)

def runTestCases(files):
    print()
    results = [runTest(file) for file in files]
    print("\nSummary:")
    getPassed = lambda res: res.status
    getFailed = lambda res: not res.status
    print("Ran %d test%s (%d passed, %d failed)." % (
        len(results),
        "s" if len(results) != 1 else "",
        len(list(filter(getPassed, results))),
        len(list(filter(getFailed, results)))
    ))

if __name__ == "__main__":
    buildCompiler()
    files = gatherTestCases()
    runTestCases(files)

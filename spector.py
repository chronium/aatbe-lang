#!/usr/bin/env python3

import os
from itertools import chain
from functools import reduce
from collections import namedtuple
from subprocess import Popen, PIPE, DEVNULL
from datetime import datetime, timedelta

# Character Sequences
SYM_TICK = '\033[32m✓\033[0m'
SYM_INFO = '\033[33m?\033[0m'
SYM_FAIL = '\033[31m✗\033[0m'

# Type Definitions
TestCase = namedtuple('TestCase', 'options suite path src')
TestStatistics = namedtuple('TestStatistics', 'name status time errors')

def buildCompiler():
    """Build cargo before testing"""

    # Spawn cargo build
    proc = Popen(
        ["cargo", "build"],
        stdout=DEVNULL,
        stderr=PIPE,
        close_fds=True
    )

    # Grab process output
    (output, err) = proc.communicate()

    # Wait for process to exit
    exitCode = proc.wait()

    # Check exit code
    if exitCode != 0:
        print("%s Compiler build failed:" % SYM_FAIL)
        print(err)
        exit(1)
    print("%s Compiler build succeeded." % SYM_TICK)

def gatherTestCases():
    """Find test cases in spec/**/*"""
    def getFileNames(tpl):
        """Build filenames from os.walk data"""
        dirname, _, filenames = tpl
        return [os.path.join(dirname, file) for file in filenames]
    return sorted(chain(*map(getFileNames, os.walk("spec"))))

def parseTest(file):
    """Generate structured test cases from source files"""

    # Read file
    with open(file, "r") as f:
        content = f.read()

    def isSpectorComment(line):
        """Test whether a line contains a spector property"""
        line = line.strip()
        if not line.startswith('//'): return False
        line = line.split('//', 1)[1].strip()
        return True if line.startswith('spector:') else False

    # Define property dictionary
    props = dict()

    # Extract properties from spector comments
    for line in list(filter(isSpectorComment, content.splitlines())):
        values = line.split('spector:', 1)[1].split(' ', 1)
        if len(values) == 1:
            props[values[0]] = ''
        else:
            key, val = values
            props[key] = val

    # Get relevant properties
    name = props.get('name', os.path.basename(file))
    shouldfail = 'shouldfail' in props

    # Build final options
    opts = {
        'name': name,
        'shouldfail': shouldfail,
    }

    # Determine test suite
    suite = os.path.dirname(file).split('spec/', 1)[1]

    # Build the structured test case
    return TestCase(opts, suite, file, content)

def runTest(test, testCaseMaxLen):
    """Run a single test"""

    # Try building the test case
    proc = Popen(
        ["cargo", "run", "--quiet", "--", test.path],
        stdout=PIPE,
        stderr=PIPE
    )

    # Measure starting time
    startTime = datetime.now()

    # Wait for process to exit
    (output, err) = proc.communicate()
    exitCode = proc.wait()

    # Measure exiting time
    endTime = datetime.now()

    # Get time difference
    diff = endTime - startTime
    diffMs = diff.seconds * 1000 + diff.microseconds / 1000
    diffStr = "{}ms".format(str(int(diffMs))).ljust(6)

    # Determine success
    success = exitCode == 0 or (test.options['shouldfail'] and exitCode != 0)

    # Print results
    errors = None
    if success:
        print("{suite} {symbol} {time} {name}".format(
            suite = test.suite.ljust(testCaseMaxLen, ' '),
            symbol = SYM_TICK,
            time = diffStr,
            name = test.options['name']
        ))
    else:
        print("{suite} {symbol} {time} {name} (code {code})".format(
            suite = test.suite.ljust(testCaseMaxLen, ' '),
            symbol = SYM_FAIL,
            time = diffStr,
            name = test.options['name'],
            code = exitCode
        ))
        errors = err.decode('utf-8').replace("\\n", "\n")
        errors = ''.join(filter(lambda line: 'panicked at' in line, errors.splitlines()))
        print("{padding}\_ {symbol} {error}".format(
            padding = ''.ljust(testCaseMaxLen - 2, ' '),
            symbol = SYM_INFO,
            error=errors
        ))
        # print(str(output).replace("\\n", "\n"))

    # Build test statistics
    return TestStatistics(test.options['name'], success, diffMs, errors)

def runTestCases(tests, testCaseMaxLen):
    """Run tests and print a summary"""
    print()

    # Run all tests
    results = [runTest(test, testCaseMaxLen) for test in tests]

    # Print errors
    # print("\nErrors:")
    # for test in filter(lambda res: res.errors is not None, results):
    #     print("[%s]:\n%s" % (test.name, test.errors))
    #     pass

    # Print summary
    print("\nSummary:")
    getPassed = lambda res: res.status
    getFailed = lambda res: not res.status
    elapsedMs = reduce(lambda acc, res: acc + res.time, results, 0)
    print("Ran %d test%s in %dms (\033[32m%d\033[0m passed, \033[31m%d\033[0m failed)." % (
        len(results),
        "s" if len(results) != 1 else "",
        elapsedMs,
        len(list(filter(getPassed, results))),
        len(list(filter(getFailed, results)))
    ))

# Entry Point
if __name__ == "__main__":
    buildCompiler()
    files = gatherTestCases()
    tests = [parseTest(file) for file in files]
    maxlen = max([len(test.suite) for test in tests])
    runTestCases(tests, maxlen)

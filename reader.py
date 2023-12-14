import sys
import pprint
import json
from subprocess import Popen, PIPE

sys.path.append('/Library/Frameworks/Python.framework/Versions/3.6/lib/python3.6/site-packages/')
from solidity_parser import parser

if __name__ == "__main__":
    sourceUnit = parser.parse_file(sys.argv[1], loc=False) # loc=True -> add location information to ast nodes
    pprint.pprint(sourceUnit)

class ReaderConsensys:
    ## This class relies on the solidity parser by Consensys.
    ## https://github.com/Consensys/python-solidity-parser

    def __init__(self):
        self.sourceUnit = dict()

    def load(self, filepath):
        self.sourceUnit = parser.parse_file(filepath, loc=False)

    def toObject(self):
        return parser.objectify(self.sourceUnit)
    
    def toPrettyPrintString(self):
        return pprint.pprint(self.sourceUnit)
    
    def toString(self):
        return str(self.sourceUnit)

class ReaderSolc:
    ## This class relies on the official solc command's --ast-compact-json option.

    def __init__(self):
        self.jsonObj = dict()
        self.sourceText = ""

    def load(self, filepath):
        process = Popen(["solc", "--ast-compact-json", filepath], stdout=PIPE)
        (self.output, self.err) = process.communicate()
        self.exit_code = process.wait()
        self.outputBodyStr = self.output.decode('utf-8').split('\n')[-1]
        self.jsonObj = json.loads(self.outputBodyStr)
        with open(filepath, "r") as f:
            self.sourceText = f.read()

    def byteOffsetToLineIndex(self, offset, filepath):
        indices = [i for i,c in enumerate(self.sourceText) if c == '\n']
        lineIndex = len([ i for i in indices if i < offset ])
        return lineIndex

    def toJson(self):
        return self.jsonObj


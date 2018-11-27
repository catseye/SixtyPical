# encoding: UTF-8

import re


class SixtyPicalSyntaxError(ValueError):
    def __init__(self, filename, line_number, message):
        super(SixtyPicalSyntaxError, self).__init__(filename, line_number, message)

    def __str__(self):
        return "{}, line {}: {}".format(self.args[0], self.args[1], self.args[2])


class Scanner(object):
    def __init__(self, text, filename):
        self.text = text
        self.filename = filename
        self.token = None
        self.type = None
        self.pos = 0
        self.line_number = 1
        self.scan()

    def scan_pattern(self, pattern, type, token_group=1):
        pattern = r'(' + pattern + r')'
        regexp = re.compile(pattern, flags=re.DOTALL)
        match = regexp.match(self.text, pos=self.pos)
        if not match:
            return False
        else:
            self.type = type
            self.token = match.group(token_group)
            self.pos += len(match.group(0))
            self.line_number += self.token.count('\n')
            return True

    def scan(self):
        self.scan_pattern(r'[ \t\n\r]*', 'whitespace')
        while self.scan_pattern(r'\/\/.*?[\n\r]', 'comment'):
            self.scan_pattern(r'[ \t\n\r]*', 'whitespace')
        if self.pos >= len(self.text):
            self.token = None
            self.type = 'EOF'
            return
        if self.scan_pattern(r'\,|\@|\+|\:|\<|\>|\{|\}|\[|\]|\^', 'operator'):
            return
        if self.scan_pattern(r'\d+', 'integer literal'):
            return
        if self.scan_pattern(r'\$([0-9a-fA-F]+)', 'integer literal', token_group=2):
            # ecch
            self.token = str(eval('0x' + self.token))
            return
        if self.scan_pattern(r'\"(.*?)\"', 'string literal', token_group=2):
            return
        if self.scan_pattern(r'\w+', 'identifier'):
            return
        if self.scan_pattern(r'.', 'unknown character'):
            return
        else:
            raise AssertionError("this should never happen, self.text=({}), self.pos=({})".format(self.text, self.pos))

    def expect(self, token):
        if self.token == token:
            self.scan()
        else:
            self.syntax_error("Expected '{}', but found '{}'".format(token, self.token))

    def on(self, *tokens):
        return self.token in tokens

    def on_type(self, type):
        return self.type == type

    def check_type(self, type):
        if not self.type == type:
            self.syntax_error("Expected {}, but found '{}'".format(self.type, self.token))

    def consume(self, token):
        if self.token == token:
            self.scan()
            return True
        else:
            return False

    def syntax_error(self, msg):
        raise SixtyPicalSyntaxError(self.filename, self.line_number, msg)

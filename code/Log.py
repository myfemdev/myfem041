#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""Log class."""
import datetime
import shutil
import logging

class Log():
    """Log class."""
    filename1 = 'log.txt'
    filename2 = 'log.old.txt'
    def __init__(self):
        self.loggers = []
        self.file = None

    def start(self, problem):
        """Start log."""
        if self.file is not None:
            end()
        try:
            self.file = open(self.filename1, "a")
        except IOError as x:
            logging.warning(f"Log.py: {repr(x)}")
            pass
        text = 'LOG START: '+Log.get_now()
        if isinstance(problem, str):
            text += ' '+problem
        text += '\n'
        logging.debug(text)
        return self.file is not None

    def end(self):
        """End log."""
        self.add_text('LOG END:   '+Log.get_now()+'\n')
        self.clear_text()
        if self.file is not None:
            self.file.close()
            self.file = None

    def change(self):
        """Move 'log.txt' to 'log.old.txt'."""
        try:
            shutil.move(self.filename1, self.filename2)
        except IOError:
            pass

    def add_logger(self, logger):
        """Add logger."""
        self.loggers.append(logger)

    def clear_text(self):
        """Clear text."""
        for logger in self.loggers:
            logger.clear_text()

    def add_text(self, txt):
        """Add text.
        
        Notes:
            It failed in some cases when printing binary '\u2029', '\u2013'.
            utf-8 could be incorrect in Windows, but other codings does not have, for example, hat{h}.
        """
        txte = txt#.encode('utf-8') # Python 2
        if len(txte)>0:
            logging.debug(txte)
        if self.file is not None:
            self.file.write(txt)#.encode('utf-8'))  # Python 2
            self.file.flush()
        for logger in self.loggers:
            logger.add_text(txt)

    @staticmethod
    def get_now():
        """Get current date and time.
        
        Note:
            Static method.
        """
        return datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S')
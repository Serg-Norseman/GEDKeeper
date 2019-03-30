/* LogFile.cs
 * 
 * Copyright 2009 Alexander Curtis <alex@logicmill.com>
 * This file is part of GEDmill - A family history website creator
 * 
 * GEDmill is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * GEDmill is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GEDmill.  If not, see <http://www.gnu.org/licenses/>.
 *
 *
 * History:  
 * 10Dec08 AlexC          Migrated from GEDmill 1.10
 *
 */

using System;
using System.IO;

namespace GEDmill
{
    /// <summary>
    /// Manages the debug message log file. Log messages are optionally written to a file on disk, and also the most
    /// recent messages are stored to be displayed by the applications exception handler.
    /// </summary>
    public class LogFile
    {
        // Used to set the degree of verbosity of the tracing
        public enum EDebugLevel
        {
            All,
            Note,
            Warning,
            Error,
            None
        };

        // Bitmasks define the areas of the application that we are interested in logging
        public static uint DT_ALL = 0xffffffff;
        public static uint DT_GEDCOM = 0x00000001;
        public static uint DT_HTML = 0x00000002;
        public static uint DT_FTP = 0x00000004;
        public static uint DT_CONFIG = 0x00000008;
        public static uint DT_APP = 0x00000010;
        public static uint DT_NONE = 0x00000000;

        // Number of most recent messages to keep
        private static int LINE_BUFSIZE = 10;

        // Number of characters to display per line when line-breaking the recent messages for display purposes.
        private static int ERROR_LINE_LENGTH = 135;

        // Filename of the log file on disk
        private static LogFile fLogFilename;

        // Set to level of debug output needed
        private EDebugLevel fDebugLevel;

        // The areas of the application to log
        private uint fDebugFilter;

        // The stream writer to write the log file to the file stream
        private StreamWriter fWriter;

        // The file stream to write the log file to the disk
        private FileStream fStream;

        // The circular buffer of most recent log messages
        private string[] fLatestLines;

        // Index into the circular buffer
        private int fLatestLineIndex;


        // Constructor, default values
        private LogFile()
        {
            fDebugFilter = DT_NONE;
            fDebugLevel = EDebugLevel.None;
            fWriter = null;
            fStream = null;
            fLatestLines = new string[LINE_BUFSIZE]; // TODO: Check all entries initialise
            fLatestLineIndex = 0;
        }

        // Singleton
        public static LogFile Instance
        {
            get {
                if (fLogFilename == null) {
                    fLogFilename = new LogFile();
                }
                return fLogFilename;
            }
        }

        // Opens the log file on disk
        public void StartLogFile(string filename)
        {
            fStream = new FileStream(filename, FileMode.Create);
            fWriter = new StreamWriter(fStream);
        }

        // Closes the log file on disk
        public void StopLogFile()
        {
            if (fWriter != null) {
                fWriter.Close();
            }
            if (fStream != null) {
                fStream.Close();
            }
            fStream = null;
            fWriter = null;
        }

        // Sets level of verbosity
        public void SetLogLevel(EDebugLevel d)
        {
            fDebugLevel = d;
        }

        // Sets filter for which areas of the app to trace
        public void SetDebugAllowFilter(uint debugFilter)
        {
            fDebugFilter = debugFilter;
        }

        // Writes a line to the log file
        public void WriteLine(uint t, EDebugLevel d, string s)
        {
            WriteLine(t, d, s, false);
        }

        // Writes a line to the log file, but not to the log buffer that gets printed by the exception handler
        public void WriteLine(uint t, EDebugLevel d, string s, bool b_exclude_from_exception_message)
        {
            if (b_exclude_from_exception_message == false) {
                fLatestLines[fLatestLineIndex] = s;
                fLatestLineIndex++;
                if (fLatestLineIndex == LINE_BUFSIZE) {
                    fLatestLineIndex = 0;
                }
            }
            try {
                if ((t & fDebugFilter) != 0) {
                    if (d >= fDebugLevel) {
                        if (fWriter != null) {
                            fWriter.WriteLine(s);
                        }
                        System.Diagnostics.Trace.WriteLine(System.DateTime.Now.ToFileTime() + " " + s);
                    }
                }
            } catch (Exception e) {
                System.Diagnostics.Trace.WriteLine(e.Message);
            }
        }

        // Prints the log buffer of most recent lines written to the log file (or not written if no file active)
        public string ErrorReport()
        {
            int nLatestLine = fLatestLineIndex;
            string s = String.Concat(MainForm.SoftwareName, "\r\n");
            string t;
            do {
                t = fLatestLines[nLatestLine++];
                if (t != null && t != "") {
                    do {
                        int tlen = t.Length;
                        if (tlen > ERROR_LINE_LENGTH) {
                            tlen = t.LastIndexOf(' ', ERROR_LINE_LENGTH);
                            if (tlen == -1) {
                                tlen = ERROR_LINE_LENGTH;
                            }

                        }
                        string tsub = t.Substring(0, tlen);
                        t = t.Substring(tlen);
                        if (t != "") {
                            // Indent non blank strings. Blank string signifies end of loop.
                            t = " " + t;
                        }
                        s = String.Concat(s, tsub, "\r\n");
                    }
                    while (t != "");
                }

                if (nLatestLine == LINE_BUFSIZE) {
                    nLatestLine = 0;
                }
            }
            while (nLatestLine != fLatestLineIndex);

            return s;
        }
    }
}

/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using System.Collections.Generic;
using System.Text;

namespace GDModel
{
    /// <summary>
    /// Container for a list of strings with some additional features.
    /// </summary>
    public sealed class GDMLines : List<string>, IGDMLines
    {
        private const string LINE_BREAK = "\r\n";


        public string Text
        {
            get {
                var buffer = new StringBuilder();

                for (int i = 0, num = base.Count; i < num; i++) {
                    if (buffer.Length > 0) {
                        buffer.Append(LINE_BREAK);
                    }
                    buffer.Append(base[i]);
                }

                return buffer.ToString();
            }
            set {
                Clear();
                ParseLine(value);
            }
        }


        public GDMLines()
        {
        }

        public GDMLines(string text) : this()
        {
            Text = text;
        }

        public GDMLines(string[] list) : this()
        {
            for (int i = 0; i < list.Length; i++) {
                ParseLine(list[i]);
            }
        }

        public override int GetHashCode()
        {
            var result = new HashCode();

            for (int i = 0, num = base.Count; i < num; i++) {
                result.Add(base[i]);
            }

            return result.ToHashCode();
        }

        public bool IsEmpty()
        {
            return (base.Count <= 0) || (base.Count == 1 && (base[0] == "" || base[0] == LINE_BREAK));
        }

        public void Assign(GDMLines source)
        {
            if (source == null) return;

            Clear();
            AddRange(source);
        }

        /// <summary>
        /// A function for safely platform-independent string splitting by newlines.
        /// Because in some UI (Eto.Forms) string on Windows may come with '\n' delimiter instead of '\r\n'.
        ///
        /// Variants: \n - Unix & Unix-like (including MacOSX aka macOS?), \r\n - Windows, \r - classic MacOS before 2001.
        /// </summary>
        private void ParseLine(string line)
        {
            if (line == null)
                return;

            if (line == string.Empty) {
                Add(line);
                return;
            }

            int inPos = 0, inLen = line.Length;
            int outPos = 0, outLen = 0;
            bool wasLF = false;
            while (true) {
                char ch = (inPos >= inLen) ? '\0' : line[inPos];
                inPos += 1;

                if (ch == '\n' || ch == '\0') {
                    if (outLen > 0 || wasLF) {
                        string piece = line.Substring(outPos, outLen);
                        Add(piece);
                    }
                    outPos = inPos;
                    outLen = 0;
                    wasLF = true;
                } else if (ch == '\r') {
                    // skipped
                } else {
                    outLen += 1;
                    wasLF = false;
                }

                if (ch == '\0') break;
            }
        }
    }
}

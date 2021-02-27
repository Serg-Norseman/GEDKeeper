/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2021 by Sergey V. Zhdanovskih.
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

using System.Collections.Generic;
using System.Text;

namespace GDModel
{
    /// <summary>
    /// Container for a list of strings with some additional features.
    /// </summary>
    public sealed class GDMLines : List<string>
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

                int start = 0;
                int lbLen = LINE_BREAK.Length;
                int pos = value.IndexOf(LINE_BREAK);

                while (pos >= 0) {
                    string s = value.Substring(start, pos - start);
                    Add(s);
                    start = pos + lbLen;
                    pos = value.IndexOf(LINE_BREAK, start);
                }

                if (start <= value.Length) {
                    string s = value.Substring(start, (value.Length - start));
                    Add(s);
                }
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
            AddRange(list);
        }

        public bool IsEmpty()
        {
            return (base.Count <= 0);
        }

        public void Assign(GDMLines source)
        {
            if (source == null) return;

            Clear();
            AddRange(source);
        }
    }
}

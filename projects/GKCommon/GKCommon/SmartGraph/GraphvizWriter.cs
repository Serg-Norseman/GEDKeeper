/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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

using System.IO;
using System.Text;

namespace GKCommon.SmartGraph
{
    public sealed class GraphvizWriter
    {
        private readonly StringBuilder fBuffer;

        public GraphvizWriter(string name, params string[] options)
        {
            fBuffer = new StringBuilder();
            fBuffer.AppendLine("digraph " + name.Trim().Replace(' ', '_') + "{");
            foreach (string option in options)
            {
                fBuffer.AppendLine("\t" + option + ";");
            }
        }

        public void WriteEdge(string frm, string to)
        {
            fBuffer.AppendLine(string.Format("\"{0}\" -> \"{1}\";", frm, to));
        }

        public void WriteNode(string id, string name, string style, string color, string shape)
        {
            fBuffer.AppendLine(string.Format("\"{0}\" [ label=\"{1}\",shape=\"{2}\",style=\"{3}\",color=\"{4}\" ];", id, name, shape, style, color));
        }

        public void SaveFile(string path)
        {
            fBuffer.AppendLine("}");
            using (StreamWriter sw = new StreamWriter(path, false, Encoding.GetEncoding(1251)))
            {
                sw.Write(fBuffer.ToString());
            }
        }
    }
}
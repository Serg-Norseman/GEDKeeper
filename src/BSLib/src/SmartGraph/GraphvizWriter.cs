/*
 *  "SmartGraph", the small library for store and manipulations over graphs.
 *  Copyright (C) 2011-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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

namespace BSLib.SmartGraph
{
	public sealed class GraphvizWriter
	{
		private readonly StringBuilder fBuffer;

		public GraphvizWriter(string name)
		{
            fBuffer = new StringBuilder();
			fBuffer.AppendLine("digraph " + name.Trim().Replace(' ', '_')+"{");
		}

		public GraphvizWriter(string name, string[] Options) : this(name)
		{
			fBuffer.AppendLine("digraph " + name.Trim().Replace(' ', '_') + "{");
			foreach (string option in Options)
			{
				fBuffer.AppendLine("\t" + option + ";");
			}
		}

		public void ConnNode(string From, string To)
		{
			fBuffer.AppendLine(string.Format("\"{0}\" -> \"{1}\";",From,To));
		}

		public void ListNode(string ID, string Name, string style, string color, string shape)
		{
			fBuffer.AppendLine(string.Format("\"{0}\" [ label=\"{1}\",shape=\"{2}\",style=\"{3}\",color=\"{4}\" ];", ID, Name, shape,style,color));
		}

		public void SaveFile(string path)
		{
			fBuffer.AppendLine("}");
			using (StreamWriter SW = new StreamWriter(path, false, Encoding.GetEncoding(1251)))
			{
				SW.Write(fBuffer.ToString());
				System.Console.Write(fBuffer.ToString());
			}
		}

	}
}
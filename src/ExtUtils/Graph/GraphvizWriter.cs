using System.IO;
using System.Text;

namespace ExtUtils.Graph
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
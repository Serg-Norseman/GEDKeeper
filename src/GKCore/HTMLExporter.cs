using System;
using System.IO;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKCore
{
	public abstract class HTMLExporter : Exporter
	{
		public HTMLExporter(TGenEngine aEngine, string aPath) : base(aEngine, aPath)
		{
		}

		protected void WriteHTMLHeader(StreamWriter aStream, string aTitle)
		{
			aStream.WriteLine("<html>");
			aStream.WriteLine("<head>");
			aStream.WriteLine("<meta HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html; charset=windows-1251\">");
			aStream.WriteLine("<link rel=\"stylesheet\" href=\"style.css\" type=\"text/css\"/>");
			aStream.WriteLine("<title>" + aTitle + "</title>");
			aStream.WriteLine("</head>");
			aStream.WriteLine("<body>");
		}

		protected void WriteHTMLFooter(StreamWriter aStream)
		{
			aStream.WriteLine("</body>");
			aStream.WriteLine("</html>");
		}
	}
}

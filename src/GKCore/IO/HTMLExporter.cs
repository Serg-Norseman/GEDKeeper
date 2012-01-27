using System;
using System.IO;

/// <summary>
/// Localization: unknown
/// </summary>

namespace GKCore.IO
{
	public abstract class HTMLExporter : Exporter
	{
		protected void WriteHeader(StreamWriter aStream, string aTitle)
		{
			aStream.WriteLine("<html>");
			aStream.WriteLine("<head>");
			aStream.WriteLine("<meta HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html; charset=windows-1251\">");
			aStream.WriteLine("<link rel=\"stylesheet\" href=\"style.css\" type=\"text/css\"/>");
			aStream.WriteLine("<title>" + aTitle + "</title>");
			aStream.WriteLine("</head>");
			aStream.WriteLine("<body>");
		}

		protected void WriteFooter(StreamWriter aStream)
		{
			aStream.WriteLine("</body>");
			aStream.WriteLine("</html>");
		}

		public HTMLExporter(TGenEngine aEngine, string aPath) : base(aEngine, aPath)
		{
		}
	}
}

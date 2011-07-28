using GKCore;
using GKSys;
using System;
using System.IO;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GKCore
{
	public abstract class THTMLExporter : TExporter
	{
		protected internal void WriteHeader(StreamWriter aStream, string aTitle)
		{
			aStream.WriteLine("<html>");
			aStream.WriteLine("<head>");
			aStream.WriteLine("<meta HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html; charset=windows-1251\">");
			aStream.WriteLine("<link rel=\"stylesheet\" href=\"style.css\" type=\"text/css\"/>");
			aStream.WriteLine("<title>" + aTitle + "</title>");
			aStream.WriteLine("</head>");
			aStream.WriteLine("<body>");
		}

		protected internal void WriteFooter(StreamWriter aStream)
		{
			aStream.WriteLine("</body>");
			aStream.WriteLine("</html>");
		}

		public THTMLExporter(TGenEngine aEngine, string aPath) : base(aEngine, aPath)
		{
		}
	}
}

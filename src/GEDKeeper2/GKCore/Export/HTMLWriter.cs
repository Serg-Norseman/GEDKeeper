using System;
using System.Drawing;
using System.IO;

namespace GKCore.Export
{
	/// <summary>
	/// Description of HTMLWriter.
	/// </summary>
	public class HTMLWriter : CustomWriter
	{
		private StreamWriter fStream;

		public HTMLWriter()
		{
		}

		public override void beginWrite()
		{
			this.fStream = new StreamWriter(new FileStream(this.fFileName, FileMode.Create));

			this.WriteHTMLHeader(this.fStream);
		}

		public override void endWrite()
		{
			this.WriteHTMLFooter(this.fStream);

			this.fStream.Flush();
			this.fStream.Close();
		}

		public override void setAlbumPage(bool value)
		{
			// dummy
		}

		private void WriteHTMLHeader(StreamWriter stream)
		{
			stream.WriteLine("<html>");
			stream.WriteLine("<head>");
			stream.WriteLine("<meta HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html; charset=windows-1251\">");
			stream.WriteLine("<link rel=\"stylesheet\" href=\"style.css\" type=\"text/css\"/>");
			stream.WriteLine("<title>" + this.fDocumentTitle + "</title>");
			stream.WriteLine("</head>");
			stream.WriteLine("<body>");
		}

		private void WriteHTMLFooter(StreamWriter stream)
		{
			stream.WriteLine("</body>");
			stream.WriteLine("</html>");
		}

		public override void addParagraph(string text, object font, TextAlignment alignment)
		{
			
		}

		public override void addParagraph(string text, object font)
		{
		}

		public override void addParagraphAnchor(string text, object font, string anchor)
		{
			
		}

		public override void addParagraphLink(string text, object font, string link, object linkFont)
		{
			
		}

		public override object createFont(string name, int size, bool bold, bool underline, Color color)
		{
			return null;
		}

		public override void beginList()
		{
			
		}

		public override void addListItem(string text, object font)
		{
			
		}

		public override void addListItemLink(string text, object font, string link, object linkFont)
		{
			
		}

		public override void endList()
		{
			
		}

		public override void beginParagraph(TextAlignment alignment)
		{
			
		}

		public override void addParagraphChunk(string text, object font)
		{
			
		}

		public override void addParagraphChunkAnchor(string text, object font, string anchor)
		{
			
		}

		public override void addParagraphChunkLink(string text, object font, string link, object linkFont, bool sup)
		{
			
		}

		public override void endParagraph()
		{
			
		}
	}
}

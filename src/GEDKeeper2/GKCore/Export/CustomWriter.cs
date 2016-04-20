using System;
using System.Drawing;
using GKCommon;

namespace GKCore.Export
{
	/// <summary>
	/// Description of CustomWriter.
	/// </summary>
	public abstract class CustomWriter : BaseObject
	{
		public enum TextAlignment { taLeft, taCenter, taRight, taJustify };

		protected string fDocumentTitle;
		protected string fFileName;

		public CustomWriter()
		{
		}

		public void setDocumentTitle(string title)
		{
			this.fDocumentTitle = title;
		}

		public void setFileName(string fileName)
		{
			this.fFileName = fileName;
		}

		public abstract void setAlbumPage(bool value);

		public abstract void beginWrite();
		public abstract void endWrite();

		public abstract void addParagraph(string text, object font);
		public abstract void addParagraph(string text, object font, TextAlignment alignment);
		public abstract void addParagraphAnchor(string text, object font, string anchor);
		public abstract void addParagraphLink(string text, object font, string link, object linkFont);

		public abstract object createFont(string name, int size, bool bold, bool underline, Color color);

		public abstract void beginList();
		public abstract void addListItem(string text, object font);
		public abstract void addListItemLink(string text, object font, string link, object linkFont);
		public abstract void endList();

		public abstract void beginParagraph(TextAlignment alignment);
		public abstract void addParagraphChunk(string text, object font);
		public abstract void addParagraphChunkAnchor(string text, object font, string anchor);
		public abstract void addParagraphChunkLink(string text, object font, string link, object linkFont, bool sup);
		public abstract void endParagraph();
	}
}

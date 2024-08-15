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

using BSLib;
using GKCore.Charts;
using GKCore.Design.Graphics;

namespace GKCore.Export
{
    public enum TextAlignment
    {
        taLeft,
        taCenter,
        taRight,
        taJustify
    }

    /// <summary>
    /// 
    /// </summary>
    public abstract class CustomWriter : BaseObject
    {
        protected bool fAlbumPage;
        protected string fDocumentTitle;
        protected string fFileName;
        protected ExtMargins fMargins;

        protected CustomWriter() : this(false)
        {
        }

        protected CustomWriter(bool albumPage)
        {
            fAlbumPage = albumPage;
            fMargins = new ExtMargins(20);
        }

        public abstract bool SupportedText();

        public abstract bool SupportedTables();

        public virtual ChartRenderer GetPageRenderer()
        {
            return null;
        }

        public virtual ExtRectF GetPageSize()
        {
            return ExtRectF.CreateEmpty();
        }

        public virtual void SetAlbumPage(bool value)
        {
            fAlbumPage = value;
        }

        public void SetDocumentTitle(string title)
        {
            fDocumentTitle = title;
        }

        public void SetFileName(string fileName)
        {
            fFileName = fileName;
        }

        public abstract void BeginWrite();
        public abstract void EndWrite();
        public abstract void EnablePageNumbers();

        public void AddParagraph(string text, IFont font, TextAlignment alignment = TextAlignment.taLeft)
        {
            BeginParagraph(alignment, 0, 0, 0);
            AddParagraphChunk(text, font);
            EndParagraph();
        }

        public void AddParagraphAnchor(string text, IFont font, string anchor)
        {
            BeginParagraph(TextAlignment.taLeft, 0, 0, 0);
            AddParagraphChunkAnchor(text, font, anchor);
            EndParagraph();
        }

        public void AddParagraphLink(string text, IFont font, string link)
        {
            BeginParagraph(TextAlignment.taLeft, 0, 0, 0);
            AddParagraphChunkLink(text, font, link);
            EndParagraph();
        }

        public abstract IFont CreateFont(string name, float size, bool bold, bool underline, IColor color);
        public abstract void NewPage();
        public abstract void NewLine(float spacingBefore = 0.0f, float spacingAfter = 0.0f);

        public abstract void BeginMulticolumns(int columnCount, float columnSpacing);
        public abstract void EndMulticolumns();

        public abstract void BeginList();
        public abstract void EndList();
        public abstract void BeginListItem();
        public abstract void EndListItem();

        public virtual void AddListItem(string text, IFont font)
        {
            BeginListItem();
            AddParagraphChunk(text, font);
            EndListItem();
        }

        public virtual void AddListItemLink(string text, IFont font, string link, IFont linkFont)
        {
            BeginListItem();
            AddParagraphChunk(text, font);
            AddParagraphChunkLink(link, linkFont, link);
            EndListItem();
        }

        public abstract void BeginParagraph(TextAlignment alignment,
                                            float spacingBefore, float spacingAfter,
                                            float indent = 0.0f, bool keepTogether = false);
        public abstract void AddParagraphChunk(string text, IFont font);
        public abstract void AddParagraphChunkAnchor(string text, IFont font, string anchor);
        public abstract void AddParagraphChunkLink(string text, IFont font, string link, bool sup = false);
        public abstract void EndParagraph();

        public abstract void AddImage(IImage image, TextAlignment alignment);

        public abstract void BeginTable(int columnsCount, int rowsCount);
        public abstract void EndTable();

        public abstract void BeginTableRow(bool header = false);
        public abstract void EndTableRow();

        public abstract void AddTableCell(string content, IFont font = null, TextAlignment alignment = TextAlignment.taLeft);
    }
}

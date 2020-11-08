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

using BSLib;
using BSLib.Design.Graphics;
using GKCore.Charts;

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

        public abstract void AddParagraph(string text, IFont font);
        public abstract void AddParagraph(string text, IFont font, TextAlignment alignment);
        public abstract void AddParagraphAnchor(string text, IFont font, string anchor);
        public abstract void AddParagraphLink(string text, IFont font, string link);
        public abstract void AddParagraphLink(string text, IFont font, string link, IFont linkFont);

        public abstract IFont CreateFont(string name, float size, bool bold, bool underline, IColor color);
        public abstract void NewPage();
        public abstract void NewLine(float spacingBefore = 0.0f, float spacingAfter = 0.0f);

        public abstract void BeginMulticolumns(int columnCount, float columnSpacing);
        public abstract void EndMulticolumns();

        public abstract void BeginList();
        public abstract void AddListItem(string text, IFont font);
        public abstract void AddListItemLink(string text, IFont font, string link, IFont linkFont);
        public abstract void EndList();

        public abstract void BeginParagraph(TextAlignment alignment,
                                            float spacingBefore, float spacingAfter,
                                            float indent = 0.0f, bool keepTogether = false);
        public abstract void AddParagraphChunk(string text, IFont font);
        public abstract void AddParagraphChunkAnchor(string text, IFont font, string anchor);
        public abstract void AddParagraphChunkLink(string text, IFont font, string link, bool sup = false);
        public abstract void EndParagraph();

        public abstract void AddNote(string text, IFont font);

        public abstract void AddImage(IImage image);
    }
}

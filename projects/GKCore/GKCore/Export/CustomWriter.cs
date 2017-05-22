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

using GKCommon;
using GKCore.Interfaces;

namespace GKCore.Export
{
    /// <summary>
    /// 
    /// </summary>
    public abstract class CustomWriter : BaseObject
    {
        public enum TextAlignment { taLeft, taCenter, taRight, taJustify }

        protected bool fAlbumPage;
        protected string fDocumentTitle;
        protected string fFileName;
        protected Margins fMargins;

        protected CustomWriter()
        {
            fAlbumPage = false;
            fMargins = new Margins(20);
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

        public abstract void beginWrite();
        public abstract void endWrite();

        public abstract void addParagraph(string text, IFont font);
        public abstract void addParagraph(string text, IFont font, TextAlignment alignment);
        public abstract void addParagraphAnchor(string text, IFont font, string anchor);
        public abstract void addParagraphLink(string text, IFont font, string link, IFont linkFont);

        public abstract IFont CreateFont(string name, float size, bool bold, bool underline, IColor color);

        public abstract void beginList();
        public abstract void addListItem(string text, IFont font);
        public abstract void addListItemLink(string text, IFont font, string link, IFont linkFont);
        public abstract void endList();

        public abstract void beginParagraph(TextAlignment alignment, float spacingBefore, float spacingAfter);
        public abstract void addParagraphChunk(string text, IFont font);
        public abstract void addParagraphChunkAnchor(string text, IFont font, string anchor);
        public abstract void addParagraphChunkLink(string text, IFont font, string link, IFont linkFont, bool sup);
        public abstract void endParagraph();

        public abstract void addNote(string text, IFont font);
    }
}

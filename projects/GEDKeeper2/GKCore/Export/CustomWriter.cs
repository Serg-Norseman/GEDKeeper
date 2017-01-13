/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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

using System.Drawing;
using System.Windows.Forms;
using GKCommon;

namespace GKCore.Export
{
    /// <summary>
    /// 
    /// </summary>
    public abstract class CustomWriter : BaseObject
    {
        public enum TextAlignment { taLeft, taCenter, taRight, taJustify };

        protected bool fAlbumPage;
        protected string fDocumentTitle;
        protected string fFileName;
        protected Padding fMargins;

        protected CustomWriter()
        {
            fAlbumPage = false;
            fMargins = new Padding(20);
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

        public abstract void addParagraph(string text, object font);
        public abstract void addParagraph(string text, object font, TextAlignment alignment);
        public abstract void addParagraphAnchor(string text, object font, string anchor);
        public abstract void addParagraphLink(string text, object font, string link, object linkFont);

        public abstract object CreateFont(string name, float size, bool bold, bool underline, Color color);

        public abstract void beginList();
        public abstract void addListItem(string text, object font);
        public abstract void addListItemLink(string text, object font, string link, object linkFont);
        public abstract void endList();

        public abstract void beginParagraph(TextAlignment alignment, float spacingBefore, float spacingAfter);
        public abstract void addParagraphChunk(string text, object font);
        public abstract void addParagraphChunkAnchor(string text, object font, string anchor);
        public abstract void addParagraphChunkLink(string text, object font, string link, object linkFont, bool sup);
        public abstract void endParagraph();
    }
}

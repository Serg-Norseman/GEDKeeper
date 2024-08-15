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

using GKCore.Design.Graphics;

namespace GKCore.Export
{
    /// <summary>
    /// 
    /// </summary>
    public abstract class TableWriter : CustomWriter
    {
        protected TableWriter()
        {
        }

        public override bool SupportedText()
        {
            return false;
        }

        public override bool SupportedTables()
        {
            return true;
        }

        public override void EnablePageNumbers()
        {
        }

        public override void NewPage()
        {
        }

        public override void NewLine(float spacingBefore = 0.0f, float spacingAfter = 0.0f)
        {
        }

        public override IFont CreateFont(string name, float size, bool bold, bool underline, IColor color)
        {
            return null;
        }

        public override void BeginMulticolumns(int columnCount, float columnSpacing)
        {
        }

        public override void EndMulticolumns()
        {
        }

        public override void BeginList()
        {
        }

        public override void EndList()
        {
        }

        public override void BeginListItem()
        {
        }

        public override void EndListItem()
        {
        }

        public override void AddListItem(string text, IFont font)
        {
        }

        public override void AddListItemLink(string text, IFont font, string link, IFont linkFont)
        {
        }

        public override void BeginParagraph(TextAlignment alignment,
                                            float spacingBefore, float spacingAfter,
                                            float indent = 0.0f, bool keepTogether = false)
        {
        }

        public override void EndParagraph()
        {
        }

        public override void AddParagraphChunk(string text, IFont font)
        {
        }

        public override void AddParagraphChunkAnchor(string text, IFont font, string anchor)
        {
        }

        public override void AddParagraphChunkLink(string text, IFont font, string link, bool sup = false)
        {
        }

        public override void AddImage(IImage image, TextAlignment alignment)
        {
        }

        public override void EndTable()
        {
        }

        public override void BeginTableRow(bool header = false)
        {
        }

        public override void EndTableRow()
        {
        }
    }
}

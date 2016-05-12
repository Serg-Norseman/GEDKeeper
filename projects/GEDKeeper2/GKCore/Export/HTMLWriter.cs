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

using System;
using System.Collections.Generic;
using System.Drawing;
using System.IO;
using System.Text;

namespace GKCore.Export
{
    /// <summary>
    /// Description of HTMLWriter.
    /// </summary>
    public class HTMLWriter : CustomWriter
    {
        private StreamWriter fStream;
        private readonly Dictionary<string, string> fStyles;

        public HTMLWriter()
        {
            this.fStyles = new Dictionary<string, string>();
        }

        public override void beginWrite()
        {
            this.fStream = new StreamWriter(new FileStream(this.fFileName, FileMode.Create), Encoding.UTF8);

            this.fStream.WriteLine("<html>");
            this.fStream.WriteLine("<head>");
            this.fStream.WriteLine("<meta HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html; charset=utf-8\">");
            this.fStream.WriteLine("<link rel=\"stylesheet\" href=\"style.css\" type=\"text/css\"/>");
            this.fStream.WriteLine("<title>" + this.fDocumentTitle + "</title>");

            this.fStream.WriteLine("<style type=\"text/css\">");
            foreach (KeyValuePair<string, string> entry in this.fStyles)
            {
                this.fStream.WriteLine("." + entry.Key + " { " + entry.Value + " }");
            }
            this.fStream.WriteLine("</style>");

            this.fStream.WriteLine("</head>");
            this.fStream.WriteLine("<body>");
        }

        public override void endWrite()
        {
            this.fStream.WriteLine("</body>");
            this.fStream.WriteLine("</html>");

            this.fStream.Flush();
            this.fStream.Close();
        }

        public override void setAlbumPage(bool value)
        {
        }

        public override void addParagraph(string text, object font, TextAlignment alignment)
        {
            fStream.WriteLine("<p class=\""+font+"\">"+text+"</p>");
        }

        public override void addParagraph(string text, object font)
        {
            fStream.WriteLine("<p class=\""+font+"\">"+text+"</p>");
        }

        public override void addParagraphAnchor(string text, object font, string anchor)
        {
            fStream.WriteLine("<p class=\""+font+"\"><a name=\""+anchor+"\">"+text+"</a></p>");
        }

        public override void addParagraphLink(string text, object font, string link, object linkFont)
        {
            fStream.WriteLine("<p class=\""+font+"\"><a href=\"#"+link+"\">"+text+"</a></p>");
        }

        public override object createFont(string name, float size, bool bold, bool underline, Color color)
        {
            string style;
            
            style = "font-family: " + name;
            style += "; font-size: " + size + "pt";
            style += "; color: " + color.Name;
            if (bold) style += "; font-weight: bold";
            if (underline) style += "; text-decoration: underline";
            
            int index = this.fStyles.Count;
            string key = "style_" + index;
            this.fStyles.Add(key, style);

            return key;
        }

        public override void beginList()
        {
            fStream.WriteLine("<ul>");
        }

        public override void endList()
        {
            fStream.WriteLine("</ul>");
        }

        public override void addListItem(string text, object font)
        {
            fStream.WriteLine("<li class=\""+font+"\">"+text+"</li>");
        }

        public override void addListItemLink(string text, object font, string link, object linkFont)
        {
            fStream.WriteLine("<li class=\""+font+"\"><a href=\"#"+link+"\">"+text+"</a></li>");
        }

        public override void beginParagraph(TextAlignment alignment, float spacingBefore, float spacingAfter)
        {
            fStream.WriteLine("<p>");
        }

        public override void endParagraph()
        {
            fStream.WriteLine("</p>");
        }

        public override void addParagraphChunk(string text, object font)
        {
            fStream.WriteLine(text);
        }

        public override void addParagraphChunkAnchor(string text, object font, string anchor)
        {
            fStream.WriteLine("<a name=\""+anchor+"\">"+text+"</a>");
        }

        public override void addParagraphChunkLink(string text, object font, string link, object linkFont, bool sup)
        {
            if (sup) fStream.WriteLine("<sup>");
            fStream.WriteLine("<a href=\"#"+link+"\">"+text+"</a>");
            if (sup) fStream.WriteLine("</sup>");
        }
    }
}

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

using System.Collections.Generic;
using System.IO;
using System.Text;

using BSLib;
using GKCore.Interfaces;

namespace GKCore.Export
{
    /// <summary>
    /// 
    /// </summary>
    public class HTMLWriter : CustomWriter
    {
        private sealed class FontHandler: TypeHandler<string>, IFont
        {
            public string FontFamilyName
            {
                get { return string.Empty; } // dummy
            }

            public string Name
            {
                get { return string.Empty; } // dummy
            }

            public float Size
            {
                get { return 0; } // dummy
            }

            public FontHandler(string handle) : base(handle)
            {
            }
        }

        private StreamWriter fStream;
        private readonly Dictionary<string, string> fStyles;

        public HTMLWriter()
        {
            fStyles = new Dictionary<string, string>();
        }

        public override void BeginWrite()
        {
            fStream = new StreamWriter(new FileStream(fFileName, FileMode.Create), Encoding.UTF8);

            fStream.WriteLine("<html>");
            fStream.WriteLine("<head>");
            fStream.WriteLine("<meta HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html; charset=utf-8\">");
            fStream.WriteLine("<link rel=\"stylesheet\" href=\"style.css\" type=\"text/css\"/>");
            fStream.WriteLine("<title>" + fDocumentTitle + "</title>");

            fStream.WriteLine("<style type=\"text/css\">");
            foreach (KeyValuePair<string, string> entry in fStyles)
            {
                fStream.WriteLine("." + entry.Key + " { " + entry.Value + " }");
            }
            fStream.WriteLine("</style>");

            fStream.WriteLine("</head>");
            fStream.WriteLine("<body>");
        }

        public override void EndWrite()
        {
            fStream.WriteLine("</body>");
            fStream.WriteLine("</html>");

            fStream.Flush();
            fStream.Close();
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

        public override void AddParagraph(string text, IFont font, TextAlignment alignment)
        {
            fStream.WriteLine("<p class=\""+((FontHandler)font).Handle+"\">"+text+"</p>");
        }

        public override void AddParagraph(string text, IFont font)
        {
            fStream.WriteLine("<p class=\""+((FontHandler)font).Handle+"\">"+text+"</p>");
        }

        public override void AddParagraphAnchor(string text, IFont font, string anchor)
        {
            fStream.WriteLine("<p class=\""+((FontHandler)font).Handle+"\"><a name=\""+anchor+"\">"+text+"</a></p>");
        }

        public override void AddParagraphLink(string text, IFont font, string link)
        {
            fStream.WriteLine("<p class=\""+((FontHandler)font).Handle+"\"><a href=\"#"+link+"\">"+text+"</a></p>");
        }

        public override void AddParagraphLink(string text, IFont font, string link, IFont linkFont)
        {
            fStream.WriteLine("<p class=\""+((FontHandler)font).Handle+"\"><a href=\"#"+link+"\">"+text+"</a></p>");
        }

        public override IFont CreateFont(string name, float size, bool bold, bool underline, IColor color)
        {
            string style;

            style = "font-family: " + name;
            style += "; font-size: " + size + "pt";
            style += "; color: \"#" + color.GetCode() + "\"";
            if (bold) style += "; font-weight: bold";
            if (underline) style += "; text-decoration: underline";

            int index = fStyles.Count;
            string key = "style_" + index;
            fStyles.Add(key, style);

            return new FontHandler(key);
        }

        public override void BeginMulticolumns(int columnCount, float columnSpacing)
        {
        }

        public override void EndMulticolumns()
        {
        }

        public override void BeginList()
        {
            fStream.WriteLine("<ul>");
        }

        public override void EndList()
        {
            fStream.WriteLine("</ul>");
        }

        public override void AddListItem(string text, IFont font)
        {
            fStream.WriteLine("<li class=\""+((FontHandler)font).Handle+"\">"+text+"</li>");
        }

        public override void AddListItemLink(string text, IFont font, string link, IFont linkFont)
        {
            string alink = "";
            if (!string.IsNullOrEmpty(link)) {
                alink = "<a href=\"#" + link + "\">" + link + "</a>";
            }

            fStream.WriteLine("<li class=\""+((FontHandler)font).Handle+"\">" + text + alink + "</li>");
        }

        public override void BeginParagraph(TextAlignment alignment,
                                            float spacingBefore, float spacingAfter,
                                            float indent = 0.0f, bool keepTogether = false)
        {
            fStream.WriteLine("<p>");
        }

        public override void EndParagraph()
        {
            fStream.WriteLine("</p>");
        }

        public override void AddParagraphChunk(string text, IFont font)
        {
            fStream.WriteLine(text);
        }

        public override void AddParagraphChunkAnchor(string text, IFont font, string anchor)
        {
            fStream.WriteLine("<a name=\""+anchor+"\">"+text+"</a>");
        }

        public override void AddParagraphChunkLink(string text, IFont font, string link, bool sup)
        {
            if (sup) fStream.WriteLine("<sup>");
            fStream.WriteLine("<a href=\"#"+link+"\">"+text+"</a>");
            if (sup) fStream.WriteLine("</sup>");
        }

        public override void AddNote(string text, IFont font)
        {
            
        }

        public override void AddImage(IImage image)
        {
        }
    }
}

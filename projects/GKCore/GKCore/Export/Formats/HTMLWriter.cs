/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using BSLib;
using GKCore.Design.Graphics;

namespace GKCore.Export.Formats
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

            public float Height
            {
                get { return 0; } // dummy
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

        private string fExternalStyles;
        private StreamWriter fStream;
        private readonly Dictionary<string, string> fStyles;
        private int fTableCol, fTableColsCount;
        private int fTableRow, fTableRowsCount;

        public HTMLWriter()
        {
            fStyles = new Dictionary<string, string>();
        }

        public override void SetExternalStyles(string fileName)
        {
            fExternalStyles = fileName;
        }

        public override bool SupportedText()
        {
            return true;
        }

        public override bool SupportedTables()
        {
            return true;
        }

        public override void BeginWrite()
        {
            fStream = new StreamWriter(new FileStream(fFileName, FileMode.Create, FileAccess.Write), Encoding.UTF8);

            fStream.WriteLine("<html>");
            fStream.WriteLine("<head>");
            fStream.WriteLine("<meta HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html; charset=utf-8\">");
            fStream.WriteLine("<link rel=\"stylesheet\" href=\"style.css\" type=\"text/css\"/>");
            fStream.WriteLine("<title>" + fDocumentTitle + "</title>");
            fStream.WriteLine("</head>");
            fStream.WriteLine("<body>");
        }

        public override void EndWrite()
        {
            fStream.WriteLine("</body>");
            fStream.WriteLine("</html>");

            fStream.Flush();
            fStream.Close();

            WriteStyles();
        }

        private void WriteStyles()
        {
            string targetStylesFile = Path.GetDirectoryName(fFileName) + Path.DirectorySeparatorChar + "style.css";

            if (!string.IsNullOrEmpty(fExternalStyles)) {
                string sourceStylesFile = Path.Combine(GKUtils.GetExternalsPath(), fExternalStyles);

                if (File.Exists(sourceStylesFile)) {
                    File.Copy(sourceStylesFile, targetStylesFile, true);
                    return;
                }
            }

            // default styles
            using (var stream = new StreamWriter(new FileStream(targetStylesFile, FileMode.Create, FileAccess.Write), Encoding.UTF8)) {
                foreach (KeyValuePair<string, string> entry in fStyles) {
                    stream.WriteLine("." + entry.Key + " { " + entry.Value + " }");
                }
                stream.Flush();
            }
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

        public override void BeginListItem()
        {
            fStream.WriteLine("<li>");
        }

        public override void EndListItem()
        {
            fStream.WriteLine("</li>");
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
            fStream.WriteLine("<span class=\"" + ((FontHandler)font).Handle + "\">" + text + "</span>");
        }

        public override void AddParagraphChunkAnchor(string text, IFont font, string anchor)
        {
            fStream.WriteLine("<a name=\""+anchor+"\">");
            AddParagraphChunk(text, font);
            fStream.WriteLine("</a>");
        }

        public override void AddParagraphChunkLink(string text, IFont font, string link, bool sup = false)
        {
            fStream.WriteLine("<a href=\"#" + link + "\">");
            if (sup) fStream.WriteLine("<sup>");
            AddParagraphChunk(text, font);
            if (sup) fStream.WriteLine("</sup>");
            fStream.WriteLine("</a>");
        }

        private readonly string[] iAlignments = { "left", "center", "right", "justify" };

        private int fImagesIndex = 0;

        public override void AddImage(IImage image, TextAlignment alignment)
        {
            try {
                if (image == null) return;

                var bytes = image.GetBytes("png");

                var imName = Path.DirectorySeparatorChar + string.Format("{0}.png", ++fImagesIndex);

                string expFileName = Path.GetFileNameWithoutExtension(fFileName);
                string imagesDir = Path.GetDirectoryName(fFileName) + Path.DirectorySeparatorChar + expFileName;
                string imFilePath = imagesDir + imName;
                string imFileRef = "." + Path.DirectorySeparatorChar + expFileName + imName;

                if (!Directory.Exists(imagesDir)) {
                    Directory.CreateDirectory(imagesDir);
                }

                File.WriteAllBytes(imFilePath, bytes);

                fStream.WriteLine(string.Format("<img src=\"{0}\" align=\"{1}\" />", imFileRef, iAlignments[(int)alignment]));
            } catch (Exception ex) {
                Logger.WriteError("HTMLWriter.AddImage()", ex);
            }
        }

        public override void BeginTable(int columnsCount, int rowsCount)
        {
            fStream.WriteLine("<table>");
            fTableRowsCount = rowsCount;
            fTableRow = 0;
            fTableColsCount = columnsCount;
            fTableCol = 0;
        }

        public override void EndTable()
        {
            fStream.WriteLine("</table>");
        }

        public override void BeginTableRow(bool header = false)
        {
            fStream.WriteLine("<tr>");
        }

        public override void EndTableRow()
        {
            fStream.WriteLine("</tr>");
        }

        public override void AddTableCell(string content, IFont font = null, TextAlignment alignment = TextAlignment.taLeft)
        {
            string tdClass = (font == null) ? string.Empty : string.Format(" class=\"{0}\"", ((FontHandler)font).Handle);
            fStream.WriteLine("<td" + tdClass + ">" + content + "</td>");

            fTableCol += 1;
            if (fTableCol == fTableColsCount) {
                fTableRow += 1;
                fTableCol = 0;

                if (fTableRow < fTableRowsCount) {
                }
            }
        }
    }
}

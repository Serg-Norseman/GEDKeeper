/* 
 * Copyright 2009 Alexander Curtis <alex@logicmill.com>
 * This file is part of GEDmill - A family history website creator
 * 
 * GEDmill is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * GEDmill is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GEDmill.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using System.Globalization;
using System.IO;
using System.Text;
using GKCore.Logging;

namespace GEDmill.HTML
{
    /// <summary>
    /// Used to create an HTML file with a standard header and footer.
    /// </summary>
    public class HTMLFile
    {
        private static readonly ILogger fLogger = LogManager.GetLogger(CConfig.LOG_FILE, CConfig.LOG_LEVEL, typeof(HTMLFile).Name);

        private FileStream fStream;
        private StreamWriter fWriter;


        // Constructor. Creates a file with the given name and writes a standard HTML header.
        public HTMLFile(string filename, string title, string description, string keywords)
        {
            // This is for CJ who ended up with 17000 files plastered all over her desktop...
            if (GMHelper.IsDesktop(filename)) {
                throw new HTMLException("A problem occurred when creating an HTML file:\r\nGEDmill will not place files onto the Desktop.");
            }

            fLogger.WriteInfo("CHTMLFile : " + filename);

            if (File.Exists(filename)) {
                // Delete any current file
                File.SetAttributes(filename, FileAttributes.Normal);
                File.Delete(filename);
            }
            fStream = null;
            try {
                fStream = new FileStream(filename, FileMode.Create);
            } catch (NotSupportedException) {
                throw new HTMLException(string.Format("A problem occurred when creating an HTML file:\r\nThe path or filename {0} is not valid.", filename));
            }

            if (fStream != null) {
                var encoding = new UTF8EncodingWithoutPreamble();
                fWriter = new StreamWriter(fStream, encoding);

                DateTime dt = DateTime.Now;
                string date = dt.ToString("yyyy-MM-dd", DateTimeFormatInfo.InvariantInfo);

                fWriter.WriteLine("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">");
                fWriter.WriteLine("<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">");
                fWriter.WriteLine("  <head>");
                fWriter.WriteLine("    <link rel=\"stylesheet\" type=\"text/css\" href=\"" + CConfig.Instance.StylesheetFilename + ".css\" />");
                if (CConfig.Instance.AllowMultipleImages) // Multiple images feature is currently (10Dec08) the only thing that uses javascript
                {
                    fWriter.WriteLine("    <script type=\"text/javascript\" src=\"gedmill.js\"></script>");
                }
                fWriter.WriteLine("    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />");
                fWriter.WriteLine("    <meta http-equiv=\"imagetoolbar\" content=\"no\" />");
                fWriter.WriteLine(string.Concat("    <meta name=\"Title\" content=\"", title, "\" />"));
                fWriter.WriteLine(string.Concat("    <meta name=\"Description\" content=\"", description, "\" />"));
                fWriter.WriteLine(string.Concat("    <meta name=\"Keywords\" content=\"", keywords, "\" />"));
                fWriter.WriteLine("    <meta name=\"Version\" content=\"1.00\" />");
                fWriter.WriteLine(string.Concat("    <meta name=\"VersionDate\" content=\"", date, "\" />"));
                fWriter.WriteLine(string.Concat("    <title>", title, "</title>"));
                fWriter.WriteLine("  </head>");
                fWriter.WriteLine("  ");
                fWriter.WriteLine("  <body>");
            }
        }

        // Write HTML footer and close the file.
        public void Close()
        {
            fWriter.WriteLine("  </body>");
            fWriter.WriteLine("</html>");
            fWriter.Close();
        }

        public void WriteLine(string value)
        {
            fWriter.WriteLine(value);
        }

        public void WriteLine(string format, params object[] arg)
        {
            fWriter.WriteLine(string.Format(format, arg));
        }

        public void WriteH1(string text)
        {
            fWriter.WriteLine(string.Concat("<h1>", text, "</h1>"));
        }

        /// <summary>
        /// A class just like .Net's UTF8Encoding, except it doesn't write the Byte Order Mark (BOM).
        /// </summary>
        private class UTF8EncodingWithoutPreamble : UTF8Encoding
        {
            private static readonly byte[] Preamble = new byte[0];

            public override byte[] GetPreamble()
            {
                return Preamble;
            }
        }
    }
}

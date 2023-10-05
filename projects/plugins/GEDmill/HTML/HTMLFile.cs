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
using System.IO;
using System.Text;
using GKCore.Interfaces;
using GKCore.Logging;

namespace GEDmill.HTML
{
    /// <summary>
    /// An exception that can be thrown during HTML page creation.
    /// </summary>
    public class HTMLException : Exception
    {
        public HTMLException(string message) : base(message)
        {
        }
    }


    /// <summary>
    /// Used to create an HTML file with a standard header and footer.
    /// </summary>
    public sealed class HTMLFile
    {
        private static readonly ILogger fLogger = LogManager.GetLogger(GMConfig.LOG_FILE, GMConfig.LOG_LEVEL, typeof(HTMLFile).Name);

        private readonly ILangMan fLangMan;
        private FileStream fStream;
        private StreamWriter fWriter;


        /// <summary>
        /// Constructor. Creates a file with the given name and writes a standard HTML header.
        /// </summary>
        public HTMLFile(ILangMan langMan, string filename, string title, string description, string keywords)
        {
            fLangMan = langMan;

            // This is for CJ who ended up with 17000 files plastered all over her desktop...
            if (GMHelper.IsDesktop(filename)) {
                throw new HTMLException(fLangMan.LS(PLS.DesktopException));
            }

            fLogger.WriteInfo("HTMLFile : " + filename);

            if (File.Exists(filename)) {
                // Delete any current file
                File.SetAttributes(filename, FileAttributes.Normal);
                File.Delete(filename);
            }
            fStream = null;
            try {
                fStream = new FileStream(filename, FileMode.Create, FileAccess.Write);
            } catch (NotSupportedException) {
                throw new HTMLException(string.Format(fLangMan.LS(PLS.PathException), filename));
            }

            if (fStream != null) {
                var encoding = new UTF8EncodingWithoutPreamble();
                fWriter = new StreamWriter(fStream, encoding);

                fWriter.WriteLine("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">");
                fWriter.WriteLine("<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">");
                fWriter.WriteLine("  <head>");
                fWriter.WriteLine("    <link rel=\"stylesheet\" type=\"text/css\" href=\"" + GMConfig.StylesheetFilename + "\" />");
                if (GMConfig.Instance.AllowMultipleImages) // Multiple images feature is currently (10Dec08) the only thing that uses javascript
                {
                    fWriter.WriteLine("    <script type=\"text/javascript\" src=\"gedmill.js\"></script>");
                }
                fWriter.WriteLine("    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />");
                fWriter.WriteLine("    <meta http-equiv=\"imagetoolbar\" content=\"no\" />");
                fWriter.WriteLine(string.Concat("    <meta name=\"Title\" content=\"", title, "\" />"));
                fWriter.WriteLine(string.Concat("    <meta name=\"Description\" content=\"", description, "\" />"));
                fWriter.WriteLine(string.Concat("    <meta name=\"Keywords\" content=\"", keywords, "\" />"));
                fWriter.WriteLine("    <meta name=\"Version\" content=\"1.00\" />");
                fWriter.WriteLine(string.Concat("    <meta name=\"VersionDate\" content=\"", GMHelper.GetNowDateStr(), "\" />"));
                fWriter.WriteLine(string.Concat("    <title>", title, "</title>"));
                fWriter.WriteLine("  </head>");
                fWriter.WriteLine("  ");
                fWriter.WriteLine("  <body>");
            }
        }

        /// <summary>
        /// Write HTML footer and close the file.
        /// </summary>
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

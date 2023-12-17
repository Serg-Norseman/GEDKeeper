/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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

using System.IO;
using System.Text;
using GDModel.Providers.GEDCOM;
using GKCore;

namespace GDModel.Providers.GEDZIP
{
    /// <summary>
    /// Processing the GEDZIP format is one part of the Genealogical Data Model (GDM).
    /// </summary>
    public class GEDZIPProvider : FileProvider
    {
        static GEDZIPProvider()
        {
            // Static initialization of the GEDCOMProvider is needed, 
            // otherwise the standard tag identifiers are out of sync
            SysUtils.DoNotInline(GEDCOMProvider.GEDCOMFormats);
        }

        public GEDZIPProvider(GDMTree tree) : base(tree)
        {
        }

        public override string GetFilesFilter()
        {
            return "GEDZIP files (*.gdz,*.zip)|*.gdz,*.zip";
        }

        protected override Encoding GetDefaultEncoding()
        {
            return Encoding.UTF8;
        }

        protected override string DetectCharset(Stream inputStream, bool charsetDetection)
        {
            string streamCharset = null;
            return streamCharset;
        }

        public override void LoadFromStreamExt(Stream fileStream, Stream inputStream, bool charsetDetection = false)
        {
        }

        protected override void LoadFromReader(Stream fileStream, StreamReader reader, string streamCharset = null)
        {
        }
    }
}

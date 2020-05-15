/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2020 by Sergey V. Zhdanovskih.
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
using System.IO;
using System.Text;
using System.Xml;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore;

namespace GDModel.Providers.FamilyShow
{
#if !NETSTANDARD
    using System.IO.Packaging;
#endif

    /// <summary>
    /// Processing the FamilyX format is one part of the Genealogical Data Model (GDM).
    /// </summary>
    public class FamilyXProvider : FileProvider
    {
        private const string OPCContentFileName = "content.xml";


        public FamilyXProvider(GDMTree tree) : base(tree)
        {
        }

        public override string GetFilesFilter()
        {
            return "Family.Show files (*.familyx)|*.familyx";
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
#if !NETSTANDARD
            using (Package package = Package.Open(inputStream, FileMode.Open, FileAccess.Read)) {
                PackagePart documentPart = package.GetPart(new Uri("/" + OPCContentFileName, UriKind.Relative));
                using (MemoryStream memStream = new MemoryStream()) {
                    OPCUtility.CopyStream(documentPart.GetStream(), memStream);
                    memStream.Position = 0;

                    XmlReaderSettings settings = new XmlReaderSettings();
                    settings.DtdProcessing = DtdProcessing.Ignore;
                    using (XmlReader xr = XmlReader.Create(memStream, settings)) {
                    }
                }
            }
#endif
        }

        protected override void LoadFromReader(Stream fileStream, StreamReader reader, string streamCharset = null)
        {
            /*fTree.State = GDMTreeState.osLoading;
            try {
            } finally {
                fTree.State = GDMTreeState.osReady;
            }*/
        }
    }
}

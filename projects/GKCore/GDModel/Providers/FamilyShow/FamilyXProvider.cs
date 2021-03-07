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

                    LoadFromReader(memStream, null, null);
                }
            }
#endif
        }

        protected override void LoadFromReader(Stream fileStream, StreamReader reader, string streamCharset = null)
        {
            fTree.State = GDMTreeState.osLoading;
            try {
                ProgressEventHandler progressHandler = fTree.OnProgress;

                long fileSize = fileStream.Length;
                int progress = 0;

                GDMIndividualRecord lastIndividual = null;
                GDMTag lastTag = null;
                GEDCOMTagType lastTagType = GEDCOMTagType.Unknown;

                XmlReaderSettings settings = new XmlReaderSettings();
                settings.DtdProcessing = DtdProcessing.Ignore;
                using (XmlReader xr = XmlReader.Create(fileStream, settings)) {
                    while (xr.Read()) {
                        if (xr.NodeType == XmlNodeType.Element && !xr.IsEmptyElement) {
                            if (xr.Name == "Person") {
                                lastIndividual = fTree.CreateIndividual();
                                var persName = new GDMPersonalName(lastIndividual);
                                lastIndividual.AddPersonalName(persName);

                                lastIndividual.UID = xr.GetAttribute("Id");
                            } else if (xr.Name == "Gender") {
                                lastTagType = GEDCOMTagType.SEX;
                                lastTag = null;
                            } else if (xr.Name == "FirstName") {
                                lastTagType = GEDCOMTagType.GIVN;
                                lastTag = null;
                            } else if (xr.Name == "LastName") {
                                lastTagType = GEDCOMTagType.SURN;
                                lastTag = null;
                            }
                        } else if (xr.NodeType == XmlNodeType.Text) {
                            string nodeValue = xr.Value;

                            if (lastTag != null) {
                                lastTag = null;
                            } else {
                                switch (lastTagType) {
                                    case GEDCOMTagType.SEX:
                                        if (nodeValue == "Male") {
                                            lastIndividual.Sex = GDMSex.svMale;
                                        } else if (nodeValue == "Female") {
                                            lastIndividual.Sex = GDMSex.svFemale;
                                        }
                                        break;
                                    case GEDCOMTagType.GIVN:
                                        lastIndividual.PersonalNames[0].FirstPart = nodeValue;
                                        break;
                                    case GEDCOMTagType.SURN:
                                        lastIndividual.PersonalNames[0].Surname = nodeValue;
                                        break;
                                }
                                lastTagType = GEDCOMTagType.Unknown;
                            }
                        }

                        if (progressHandler != null) {
                            int newProgress = (int)Math.Min(100, (fileStream.Position * 100.0f) / fileSize);
                            if (progress != newProgress) {
                                progress = newProgress;
                                progressHandler(fTree, progress);
                            }
                        }
                    }
                }
            } finally {
                fTree.State = GDMTreeState.osReady;
            }
        }
    }
}

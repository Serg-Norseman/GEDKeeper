﻿/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2019 by Sergey V. Zhdanovskih.
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

namespace GDModel.Providers.GedML
{
    /// <summary>
    /// 
    /// </summary>
    public class GedMLProvider : FileProvider
    {
        public GedMLProvider(GDMTree tree) : base(tree)
        {
        }

        public override string GetFilesFilter()
        {
            return LangMan.LS(LSID.LSID_GedMLFilter);
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

        protected override void LoadFromReader(Stream fileStream, StreamReader reader, string streamCharset = null)
        {
            fTree.State = GDMTreeState.osLoading;
            try {
                ProgressEventHandler progressHandler = fTree.OnProgress;

                long fileSize = fileStream.Length;
                int progress = 0;
                var invariantText = GEDCOMUtils.InvariantTextInfo;

                var strTok = new GEDCOMParser(false);
                GDMTag curRecord = null;
                GDMTag curTag = null;
                var stack = new Stack<StackTuple>(9);

                XmlReaderSettings settings = new XmlReaderSettings();
                settings.DtdProcessing = DtdProcessing.Ignore;

                int tagLevel = -1;
                string xrefPtr, tagName = null, tagValue = null, xrefId;
                int tagId = 0; // Unknown
                bool tagOpened = false;

                using (XmlReader xr = XmlReader.Create(reader, settings)) {
                    while (xr.Read()) {
                        if (xr.NodeType == XmlNodeType.Element) {
                            if (tagOpened) {
                                curTag = GEDCOMProvider.ProcessTag(stack, tagLevel, tagId, tagValue);
                                tagOpened = false;
                            }

                            tagName = invariantText.ToUpper(xr.Name); // the name of the current element
                            tagId = GEDCOMTagsTable.Lookup(tagName);
                            tagLevel = xr.Depth - 1;
                            // GEDML only has 2 attributes - REF and ID.
                            xrefPtr = xr.GetAttribute("REF");
                            xrefId = xr.GetAttribute("ID");
                            tagValue = string.Empty;

                            if (tagLevel == 0) {
                                StackTuple stackTuple = GEDCOMProvider.AddTreeTag(fTree, tagLevel, tagId, string.Empty);
                                if (stackTuple != null) {
                                    stack.Clear();
                                    stack.Push(stackTuple);

                                    curRecord = stackTuple.Tag;
                                    if (!string.IsNullOrEmpty(xrefId)) {
                                        ((GDMRecord)curRecord).XRef = xrefId;
                                    }
                                }
                            } else if (tagLevel > 0) {
                                if (!string.IsNullOrEmpty(xrefPtr)) {
                                    // since the default method of the GEDCOM provider is used, 
                                    // a standard character `@` is expected
                                    curTag = GEDCOMProvider.ProcessTag(stack, tagLevel, tagId, "@" + xrefPtr + "@");
                                } else {
                                    tagOpened = true;
                                }
                            }
                        } else if (xr.NodeType == XmlNodeType.Text) {
                            tagValue = xr.Value;

                            if (tagLevel > 0 && curRecord != null) {
                                curTag = GEDCOMProvider.ProcessTag(stack, tagLevel, tagId, tagValue);
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

                stack.Clear();
            } finally {
                fTree.State = GDMTreeState.osReady;
            }
        }
    }
}

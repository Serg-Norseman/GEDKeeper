/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;
using System.IO;
using System.Xml;
using GDModel.Providers.GEDCOM;
using GKCore.Locales;

namespace GDModel.Providers.GedML
{
    /// <summary>
    /// Processing the GedML format is one part of the Genealogical Data Model (GDM).
    /// </summary>
    public class GedMLProvider : GEDCOMProvider
    {
        public GedMLProvider(GDMTree tree) : base(tree)
        {
        }

        public override string GetFilesFilter()
        {
            return LangMan.LS(LSID.GedMLFilter);
        }

        protected override void ReadStream(Stream inputStream, bool charsetDetection = false)
        {
            fTree.State = GDMTreeState.osLoading;
            try {
                var invariantText = GEDCOMUtils.InvariantTextInfo;
                GDMTag curRecord = null;
                GDMTag curTag = null;
                var stack = new Stack<StackTuple>(9);

                var settings = new XmlReaderSettings();
                settings.DtdProcessing = DtdProcessing.Ignore;

                int tagLevel = -1;
                string xrefPtr, tagName = null, xrefId;
                StringSpan tagValue = StringSpan.Empty;
                int tagId = 0; // Unknown
                bool tagOpened = false;

                using (XmlReader xr = XmlReader.Create(inputStream, settings)) {
                    while (xr.Read()) {
                        if (xr.NodeType == XmlNodeType.Element) {
                            if (tagOpened) {
                                curTag = GEDCOMProvider.ProcessTag(fTree, stack, tagLevel, tagId, tagValue);
                                tagOpened = false;
                            }

                            tagName = invariantText.ToUpper(xr.Name); // the name of the current element
                            tagId = GEDCOMTagsTable.Lookup(tagName);
                            tagLevel = xr.Depth - 1;
                            // GEDML only has 2 attributes - REF and ID.
                            xrefPtr = xr.GetAttribute("REF");
                            xrefId = xr.GetAttribute("ID");
                            tagValue = StringSpan.Empty;

                            if (tagLevel == 0) {
                                StackTuple stackTuple = AddTreeTag(fTree, tagLevel, tagId, StringSpan.Empty);
                                if (stackTuple.Level >= 0) {
                                    stack.Clear();
                                    stack.Push(stackTuple);

                                    curRecord = stackTuple.Tag;
                                    if (!string.IsNullOrEmpty(xrefId)) {
                                        ((GDMRecord)curRecord).SetXRef(fTree, xrefId, false);
                                    }
                                }
                            } else if (tagLevel > 0) {
                                if (!string.IsNullOrEmpty(xrefPtr)) {
                                    // since the default method of the GEDCOM provider is used, 
                                    // a standard character `@` is expected
                                    curTag = GEDCOMProvider.ProcessTag(fTree, stack, tagLevel, tagId, "@" + xrefPtr + "@");
                                } else {
                                    tagOpened = true;
                                }
                            }
                        } else if (xr.NodeType == XmlNodeType.Text) {
                            tagValue = xr.Value;

                            if (tagLevel > 0 && curRecord != null) {
                                curTag = GEDCOMProvider.ProcessTag(fTree, stack, tagLevel, tagId, tagValue);
                            }
                        }

                        // Only non-container files are loaded (FileStream -> xml).
                        NotifyProgress(inputStream.Position);
                    }
                }

                stack.Clear();
            } finally {
                fTree.State = GDMTreeState.osReady;
            }
        }
    }
}

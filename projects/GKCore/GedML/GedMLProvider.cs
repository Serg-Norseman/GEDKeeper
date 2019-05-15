/*
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
using BSLib;
using GKCommon.GEDCOM;

namespace GKCommon.GedML
{
    /// <summary>
    /// 
    /// </summary>
    public class GedMLProvider
    {
        private static readonly Encoding DEFAULT_ENCODING = Encoding.UTF8;

        private readonly GEDCOMTree fTree;

        public GedMLProvider(GEDCOMTree tree)
        {
            fTree = tree;
        }

        public void LoadFromFile(string fileName)
        {
            using (FileStream fileStream = new FileStream(fileName, FileMode.Open, FileAccess.Read)) {
                LoadFromStreamExt(fileStream, fileStream);
            }
        }

        public void LoadFromStreamExt(Stream fileStream, Stream inputStream)
        {
            using (StreamReader reader = FileHelper.OpenStreamReader(inputStream, DEFAULT_ENCODING)) {
                fTree.Clear();
                LoadFromReader(fileStream, reader);
                fTree.Header.CharacterSet = GEDCOMCharacterSet.csASCII;
            }
        }

        private void LoadFromReader(Stream fileStream, StreamReader reader)
        {
            fTree.State = GEDCOMState.osLoading;
            try {
                ProgressEventHandler progressHandler = fTree.OnProgress;

                long fileSize = fileStream.Length;
                int progress = 0;
                var invariantText = GEDCOMUtils.InvariantTextInfo;

                var strTok = new GEDCOMParser(false);
                GEDCOMCustomRecord curRecord = null;
                var stack = new Stack<StackTuple>(9);

                XmlReaderSettings settings = new XmlReaderSettings();
                settings.DtdProcessing = DtdProcessing.Ignore;

                int tagLevel = -1;
                string tagXRef, tagName = null, tagValue, tagId;

                using (XmlReader xr = XmlReader.Create(reader, settings)) {
                    while (xr.Read()) {
                        if (xr.NodeType == XmlNodeType.Element) {
                            tagName = invariantText.ToUpper(xr.Name); // the name of the current element
                            tagLevel = xr.Depth - 1;
                            // GEDML only has 2 attributes - REF and ID.
                            tagXRef = xr.GetAttribute("REF");
                            tagId = xr.GetAttribute("ID");

                            if (tagLevel == 0) {
                                StackTuple stackTuple = AddTreeTagHandler(fTree, tagLevel, tagName, string.Empty);
                                if (stackTuple != null) {
                                    stack.Clear();
                                    stack.Push(stackTuple);
                                    curRecord = (GEDCOMCustomRecord)stackTuple.Tag;
                                    if (!string.IsNullOrEmpty(tagId)) {
                                        curRecord.XRef = tagId;
                                    }
                                }
                            }
                        } else if (xr.NodeType == XmlNodeType.Text) {
                            tagValue = xr.Value;

                            if (tagLevel > 0) {
                                if (curRecord != null) {
                                    GEDCOMTag parentTag = null;
                                    AddTagHandler addTagHandler = null;
                                    while (stack.Count > 0) {
                                        var tuple = stack.Peek();
                                        if (tagLevel > tuple.Level) {
                                            parentTag = tuple.Tag;
                                            addTagHandler = tuple.AddHandler;
                                            break;
                                        }
                                        stack.Pop();
                                    }

                                    if (parentTag != null && addTagHandler != null) {
                                        StackTuple stackTuple = addTagHandler(parentTag, tagLevel, tagName, tagValue);
                                        if (stackTuple != null) {
                                            stack.Push(stackTuple);
                                        }
                                    }
                                }
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
                fTree.State = GEDCOMState.osReady;
            }
        }

        private static StackTuple AddTreeTagHandler(GEDCOMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GEDCOMTree tree = (GEDCOMTree)owner;
            GEDCOMCustomRecord curRecord = null;
            AddTagHandler addHandler = null;

            if (tagName == GEDCOMTagType.INDI) {
                curRecord = tree.AddRecord(new GEDCOMIndividualRecord(tree));
                addHandler = null;
            } else if (tagName == GEDCOMTagType.FAM) {
                curRecord = tree.AddRecord(new GEDCOMFamilyRecord(tree));
                addHandler = null;
            } else if (tagName == GEDCOMTagType.OBJE) {
                curRecord = tree.AddRecord(new GEDCOMMultimediaRecord(tree));
                addHandler = null;
            } else if (tagName == GEDCOMTagType.NOTE) {
                curRecord = tree.AddRecord(new GEDCOMNoteRecord(tree));
                curRecord.ParseString(tagValue);
                addHandler = null;
            } else if (tagName == GEDCOMTagType.REPO) {
                curRecord = tree.AddRecord(new GEDCOMRepositoryRecord(tree));
                addHandler = null;
            } else if (tagName == GEDCOMTagType.SOUR) {
                curRecord = tree.AddRecord(new GEDCOMSourceRecord(tree));
                addHandler = null;
            } else if (tagName == GEDCOMTagType.SUBN) {
                curRecord = tree.AddRecord(new GEDCOMSubmissionRecord(tree));
                addHandler = null;
            } else if (tagName == GEDCOMTagType.SUBM) {
                curRecord = tree.AddRecord(new GEDCOMSubmitterRecord(tree));
                addHandler = AddSubmitterTagHandler;
            } else if (tagName == GEDCOMTagType._GROUP) {
                curRecord = tree.AddRecord(new GEDCOMGroupRecord(tree));
                addHandler = null;
            } else if (tagName == GEDCOMTagType._RESEARCH) {
                curRecord = tree.AddRecord(new GEDCOMResearchRecord(tree));
                addHandler = null;
            } else if (tagName == GEDCOMTagType._TASK) {
                curRecord = tree.AddRecord(new GEDCOMTaskRecord(tree));
                addHandler = null;
            } else if (tagName == GEDCOMTagType._COMM) {
                curRecord = tree.AddRecord(new GEDCOMCommunicationRecord(tree));
                addHandler = null;
            } else if (tagName == GEDCOMTagType._LOC) {
                curRecord = tree.AddRecord(new GEDCOMLocationRecord(tree));
                addHandler = null;
            } else if (tagName == GEDCOMTagType.HEAD) {
                curRecord = tree.Header;
                addHandler = null;
            }

            if (curRecord != null) {
                return new StackTuple(tagLevel, curRecord, addHandler);
            } else {
                return null;
            }
        }

        private static StackTuple AddSubmitterTagHandler(GEDCOMObject owner, int tagLevel, string tagName, string tagValue)
        {
            GEDCOMSubmitterRecord submRec = (GEDCOMSubmitterRecord)owner;

            GEDCOMTag resultTag = null;

            if (tagName == GEDCOMTagType.NAME) {
                resultTag = submRec.AddTag(tagName, tagValue, GEDCOMPersonalName.Create);
            } else if (tagName == GEDCOMTagType.PHON || tagName == GEDCOMTagType.EMAIL || tagName == GEDCOMTagType.FAX || tagName == GEDCOMTagType.WWW) {
                resultTag = submRec.Address.AddTag(tagName, tagValue, null);
            } else if (tagName == GEDCOMTagType.LANG) {
                resultTag = submRec.AddLanguage(new GEDCOMLanguage(submRec, tagName, tagValue));
            } else {
                // 'ADDR' defines by default
                //result = AddRecordTagHandler(tagName, tagValue, tagConstructor);
            }

            if (resultTag != null) {
                return new StackTuple(tagLevel, resultTag, null);
            } else {
                return null;
            }
        }
    }
}

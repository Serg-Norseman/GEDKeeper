/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
using BSLib;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Calendar;
using GKCore.Design.Graphics;
using GKCore.Interfaces;
using GKCore.Options;
using GKCore.Types;

namespace GKCore.Export
{
    /// <summary>
    ///
    /// </summary>
    public sealed class PedigreeExporter : ReportExporter
    {
        private class PedigreePerson
        {
            public PedigreePerson Parent;
            public string Id;
            public GDMIndividualRecord IRec;
            public int Level;
            public readonly List<string> Sources;
            public int FamilyOrder;
            public int ChildIdx;

            public PedigreePerson()
            {
                Sources = new List<string>();
            }

            private string GetOrderStr()
            {
                string order = ConvertHelper.AdjustNumber(FamilyOrder, 2);
                string result = ((Parent == null) ? order : Parent.GetOrderStr() + order);
                return result;
            }

            public string GetInternalStr()
            {
                return ConvertHelper.AdjustNumber(Level, 2) + GetOrderStr();
            }
        }

        private class PedigreeEvent
        {
            public readonly GDMIndividualRecord IRec;
            public readonly GDMCustomEvent Event;
            public readonly UDN Date;
            public readonly List<string> Sources;

            public PedigreeEvent(GDMIndividualRecord iRec, GDMCustomEvent evt)
            {
                IRec = iRec;
                Event = evt;
                Date = (evt == null) ? UDN.Unknown : evt.Date.GetUDN();
                Sources = new List<string>();
            }
        }

        private PedigreeFormat fFormat;
        private PedigreeNumbering fNumbering;
        private PedigreeType fType;
        private List<PedigreePerson> fPersonList;
        private readonly GDMIndividualRecord fRoot;
        private readonly ShieldState fShieldState;
        private StringList fSourceList;

        private IFont fPersonFont;
        private IFont fLinkFont;
        private IFont fTextFont, fSupText;

        public PedigreeType Type
        {
            get { return fType; }
            set {
                fType = value;
                fNumbering = (fType == PedigreeType.Ascend) ? fOptions.PedigreeOptions.AscendNumbering : fOptions.PedigreeOptions.DescendNumbering;
            }
        }

        public GDMIndividualRecord Root
        {
            get { return fRoot; }
        }

        public ShieldState ShieldState
        {
            get { return fShieldState; }
        }

        public PedigreeExporter(IBaseWindow baseWin, GDMIndividualRecord root) : base(baseWin, false)
        {
            fRoot = root;
            fTitle = LangMan.LS(LSID.ExpPedigree) + ": " + GKUtils.GetNameString(fRoot, true, false);
            fShieldState = baseWin.Context.ShieldState;
        }

        private PedigreePerson FindPerson(GDMIndividualRecord iRec)
        {
            if (iRec == null) return null;

            PedigreePerson res = null;

            int num = fPersonList.Count;
            for (int i = 0; i < num; i++) {
                PedigreePerson item = fPersonList[i];

                if (item.IRec == iRec) {
                    res = item;
                    break;
                }
            }

            return res;
        }

        private void WriteSourceCitations(List<string> sources)
        {
            if (!fOptions.PedigreeOptions.IncludeSources || sources.Count <= 0)
                return;

            fWriter.AddParagraphChunk(" ", fTextFont);

            int num = sources.Count;
            for (int i = 0; i < num; i++) {
                string lnk = sources[i];

                if (i > 0) {
                    fWriter.AddParagraphChunkLink(", ", fTextFont, "", true);
                }

                fWriter.AddParagraphChunkLink(lnk, fSupText, "src_" + lnk, true);
            }
        }

        private void WritePerson(PedigreePerson person)
        {
            fWriter.BeginParagraph(TextAlignment.taJustify, 12f, 6f);

            fWriter.AddParagraphChunkAnchor(GetIdStr(person) + ". " + GKUtils.GetNameString(person.IRec, true, false), fPersonFont, person.Id);
            fWriter.AddParagraphChunk(GKUtils.GetPedigreeLifeStr(person.IRec, fOptions.PedigreeOptions.Format), fTextFont);
            WriteSourceCitations(person.Sources);

            fWriter.EndParagraph();

            switch (fFormat) {
                case PedigreeFormat.Excess:
                    WriteExcessFmt(person);
                    break;

                case PedigreeFormat.Compact:
                    WriteCompactFmt(person);
                    break;
            }
        }

        private string idLink(GDMIndividualRecord iRec)
        {
            PedigreePerson person = FindPerson(iRec);
            return (person == null) ? "" : person.Id;
        }

        private string GetIdStr(PedigreePerson person)
        {
            string result = person.Id;

            if (fNumbering == PedigreeNumbering.Kobrin_Konovalov_D && person.Parent != null) {
                GDMFamilyRecord family = fTree.GetPtrValue(person.IRec.ChildToFamilyLinks[0]);
                string spStr = "";
                int idx = person.Parent.IRec.IndexOfSpouse(family);
                if (person.Parent.IRec.SpouseToFamilyLinks.Count > 1) {
                    spStr = "/" + (idx + 1).ToString();
                }
                result += spStr;
            }
            return result;
        }

        private void WriteExcessFmt(PedigreePerson person)
        {
            if (fOptions.PedigreeOptions.IncludePortraits) {
                float factor = (fWriter is PDFWriter) ? 0.6f : 1.0f;
                IImage image = fBase.Context.GetPrimaryBitmap(person.IRec, (int)(fDefImageWidth * factor), (int)(fDefImageHeight * factor), false);
                fWriter.AddImage(image, TextAlignment.taRight);
            }

            fWriter.AddParagraph(LangMan.LS(LSID.Sex) + ": " + GKUtils.SexStr(person.IRec.Sex), fTextFont);

            string st = GKUtils.GetLifeExpectancyStr(person.IRec);
            if (st != "?" && st != "") {
                fWriter.AddParagraph(LangMan.LS(LSID.LifeExpectancy) + ": " + st, fTextFont);
            }

            GDMIndividualRecord father, mother;
            fTree.GetParents(person.IRec, out father, out mother);

            if (father != null) {
                fWriter.AddParagraphLink(LangMan.LS(LSID.Father) + ": " + GKUtils.GetNameString(father, true, false) + " ", fLinkFont, idLink(father));
            }

            if (mother != null) {
                fWriter.AddParagraphLink(LangMan.LS(LSID.Mother) + ": " + GKUtils.GetNameString(mother, true, false) + " ", fLinkFont, idLink(mother));
            }

            var evList = new List<PedigreeEvent>();
            int i;
            if (person.IRec.HasEvents) {
                fWriter.AddParagraph(LangMan.LS(LSID.Events) + ":", fTextFont);

                int num = person.IRec.Events.Count;
                for (i = 0; i < num; i++) {
                    GDMCustomEvent evt = person.IRec.Events[i];
                    if (!(evt is GDMIndividualAttribute) || fOptions.PedigreeOptions.IncludeAttributes) {
                        var pEvt = new PedigreeEvent(person.IRec, evt);
                        ProcessSourceCitations(evt, pEvt.Sources);
                        evList.Add(pEvt);
                    }
                }
                WriteEventList(person, evList);
            }

            int num2 = person.IRec.SpouseToFamilyLinks.Count;
            for (i = 0; i < num2; i++) {
                GDMFamilyRecord family = fTree.GetPtrValue(person.IRec.SpouseToFamilyLinks[i]);
                if (!fBase.Context.IsRecordAccess(family.Restriction)) continue;

                GDMIndividualRecord spRec;
                string unk;
                if (person.IRec.Sex == GDMSex.svMale) {
                    spRec = fTree.GetPtrValue(family.Wife);
                    st = LangMan.LS(LSID.Wife) + ": ";
                    unk = LangMan.LS(LSID.UnkFemale);
                } else {
                    spRec = fTree.GetPtrValue(family.Husband);
                    st = LangMan.LS(LSID.Husband) + ": ";
                    unk = LangMan.LS(LSID.UnkMale);
                }

                string sps;
                if (spRec != null) {
                    sps = st + GKUtils.GetNameString(spRec, true, false) + GKUtils.GetPedigreeLifeStr(spRec, fOptions.PedigreeOptions.Format)/* + this.idLink(this.FindPerson(irec))*/;
                } else {
                    sps = st + unk;
                }

                fWriter.AddParagraph(sps, fTextFont);

                evList.Clear();
                int childrenCount = family.Children.Count;
                for (int j = 0; j < childrenCount; j++) {
                    GDMIndividualRecord child = fTree.GetPtrValue(family.Children[j]);
                    var evt = child.FindEvent(GEDCOMTagType.BIRT);
                    var pEvt = new PedigreeEvent(child, evt);
                    ProcessSourceCitations(evt, pEvt.Sources);
                    evList.Add(pEvt);
                }
                WriteEventList(person, evList);
            }

            WriteNotes(person.IRec);
        }

        private void WriteNotes(IGDMStructWithNotes swn, string indent = "")
        {
            if (swn != null && fOptions.PedigreeOptions.IncludeNotes && swn.HasNotes) {
                fWriter.AddParagraph(indent + LangMan.LS(LSID.RPNotes) + ":", fTextFont);

                int notesCount = swn.Notes.Count;
                for (int j = 0; j < notesCount; j++) {
                    GDMLines noteLines = fTree.GetNoteLines(swn.Notes[j]);
                    fWriter.AddParagraph(indent + " — " + GKUtils.MergeStrings(noteLines), fTextFont);
                }
            }
        }

        private void WriteCompactFmt(PedigreePerson person)
        {
            if (fOptions.PedigreeOptions.IncludeNotes && person.IRec.HasNotes) {
                int num = person.IRec.Notes.Count;
                for (int i = 0; i < num; i++) {
                    GDMLines noteLines = fTree.GetNoteLines(person.IRec.Notes[i]);
                    fWriter.AddParagraph(GKUtils.MergeStrings(noteLines), fTextFont);
                }
            }

            bool spIndex = person.IRec.SpouseToFamilyLinks.Count > 1;

            int num2 = person.IRec.SpouseToFamilyLinks.Count;
            for (int i = 0; i < num2; i++) {
                GDMFamilyRecord family = fTree.GetPtrValue(person.IRec.SpouseToFamilyLinks[i]);
                if (fBase.Context.IsRecordAccess(family.Restriction)) {
                    GDMIndividualRecord spRec;
                    string st;
                    string unk;
                    if (person.IRec.Sex == GDMSex.svMale) {
                        spRec = fTree.GetPtrValue(family.Wife);
                        st = LangMan.LS(LSID.WifeSign);
                        unk = LangMan.LS(LSID.UnkFemale);
                    } else {
                        spRec = fTree.GetPtrValue(family.Husband);
                        st = LangMan.LS(LSID.HusbSign);
                        unk = LangMan.LS(LSID.UnkMale);
                    }

                    if (spIndex) {
                        st += (i + 1).ToString();
                    }
                    st += " - ";

                    if (spRec != null) {
                        st = st + GKUtils.GetNameString(spRec, true, false) + GKUtils.GetPedigreeLifeStr(spRec, fOptions.PedigreeOptions.Format)/* + this.idLink(this.FindPerson(irec))*/;
                    } else {
                        st += unk;
                    }

                    fWriter.AddParagraph(st, fTextFont);
                }
            }
        }

        private static int EventsCompare(PedigreeEvent item1, PedigreeEvent item2)
        {
            return item1.Date.CompareTo(item2.Date);
        }

        private void WriteEventList(PedigreePerson person, List<PedigreeEvent> evList)
        {
            SortHelper.QuickSort(evList, EventsCompare);

            int evtNum = evList.Count;
            for (int i = 0; i < evtNum; i++) {
                GDMCustomEvent evt = evList[i].Event;
                if (evt != null && Equals(evList[i].IRec, person.IRec)) {
                    var evtType = evt.GetTagType();

                    if (evtType == GEDCOMTagType.BIRT) {
                        Exchange(evList, i, 0);
                    } else if (evtType == GEDCOMTagType.DEAT) {
                        Exchange(evList, i, evtNum - 1);
                    }
                }
            }

            fWriter.BeginList();

            var dateFormat = GlobalOptions.Instance.DefDateFormat;
            for (int i = 0; i < evtNum; i++) {
                PedigreeEvent evObj = evList[i];
                GDMCustomEvent evt = evObj.Event;
                string li;

                fWriter.BeginListItem();

                if (evObj.IRec == person.IRec) {
                    li = GKUtils.GetEventStr(evt);

                    fWriter.AddParagraphChunk(" " + li, fTextFont);
                } else {
                    string dt = (evt == null) ? "?" : GKUtils.GEDCOMEventToDateStr(evt, dateFormat, false);

                    string st = (evObj.IRec.Sex == GDMSex.svMale) ? LangMan.LS(LSID.HeWasBorn) : LangMan.LS(LSID.SheWasBorn);

                    li = string.Format("{0}: {1} {2}", dt, st, GKUtils.GetNameString(evObj.IRec, true, false));
                    PedigreePerson prs = FindPerson(evObj.IRec);
                    string id = (prs != null) ? prs.Id : "";

                    fWriter.AddParagraphChunk(" " + li + " ", fTextFont);
                    fWriter.AddParagraphChunkLink(id, fLinkFont, id);
                }

                WriteSourceCitations(evObj.Sources);

                WriteNotes(evt, "\t");

                fWriter.EndListItem();
            }

            fWriter.EndList();
        }

        private static void Exchange<T>(List<T> list, int index1, int index2)
        {
            var f = list[index1];
            list[index1] = list[index2];
            list[index2] = f;
        }

        private void ProcessSourceCitations(IGDMStructWithSourceCitations swsc, List<string> sources)
        {
            if (fOptions.PedigreeOptions.IncludeSources && swsc != null && swsc.HasSourceCitations) {
                for (int i = 0, num = swsc.SourceCitations.Count; i < num; i++) {
                    var srcCit = swsc.SourceCitations[i];
                    var sourceRec = fTree.GetPtrValue<GDMSourceRecord>(srcCit);
                    if (sourceRec == null) continue;

                    string key = srcCit.XRef;
                    if (fOptions.PedigreeOptions.IncludeSourcePages) {
                        key += "@" + srcCit.Page;
                    }

                    int srcIndex = fSourceList.IndexOf(key);
                    if (srcIndex < 0) {
                        srcIndex = fSourceList.AddObject(key, srcCit);
                    }
                    sources.Add((srcIndex + 1).ToString());
                }
            }
        }

        private void GenStep(PedigreePerson parent, GDMIndividualRecord iRec, int level, int familyOrder)
        {
            if (iRec == null) return;

            PedigreePerson res = new PedigreePerson();
            res.Parent = parent;
            res.IRec = iRec;
            res.Level = level;
            res.ChildIdx = 0;
            res.FamilyOrder = familyOrder;
            fPersonList.Add(res);

            ProcessSourceCitations(iRec, res.Sources);

            if (fType == PedigreeType.Ascend) {
                if (iRec.ChildToFamilyLinks.Count > 0) {
                    GDMFamilyRecord family = fTree.GetPtrValue(iRec.ChildToFamilyLinks[0]);
                    if (fBase.Context.IsRecordAccess(family.Restriction)) {
                        GDMIndividualRecord father, mother;
                        fBase.Context.Tree.GetSpouses(family, out father, out mother);

                        GenStep(res, father, level + 1, 1);
                        GenStep(res, mother, level + 1, 1);
                    }
                }
            } else {
                int num2 = iRec.SpouseToFamilyLinks.Count;
                for (int j = 0; j < num2; j++) {
                    GDMFamilyRecord family = fTree.GetPtrValue(iRec.SpouseToFamilyLinks[j]);
                    if (!fBase.Context.IsRecordAccess(family.Restriction)) continue;

                    fBase.Context.ProcessFamily(family);

                    int num3 = family.Children.Count;
                    for (int i = 0; i < num3; i++) {
                        GDMIndividualRecord child = fTree.GetPtrValue(family.Children[i]);
                        GenStep(res, child, level + 1, i + 1);
                    }
                }
            }
        }

        private static int PersonsCompare(PedigreePerson item1, PedigreePerson item2)
        {
            return string.CompareOrdinal(item1.GetInternalStr(), item2.GetInternalStr());
        }

        private void ReIndex()
        {
            SortHelper.QuickSort(fPersonList, PersonsCompare);

            int num3 = fPersonList.Count;
            for (int i = 0; i < num3; i++) {
                PedigreePerson obj = fPersonList[i];

                switch (fNumbering) {
                    case PedigreeNumbering.Aboville:
                        if (obj.Parent == null) {
                            obj.Id = "1";
                        } else {
                            obj.Parent.ChildIdx++;
                            obj.Id = obj.Parent.Id + "." + obj.Parent.ChildIdx.ToString();
                        }
                        break;

                    case PedigreeNumbering.Kobrin_Konovalov_A:
                    case PedigreeNumbering.Kobrin_Konovalov_D:
                        obj.Id = (i + 1).ToString();
                        if (obj.Parent != null) {
                            string pid = obj.Parent.Id;

                            int p = pid.IndexOf("-");
                            if (p >= 0) pid = pid.Substring(0, p);

                            obj.Id = obj.Id + "-" + pid;
                        }
                        break;

                    case PedigreeNumbering.Sosa_Stradonitz:
                        obj.Id = (i + 1).ToString();
                        break;
                }
            }
        }

        protected override void InternalGenerate()
        {
            bool isRtf = (fWriter is RTFWriter);

            IColor clrBlack = AppHost.GfxProvider.CreateColor(0x000000);
            IColor clrBlue = AppHost.GfxProvider.CreateColor(0x0000FF);

            fPersonFont = fWriter.CreateFont("", 14f, true, false, clrBlack);
            fLinkFont = fWriter.CreateFont("", 10f, false, true, clrBlue);
            fTextFont = fWriter.CreateFont("", 10f, false, false, clrBlack);
            fSupText = fWriter.CreateFont("", (isRtf ? 12f : 5f), false, false, clrBlue);
            var chapFont = fWriter.CreateFont("", 16f, true, false, clrBlack);

            fFormat = fOptions.PedigreeOptions.Format;

            bool includeGens = fOptions.PedigreeOptions.IncludeGenerations;

            var titleFont = fWriter.CreateFont("", 16f, true, false, clrBlack);
            fWriter.AddParagraph(fTitle, titleFont, TextAlignment.taCenter);

            fPersonList = new List<PedigreePerson>();
            fSourceList = new StringList();
            try {
                GenStep(null, fRoot, 1, 1);
                ReIndex();

                int curLevel = 0;
                int num = fPersonList.Count;
                for (int i = 0; i < num; i++) {
                    PedigreePerson person = fPersonList[i];

                    if (includeGens && curLevel != person.Level) {
                        curLevel = person.Level;
                        string genTitle = LangMan.LS(LSID.Generation) + " " + ConvertHelper.GetRome(curLevel);

                        fWriter.BeginParagraph(TextAlignment.taLeft, 16f, 6f);
                        fWriter.AddParagraphChunk(genTitle, chapFont);
                        fWriter.EndParagraph();
                    }

                    WritePerson(person);
                }

                if (fSourceList.Count > 0) {
                    fWriter.BeginParagraph(TextAlignment.taCenter, 16f, 6f);
                    fWriter.AddParagraphChunk(LangMan.LS(LSID.RPSources), chapFont);
                    fWriter.EndParagraph();

                    int num2 = fSourceList.Count;
                    for (int j = 0; j < num2; j++) {
                        var srcCit = fSourceList.GetObject(j) as GDMSourceCitation;
                        var sourceRec = fTree.GetPtrValue<GDMSourceRecord>(srcCit);
                        if (sourceRec == null) continue;

                        string srcName = sourceRec.ShortTitle;
                        if (fOptions.PedigreeOptions.IncludeSourcePages) {
                            string srcPage = srcCit.Page;
                            if (!string.IsNullOrEmpty(srcName) && !string.IsNullOrEmpty(srcPage)) {
                                srcName += ", ";
                            }
                            srcName += srcPage;
                        }
                        string srcTitle = GKUtils.MergeStrings(sourceRec.Title.Lines);
                        if (!string.IsNullOrEmpty(srcName) && !string.IsNullOrEmpty(srcTitle)) {
                            srcName += "\n";
                        }
                        srcName += srcTitle;

                        string sn = (j + 1).ToString();
                        string sst = sn + ". " + srcName;
                        string sanc = "src_" + sn;

                        fWriter.AddParagraphAnchor(sst, fTextFont, sanc);
                    }
                }
            } finally {
                fSourceList.Dispose();
            }
        }
    }
}

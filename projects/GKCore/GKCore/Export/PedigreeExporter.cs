/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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

using BSLib;
using BSLib.Calendar;
using GKCommon;
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKCore.Types;

namespace GKCore.Export
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class PedigreeExporter : Exporter
    {
        private class PedigreePerson
        {
            public PedigreePerson Parent;
            public string Id;
            public GEDCOMIndividualRecord IRec;
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
                string order = SysUtils.AdjustNum(FamilyOrder, 2);
                string result = ((Parent == null) ? order : Parent.GetOrderStr() + order);
                return result;
            }

            public string GetInternalStr()
            {
                return SysUtils.AdjustNum(Level, 2) + GetOrderStr();
            }
        }

        private class PedigreeEvent
        {
            public readonly GEDCOMIndividualRecord IRec;
            public readonly GEDCOMCustomEvent Event;
            public readonly UDN Date;

            public PedigreeEvent(GEDCOMIndividualRecord iRec, GEDCOMCustomEvent evt)
            {
                IRec = iRec;
                Event = evt;
                Date = (evt == null) ? UDN.CreateEmpty() : evt.Date.GetUDN();
            }
        }

        public enum PedigreeKind
        {
            pkAscend,
            pkDescend_dAboville,
            pkDescend_Konovalov
        }

        private GEDCOMIndividualRecord fRoot;
        private PedigreeKind fKind;
        private ExtList<PedigreePerson> fPersonList;
        private ShieldState fShieldState;
        private StringList fSourceList;

        private IFont fTitleFont;
        private IFont fChapFont;
        private IFont fPersonFont;
        private IFont fLinkFont;
        private IFont fTextFont, fSupText;

        private PedigreeFormat fFormat;
        private string fTitle;

        public GEDCOMIndividualRecord Root
        {
            get { return fRoot; }
            set { fRoot = value; }
        }

        public PedigreeKind Kind
        {
            get { return fKind; }
            set { fKind = value; }
        }

        public ShieldState ShieldState
        {
            get { return fShieldState; }
            set { fShieldState = value; }
        }

        public PedigreeExporter(IBaseWindow baseWin) : base(baseWin)
        {
        }

        private PedigreePerson FindPerson(GEDCOMIndividualRecord iRec)
        {
            if (iRec == null) return null;

            PedigreePerson res = null;

            int num = fPersonList.Count;
            for (int i = 0; i < num; i++)
            {
                PedigreePerson item = fPersonList[i];
                
                if (item.IRec == iRec) {
                    res = item;
                    break;
                }
            }

            return res;
        }

        private void WritePerson(PedigreePerson person)
        {
            fWriter.beginParagraph(CustomWriter.TextAlignment.taJustify, 6f, 6f);
            fWriter.addParagraphChunkAnchor(GetIdStr(person) + ". " + GKUtils.GetNameString(person.IRec, true, false), fPersonFont, person.Id);
            fWriter.addParagraphChunk(GKUtils.GetPedigreeLifeStr(person.IRec, fOptions.PedigreeOptions.Format), fTextFont);

            if (fOptions.PedigreeOptions.IncludeSources && person.Sources.Count > 0)
            {
                fWriter.addParagraphChunk(" ", fTextFont);

                int num = person.Sources.Count;
                for (int i = 0; i < num; i++) {
                    string lnk = person.Sources[i];

                    if (i > 0) {
                        fWriter.addParagraphChunkLink(", ", fTextFont, "", null, true);
                    }

                    fWriter.addParagraphChunkLink(lnk, fSupText, "src_" + lnk, fLinkFont, true);
                }
            }

            fWriter.endParagraph();

            switch (fFormat) {
                case PedigreeFormat.Excess:
                    WriteExcessFmt(person);
                    break;

                case PedigreeFormat.Compact:
                    WriteCompactFmt(person);
                    break;
            }
        }

        private string idLink(GEDCOMIndividualRecord iRec)
        {
            PedigreePerson person = FindPerson(iRec);
            return (person == null) ? "" : person.Id;
        }

        private string GetIdStr(PedigreePerson person)
        {
            string result = person.Id;

            if (fKind == PedigreeKind.pkDescend_Konovalov && person.Parent != null)
            {
                GEDCOMFamilyRecord family = person.IRec.ChildToFamilyLinks[0].Family;
                string spStr = "";
                int idx = person.Parent.IRec.IndexOfSpouse(family);
                if (person.Parent.IRec.SpouseToFamilyLinks.Count > 1)
                {
                    spStr = "/" + (idx + 1).ToString();
                }
                result += spStr;
            }
            return result;
        }

        private void WriteExcessFmt(PedigreePerson person)
        {
            fWriter.addParagraph(LangMan.LS(LSID.LSID_Sex) + ": " + GKUtils.SexStr(person.IRec.Sex), fTextFont);

            string st = GKUtils.GetLifeExpectancyStr(person.IRec);
            if (st != "?" && st != "") {
                fWriter.addParagraph(LangMan.LS(LSID.LSID_LifeExpectancy) + ": " + st, fTextFont);
            }

            GEDCOMIndividualRecord father, mother;
            GEDCOMFamilyRecord fam = person.IRec.GetParentsFamily();
            if (fam == null) {
                father = null;
                mother = null;
            } else {
                father = fam.GetHusband();
                mother = fam.GetWife();
            }

            if (father != null) {
                fWriter.addParagraphLink(LangMan.LS(LSID.LSID_Father) + ": " + GKUtils.GetNameString(father, true, false) + " ", fTextFont, idLink(father), fLinkFont);
            }
            if (mother != null) {
                fWriter.addParagraphLink(LangMan.LS(LSID.LSID_Mother) + ": " + GKUtils.GetNameString(mother, true, false) + " ", fTextFont, idLink(mother), fLinkFont);
            }

            ExtList<PedigreeEvent> evList = new ExtList<PedigreeEvent>(true);
            try
            {
                int i;
                if (person.IRec.Events.Count > 0)
                {
                    fWriter.addParagraph(LangMan.LS(LSID.LSID_Events) + ":", fTextFont);

                    int num = person.IRec.Events.Count;
                    for (i = 0; i < num; i++)
                    {
                        GEDCOMCustomEvent evt = person.IRec.Events[i];
                        if (!(evt is GEDCOMIndividualAttribute) || fOptions.PedigreeOptions.IncludeAttributes)
                        {
                            evList.Add(new PedigreeEvent(person.IRec, evt));
                        }
                    }
                    WriteEventList(person, evList);
                }

                int num2 = person.IRec.SpouseToFamilyLinks.Count;
                for (i = 0; i < num2; i++)
                {
                    GEDCOMFamilyRecord family = person.IRec.SpouseToFamilyLinks[i].Family;
                    if (!fBase.Context.IsRecordAccess(family.Restriction)) continue;

                    GEDCOMPointer sp;
                    string unk;
                    if (person.IRec.Sex == GEDCOMSex.svMale) {
                        sp = family.Wife;
                        st = LangMan.LS(LSID.LSID_Wife) + ": ";
                        unk = LangMan.LS(LSID.LSID_UnkFemale);
                    } else {
                        sp = family.Husband;
                        st = LangMan.LS(LSID.LSID_Husband) + ": ";
                        unk = LangMan.LS(LSID.LSID_UnkMale);
                    }

                    GEDCOMIndividualRecord irec = sp.Value as GEDCOMIndividualRecord;
                    string sps;
                    if (irec != null) {
                        sps = st + GKUtils.GetNameString(irec, true, false) + GKUtils.GetPedigreeLifeStr(irec, fOptions.PedigreeOptions.Format)/* + this.idLink(this.FindPerson(irec))*/;
                    } else {
                        sps = st + unk;
                    }

                    fWriter.addParagraph(sps, fTextFont);

                    evList.Clear();
                    int childrenCount = family.Children.Count;
                    for (int j = 0; j < childrenCount; j++)
                    {
                        irec = (GEDCOMIndividualRecord)family.Children[j].Value;
                        evList.Add(new PedigreeEvent(irec, irec.FindEvent("BIRT")));
                    }
                    WriteEventList(person, evList);
                }
            }
            finally
            {
                evList.Dispose();
            }

            if (fOptions.PedigreeOptions.IncludeNotes && person.IRec.Notes.Count != 0)
            {
                fWriter.addParagraph(LangMan.LS(LSID.LSID_RPNotes) + ":", fTextFont);

                fWriter.beginList();

                int notesCount = person.IRec.Notes.Count;
                for (int i = 0; i < notesCount; i++)
                {
                    GEDCOMNotes note = person.IRec.Notes[i];
                    fWriter.addListItem(" " + GKUtils.MergeStrings(note.Notes), fTextFont);
                }
                
                fWriter.endList();
            }
        }

        private void WriteCompactFmt(PedigreePerson person)
        {
            if (fOptions.PedigreeOptions.IncludeNotes && person.IRec.Notes.Count != 0)
            {
                int num = person.IRec.Notes.Count;
                for (int i = 0; i < num; i++)
                {
                    GEDCOMNotes note = person.IRec.Notes[i];
                    fWriter.addParagraph(GKUtils.MergeStrings(note.Notes), fTextFont);
                }
            }

            bool spIndex = person.IRec.SpouseToFamilyLinks.Count > 1;

            int num2 = person.IRec.SpouseToFamilyLinks.Count;
            for (int i = 0; i < num2; i++)
            {
                GEDCOMFamilyRecord family = person.IRec.SpouseToFamilyLinks[i].Family;
                if (fBase.Context.IsRecordAccess(family.Restriction))
                {
                    GEDCOMPointer sp;
                    string st;
                    string unk;
                    if (person.IRec.Sex == GEDCOMSex.svMale)
                    {
                        sp = family.Wife;
                        st = LangMan.LS(LSID.LSID_WifeSign);
                        unk = LangMan.LS(LSID.LSID_UnkFemale);
                    }
                    else
                    {
                        sp = family.Husband;
                        st = LangMan.LS(LSID.LSID_HusbSign);
                        unk = LangMan.LS(LSID.LSID_UnkMale);
                    }

                    if (spIndex)
                    {
                        st += (i + 1).ToString();
                    }
                    st += " - ";

                    GEDCOMIndividualRecord irec = sp.Value as GEDCOMIndividualRecord;
                    if (irec != null)
                    {
                        st = st + GKUtils.GetNameString(irec, true, false) + GKUtils.GetPedigreeLifeStr(irec, fOptions.PedigreeOptions.Format)/* + this.idLink(this.FindPerson(irec))*/;
                    }
                    else
                    {
                        st += unk;
                    }

                    fWriter.addParagraph(st, fTextFont);
                }
            }
        }

        private static int EventsCompare(PedigreeEvent item1, PedigreeEvent item2)
        {
            return item1.Date.CompareTo(item2.Date);
        }

        private void WriteEventList(PedigreePerson person, ExtList<PedigreeEvent> evList)
        {
            evList.QuickSort(EventsCompare);

            int num3 = evList.Count;
            for (int i = 0; i < num3; i++)
            {
                GEDCOMCustomEvent evt = evList[i].Event;
                if (evt != null && Equals(evList[i].IRec, person.IRec))
                {
                    if (evt.Name == "BIRT") {
                        evList.Exchange(i, 0);
                    } else if (evt.Name == "DEAT") {
                        evList.Exchange(i, evList.Count - 1);
                    }
                }
            }

            fWriter.beginList();

            int num4 = evList.Count;
            for (int i = 0; i < num4; i++)
            {
                PedigreeEvent evObj = evList[i];
                GEDCOMCustomEvent evt = evObj.Event;
                string li;

                if (evObj.IRec == person.IRec)
                {
                    int ev = GKUtils.GetPersonEventIndex(evt.Name);
                    string st;
                    if (ev == 0) {
                        st = evt.Classification;
                    } else {
                        st = (ev > 0) ? LangMan.LS(GKData.PersonEvents[ev].Name) : evt.Name;
                    }

                    string dt = GKUtils.GEDCOMEventToDateStr(evt, DateFormat.dfDD_MM_YYYY, false);
                    li = dt + ": " + st + ".";
                    if (evt.Place.StringValue != "")
                    {
                        li = li + " " + LangMan.LS(LSID.LSID_Place) + ": " + evt.Place.StringValue;
                    }

                    fWriter.addListItem(" " + li, fTextFont);
                }
                else
                {
                    string dt = (evt == null) ? "?" : GKUtils.GEDCOMEventToDateStr(evt, DateFormat.dfDD_MM_YYYY, false);

                    string st = (evObj.IRec.Sex == GEDCOMSex.svMale) ? ": Родился " : ": Родилась ";

                    li = dt + st + GKUtils.GetNameString(evObj.IRec, true, false);
                    PedigreePerson prs = FindPerson(evObj.IRec);
                    string id = (prs != null) ? prs.Id : "";

                    fWriter.addListItemLink(" " + li + " ", fTextFont, id, fLinkFont);
                }
            }

            fWriter.endList();
        }

        private void InternalGenerate()
        {
            bool includeGens = fOptions.PedigreeOptions.IncludeGenerations;

            fWriter.addParagraph(fTitle, fTitleFont, CustomWriter.TextAlignment.taCenter);

            fPersonList = new ExtList<PedigreePerson>(true);
            fSourceList = new StringList();
            try
            {
                GenStep(null, fRoot, 1, 1);
                ReIndex();

                int curLevel = 0;
                int num = fPersonList.Count;
                for (int i = 0; i < num; i++)
                {
                    PedigreePerson person = fPersonList[i];

                    if (includeGens && curLevel != person.Level)
                    {
                        curLevel = person.Level;
                        string genTitle = LangMan.LS(LSID.LSID_Generation) + " " + SysUtils.GetRome(curLevel);

                        fWriter.beginParagraph(CustomWriter.TextAlignment.taLeft, 12f, 6f);
                        fWriter.addParagraphChunk(genTitle, fChapFont);
                        fWriter.endParagraph();
                    }

                    WritePerson(person);
                }

                if (fSourceList.Count > 0)
                {
                    fWriter.beginParagraph(CustomWriter.TextAlignment.taCenter, 12f, 6f);
                    fWriter.addParagraphChunk(LangMan.LS(LSID.LSID_RPSources), fChapFont);
                    fWriter.endParagraph();

                    int num2 = fSourceList.Count;
                    for (int j = 0; j < num2; j++)
                    {
                        string sn = (j + 1).ToString();
                        string sst = sn + ". " + fSourceList[j];
                        string sanc = "src_" + sn;

                        fWriter.addParagraphAnchor(sst, fTextFont, sanc);
                    }
                }
            }
            finally
            {
                fSourceList.Dispose();
                fPersonList.Dispose();
            }
        }

        public bool Generate(CustomWriter writer)
        {
            bool result = false;

            fFormat = fOptions.PedigreeOptions.Format;

            try
            {
                fWriter = writer;
                fWriter.SetAlbumPage(false);
                fTitle = LangMan.LS(LSID.LSID_ExpPedigree) + ": " + GKUtils.GetNameString(fRoot, true, false);
                fWriter.SetDocumentTitle(fTitle);
                fWriter.SetFileName(fPath);

                IColor clrBlack = AppHost.GfxProvider.CreateColor(0x000000);
                IColor clrBlue = AppHost.GfxProvider.CreateColor(0x0000FF);

                fWriter.beginWrite();
                try
                {
                    fTitleFont = fWriter.CreateFont("", 16f/*20f*/, true, false, clrBlack);
                    fChapFont = fWriter.CreateFont("", 14f/*16f*/, true, false, clrBlack);
                    fPersonFont = fWriter.CreateFont("", 12f/*10f*/, true, false, clrBlack);
                    fLinkFont = fWriter.CreateFont("", 10f/*8f*/, false, true, clrBlue);
                    fTextFont = fWriter.CreateFont("", 10f/*8f*/, false, false, clrBlack);
                    fSupText = fWriter.CreateFont("", ((fWriter is RTFWriter) ? 12f : 5f) /*5f*/, false, false, clrBlue);

                    InternalGenerate();
                    result = true;
                }
                finally
                {
                    fWriter.endWrite();
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("PedigreeExporter.Generate(): " + ex.Message);
                Logger.LogWrite("PedigreeExporter.Generate(): " + ex.StackTrace);
            }

            return result;
        }

        public override void Generate(bool show)
        {
            if (fRoot == null)
            {
                AppHost.StdDialogs.ShowError(LangMan.LS(LSID.LSID_NotSelectedPerson));
                return;
            }

            string availableFormats = LangMan.LS(LSID.LSID_HTMLFilter) + "|" + LangMan.LS(LSID.LSID_RTFFilter);
            availableFormats += "|" + LangMan.LS(LSID.LSID_PDFFilter);

            fPath = AppHost.StdDialogs.GetSaveFile(availableFormats);
            if (string.IsNullOrEmpty(fPath)) return;

            string ext = SysUtils.GetFileExtension(fPath);

            CustomWriter writer;
            if (string.Equals(ext, ".html")) {
                writer = new HTMLWriter();
            } else if (string.Equals(ext, ".rtf")) {
                writer = new RTFWriter();
            } else {
                writer = new PDFWriter();
            }

            bool success = Generate(writer);

            #if !CI_MODE
            if (!success) {
                AppHost.StdDialogs.ShowError(LangMan.LS(LSID.LSID_GenerationFailed));
            } else {
                if (show) ShowResult();
            }
            #endif
        }

        private void GenStep(PedigreePerson parent, GEDCOMIndividualRecord iRec, int level, int familyOrder)
        {
            if (iRec == null) return;

            PedigreePerson res = new PedigreePerson();
            res.Parent = parent;
            res.IRec = iRec;
            res.Level = level;
            res.ChildIdx = 0;
            res.FamilyOrder = familyOrder;
            fPersonList.Add(res);

            if (fOptions.PedigreeOptions.IncludeSources)
            {
                int num = iRec.SourceCitations.Count;
                for (int i = 0; i < num; i++)
                {
                    GEDCOMSourceRecord sourceRec = iRec.SourceCitations[i].Value as GEDCOMSourceRecord;
                    if (sourceRec == null) continue;

                    string srcName = GKUtils.MergeStrings(sourceRec.Title);
                    if (srcName == "") {
                        srcName = sourceRec.FiledByEntry;
                    }

                    int j = fSourceList.IndexOf(srcName);
                    if (j < 0) {
                        j = fSourceList.Add(srcName);
                    }

                    res.Sources.Add((j + 1).ToString());
                }
            }

            if (fKind == PedigreeKind.pkAscend) {
                if (iRec.ChildToFamilyLinks.Count > 0) {
                    GEDCOMFamilyRecord family = iRec.ChildToFamilyLinks[0].Family;
                    if (fBase.Context.IsRecordAccess(family.Restriction))
                    {
                        GEDCOMIndividualRecord prnt;

                        prnt = family.GetWife();
                        GenStep(res, prnt, level + 1, 1);

                        prnt = family.GetHusband();
                        GenStep(res, prnt, level + 1, 1);
                    }
                }
            } else {
                int num2 = iRec.SpouseToFamilyLinks.Count;
                for (int j = 0; j < num2; j++)
                {
                    GEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[j].Family;
                    if (!fBase.Context.IsRecordAccess(family.Restriction)) continue;

                    family.SortChilds();

                    int num3 = family.Children.Count;
                    for (int i = 0; i < num3; i++)
                    {
                        GEDCOMIndividualRecord child = family.Children[i].Value as GEDCOMIndividualRecord;
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
            fPersonList.QuickSort(PersonsCompare);

            int num3 = fPersonList.Count;
            for (int i = 0; i < num3; i++)
            {
                PedigreePerson obj = fPersonList[i];

                switch (fKind)
                {
                    case PedigreeKind.pkDescend_dAboville:
                        if (obj.Parent == null) {
                            obj.Id = "1";
                        } else {
                            obj.Parent.ChildIdx++;
                            obj.Id = obj.Parent.Id + "." + obj.Parent.ChildIdx.ToString();
                        }
                        break;

                    case PedigreeKind.pkAscend:
                    case PedigreeKind.pkDescend_Konovalov:
                        obj.Id = (i + 1).ToString();
                        if (obj.Parent != null)
                        {
                            string pid = obj.Parent.Id;

                            int p = pid.IndexOf("-");
                            if (p >= 0) pid = pid.Substring(0, p);

                            obj.Id = obj.Id + "-" + pid;
                        }
                        break;
                }
            }
        }
    }
}

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
using GKCommon.GEDCOM;
using GKCore.Interfaces;
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

        private PedigreeFormat fFormat;
        private PedigreeKind fKind;
        private ExtList<PedigreePerson> fPersonList;
        private readonly GEDCOMIndividualRecord fRoot;
        private readonly ShieldState fShieldState;
        private StringList fSourceList;

        private IFont fTitleFont;
        private IFont fChapFont;
        private IFont fPersonFont;
        private IFont fLinkFont;
        private IFont fTextFont, fSupText;

        public PedigreeKind Kind
        {
            get { return fKind; }
            set { fKind = value; }
        }

        public GEDCOMIndividualRecord Root
        {
            get { return fRoot; }
        }

        public ShieldState ShieldState
        {
            get { return fShieldState; }
        }

        public PedigreeExporter(IBaseWindow baseWin, GEDCOMIndividualRecord root) : base(baseWin, false)
        {
            fRoot = root;
            fTitle = LangMan.LS(LSID.LSID_ExpPedigree) + ": " + GKUtils.GetNameString(fRoot, true, false);
            fShieldState = baseWin.Context.ShieldState;
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
            fWriter.BeginParagraph(TextAlignment.taJustify, 6f, 6f);
            fWriter.AddParagraphChunkAnchor(GetIdStr(person) + ". " + GKUtils.GetNameString(person.IRec, true, false), fPersonFont, person.Id);
            fWriter.AddParagraphChunk(GKUtils.GetPedigreeLifeStr(person.IRec, fOptions.PedigreeOptions.Format), fTextFont);

            if (fOptions.PedigreeOptions.IncludeSources && person.Sources.Count > 0)
            {
                fWriter.AddParagraphChunk(" ", fTextFont);

                int num = person.Sources.Count;
                for (int i = 0; i < num; i++) {
                    string lnk = person.Sources[i];

                    if (i > 0) {
                        fWriter.AddParagraphChunkLink(", ", fTextFont, "", true);
                    }

                    fWriter.AddParagraphChunkLink(lnk, fSupText, "src_" + lnk, true);
                }
            }

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
            fWriter.AddParagraph(LangMan.LS(LSID.LSID_Sex) + ": " + GKUtils.SexStr(person.IRec.Sex), fTextFont);

            string st = GKUtils.GetLifeExpectancyStr(person.IRec);
            if (st != "?" && st != "") {
                fWriter.AddParagraph(LangMan.LS(LSID.LSID_LifeExpectancy) + ": " + st, fTextFont);
            }

            GEDCOMFamilyRecord fam = person.IRec.GetParentsFamily();
            if (fam != null) {
                GEDCOMIndividualRecord father = fam.GetHusband();
                if (father != null) {
                    fWriter.AddParagraphLink(LangMan.LS(LSID.LSID_Father) + ": " + GKUtils.GetNameString(father, true, false) + " ", fTextFont, idLink(father), fLinkFont);
                }

                GEDCOMIndividualRecord mother = fam.GetWife();
                if (mother != null) {
                    fWriter.AddParagraphLink(LangMan.LS(LSID.LSID_Mother) + ": " + GKUtils.GetNameString(mother, true, false) + " ", fTextFont, idLink(mother), fLinkFont);
                }
            }

            ExtList<PedigreeEvent> evList = new ExtList<PedigreeEvent>(true);
            try
            {
                int i;
                if (person.IRec.Events.Count > 0)
                {
                    fWriter.AddParagraph(LangMan.LS(LSID.LSID_Events) + ":", fTextFont);

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

                    fWriter.AddParagraph(sps, fTextFont);

                    evList.Clear();
                    int childrenCount = family.Children.Count;
                    for (int j = 0; j < childrenCount; j++)
                    {
                        irec = (GEDCOMIndividualRecord)family.Children[j].Value;
                        evList.Add(new PedigreeEvent(irec, irec.FindEvent(GEDCOMTagType.BIRT)));
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
                fWriter.AddParagraph(LangMan.LS(LSID.LSID_RPNotes) + ":", fTextFont);

                fWriter.BeginList();

                int notesCount = person.IRec.Notes.Count;
                for (int i = 0; i < notesCount; i++)
                {
                    GEDCOMNotes note = person.IRec.Notes[i];
                    fWriter.AddListItem(" " + GKUtils.MergeStrings(note.Notes), fTextFont);
                }
                
                fWriter.EndList();
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
                    fWriter.AddParagraph(GKUtils.MergeStrings(note.Notes), fTextFont);
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

                    fWriter.AddParagraph(st, fTextFont);
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
                    if (evt.Name == GEDCOMTagType.BIRT) {
                        evList.Exchange(i, 0);
                    } else if (evt.Name == GEDCOMTagType.DEAT) {
                        evList.Exchange(i, evList.Count - 1);
                    }
                }
            }

            fWriter.BeginList();

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

                    fWriter.AddListItem(" " + li, fTextFont);
                }
                else
                {
                    string dt = (evt == null) ? "?" : GKUtils.GEDCOMEventToDateStr(evt, DateFormat.dfDD_MM_YYYY, false);

                    string st = (evObj.IRec.Sex == GEDCOMSex.svMale) ? ": Родился " : ": Родилась ";

                    li = dt + st + GKUtils.GetNameString(evObj.IRec, true, false);
                    PedigreePerson prs = FindPerson(evObj.IRec);
                    string id = (prs != null) ? prs.Id : "";

                    fWriter.AddListItemLink(" " + li + " ", fTextFont, id, fLinkFont);
                }
            }

            fWriter.EndList();
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
                        srcName = sourceRec.ShortTitle;
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

                    fBase.Context.ProcessFamily(family);

                    int num3 = family.Children.Count;
                    for (int i = 0; i < num3; i++) {
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

        protected override void InternalGenerate()
        {
            bool isRtf = false;
#if !NETSTANDARD
            isRtf = (fWriter is RTFWriter);
#endif

            IColor clrBlack = AppHost.GfxProvider.CreateColor(0x000000);
            IColor clrBlue = AppHost.GfxProvider.CreateColor(0x0000FF);

            fTitleFont = fWriter.CreateFont("", 16f/*20f*/, true, false, clrBlack);
            fChapFont = fWriter.CreateFont("", 14f/*16f*/, true, false, clrBlack);
            fPersonFont = fWriter.CreateFont("", 12f/*10f*/, true, false, clrBlack);
            fLinkFont = fWriter.CreateFont("", 10f/*8f*/, false, true, clrBlue);
            fTextFont = fWriter.CreateFont("", 10f/*8f*/, false, false, clrBlack);
            fSupText = fWriter.CreateFont("", (isRtf ? 12f : 5f) /*5f*/, false, false, clrBlue);

            fFormat = fOptions.PedigreeOptions.Format;

            bool includeGens = fOptions.PedigreeOptions.IncludeGenerations;

            fWriter.AddParagraph(fTitle, fTitleFont, TextAlignment.taCenter);

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
                        string genTitle = LangMan.LS(LSID.LSID_Generation) + " " + ConvertHelper.GetRome(curLevel);

                        fWriter.BeginParagraph(TextAlignment.taLeft, 12f, 6f);
                        fWriter.AddParagraphChunk(genTitle, fChapFont);
                        fWriter.EndParagraph();
                    }

                    WritePerson(person);
                }

                if (fSourceList.Count > 0)
                {
                    fWriter.BeginParagraph(TextAlignment.taCenter, 12f, 6f);
                    fWriter.AddParagraphChunk(LangMan.LS(LSID.LSID_RPSources), fChapFont);
                    fWriter.EndParagraph();

                    int num2 = fSourceList.Count;
                    for (int j = 0; j < num2; j++)
                    {
                        string sn = (j + 1).ToString();
                        string sst = sn + ". " + fSourceList[j];
                        string sanc = "src_" + sn;

                        fWriter.AddParagraphAnchor(sst, fTextFont, sanc);
                    }
                }
            }
            finally
            {
                fSourceList.Dispose();
                fPersonList.Dispose();
            }
        }
    }
}

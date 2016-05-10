/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
using System.Drawing;
using System.Windows.Forms;

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
            //public string BirthDate;
            public readonly List<string> Sources;
            public int FamilyOrder;
            public int ChildIdx;

            public PedigreePerson()
            {
                this.Sources = new List<string>();
            }

            private string GetOrderStr()
            {
                string order = ConvHelper.AdjustNum(this.FamilyOrder, 2);
                string result = ((this.Parent == null) ? order : this.Parent.GetOrderStr() + order);
                return result;
            }

            public string GetInternalStr()
            {
                return ConvHelper.AdjustNum(this.Level, 2) + this.GetOrderStr();
            }
        }

        private class PedigreeEvent
        {
            public readonly GEDCOMIndividualRecord IRec;
            public readonly GEDCOMCustomEvent Event;
            public readonly UDN Date;

            public PedigreeEvent(GEDCOMIndividualRecord iRec, GEDCOMCustomEvent evt)
            {
                this.IRec = iRec;
                this.Event = evt;
                this.Date = GEDCOMUtils.GetUDN(evt);
            }
        }

        public enum PedigreeKind
        {
            pk_dAboville,
            pk_Konovalov
        }

        private GEDCOMIndividualRecord fAncestor;
        private PedigreeKind fKind;
        private ExtList<PedigreePerson> fPersonList;
        private ShieldState fShieldState;
        private StringList fSourceList;

        private object fTitleFont;
        private object fChapFont;
        private object fPersonFont;
        private object fLinkFont;
        private object fTextFont, fSupText;
        
        private PedigreeFormat fFormat;
        private string fTitle;

        public GEDCOMIndividualRecord Ancestor
        {
            get { return this.fAncestor; }
            set { this.fAncestor = value; }
        }

        public PedigreeKind Kind
        {
            get { return this.fKind; }
            set { this.fKind = value; }
        }

        public ShieldState ShieldState
        {
            get { return this.fShieldState; }
            set { this.fShieldState = value; }
        }

        public PedigreeExporter(IBaseWindow aBase) : base(aBase)
        {
        }

        private PedigreePerson FindPerson(GEDCOMIndividualRecord iRec)
        {
            if (iRec == null) return null;

            PedigreePerson res = null;

            int num = this.fPersonList.Count;
            for (int i = 0; i < num; i++)
            {
                PedigreePerson item = this.fPersonList[i];
                
                if (item.IRec == iRec) {
                    res = item;
                    break;
                }
            }

            return res;
        }

        private void WritePerson(PedigreePerson person)
        {
            this.fWriter.beginParagraph(CustomWriter.TextAlignment.taJustify);
            /*p.SpacingBefore = 6f;
			p.SpacingAfter = 6f;*/
            this.fWriter.addParagraphChunkAnchor(this.GetIdStr(person) + ". " + person.IRec.GetNameString(true, false), fPersonFont, person.Id);
            this.fWriter.addParagraphChunk(GKUtils.GetPedigreeLifeStr(person.IRec, this.fOptions.PedigreeOptions.Format), fTextFont);

            if (this.fOptions.PedigreeOptions.IncludeSources && person.Sources.Count > 0)
            {
                this.fWriter.addParagraphChunk(" ", fTextFont);

                int num = person.Sources.Count;
                for (int i = 0; i < num; i++) {
                    string lnk = person.Sources[i];

                    if (i > 0) {
                        this.fWriter.addParagraphChunkLink(", ", fTextFont, "", null, true);
                    }

                    this.fWriter.addParagraphChunkLink(lnk, fSupText, "src_" + lnk, fLinkFont, true);
                }
            }

            this.fWriter.endParagraph();

            switch (fFormat) {
                case PedigreeFormat.Excess:
                    this.WriteExcessFmt(person);
                    break;

                case PedigreeFormat.Compact:
                    this.WriteCompactFmt(person);
                    break;
            }
        }

        /*private Chunk idLink(PedigreePerson person)
		{
		    return (person == null) ? new Chunk() : new Chunk(person.Id, fLinkFont).SetLocalGoto(person.Id);
		}*/

        private string GetIdStr(PedigreePerson person)
        {
            string result = person.Id;

            if (this.fKind == PedigreeKind.pk_Konovalov && person.Parent != null)
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
            this.fWriter.addParagraph(LangMan.LS(LSID.LSID_Sex) + ": " + GKUtils.SexStr(person.IRec.Sex), fTextFont);

            string st = GKUtils.GetLifeExpectancyStr(person.IRec);
            if (st != "?" && st != "") {
                this.fWriter.addParagraph(LangMan.LS(LSID.LSID_LifeExpectancy) + ": " + st, fTextFont);
            }

            GEDCOMIndividualRecord father, mother;
            person.IRec.GetParents(out father, out mother);
            PedigreePerson prs;
            string id;
            if (father != null) {
                prs = this.FindPerson(father);
                id = (prs != null) ? prs.Id : "";
                this.fWriter.addParagraphLink(LangMan.LS(LSID.LSID_Father) + ": " + father.GetNameString(true, false) + " ", fTextFont, id, fLinkFont);
            }
            if (mother != null) {
                prs = this.FindPerson(mother);
                id = (prs != null) ? prs.Id : "";
                this.fWriter.addParagraphLink(LangMan.LS(LSID.LSID_Mother) + ": " + mother.GetNameString(true, false) + " ", fTextFont, id, fLinkFont);
            }

            ExtList<PedigreeEvent> evList = new ExtList<PedigreeEvent>(true);
            try
            {
                int i;
                if (person.IRec.Events.Count > 0)
                {
                    this.fWriter.addParagraph(LangMan.LS(LSID.LSID_Events) + ":", fTextFont);

                    int num = person.IRec.Events.Count;
                    for (i = 0; i < num; i++)
                    {
                        GEDCOMCustomEvent evt = person.IRec.Events[i];
                        if (!(evt is GEDCOMIndividualAttribute) || (evt is GEDCOMIndividualAttribute && this.fOptions.PedigreeOptions.IncludeAttributes))
                        {
                            evList.Add(new PedigreeEvent(person.IRec, evt));
                        }
                    }
                    this.WriteEventList(person, evList);
                }

                int num2 = person.IRec.SpouseToFamilyLinks.Count;
                for (i = 0; i < num2; i++)
                {
                    GEDCOMFamilyRecord family = person.IRec.SpouseToFamilyLinks[i].Family;
                    if (GKUtils.IsRecordAccess(family.Restriction, this.fShieldState))
                    {
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
                            sps = st + irec.GetNameString(true, false) + GKUtils.GetPedigreeLifeStr(irec, this.fOptions.PedigreeOptions.Format)/* + this.idLink(this.FindPerson(irec))*/;
                        } else {
                            sps = st + unk;
                        }

                        this.fWriter.addParagraph(sps, fTextFont);

                        evList.Clear();
                        int num3 = family.Childrens.Count;
                        for (int j = 0; j < num3; j++)
                        {
                            irec = (GEDCOMIndividualRecord)family.Childrens[j].Value;
                            evList.Add(new PedigreeEvent(irec, irec.FindEvent("BIRT")));
                        }
                        this.WriteEventList(person, evList);
                    }
                }
            }
            finally
            {
                evList.Dispose();
            }

            if (this.fOptions.PedigreeOptions.IncludeNotes && person.IRec.Notes.Count != 0)
            {
                this.fWriter.addParagraph(LangMan.LS(LSID.LSID_RPNotes) + ":", fTextFont);

                this.fWriter.beginList();

                int num4 = person.IRec.Notes.Count;
                for (int i = 0; i < num4; i++)
                {
                    GEDCOMNotes note = person.IRec.Notes[i];
                    this.fWriter.addListItem(" " + GKUtils.MergeStrings(note.Notes), fTextFont);
                }
                
                this.fWriter.endList();
            }
        }

        private void WriteCompactFmt(PedigreePerson person)
        {
            if (this.fOptions.PedigreeOptions.IncludeNotes && person.IRec.Notes.Count != 0)
            {
                int num = person.IRec.Notes.Count;
                for (int i = 0; i < num; i++)
                {
                    GEDCOMNotes note = person.IRec.Notes[i];
                    this.fWriter.addParagraph(GKUtils.MergeStrings(note.Notes), fTextFont);
                }
            }

            try
            {
                bool spIndex = person.IRec.SpouseToFamilyLinks.Count > 1;

                int num2 = person.IRec.SpouseToFamilyLinks.Count;
                for (int i = 0; i < num2; i++)
                {
                    GEDCOMFamilyRecord family = person.IRec.SpouseToFamilyLinks[i].Family;
                    if (GKUtils.IsRecordAccess(family.Restriction, this.fShieldState))
                    {
                        GEDCOMPointer sp;
                        string st;
                        string unk;
                        if (person.IRec.Sex == GEDCOMSex.svMale)
                        {
                            sp = family.Wife;
                            st = "Ж";
                            unk = LangMan.LS(LSID.LSID_UnkFemale);
                        }
                        else
                        {
                            sp = family.Husband;
                            st = "М";
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
                            st = st + irec.GetNameString(true, false) + GKUtils.GetPedigreeLifeStr(irec, this.fOptions.PedigreeOptions.Format)/* + this.idLink(this.FindPerson(irec))*/;
                        }
                        else
                        {
                            st += unk;
                        }

                        this.fWriter.addParagraph(st, fTextFont);
                    }
                }
            }
            finally
            {
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
                if (evt != null && object.Equals(evList[i].IRec, person.IRec))
                {
                    if (evt.Name == "BIRT") {
                        evList.Exchange(i, 0);
                    } else if (evt.Name == "DEAT") {
                        evList.Exchange(i, evList.Count - 1);
                    }
                }
            }

            this.fWriter.beginList();

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
                        st = evt.Detail.Classification;
                    } else {
                        st = (ev > 0) ? LangMan.LS(GKData.PersonEvents[ev].Name) : evt.Name;
                    }

                    string dt = GKUtils.GEDCOMEventToDateStr(evt, DateFormat.dfDD_MM_YYYY, false);
                    li = dt + ": " + st + ".";
                    if (evt.Detail.Place.StringValue != "")
                    {
                        li = li + " " + LangMan.LS(LSID.LSID_Place) + ": " + evt.Detail.Place.StringValue;
                    }

                    this.fWriter.addListItem(" " + li, fTextFont);
                }
                else
                {
                    string dt = (evt == null) ? "?" : GKUtils.GEDCOMEventToDateStr(evt, DateFormat.dfDD_MM_YYYY, false);

                    string st = (evObj.IRec.Sex == GEDCOMSex.svMale) ? ": Родился " : ": Родилась ";

                    li = dt + st + evObj.IRec.GetNameString(true, false);
                    PedigreePerson prs;
                    string id;
                    prs = this.FindPerson(evObj.IRec);
                    id = (prs != null) ? prs.Id : "";

                    this.fWriter.addListItemLink(" " + li + " ", fTextFont, id, fLinkFont);
                }
            }

            this.fWriter.endList();
        }

        private void InternalGenerate()
        {
            this.fWriter.addParagraph(fTitle, fTitleFont, CustomWriter.TextAlignment.taCenter /*, SpacingAfter = 6f*/);

            this.fPersonList = new ExtList<PedigreePerson>(true);
            this.fSourceList = new StringList();
            try
            {
                this.GenStep(null, this.fAncestor, 1, 1);
                this.ReIndex();

                int curLevel = 0;
                int num = this.fPersonList.Count;
                for (int i = 0; i < num; i++)
                {
                    PedigreePerson person = this.fPersonList[i];
                    if (curLevel != person.Level)
                    {
                        curLevel = person.Level;
                        string genTitle = LangMan.LS(LSID.LSID_Generation) + " " + ConvHelper.GetRome(curLevel);

                        this.fWriter.addParagraph(genTitle, fChapFont, CustomWriter.TextAlignment.taLeft/*, SpacingBefore = 2f, SpacingAfter = 2f*/);
                    }

                    this.WritePerson(person);
                }

                if (this.fSourceList.Count > 0)
                {
                    this.fWriter.addParagraph(LangMan.LS(LSID.LSID_RPSources), fChapFont, CustomWriter.TextAlignment.taCenter);

                    int num2 = this.fSourceList.Count;
                    for (int j = 0; j < num2; j++)
                    {
                        string sn = (j + 1).ToString();
                        string sst = sn + ". " + this.fSourceList[j];
                        string sanc = "src_" + sn;

                        this.fWriter.addParagraphAnchor(sst, fTextFont, sanc);
                    }
                }
            }
            finally
            {
                this.fSourceList.Dispose();
                this.fPersonList.Dispose();
            }
        }

        public override void Generate(bool show)
        {
            this.fFormat = this.fOptions.PedigreeOptions.Format;

            if (this.fAncestor == null)
            {
                GKUtils.ShowError(LangMan.LS(LSID.LSID_NotSelectedPerson));
                return;
            }

            bool success = false;
            if (!this.IsRequireFilename("HTML files (*.html)|*.html|PDF files (*.pdf)|*.pdf")) return;

            string ext = FileHelper.GetFileExtension(this.fPath);

            if (string.Equals(ext, ".html")) {
                this.fWriter = new HTMLWriter();
            } else {
                this.fWriter = new PDFWriter();
            }

            this.fWriter.setAlbumPage(false);

            try
            {
                this.fTitle = LangMan.LS(LSID.LSID_ExpPedigree) + ": " + this.fAncestor.GetNameString(true, false);
                this.fWriter.setDocumentTitle(this.fTitle);
                this.fWriter.setFileName(this.fPath);

                fTitleFont = this.fWriter.createFont("", 16f/*20f*/, true, false, Color.Black);
                fChapFont = this.fWriter.createFont("", 14f/*16f*/, true, false, Color.Black);
                fPersonFont = this.fWriter.createFont("", 12f/*10f*/, true, false, Color.Black);
                fLinkFont = this.fWriter.createFont("", 10f/*8f*/, false, true, Color.Blue);
                fTextFont = this.fWriter.createFont("", 10f/*8f*/, false, false, Color.Black);
                fSupText = this.fWriter.createFont("", 5f, false, false, Color.Blue);

                this.fWriter.beginWrite();
                try
                {
                    this.InternalGenerate();
                    success = true;
                }
                finally
                {
                    this.fWriter.endWrite();
                }
            }
            catch (Exception ex)
            {
                this.fBase.Host.LogWrite("PedigreeExporter.Generate(): " + ex.Message);
                this.fBase.Host.LogWrite("PedigreeExporter.Generate(): " + ex.StackTrace);
            }

            if (!success) {
                MessageBox.Show(LangMan.LS(LSID.LSID_GenerationFailed));
            } else {
                if (show) this.ShowResult();
            }
        }

        private void GenStep(PedigreePerson parent, GEDCOMIndividualRecord iRec, int level, int familyOrder)
        {
            if (iRec != null)
            {
                PedigreePerson res = new PedigreePerson();
                res.Parent = parent;
                res.IRec = iRec;
                res.Level = level;
                res.ChildIdx = 0;
                //res.BirthDate = GKUtils.GetBirthDate(iRec, TDateFormat.dfYYYY_MM_DD, true);
                res.FamilyOrder = familyOrder;
                this.fPersonList.Add(res);

                //string[] i_sources = new string[0];

                if (this.fOptions.PedigreeOptions.IncludeSources)
                {
                    int num = iRec.SourceCitations.Count;
                    for (int i = 0; i < num; i++)
                    {
                        GEDCOMSourceRecord sourceRec = iRec.SourceCitations[i].Value as GEDCOMSourceRecord;

                        if (sourceRec != null) {
                            string srcName = GKUtils.MergeStrings(sourceRec.Title);
                            if (srcName == "") {
                                srcName = sourceRec.FiledByEntry;
                            }

                            int j = this.fSourceList.IndexOf(srcName);
                            if (j < 0) {
                                j = this.fSourceList.Add(srcName);
                            }

                            res.Sources.Add((j + 1).ToString());
                        }
                    }
                }

                int num2 = iRec.SpouseToFamilyLinks.Count;
                for (int j = 0; j < num2; j++)
                {
                    GEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[j].Family;
                    if (GKUtils.IsRecordAccess(family.Restriction, this.fShieldState))
                    {
                        family.SortChilds();

                        int num3 = family.Childrens.Count;
                        for (int i = 0; i < num3; i++)
                        {
                            GEDCOMIndividualRecord child = family.Childrens[i].Value as GEDCOMIndividualRecord;
                            GenStep(res, child, level + 1, i + 1);
                        }
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

            int num3 = this.fPersonList.Count;
            for (int i = 0; i < num3; i++)
            {
                PedigreePerson obj = this.fPersonList[i];

                switch (this.fKind)
                {
                    case PedigreeKind.pk_dAboville:
                        if (obj.Parent == null) {
                            obj.Id = "1";
                        } else {
                            obj.Parent.ChildIdx++;
                            obj.Id = obj.Parent.Id + "." + obj.Parent.ChildIdx.ToString();
                        }
                        break;

                    case PedigreeKind.pk_Konovalov:
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

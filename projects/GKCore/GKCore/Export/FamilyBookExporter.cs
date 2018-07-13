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
using BSLib;
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKCore.Types;

namespace GKCore.Export
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class FamilyBookExporter : ReportExporter
    {
        private enum BookCatalog
        {
            Catalog_First = 0,

            Catalog_BirthYears = Catalog_First,
            Catalog_DeathYears,
            Catalog_BirthPlaces,
            Catalog_DeathPlaces,
            Catalog_DeathCauses,
            Catalog_Occupations,
            Catalog_Religion,
            Catalog_Sources,

            Catalog_Last = Catalog_Sources
        }

        private sealed class CatalogProps
        {
            public readonly string Sign;
            public readonly string Title;
            public StringList Index;

            public CatalogProps(string sign, string title)
            {
                Sign = sign;
                Title = title;
                Index = null;
            }
        }

        private readonly CatalogProps[] BookCatalogs = {
            new CatalogProps("Catalog_BirthYears", LangMan.LS(LSID.LSID_BirthYears)),
            new CatalogProps("Catalog_DeathYears", LangMan.LS(LSID.LSID_DeathYears)),
            new CatalogProps("Catalog_BirthPlaces", LangMan.LS(LSID.LSID_MSBirthPlaces)),
            new CatalogProps("Catalog_DeathPlaces", LangMan.LS(LSID.LSID_MSDeathPlaces)),
            new CatalogProps("Catalog_DeathCauses", LangMan.LS(LSID.LSID_DeathCauses)),
            new CatalogProps("Catalog_Occupations", LangMan.LS(LSID.LSID_Occupation)),
            new CatalogProps("Catalog_Religion", LangMan.LS(LSID.LSID_Religion)),
            new CatalogProps("Catalog_Sources", LangMan.LS(LSID.LSID_RPSources))
        };

        private IFont fTitleFont;
        private IFont fChapFont;
        private IFont fSubchapFont;
        private IFont fLinkFont;
        private IFont fTextFont;
        private IFont fBoldFont;
        private IFont fSymFont;

        private StringList mainIndex;
        private StringList byIndex, dyIndex, bpIndex, dpIndex;
        private StringList deathCauses, occuIndex, reliIndex, sourcesIndex;

        // temp options
        public bool SkipEmptyCatalogs = true;
        public bool CatalogNewPages = false;
        public bool IncludeEvents = true;
        public bool IncludeNotes = true;


        public FamilyBookExporter(IBaseWindow baseWin)
            : base(baseWin, true)
        {
            fTitle = LangMan.LS(LSID.LSID_FamilyBook);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (mainIndex != null)
                    mainIndex.Dispose();
                if (byIndex != null)
                    byIndex.Dispose();
                if (dyIndex != null)
                    dyIndex.Dispose();
                if (bpIndex != null)
                    bpIndex.Dispose();
                if (dpIndex != null)
                    dpIndex.Dispose();
                if (deathCauses != null)
                    deathCauses.Dispose();
                if (occuIndex != null)
                    occuIndex.Dispose();
                if (reliIndex != null)
                    reliIndex.Dispose();
                if (sourcesIndex != null)
                    sourcesIndex.Dispose();
            }
            base.Dispose(disposing);
        }

        protected override void InternalGenerate()
        {
            try {
                PrepareData();

                IColor clrBlack = AppHost.GfxProvider.CreateColor(0x000000);
                IColor clrBlue = AppHost.GfxProvider.CreateColor(0x0000FF);

                fTitleFont = fWriter.CreateFont("", 30f, true, false, clrBlack);
                fChapFont = fWriter.CreateFont("", 16f, true, false, clrBlack);
                fSubchapFont = fWriter.CreateFont("", 14f, true, false, clrBlack);
                fLinkFont = fWriter.CreateFont("", 8f, false, true, clrBlue);
                fTextFont = fWriter.CreateFont("", 8f, false, false, clrBlack);
                fBoldFont = fWriter.CreateFont("", 8f, true, false, clrBlack);
                fSymFont = fWriter.CreateFont("", 12f, true, false, clrBlack);

                fWriter.EnablePageNumbers();

                var pageSize = fWriter.GetPageSize();
                float halfpage = (pageSize.GetHeight() - (fTitleFont.Size * 4)) / 2f;
                fWriter.NewLine(0.0f, halfpage);
                fWriter.AddParagraph(fTitle, fTitleFont, TextAlignment.taCenter);
                fWriter.NewPage();

                fWriter.AddParagraph(LangMan.LS(LSID.LSID_TableOfContents), fChapFont);
                fWriter.NewLine();
                fWriter.AddParagraphLink("1. " + LangMan.LS(LSID.LSID_PersonalRecords), fLinkFont, "IndividualRecords");
                fWriter.AddParagraphLink("2. " + LangMan.LS(LSID.LSID_Indexes), fLinkFont, "Catalogs");

                int catNum = 0;
                for (BookCatalog cat = BookCatalog.Catalog_First; cat <= BookCatalog.Catalog_Last; cat++) {
                    CatalogProps catProps = BookCatalogs[(int)cat];

                    if (!SkipEmptyCatalogs || catProps.Index.Count > 0) {
                        catNum++;
                        string title = "2." + catNum.ToString() + ". " + catProps.Title;

                        fWriter.BeginParagraph(TextAlignment.taLeft, 0.0f, 0.0f, 1f);
                        fWriter.AddParagraphChunkLink(title, fLinkFont, catProps.Sign);
                        fWriter.EndParagraph();
                    }
                }

                fWriter.NewPage();

                fWriter.BeginParagraph(TextAlignment.taCenter, 0, 20f);
                fWriter.AddParagraphChunkAnchor(LangMan.LS(LSID.LSID_PersonalRecords), fChapFont, "IndividualRecords");
                fWriter.EndParagraph();
                fWriter.NewLine();

                fWriter.BeginMulticolumns(3, 10f);
                char sym = '!';
                int num = mainIndex.Count;
                for (int i = 0; i < num; i++) {
                    string text = mainIndex[i];
                    GEDCOMIndividualRecord iRec = mainIndex.GetObject(i) as GEDCOMIndividualRecord;

                    char isym = text[0];
                    if ((isym >= 'A' && isym <= 'Z') || (isym >= 'А' && isym <= 'Я')) {
                        if (sym != isym) {
                            fWriter.AddParagraph("" + isym, fSymFont, TextAlignment.taCenter);
                            fWriter.NewLine();
                            sym = isym;
                        }
                    }

                    ExposePerson(iRec, text);

                    fWriter.NewLine();
                }
                fWriter.EndMulticolumns();

                fWriter.NewPage();

                fWriter.BeginParagraph(TextAlignment.taCenter, 0, 20f);
                fWriter.AddParagraphChunkAnchor(LangMan.LS(LSID.LSID_Indexes), fChapFont, "Catalogs");
                fWriter.EndParagraph();
                fWriter.NewLine();

                fWriter.BeginMulticolumns(3, 10f);
                for (BookCatalog cat = BookCatalog.Catalog_First; cat <= BookCatalog.Catalog_Last; cat++) {
                    CatalogProps catProps = BookCatalogs[(int)cat];
                    
                    if (!SkipEmptyCatalogs || catProps.Index.Count > 0) {
                        if (CatalogNewPages) {
                            fWriter.NewPage();
                            fWriter.BeginMulticolumns(3, 10f);
                        }

                        ExposeCatalog(catProps);
                    }
                }

                fWriter.EndMulticolumns();
            } catch (Exception ex) {
                Logger.LogWrite("FamilyBookExporter.InternalGenerate(): " + ex.Message);
                throw;
            }
        }

        private void PrepareData()
        {
            mainIndex = new StringList();
            byIndex = new StringList();
            dyIndex = new StringList();

            bpIndex = new StringList();
            dpIndex = new StringList();
            
            deathCauses = new StringList();
            occuIndex = new StringList();
            reliIndex = new StringList();
            sourcesIndex = new StringList();
            
            GEDCOMRecord rec;

            var iEnum = fTree.GetEnumerator(GEDCOMRecordType.rtIndividual);
            while (iEnum.MoveNext(out rec)) {
                GEDCOMIndividualRecord iRec = (GEDCOMIndividualRecord)rec;
                string text = GKUtils.GetNameString(iRec, true, false);
                string st;

                mainIndex.AddObject(text, iRec);

                int evNum = iRec.Events.Count;
                for (int k = 0; k < evNum; k++) {
                    GEDCOMCustomEvent evt = iRec.Events[k];
                    if (evt == null)
                        continue;

                    int srcNum2 = evt.SourceCitations.Count;
                    for (int m = 0; m < srcNum2; m++) {
                        GEDCOMSourceRecord src = evt.SourceCitations[m].Value as GEDCOMSourceRecord;
                        if (src == null)
                            continue;

                        st = src.FiledByEntry;
                        if (string.IsNullOrEmpty(st))
                            st = src.Title.Text;
                        PrepareSpecIndex(sourcesIndex, st, iRec);
                    }

                    // The analysis places
                    //						st = ev.Detail.Place.StringValue;
                    //						if (!string.IsNullOrEmpty(st)) PrepareSpecIndex(places, st, iRec);

                    if (evt.Name == "BIRT") {
                        // Analysis on births
                        PrepareEventYear(byIndex, evt, iRec);
                        st = GKUtils.GetPlaceStr(evt, false);
                        if (!string.IsNullOrEmpty(st))
                            PrepareSpecIndex(bpIndex, st, iRec);
                    } else if (evt.Name == "DEAT") {
                        // Analysis by causes of death
                        PrepareEventYear(dyIndex, evt, iRec);
                        st = GKUtils.GetPlaceStr(evt, false);
                        if (!string.IsNullOrEmpty(st))
                            PrepareSpecIndex(dpIndex, st, iRec);

                        st = evt.Cause;
                        if (!string.IsNullOrEmpty(st))
                            PrepareSpecIndex(deathCauses, st, iRec);
                    } else if (evt.Name == "OCCU") {
                        // Analysis by occupation
                        st = evt.StringValue;
                        if (!string.IsNullOrEmpty(st))
                            PrepareSpecIndex(occuIndex, st, iRec);
                    } else if (evt.Name == "RELI") {
                        // Analysis by religion
                        st = evt.StringValue;
                        if (!string.IsNullOrEmpty(st))
                            PrepareSpecIndex(reliIndex, st, iRec);
                    }
                }

                int srcNum = iRec.SourceCitations.Count;
                for (int k = 0; k < srcNum; k++) {
                    GEDCOMSourceRecord src = iRec.SourceCitations[k].Value as GEDCOMSourceRecord;
                    if (src == null)
                        continue;

                    st = src.FiledByEntry;
                    if (string.IsNullOrEmpty(st))
                        st = src.Title.Text;
                    PrepareSpecIndex(sourcesIndex, st, iRec);
                }
            }

            mainIndex.Sort();

            BookCatalogs[(int)BookCatalog.Catalog_BirthYears].Index = byIndex;
            BookCatalogs[(int)BookCatalog.Catalog_DeathYears].Index = dyIndex;
            BookCatalogs[(int)BookCatalog.Catalog_BirthPlaces].Index = bpIndex;
            BookCatalogs[(int)BookCatalog.Catalog_DeathPlaces].Index = dpIndex;
            BookCatalogs[(int)BookCatalog.Catalog_DeathCauses].Index = deathCauses;
            BookCatalogs[(int)BookCatalog.Catalog_Occupations].Index = occuIndex;
            BookCatalogs[(int)BookCatalog.Catalog_Religion].Index = reliIndex;
            BookCatalogs[(int)BookCatalog.Catalog_Sources].Index = sourcesIndex;
        }

        private void ExposePerson(GEDCOMIndividualRecord iRec, string iName)
        {
            fWriter.BeginParagraph(TextAlignment.taLeft, 0, 0, 0, true);
            fWriter.AddParagraphChunkAnchor(iName, fBoldFont, iRec.XRef);
            fWriter.AddParagraphChunk(GKUtils.GetPedigreeLifeStr(iRec, PedigreeFormat.Compact), fTextFont);
            fWriter.EndParagraph();

            IImage image = fBase.Context.GetPrimaryBitmap(iRec, 0, 0, false);
            fWriter.AddImage(image);

            GEDCOMIndividualRecord father, mother;
            GEDCOMFamilyRecord fam = iRec.GetParentsFamily();
            if (fam == null) {
                father = null;
                mother = null;
            } else {
                father = fam.GetHusband();
                mother = fam.GetWife();
            }

            if (father != null) {
                fWriter.BeginParagraph(TextAlignment.taLeft, 0, 0, 0);
                fWriter.AddParagraphChunk(LangMan.LS(LSID.LSID_Father) + ": ", fTextFont);
                fWriter.AddParagraphChunkLink(GKUtils.GetNameString(father, true, false), fLinkFont, father.XRef);
                fWriter.EndParagraph();
            }

            if (mother != null) {
                fWriter.BeginParagraph(TextAlignment.taLeft, 0, 0, 0);
                fWriter.AddParagraphChunk(LangMan.LS(LSID.LSID_Mother) + ": ", fTextFont);
                fWriter.AddParagraphChunkLink(GKUtils.GetNameString(mother, true, false), fLinkFont, mother.XRef);
                fWriter.EndParagraph();
            }

            if (IncludeEvents && iRec.Events.Count != 0) {
                int num = iRec.Events.Count;
                for (int i = 0; i < num; i++) {
                    GEDCOMCustomEvent evt = iRec.Events[i];
                    if (evt.Name == "BIRT" || evt.Name == "DEAT")
                        continue;
                    
                    string evtName = GKUtils.GetEventName(evt);
                    string evtVal = evt.StringValue;
                    string evtDesc = GKUtils.GetEventDesc(evt, false);

                    string tmp = evtName + ": " + evtVal;
                    if (evtVal != "")
                        tmp += ", ";
                    tmp += evtDesc;

                    fWriter.AddParagraph(tmp, fTextFont);
                }
            }

            if (IncludeNotes && iRec.Notes.Count != 0) {
                int num = iRec.Notes.Count;
                for (int i = 0; i < num; i++) {
                    GEDCOMNotes note = iRec.Notes[i];
                    fWriter.AddParagraph(GKUtils.MergeStrings(note.Notes), fTextFont);
                }
            }
        }

        private void ExposeCatalog(CatalogProps catProps)
        {
            StringList index = catProps.Index;
            if (index == null)
                return;

            fWriter.BeginParagraph(TextAlignment.taCenter, 0, 0, 0);
            fWriter.AddParagraphAnchor(catProps.Title, fSubchapFont, catProps.Sign);
            fWriter.EndParagraph();
            fWriter.NewLine();

            index.Sort();
            int num = index.Count;
            for (int i = 0; i < num; i++) {
                fWriter.BeginParagraph(TextAlignment.taLeft, 0, 20, 0);
                fWriter.AddParagraphChunk(index[i], fSymFont);
                fWriter.EndParagraph();
                fWriter.NewLine();

                StringList persons = (StringList)index.GetObject(i);

                persons.Sort();
                int num2 = persons.Count;
                for (int k = 0; k < num2; k++) {
                    GEDCOMIndividualRecord iRec = (GEDCOMIndividualRecord)persons.GetObject(k);

                    fWriter.BeginParagraph(TextAlignment.taLeft, 0, 0, 0);
                    fWriter.AddParagraphChunkLink(persons[k], fTextFont, iRec.XRef);
                    fWriter.EndParagraph();
                }

                fWriter.NewLine(0, 10f);
            }
        }

        private static void PrepareSpecIndex(StringList index, string val, GEDCOMIndividualRecord iRec)
        {
            if (index == null)
                throw new ArgumentNullException("index");

            if (iRec == null)
                throw new ArgumentNullException("iRec");

            StringList persons;

            int idx = index.IndexOf(val);
            if (idx < 0) {
                persons = new StringList();
                index.AddObject(val, persons);
            } else {
                persons = (StringList)index.GetObject(idx);
            }

            if (persons.IndexOfObject(iRec) < 0) {
                persons.AddObject(GKUtils.GetNameString(iRec, true, false), iRec);
            }
        }

        private static void PrepareEventYear(StringList index, GEDCOMCustomEvent evt, GEDCOMIndividualRecord iRec)
        {
            if (evt == null)
                return;

            int dtY = evt.GetChronologicalYear();
            if (dtY != 0) {
                PrepareSpecIndex(index, dtY.ToString(), iRec);
            }
        }
    }
}

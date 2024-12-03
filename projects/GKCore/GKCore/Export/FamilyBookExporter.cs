/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Design.Graphics;
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
            new CatalogProps("Catalog_BirthYears", LangMan.LS(LSID.BirthYears)),
            new CatalogProps("Catalog_DeathYears", LangMan.LS(LSID.DeathYears)),
            new CatalogProps("Catalog_BirthPlaces", LangMan.LS(LSID.MSBirthPlaces)),
            new CatalogProps("Catalog_DeathPlaces", LangMan.LS(LSID.MSDeathPlaces)),
            new CatalogProps("Catalog_DeathCauses", LangMan.LS(LSID.DeathCauses)),
            new CatalogProps("Catalog_Occupations", LangMan.LS(LSID.Occupation)),
            new CatalogProps("Catalog_Religion", LangMan.LS(LSID.Religion)),
            new CatalogProps("Catalog_Sources", LangMan.LS(LSID.RPSources))
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
            fTitle = LangMan.LS(LSID.FamilyBook);
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

                fWriter.AddParagraph(LangMan.LS(LSID.TableOfContents), fChapFont);
                fWriter.NewLine();
                fWriter.AddParagraphLink("1. " + LangMan.LS(LSID.PersonalRecords), fLinkFont, "IndividualRecords");
                fWriter.AddParagraphLink("2. " + LangMan.LS(LSID.Indexes), fLinkFont, "Catalogs");

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
                fWriter.AddParagraphChunkAnchor(LangMan.LS(LSID.PersonalRecords), fChapFont, "IndividualRecords");
                fWriter.EndParagraph();
                fWriter.NewLine();

                fWriter.BeginMulticolumns(3, 10f);
                char sym = '!';
                int num = mainIndex.Count;
                for (int i = 0; i < num; i++) {
                    string text = mainIndex[i];
                    GDMIndividualRecord iRec = mainIndex.GetObject(i) as GDMIndividualRecord;

                    char isym = (string.IsNullOrEmpty(text)) ? '?' : text[0];
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
                fWriter.AddParagraphChunkAnchor(LangMan.LS(LSID.Indexes), fChapFont, "Catalogs");
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
                Logger.WriteError("FamilyBookExporter.InternalGenerate()", ex);
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
            
            var iEnum = fTree.GetEnumerator<GDMIndividualRecord>();
            GDMIndividualRecord iRec;
            while (iEnum.MoveNext(out iRec)) {
                string text = GKUtils.GetNameString(iRec, true, false);
                string st;

                mainIndex.AddObject(text, iRec);

                int evNum = iRec.Events.Count;
                for (int k = 0; k < evNum; k++) {
                    GDMCustomEvent evt = iRec.Events[k];
                    if (evt == null)
                        continue;

                    int srcNum2 = evt.SourceCitations.Count;
                    for (int m = 0; m < srcNum2; m++) {
                        var sourceRec = fTree.GetPtrValue<GDMSourceRecord>(evt.SourceCitations[m]);
                        if (sourceRec == null) continue;

                        st = sourceRec.ShortTitle;
                        if (string.IsNullOrEmpty(st))
                            st = sourceRec.Title.Lines.Text;
                        PrepareSpecIndex(sourcesIndex, st, iRec);
                    }

                    // The analysis places
                    // st = ev.Detail.Place.StringValue;
                    // if (!string.IsNullOrEmpty(st)) PrepareSpecIndex(places, st, iRec);

                    var evtType = evt.GetTagType();
                    if (evtType == GEDCOMTagType.BIRT) {
                        // Analysis on births
                        PrepareEventYear(byIndex, evt, iRec);
                        st = GKUtils.GetPlaceStr(evt, false);
                        if (!string.IsNullOrEmpty(st))
                            PrepareSpecIndex(bpIndex, st, iRec);
                    } else if (evtType == GEDCOMTagType.DEAT) {
                        // Analysis by causes of death
                        PrepareEventYear(dyIndex, evt, iRec);
                        st = GKUtils.GetPlaceStr(evt, false);
                        if (!string.IsNullOrEmpty(st))
                            PrepareSpecIndex(dpIndex, st, iRec);

                        st = evt.Cause;
                        if (!string.IsNullOrEmpty(st))
                            PrepareSpecIndex(deathCauses, st, iRec);
                    } else if (evtType == GEDCOMTagType.OCCU) {
                        // Analysis by occupation
                        st = evt.StringValue;
                        if (!string.IsNullOrEmpty(st))
                            PrepareSpecIndex(occuIndex, st, iRec);
                    } else if (evtType == GEDCOMTagType.RELI) {
                        // Analysis by religion
                        st = evt.StringValue;
                        if (!string.IsNullOrEmpty(st))
                            PrepareSpecIndex(reliIndex, st, iRec);
                    }
                }

                int srcNum = iRec.SourceCitations.Count;
                for (int k = 0; k < srcNum; k++) {
                    var sourceRec = fTree.GetPtrValue<GDMSourceRecord>(iRec.SourceCitations[k]);
                    if (sourceRec == null) continue;

                    st = sourceRec.ShortTitle;
                    if (string.IsNullOrEmpty(st))
                        st = sourceRec.Title.Lines.Text;
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

        private void ExposePerson(GDMIndividualRecord iRec, string iName)
        {
            fWriter.BeginParagraph(TextAlignment.taLeft, 0, 0, 0, true);
            fWriter.AddParagraphChunkAnchor(iName, fBoldFont, iRec.XRef);
            fWriter.AddParagraphChunk(GKUtils.GetPedigreeLifeStr(iRec, PedigreeFormat.Compact), fTextFont);
            fWriter.EndParagraph();

            IImage image = fBase.Context.GetPrimaryBitmap(iRec, fDefImageWidth, fDefImageHeight, false);
            fWriter.AddImage(image, TextAlignment.taRight);

            GDMIndividualRecord father, mother;
            fBase.Context.Tree.GetParents(iRec, out father, out mother);

            if (father != null) {
                fWriter.BeginParagraph(TextAlignment.taLeft, 0, 0, 0);
                fWriter.AddParagraphChunk(LangMan.LS(LSID.Father) + ": ", fTextFont);
                fWriter.AddParagraphChunkLink(GKUtils.GetNameString(father, true, false), fLinkFont, father.XRef);
                fWriter.EndParagraph();
            }

            if (mother != null) {
                fWriter.BeginParagraph(TextAlignment.taLeft, 0, 0, 0);
                fWriter.AddParagraphChunk(LangMan.LS(LSID.Mother) + ": ", fTextFont);
                fWriter.AddParagraphChunkLink(GKUtils.GetNameString(mother, true, false), fLinkFont, mother.XRef);
                fWriter.EndParagraph();
            }

            if (IncludeEvents && iRec.HasEvents) {
                int num = iRec.Events.Count;
                for (int i = 0; i < num; i++) {
                    GDMCustomEvent evt = iRec.Events[i];
                    var evtType = evt.GetTagType();
                    if (evtType == GEDCOMTagType.BIRT || evtType == GEDCOMTagType.DEAT)
                        continue;
                    
                    string evtName = GKUtils.GetEventName(evt);
                    string evtVal = evt.StringValue;
                    string evtDesc = GKUtils.GetEventDesc(fBase.Context.Tree, evt, false);

                    string tmp = evtName + ": " + evtVal;
                    if (evtVal != "")
                        tmp += ", ";
                    tmp += evtDesc;

                    fWriter.AddParagraph(tmp, fTextFont);
                }
            }

            if (IncludeNotes && iRec.HasNotes) {
                int num = iRec.Notes.Count;
                for (int i = 0; i < num; i++) {
                    GDMLines noteLines = fTree.GetNoteLines(iRec.Notes[i]);
                    fWriter.AddParagraph(GKUtils.MergeStrings(noteLines), fTextFont);
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
                    GDMIndividualRecord iRec = (GDMIndividualRecord)persons.GetObject(k);

                    fWriter.BeginParagraph(TextAlignment.taLeft, 0, 0, 0);
                    fWriter.AddParagraphChunkLink(persons[k], fTextFont, iRec.XRef);
                    fWriter.EndParagraph();
                }

                fWriter.NewLine(0, 10f);
            }
        }

        private static void PrepareSpecIndex(StringList index, string val, GDMIndividualRecord iRec)
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

        private static void PrepareEventYear(StringList index, GDMCustomEvent evt, GDMIndividualRecord iRec)
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

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
using GKCommon;
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKCore.Types;
using iTextSharp.text;
using iTextSharp.text.pdf;

namespace GKCore.Export
{
    using itFont = iTextSharp.text.Font;
    using itImage = iTextSharp.text.Image;

    /// <summary>
    /// 
    /// CodeTransformation: need
    /// </summary>
    public sealed class FamilyBookExporter : PDFExporter
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

        private itFont fTitleFont;
        private itFont fChapFont;
        private itFont fSubchapFont;
        private itFont fLinkFont;
        private itFont fTextFont;
        private itFont fBoldFont;
        private itFont fSymFont;

        private StringList mainIndex;
        private StringList byIndex, dyIndex, bpIndex, dpIndex;
        private StringList deathCauses, occuIndex, reliIndex, sourcesIndex;

        // temp options
        public bool SkipEmptyCatalogs = true;
        public bool CatalogNewPages = false;
        public bool IncludeEvents = true;
        public bool IncludeNotes = true;


        public FamilyBookExporter(IBaseWindow baseWin) : base(baseWin)
        {
            fMargins = new Margins(20);
            fAlbumPage = true;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                if (mainIndex != null) mainIndex.Dispose();
                if (byIndex != null) byIndex.Dispose();
                if (dyIndex != null) dyIndex.Dispose();
                if (bpIndex != null) bpIndex.Dispose();
                if (dpIndex != null) dpIndex.Dispose();
                if (deathCauses != null) deathCauses.Dispose();
                if (occuIndex != null) occuIndex.Dispose();
                if (reliIndex != null) reliIndex.Dispose();
                if (sourcesIndex != null) sourcesIndex.Dispose();

                if (fDocument != null) fDocument.Dispose();
            }
            base.Dispose(disposing);
        }

        /*private void AddParagraph(Chunk chunk, int alignment = Element.ALIGN_LEFT)
        {
            fDocument.Add(new Paragraph(chunk) { Alignment = alignment });
        }*/

        protected override void InternalGenerate()
        {
            try
            {
                PrepareData();

                fDocument.AddTitle("FamilyBook");
                fDocument.AddSubject("FamilyBook");
                fDocument.AddAuthor("");
                fDocument.AddCreator(GKData.APP_TITLE);
                fDocument.Open();

                BaseFont baseFont = BaseFont.CreateFont(Environment.ExpandEnvironmentVariables(@"%systemroot%\fonts\Times.ttf"), "CP1251", BaseFont.EMBEDDED);
                fTitleFont = new itFont(baseFont, 30f, Font.BOLD);
                fChapFont = new itFont(baseFont, 16f, Font.BOLD, BaseColor.BLACK);
                fSubchapFont = new itFont(baseFont, 14f, Font.BOLD, BaseColor.BLACK);
                fLinkFont = new itFont(baseFont, 8f, Font.UNDERLINE, BaseColor.BLUE);
                fTextFont = new itFont(baseFont, 8f, Font.NORMAL, BaseColor.BLACK);
                fBoldFont = new itFont(baseFont, 8f, Font.BOLD, BaseColor.BLACK);
                fSymFont = new itFont(baseFont, 12f, Font.BOLD, BaseColor.BLACK);

                baseFont = BaseFont.CreateFont(Environment.ExpandEnvironmentVariables(@"%systemroot%\fonts\Calibri.ttf"), "CP1251", BaseFont.EMBEDDED);
                //Font page_font = new Font(base_font, 9f, Font.NORMAL);
                
                fPdfWriter.PageEvent = new PDFWriterEvents(baseFont, LangMan.LS(LSID.LSID_Page)+": ");

                float halfpage = (fDocument.Top - fDocument.Bottom - (fTitleFont.Size) * 4) / 2f;
                fDocument.Add(new Paragraph(Chunk.NEWLINE) { SpacingAfter = halfpage });
                fDocument.Add(new Paragraph(LangMan.LS(LSID.LSID_FamilyBook), fTitleFont) { Alignment = Element.ALIGN_CENTER });
                fDocument.NewPage();

                Chunk chapChunk = new Chunk(LangMan.LS(LSID.LSID_TableOfContents), fChapFont);
                fDocument.Add(new Paragraph(chapChunk));
                fDocument.Add(new Paragraph(Chunk.NEWLINE));

                chapChunk = new Chunk("1. "+LangMan.LS(LSID.LSID_PersonalRecords), fLinkFont);
                chapChunk.SetLocalGoto("IndividualRecords");
                fDocument.Add(new Paragraph(chapChunk));

                chapChunk = new Chunk("2. "+LangMan.LS(LSID.LSID_Indexes), fLinkFont);
                chapChunk.SetLocalGoto("Catalogs");
                fDocument.Add(new Paragraph(chapChunk));

                // debug
                /*Rectangle pgSize = fDocument.PageSize;
				fDocument.Add(new Paragraph(Chunk.NEWLINE));
				chap_chunk = new Chunk(pgSize.Height.ToString() + " / " + pgSize.Width.ToString(), fChapFont);
				fDocument.Add(new Paragraph(chap_chunk) { Alignment = 1 });*/
                
                int catNum = 0;
                for (BookCatalog cat = BookCatalog.Catalog_First; cat <= BookCatalog.Catalog_Last; cat++)
                {
                    CatalogProps catProps = BookCatalogs[(int)cat];
                    
                    if (!SkipEmptyCatalogs || catProps.Index.Count > 0) {
                        catNum++;
                        string title = "2." + catNum.ToString() + ". " + catProps.Title;
                        
                        chapChunk = new Chunk(title, fLinkFont);
                        chapChunk.SetLocalGoto(catProps.Sign);
                        fDocument.Add(new Paragraph(chapChunk) { IndentationLeft = 1f });
                    }
                }

                fDocument.NewPage();

                chapChunk = new Chunk(LangMan.LS(LSID.LSID_PersonalRecords), fChapFont);
                chapChunk.SetLocalDestination("IndividualRecords");
                fDocument.Add(new Paragraph(chapChunk) { Alignment = 1, SpacingAfter = 20f });
                fDocument.Add(new Paragraph(Chunk.NEWLINE));

                SimpleColumnText columnText = new SimpleColumnText(fDocument, fPdfWriter.DirectContent, 3, 10f);
                float pageWidth = fDocument.PageSize.Width - fDocument.LeftMargin - fDocument.RightMargin;
                float colWidth = (pageWidth - (10f * 2)) / 3;

                char sym = '!';
                int num = mainIndex.Count;
                for (int i = 0; i < num; i++)
                {
                    string text = mainIndex[i];
                    GEDCOMIndividualRecord iRec = mainIndex.GetObject(i) as GEDCOMIndividualRecord;

                    char isym = text[0];
                    if ((isym >= 'A' && isym <= 'Z') || (isym >= 'А' && isym <= 'Я')) {
                        if (sym != isym) {
                            Paragraph ps = new Paragraph(new Chunk(isym, fSymFont));
                            ps.Alignment = 1;
                            columnText.AddElement(ps);
                            columnText.AddElement(new Paragraph(Chunk.NEWLINE));
                            sym = isym;
                        }
                    }

                    ExposePerson(columnText, iRec, text, colWidth);

                    columnText.AddElement(new Paragraph(Chunk.NEWLINE));
                }

                fDocument.NewPage();

                chapChunk = new Chunk(LangMan.LS(LSID.LSID_Indexes), fChapFont);
                chapChunk.SetLocalDestination("Catalogs");
                fDocument.Add(new Paragraph(chapChunk) { Alignment = 1 });
                fDocument.Add(new Paragraph(Chunk.NEWLINE));

                //SimpleColumnText columnText;
                if (!CatalogNewPages) {
                    columnText = new SimpleColumnText(fDocument, fPdfWriter.DirectContent, 3, 10f);
                }

                for (BookCatalog cat = BookCatalog.Catalog_First; cat <= BookCatalog.Catalog_Last; cat++)
                {
                    CatalogProps catProps = BookCatalogs[(int)cat];
                    
                    if (!SkipEmptyCatalogs || catProps.Index.Count > 0) {
                        if (CatalogNewPages) {
                            fDocument.NewPage();
                            columnText = new SimpleColumnText(fDocument, fPdfWriter.DirectContent, 3, 10f);
                        }

                        ExposeCatalog(fDocument, columnText, catProps);
                    }
                }
            }
            catch (Exception ex)
            {
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
            while (iEnum.MoveNext(out rec))
            {
                GEDCOMIndividualRecord iRec = (GEDCOMIndividualRecord)rec;
                string text = GKUtils.GetNameString(iRec, true, false);
                string st;

                mainIndex.AddObject(text, iRec);

                int evNum = iRec.Events.Count;
                for (int k = 0; k < evNum; k++)
                {
                    GEDCOMCustomEvent evt = iRec.Events[k];
                    if (evt == null) continue;

                    int srcNum2 = evt.SourceCitations.Count;
                    for (int m = 0; m < srcNum2; m++)
                    {
                        GEDCOMSourceRecord src = evt.SourceCitations[m].Value as GEDCOMSourceRecord;
                        if (src == null) continue;

                        st = src.FiledByEntry;
                        if (string.IsNullOrEmpty(st)) st = src.Title.Text;
                        PrepareSpecIndex(sourcesIndex, st, iRec);
                    }

                    // The analysis places
//						st = ev.Detail.Place.StringValue;
//						if (!string.IsNullOrEmpty(st)) PrepareSpecIndex(places, st, iRec);

                    if (evt.Name == "BIRT") {
                        // Analysis on births
                        PrepareEventYear(byIndex, evt, iRec);
                        st = GKUtils.GetPlaceStr(evt, false);
                        if (!string.IsNullOrEmpty(st)) PrepareSpecIndex(bpIndex, st, iRec);
                    }
                    else if (evt.Name == "DEAT")
                    {
                        // Analysis by causes of death
                        PrepareEventYear(dyIndex, evt, iRec);
                        st = GKUtils.GetPlaceStr(evt, false);
                        if (!string.IsNullOrEmpty(st)) PrepareSpecIndex(dpIndex, st, iRec);

                        st = evt.Cause;
                        if (!string.IsNullOrEmpty(st)) PrepareSpecIndex(deathCauses, st, iRec);
                    }
                    else if (evt.Name == "OCCU")
                    {
                        // Analysis by occupation
                        st = evt.StringValue;
                        if (!string.IsNullOrEmpty(st)) PrepareSpecIndex(occuIndex, st, iRec);
                    }
                    else if (evt.Name == "RELI")
                    {
                        // Analysis by religion
                        st = evt.StringValue;
                        if (!string.IsNullOrEmpty(st)) PrepareSpecIndex(reliIndex, st, iRec);
                    }
                }

                int srcNum = iRec.SourceCitations.Count;
                for (int k = 0; k < srcNum; k++)
                {
                    GEDCOMSourceRecord src = iRec.SourceCitations[k].Value as GEDCOMSourceRecord;
                    if (src == null) continue;

                    st = src.FiledByEntry;
                    if (string.IsNullOrEmpty(st)) st = src.Title.Text;
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

        private void ExposePerson(ColumnText mct, GEDCOMIndividualRecord iRec, string iName, float colWidth)
        {
            Paragraph pg = new Paragraph();
            Chunk chunk = new Chunk(iName, fBoldFont);
            chunk.SetLocalDestination(iRec.XRef);
            pg.Add(chunk);
            chunk = new Chunk(GKUtils.GetPedigreeLifeStr(iRec, PedigreeFormat.Compact), fTextFont);
            pg.Add(chunk);
            pg.KeepTogether = true;
            mct.AddElement(pg);

            // FIXME
            IImage image = fBase.Context.GetPrimaryBitmap(iRec, 0, 0, false);
            if (image != null)
            {
                itImage img = TreeChartPDFRenderer.ConvertImage(image);

                float fitWidth = colWidth * 0.5f;
                img.ScaleToFit(fitWidth, fitWidth);

                // FIXME: the moving, if the page height is insufficient for the image height

                //img.Alignment = Image.TEXTWRAP;
                img.IndentationLeft = 5f;
                img.SpacingBefore = 5f;
                img.SpacingAfter = 5f;

                //Paragraph imgpar = new Paragraph(new Chunk(img, 0, 0, true));
                //imgpar.KeepTogether = true;
                
                mct.AddElement(img);
            }

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
                pg = new Paragraph();
                chunk = new Chunk(GKUtils.GetNameString(father, true, false), fLinkFont);
                chunk.SetLocalGoto(father.XRef);
                pg.Add(new Chunk(LangMan.LS(LSID.LSID_Father) + ": ", fTextFont)); pg.Add(chunk);
                mct.AddElement(pg);
            }

            if (mother != null) {
                pg = new Paragraph();
                chunk = new Chunk(GKUtils.GetNameString(mother, true, false), fLinkFont);
                chunk.SetLocalGoto(mother.XRef);
                pg.Add(new Chunk(LangMan.LS(LSID.LSID_Mother) + ": ", fTextFont)); pg.Add(chunk);
                mct.AddElement(pg);
            }

            if (IncludeEvents && iRec.Events.Count != 0)
            {
                int num = iRec.Events.Count;
                for (int i = 0; i < num; i++)
                {
                    GEDCOMCustomEvent evt = iRec.Events[i];
                    if (evt.Name == "BIRT" || evt.Name == "DEAT") continue;
                    
                    string evtName = GKUtils.GetEventName(evt);
                    string evtVal = evt.StringValue;
                    string evtDesc = GKUtils.GetEventDesc(evt, false);

                    string tmp = evtName + ": " + evtVal;
                    if (evtVal != "") tmp += ", ";
                    tmp += evtDesc;

                    mct.AddElement(new Paragraph(new Chunk(tmp, fTextFont)));
                }
            }

            if (IncludeNotes && iRec.Notes.Count != 0)
            {
                int num = iRec.Notes.Count;
                for (int i = 0; i < num; i++)
                {
                    GEDCOMNotes note = iRec.Notes[i];
                    mct.AddElement(new Paragraph(GKUtils.MergeStrings(note.Notes), fTextFont));
                }
            }
        }

        private void ExposeCatalog(Document document, SimpleColumnText columnText, CatalogProps catProps)
        {
            StringList index = catProps.Index;
            if (index == null) return;

            Chunk chunk = new Chunk(catProps.Title, fSubchapFont);
            chunk.SetLocalDestination(catProps.Sign);
            
            if (CatalogNewPages) {
                document.Add(new Paragraph(chunk));
                document.Add(new Paragraph(Chunk.NEWLINE));
            } else {
                columnText.AddElement(new Paragraph(chunk) { Alignment = 1 });
                columnText.AddElement(new Paragraph(Chunk.NEWLINE));
            }

            index.Sort();
            int num = index.Count;
            for (int i = 0; i < num; i++)
            {
                Paragraph ps = new Paragraph(new Chunk(index[i], fSymFont));
                ps.SpacingBefore = 0f;
                ps.SpacingAfter = 20f;
                //ps.Alignment = 1;
                ps.Add(Chunk.NEWLINE);
                columnText.AddElement(ps);

                StringList persons = (StringList)index.GetObject(i);

                persons.Sort();
                int num2 = persons.Count;
                for (int k = 0; k < num2; k++)
                {
                    GEDCOMIndividualRecord iRec = (GEDCOMIndividualRecord)persons.GetObject(k);

                    chunk = new Chunk(persons[k], fTextFont);
                    chunk.SetLocalGoto(iRec.XRef);
                    Paragraph p = new Paragraph(chunk);
                    columnText.AddElement(p);
                }

                columnText.AddElement(new Paragraph(Chunk.NEWLINE) { SpacingAfter = 10f });
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
            if (evt == null) return;

            int dtY = evt.GetChronologicalYear();
            if (dtY != 0) {
                PrepareSpecIndex(index, dtY.ToString(), iRec);
            }
        }
    }

    public sealed class PDFWriterEvents : IPdfPageEvent
    {
        private readonly BaseFont fFont;
        private readonly string fFooter;

        public PDFWriterEvents(BaseFont font, string footer)
        {
            fFont = font;
            fFooter = footer;
        }

        void IPdfPageEvent.OnOpenDocument(PdfWriter writer, Document document) { }
        void IPdfPageEvent.OnCloseDocument(PdfWriter writer, Document document) { }
        void IPdfPageEvent.OnStartPage(PdfWriter writer, Document document) { }

        void IPdfPageEvent.OnEndPage(PdfWriter writer, Document document)
        {
            if (writer.PageNumber == 1) return;

            PdfContentByte cb = writer.DirectContent;
            Rectangle pageSize = document.PageSize;
            string text = fFooter + writer.PageNumber;

            cb.SaveState();
            cb.BeginText();

            cb.SetFontAndSize(fFont, 9);
            cb.ShowTextAligned(PdfContentByte.ALIGN_RIGHT, text,
                               pageSize.GetRight(document.RightMargin), pageSize.GetBottom(document.BottomMargin), 0);

            cb.EndText();
            cb.RestoreState();
        }

        void IPdfPageEvent.OnParagraph(PdfWriter writer, Document document, float paragraphPosition) { }
        void IPdfPageEvent.OnParagraphEnd(PdfWriter writer, Document document, float paragraphPosition) { }
        void IPdfPageEvent.OnChapter(PdfWriter writer, Document document, float paragraphPosition, Paragraph title) { }
        void IPdfPageEvent.OnChapterEnd(PdfWriter writer, Document document, float paragraphPosition) { }
        void IPdfPageEvent.OnSection(PdfWriter writer, Document document, float paragraphPosition, int depth, Paragraph title) { }
        void IPdfPageEvent.OnSectionEnd(PdfWriter writer, Document document, float paragraphPosition) { }
        void IPdfPageEvent.OnGenericTag(PdfWriter writer, Document document, Rectangle rect, string text) { }
    }

    public class SimpleColumnText : ColumnText
    {
        private readonly Document fDocument;
        private readonly List<Rectangle> fColumns;
        private int fCurrentColumn;

        public SimpleColumnText(Document document, PdfContentByte content, int columnCount, float columnSpacing) : base(content)
        {
            fDocument = document;
            fColumns = new List<Rectangle>();
            fCurrentColumn = 0;
            CalculateColumnBoundries(columnCount, columnSpacing);
        }

        private void CalculateColumnBoundries(int columnCount, float columnSpacing)
        {
            float columnHeight = (fDocument.PageSize.Height - fDocument.TopMargin - fDocument.BottomMargin);
            float columnWidth = ((fDocument.PageSize.Width - fDocument.LeftMargin - fDocument.RightMargin) - (columnSpacing * (columnCount - 1))) / columnCount;

            for (int x = 0; x < columnCount; x++)
            {
                float llx = ((columnWidth + columnSpacing) * x) + fDocument.LeftMargin;
                float lly = fDocument.BottomMargin;
                float urx = llx + columnWidth;
                float ury = columnHeight;

                Rectangle newRectangle = new Rectangle(llx, lly, urx, ury);
                fColumns.Add(newRectangle);
            }
        }

        public override void AddElement(IElement element)
        {
            base.AddElement(element);

            int status = 0;
            if (fCurrentColumn == 0) {
                status = NO_MORE_COLUMN;
            }

            do {
                if (status == NO_MORE_COLUMN) {
                    if (fCurrentColumn == fColumns.Count) {
                        fDocument.NewPage();
                        fCurrentColumn = 0;
                    }
                    SetSimpleColumn(fColumns[fCurrentColumn]);
                    fCurrentColumn += 1;
                }

                status = Go();
            } while (HasMoreText(status));
        }
    }
}

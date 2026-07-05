/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.Linq;
using BSLib;
using GDModel;
using GKCore.Design;
using GKCore.Design.Graphics;
using GKCore.Export.Formats;
using GKCore.Locales;

namespace GKCore.Export
{
    public sealed class PhotoAlbumExporter : ReportExporter
    {
        private IFont fTitleFont;
        private IFont fChapFont;
        private IFont fSubchapFont;
        private IFont fLinkFont;
        private IFont fTextFont;
        private IFont fBoldFont;
        private IFont fSymFont;
        private bool fHasContent;
        private Dictionary<GDMMediaType, StringList<GDMMultimediaRecord>> fTypeIndex;
        public bool CatalogNewPages = false;


        public PhotoAlbumExporter(IBaseWindow baseWin)
            : base(baseWin, true)
        {
            fTitle = LangMan.LS(LSID.PhotoAlbum);
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

                fWriter.SetExternalStyles("fb_styles/style.css");
                fWriter.EnablePageNumbers();

                var pageSize = fWriter.GetPageSize();
                float halfpage = (pageSize.GetHeight() - (fTitleFont.Size * 4)) / 2f;
                fWriter.NewLine(0.0f, halfpage);
                fWriter.AddParagraph(fTitle, fTitleFont, TextAlignment.taCenter);

                if (fHasContent) {
                    // table of contents
                    fWriter.NewPage();
                    fWriter.AddParagraph(LangMan.LS(LSID.TableOfContents), fChapFont);
                    fWriter.NewLine();

                    var mediaTypes = fTypeIndex.Keys.ToList();
                    int catNum = 0;
                    foreach (var mType in mediaTypes) {
                        catNum++;
                        var mtTitle = LangMan.LS(GKData.MediaTypes[(int)mType]);
                        string title = catNum.ToString() + ". " + mtTitle;

                        fWriter.BeginParagraph(TextAlignment.taLeft, 0.0f, 0.0f, 1f);
                        fWriter.AddParagraphChunkLink(title, fLinkFont, mType.ToString());
                        fWriter.EndParagraph();
                    }

                    // contents
                    foreach (var mType in mediaTypes) {
                        var mtTitle = LangMan.LS(GKData.MediaTypes[(int)mType]);
                        fWriter.NewPage();
                        fWriter.BeginParagraph(TextAlignment.taCenter, 0, 20f);
                        fWriter.AddParagraphChunkAnchor(mtTitle, fChapFont, mType.ToString());
                        fWriter.EndParagraph();
                        fWriter.NewLine();

                        fWriter.BeginMulticolumns(3, 10f);
                        var mediaRecords = fTypeIndex[mType];
                        mediaRecords.Sort();
                        for (int i = 0, num = mediaRecords.Count; i < num; i++) {
                            string text = mediaRecords[i];
                            var mmRec = mediaRecords.GetObject(i);
                            ExposeMedia(mmRec, text);
                            fWriter.NewLine();
                        }
                        fWriter.EndMulticolumns();
                    }

                    // indexes
                    fWriter.NewPage();
                    fWriter.BeginParagraph(TextAlignment.taCenter, 0, 20f);
                    fWriter.AddParagraphChunkAnchor(LangMan.LS(LSID.Indexes), fChapFont, "Catalogs");
                    fWriter.EndParagraph();
                    fWriter.NewLine();
                    fWriter.BeginMulticolumns(3, 10f);
                    foreach (var mType in mediaTypes) {
                        ExposeCatalog(mType, fTypeIndex[mType]);
                    }
                    fWriter.EndMulticolumns();
                }
            } catch (Exception ex) {
                Logger.WriteError("PhotoAlbumExporter.InternalGenerate()", ex);
                throw;
            }
        }

        private void PrepareData()
        {
            fHasContent = false;
            fTypeIndex = new Dictionary<GDMMediaType, StringList<GDMMultimediaRecord>>();

            var mmEnum = fTree.GetEnumerator<GDMMultimediaRecord>();
            GDMMultimediaRecord mmRec;
            while (mmEnum.MoveNext(out mmRec)) {
                var fileRef = (mmRec.FileReferences.Count > 0) ? mmRec.FileReferences[0] : null;
                if (fileRef == null || !GKUtils.IsPictureFormat(fileRef)) continue;

                var mediaType = fileRef.MediaType;
                string text = GKUtils.GetRecordName(fTree, mmRec, false);

                StringList<GDMMultimediaRecord> mtList;
                if (!fTypeIndex.TryGetValue(mediaType, out mtList)) {
                    mtList = new StringList<GDMMultimediaRecord>();
                    fTypeIndex[mediaType] = mtList;
                }
                mtList.AddObject(text, mmRec);
                fHasContent |= true;
            }
        }

        private void ExposeMedia(GDMMultimediaRecord mmRec, string title)
        {
            fWriter.BeginParagraph(TextAlignment.taLeft, 0, 0, 0, true);
            fWriter.AddParagraphChunkAnchor(title, fBoldFont, mmRec.XRef);
            fWriter.EndParagraph();
            fWriter.NewLine(0.0f, 0.0f);

            IImage image = fBase.Context.LoadMediaImage(mmRec, 0, 0, 0, ExtRect.Empty, true, false);
            fWriter.AddImage(image, TextAlignment.taCenter);

            if (mmRec.HasNotes) {
                for (int i = 0, num = mmRec.Notes.Count; i < num; i++) {
                    GDMLines noteLines = fTree.GetNoteLines(mmRec.Notes[i]);
                    fWriter.AddParagraph(GKUtils.MergeStrings(noteLines), fTextFont);
                }
            }

            if (mmRec.HasSourceCitations) {
                for (int k = 0, srcNum = mmRec.SourceCitations.Count; k < srcNum; k++) {
                    var sourceRec = fTree.GetPtrValue<GDMSourceRecord>(mmRec.SourceCitations[k]);
                    if (sourceRec == null) continue;

                    string st = sourceRec.ShortTitle;
                    if (string.IsNullOrEmpty(st))
                        st = sourceRec.Title.Lines.Text;

                    fWriter.AddParagraph(st, fTextFont);
                }
            }
        }

        private const float MediaTitleSpacingAfter = 2.0f;
        private const float ChapterPartSpacingAfter = 6.0f;

        private void ExposeCatalog(GDMMediaType mType, StringList<GDMMultimediaRecord> typeIndex)
        {
            var mtTitle = LangMan.LS(GKData.MediaTypes[(int)mType]);

            fWriter.BeginParagraph(TextAlignment.taLeft, 0, ChapterPartSpacingAfter, 0);
            fWriter.AddParagraphChunk(mtTitle, fSymFont);
            fWriter.EndParagraph();
            fWriter.NewLine();

            for (int k = 0, num2 = typeIndex.Count; k < num2; k++) {
                var mmRec = typeIndex.GetObject(k);
                fWriter.BeginParagraph(TextAlignment.taLeft, 0, 0, 0);
                fWriter.AddParagraphChunkLink(typeIndex[k], fTextFont, mmRec.XRef);
                fWriter.EndParagraph();
            }

            fWriter.NewLine(0, ChapterPartSpacingAfter);
        }
    }
}

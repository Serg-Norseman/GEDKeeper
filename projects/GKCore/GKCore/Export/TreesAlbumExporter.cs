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
using GKCommon;
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKCore.Options;
using GKCore.Types;
using GKUI.Charts;
using iTextSharp.text;
using iTextSharp.text.pdf;

namespace GKCore.Export
{
    using itFont = iTextSharp.text.Font;

    public sealed class TreesAlbumExporter : PDFExporter
    {
        private readonly ShieldState fShieldState;
        private readonly StringList fPatList;

        private itFont fTitleFont;
        private itFont fChapFont;
        private itFont fSubchapFont;
        private itFont fLinkFont;
        private itFont fTextFont;
        private itFont fBoldFont;
        private itFont fSymFont;

        public TreesAlbumExporter(IBaseWindow baseWin) : base(baseWin)
        {
            fShieldState = fBase.ShieldState;
            fPatList = new StringList();
        }

        protected override void InternalGenerate()
        {
            try
            {
                {
                    string title = "Trees Album";//LangMan.LS(LSID.LSID_TreesAlbum)

                    fDocument.AddTitle(title);
                    fDocument.AddSubject("");
                    fDocument.AddAuthor("");
                    fDocument.AddCreator(GKData.APP_TITLE);
                    fDocument.Open();

                    BaseFont baseFont = BaseFont.CreateFont(Environment.ExpandEnvironmentVariables(@"%systemroot%\fonts\Times.ttf"), BaseFont.IDENTITY_H, BaseFont.EMBEDDED);
                    fTitleFont = new itFont(baseFont, 30f, itFont.BOLD);
                    fChapFont = new itFont(baseFont, 16f, itFont.BOLD, BaseColor.BLACK);
                    fSubchapFont = new itFont(baseFont, 14f, itFont.BOLD, BaseColor.BLACK);
                    fLinkFont = new itFont(baseFont, 8f, itFont.UNDERLINE, BaseColor.BLUE);
                    fTextFont = new itFont(baseFont, 8f, itFont.NORMAL, BaseColor.BLACK);
                    fBoldFont = new itFont(baseFont, 8f, itFont.BOLD, BaseColor.BLACK);
                    fSymFont = new itFont(baseFont, 12f, itFont.BOLD, BaseColor.BLACK);

                    float halfpage = (fDocument.Top - fDocument.Bottom - (fTitleFont.Size) * 4) / 2f;
                    fDocument.Add(new Paragraph(Chunk.NEWLINE) { SpacingAfter = halfpage });
                    fDocument.Add(new Paragraph(title, fTitleFont) { Alignment = Element.ALIGN_CENTER });
                    fDocument.NewPage();

                    PreparePatriarchs();

                    Rectangle itPS = fDocument.PageSize;
                    float pageHeight = itPS.Height;
                    float pageWidth = itPS.Width;

                    var renderer = new TreeChartPDFRenderer(pageWidth, pageHeight);
                    renderer.SetTarget(fPdfWriter.DirectContent);

                    // TODO: replace by local options in TreeChartBox
                    bool prevKinship = GlobalOptions.Instance.ChartOptions.Kinship;
                    GlobalOptions.Instance.ChartOptions.Kinship = false;

                    var treeBox = AppHub.Container.Resolve<ITreeChartBox>();
                    treeBox.SetRenderer(renderer);
                    treeBox.Base = fBase;
                    treeBox.Options = GlobalOptions.Instance.ChartOptions;
                    treeBox.DepthLimit = 3;
                    treeBox.ShieldState = fShieldState;
                    treeBox.Height = (int)pageHeight;
                    treeBox.Width = (int)pageWidth;

                    int num = fPatList.Count;
                    for (int i = 0; i < num; i++) {
                        string iName = fPatList[i];
                        GEDCOMIndividualRecord iRec = fPatList.GetObject(i) as GEDCOMIndividualRecord;

                        treeBox.SetScale(1.0f);
                        treeBox.GenChart(iRec, TreeChartKind.ckDescendants, false);

                        float scaleFactor = SysUtils.ZoomToFit(treeBox.ImageSize.Width,
                                                               treeBox.ImageSize.Height,
                                                               pageWidth, pageHeight);
                        scaleFactor = (scaleFactor > 1.0f) ? 1.0f : scaleFactor;

                        treeBox.SetScale(scaleFactor);
                        treeBox.RenderStatic(BackgroundMode.bmNone, true);

                        fDocument.NewPage();
                    }

                    GlobalOptions.Instance.ChartOptions.Kinship = prevKinship;
                }
            }
            catch (Exception)
            {
                throw;
            }
        }

        private static int PatriarchsCompare(object item1, object item2)
        {
            return ((PatriarchObj)item2).DescendantsCount - ((PatriarchObj)item1).DescendantsCount;
        }

        private void PreparePatriarchs()
        {
            using (ExtList<PatriarchObj> lst = PatriarchsMan.GetPatriarchsList(fBase.Context, 2, false))
            {
                lst.QuickSort(PatriarchsCompare);

                int num = lst.Count;
                for (int i = 0; i < num; i++) {
                    PatriarchObj pObj = lst[i];
                    fPatList.AddObject(GKUtils.GetNameString(pObj.IRec, true, false), pObj.IRec);
                }
            }
        }
    }
}

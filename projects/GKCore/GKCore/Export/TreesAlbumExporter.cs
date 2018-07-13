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
using GKCore.Charts;
using GKCore.Interfaces;
using GKCore.Options;
using GKCore.Types;

namespace GKCore.Export
{
    public sealed class TreesAlbumExporter : ReportExporter
    {
        private readonly StringList fPatList;

        private IFont fTitleFont;
        private IFont fChapFont;
        private IFont fSubchapFont;
        private IFont fLinkFont;
        private IFont fTextFont;
        private IFont fBoldFont;
        private IFont fSymFont;

        public TreesAlbumExporter(IBaseWindow baseWin) : base(baseWin, true)
        {
            fPatList = new StringList();
            fTitle = "Trees Album"; //LangMan.LS(LSID.LSID_TreesAlbum);
        }

        protected override void InternalGenerate()
        {
            try {
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
                float halfpage = (pageSize.Top - pageSize.Bottom - (fTitleFont.Size)) / 2f;
                fWriter.NewLine(0.0f, halfpage);
                fWriter.AddParagraph(fTitle, fTitleFont, TextAlignment.taCenter);
                fWriter.NewPage();

                var renderer = fWriter.GetPageRenderer();
                if (renderer == null) {
                    // TODO: warning
                    return;
                }

                PreparePatriarchs();

                var itPS = fWriter.GetPageSize();
                float pageHeight = itPS.GetHeight();
                float pageWidth = itPS.GetWidth();

                // TODO: replace by local options in TreeChartBox
                bool prevKinship = GlobalOptions.Instance.ChartOptions.Kinship;
                GlobalOptions.Instance.ChartOptions.Kinship = false;

                var treeBox = AppHost.Container.Resolve<ITreeChartBox>();
                treeBox.SetRenderer(renderer);
                treeBox.Base = fBase;
                treeBox.Options = GlobalOptions.Instance.ChartOptions;
                treeBox.DepthLimit = 3;
                treeBox.Height = (int)pageHeight;
                treeBox.Width = (int)pageWidth;

                int num = fPatList.Count;
                for (int i = 0; i < num; i++) {
                    string iName = fPatList[i];
                    GEDCOMIndividualRecord iRec = fPatList.GetObject(i) as GEDCOMIndividualRecord;

                    treeBox.SetScale(1.0f);
                    treeBox.GenChart(iRec, TreeChartKind.ckDescendants, false);

                    ExtSize imageSize = treeBox.GetImageSize();
                    float scaleFactor = GfxHelper.ZoomToFit(imageSize.Width,
                                                imageSize.Height,
                                                pageWidth, pageHeight);
                    scaleFactor = (scaleFactor > 1.0f) ? 1.0f : scaleFactor;

                    treeBox.SetScale(scaleFactor);
                    treeBox.RenderStatic(BackgroundMode.bmNone, true);

                    fWriter.NewPage();
                }

                GlobalOptions.Instance.ChartOptions.Kinship = prevKinship;
            } catch (Exception ex) {
                Logger.LogWrite("TreesAlbumExporter.InternalGenerate(): " + ex.Message);
                throw;
            }
        }

        private static int PatriarchsCompare(PatriarchObj item1, PatriarchObj item2)
        {
            return item2.DescendantsCount - item1.DescendantsCount;
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

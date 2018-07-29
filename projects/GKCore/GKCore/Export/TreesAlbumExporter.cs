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
using GKCommon.GEDCOM;
using GKCore.Charts;
using GKCore.Interfaces;
using GKCore.Options;
using GKCore.Tools;
using GKCore.Types;

namespace GKCore.Export
{
    public sealed class TreesAlbumExporter : ReportExporter
    {
        private readonly List<IndiObj> fIndiQueue;
        private ExtList<PatriarchObj> fPatList;
        private ChartRenderer fRenderer;
        private IFont fTitleFont;

        //private IFont fChapFont;
        //private IFont fSubchapFont;
        private IFont fLinkFont;
        private IFont fTextFont;
        //private IFont fBoldFont;
        //private IFont fSymFont;

        public TreesAlbumExporter(IBaseWindow baseWin) : base(baseWin, true)
        {
            fIndiQueue = new List<IndiObj>();
            fTitle = LangMan.LS(LSID.LSID_TreesAlbum);
        }

        protected override void InternalGenerate()
        {
            try {
                IColor clrBlack = AppHost.GfxProvider.CreateColor(0x000000);
                IColor clrBlue = AppHost.GfxProvider.CreateColor(0x0000FF);

                fTitleFont = fWriter.CreateFont("", 30f, true, false, clrBlack);
                //fChapFont = fWriter.CreateFont("", 16f, true, false, clrBlack);
                //fSubchapFont = fWriter.CreateFont("", 14f, true, false, clrBlack);
                fLinkFont = fWriter.CreateFont("", 8f, false, true, clrBlue);
                fTextFont = fWriter.CreateFont("", 8f, false, false, clrBlack);
                //fBoldFont = fWriter.CreateFont("", 8f, true, false, clrBlack);
                //fSymFont = fWriter.CreateFont("", 12f, true, false, clrBlack);

                fWriter.EnablePageNumbers();

                fRenderer = fWriter.GetPageRenderer();
                if (fRenderer == null) {
                    // TODO: warning
                    return;
                }

                var pageSize = fWriter.GetPageSize();
                float pageHeight = pageSize.GetHeight();
                float pageWidth = pageSize.GetWidth();
                float halfpage = (pageHeight - (fTitleFont.Size * 4)) / 2f;
                fWriter.NewLine(0.0f, halfpage);
                fWriter.AddParagraph(fTitle, fTitleFont, TextAlignment.taCenter);

                var chartOptions = new TreeChartOptions();
                chartOptions.Assign(GlobalOptions.Instance.ChartOptions);
                chartOptions.DefaultPortraits = false;
                chartOptions.HideUnknownSpouses = true;
                chartOptions.InvertedTree = false;
                chartOptions.Kinship = false;
                chartOptions.ShowPlaces = false;

                var treeBox = AppHost.Container.Resolve<ITreeChartBox>();
                treeBox.SetRenderer(fRenderer);
                treeBox.Base = fBase;
                treeBox.Options = chartOptions;
                treeBox.Height = (int)pageHeight;
                treeBox.Width = (int)pageWidth;

                fPatList = PatriarchsMan.GetPatriarchsList(fBase.Context, 2, false);
                fPatList.QuickSort(PatriarchsCompare);

                int num = fPatList.Count;
                for (int i = 0; i < num; i++) {
                    var patriarch = fPatList[i].IRec;

                    fIndiQueue.Clear();
                    fIndiQueue.Add(new IndiObj(fPatList[i].IRec, TreeChartKind.ckDescendants));

                    for (int q = 0; q < fIndiQueue.Count; q++) {
                        fWriter.NewPage();
                        TryRenderTreeSlice(treeBox, pageSize, q, patriarch);
                    }
                }
            } catch (Exception ex) {
                Logger.LogWrite("TreesAlbumExporter.InternalGenerate(): " + ex.Message);
                throw;
            }
        }

        private sealed class IndiObj
        {
            public GEDCOMIndividualRecord IRec;
            public TreeChartKind TreeKind;

            public IndiObj(GEDCOMIndividualRecord iRec, TreeChartKind treeKind)
            {
                IRec = iRec;
                TreeKind = treeKind;
            }
        }

        private enum RenderStage
        {
            Normal,
            Grow,
            Shrink,
            Break
        }

        private void TryRenderTreeSlice(ITreeChartBox treeBox, ExtRectF ps, int index,
                                        GEDCOMIndividualRecord currentPatriarch)
        {
            IndiObj indi = fIndiQueue[index];

            int depthLimit = 3;
            float scaleFactor = 1.0f;
            int tries = 0;
            RenderStage stage = RenderStage.Normal;

            while (true) {
                treeBox.Model.DepthLimit = depthLimit;
                treeBox.Model.Scale = scaleFactor;
                treeBox.GenChart(indi.IRec, indi.TreeKind, false);
                tries += 1;

                ExtSize imageSize = treeBox.GetImageSize();
                var sf = GfxHelper.ZoomToFit(imageSize.Width, imageSize.Height, ps.GetWidth(), ps.GetHeight());

                if (sf < 1.0f) {
                    // need to reduce image's size
                    switch (stage) {
                        case RenderStage.Normal:
                            depthLimit -= 1;
                            stage = RenderStage.Shrink;
                            break;

                        case RenderStage.Grow:
                            depthLimit -= 1;
                            stage = RenderStage.Break;
                            break;

                        case RenderStage.Shrink:
                            scaleFactor = sf;
                            stage = RenderStage.Break;
                            break;
                    }
                } else if (sf > 1.0f) {
                    // need to increase image's size
                    switch (stage) {
                        case RenderStage.Normal:
                        case RenderStage.Grow:
                            depthLimit += 1;
                            stage = RenderStage.Grow;
                            break;

                        case RenderStage.Shrink:
                            scaleFactor = sf;
                            stage = RenderStage.Break;
                            break;
                    }
                }

                if (stage == RenderStage.Break || tries > 10) {
                    break;
                }
            }

            scaleFactor = Math.Min(1.0f, scaleFactor);
            treeBox.Model.DepthLimit = depthLimit;
            treeBox.Model.Scale = scaleFactor;
            treeBox.GenChart(indi.IRec, indi.TreeKind, false);
            treeBox.RenderImage(RenderTarget.Printer, true);

            for (int i = 0; i < treeBox.Model.Persons.Count; i++) {
                TreeChartPerson person = treeBox.Model.Persons[i];
                GEDCOMIndividualRecord indiRec = person.Rec;
                if (indiRec == null) continue;

                var offset = treeBox.Model.GetOffsets();
                int ix = offset.X + person.Rect.Left;
                int iy = offset.Y + person.Rect.Top - (int)fTextFont.Size;
                fRenderer.DrawAnchor(indiRec.XRef, indiRec.XRef, fTextFont, null, ix, iy);

                if (!person.CanExpand) continue;

                ix = offset.X + person.Rect.Left;
                iy = offset.Y + person.Rect.Bottom;
                fRenderer.DrawHyperlink(indiRec.XRef, indiRec.XRef, fLinkFont, null, ix, iy);

                TreeChartKind treeKind;
                if (person.HasFlag(PersonFlag.pfHasInvAnc)
                    && !IsDescendantOfOtherPatriarchs(indiRec, fPatList, currentPatriarch)) {
                    treeKind = TreeChartKind.ckAncestors;
                }
                /*else
                if (person.HasFlag(PersonFlag.pfHasInvDesc)
                    && TreeTools.PL_SearchAnc(indiRec, currentPatriarch, true)) {
                    treeKind = TreeChartKind.ckDescendants;
                }*/
                else continue;

                fIndiQueue.Add(new IndiObj(indiRec, treeKind));
            }
        }

        private bool IsDescendantOfOtherPatriarchs(GEDCOMIndividualRecord iRec, ExtList<PatriarchObj> patList,
                                                   GEDCOMIndividualRecord currentPatriarch)
        {
            bool result = false;

            int num = fPatList.Count;
            for (int i = 0; i < num; i++) {
                var patriarch = fPatList[i].IRec;

                if ((patriarch != currentPatriarch) && TreeTools.PL_SearchAnc(iRec, patriarch, true)) {
                    result = true;
                    break;
                }
            }

            return result;
        }

        private static int PatriarchsCompare(PatriarchObj item1, PatriarchObj item2)
        {
            return item2.DescendantsCount - item1.DescendantsCount;
        }
    }
}

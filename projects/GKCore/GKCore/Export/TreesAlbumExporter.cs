/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2020 by Sergey V. Zhdanovskih.
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
using BSLib.Design.Graphics;
using GDModel;
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
        private readonly StringList fProcessed;

        private ExtList<PatriarchObj> fPatList;
        private ExtRectF fPageSize;
        private ChartRenderer fRenderer;
        private IFont fLinkFont;
        private IFont fTextFont;

        public TreesAlbumExporter(IBaseWindow baseWin) : base(baseWin, true)
        {
            fIndiQueue = new List<IndiObj>();
            fProcessed = new StringList();
            fTitle = LangMan.LS(LSID.LSID_TreesAlbum);
        }

        protected override void InternalGenerate()
        {
            try {
                #if !NETSTANDARD
                IColor clrBlack = AppHost.GfxProvider.CreateColor(0x000000);
                IColor clrBlue = AppHost.GfxProvider.CreateColor(0x0000FF);

                fLinkFont = fWriter.CreateFont("", 8f, false, true, clrBlue);
                fTextFont = fWriter.CreateFont("", 8f, false, false, clrBlack);

                fWriter.EnablePageNumbers();

                fRenderer = fWriter.GetPageRenderer();
                if (fRenderer == null) {
                    // TODO: warning
                    return;
                }

                IFont titleFont = fWriter.CreateFont("", 30f, true, false, clrBlack);
                fPageSize = fWriter.GetPageSize();
                float pageHeight = fPageSize.GetHeight();
                float pageWidth = fPageSize.GetWidth();
                float halfpage = (pageHeight - (((PDFWriter.FontHandler)titleFont).GetTextHeight())) / 2f;
                fWriter.NewLine(0.0f, halfpage);
                fWriter.AddParagraph(fTitle, titleFont, TextAlignment.taCenter);

                var chartOptions = new TreeChartOptions();
                chartOptions.Assign(GlobalOptions.Instance.TreeChartOptions);
                chartOptions.DefaultPortraits = false;
                chartOptions.HideUnknownSpouses = true;
                chartOptions.InvertedTree = false;
                chartOptions.Kinship = false;
                chartOptions.ShowPlaces = false;

                var treeBox = AppHost.Container.Resolve<ITreeChart>();
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
                    fIndiQueue.Add(new IndiObj(patriarch, TreeChartKind.ckDescendants));

                    for (int q = 0; q < fIndiQueue.Count; q++) {
                        fWriter.NewPage();
                        TryRenderTreeSlice(treeBox, q, patriarch);
                    }
                }
                #endif
            } catch (Exception ex) {
                Logger.WriteError("TreesAlbumExporter.InternalGenerate()", ex);
                throw;
            }
        }

        private sealed class IndiObj
        {
            public GDMIndividualRecord IRec;
            public TreeChartKind TreeKind;

            public IndiObj(GDMIndividualRecord iRec, TreeChartKind treeKind)
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

        private void TryRenderTreeSlice(ITreeChart treeBox, int index, GDMIndividualRecord currentPatriarch)
        {
            IndiObj indi = fIndiQueue[index];
            fProcessed.Add(indi.IRec.XRef);

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
                var sf = GfxHelper.ZoomToFit(imageSize.Width, imageSize.Height, fPageSize.GetWidth(), fPageSize.GetHeight());

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

            var indiNums = new GKVarCache<GDMIndividualRecord, int>();

            for (int i = 0; i < treeBox.Model.Persons.Count; i++) {
                TreeChartPerson person = treeBox.Model.Persons[i];
                GDMIndividualRecord indiRec = person.Rec;
                if (indiRec == null) continue;

                int iNum = indiNums[indiRec];

                var offset = treeBox.Model.GetOffsets();
                int ix = offset.X + person.Rect.Left;
                int iy = offset.Y + person.Rect.Top - (int)fTextFont.Size;
                string iRef = indiRec.XRef + "#" + iNum;
                fRenderer.DrawAnchor(iRef, iRef, fTextFont, null, ix, iy);

                iNum += 1;
                indiNums[indiRec] = iNum;

                if (!person.CanExpand) continue;

                ix = offset.X + person.Rect.Left;
                iy = offset.Y + person.Rect.Bottom;
                iRef = indiRec.XRef + "#" + iNum;
                fRenderer.DrawHyperlink(iRef, iRef, fLinkFont, null, ix, iy);

                if (person.HasFlag(PersonFlag.pfAncWalk)) {

                    if (person.HasFlag(PersonFlag.pfHasInvAnc) && !IsPatriarchsDescendant(indiRec, currentPatriarch)) {
                        CheckQueue(indiRec, TreeChartKind.ckAncestors);
                    }

                } else if (person.HasFlag(PersonFlag.pfDescWalk)) {

                    if (person.HasFlag(PersonFlag.pfSpouse)) {
                        if (person.HasFlag(PersonFlag.pfHasInvAnc) && !IsPatriarchsDescendant(indiRec, currentPatriarch)) {
                            CheckQueue(indiRec, TreeChartKind.ckAncestors);
                        }
                    } else {
                        if (person.HasFlag(PersonFlag.pfHasInvDesc) && TreeTools.PL_SearchAnc(indiRec, currentPatriarch, true)) {
                            CheckQueue(indiRec, TreeChartKind.ckDescendants);
                        }
                    }

                }
            }
        }

        private void CheckQueue(GDMIndividualRecord iRec, TreeChartKind treeKind)
        {
            if (fProcessed.IndexOf(iRec.XRef) >= 0) return;

            fIndiQueue.Add(new IndiObj(iRec, treeKind));
        }

        private bool IsPatriarchsDescendant(GDMIndividualRecord iRec, GDMIndividualRecord currentPatriarch)
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

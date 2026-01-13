/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using BSLib.DataViz.TreeMap;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Locales;
using GKUI.Components;
using SkiaSharp;

namespace GKUI.Forms
{
    public sealed partial class TTFamilyGroupsDlg : CommonWindow<IFragmentSearchDlg, FragmentSearchController>, IFragmentSearchDlg
    {
        #region View Interface

        ITreeView IFragmentSearchDlg.GroupsTree
        {
            get { return GetControlHandler<ITreeView>(tvGroups); }
        }

        ILogChart IFragmentSearchDlg.LogChart
        {
            get { return GetControlHandler<ILogChart>(gkLogChart1); }
        }

        #endregion

        public TTFamilyGroupsDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            fController = new FragmentSearchController(this);
            fController.Init(baseWin);

            fDataMap.Model.CreatingItem += fController.DataMap_CreateGroupItem;
            fDataMap.MouseDoubleClick += DataMap_DoubleClick;
            fDataMap.PaintItem += DataMap_PaintItem;

            tvGroups.MouseDoubleClick += tvGroups_DoubleClick;
            gkLogChart1.OnHintRequest += HintRequestEventHandler;
        }

        /*
        <TreeView.ContextMenu>
            <ContextMenu Opening="contextMenu_Opening">
                <ButtonMenuItem x:Name="miDetails" Clicked="miDetails_Click" />
                <ButtonMenuItem x:Name="miGoToRecord" Clicked="miGoToRecord_Click" />
                <ButtonMenuItem x:Name="miCopyXRef" Clicked="miCopyXRef_Click" />
            </ContextMenu>
        </TreeView.ContextMenu>
        <comcom:TreeMapViewer.ContextMenu>
            <ContextMenu>
                <ButtonMenuItem x:Name="miDQRefresh" Clicked="miRefresh_Click" />
                <ButtonMenuItem x:Name="miDQResetFilter" Clicked="miResetFilter_Click" />
            </ContextMenu>
        </comcom:TreeMapViewer.ContextMenu>
         */

        private void Form_Closed(object sender, EventArgs e)
        {
            fController.SetExternalFilter(null);
        }

        public override void SetLocale()
        {
            fController.SetLocale();
        }

        private void btnAnalyseGroups_Click(object sender, EventArgs e)
        {
            fController.CheckGroups();
            UpdateTreeMap();
        }

        private void tvGroups_DoubleClick(object sender, EventArgs e)
        {
            //fController.SelectPerson();
        }

        private void HintRequestEventHandler(object sender, HintRequestEventArgs args)
        {
            if (args == null) return;

            args.Hint = string.Format(LangMan.LS(LSID.LogHint), args.FragmentNumber, args.Size);
        }

        private void miDetails_Click(object sender, EventArgs e)
        {
            fController.ShowDetails();
        }

        private void miGoToRecord_Click(object sender, EventArgs e)
        {
            fController.SelectPerson();
        }

        private void contextMenu_Opening(object sender, EventArgs e)
        {
            fController.OpeningContextMenu();
        }

        public void miCopyXRef_Click(object sender, EventArgs e)
        {
            fController.CopySelectedXRef();
        }

        #region Data Quality

        private void miResetFilter_Click(object sender, EventArgs e)
        {
            fController.SetExternalFilter(null);
        }

        private void miRefresh_Click(object sender, EventArgs e)
        {
            UpdateTreeMap();
        }

        private void UpdateTreeMap()
        {
            fController.UpdateTreeMap(fDataMap.Model);
            fDataMap.UpdateView();
        }

        private void DataMap_PaintItem(object sender, PaintItemEventArgs args)
        {
            var fDrawFont = new SKPaint(new SKFont(SKTypeface.FromFamilyName("Sans"), 9)) { Color = SKColors.Black };
            var fBorderPen = new SKPaint() { Color = SKColors.Black, Style = SKPaintStyle.Stroke };

            var gfx = args.Canvas;
            var item = args.Item;
            MapRect bounds = item.Bounds;
            if (bounds.W > 2f && bounds.H > 2f) {
                var simpleItem = (FragmentSearchController.GroupMapItem)item;

                var skRect = SKRect.Create(bounds.X, bounds.Y, bounds.W, bounds.H);

                var fillColor = new SKColor((uint)(simpleItem.Color & 0xffffff));
                var fillPaint = new SKPaint { Color = fillColor, Style = SKPaintStyle.Fill };
                gfx.DrawRect(skRect, fillPaint);

                gfx.DrawRect(skRect, fBorderPen);
                gfx.DrawText(item.Name, bounds.X, bounds.Y, fDrawFont);
            }
        }

        private void DataMap_DoubleClick(object sender, EventArgs e)
        {
            //Point mpt = new Point(e.Location);
            //fController.DoubleClickGroupItem(fDataMap.Model, mpt.X, mpt.Y);
        }

        #endregion
    }
}

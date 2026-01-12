/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

#pragma warning disable CS0618

using System;
using BSLib.DataViz.TreeMap;
using Eto.Drawing;
using Eto.Forms;
using Eto.Serialization.Xaml;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Locales;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class TTFamilyGroupsDlg : CommonWindow<IFragmentSearchDlg, FragmentSearchController>, IFragmentSearchDlg
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private Button btnClose;
        private TabPage pageFamilyGroups;
        private TreeView tvGroups;
        private GKUI.Components.LogChart gkLogChart1;
        private Button btnAnalyseGroups;
        private ButtonMenuItem miDetails;
        private ButtonMenuItem miGoToRecord;
        private ButtonMenuItem miCopyXRef;
        private TabPage pageDataQuality;
        private TreeMapViewer fDataMap;
        private ButtonMenuItem miDQRefresh;
        private ButtonMenuItem miDQResetFilter;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

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
            XamlReader.Load(this);

            fController = new FragmentSearchController(this);
            fController.Init(baseWin);

            fDataMap.Model.CreatingItem += fController.DataMap_CreateGroupItem;
            fDataMap.MouseDoubleClick += DataMap_DoubleClick;
            fDataMap.PaintItem += DataMap_PaintItem;

            gkLogChart1.OnHintRequest += HintRequestEventHandler;
        }

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

        private void tvGroups_DoubleClick(object sender, MouseEventArgs e)
        {
            fController.SelectPerson();
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
            var gfx = args.Graphics;
            var item = args.Item;
            MapRect bounds = item.Bounds;
            if (bounds.W > 2f && bounds.H > 2f) {
                var simpleItem = (FragmentSearchController.GroupMapItem)item;
                gfx.FillRectangle(new SolidBrush(Color.FromArgb(simpleItem.Color)), bounds.X, bounds.Y, bounds.W, bounds.H);
                gfx.DrawRectangle(fDataMap.BorderPen, bounds.X, bounds.Y, bounds.W, bounds.H);

                var rect = new RectangleF(bounds.X, bounds.Y, bounds.W, bounds.H);
                gfx.DrawText(fDataMap.DrawFont, new SolidBrush(Colors.Black), rect, simpleItem.Name);
            }
        }

        private void DataMap_DoubleClick(object sender, MouseEventArgs e)
        {
            Point mpt = new Point(e.Location);
            fController.DoubleClickGroupItem(fDataMap.Model, mpt.X, mpt.Y);
        }

        #endregion
    }
}

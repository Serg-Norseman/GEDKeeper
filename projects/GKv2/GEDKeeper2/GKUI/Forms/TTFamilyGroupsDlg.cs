/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Drawing;
using System.Windows.Forms;
using BSLib.DataViz.TreeMap;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Locales;

namespace GKUI.Forms
{
    public sealed partial class TTFamilyGroupsDlg : CommonWindow<IFragmentSearchDlg, FragmentSearchController>, IFragmentSearchDlg
    {
        private TreeMapViewer fDataMap;

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

            fDataMap = new TreeMapViewer();
            fDataMap.Model.CreatingItem += fController.DataMap_CreateGroupItem;
            fDataMap.Dock = DockStyle.Fill;
            fDataMap.MouseDoubleClick += DataMap_DoubleClick;
            fDataMap.PaintItem += DataMap_PaintItem;
            fDataMap.ContextMenuStrip = menuDQ;
            pageDataQuality.Controls.Add(fDataMap);

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

        private void tvGroups_DoubleClick(object sender, EventArgs e)
        {
            fController.SelectPerson();
        }

        private void HintRequestEventHandler(object sender, Components.HintRequestEventArgs args)
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

        private void contextMenu_Opening(object sender, System.ComponentModel.CancelEventArgs e)
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
                gfx.DrawString(simpleItem.Name, Font, fDataMap.HeaderBrush, rect);
            }
        }

        private void DataMap_DoubleClick(object sender, MouseEventArgs e)
        {
            fController.DoubleClickGroupItem(fDataMap.Model, e.X, e.Y);
        }

        #endregion
    }
}

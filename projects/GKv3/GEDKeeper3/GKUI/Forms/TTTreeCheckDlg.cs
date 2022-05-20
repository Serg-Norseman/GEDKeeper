/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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
using System.Text;
using Eto.Forms;
using Eto.Serialization.Xaml;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.MVP.Views;
using GKCore.Tools;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class TTTreeCheckDlg : CommonDialog, ITreeCheckDlg
    {
        #region Design components
#pragma warning disable CS0169

        private Button btnClose;
        private TabPage pageTreeCheck;
        private Button btnAnalyseBase;
        private Button btnBaseRepair;
        private Panel panProblemsContainer;
        private ContextMenu contextMenu;
        private ButtonMenuItem miDetails;
        private ButtonMenuItem miGoToRecord;
        private ButtonMenuItem miCopyXRef;

#pragma warning restore CS0169
        #endregion

        private readonly TreeCheckController fController;

        private GKListView ListChecks;

        #region View Interface

        IListViewEx ITreeCheckDlg.ChecksList
        {
            get { return ListChecks; }
        }

        #endregion

        public TTTreeCheckDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            ListChecks = UIHelper.CreateListView(panProblemsContainer);
            ListChecks.MouseDoubleClick += ListChecks_DblClick;
            ListChecks.AddCheckedColumn(@"x", 50, false);
            ListChecks.ContextMenu = contextMenu;

            fController = new TreeCheckController(this);
            fController.Init(baseWin);
        }

        private void InitializeComponent()
        {
            XamlReader.Load(this);

            miDetails = new ButtonMenuItem();
            miDetails.Click += miDetails_Click;

            miGoToRecord = new ButtonMenuItem();
            miGoToRecord.Click += miGoToRecord_Click;

            miCopyXRef = new ButtonMenuItem();
            miCopyXRef.Click += miCopyXRef_Click;

            contextMenu = new ContextMenu();
            contextMenu.Items.AddRange(new MenuItem[] {
                miDetails,
                miGoToRecord,
                miCopyXRef
            });
            contextMenu.Opening += contextMenu_Opening;
        }

        private void btnAnalyseBase_Click(object sender, EventArgs e)
        {
            fController.CheckBase();
        }

        private void btnBaseRepair_Click(object sender, EventArgs e)
        {
            fController.Repair();
        }

        private void ListChecks_DblClick(object sender, EventArgs e)
        {
            fController.SelectRecord();
        }

        private void miDetails_Click(object sender, EventArgs e)
        {
            fController.ShowDetails();
        }

        private void miGoToRecord_Click(object sender, EventArgs e)
        {
            fController.SelectRecord();
        }

        private void contextMenu_Opening(object sender, EventArgs e)
        {
            var rec = fController.GetSelectedRecord();
            miDetails.Enabled = (rec != null);
            miGoToRecord.Enabled = (rec != null);
            miCopyXRef.Enabled = (rec != null);
        }

        public void miCopyXRef_Click(object sender, EventArgs e)
        {
            var list = ListChecks.GetSelectedItems();
            var text = new StringBuilder();
            foreach (var item in list) {
                var checkObj = (TreeTools.CheckObj)item;
                text.Append(checkObj.Rec.XRef);
                text.Append("\r\n");
            }

            UIHelper.SetClipboardText(text.ToString());
        }
    }
}

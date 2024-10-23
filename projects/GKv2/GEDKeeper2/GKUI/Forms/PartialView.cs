/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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
using System.ComponentModel;
using System.Drawing;
using System.Security.Permissions;
using System.Windows.Forms;
using GDModel;
using GKCore;
using GKCore.Charts;
using GKCore.Controllers;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Options;
using GKUI.Components;
using GKUI.Platform;

namespace GKUI.Forms
{
    public sealed partial class PartialView : CommonWindow, IPartialView
    {
        private readonly PartialViewController fController;


        public IWindow OwnerWindow
        {
            get { return fController.Base; }
        }


        public PartialView(IBaseWindow baseWin, GDMRecordType recordType, IListFilter filter)
        {
            InitializeComponent();

            Icon = new Icon(GKUtils.LoadResourceStream("Resources.icon_gedkeeper.ico"));
            tbFileSave.Image = UIHelper.LoadResourceImage("Resources.btn_save.gif");
            tbRecordAdd.Image = UIHelper.LoadResourceImage("Resources.btn_rec_new.gif");
            tbRecordEdit.Image = UIHelper.LoadResourceImage("Resources.btn_rec_edit.gif");
            tbRecordDelete.Image = UIHelper.LoadResourceImage("Resources.btn_rec_delete.gif");
            tbFilter.Image = UIHelper.LoadResourceImage("Resources.btn_filter.gif");
            tbTreeAncestors.Image = UIHelper.LoadResourceImage("Resources.btn_tree_ancestry.gif");
            tbTreeDescendants.Image = UIHelper.LoadResourceImage("Resources.btn_tree_descendants.gif");
            tbTreeBoth.Image = UIHelper.LoadResourceImage("Resources.btn_tree_both.gif");
            tbPrev.Image = UIHelper.LoadResourceImage("Resources.btn_left.gif");
            tbNext.Image = UIHelper.LoadResourceImage("Resources.btn_right.gif");

            UIHelper.FixToolStrip(ToolBar1);

            fController = new PartialViewController(this, baseWin, recordType);

            CreatePage(baseWin, recordType);

            fController.SetLocale();

            var listMan = fController.GetRecordsListManByType();
            listMan.Filter.Assign(filter);
            fController.ApplyFilter();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fController.Dispose();
                #if !MONO
                if (components != null) components.Dispose();
                #endif
            }
            base.Dispose(disposing);
        }

        private void CreatePage(IBaseWindow baseWin, GDMRecordType recType)
        {
            var summary = new HyperView();
            summary.BorderWidth = 4;
            summary.Dock = DockStyle.Right;
            summary.Size = new Size(300, 290);
            summary.OnLink += mPersonSummaryLink;
            summary.ContextMenuStrip = summaryMenu;

            var recView = new GKListView();
            recView.HideSelection = false;
            recView.LabelEdit = false;
            recView.FullRowSelect = true;
            recView.View = View.Details;
            recView.Dock = DockStyle.Fill;
            recView.DoubleClick += miRecordEdit_Click;
            recView.SelectedIndexChanged += List_SelectedIndexChanged;
            recView.ContextMenuStrip = contextMenu;
            recView.ListMan = RecordsListModel<GDMRecord>.Create(baseWin.Context, recType, false);
            recView.UpdateContents();

            var spl = new Splitter();
            spl.Name = "splitter" + ((int)recType).ToString();
            spl.Dock = DockStyle.Right;
            spl.Size = new Size(4, 290);
            spl.MinExtra = 100;
            spl.MinSize = 100;

            this.Controls.Add(recView);
            this.Controls.Add(summary);
            this.Controls.Add(spl);

            this.Controls.SetChildIndex(recView, 0);
            this.Controls.SetChildIndex(spl, 1);
            this.Controls.SetChildIndex(summary, 2);

            fController.SetTabPart(recType, recView, spl.Name, summary);
        }

        [SecurityPermission(SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.UnmanagedCode), SecurityPermission(SecurityAction.InheritanceDemand, Flags = SecurityPermissionFlag.UnmanagedCode)]
        protected override void WndProc(ref Message m)
        {
            base.WndProc(ref m);

            if (m.Msg == WFAppHost.WM_KEEPMODELESS) {
                AppHost.Instance.WidgetsEnable();
            }
        }

        private void Form_Load(object sender, EventArgs e)
        {
            try {
                AppHost.Instance.SetWindowBounds(this, GlobalOptions.Instance.ChartWindowsShowMode);
                ((IWorkWindow)this).UpdateSettings();
                fController.UpdateControls(false);
            } catch (Exception ex) {
                Logger.WriteError("PartialView.Form_Load()", ex);
            }
        }

        private void Form_Closing(object sender, FormClosingEventArgs e)
        {
            fController.SaveListsSettings();
            FormClosing -= Form_Closing;
        }

        private void Form_Closed(object sender, FormClosedEventArgs e)
        {
            AppHost.Instance.CloseDependentWindows(this);
        }

        private void Form_KeyDown(object sender, KeyEventArgs e)
        {
            switch (e.KeyCode) {
                    /*case Keys.I:
						ItemAdd();
						break;
					case Keys.D:
						ItemDelete();
						break;*/

                case Keys.Return:
                    if (e.Control) {
                        fController.EditRecord();
                    }
                    break;

                case Keys.F12:
                    break;

                    /*case Keys.F:
        			if (e.Control) {
        				QuickFind();
        			}
        			break;*/
            }
        }

        private void contextMenu_Opening(object sender, CancelEventArgs e)
        {
            miContRecordDuplicate.Enabled = (fController.RecordType == GDMRecordType.rtIndividual || fController.RecordType == GDMRecordType.rtLocation);
        }

        private void miRecordAdd_Click(object sender, EventArgs e)
        {
            fController.AddRecord();
        }

        private void miRecordEdit_Click(object sender, EventArgs e)
        {
            fController.EditRecord();
        }

        private void miRecordDelete_Click(object sender, EventArgs e)
        {
            fController.DeleteRecord();
        }

        private void miRecordDuplicate_Click(object sender, EventArgs e)
        {
            fController.DuplicateRecord();
        }

        private void miRecordMerge_Click(object sender, EventArgs e)
        {
            var recView = fController.GetRecordsViewByType() as GKListView;
            if (recView != null) {
                var items = recView.GetSelectedItems();
                BaseController.ShowRecMerge(this, fController.Base,
                    items.Count > 0 ? items[0] as GDMRecord : null,
                    items.Count > 1 ? items[1] as GDMRecord : null
                );
            }
        }

        private void List_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (sender != null) {
                fController.ChangeListItem((IListView)sender);
            }
        }

        private void mPersonSummaryLink(object sender, string linkName)
        {
            fController.SelectSummaryLink((IHyperView)sender, linkName);
        }

        private void miCopyContent_Click(object sender, EventArgs e)
        {
            fController.CopyContent();
        }

        public bool AllowFilter()
        {
            return true;
        }

        public void SetFilter()
        {
            fController.SetFilter();
        }

        #region ILocalizable implementation

        public override void SetLocale()
        {
            fController.SetLocale();
        }

        #endregion

        #region IThemedView implementation

        public override void ApplyTheme()
        {
            base.ApplyTheme();
            fController.ApplyTheme();
        }

        #endregion

        #region IWorkWindow implementation

        void IWorkWindow.UpdateControls()
        {
            var listMan = fController.GetRecordsListManByType();
            string statusLine = LangMan.LS(LSID.SBRecords) + ": " + listMan.TotalCount.ToString();
            statusLine = statusLine + ", " + LangMan.LS(LSID.SBFiltered) + ": " + listMan.FilteredCount.ToString();

            StatusBar.Items[0].Text = statusLine;
        }

        void IWorkWindow.UpdateSettings()
        {
            fController.UpdateSettings();
        }

        void IWorkWindow.NavNext()
        {
            fController.NavNext();
        }

        void IWorkWindow.NavPrev()
        {
            fController.NavPrev();
        }

        bool IWorkWindow.NavCanBackward()
        {
            return fController.NavCanBackward();
        }

        bool IWorkWindow.NavCanForward()
        {
            return fController.NavCanForward();
        }

        public bool AllowQuickSearch()
        {
            return true;
        }

        IList<ISearchResult> IWorkWindow.FindAll(string searchPattern)
        {
            return fController.FindAll(searchPattern);
        }

        void IWorkWindow.SelectByRec(GDMRecord record)
        {
            fController.SelectByRec(record);
        }

        void IWorkWindow.QuickSearch()
        {
            if (!AllowQuickSearch()) return;

            QuickSearchDlg qsDlg = new QuickSearchDlg(this);
            qsDlg.Show();
        }

        #endregion

        private void miFileSave_Click(object sender, EventArgs e)
        {
            fController.Base.SaveFileEx(false);
        }

        private void miFilter_Click(object sender, EventArgs e)
        {
            fController.SetFilter();
        }

        private void tbPrev_Click(object sender, EventArgs e)
        {
            fController.NavPrev();
        }

        private void tbNext_Click(object sender, EventArgs e)
        {
            fController.NavNext();
        }

        private void miTreeAncestors_Click(object sender, EventArgs e)
        {
            fController.ShowTreeChart(TreeChartKind.ckAncestors);
        }

        private void miTreeDescendants_Click(object sender, EventArgs e)
        {
            fController.ShowTreeChart(TreeChartKind.ckDescendants);
        }

        private void miTreeBoth_Click(object sender, EventArgs e)
        {
            fController.ShowTreeChart(TreeChartKind.ckBoth);
        }
    }
}

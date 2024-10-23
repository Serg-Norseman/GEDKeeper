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
using Eto.Forms;
using Eto.Serialization.Xaml;
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

namespace GKUI.Forms
{
    public sealed partial class PartialView : CommonWindow, IPartialView
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private TableLayout StatusBar;
        private Label panStatusText;
        private ToolBar ToolBar1;
        private ButtonToolItem tbFileSave;
        private ButtonToolItem tbRecordAdd;
        private ButtonToolItem tbRecordEdit;
        private ButtonToolItem tbRecordDelete;
        private ButtonToolItem tbFilter;
        private ButtonToolItem tbTreeAncestors;
        private ButtonToolItem tbTreeDescendants;
        private ButtonToolItem tbStats;
        private ButtonToolItem tbPrev;
        private ButtonToolItem tbNext;
        private ButtonToolItem tbTreeBoth;
        private ButtonMenuItem miContRecordDuplicate;
        private ButtonMenuItem miContRecordDelete;
        private ButtonMenuItem miContRecordEdit;
        private ButtonMenuItem miContRecordMerge;
        private ContextMenu contextMenu;
        private ButtonMenuItem miContRecordAdd;
        private ContextMenu summaryMenu;
        private ButtonMenuItem miCopyContent;
        private Panel panel;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        private readonly PartialViewController fController;


        public IWindow OwnerWindow
        {
            get { return fController.Base; }
        }


        public PartialView(IBaseWindow baseWin, GDMRecordType recordType, IListFilter filter)
        {
            XamlReader.Load(this);
            InitializeComponent();

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
            }
            base.Dispose(disposing);
        }

        private void InitializeComponent()
        {
            miContRecordAdd = new ButtonMenuItem(miRecordAdd_Click);
            miContRecordEdit = new ButtonMenuItem(miRecordEdit_Click);
            miContRecordDelete = new ButtonMenuItem(miRecordDelete_Click);
            miContRecordDuplicate = new ButtonMenuItem(miRecordDuplicate_Click);
            miContRecordMerge = new ButtonMenuItem(miRecordMerge_Click);
            contextMenu = new ContextMenu();
            contextMenu.Items.AddRange(new MenuItem[] { miContRecordAdd, miContRecordEdit, miContRecordDelete, miContRecordDuplicate, miContRecordMerge });
            contextMenu.Opening += contextMenu_Opening;

            miCopyContent = new ButtonMenuItem(miCopyContent_Click);
            summaryMenu = new ContextMenu();
            summaryMenu.Items.AddRange(new MenuItem[] { miCopyContent });
        }

        private void CreatePage(IBaseWindow baseWin, GDMRecordType recType)
        {
            var summary = new HyperView();
            summary.BorderWidth = 4;
            summary.OnLink += mPersonSummaryLink;
            summary.ContextMenu = summaryMenu;

            var recView = new GKListView();
            recView.AllowMultipleSelection = true;
            recView.MouseDoubleClick += miRecordEdit_Click;
            recView.SelectedItemsChanged += List_SelectedIndexChanged;
            recView.KeyDown += Form_KeyDown;
            recView.ContextMenu = contextMenu;
            recView.ListMan = RecordsListModel<GDMRecord>.Create(fController.Base.Context, recType, false);
            recView.UpdateContents();

            var spl = new Splitter();
            spl.ID = "splitter" + ((int)recType).ToString();
            spl.Panel1 = recView;
            spl.Panel2 = summary;
            spl.RelativePosition = 300;
            spl.Orientation = Orientation.Horizontal;
            spl.FixedPanel = SplitterFixedPanel.Panel2;

            panel.Content = spl;

            fController.SetTabPart(recType, recView, spl.ID, summary);
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

        private void Form_Closing(object sender, CancelEventArgs e)
        {
            fController.SaveListsSettings();
        }

        private void Form_Closed(object sender, EventArgs e)
        {
            AppHost.Instance.CloseDependentWindows(this);
        }

        private void Form_KeyDown(object sender, KeyEventArgs e)
        {
            switch (e.Key) {
                    /*case Keys.I:
						ItemAdd();
						break;
					case Keys.D:
						ItemDelete();
						break;*/

                case Keys.Enter:
                    if (e.Control) {
                        fController.EditRecord();
                    }
                    break;

                case Keys.Home:
                case Keys.End:
                    if (sender is GKListView) {
                        var listView = sender as GKListView;
                        if (e.Key == Keys.Home) {
                            listView.SelectedIndex = 0;
                        } else {
                            listView.SelectedIndex = -1;
                        }
                        e.Handled = true;
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

        private void contextMenu_Opening(object sender, EventArgs e)
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

            panStatusText.Text = statusLine;
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

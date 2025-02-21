/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
using System.IO;
using GKCore;
using GKUI.Components;

namespace GKNavigatorPlugin
{
#if !GK3
    using System.Drawing;
    using System.Windows.Forms;
#else
    using Eto.Drawing;
    using Eto.Forms;
#endif


    public class WebLinksView : Panel
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051
#if !GK3
        private ToolStrip toolStrip1;
        private ToolStripButton btnCheck;
        private GKListView lvData;
        private ToolStripButton btnLoadFile;
        private ToolStripSeparator toolStripSeparator1;
        private ToolStripComboBox cbDataFiles;
#else
        private Panel toolStrip1;
        private Button btnCheck;
        private Button btnLoadFile;
        private ComboBox cbDataFiles;
        private GKListView lvData;
#endif
#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        private readonly HistoryData fData;


        public WebLinksView()
        {
            InitializeComponent();

            fData = new HistoryData();

            UpdateFiles();

            SetLocale();
        }

        private void InitializeComponent()
        {
#if !GK3
            btnCheck = new ToolStripButton();
            btnCheck.DisplayStyle = ToolStripItemDisplayStyle.Text;
            btnCheck.Size = new Size(52, 25);
            btnCheck.Text = "Check";
            btnCheck.Click += new EventHandler(btnCheck_Click);

            btnLoadFile = new ToolStripButton();
            btnLoadFile.DisplayStyle = ToolStripItemDisplayStyle.Text;
            btnLoadFile.Size = new Size(71, 25);
            btnLoadFile.Text = "Load file";
            btnLoadFile.Click += new EventHandler(btnLoadFile_Click);

            toolStripSeparator1 = new ToolStripSeparator();
            toolStripSeparator1.Size = new Size(6, 28);

            cbDataFiles = new ToolStripComboBox();
            cbDataFiles.DropDownStyle = ComboBoxStyle.DropDownList;
            cbDataFiles.Size = new Size(200, 28);

            lvData = new GKListView();
            lvData.Dock = DockStyle.Fill;
            lvData.FullRowSelect = true;
            lvData.HideSelection = false;
            lvData.ListMan = null;
            lvData.Location = new Point(0, 28);
            lvData.Name = "lvData";
            lvData.SortOrder = GKCore.Design.BSDTypes.SortOrder.None;
            lvData.OwnerDraw = true;
            lvData.Size = new Size(694, 369);
            lvData.SortColumn = 0;
            lvData.TabIndex = 1;
            lvData.UseCompatibleStateImageBehavior = false;
            lvData.View = View.Details;
            lvData.MouseDoubleClick += new MouseEventHandler(lvData_MouseDoubleClick);

            toolStrip1 = new ToolStrip();
            toolStrip1.SuspendLayout();
            toolStrip1.ImageScalingSize = new Size(20, 20);
            toolStrip1.Items.AddRange(new ToolStripItem[] { btnCheck, btnLoadFile, toolStripSeparator1, cbDataFiles });
            toolStrip1.Location = new Point(0, 0);
            toolStrip1.Size = new Size(694, 28);
            toolStrip1.ResumeLayout(false);
            toolStrip1.PerformLayout();

            SuspendLayout();
            Controls.Add(lvData);
            Controls.Add(toolStrip1);
            ResumeLayout(false);
            PerformLayout();
#else
            btnCheck = new Button();
            btnCheck.Text = "Check";
            btnCheck.Click += btnCheck_Click;

            btnLoadFile = new Button();
            btnLoadFile.Text = "Load file";
            btnLoadFile.Click += btnLoadFile_Click;

            cbDataFiles = new ComboBox();

            lvData = new GKListView();
            lvData.Size = new Size(694, 369);
            lvData.MouseDoubleClick += lvData_MouseDoubleClick;

            toolStrip1 = new Panel();
            toolStrip1.Content = new StackLayout() {
                Orientation = Orientation.Horizontal,
                Spacing = 10,
                Items = { btnCheck, btnLoadFile, cbDataFiles }
            };

            Content = new TableLayout() {
                Rows = {
                    new TableRow() {
                        Cells = { toolStrip1 }
                    },
                    new TableRow() {
                        ScaleHeight = true,
                        Cells = { lvData }
                    }
                }
            };
#endif
        }

        public void SetLocale()
        {
#if !GK3
            //Text = fPlugin.LangMan.LS(PLS.HistoryData);
#else
            //Title = fPlugin.LangMan.LS(PLS.HistoryData);
#endif
        }

        private void UpdateFiles()
        {
            cbDataFiles.Items.Clear();
            foreach (string cf in fData.CSVFiles) {
                cbDataFiles.Items.Add(Path.GetFileName(cf));
            }
        }

        private void lvData_MouseDoubleClick(object sender, MouseEventArgs e)
        {
#if !GK3
            GKListItem item = lvData.GetSelectedItem();
            if (item != null && fData.LinkColumn != -1) {
                GKUtils.LoadExtFile(item.SubItems[fData.LinkColumn].Text);
            }
#else
            var item = lvData.GetSelectedData() as LinkItem;
            if (item != null && fData.LinkColumn != -1) {
                GKUtils.LoadExtFile(item.Data[fData.LinkColumn].ToString());
            }
#endif
        }

        private void btnCheck_Click(object sender, EventArgs e)
        {
            fData.Check();
            RefreshList();
        }

        private void RefreshList()
        {
            lvData.Clear();

            for (int i = 0; i < fData.Headers.Length; i++) {
                bool autoSize = (i == 0);
                lvData.AddColumn(fData.Headers[i].ToString(), 200, autoSize);
            }

            for (int i = 0; i < fData.Items.Count; i++) {
                var item = fData.Items[i];
#if !GK3
                item.Item = lvData.AddItem(null, item.Data);
                var listItem = item.Item as GKListItem;

                switch (item.State) {
                    case LinkState.Normal:
                        listItem.BackColor = Color.PaleGreen;
                        break;
                    case LinkState.Invalid:
                        listItem.BackColor = Color.IndianRed;
                        break;
                    case LinkState.Duplicate:
                        listItem.BackColor = Color.Orange;
                        break;
                }
#else
                item.Item = lvData.AddItem(item, item.Data);
                var listItem = item.Item as GKListItem;

                switch (item.State) {
                    case LinkState.Normal:
                        listItem.BackColor = Colors.PaleGreen;
                        break;
                    case LinkState.Invalid:
                        listItem.BackColor = Colors.IndianRed;
                        break;
                    case LinkState.Duplicate:
                        listItem.BackColor = Colors.Orange;
                        break;
                }
#endif
            }
        }

        private void btnLoadFile_Click(object sender, EventArgs e)
        {
            if (fData.LoadDataFile(cbDataFiles.Text)) {
                RefreshList();
            }
        }
    }
}

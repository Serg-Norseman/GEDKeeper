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
using GKCore;
using GKCore.Design.Controls;
using GKCore.Filters;
using GKCore.Lists;
using GKCore.Locales;
using GKUI.Themes;

namespace GKUI.Components
{
    public class FilterGridView : ContainerControl, IFilterGridView
    {
        private readonly ToolStripButton fBtnAdd;
        private readonly ToolStripButton fBtnDelete;
        private readonly ToolStrip fToolBar;
        private readonly DataGridView fGridView;
        private readonly IRecordsListModel fListMan;
        private string[] fFields;
        private MaskedTextBox fMaskedTextBox;


        public int Count
        {
            get { return fGridView.Rows.Count; }
        }

        public ColumnConditionExpression this[int index]
        {
            get {
                DataGridViewRow row = fGridView.Rows[index];

                // ".Value" can be null, so that we should to use direct cast
                string fld = (string)row.Cells[0].Value;
                string cnd = (string)row.Cells[1].Value;
                string val = (string)row.Cells[2].Value;

                ColumnConditionExpression fcond = null;
                if (!string.IsNullOrEmpty(fld)) {
                    int colId = fListMan.GetFieldColumnId(fFields, fld);
                    if (colId != -1) {
                        ConditionOperator cond = ListFilter.GetCondByName(cnd);
                        fcond = new ColumnConditionExpression((byte)colId, cond, val);
                    }
                }
                return fcond;
            }
        }


        public FilterGridView(IRecordsListModel listMan)
        {
            fBtnDelete = CreateButton("btnDelete", UIHelper.LoadResourceImage("Resources.btn_rec_delete.gif"), LangMan.LS(LSID.MIRecordDelete), ItemDelete);
            fBtnAdd = CreateButton("btnAdd", UIHelper.LoadResourceImage("Resources.btn_rec_new.gif"), LangMan.LS(LSID.MIRecordAdd), ItemAdd);

            fToolBar = new ToolStrip();
            fToolBar.Name = "ToolBar";
            fToolBar.Dock = DockStyle.Right;
            fToolBar.Items.AddRange(new ToolStripItem[] { fBtnAdd, fBtnDelete });
            fToolBar.GripStyle = ToolStripGripStyle.Hidden;
            fToolBar.ImageScalingSize = new Size(24, 20);
            fToolBar.AutoSize = true;
            fToolBar.ShowItemToolTips = true;

            fGridView = new DataGridView();
            fGridView.Dock = DockStyle.Fill;

            SuspendLayout();
            Controls.Add(fGridView);
            Controls.Add(fToolBar);
            ResumeLayout(false);

            fGridView.AllowUserToResizeRows = false;
            fGridView.ColumnHeadersHeightSizeMode = DataGridViewColumnHeadersHeightSizeMode.AutoSize;
            fGridView.Margin = new Padding(2);
            fGridView.MultiSelect = false;
            fGridView.RowHeadersVisible = false;
            fGridView.Name = "dataGridView1";

            fListMan = listMan;
            fFields = fListMan.CreateFields();

            InitGrid();
        }

        public void AddCondition(ColumnConditionExpression fcond)
        {
            int r = fGridView.Rows.Add();
            DataGridViewRow row = fGridView.Rows[r];

            int condIndex = ((IConvertible)fcond.Operator).ToByte(null);

            row.Cells[0].Value = fFields[fcond.ColumnIndex + 1];
            row.Cells[1].Value = GKData.CondSigns[condIndex];
            row.Cells[2].Value = fcond.Value.ToString();
        }

        public void RemoveCondition(int index)
        {
            if (index >= 0 && index < fGridView.Rows.Count) {
                fGridView.Rows.RemoveAt(index);
            }
        }

        public void Clear()
        {
            fGridView.Rows.Clear();
        }

        public void Activate()
        {
            Select();
        }

        public void ApplyTheme()
        {
            fGridView.BackgroundColor = this.BackColor;

            UIHelper.SetButtonThemeImage(fBtnDelete, ThemeElement.Glyph_ItemDelete);
            UIHelper.SetButtonThemeImage(fBtnAdd, ThemeElement.Glyph_ItemAdd);
        }

        #region Private functions

        private ToolStripButton CreateButton(string name, Image image, string toolTip, EventHandler click)
        {
            var btn = new ToolStripButton();
            btn.Name = name;
            btn.Image = image;
            btn.ToolTipText = toolTip;
            btn.Click += click;
            return btn;
        }

        private static DataGridViewColumn AddTextColumn(string colName, string headerText, int width)
        {
            DataGridViewColumn col = new DataGridViewTextBoxColumn();
            col.HeaderText = headerText;
            col.Name = colName;
            col.Width = width;
            return col;
        }

        private static DataGridViewColumn AddComboColumn(string colName, string headerText, object[] items, int width)
        {
            DataGridViewComboBoxColumn col = new DataGridViewComboBoxColumn();
            col.HeaderText = headerText;
            col.Name = colName;
            col.Width = width;
            col.Items.AddRange(items);
            return col;
        }

        private void InitGrid()
        {
            fMaskedTextBox = new MaskedTextBox();
            fMaskedTextBox.Visible = false;
            fMaskedTextBox.Name = "fMaskedTextBox";
            fMaskedTextBox.Mask = @"00/00/0000";
            fMaskedTextBox.TextMaskFormat = MaskFormat.IncludePromptAndLiterals;
            Controls.Add(fMaskedTextBox);

            fGridView.Rows.Clear();
            ((System.ComponentModel.ISupportInitialize)(fGridView)).BeginInit();
            fGridView.Columns.AddRange(new DataGridViewColumn[] {
                AddComboColumn("FField", LangMan.LS(LSID.Field), fFields, 200),
                AddComboColumn("FCondition", LangMan.LS(LSID.Condition), GKData.CondSigns, 150),
                AddTextColumn("FValue", LangMan.LS(LSID.Value), 300)
            });

            fGridView.CellBeginEdit += dataGridView1_CellBeginEdit;
            fGridView.CellEndEdit += dataGridView1_CellEndEdit;
            Scroll += dataGridView1_Scroll;

            ((System.ComponentModel.ISupportInitialize)(fGridView)).EndInit();

            //this.dataGridView1.AutoResizeColumns();
            fGridView.Columns[0].Width = 150;
            fGridView.Columns[1].Width = 150;
            fGridView.Columns[2].Width = 150;
        }

        private bool IsGEDCOMDateCell(int rowIndex)
        {
            DataGridViewRow row = fGridView.Rows[rowIndex];

            string fld = (string)row.Cells[0].Value;
            if (!string.IsNullOrEmpty(fld)) {
                int colId = fListMan.GetFieldColumnId(fFields, fld);
                DataType dataType = fListMan.GetColumnDataType(colId);

                return (dataType == DataType.dtGEDCOMDate);
            }

            return false;
        }

        private void dataGridView1_Scroll(object sender, ScrollEventArgs e)
        {
            if (fMaskedTextBox.Visible) {
                DataGridViewCell cell = fGridView.CurrentCell;
                Rectangle rect = fGridView.GetCellDisplayRectangle(cell.ColumnIndex, cell.RowIndex, true);
                fMaskedTextBox.Location = rect.Location;
            }
        }

        private void dataGridView1_CellBeginEdit(object sender, DataGridViewCellCancelEventArgs e)
        {
            if (e.ColumnIndex == 2 && e.RowIndex < fGridView.NewRowIndex) {
                if (IsGEDCOMDateCell(e.RowIndex)) {
                    Rectangle rect = fGridView.GetCellDisplayRectangle(e.ColumnIndex, e.RowIndex, true);

                    if (fGridView[e.ColumnIndex, e.RowIndex].Value != null) {
                        fMaskedTextBox.Text = fGridView[e.ColumnIndex, e.RowIndex].Value.ToString();
                    } else {
                        fMaskedTextBox.Text = "";
                    }

                    fMaskedTextBox.Location = rect.Location;
                    fMaskedTextBox.Size = rect.Size;
                    fMaskedTextBox.Visible = true;
                    fMaskedTextBox.Focus();
                }
            }
        }

        private void dataGridView1_CellEndEdit(object sender, DataGridViewCellEventArgs e)
        {
            if (fMaskedTextBox.Visible) {
                fGridView.CurrentCell.Value = fMaskedTextBox.Text;
                fMaskedTextBox.Visible = false;
                Focus();
            }
        }

        private void ItemAdd(object sender, EventArgs e)
        {
            fGridView.Rows.Add();
        }

        private void ItemDelete(object sender, EventArgs e)
        {
            RemoveCondition(fGridView.CurrentRow.Index);
        }

        #endregion
    }
}

/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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
using System.Drawing;
using System.Windows.Forms;
using GKCore;
using GKCore.Design.Controls;
using GKCore.Interfaces;

namespace GKUI.Components
{
    public class FilterGridView : DataGridView, IFilterGridView
    {
        private readonly IRecordsListModel fListMan;
        private string[] fFields;
        private MaskedTextBox fMaskedTextBox;


        public FilterGridView(IRecordsListModel listMan)
        {
            AllowUserToResizeRows = false;
            ColumnHeadersHeightSizeMode = DataGridViewColumnHeadersHeightSizeMode.AutoSize;
            Margin = new Padding(2);
            MultiSelect = false;
            RowHeadersVisible = false;

            fListMan = listMan;
            fFields = fListMan.CreateFields();

            InitGrid();
        }

        public int Count
        {
            get { return Rows.Count; }
        }

        public FilterCondition this[int index]
        {
            get {
                DataGridViewRow row = Rows[index];

                // ".Value" can be null, so that we should to use direct cast
                string fld = (string)row.Cells[0].Value;
                string cnd = (string)row.Cells[1].Value;
                string val = (string)row.Cells[2].Value;

                FilterCondition fcond = null;
                if (!string.IsNullOrEmpty(fld)) {
                    int colId = fListMan.GetFieldColumnId(fFields, fld);
                    if (colId != -1) {
                        ConditionKind cond = fListMan.GetCondByName(cnd);
                        fcond = new FilterCondition((byte)colId, cond, val);
                    }
                }
                return fcond;
            }
        }

        public void AddCondition(FilterCondition fcond)
        {
            int r = Rows.Add();
            DataGridViewRow row = Rows[r];

            int condIndex = ((IConvertible)fcond.Condition).ToByte(null);

            row.Cells[0].Value = fFields[fcond.ColumnIndex + 1];
            row.Cells[1].Value = GKData.CondSigns[condIndex];
            row.Cells[2].Value = fcond.Value.ToString();
        }

        public void Clear()
        {
            Rows.Clear();
        }

        public void Activate()
        {
            Select();
        }

        #region Private functions

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

            Rows.Clear();
            ((System.ComponentModel.ISupportInitialize)(this)).BeginInit();
            Columns.AddRange(new DataGridViewColumn[] {
                                               AddComboColumn("FField", LangMan.LS(LSID.LSID_Field), fFields, 200),
                                               AddComboColumn("FCondition", LangMan.LS(LSID.LSID_Condition), GKData.CondSigns, 150),
                                               AddTextColumn("FValue", LangMan.LS(LSID.LSID_Value), 300)});

            CellBeginEdit += dataGridView1_CellBeginEdit;
            CellEndEdit += dataGridView1_CellEndEdit;
            Scroll += dataGridView1_Scroll;

            ((System.ComponentModel.ISupportInitialize)(this)).EndInit();

            //this.dataGridView1.AutoResizeColumns();
            Columns[0].Width = 150;
            Columns[1].Width = 150;
            Columns[2].Width = 150;
        }

        private bool IsGEDCOMDateCell(int rowIndex)
        {
            DataGridViewRow row = Rows[rowIndex];

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
                DataGridViewCell cell = CurrentCell;
                Rectangle rect = GetCellDisplayRectangle(cell.ColumnIndex, cell.RowIndex, true);
                fMaskedTextBox.Location = rect.Location;
            }
        }

        private void dataGridView1_CellBeginEdit(object sender, DataGridViewCellCancelEventArgs e)
        {
            if (e.ColumnIndex == 2 && e.RowIndex < NewRowIndex) {
                if (IsGEDCOMDateCell(e.RowIndex)) {
                    Rectangle rect = GetCellDisplayRectangle(e.ColumnIndex, e.RowIndex, true);

                    if (this[e.ColumnIndex, e.RowIndex].Value != null) {
                        fMaskedTextBox.Text = this[e.ColumnIndex, e.RowIndex].Value.ToString();
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
                CurrentCell.Value = fMaskedTextBox.Text;
                fMaskedTextBox.Visible = false;
                Focus();
            }
        }

        #endregion
    }
}

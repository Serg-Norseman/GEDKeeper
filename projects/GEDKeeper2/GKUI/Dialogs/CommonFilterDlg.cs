/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
using GKCore.Interfaces;
using GKCore.Lists;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public partial class CommonFilterDlg : Form
    {
        private readonly IBaseWindow fBase;
        private readonly string[] fFields;
        private readonly ListManager fListMan;
        private MaskedTextBox fMaskedTextBox;

        public IBaseWindow Base
        {
            get { return this.fBase; }
        }

        public CommonFilterDlg()
        {
            this.InitializeComponent();
        }

        public CommonFilterDlg(IBaseWindow aBase, ListManager listMan)
        {
            this.InitializeComponent();

            this.btnAccept.Image = (Image)MainWin.ResourceManager.GetObjectEx("iBtnAccept");
            this.btnCancel.Image = (Image)MainWin.ResourceManager.GetObjectEx("iBtnCancel");

            this.fBase = aBase;
            this.fListMan = listMan;

            Type colEnum = fListMan.GetColumnsEnum();
            Array enums = Enum.GetValues(colEnum);
            this.fFields = new string[enums.Length + 1];
            this.fFields[0] = "";
            int idx = 1;
            foreach (Enum e in enums)
            {
                this.fFields[idx] = fListMan.GetColumnName(e);
                idx++;
            }

            this.InitGrid();
            this.UpdateGrid();

            this.SetLang();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
            }
            base.Dispose(disposing);
        }

        #region Private functions

        private static ConditionKind GetCondByName(string condName)
        {
            ConditionKind res = ConditionKind.ck_NotEq;

            for (ConditionKind pl = ConditionKind.ck_NotEq; pl <= ConditionKind.ck_NotContains; pl++)
            {
                if (GKData.CondSigns[(int)pl] == condName)
                {
                    res = pl;
                    break;
                }
            }

            return res;
        }

        private Enum GetFieldColumn(string fieldName)
        {
            int idx = -1;
            for (int i = 0; i < fFields.Length; i++)
            {
                if (fFields[i] == fieldName)
                {
                    idx = i;
                    break;
                }
            }

            idx = idx - 1; // exclude empty item
            Type colEnum = fListMan.GetColumnsEnum();
            Array enums = Enum.GetValues(colEnum);
            return (Enum)enums.GetValue(idx);
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
            this.fMaskedTextBox = new MaskedTextBox();
            this.fMaskedTextBox.Visible = false;
            this.fMaskedTextBox.Mask = @"00/00/0000";
            this.fMaskedTextBox.TextMaskFormat = MaskFormat.IncludePromptAndLiterals;
            this.dataGridView1.Controls.Add(this.fMaskedTextBox);

            this.dataGridView1.Rows.Clear();
            ((System.ComponentModel.ISupportInitialize)(this.dataGridView1)).BeginInit();
            this.dataGridView1.Columns.AddRange(new DataGridViewColumn[] {
                                                    AddComboColumn("FField", LangMan.LS(LSID.LSID_Field), this.fFields, 200),
                                                    AddComboColumn("FCondition", LangMan.LS(LSID.LSID_Condition), GKData.CondSigns, 150),
                                                    AddTextColumn("FValue", LangMan.LS(LSID.LSID_Value), 300)});

            this.dataGridView1.CellBeginEdit += dataGridView1_CellBeginEdit;
            this.dataGridView1.CellEndEdit += dataGridView1_CellEndEdit;
            this.dataGridView1.Scroll += dataGridView1_Scroll;

            ((System.ComponentModel.ISupportInitialize)(this.dataGridView1)).EndInit();
        }

        private bool IsGEDCOMDateCell(int rowIndex)
        {
            DataGridViewRow row = dataGridView1.Rows[rowIndex];

            string fld = (string)row.Cells[0].Value;
            if (!string.IsNullOrEmpty(fld)) {
                Enum column = GetFieldColumn(fld);
                int col = (column as IConvertible).ToByte(null);
                DataType dataType = fListMan.GetColumnDataType(col);
                
                return (dataType == DataType.dtGEDCOMDate);
            }

            return false;
        }

        private void dataGridView1_Scroll(object sender, ScrollEventArgs e)
        {
            if (this.fMaskedTextBox.Visible)
            {
                DataGridViewCell cell = this.dataGridView1.CurrentCell;
                Rectangle rect = this.dataGridView1.GetCellDisplayRectangle(cell.ColumnIndex, cell.RowIndex, true);
                this.fMaskedTextBox.Location = rect.Location;
            }
        }

        private void dataGridView1_CellBeginEdit(object sender, DataGridViewCellCancelEventArgs e)
        {
            if (e.ColumnIndex == 2 && e.RowIndex < this.dataGridView1.NewRowIndex) {
                if (this.IsGEDCOMDateCell(e.RowIndex)) {
                    Rectangle rect = this.dataGridView1.GetCellDisplayRectangle(e.ColumnIndex, e.RowIndex, true);

                    if (this.dataGridView1[e.ColumnIndex, e.RowIndex].Value != null) {
                        this.fMaskedTextBox.Text = this.dataGridView1[e.ColumnIndex, e.RowIndex].Value.ToString();
                    } else {
                        this.fMaskedTextBox.Text = "";
                    }

                    this.fMaskedTextBox.Location = rect.Location;
                    this.fMaskedTextBox.Size = rect.Size;
                    this.fMaskedTextBox.Visible = true;
                    this.fMaskedTextBox.Focus();
                }
            }
        }

        private void dataGridView1_CellEndEdit(object sender, DataGridViewCellEventArgs e)
        {
            if (this.fMaskedTextBox.Visible) {
                this.dataGridView1.CurrentCell.Value = this.fMaskedTextBox.Text;
                this.fMaskedTextBox.Visible = false;
                this.dataGridView1.Focus();
            }
        }

        private void UpdateGrid()
        {
            this.dataGridView1.Rows.Clear();

            int num = fListMan.Filter.Conditions.Count;
            for (int i = 0; i < num; i++) {
                FilterCondition fcond = fListMan.Filter.Conditions[i];

                int r = this.dataGridView1.Rows.Add();
                DataGridViewRow row = dataGridView1.Rows[r];

                int condIndex = ((IConvertible)fcond.Condition).ToByte(null);

                row.Cells[0].Value = this.fFields[fcond.ColumnIndex + 1];
                row.Cells[1].Value = GKData.CondSigns[condIndex];
                row.Cells[2].Value = fcond.Value.ToString();
            }

            //this.dataGridView1.AutoResizeColumns();
            this.dataGridView1.Columns[0].Width = 150;
            this.dataGridView1.Columns[1].Width = 150;
            this.dataGridView1.Columns[2].Width = 150;
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            try
            {
                this.AcceptChanges();
                base.DialogResult = DialogResult.OK;
            }
            catch (Exception ex)
            {
                this.fBase.Host.LogWrite("CommonFilterDlg.btnAccept_Click(): " + ex.Message);
                base.DialogResult = DialogResult.None;
            }
        }

        private void btnReset_Click(object sender, EventArgs e)
        {
            this.DoReset();
        }

        #endregion

        public virtual void AcceptChanges()
        {
            fListMan.Filter.Clear();

            int num = dataGridView1.Rows.Count;
            for (int r = 0; r < num; r++)
            {
                DataGridViewRow row = dataGridView1.Rows[r];

                string fld = (string)row.Cells[0].Value;
                string cnd = (string)row.Cells[1].Value;
                string val = (string)row.Cells[2].Value;

                if (!string.IsNullOrEmpty(fld)) {
                    ConditionKind cond = GetCondByName(cnd);
                    Enum column = GetFieldColumn(fld);
                    fListMan.AddCondition(column, cond, val);
                }
            }

            DialogResult = DialogResult.OK;
        }

        public void SetLang()
        {
            this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            this.btnReset.Text = LangMan.LS(LSID.LSID_DlgReset);
            this.tsFieldsFilter.Text = LangMan.LS(LSID.LSID_FieldsFilter);
        }

        public virtual void DoReset()
        {
            fListMan.Filter.Clear();
            this.UpdateGrid();
        }
    }
}

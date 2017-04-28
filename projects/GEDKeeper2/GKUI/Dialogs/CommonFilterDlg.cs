/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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

using GKCommon;
using GKCore;
using GKCore.Interfaces;
using GKCore.Lists;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public partial class CommonFilterDlg : Form, ICommonDialog
    {
        private readonly IBaseWindow fBase;
        private readonly string[] fFields;
        private readonly IListManager fListMan;
        private MaskedTextBox fMaskedTextBox;

        public IBaseWindow Base
        {
            get { return fBase; }
        }

        public IListManager ListMan
        {
            get { return fListMan; }
        }

        public CommonFilterDlg()
        {
            InitializeComponent();
        }

        public CommonFilterDlg(IBaseWindow baseWin, IListManager listMan)
        {
            if (baseWin == null)
                throw new ArgumentNullException("baseWin");

            if (listMan == null)
                throw new ArgumentNullException("listMan");

            InitializeComponent();

            btnAccept.Image = GKResources.iBtnAccept;
            btnCancel.Image = GKResources.iBtnCancel;

            fBase = baseWin;
            fListMan = listMan;

            ListColumns listColumns = (ListColumns)fListMan.ListColumns;
            fFields = new string[listColumns.Count + 1]; // +empty item
            fFields[0] = "";

            for (int idx = 0; idx < listColumns.Count; idx++)
            {
                var cs = listColumns[idx];
                fFields[idx + 1] = fListMan.GetColumnName(cs.Id);
            }

            SetLang();

            InitGrid();
            UpdateGrid();
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

        private int GetFieldColumnId(string fieldName)
        {
            int idx = -1;
            for (int i = 0; i < fFields.Length; i++)
            {
                if (fFields[i] == fieldName)
                {
                    idx = i - 1; // exclude empty item
                    break;
                }
            }

            return idx;
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
            dataGridView1.Controls.Add(fMaskedTextBox);

            dataGridView1.Rows.Clear();
            ((System.ComponentModel.ISupportInitialize)(dataGridView1)).BeginInit();
            dataGridView1.Columns.AddRange(new DataGridViewColumn[] {
                                               AddComboColumn("FField", LangMan.LS(LSID.LSID_Field), fFields, 200),
                                               AddComboColumn("FCondition", LangMan.LS(LSID.LSID_Condition), GKData.CondSigns, 150),
                                               AddTextColumn("FValue", LangMan.LS(LSID.LSID_Value), 300)});

            dataGridView1.CellBeginEdit += dataGridView1_CellBeginEdit;
            dataGridView1.CellEndEdit += dataGridView1_CellEndEdit;
            dataGridView1.Scroll += dataGridView1_Scroll;

            ((System.ComponentModel.ISupportInitialize)(dataGridView1)).EndInit();
        }

        private bool IsGEDCOMDateCell(int rowIndex)
        {
            DataGridViewRow row = dataGridView1.Rows[rowIndex];

            string fld = (string)row.Cells[0].Value;
            if (!string.IsNullOrEmpty(fld)) {
                int colId = GetFieldColumnId(fld);
                DataType dataType = fListMan.GetColumnDataType(colId);

                return (dataType == DataType.dtGEDCOMDate);
            }

            return false;
        }

        private void dataGridView1_Scroll(object sender, ScrollEventArgs e)
        {
            if (fMaskedTextBox.Visible)
            {
                DataGridViewCell cell = dataGridView1.CurrentCell;
                Rectangle rect = dataGridView1.GetCellDisplayRectangle(cell.ColumnIndex, cell.RowIndex, true);
                fMaskedTextBox.Location = rect.Location;
            }
        }

        private void dataGridView1_CellBeginEdit(object sender, DataGridViewCellCancelEventArgs e)
        {
            if (e.ColumnIndex == 2 && e.RowIndex < dataGridView1.NewRowIndex) {
                if (IsGEDCOMDateCell(e.RowIndex)) {
                    Rectangle rect = dataGridView1.GetCellDisplayRectangle(e.ColumnIndex, e.RowIndex, true);

                    if (dataGridView1[e.ColumnIndex, e.RowIndex].Value != null) {
                        fMaskedTextBox.Text = dataGridView1[e.ColumnIndex, e.RowIndex].Value.ToString();
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
                dataGridView1.CurrentCell.Value = fMaskedTextBox.Text;
                fMaskedTextBox.Visible = false;
                dataGridView1.Focus();
            }
        }

        private void UpdateGrid()
        {
            dataGridView1.Rows.Clear();

            int num = fListMan.Filter.Conditions.Count;
            for (int i = 0; i < num; i++)
            {
                FilterCondition fcond = fListMan.Filter.Conditions[i];

                int r = dataGridView1.Rows.Add();
                DataGridViewRow row = dataGridView1.Rows[r];

                int condIndex = ((IConvertible)fcond.Condition).ToByte(null);

                row.Cells[0].Value = fFields[fcond.ColumnIndex + 1];
                row.Cells[1].Value = GKData.CondSigns[condIndex];
                row.Cells[2].Value = fcond.Value.ToString();
            }

            //this.dataGridView1.AutoResizeColumns();
            dataGridView1.Columns[0].Width = 150;
            dataGridView1.Columns[1].Width = 150;
            dataGridView1.Columns[2].Width = 150;
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            try
            {
                AcceptChanges();
                DialogResult = DialogResult.OK;
            }
            catch (Exception ex)
            {
                Logger.LogWrite("CommonFilterDlg.btnAccept_Click(): " + ex.Message);
                DialogResult = DialogResult.None;
            }
        }

        private void btnReset_Click(object sender, EventArgs e)
        {
            DoReset();
        }

        #endregion

        public virtual void AcceptChanges()
        {
            fListMan.Filter.Clear();

            int num = dataGridView1.Rows.Count;
            for (int r = 0; r < num; r++)
            {
                DataGridViewRow row = dataGridView1.Rows[r];

                // ".Value" can be null, so that we should to use direct cast
                string fld = (string)row.Cells[0].Value;
                string cnd = (string)row.Cells[1].Value;
                string val = (string)row.Cells[2].Value;

                if (!string.IsNullOrEmpty(fld)) {
                    int colId = GetFieldColumnId(fld);
                    if (colId != -1) {
                        ConditionKind cond = GetCondByName(cnd);
                        fListMan.AddCondition((byte)colId, cond, val);
                    }
                }
            }

            DialogResult = DialogResult.OK;
        }

        public void SetLang()
        {
            GKData.CondSigns[6] = LangMan.LS(LSID.LSID_CondContains);
            GKData.CondSigns[7] = LangMan.LS(LSID.LSID_CondNotContains);

            btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            btnReset.Text = LangMan.LS(LSID.LSID_DlgReset);
            tsFieldsFilter.Text = LangMan.LS(LSID.LSID_FieldsFilter);
        }

        public virtual void DoReset()
        {
            fListMan.Filter.Clear();
            UpdateGrid();
        }

        public bool ShowModalX()
        {
            return (ShowDialog() == DialogResult.OK);
        }
    }
}

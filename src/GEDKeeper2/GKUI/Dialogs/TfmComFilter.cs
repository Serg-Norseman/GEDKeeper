using System;
using System.Windows.Forms;

using GKCore;
using GKCore.Interfaces;
using GKCore.Lists;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public partial class TfmComFilter : Form
	{
        private readonly IBaseWindow fBase;
		private readonly string[] fFields;
		private readonly ListManager fListMan;

		public IBaseWindow Base
		{
			get { return this.fBase; }
		}

        public TfmComFilter(IBaseWindow aBase, ListManager listMan)
        {
            this.InitializeComponent();

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

        private ConditionKind GetCondByName(string condName)
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

		private void InitGrid()
		{
			this.dataGridView1.Rows.Clear();
			((System.ComponentModel.ISupportInitialize)(this.dataGridView1)).BeginInit();
			this.dataGridView1.Columns.AddRange(new System.Windows.Forms.DataGridViewColumn[] {
			                     	AddComboColumn("FField", LangMan.LS(LSID.LSID_Field), this.fFields, 200),
			                     	AddComboColumn("FCondition", LangMan.LS(LSID.LSID_Condition), GKData.CondSigns, 150),
			                     	AddTextColumn("FValue", LangMan.LS(LSID.LSID_Value), 300)});
			((System.ComponentModel.ISupportInitialize)(this.dataGridView1)).EndInit();
		}

		private void UpdateGrid()
		{
			this.dataGridView1.Rows.Clear();

			int num = fListMan.Filter.ColumnsFilter.Count;
			for (int i = 0; i < num; i++) {
				FilterCondition fcond = fListMan.Filter.ColumnsFilter[i];

				int r = this.dataGridView1.Rows.Add();
				DataGridViewRow row = dataGridView1.Rows[r];

				int condIndex = ((IConvertible)fcond.condition).ToByte(null);

				row.Cells[0].Value = this.fFields[fcond.col_index + 1];
				row.Cells[1].Value = GKData.CondSigns[condIndex];
				row.Cells[2].Value = fcond.value.ToString();
			}
		}

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
		
		void btnAccept_Click(object sender, EventArgs e)
		{
			try
			{
				this.AcceptChanges();
				base.DialogResult = DialogResult.OK;
			}
			catch (Exception ex)
			{
				this.fBase.Host.LogWrite("TfmComFilter.btnAccept_Click(): " + ex.Message);
				base.DialogResult = DialogResult.None;
			}
		}

        public void SetLang()
		{
            this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            this.btnReset.Text = LangMan.LS(LSID.LSID_DlgReset);
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

		public virtual void DoReset()
		{
			fListMan.Filter.Clear();
			this.UpdateGrid();
		}
		
		private void btnReset_Click(object sender, EventArgs e)
		{
			this.DoReset();
		}
	}
}

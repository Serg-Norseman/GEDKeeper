using System;
using System.Windows.Forms;

using GKCore.Interfaces;
using GKUI.Lists;

namespace GKUI.Dialogs
{
    /// <summary>
    /// Localization: dirty
    /// </summary>
    public partial class TfmComFilter : Form
	{
        private readonly IBase fBase;
        private readonly string[] fCondSigns;
		private readonly string[] fFields;
		private readonly ListManager fListMan;

		public IBase Base
		{
			get { return this.fBase; }
		}

        public TfmComFilter(IBase aBase, ListManager aListMan)
        {
            this.InitializeComponent();

            this.fBase = aBase;
            this.fListMan = aListMan;

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

            fCondSigns = new string[]
			{
				"!=", "<", "<=", "==", "=>", ">", "содержит", "не содержит"
			};

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

        private TConditionKind GetCondByName(string aName)
		{
			TConditionKind res = TConditionKind.ck_NotEq;

			for (TConditionKind pl = TConditionKind.ck_NotEq; pl <= TConditionKind.ck_NotContains; pl++)
			{
				if (fCondSigns[(int)pl] == aName)
				{
					res = pl;
					break;
				}
			}

			return res;
		}

		private Enum GetFieldColumn(string aField)
		{
			int idx = -1;
			for (int i = 0; i < fFields.Length; i++)
			{
				if (fFields[i] == aField)
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

			//LangMan.LS(LSID.LSID_Join)
			((System.ComponentModel.ISupportInitialize)(this.dataGridView1)).BeginInit();
			this.dataGridView1.Columns.AddRange(new System.Windows.Forms.DataGridViewColumn[] {
			                     	AddComboColumn("FField", "Поле", this.fFields, 200),
			                     	AddComboColumn("FCondition", "Условие", fCondSigns, 150),
			                     	AddTextColumn("FValue", "Значение", 300)});
			((System.ComponentModel.ISupportInitialize)(this.dataGridView1)).EndInit();
		}

		private void UpdateGrid()
		{
			this.dataGridView1.Rows.Clear();
			for (int i = 0; i < fListMan.Filter.ColumnsFilter.Count; i++) {
				TFilterCondition fcond = fListMan.Filter.ColumnsFilter[i];
				int r = this.dataGridView1.Rows.Add();
				DataGridViewRow row = dataGridView1.Rows[r];

				int condIndex = ((IConvertible)fcond.condition).ToByte(null);

				row.Cells[0].Value = this.fFields[fcond.col_index + 1];
				row.Cells[1].Value = this.fCondSigns[condIndex];
				row.Cells[2].Value = fcond.value.ToString();
			}
		}

		public virtual void AcceptChanges()
		{
			fListMan.Filter.Clear();

			for (int r = 0; r <= dataGridView1.Rows.Count - 1; r++)
			{
				DataGridViewRow row = dataGridView1.Rows[r];

				string fld = (string)row.Cells[0].Value;
				string cnd = (string)row.Cells[1].Value;
				string val = (string)row.Cells[2].Value;

				if (!string.IsNullOrEmpty(fld)) {
					TConditionKind cond = GetCondByName(cnd);
					Enum column = GetFieldColumn(fld);
					fListMan.AddCondition(column, cond, val);
				}
			}

			DialogResult = DialogResult.OK;
		}
		
		void btnAcceptClick(object sender, EventArgs e)
		{
			try
			{
				this.AcceptChanges();
				base.DialogResult = DialogResult.OK;
			}
			catch (Exception ex)
			{
				this.fBase.Host.LogWrite("TfmComFilter.btnAcceptClick(): " + ex.Message);
				base.DialogResult = DialogResult.None;
			}
		}

        public void SetLang()
		{
			/*this.btnParse.Text = LangMan.LS(LSID.101];
			this.btnClose.Text = LangMan.LS(LSID.99];
			this.Text = LangMan.LS(LSID.22];
			this.tsSimpleInput.Text = LangMan.LS(LSID.485];
			this.btnMale.Text = new string(LangMan.LS(LSID.66][0], 1);
			//this.btnFemale.Text = new string(LangMan.LS(LSID.67][0], 1);
			this.Label1.Text = LangMan.LS(LSID.301];
			this.CheckBirth.Text = LangMan.LS(LSID.321];
			this.Label3.Text = LangMan.LS(LSID.122];
			this.Label5.Text = LangMan.LS(LSID.302];
			this.CheckDeath.Text = LangMan.LS(LSID.332];
			this.Label6.Text = LangMan.LS(LSID.123];
			this.Label7.Text = LangMan.LS(LSID.303];
			this.Label2.Text = LangMan.LS(LSID.108];
			this.tsSourceInput.Text = LangMan.LS(LSID.486];
			this.rgSourceKind.Text = LangMan.LS(LSID.487];
			this.Label4.Text = LangMan.LS(LSID.109];
			this.Label8.Text = LangMan.LS(LSID.110];
			this.Label9.Text = LangMan.LS(LSID.490];
			this.Label10.Text = LangMan.LS(LSID.491];
			this.gbMetrics.Text = LangMan.LS(LSID.489];
			this.Label11.Text = LangMan.LS(LSID.492];
			this.Label12.Text = LangMan.LS(LSID.493];
			
			radioButton1.Text = LangMan.LS(LSID.LSID_SK_Rev);
			radioButton2.Text = LangMan.LS(LSID.LSID_SK_Met);*/
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
		
		void BtnResetClick(object sender, EventArgs e)
		{
			this.DoReset();
		}
	}
}

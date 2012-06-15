using System;
using System.Runtime.InteropServices;
using System.Windows.Forms;

using GKUI.Lists;

/// <summary>
/// Localization: dirty
/// </summary>

namespace GKUI
{
	public partial class TfmComFilter : Form
	{
		private string[] FCondSigns;
		private string[] FFields;

		private TfmBase FBase;
		private TListManager FListMan;

		public TfmBase Base
		{
			get { return this.FBase; }
		}

		private TConditionKind GetCondByName([In] string aName)
		{
			TConditionKind res = TConditionKind.ck_NotEq;

			for (TConditionKind pl = TConditionKind.ck_NotEq; pl <= TConditionKind.ck_Contains; pl++)
			{
				if (FCondSigns[(int)pl] == aName)
				{
					res = pl;
					break;
				}
			}

			return res;
		}

		private Enum GetFieldColumn([In] string aField)
		{
			int idx = -1;
			for (int i = 0; i < FFields.Length; i++)
			{
				if (FFields[i] == aField)
				{
					idx = i;
					break;
				}
			}

			idx = idx - 1; // exclude empty item
			Type colEnum = FListMan.GetColumnsEnum();
			Array enums = Enum.GetValues(colEnum);
			return (Enum)enums.GetValue(idx);
		}

		private void InitGrid()
		{
			this.dataGridView1.Rows.Clear();

			//LangMan.LS(LSID.LSID_Join)
			((System.ComponentModel.ISupportInitialize)(this.dataGridView1)).BeginInit();
			this.dataGridView1.Columns.AddRange(new System.Windows.Forms.DataGridViewColumn[] {
			                     	AddComboColumn("FField", "Поле", this.FFields, 200),
			                     	AddComboColumn("FCondition", "Условие", FCondSigns, 150),
			                     	AddTextColumn("FValue", "Значение", 300)});
			((System.ComponentModel.ISupportInitialize)(this.dataGridView1)).EndInit();
		}

		private void UpdateGrid()
		{
			this.dataGridView1.Rows.Clear();
			for (int i = 0; i < FListMan.ColumnsFilter.Count; i++) {
				TFilterCondition fcond = FListMan.ColumnsFilter[i];
				int r = this.dataGridView1.Rows.Add();
				DataGridViewRow row = dataGridView1.Rows[r];

				int cond_index =  ((IConvertible)fcond.condition).ToByte(null);

				row.Cells[0].Value = this.FFields[fcond.col_index];
				row.Cells[1].Value = this.FCondSigns[cond_index];
				row.Cells[2].Value = fcond.value.ToString();
			}
		}

		void btnAcceptClick(object sender, EventArgs e)
		{
			try
			{
				FListMan.ColumnsFilter.Clear();

				for (int r = 0; r <= dataGridView1.Rows.Count - 1; r++)
				{
					DataGridViewRow row = dataGridView1.Rows[r];

					string fld = (string)row.Cells[0].Value;
					string cnd = (string)row.Cells[1].Value;
					string val = (string)row.Cells[2].Value;

					if (!string.IsNullOrEmpty(fld)) {
						TConditionKind cond = GetCondByName(cnd);
						Enum column = GetFieldColumn(fld);
						FListMan.AddCondition(column, cond, val);
					}
				}

				DialogResult = DialogResult.OK;
			}
			finally
			{
			}
		}

		protected override void Dispose(bool Disposing)
		{
			if (Disposing)
			{
			}
			base.Dispose(Disposing);
		}

		public TfmComFilter(TfmBase aBase, TListManager aListMan)
		{
			this.InitializeComponent();

			this.FBase = aBase;
			this.FListMan = aListMan;

			Type colEnum = FListMan.GetColumnsEnum();
			Array enums = Enum.GetValues(colEnum);
			this.FFields = new string[enums.Length + 1];
			this.FFields[0] = "";
			int idx = 1;
			foreach (Enum e in enums)
			{
				this.FFields[idx] = FListMan.GetColumnName(e);
				idx++;
			}

			FCondSigns = new string[]
			{
				"!=", "<", "<=", "==", "=>", ">", "содержит"
			};

			this.InitGrid();
			this.UpdateGrid();

			this.SetLang();
		}

		public void SetLang()
		{
			/*this.btnParse.Text = LangMan.LSList[101];
			this.btnClose.Text = LangMan.LSList[99];
			this.Text = LangMan.LSList[22];
			this.tsSimpleInput.Text = LangMan.LSList[485];
			this.btnMale.Text = new string(LangMan.LSList[66][0], 1);
			//this.btnFemale.Text = new string(LangMan.LSList[67][0], 1);
			this.Label1.Text = LangMan.LSList[301];
			this.CheckBirth.Text = LangMan.LSList[321];
			this.Label3.Text = LangMan.LSList[122];
			this.Label5.Text = LangMan.LSList[302];
			this.CheckDeath.Text = LangMan.LSList[332];
			this.Label6.Text = LangMan.LSList[123];
			this.Label7.Text = LangMan.LSList[303];
			this.Label2.Text = LangMan.LSList[108];
			this.tsSourceInput.Text = LangMan.LSList[486];
			this.rgSourceKind.Text = LangMan.LSList[487];
			this.Label4.Text = LangMan.LSList[109];
			this.Label8.Text = LangMan.LSList[110];
			this.Label9.Text = LangMan.LSList[490];
			this.Label10.Text = LangMan.LSList[491];
			this.gbMetrics.Text = LangMan.LSList[489];
			this.Label11.Text = LangMan.LSList[492];
			this.Label12.Text = LangMan.LSList[493];
			
			radioButton1.Text = LangMan.LS(LSID.LSID_SK_Rev);
			radioButton2.Text = LangMan.LS(LSID.LSID_SK_Met);*/
		}

		private DataGridViewColumn AddTextColumn(string colName, string headerText, int width)
		{
			DataGridViewColumn col = new System.Windows.Forms.DataGridViewTextBoxColumn();
			col.HeaderText = headerText;
			col.Name = colName;
			col.Width = width;
			return col;
		}

		private DataGridViewColumn AddComboColumn(string colName, string headerText, object[] items, int width)
		{
			DataGridViewComboBoxColumn col = new System.Windows.Forms.DataGridViewComboBoxColumn();
			col.HeaderText = headerText;
			col.Name = colName;
			col.Width = width;
			col.Items.AddRange(items);
			return col;
		}

		void BtnResetClick(object sender, EventArgs e)
		{
			FListMan.ColumnsFilter.Clear();
			this.UpdateGrid();
		}
	}
}

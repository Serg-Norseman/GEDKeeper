using System;
using System.Windows.Forms;

using Ext.Utils;
using GedCom551;
using GKCore;
using GKUI.Controls;
using GKUI.Lists;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI
{
	public partial class TfmFilter : Form
	{
		private TfmBase FBase;

		public TfmBase Base
		{
			get { return this.FBase; }
		}

		private void rgLifeClick(object sender, EventArgs e)
		{
			this.edAliveBeforeDate.Enabled = this.RadioButton4.Checked;
		}

		private void btnCancel_Click(object sender, EventArgs e)
		{
			this.Base.Filter.Clear();
			this.Base.ApplyFilter();
		}

		private void btnAccept_Click(object sender, EventArgs e)
		{
			try
			{
				this.AcceptChanges();
				base.DialogResult = DialogResult.OK;
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("TfmFilter.Accept(): " + E.Message);
				base.DialogResult = DialogResult.None;
			}
		}

		private void AcceptChanges()
		{
			string fs = this.edName.Text.Trim();
			if (fs != "" && fs != "*" && GKUI.TfmGEDKeeper.Instance.Options.NameFilters.IndexOf(fs) < 0)
			{
				GKUI.TfmGEDKeeper.Instance.Options.NameFilters.Add(fs);
			}

			fs = this.cbResidence.Text.Trim();
			if (fs != "" && fs != "*" && GKUI.TfmGEDKeeper.Instance.Options.ResidenceFilters.IndexOf(fs) < 0)
			{
				GKUI.TfmGEDKeeper.Instance.Options.ResidenceFilters.Add(fs);
			}

			fs = this.cbEventVal.Text.Trim();
			if (fs != "" && fs != "*" && GKUI.TfmGEDKeeper.Instance.Options.EventFilters.IndexOf(fs) < 0)
			{
				GKUI.TfmGEDKeeper.Instance.Options.EventFilters.Add(fs);
			}

			this.Base.Filter.PatriarchOnly = this.CheckPatriarch.Checked;

			int life_sel = 0;
			if (this.RadioButton1.Checked) life_sel = 0;
			if (this.RadioButton2.Checked) life_sel = 1;
			if (this.RadioButton3.Checked) life_sel = 2;
			if (this.RadioButton4.Checked) life_sel = 3;

			if (this.Base.Filter.LifeMode != TGenEngine.TLifeMode.lmTimeLine)
			{
				this.Base.Filter.AliveBeforeDate = this.edAliveBeforeDate.Text;
				if (life_sel == 3)
				{
					try
					{
						DateTime dt = DateTime.Parse(this.edAliveBeforeDate.Text);
					}
					catch
					{
						TGenEngine.ShowError(LangMan.LSList[532]);
						base.DialogResult = DialogResult.None;
					}
				}
				this.Base.Filter.LifeMode = (TGenEngine.TLifeMode)life_sel;
			}

			int sex_sel = 0;
			if (this.RadioButton5.Checked) sex_sel = 0;
			if (this.RadioButton6.Checked) sex_sel = 1;
			if (this.RadioButton7.Checked) sex_sel = 2;
			this.Base.Filter.Sex = (TGEDCOMSex)sex_sel;

			if (this.edName.Text == "") this.edName.Text = "*";
			this.Base.Filter.Name = this.edName.Text;

			if (this.cbResidence.Text == "") this.cbResidence.Text = "*";
			this.Base.Filter.Residence = this.cbResidence.Text;

			if (this.cbEventVal.Text == "") this.cbEventVal.Text = "*";
			this.Base.Filter.EventVal = this.cbEventVal.Text;

			int selectedIndex = this.cbGroup.SelectedIndex;
			if (selectedIndex >= 0 && selectedIndex < 3) {
				this.Base.Filter.GroupMode = (CustomFilter.TGroupMode)this.cbGroup.SelectedIndex;
				this.Base.Filter.GroupRef = "";
			} else {
				TGEDCOMRecord rec = (this.cbGroup.Items[this.cbGroup.SelectedIndex] as GKComboItem).Data as TGEDCOMRecord;
				if (rec != null) {
					this.Base.Filter.GroupMode = CustomFilter.TGroupMode.gmSelected;
					this.Base.Filter.GroupRef = rec.XRef;
				} else {
					this.Base.Filter.GroupMode = CustomFilter.TGroupMode.gmAll;
					this.Base.Filter.GroupRef = "";
				}
			}

			int selectedIndex2 = this.cbSource.SelectedIndex;
			if (selectedIndex2 >= 0 && selectedIndex2 < 3) {
				this.Base.Filter.SourceMode = (CustomFilter.TGroupMode)this.cbSource.SelectedIndex;
				this.Base.Filter.SourceRef = "";
			} else {
				TGEDCOMRecord rec = (this.cbSource.Items[this.cbSource.SelectedIndex] as GKComboItem).Data as TGEDCOMRecord;
				if (rec != null) {
					this.Base.Filter.SourceMode = CustomFilter.TGroupMode.gmSelected;
					this.Base.Filter.SourceRef = rec.XRef;
				} else {
					this.Base.Filter.SourceMode = CustomFilter.TGroupMode.gmAll;
					this.Base.Filter.SourceRef = "";
				}
			}

			this.Base.ApplyFilter();
			base.DialogResult = DialogResult.OK;
		}

		private void TfmFilter_Load(object sender, EventArgs e)
		{
			this.edName.Items.AddRange(GKUI.TfmGEDKeeper.Instance.Options.NameFilters.ToArray());
			this.cbResidence.Items.AddRange(GKUI.TfmGEDKeeper.Instance.Options.ResidenceFilters.ToArray());
			this.cbEventVal.Items.AddRange(GKUI.TfmGEDKeeper.Instance.Options.EventFilters.ToArray());

			int life_sel;
			if (this.Base.Filter.LifeMode != TGenEngine.TLifeMode.lmTimeLine)
			{
				life_sel = (int)this.Base.Filter.LifeMode;
				this.rgLife.Enabled = true;
				this.edAliveBeforeDate.Text = this.Base.Filter.AliveBeforeDate;
			} else {
				life_sel = -1;
				this.rgLife.Enabled = false;
				this.edAliveBeforeDate.Text = "";
			}

			switch (life_sel) {
				case 0:
					this.RadioButton1.Checked = true;
					break;
				case 1:
					this.RadioButton2.Checked = true;
					break;
				case 2:
					this.RadioButton3.Checked = true;
					break;
				case 3:
					this.RadioButton4.Checked = true;
					break;
			}

			int sex_sel = (int)this.Base.Filter.Sex;
			switch (sex_sel) {
				case 0:
					this.RadioButton5.Checked = true;
					break;
				case 1:
					this.RadioButton6.Checked = true;
					break;
				case 2:
					this.RadioButton7.Checked = true;
					break;
			}

			this.edName.Text = this.Base.Filter.Name;
			this.cbResidence.Text = this.Base.Filter.Residence;
			this.cbEventVal.Text = this.Base.Filter.EventVal;
			this.CheckPatriarch.Checked = this.Base.Filter.PatriarchOnly;

			TGEDCOMTree tree = this.Base.Tree;

			this.cbGroup.Sorted = true;
			int num = tree.RecordsCount - 1;
			for (int i = 0; i <= num; i++) {
				TGEDCOMRecord rec = tree[i];
				if (rec is TGEDCOMGroupRecord) {
					this.cbGroup.Items.Add(new GKComboItem((rec as TGEDCOMGroupRecord).GroupName, rec));
				}
			}
			this.cbGroup.Sorted = false;
			this.cbGroup.Items.Insert(0, new GKComboItem(LangMan.LSList[500], null));
			this.cbGroup.Items.Insert(1, new GKComboItem(LangMan.LSList[501], null));
			this.cbGroup.Items.Insert(2, new GKComboItem(LangMan.LSList[502], null));
			if (this.Base.Filter.GroupMode != CustomFilter.TGroupMode.gmSelected)
			{
				this.cbGroup.SelectedIndex = (int)this.Base.Filter.GroupMode;
			} else {
				this.cbGroup.Text = ((TGEDCOMGroupRecord)(tree.XRefIndex_Find(this.Base.Filter.GroupRef))).GroupName;
			}

			this.cbSource.Sorted = true;
			for (int i = 0; i <= tree.RecordsCount - 1; i++) {
				TGEDCOMRecord rec = tree[i];
				if (tree[i] is TGEDCOMSourceRecord) {
					this.cbSource.Items.Add(new GKComboItem((rec as TGEDCOMSourceRecord).FiledByEntry, rec));
				}
			}
			this.cbSource.Sorted = false;
			this.cbSource.Items.Insert(0, new GKComboItem(LangMan.LSList[500], null));
			this.cbSource.Items.Insert(1, new GKComboItem(LangMan.LSList[501], null));
			this.cbSource.Items.Insert(2, new GKComboItem(LangMan.LSList[502], null));
			if (this.Base.Filter.SourceMode != CustomFilter.TGroupMode.gmSelected)
			{
				this.cbSource.SelectedIndex = (int)this.Base.Filter.SourceMode;
			} else {
				this.cbSource.Text = ((TGEDCOMSourceRecord)(tree.XRefIndex_Find(this.Base.Filter.SourceRef))).FiledByEntry;
			}
		}

		public TfmFilter(TfmBase aBase)
		{
			this.InitializeComponent();
			this.FBase = aBase;
			this.SetLang();
		}

		public void SetLang()
		{
			this.Text = LangMan.LS(LSID.LSID_MIFilter);
			this.btnAccept.Text = LangMan.LSList[97];
			this.btnCancel.Text = LangMan.LSList[98];
			this.RadioButton1.Text = LangMan.LSList[522];
			this.RadioButton2.Text = LangMan.LSList[523];
			this.RadioButton3.Text = LangMan.LSList[524];
			this.RadioButton4.Text = LangMan.LSList[525].ToLower();
			this.RadioButton5.Text = LangMan.LSList[522];
			this.RadioButton6.Text = LangMan.LSList[526];
			this.RadioButton7.Text = LangMan.LSList[527];
			this.Label2.Text = LangMan.LSList[525] + ":";
			this.Label1.Text = LangMan.LSList[528];
			this.Label3.Text = LangMan.LSList[529];
			this.Label6.Text = LangMan.LSList[530];
			this.Label4.Text = LangMan.LSList[58];
			this.Label5.Text = LangMan.LSList[56];
			this.CheckPatriarch.Text = LangMan.LSList[531];
		}
	}
}

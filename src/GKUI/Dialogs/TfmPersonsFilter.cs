using System;
using System.Windows.Forms;

using Ext.Utils;
using GedCom551;
using GKCore;
using GKUI.Controls;
using GKUI.Lists;

/// <summary>
/// Localization: dirty
/// </summary>

namespace GKUI
{
    public partial class TfmPersonsFilter : GKUI.TfmComFilter
    {
    	private TIndividualListMan FListMan;
    	
        public TfmPersonsFilter()
            : base()
        {
            InitializeComponent();
        }

        public TfmPersonsFilter(TfmBase aBase, TListManager aListMan)
            : base(aBase, aListMan)
        {
            InitializeComponent();
			this.SetSpecificLang();

            FListMan = (TIndividualListMan)aListMan;
            UpdateSpecific();
            PageControl1.SelectedIndex = 1;
        }

		public void SetSpecificLang()
		{
			this.Text = LangMan.LS(LSID.LSID_MIFilter);
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

		private void rgLifeClick(object sender, EventArgs e)
		{
			this.edAliveBeforeDate.Enabled = this.RadioButton4.Checked;
		}

		public override void DoReset()
		{
			base.DoReset();
			this.UpdateSpecific();
		}
		
        private void UpdateSpecific()
        {
        	TIndividualListFilter iFilter = (TIndividualListFilter)FListMan.Filter;
        	
        	this.edName.Items.Clear();
			this.edName.Items.AddRange(GKUI.TfmGEDKeeper.Instance.Options.NameFilters.ToArray());
			this.edName.Items.Insert(0, "*");

			this.cbResidence.Items.Clear();
			this.cbResidence.Items.AddRange(GKUI.TfmGEDKeeper.Instance.Options.ResidenceFilters.ToArray());
			this.cbResidence.Items.Insert(0, "*");

			this.cbEventVal.Items.Clear();
			this.cbEventVal.Items.AddRange(GKUI.TfmGEDKeeper.Instance.Options.EventFilters.ToArray());

			int life_sel;
			if (iFilter.LifeMode != TLifeMode.lmTimeLine)
			{
				life_sel = (int)iFilter.LifeMode;
				this.rgLife.Enabled = true;
				this.edAliveBeforeDate.Text = iFilter.AliveBeforeDate;
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

			int sex_sel = (int)iFilter.Sex;
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

			this.edName.Text = iFilter.Name;
			this.cbResidence.Text = iFilter.Residence;
			this.cbEventVal.Text = iFilter.EventVal;
			this.CheckPatriarch.Checked = iFilter.PatriarchOnly;

			TGEDCOMTree tree = this.Base.Tree;

			this.cbGroup.Items.Clear();
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
			if (iFilter.GroupMode != CustomFilter.TGroupMode.gmSelected)
			{
				this.cbGroup.SelectedIndex = (int)iFilter.GroupMode;
			} else {
				this.cbGroup.Text = (tree.XRefIndex_Find(iFilter.GroupRef) as TGEDCOMGroupRecord).GroupName;
			}

			this.cbSource.Items.Clear();
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
			if (iFilter.SourceMode != CustomFilter.TGroupMode.gmSelected)
			{
				this.cbSource.SelectedIndex = (int)iFilter.SourceMode;
			} else {
				this.cbSource.Text = (tree.XRefIndex_Find(iFilter.SourceRef) as TGEDCOMSourceRecord).FiledByEntry;
			}
        }
        
        private void SaveFilter(string flt, StringList filters)
        {
            if (flt != "" && flt != "*" && filters.IndexOf(flt) < 0) filters.Add(flt);
        }

		public override void AcceptChanges()
		{
			base.AcceptChanges();

			TIndividualListFilter iFilter = (TIndividualListFilter)FListMan.Filter;
			
			string fs = this.edName.Text.Trim();
            SaveFilter(fs, GKUI.TfmGEDKeeper.Instance.Options.NameFilters);

			fs = this.cbResidence.Text.Trim();
            SaveFilter(fs, GKUI.TfmGEDKeeper.Instance.Options.ResidenceFilters);

			fs = this.cbEventVal.Text.Trim();
            SaveFilter(fs, GKUI.TfmGEDKeeper.Instance.Options.EventFilters);

			iFilter.PatriarchOnly = this.CheckPatriarch.Checked;

			int life_sel = 0;
			if (this.RadioButton1.Checked) life_sel = 0;
			if (this.RadioButton2.Checked) life_sel = 1;
			if (this.RadioButton3.Checked) life_sel = 2;
			if (this.RadioButton4.Checked) life_sel = 3;

			if (iFilter.LifeMode != TLifeMode.lmTimeLine)
			{
				iFilter.AliveBeforeDate = this.edAliveBeforeDate.Text;
				if (life_sel == 3)
				{
					try
					{
						DateTime dt = DateTime.Parse(this.edAliveBeforeDate.Text);
					}
					catch
					{
						GKUtils.ShowError(LangMan.LSList[532]);
						base.DialogResult = DialogResult.None;
					}
				}
				iFilter.LifeMode = (TLifeMode)life_sel;
			}

			int sex_sel = 0;
			if (this.RadioButton5.Checked) sex_sel = 0;
			if (this.RadioButton6.Checked) sex_sel = 1;
			if (this.RadioButton7.Checked) sex_sel = 2;
			iFilter.Sex = (TGEDCOMSex)sex_sel;

			if (this.edName.Text == "") this.edName.Text = "*";
			iFilter.Name = this.edName.Text;

			if (this.cbResidence.Text == "") this.cbResidence.Text = "*";
			iFilter.Residence = this.cbResidence.Text;

			if (this.cbEventVal.Text == "") this.cbEventVal.Text = "*";
			iFilter.EventVal = this.cbEventVal.Text;

			int selectedIndex = this.cbGroup.SelectedIndex;
			if (selectedIndex >= 0 && selectedIndex < 3) {
				iFilter.GroupMode = (CustomFilter.TGroupMode)this.cbGroup.SelectedIndex;
				iFilter.GroupRef = "";
			} else {
				TGEDCOMRecord rec = (this.cbGroup.Items[this.cbGroup.SelectedIndex] as GKComboItem).Data as TGEDCOMRecord;
				if (rec != null) {
					iFilter.GroupMode = CustomFilter.TGroupMode.gmSelected;
					iFilter.GroupRef = rec.XRef;
				} else {
					iFilter.GroupMode = CustomFilter.TGroupMode.gmAll;
					iFilter.GroupRef = "";
				}
			}

			int selectedIndex2 = this.cbSource.SelectedIndex;
			if (selectedIndex2 >= 0 && selectedIndex2 < 3) {
				iFilter.SourceMode = (CustomFilter.TGroupMode)this.cbSource.SelectedIndex;
				iFilter.SourceRef = "";
			} else {
				TGEDCOMRecord rec = (this.cbSource.Items[this.cbSource.SelectedIndex] as GKComboItem).Data as TGEDCOMRecord;
				if (rec != null) {
					iFilter.SourceMode = CustomFilter.TGroupMode.gmSelected;
					iFilter.SourceRef = rec.XRef;
				} else {
					iFilter.SourceMode = CustomFilter.TGroupMode.gmAll;
					iFilter.SourceRef = "";
				}
			}
		}
    }
}

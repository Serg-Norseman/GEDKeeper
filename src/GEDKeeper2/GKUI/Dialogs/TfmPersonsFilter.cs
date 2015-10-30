using System;
using System.Windows.Forms;

using GKCommon;
using GKCommon.GEDCOM;
using GKCommon.GEDCOM.Enums;
using GKCore;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Options;
using GKCore.Types;
using GKUI.Controls;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public partial class TfmPersonsFilter : TfmComFilter
    {
    	private readonly IndividualListMan fListMan;
    	
        public TfmPersonsFilter(IBaseWindow aBase, ListManager aListMan) : base(aBase, aListMan)
        {
            InitializeComponent();

            this.SetSpecificLang();

            fListMan = (IndividualListMan)aListMan;
            UpdateSpecific();
            PageControl1.SelectedIndex = 1;
        }

		public void SetSpecificLang()
		{
			this.Text = LangMan.LS(LSID.LSID_MIFilter);
			this.RadioButton1.Text = LangMan.LS(LSID.LSID_All);
			this.RadioButton2.Text = LangMan.LS(LSID.LSID_OnlyAlive);
			this.RadioButton3.Text = LangMan.LS(LSID.LSID_OnlyDied);
			this.RadioButton4.Text = LangMan.LS(LSID.LSID_AliveBefore).ToLower();
			this.RadioButton5.Text = LangMan.LS(LSID.LSID_All);
			this.RadioButton6.Text = LangMan.LS(LSID.LSID_OnlyMans);
			this.RadioButton7.Text = LangMan.LS(LSID.LSID_OnlyWomans);
			this.Label2.Text = LangMan.LS(LSID.LSID_AliveBefore) + ":";
			this.Label1.Text = LangMan.LS(LSID.LSID_NameMask);
			this.Label3.Text = LangMan.LS(LSID.LSID_PlaceMask);
			this.Label6.Text = LangMan.LS(LSID.LSID_EventMask);
			this.Label4.Text = LangMan.LS(LSID.LSID_RPGroups);
			this.Label5.Text = LangMan.LS(LSID.LSID_RPSources);
			this.CheckPatriarch.Text = LangMan.LS(LSID.LSID_OnlyPatriarchs);
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
        	IndividualListFilter iFilter = (IndividualListFilter)fListMan.Filter;
            GlobalOptions options = TfmGEDKeeper.Instance.Options;
        	
        	this.edName.Items.Clear();
            this.edName.Items.AddRange(options.NameFilters.ToArray());
			this.edName.Items.Insert(0, "*");

			this.cbResidence.Items.Clear();
            this.cbResidence.Items.AddRange(options.ResidenceFilters.ToArray());
			this.cbResidence.Items.Insert(0, "*");

			this.cbEventVal.Items.Clear();
            this.cbEventVal.Items.AddRange(options.EventFilters.ToArray());

			int lifeSel;
			if (iFilter.FilterLifeMode != FilterLifeMode.lmTimeLocked)
			{
				lifeSel = (int)iFilter.FilterLifeMode;
				this.rgLife.Enabled = true;
				this.edAliveBeforeDate.Text = iFilter.AliveBeforeDate;
			} else {
				lifeSel = -1;
				this.rgLife.Enabled = false;
				this.edAliveBeforeDate.Text = "";
			}

			switch (lifeSel) {
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

			int sexSel = (int)iFilter.Sex;
			switch (sexSel) {
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

			GEDCOMTree tree = this.Base.Tree;

			this.cbGroup.Items.Clear();
			this.cbGroup.Sorted = true;
			int num = tree.RecordsCount - 1;
			for (int i = 0; i <= num; i++) {
				GEDCOMRecord rec = tree[i];
				if (rec is GEDCOMGroupRecord) {
					this.cbGroup.Items.Add(new GKComboItem((rec as GEDCOMGroupRecord).GroupName, rec));
				}
			}
			this.cbGroup.Sorted = false;
			this.cbGroup.Items.Insert(0, new GKComboItem(LangMan.LS(LSID.LSID_SrcAll), null));
			this.cbGroup.Items.Insert(1, new GKComboItem(LangMan.LS(LSID.LSID_SrcNot), null));
			this.cbGroup.Items.Insert(2, new GKComboItem(LangMan.LS(LSID.LSID_SrcAny), null));
			if (iFilter.FilterGroupMode != FilterGroupMode.gmSelected) {
				this.cbGroup.SelectedIndex = (int)iFilter.FilterGroupMode;
			} else {
			    GEDCOMGroupRecord groupRec = tree.XRefIndex_Find(iFilter.GroupRef) as GEDCOMGroupRecord;
                this.cbGroup.Text = groupRec.GroupName;
			}

			this.cbSource.Items.Clear();
			this.cbSource.Sorted = true;
			for (int i = 0; i <= tree.RecordsCount - 1; i++) {
				GEDCOMRecord rec = tree[i];
				if (rec is GEDCOMSourceRecord) {
					this.cbSource.Items.Add(new GKComboItem((rec as GEDCOMSourceRecord).FiledByEntry, rec));
				}
			}
			this.cbSource.Sorted = false;
			this.cbSource.Items.Insert(0, new GKComboItem(LangMan.LS(LSID.LSID_SrcAll), null));
			this.cbSource.Items.Insert(1, new GKComboItem(LangMan.LS(LSID.LSID_SrcNot), null));
			this.cbSource.Items.Insert(2, new GKComboItem(LangMan.LS(LSID.LSID_SrcAny), null));
			if (iFilter.SourceMode != FilterGroupMode.gmSelected) {
				this.cbSource.SelectedIndex = (int)iFilter.SourceMode;
			} else {
			    GEDCOMSourceRecord sourceRec = tree.XRefIndex_Find(iFilter.SourceRef) as GEDCOMSourceRecord;
                this.cbSource.Text = sourceRec.FiledByEntry;
			}
        }
        
        private static void SaveFilter(string flt, StringList filters)
        {
            if (flt != "" && flt != "*" && filters.IndexOf(flt) < 0) filters.Add(flt);
        }

		public override void AcceptChanges()
		{
			base.AcceptChanges();

			IndividualListFilter iFilter = (IndividualListFilter)fListMan.Filter;
			
			string fs = this.edName.Text.Trim();
            SaveFilter(fs, TfmGEDKeeper.Instance.Options.NameFilters);

			fs = this.cbResidence.Text.Trim();
            SaveFilter(fs, TfmGEDKeeper.Instance.Options.ResidenceFilters);

			fs = this.cbEventVal.Text.Trim();
            SaveFilter(fs, TfmGEDKeeper.Instance.Options.EventFilters);

			iFilter.PatriarchOnly = this.CheckPatriarch.Checked;

			int lifeSel = 0;
			if (this.RadioButton1.Checked) lifeSel = 0;
			if (this.RadioButton2.Checked) lifeSel = 1;
			if (this.RadioButton3.Checked) lifeSel = 2;
			if (this.RadioButton4.Checked) lifeSel = 3;

			if (iFilter.FilterLifeMode != FilterLifeMode.lmTimeLocked)
			{
				iFilter.AliveBeforeDate = this.edAliveBeforeDate.Text;
				if (lifeSel == 3)
				{
					try
					{
						/*DateTime dt = */
                        DateTime.Parse(this.edAliveBeforeDate.Text);
					}
					catch
					{
						GKUtils.ShowError(LangMan.LS(LSID.LSID_DateInvalid));
						base.DialogResult = DialogResult.None;
					}
				}
				iFilter.FilterLifeMode = (FilterLifeMode)lifeSel;
			}

			int sexSel = 0;
			if (this.RadioButton5.Checked) sexSel = 0;
			if (this.RadioButton6.Checked) sexSel = 1;
			if (this.RadioButton7.Checked) sexSel = 2;
			iFilter.Sex = (GEDCOMSex)sexSel;

			if (this.edName.Text == "") this.edName.Text = "*";
			iFilter.Name = this.edName.Text;

			if (this.cbResidence.Text == "") this.cbResidence.Text = "*";
			iFilter.Residence = this.cbResidence.Text;

			if (this.cbEventVal.Text == "") this.cbEventVal.Text = "*";
			iFilter.EventVal = this.cbEventVal.Text;

			int selectedIndex = this.cbGroup.SelectedIndex;
			if (selectedIndex >= 0 && selectedIndex < 3) {
				iFilter.FilterGroupMode = (FilterGroupMode)this.cbGroup.SelectedIndex;
				iFilter.GroupRef = "";
			} else {
			    GKComboItem item = this.cbGroup.Items[this.cbGroup.SelectedIndex] as GKComboItem;
				GEDCOMRecord rec = item.Data as GEDCOMRecord;
				if (rec != null) {
					iFilter.FilterGroupMode = FilterGroupMode.gmSelected;
					iFilter.GroupRef = rec.XRef;
				} else {
					iFilter.FilterGroupMode = FilterGroupMode.gmAll;
					iFilter.GroupRef = "";
				}
			}

			int selectedIndex2 = this.cbSource.SelectedIndex;
			if (selectedIndex2 >= 0 && selectedIndex2 < 3) {
				iFilter.SourceMode = (FilterGroupMode)this.cbSource.SelectedIndex;
				iFilter.SourceRef = "";
			} else {
			    GKComboItem item = this.cbSource.Items[this.cbSource.SelectedIndex] as GKComboItem;
				GEDCOMRecord rec = item.Data as GEDCOMRecord;
				if (rec != null) {
					iFilter.SourceMode = FilterGroupMode.gmSelected;
					iFilter.SourceRef = rec.XRef;
				} else {
					iFilter.SourceMode = FilterGroupMode.gmAll;
					iFilter.SourceRef = "";
				}
			}
		}
    }
}

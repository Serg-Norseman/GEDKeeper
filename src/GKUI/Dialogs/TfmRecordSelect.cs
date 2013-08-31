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
	public partial class TfmRecordSelect : Form
	{
		private TfmBase FBase;
		private TGEDCOMRecordType FMode;
		private string FFilter;
		private TTargetMode FTargetMode;
		public TGEDCOMIndividualRecord FTarget;
		public TGEDCOMSex FNeedSex;
		public TGEDCOMRecord ResultRecord;
		public GKRecordsView ListRecords;

		public TfmBase Base
		{
			get { return this.FBase; }
		}

		public string Filter
		{
			get { return this.FFilter; }
			set { this.SetFilter(value); }
		}

		public TGEDCOMRecordType Mode
		{
			get { return this.FMode; }
			set { this.SetMode(value); }
		}

		public TTargetMode TargetMode
		{
			get { return this.FTargetMode; }
			set { this.SetTargetMode(value); }
		}

		private void SetMode(TGEDCOMRecordType Value)
		{
			this.FMode = Value;
			this.DataRefresh();
		}

		private void DataRefresh()
		{
			if (this.ListRecords != null)
			{
				this.ListRecords.Dispose();
				this.ListRecords = null;
			}
			this.Base.CreateRecordsView(this.panList, this.FMode, ref this.ListRecords);

			this.ListRecords.ListMan.Filter.Clear();
			this.ListRecords.ListMan.QuickFilter = this.FFilter;

			if (this.FMode == TGEDCOMRecordType.rtIndividual) {
				TIndividualListFilter iFilter = (TIndividualListFilter)this.ListRecords.ListMan.Filter;
				iFilter.Sex = this.FNeedSex;
				iFilter.ChildSelector = (this.FTargetMode == TTargetMode.tmParent);
			}

			this.ListRecords.UpdateContents(this.Base.ShieldState, true, 1);
		}

		private void SetFilter(string Value)
		{
			this.FFilter = Value;
			if (this.FFilter == "") {
				this.FFilter = "*";
			} else if (this.FFilter != "*") {
				this.FFilter = "*" + this.FFilter + "*";
			}
			this.DataRefresh();
		}

		private void SetTargetMode(TTargetMode Value)
		{
			this.FTargetMode = Value;
		}

		private void btnSelect_Click(object sender, EventArgs e)
		{
			try
			{
				this.ResultRecord = this.ListRecords.GetSelectedRecord();
				base.DialogResult = DialogResult.OK;
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("TfmRecordSelect.Select(): " + E.Message);
				this.ResultRecord = null;
				base.DialogResult = DialogResult.None;
			}
		}

		private void btnCreate_Click(object sender, EventArgs e)
		{
			try
			{
				switch (this.FMode) {
					case TGEDCOMRecordType.rtIndividual:
						{
							TGEDCOMIndividualRecord iRec = this.Base.CreatePersonDialog(this.FTarget, this.FTargetMode, this.FNeedSex);
							if (iRec != null)
							{
								this.ResultRecord = iRec;
								base.DialogResult = DialogResult.OK;
							}
							break;
						}

					case TGEDCOMRecordType.rtFamily:
						{
							TGEDCOMFamilyRecord famRec = null;
							TFamilyTarget fam_target;
							if (this.FTargetMode == TTargetMode.tmChildToFamily)
							{
								fam_target = TFamilyTarget.ftChild;
							}
							else
							{
								fam_target = TFamilyTarget.ftNone;
							}
							if (this.Base.ModifyFamily(ref famRec, fam_target, this.FTarget))
							{
								this.ResultRecord = famRec;
								base.DialogResult = DialogResult.OK;
							}
							break;
						}

					case TGEDCOMRecordType.rtNote:
						{
							TGEDCOMNoteRecord noteRec = null;
							if (this.Base.ModifyNote(ref noteRec))
							{
								this.ResultRecord = noteRec;
								base.DialogResult = DialogResult.OK;
							}
							break;
						}

					case TGEDCOMRecordType.rtMultimedia:
						{
							TGEDCOMMultimediaRecord mmRec = null;
							if (this.Base.ModifyMedia(ref mmRec))
							{
								this.ResultRecord = mmRec;
								base.DialogResult = DialogResult.OK;
							}
							break;
						}

					case TGEDCOMRecordType.rtSource:
						{
							TGEDCOMSourceRecord sourceRec = null;
							if (this.Base.ModifySource(ref sourceRec))
							{
								this.ResultRecord = sourceRec;
								base.DialogResult = DialogResult.OK;
							}
							break;
						}

					case TGEDCOMRecordType.rtRepository:
						{
							TGEDCOMRepositoryRecord repRec = null;
							if (this.Base.ModifyRepository(ref repRec))
							{
								this.ResultRecord = repRec;
								base.DialogResult = DialogResult.OK;
							}
							break;
						}

					case TGEDCOMRecordType.rtGroup:
						{
							TGEDCOMGroupRecord groupRec = null;
							if (this.Base.ModifyGroup(ref groupRec))
							{
								this.ResultRecord = groupRec;
								base.DialogResult = DialogResult.OK;
							}
							break;
						}

					case TGEDCOMRecordType.rtTask:
						{
							TGEDCOMTaskRecord taskRec = null;
							if (this.Base.ModifyTask(ref taskRec))
							{
								this.ResultRecord = taskRec;
								base.DialogResult = DialogResult.OK;
							}
							break;
						}

					case TGEDCOMRecordType.rtCommunication:
						{
							TGEDCOMCommunicationRecord corrRec = null;
							if (this.Base.ModifyCommunication(ref corrRec))
							{
								this.ResultRecord = corrRec;
								base.DialogResult = DialogResult.OK;
							}
							break;
						}

					case TGEDCOMRecordType.rtLocation:
						{
							TGEDCOMLocationRecord locRec = null;
							if (this.Base.ModifyLocation(ref locRec))
							{
								this.ResultRecord = locRec;
								base.DialogResult = DialogResult.OK;
							}
							break;
						}
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("TfmRecordSelect.Create(): " + E.Message);
				this.ResultRecord = null;
				base.DialogResult = DialogResult.None;
			}
		}

		private void edFastFilter_TextChanged(object sender, EventArgs e)
		{
			this.SetFilter(this.edFastFilter.Text);
		}

		protected override void Dispose(bool Disposing)
		{
			if (Disposing)
			{
			}
			base.Dispose(Disposing);
		}

		public TfmRecordSelect(TfmBase aBase)
		{
			this.InitializeComponent();
			this.FBase = aBase;
			this.FFilter = "*";
			this.Text = LangMan.LSList[105];
			this.btnCreate.Text = LangMan.LSList[101];
			this.btnSelect.Text = LangMan.LSList[100];
			this.btnCancel.Text = LangMan.LSList[98];
		}
	}
}

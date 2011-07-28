using GedCom551;
using GKCore;
using GKSys;
using GKUI.Lists;
using System;
using System.ComponentModel;
using System.Drawing;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Windows.Forms;

namespace GKUI
{
	public class TfmRecordSelect : Form
	{
		private Button btnSelect;
		private Button btnCreate;
		private Button btnCancel;
		private Panel panList;
		private Panel panFilter;
		private TfmBase FBase;
		private TGEDCOMRecord.TGEDCOMRecordType FMode;
		private string FFilter;
		private TGenEngine.TTargetMode FTargetMode;
		private TPersonsFilter FLocalFilter;
		public TextBox edFastFilter;
		public TGEDCOMIndividualRecord FTarget;
		public TGEDCOMObject.TGEDCOMSex FNeedSex;
		public TGEDCOMRecord ResultRecord;
		public TRecordsView ListRecords;

		[Browsable(false)]
		public TfmBase Base
		{
			get
			{
				return this.FBase;
			}
		}
		[Browsable(false)]
		public string Filter
		{
			get
			{
				return this.FFilter;
			}
			set
			{
				this.SetFilter(value);
			}
		}
		[Browsable(false)]
		public TGEDCOMRecord.TGEDCOMRecordType Mode
		{
			get
			{
				return this.FMode;
			}
			set
			{
				this.SetMode(value);
			}
		}
		[Browsable(false)]
		public TGenEngine.TTargetMode TargetMode
		{
			get
			{
				return this.FTargetMode;
			}
			set
			{
				this.SetTargetMode(value);
			}
		}
		private void SetMode([In] TGEDCOMRecord.TGEDCOMRecordType Value)
		{
			this.FMode = Value;
			this.DataRefresh();
		}
		private void DataRefresh()
		{
			this.FLocalFilter.Clear();
			this.FLocalFilter.Name = this.FFilter;
			this.FLocalFilter.Sex = this.FNeedSex;
			if (this.ListRecords != null)
			{
				this.ListRecords.Dispose();
				this.ListRecords = null;
			}
			this.Base.CreateRecordsView(this.panList, this.FMode, ref this.ListRecords);
			this.ListRecords.UpdateContents(this.Base.ShieldState, true, this.FLocalFilter, 1);
		}
		private void SetFilter([In] string Value)
		{
			this.FFilter = Value;
			if (BDSSystem.WStrCmp(this.FFilter, "") == 0)
			{
				this.FFilter = "*";
			}
			else
			{
				if (BDSSystem.WStrCmp(this.FFilter, "*") != 0)
				{
					this.FFilter = "*" + this.FFilter + "*";
				}
			}
			this.DataRefresh();
		}
		private void SetTargetMode([In] TGenEngine.TTargetMode Value)
		{
			this.FTargetMode = Value;
			this.FLocalFilter.ChildSelector = (this.FTargetMode == TGenEngine.TTargetMode.tmAncestor);
		}
		private void InitializeComponent()
		{
			this.btnSelect = new Button();
			this.btnCreate = new Button();
			this.btnCancel = new Button();
			this.panList = new Panel();
			this.panFilter = new Panel();
			this.edFastFilter = new TextBox();
			this.panFilter.SuspendLayout();
			base.SuspendLayout();
			this.btnSelect.Location = new Point(200, 384);
			this.btnSelect.Name = "btnSelect";
			this.btnSelect.Size = new Size(81, 25);
			this.btnSelect.TabIndex = 3;
			this.btnSelect.Text = "Выбрать";
			this.btnSelect.Click += new EventHandler(this.btnSelect_Click);
			this.btnCreate.Location = new Point(104, 384);
			this.btnCreate.Name = "btnCreate";
			this.btnCreate.Size = new Size(81, 25);
			this.btnCreate.TabIndex = 2;
			this.btnCreate.Text = "Добавить";
			this.btnCreate.Click += new EventHandler(this.btnCreate_Click);
			this.btnCancel.DialogResult = DialogResult.Cancel;
			this.btnCancel.Location = new Point(296, 384);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new Size(81, 25);
			this.btnCancel.TabIndex = 4;
			this.btnCancel.Text = "Отменить";
			this.panList.Location = new Point(0, 41);
			this.panList.Name = "panList";
			this.panList.Size = new Size(385, 329);
			this.panList.TabIndex = 1;
			this.panFilter.Controls.Add(this.edFastFilter);
			this.panFilter.Location = new Point(0, 0);
			this.panFilter.Name = "panFilter";
			this.panFilter.Size = new Size(385, 41);
			this.panFilter.TabIndex = 0;
			this.edFastFilter.Location = new Point(8, 8);
			this.edFastFilter.Name = "edFastFilter";
			this.edFastFilter.Size = new Size(361, 21);
			this.edFastFilter.TabIndex = 0;
			this.edFastFilter.Text = "";
			this.edFastFilter.TextChanged += new EventHandler(this.edFastFilter_TextChanged);
			this.AutoScaleBaseSize = new Size(5, 14);
			base.CancelButton = this.btnCancel;
			base.ClientSize = new Size(385, 417);
			base.Controls.Add(this.panFilter);
			base.Controls.Add(this.panList);
			base.Controls.Add(this.btnSelect);
			base.Controls.Add(this.btnCreate);
			base.Controls.Add(this.btnCancel);
			this.Font = new Font("Tahoma", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 204);
			base.FormBorderStyle = FormBorderStyle.FixedDialog;
			base.KeyPreview = true;
			base.MaximizeBox = false;
			base.MinimizeBox = false;
			base.Name = "TfmRecordSelect";
			base.ShowInTaskbar = false;
			base.StartPosition = FormStartPosition.CenterScreen;
			this.Text = "Выбор записи";
			this.panFilter.ResumeLayout(false);
			base.ResumeLayout(false);
		}
		private void btnSelect_Click(object sender, EventArgs e)
		{
			this.ResultRecord = this.ListRecords.GetSelectedRecord();
			base.DialogResult = DialogResult.OK;
		}
		private void btnCreate_Click(object sender, EventArgs e)
		{
			switch (this.FMode)
			{
				case TGEDCOMRecord.TGEDCOMRecordType.rtIndividual:
				{
					TGEDCOMIndividualRecord iRec = this.Base.CreatePersonDialog(this.FTarget, this.FTargetMode, this.FNeedSex);
					if (iRec != null)
					{
						this.ResultRecord = iRec;
						base.DialogResult = DialogResult.OK;
					}
					break;
				}
				case TGEDCOMRecord.TGEDCOMRecordType.rtFamily:
				{
					TGEDCOMFamilyRecord famRec = null;
					TGenEngine.TFamilyTarget fam_target;
					if (this.FTargetMode == TGenEngine.TTargetMode.tmChildToFamily)
					{
						fam_target = TGenEngine.TFamilyTarget.ftChild;
					}
					else
					{
						fam_target = TGenEngine.TFamilyTarget.ftNone;
					}
					if (this.Base.ModifyFamily(ref famRec, fam_target, this.FTarget))
					{
						this.ResultRecord = famRec;
						base.DialogResult = DialogResult.OK;
					}
					break;
				}
				case TGEDCOMRecord.TGEDCOMRecordType.rtNote:
				{
					TGEDCOMNoteRecord noteRec = null;
					if (this.Base.ModifyNote(ref noteRec))
					{
						this.ResultRecord = noteRec;
						base.DialogResult = DialogResult.OK;
					}
					break;
				}
				case TGEDCOMRecord.TGEDCOMRecordType.rtMultimedia:
				{
					TGEDCOMMultimediaRecord mmRec = null;
					if (this.Base.ModifyMedia(ref mmRec))
					{
						this.ResultRecord = mmRec;
						base.DialogResult = DialogResult.OK;
					}
					break;
				}
				case TGEDCOMRecord.TGEDCOMRecordType.rtSource:
				{
					TGEDCOMSourceRecord sourceRec = null;
					if (this.Base.ModifySource(ref sourceRec))
					{
						this.ResultRecord = sourceRec;
						base.DialogResult = DialogResult.OK;
					}
					break;
				}
				case TGEDCOMRecord.TGEDCOMRecordType.rtRepository:
				{
					TGEDCOMRepositoryRecord repRec = null;
					if (this.Base.ModifyRepository(ref repRec))
					{
						this.ResultRecord = repRec;
						base.DialogResult = DialogResult.OK;
					}
					break;
				}
				case TGEDCOMRecord.TGEDCOMRecordType.rtGroup:
				{
					TGEDCOMGroupRecord groupRec = null;
					if (this.Base.ModifyGroup(ref groupRec))
					{
						this.ResultRecord = groupRec;
						base.DialogResult = DialogResult.OK;
					}
					break;
				}
				case TGEDCOMRecord.TGEDCOMRecordType.rtTask:
				{
					TGEDCOMTaskRecord taskRec = null;
					if (this.Base.ModifyTask(ref taskRec))
					{
						this.ResultRecord = taskRec;
						base.DialogResult = DialogResult.OK;
					}
					break;
				}
				case TGEDCOMRecord.TGEDCOMRecordType.rtCommunication:
				{
					TGEDCOMCommunicationRecord corrRec = null;
					if (this.Base.ModifyCommunication(ref corrRec))
					{
						this.ResultRecord = corrRec;
						base.DialogResult = DialogResult.OK;
					}
					break;
				}
				case TGEDCOMRecord.TGEDCOMRecordType.rtLocation:
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
		private void edFastFilter_TextChanged(object sender, EventArgs e)
		{
			this.SetFilter(this.edFastFilter.Text);
		}

		protected override void Dispose(bool Disposing)
		{
			if (Disposing)
			{
				this.FLocalFilter.Free();
			}
			base.Dispose(Disposing);
		}

		public TfmRecordSelect(TfmBase aBase)
		{
			this.InitializeComponent();
			this.FBase = aBase;
			this.FLocalFilter = new TPersonsFilter();
			this.FLocalFilter.List = TPersonsFilter.TListFilterMode.flSelector;
			this.FFilter = "*";
			this.Text = GKL.LSList[105];
			this.btnCreate.Text = GKL.LSList[101];
			this.btnSelect.Text = GKL.LSList[100];
			this.btnCancel.Text = GKL.LSList[98];
		}
	}
}

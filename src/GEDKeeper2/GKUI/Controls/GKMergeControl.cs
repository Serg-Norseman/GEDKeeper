using System;
using System.Drawing;
using System.Windows.Forms;
using GKCommon.Controls;
using GKCommon.GEDCOM;
using GKCommon.GEDCOM.Enums;
using GKCore;
using GKCore.Interfaces;

namespace GKUI.Controls
{
	public partial class GKMergeControl : UserControl
	{
		private GEDCOMRecord fRec1;
		private GEDCOMRecord fRec2;

		private readonly HyperView Memo1;
		private readonly HyperView Memo2;

		private IBase fBase;
		private GEDCOMTree fTree;
		private GEDCOMRecordType fMergeMode;

		public IBase Base
		{
			get	{
				return this.fBase;
			}
			set
			{
			    this.fBase = value;
			    this.fTree = (value != null) ? this.fBase.Tree : null;
			}
		}

		public GEDCOMRecordType MergeMode
		{
			get { return this.fMergeMode; }
			set { this.fMergeMode = value; }
		}

		public GEDCOMRecord Rec1
		{
			get { return this.fRec1; }
		}

		public GEDCOMRecord Rec2
		{
			get { return this.fRec2; }
		}

		public GKMergeControl()
		{
			InitializeComponent();

			this.Memo1 = new HyperView();
			this.Memo1.Location = new Point(8, 56);
			this.Memo1.Size = new Size(329, 248);
			this.Controls.Add(this.Memo1);

			this.Memo2 = new HyperView();
			this.Memo2.Location = new Point(344, 56);
			this.Memo2.Size = new Size(329, 248);
			this.Controls.Add(this.Memo2);

			this.SetRec1(null);
			this.SetRec2(null);

			this.btnRec1Select.Text = LangMan.LS(LSID.LSID_DlgSelect) + "...";
			this.btnRec2Select.Text = LangMan.LS(LSID.LSID_DlgSelect) + "...";
		}


		private void RecordMerge(GEDCOMRecord targetRec, GEDCOMRecord aRecCopy)
		{
			XRefReplacer repMap = new XRefReplacer();
			try
			{
				repMap.AddXRef(aRecCopy, aRecCopy.XRef, targetRec.XRef);

				int num = this.fTree.RecordsCount;
				for (int i = 0; i < num; i++) {
					this.fTree[i].ReplaceXRefs(repMap);
				}

				switch (targetRec.RecordType)
				{
					case GEDCOMRecordType.rtIndividual:
                        GEDCOMIndividualRecord indRec = (aRecCopy as GEDCOMIndividualRecord);
                        indRec.MoveTo(targetRec, false);
                        this.Base.RecordDelete(indRec, false);
						break;

					case GEDCOMRecordType.rtNote:
                        GEDCOMNoteRecord noteRec = (aRecCopy as GEDCOMNoteRecord);
                        noteRec.MoveTo(targetRec, false);
                        this.Base.RecordDelete(noteRec, false);
						break;

					case GEDCOMRecordType.rtFamily:
                        GEDCOMFamilyRecord famRec = (aRecCopy as GEDCOMFamilyRecord);
                        famRec.MoveTo(targetRec, false);
                        this.Base.RecordDelete(famRec, false);
						break;

					case GEDCOMRecordType.rtSource:
                        GEDCOMSourceRecord srcRec = (aRecCopy as GEDCOMSourceRecord);
                        srcRec.MoveTo(targetRec, false);
                        this.Base.RecordDelete(srcRec, false);
						break;
				}

				this.Base.ChangeRecord(targetRec);

				this.Base.RefreshLists(false);
			}
			finally
			{
                repMap.Dispose();
			}
		}

		private void UpdateMergeButtons()
		{
			this.btnMergeToLeft.Enabled = (this.fRec1 != null && this.fRec2 != null);
			this.btnMergeToRight.Enabled = (this.fRec1 != null && this.fRec2 != null);
		}

		public void SetRec1(GEDCOMRecord value)
		{
			this.fRec1 = value;
			this.UpdateMergeButtons();

			if (this.fRec1 == null)
			{
				this.Lab1.Text = "XXX1";
				this.Edit1.Text = "";
				this.Memo1.Lines.Clear();
			}
			else
			{
				this.Lab1.Text = this.fRec1.XRef;

				switch (this.fMergeMode) {
					case GEDCOMRecordType.rtIndividual:
						{
							GEDCOMIndividualRecord iRec = (this.fRec1 as GEDCOMIndividualRecord);
							this.Edit1.Text = iRec.aux_GetNameStr(true, false);
							GKUtils.ShowPersonInfo(iRec, this.Memo1.Lines, this.fBase.ShieldState);
							break;
						}
					case GEDCOMRecordType.rtNote:
						{
							GEDCOMNoteRecord nRec = (this.fRec1 as GEDCOMNoteRecord);
							this.Edit1.Text = nRec.Note[0];
                            GKUtils.ShowNoteInfo(nRec, this.Memo1.Lines);
							break;
						}
					case GEDCOMRecordType.rtFamily:
						{
							GEDCOMFamilyRecord famRec = (this.fRec1 as GEDCOMFamilyRecord);
							this.Edit1.Text = GKUtils.aux_GetFamilyStr(famRec);
                            GKUtils.ShowFamilyInfo(famRec, this.Memo1.Lines, this.fBase.ShieldState);
							break;
						}
					case GEDCOMRecordType.rtSource:
						{
							GEDCOMSourceRecord srcRec = (this.fRec1 as GEDCOMSourceRecord);
							this.Edit1.Text = srcRec.FiledByEntry;
                            GKUtils.ShowSourceInfo(srcRec, this.Memo1.Lines);
							break;
						}
				}
			}
		}

		public void SetRec2(GEDCOMRecord value)
		{
			this.fRec2 = value;
			this.UpdateMergeButtons();

			if (this.fRec2 == null)
			{
				this.Lab2.Text = "XXX2";
				this.Edit2.Text = "";
				this.Memo2.Lines.Clear();
			}
			else
			{
				this.Lab2.Text = this.fRec2.XRef;

				switch (this.fMergeMode) {
					case GEDCOMRecordType.rtIndividual:
						{
							GEDCOMIndividualRecord iRec = (this.fRec2 as GEDCOMIndividualRecord);
							this.Edit2.Text = iRec.aux_GetNameStr(true, false);
                            GKUtils.ShowPersonInfo(iRec, this.Memo2.Lines, this.fBase.ShieldState);
							break;
						}
					case GEDCOMRecordType.rtNote:
						{
							GEDCOMNoteRecord nRec = (this.fRec2 as GEDCOMNoteRecord);
							this.Edit2.Text = nRec.Note[0];
                            GKUtils.ShowNoteInfo(nRec, this.Memo2.Lines);
							break;
						}
					case GEDCOMRecordType.rtFamily:
						{
							GEDCOMFamilyRecord famRec = (this.fRec2 as GEDCOMFamilyRecord);
							this.Edit2.Text = GKUtils.aux_GetFamilyStr(famRec);
                            GKUtils.ShowFamilyInfo(famRec, this.Memo2.Lines, this.fBase.ShieldState);
							break;
						}
					case GEDCOMRecordType.rtSource:
						{
							GEDCOMSourceRecord srcRec = (this.fRec2 as GEDCOMSourceRecord);
							this.Edit2.Text = srcRec.FiledByEntry;
                            GKUtils.ShowSourceInfo(srcRec, this.Memo2.Lines);
							break;
						}
				}
			}
		}

		void btnRec1Select_Click(object sender, EventArgs e)
		{
			GEDCOMRecord irec = this.Base.SelectRecord(this.fMergeMode, null);
			if (irec != null) this.SetRec1(irec);
		}

		void btnRec2Select_Click(object sender, EventArgs e)
		{
			GEDCOMRecord irec = this.Base.SelectRecord(this.fMergeMode, null);
			if (irec != null) this.SetRec2(irec);
		}

		void btnMergeToLeft_Click(object sender, EventArgs e)
		{
			this.RecordMerge(this.fRec1, this.fRec2);
			this.SetRec1(this.fRec1);
			this.SetRec2(null);
		}

		void btnMergeToRight_Click(object sender, EventArgs e)
		{
			this.RecordMerge(this.fRec2, this.fRec1);
			this.SetRec1(null);
			this.SetRec2(this.fRec2);
		}

	}
}

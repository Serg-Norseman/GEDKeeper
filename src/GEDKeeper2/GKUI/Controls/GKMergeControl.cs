using System;
using System.Drawing;
using System.Windows.Forms;

using ExtUtils.Controls;
using GedCom551;
using GKCore;
using GKCore.Interfaces;

namespace GKUI.Controls
{
	public partial class GKMergeControl : UserControl
	{
		private TGEDCOMRecord fRec1;
		private TGEDCOMRecord fRec2;

		private readonly HyperView Memo1;
		private readonly HyperView Memo2;

		private IBase fBase;
		private TGEDCOMTree fTree;
		private TGEDCOMRecordType fMergeMode;

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

		public TGEDCOMRecordType MergeMode
		{
			get { return this.fMergeMode; }
			set { this.fMergeMode = value; }
		}

		public TGEDCOMRecord Rec1
		{
			get { return this.fRec1; }
		}

		public TGEDCOMRecord Rec2
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


		private void RecordMerge(TGEDCOMRecord targetRec, TGEDCOMRecord aRecCopy)
		{
			XRefReplacer repMap = new XRefReplacer();
			try
			{
				repMap.AddXRef(aRecCopy, aRecCopy.XRef, targetRec.XRef);

				int num = this.fTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					this.fTree[i].ReplaceXRefs(repMap);
				}

				switch (targetRec.RecordType)
				{
					case TGEDCOMRecordType.rtIndividual:
                        TGEDCOMIndividualRecord indRec = (aRecCopy as TGEDCOMIndividualRecord);
                        indRec.MoveTo(targetRec, false);
                        this.Base.RecordDelete(indRec, false);
						break;

					case TGEDCOMRecordType.rtNote:
                        TGEDCOMNoteRecord noteRec = (aRecCopy as TGEDCOMNoteRecord);
                        noteRec.MoveTo(targetRec, false);
                        this.Base.RecordDelete(noteRec, false);
						break;

					case TGEDCOMRecordType.rtFamily:
                        TGEDCOMFamilyRecord famRec = (aRecCopy as TGEDCOMFamilyRecord);
                        famRec.MoveTo(targetRec, false);
                        this.Base.RecordDelete(famRec, false);
						break;

					case TGEDCOMRecordType.rtSource:
                        TGEDCOMSourceRecord srcRec = (aRecCopy as TGEDCOMSourceRecord);
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

		public void SetRec1(TGEDCOMRecord value)
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
					case TGEDCOMRecordType.rtIndividual:
						{
							TGEDCOMIndividualRecord iRec = (this.fRec1 as TGEDCOMIndividualRecord);
							this.Edit1.Text = iRec.aux_GetNameStr(true, false);
							GKUtils.ShowPersonInfo(iRec, this.Memo1.Lines, this.fBase.ShieldState);
							break;
						}
					case TGEDCOMRecordType.rtNote:
						{
							TGEDCOMNoteRecord nRec = (this.fRec1 as TGEDCOMNoteRecord);
							this.Edit1.Text = nRec.Note[0];
                            GKUtils.ShowNoteInfo(nRec, this.Memo1.Lines);
							break;
						}
					case TGEDCOMRecordType.rtFamily:
						{
							TGEDCOMFamilyRecord famRec = (this.fRec1 as TGEDCOMFamilyRecord);
							this.Edit1.Text = GKUtils.aux_GetFamilyStr(famRec);
                            GKUtils.ShowFamilyInfo(famRec, this.Memo1.Lines, this.fBase.ShieldState);
							break;
						}
					case TGEDCOMRecordType.rtSource:
						{
							TGEDCOMSourceRecord srcRec = (this.fRec1 as TGEDCOMSourceRecord);
							this.Edit1.Text = srcRec.FiledByEntry;
                            GKUtils.ShowSourceInfo(srcRec, this.Memo1.Lines);
							break;
						}
				}
			}
		}

		public void SetRec2(TGEDCOMRecord value)
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
					case TGEDCOMRecordType.rtIndividual:
						{
							TGEDCOMIndividualRecord iRec = (this.fRec2 as TGEDCOMIndividualRecord);
							this.Edit2.Text = iRec.aux_GetNameStr(true, false);
                            GKUtils.ShowPersonInfo(iRec, this.Memo2.Lines, this.fBase.ShieldState);
							break;
						}
					case TGEDCOMRecordType.rtNote:
						{
							TGEDCOMNoteRecord nRec = (this.fRec2 as TGEDCOMNoteRecord);
							this.Edit2.Text = nRec.Note[0];
                            GKUtils.ShowNoteInfo(nRec, this.Memo2.Lines);
							break;
						}
					case TGEDCOMRecordType.rtFamily:
						{
							TGEDCOMFamilyRecord famRec = (this.fRec2 as TGEDCOMFamilyRecord);
							this.Edit2.Text = GKUtils.aux_GetFamilyStr(famRec);
                            GKUtils.ShowFamilyInfo(famRec, this.Memo2.Lines, this.fBase.ShieldState);
							break;
						}
					case TGEDCOMRecordType.rtSource:
						{
							TGEDCOMSourceRecord srcRec = (this.fRec2 as TGEDCOMSourceRecord);
							this.Edit2.Text = srcRec.FiledByEntry;
                            GKUtils.ShowSourceInfo(srcRec, this.Memo2.Lines);
							break;
						}
				}
			}
		}

		void btnRec1Select_Click(object sender, EventArgs e)
		{
			TGEDCOMRecord irec = this.Base.SelectRecord(this.fMergeMode, null);
			if (irec != null) this.SetRec1(irec);
		}

		void btnRec2Select_Click(object sender, EventArgs e)
		{
			TGEDCOMRecord irec = this.Base.SelectRecord(this.fMergeMode, null);
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

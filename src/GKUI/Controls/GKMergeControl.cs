using System;
using System.Drawing;
using System.Windows.Forms;

using GedCom551;
using GKCore;

namespace GKUI.Controls
{
	public partial class GKMergeControl : UserControl
	{
		private TGEDCOMRecord FRec1;
		private TGEDCOMRecord FRec2;

		private GKHyperView Memo1;
		private GKHyperView Memo2;

		private TfmBase FBase;
		private TGEDCOMTree FTree;
		private TGEDCOMRecordType FMergeMode;

		public TfmBase Base
		{
			get	{
				return this.FBase;
			}
			set {
				this.FBase = value;

				if (value != null) {
					this.FTree = this.FBase.Tree;
				} else {
					this.FTree = null;
				}
			}
		}

		public TGEDCOMRecordType MergeMode
		{
			get {
				return this.FMergeMode;
			}
			set {
				this.FMergeMode = value;
			}
		}

		public TGEDCOMRecord Rec1
		{
			get { return this.FRec1; }
		}

		public TGEDCOMRecord Rec2
		{
			get { return this.FRec2; }
		}

		public GKMergeControl()
		{
			InitializeComponent();

			this.Memo1 = new GKHyperView();
			this.Memo1.Location = new Point(8, 56);
			this.Memo1.Size = new Size(329, 248);
			this.Controls.Add(this.Memo1);

			this.Memo2 = new GKHyperView();
			this.Memo2.Location = new Point(344, 56);
			this.Memo2.Size = new Size(329, 248);
			this.Controls.Add(this.Memo2);

			this.SetRec1(null);
			this.SetRec2(null);

			this.btnRec1Select.Text = LangMan.LSList[100] + "...";
			this.btnRec2Select.Text = LangMan.LSList[100] + "...";
		}


		private void RecordMerge(TGEDCOMRecord target_rec, TGEDCOMRecord aRecCopy)
		{
			TXRefReplaceMap repMap = new TXRefReplaceMap();
			try
			{
				repMap.AddXRef(aRecCopy, aRecCopy.XRef, target_rec.XRef);

				int num = this.FTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					this.FTree[i].ReplaceXRefs(repMap);
				}

				switch (target_rec.RecordType)
				{
					case TGEDCOMRecordType.rtIndividual:
                        TGEDCOMIndividualRecord indRec = (aRecCopy as TGEDCOMIndividualRecord);
                        indRec.MoveTo(target_rec, false);
                        this.Base.DeleteIndividualRecord(indRec, false);
						break;

					case TGEDCOMRecordType.rtNote:
                        TGEDCOMNoteRecord noteRec = (aRecCopy as TGEDCOMNoteRecord);
                        noteRec.MoveTo(target_rec, false);
                        this.Base.DeleteNoteRecord(noteRec, false);
						break;

					case TGEDCOMRecordType.rtFamily:
                        TGEDCOMFamilyRecord famRec = (aRecCopy as TGEDCOMFamilyRecord);
                        famRec.MoveTo(target_rec, false);
                        this.Base.DeleteFamilyRecord(famRec, false);
						break;

					case TGEDCOMRecordType.rtSource:
                        TGEDCOMSourceRecord srcRec = (aRecCopy as TGEDCOMSourceRecord);
                        srcRec.MoveTo(target_rec, false);
                        this.Base.DeleteSourceRecord(srcRec, false);
						break;
				}

				this.Base.ChangeRecord(target_rec);
				this.Base.ListsRefresh(false);
			}
			finally
			{
				repMap.Free();
			}
		}

		private void UpdateMergeButtons()
		{
			this.btnMergeToLeft.Enabled = (this.FRec1 != null && this.FRec2 != null);
			this.btnMergeToRight.Enabled = (this.FRec1 != null && this.FRec2 != null);
		}

		public void SetRec1(TGEDCOMRecord Value)
		{
			this.FRec1 = Value;
			this.UpdateMergeButtons();

			if (this.FRec1 == null)
			{
				this.Lab1.Text = "XXX1";
				this.Edit1.Text = "";
				this.Memo1.Lines.Clear();
			}
			else
			{
				this.Lab1.Text = this.FRec1.XRef;

				switch (this.FMergeMode) {
					case TGEDCOMRecordType.rtIndividual:
						{
							TGEDCOMIndividualRecord iRec = (this.FRec1 as TGEDCOMIndividualRecord);
							this.Edit1.Text = iRec.aux_GetNameStr(true, false);
							this.Base.ShowPersonInfo(iRec, this.Memo1.Lines);
							break;
						}
					case TGEDCOMRecordType.rtNote:
						{
							TGEDCOMNoteRecord nRec = (this.FRec1 as TGEDCOMNoteRecord);
							this.Edit1.Text = nRec.Note[0];
							this.Base.ShowNoteInfo(nRec, this.Memo1.Lines);
							break;
						}
					case TGEDCOMRecordType.rtFamily:
						{
							TGEDCOMFamilyRecord famRec = (this.FRec1 as TGEDCOMFamilyRecord);
							this.Edit1.Text = TGenEngine.aux_GetFamilyStr(famRec);
							this.Base.ShowFamilyInfo(famRec, this.Memo1.Lines);
							break;
						}
					case TGEDCOMRecordType.rtSource:
						{
							TGEDCOMSourceRecord srcRec = (this.FRec1 as TGEDCOMSourceRecord);
							this.Edit1.Text = srcRec.FiledByEntry;
							this.Base.ShowSourceInfo(srcRec, this.Memo1.Lines);
							break;
						}
				}
			}
		}

		public void SetRec2(TGEDCOMRecord Value)
		{
			this.FRec2 = Value;
			this.UpdateMergeButtons();

			if (this.FRec2 == null)
			{
				this.Lab2.Text = "XXX2";
				this.Edit2.Text = "";
				this.Memo2.Lines.Clear();
			}
			else
			{
				this.Lab2.Text = this.FRec2.XRef;

				switch (this.FMergeMode) {
					case TGEDCOMRecordType.rtIndividual:
						{
							TGEDCOMIndividualRecord iRec = (this.FRec2 as TGEDCOMIndividualRecord);
							this.Edit2.Text = iRec.aux_GetNameStr(true, false);
							this.Base.ShowPersonInfo(iRec, this.Memo2.Lines);
							break;
						}
					case TGEDCOMRecordType.rtNote:
						{
							TGEDCOMNoteRecord nRec = (this.FRec2 as TGEDCOMNoteRecord);
							this.Edit2.Text = nRec.Note[0];
							this.Base.ShowNoteInfo(nRec, this.Memo2.Lines);
							break;
						}
					case TGEDCOMRecordType.rtFamily:
						{
							TGEDCOMFamilyRecord famRec = (this.FRec2 as TGEDCOMFamilyRecord);
							this.Edit2.Text = TGenEngine.aux_GetFamilyStr(famRec);
							this.Base.ShowFamilyInfo(famRec, this.Memo2.Lines);
							break;
						}
					case TGEDCOMRecordType.rtSource:
						{
							TGEDCOMSourceRecord srcRec = (this.FRec2 as TGEDCOMSourceRecord);
							this.Edit2.Text = srcRec.FiledByEntry;
							this.Base.ShowSourceInfo(srcRec, this.Memo2.Lines);
							break;
						}
				}
			}
		}

		void btnRec1Select_Click(object sender, EventArgs e)
		{
			TGEDCOMRecord irec = this.Base.SelectRecord(this.FMergeMode, null);
			if (irec != null) this.SetRec1(irec);
		}

		void btnRec2Select_Click(object sender, EventArgs e)
		{
			TGEDCOMRecord irec = this.Base.SelectRecord(this.FMergeMode, null);
			if (irec != null) this.SetRec2(irec);
		}

		void btnMergeToLeft_Click(object sender, EventArgs e)
		{
			this.RecordMerge(this.FRec1, this.FRec2);
			this.SetRec1(this.FRec1);
			this.SetRec2(null);
		}

		void btnMergeToRight_Click(object sender, EventArgs e)
		{
			this.RecordMerge(this.FRec2, this.FRec1);
			this.SetRec1(null);
			this.SetRec2(this.FRec2);
		}


	}
}

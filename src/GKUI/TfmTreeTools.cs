using System;
using System.Collections.Generic;
using System.Drawing;
using System.IO;
using System.Runtime.InteropServices;
using System.Windows.Forms;

using Ext.Utils;
using GedCom551;
using GKCore;
using GKSandbox;
using GKUI.Controls;

/// <summary>
/// Localization: dirty
/// </summary>

namespace GKUI
{
	public partial class TfmTreeTools : Form
	{
		private class TPlaceObj : IDisposable
		{
			public string Name;
			public TList Facts;
			protected bool Disposed_;

			public TPlaceObj()
			{
				this.Facts = new TList();
			}

			public void Dispose()
			{
				if (!this.Disposed_)
				{
					this.Facts.Dispose();
					this.Disposed_ = true;
				}
			}
		}

		private static readonly string[] HelpTopics;

		private TfmBase FBase;
		private TGEDCOMTree FTree;

		private TList FSplitList;

		private TGEDCOMRecord FRec1;
		private TGEDCOMRecord FRec2;
		private TGEDCOMRecordType FRMMode;
		private StringList FRMSkip;
		private int FRMIndex;

		private StringList FPlaces;
		private TList FChecksList;

		private GKHyperView Memo1;
		private GKHyperView Memo2;
		private GKListView ListPlaces;
		private GKListView ListChecks;
		private GKListView ListPatriarchs;


		public TfmBase Base
		{
			get	{ return this.FBase; }
		}

		private void SearchDups()
		{
			TGEDCOMIndividualRecord.MatchParams mParams;
			mParams.IndistinctNameMatching = this.chkIndistinctMatching.Checked;
			mParams.IndistinctThreshold = decimal.ToDouble(this.edNameAccuracy.Value) / 100.0;
			mParams.CheckBirthYear = this.chkBirthYear.Checked;
			mParams.YearInaccuracy = decimal.ToInt32(this.edYearInaccuracy.Value);

			bool res = false;
			this.btnSkip.Enabled = false;

			try
			{
				this.ProgressBar1.Minimum = 0;
				this.ProgressBar1.Maximum = this.FTree.RecordsCount;
				this.ProgressBar1.Value = this.FRMIndex;

				int num = this.FTree.RecordsCount - 1;
				for (int i = this.FRMIndex; i <= num; i++)
				{
					this.FRMIndex = i;
					this.ProgressBar1.Increment(1);

					TGEDCOMRecord iRec = this.FTree[i];
					if (iRec.RecordType != this.FRMMode) continue;

					int num5 = this.FTree.RecordsCount - 1;
					for (int j = i + 1; j <= num5; j++)
					{
						TGEDCOMRecord kRec = this.FTree[j];
						if (kRec.RecordType != this.FRMMode) continue;

						if (iRec == kRec) continue;
						if (this.FRMSkip.IndexOf(iRec.XRef + "-" + kRec.XRef) >= 0) continue;

						res = iRec.IsMatch(kRec, 100.0F, mParams);

						if (res) {
							this.SetRec1(iRec);
							this.SetRec2(kRec);
							break;
						}
					}

					if (res) break;
				}
			}
			finally
			{
				this.btnSkip.Enabled = true;
			}
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
						(aRecCopy as TGEDCOMIndividualRecord).MoveTo(target_rec, false);
						this.Base.DeleteIndividualRecord(aRecCopy as TGEDCOMIndividualRecord, false);
						break;

					case TGEDCOMRecordType.rtNote:
						(aRecCopy as TGEDCOMNoteRecord).MoveTo(target_rec, false);
						this.Base.DeleteNoteRecord(aRecCopy as TGEDCOMNoteRecord, false);
						break;

					case TGEDCOMRecordType.rtFamily:
						(aRecCopy as TGEDCOMFamilyRecord).MoveTo(target_rec, false);
						this.Base.DeleteFamilyRecord(aRecCopy as TGEDCOMFamilyRecord, false);
						break;

					case TGEDCOMRecordType.rtSource:
						(aRecCopy as TGEDCOMSourceRecord).MoveTo(target_rec, false);
						this.Base.DeleteSourceRecord(aRecCopy as TGEDCOMSourceRecord, false);
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

		private void SetRec1([In] TGEDCOMRecord Value)
		{
			this.FRec1 = Value;
			this.btnMergeToLeft.Enabled = (this.FRec1 != null && this.FRec2 != null);
			this.btnMergeToRight.Enabled = (this.FRec1 != null && this.FRec2 != null);
			if (this.FRec1 == null)
			{
				this.Lab1.Text = "XXX1";
				this.Edit1.Text = "";
				this.Memo1.Lines.Clear();
			}
			else
			{
				this.Lab1.Text = this.FRec1.XRef;

				switch (this.FRMMode) {
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

		private void SetRec2([In] TGEDCOMRecord Value)
		{
			this.FRec2 = Value;
			this.btnMergeToLeft.Enabled = (this.FRec1 != null && this.FRec2 != null);
			this.btnMergeToRight.Enabled = (this.FRec1 != null && this.FRec2 != null);

			if (this.FRec2 == null)
			{
				this.Lab2.Text = "XXX2";
				this.Edit2.Text = "";
				this.Memo2.Lines.Clear();
			}
			else
			{
				this.Lab2.Text = this.FRec2.XRef;

				switch (this.FRMMode) {
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

		private void Select(TGEDCOMIndividualRecord aPerson, TreeTools.TTreeWalkMode aMode)
		{
			this.FSplitList.Clear();
			TreeTools.TreeWalk(aPerson, aMode, this.FSplitList);
			this.UpdateSplitLists();
		}

		private void CheckRelations()
		{
			int num = this.FSplitList.Count;
			for (int i = 0; i < num; i++)
			{
				TGEDCOMRecord rec = this.FSplitList[i] as TGEDCOMRecord;
				switch (rec.RecordType)
				{
					case TGEDCOMRecordType.rtIndividual:
					{
						_CheckRelations_CheckIndividual((TGEDCOMIndividualRecord)rec);
						break;
					}
					case TGEDCOMRecordType.rtFamily:
					{
						_CheckRelations_CheckFamily((TGEDCOMFamilyRecord)rec);
						break;
					}
					case TGEDCOMRecordType.rtNote:
					{
						_CheckRelations_CheckRecord(rec);
						break;
					}
					case TGEDCOMRecordType.rtMultimedia:
					{
						_CheckRelations_CheckRecord(rec);
						break;
					}
					case TGEDCOMRecordType.rtSource:
					{
						_CheckRelations_CheckSource((TGEDCOMSourceRecord)rec);
						break;
					}
					case TGEDCOMRecordType.rtRepository:
					{
						_CheckRelations_CheckRecord(rec);
						break;
					}
					case TGEDCOMRecordType.rtSubmitter:
					{
						_CheckRelations_CheckRecord(rec);
						break;
					}
				}
			}
		}

		private void UpdateSplitLists()
		{
			this.ListSelected.BeginUpdate();
			this.ListSelected.Items.Clear();
			this.ListSkipped.BeginUpdate();
			this.ListSkipped.Items.Clear();
			try
			{
				int cnt = 0;

				int num = this.FTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = this.FTree[i];
					if (rec is TGEDCOMIndividualRecord)
					{
						cnt++;
						TGEDCOMIndividualRecord i_rec = (TGEDCOMIndividualRecord)rec;
						string st = i_rec.XRef + " / " + i_rec.aux_GetNameStr(true, false);

						if (this.FSplitList.IndexOf(i_rec) < 0) {
							this.ListSkipped.Items.Add(st);
						} else {
							this.ListSelected.Items.Add(st);
						}
					}
				}
				this.Text = this.FSplitList.Count.ToString() + " / " + cnt.ToString();
			}
			finally
			{
				this.ListSelected.EndUpdate();
				this.ListSkipped.EndUpdate();
			}
		}

		private void CheckGroups()
		{
			TfmProgress.ProgressInit(this.FTree.RecordsCount, LangMan.LSList[521]);
			TList prepared = new TList();
			try
			{
				int group = 0;
				this.TreeView1.Nodes.Clear();

				int num = this.FTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = this.FTree[i];

					if (rec is TGEDCOMIndividualRecord)
					{
						TGEDCOMIndividualRecord iRec = (TGEDCOMIndividualRecord)rec;
						if (prepared.IndexOf(iRec) < 0)
						{
							group++;
							this.FSplitList.Clear();

							TreeTools.TreeWalk(iRec, TreeTools.TTreeWalkMode.twmAll, this.FSplitList);

							TreeNode root = this.TreeView1.Nodes.Add(
								group.ToString() + " " + LangMan.LSList[185].ToLower() + " (" + this.FSplitList.Count.ToString() + ")");

							int num2 = this.FSplitList.Count - 1;
							for (int j = 0; j <= num2; j++)
							{
								iRec = (TGEDCOMIndividualRecord)this.FSplitList[j];
								prepared.Add(iRec);
								string pn = iRec.aux_GetNameStr(true, false);
								if (iRec.Patriarch)
								{
									pn = "(*) " + pn;
								}
								root.Nodes.Add(new GKTreeNode(pn, iRec));
							}
							root.ExpandAll();
						}
					}

					TfmProgress.ProgressStep();
					Application.DoEvents();
				}
			}
			finally
			{
				this.FSplitList.Clear();
				prepared.Dispose();
				TfmProgress.ProgressDone();
			}
		}

		private void PrepareChecksList()
		{
			this.Base.CreateListView(this.Panel1, ref this.ListChecks);
			this.ListChecks.CheckBoxes = true;
			this.ListChecks.DoubleClick += new EventHandler(this.ListChecksDblClick);
			this.ListChecks.AddListColumn(LangMan.LSList[579], 400, false);
			this.ListChecks.AddListColumn(LangMan.LSList[580], 200, false);
			this.ListChecks.AddListColumn(LangMan.LSList[581], 200, false);
		}

		private void CheckBase()
		{
			TreeTools.CheckBase(this.FBase.Tree, this.FChecksList);

			this.ListChecks.Items.Clear();

			int num2 = this.FChecksList.Count - 1;
			for (int i = 0; i <= num2; i++)
			{
				TreeTools.TCheckObj checkObj = this.FChecksList[i] as TreeTools.TCheckObj;
				ListViewItem item = this.ListChecks.AddItem(checkObj.RecName, checkObj);
				item.SubItems.Add(checkObj.Comment);
			}
		}

		void btnBaseRepair_Click(object sender, EventArgs e)
		{
			try
			{
				int num = this.ListChecks.Items.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					GKListItem item = this.ListChecks.Items[i] as GKListItem;

					if (item.Checked)
					{
						TreeTools.TCheckObj checkObj = item.Data as TreeTools.TCheckObj;
						TreeTools.RepairProblem(this.FBase.Tree, checkObj);
					}
				}
			}
			finally
			{
				this.Base.ListsRefresh(false);
				this.CheckBase();
			}
		}

		void ListChecksDblClick(object sender, EventArgs e)
		{
			GKListItem item = this.ListChecks.SelectedItem();
			if (item != null)
			{
				TGEDCOMIndividualRecord i_rec = (item.Data as TreeTools.TCheckObj).Rec as TGEDCOMIndividualRecord;
				if (i_rec != null)
				{
					this.Base.SelectRecordByXRef(i_rec.XRef);
					this.Close();
				}
			}
		}

		private void PreparePatriarchsList()
		{
			this.Base.CreateListView(this.Panel3, ref this.ListPatriarchs);
			this.ListPatriarchs.DoubleClick += new EventHandler(this.ListPatriarchsDblClick);
			this.ListPatriarchs.AddListColumn(LangMan.LSList[92], 400, false);
			this.ListPatriarchs.AddListColumn(LangMan.LSList[321], 90, false);
			this.ListPatriarchs.AddListColumn(LangMan.LSList[587], 90, false);
			this.ListPatriarchs.AddListColumn(LangMan.LSList[588], 90, false);
		}

		void ListPatriarchsDblClick(object sender, EventArgs e)
		{
			GKListItem item = this.ListPatriarchs.SelectedItem();
			if (item != null)
			{
				TGEDCOMIndividualRecord i_rec = item.Data as TGEDCOMIndividualRecord;
				if (i_rec != null)
				{
					this.Base.SelectRecordByXRef(i_rec.XRef);
					this.Close();
				}
			}
		}

		private void PreparePlacesList()
		{
			this.Base.CreateListView(this.Panel4, ref this.ListPlaces);
			this.ListPlaces.DoubleClick += new EventHandler(this.ListPlacesDblClick);
			this.ListPlaces.AddListColumn(LangMan.LSList[204], 400, false);
			this.ListPlaces.AddListColumn(LangMan.LSList[589], 100, false);
		}

		private void PlacesClear()
		{
			for (int i = this.FPlaces.Count - 1; i >= 0; i--) (this.FPlaces.GetObject(i) as TPlaceObj).Dispose();
			this.FPlaces.Clear();
		}

		private void CheckPlaces()
		{
			TfmProgress.ProgressInit(this.FTree.RecordsCount, LangMan.LSList[590]);
			this.ListPlaces.BeginUpdate();
			try
			{
				this.PlacesClear();

				int num = this.FTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TfmProgress.ProgressStep();

					TGEDCOMRecord record = this.FTree[i];

					if (record is TGEDCOMIndividualRecord)
					{
						TGEDCOMIndividualRecord iRec = (TGEDCOMIndividualRecord)record;

						int num2 = iRec.IndividualEvents.Count - 1;
						for (int j = 0; j <= num2; j++)
						{
							_CheckPlaces_PrepareEvent(iRec.IndividualEvents[j]);
						}
					}
					else
					{
						if (record is TGEDCOMFamilyRecord)
						{
							TGEDCOMFamilyRecord fRec = (TGEDCOMFamilyRecord)record;

							int num3 = fRec.FamilyEvents.Count - 1;
							for (int j = 0; j <= num3; j++)
							{
								_CheckPlaces_PrepareEvent(fRec.FamilyEvents[j]);
							}
						}
					}
				}
				this.ListPlaces.Items.Clear();

				int num4 = this.FPlaces.Count - 1;
				for (int i = 0; i <= num4; i++)
				{
					TPlaceObj place_obj = this.FPlaces.GetObject(i) as TfmTreeTools.TPlaceObj;
					GKListItem item = this.ListPlaces.AddItem(this.FPlaces[i], place_obj);
					item.SubItems.Add(place_obj.Facts.Count.ToString());
				}
			}
			finally
			{
				this.ListPlaces.EndUpdate();
				TfmProgress.ProgressDone();
			}
		}

		void ListPlacesDblClick(object sender, EventArgs e)
		{
			GKListItem item = this.ListPlaces.SelectedItem();
			if (item != null)
			{
				TfmTreeTools.TPlaceObj p_obj = item.Data as TfmTreeTools.TPlaceObj;
				if (p_obj != null)
				{
					if (p_obj.Name.IndexOf("[*]") == 0)
					{
						TGenEngine.ShowMessage(LangMan.LSList[591]);
					}
					else
					{
						TGEDCOMLocationRecord loc = this.Base.SelectRecord(TGEDCOMRecordType.rtLocation, new object[] { p_obj.Name }) as TGEDCOMLocationRecord;
						if (loc != null)
						{
							int num = p_obj.Facts.Count - 1;
							for (int i = 0; i <= num; i++)
							{
								TGEDCOMCustomEvent evt = p_obj.Facts[i] as TGEDCOMCustomEvent;
								evt.Detail.Place.StringValue = loc.LocationName;
								evt.Detail.Place.Location.Value = loc;
							}
							this.CheckPlaces();
							this.Base.ListsRefresh(false);
						}
					}
				}
			}
		}

		void btnSelectFamily_Click(object sender, EventArgs e)
		{
			this.Select(this.Base.GetSelectedPerson(), TreeTools.TTreeWalkMode.twmFamily);
		}

		void btnSelectAncestors_Click(object sender, EventArgs e)
		{
			this.Select(this.Base.GetSelectedPerson(), TreeTools.TTreeWalkMode.twmAncestors);
		}

		void btnSelectDescendants_Click(object sender, EventArgs e)
		{
			this.Select(this.Base.GetSelectedPerson(), TreeTools.TTreeWalkMode.twmDescendants);
		}

		void btnDelete_Click(object sender, EventArgs e)
		{
			int num = this.FSplitList.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				object obj = this.FSplitList[i];
				if (obj is TGEDCOMIndividualRecord)
				{
					this.Base.DeleteIndividualRecord(obj as TGEDCOMIndividualRecord, false);
				}
			}
			TGenEngine.ShowMessage(LangMan.LSList[578]);
			this.FSplitList.Clear();
			this.UpdateSplitLists();
			this.Base.ListsRefresh(false);
		}

		void btnSave_Click(object sender, EventArgs e)
		{
			if (this.SaveDialog1.ShowDialog() == DialogResult.OK)
			{
				this.CheckRelations();
				string subm = this.FTree.Header.GetTagStringValue("SUBM");
				this.FTree.Header.Clear();
				this.FTree.Header.Source = "GEDKeeper";
				this.FTree.Header.ReceivingSystemName = "GEDKeeper";
				this.FTree.Header.CharacterSet = GKUI.TfmGEDKeeper.Instance.Options.DefCharacterSet;
				this.FTree.Header.Language = "Russian";
				this.FTree.Header.GEDCOMVersion = "5.5";
				this.FTree.Header.GEDCOMForm = "LINEAGE-LINKED";
				this.FTree.Header.FileName = Path.GetFileName(this.SaveDialog1.FileName);
				this.FTree.Header.TransmissionDate.Date = DateTime.Now;

				if (subm != "")
				{
					this.FTree.Header.SetTagStringValue("SUBM", subm);
				}

				StreamWriter fs = new StreamWriter(this.SaveDialog1.FileName, false, TGEDCOMObject.GetEncodingByCharacterSet(this.FTree.Header.CharacterSet));
				try
				{
					this.FTree.SaveHeaderToStream(fs);
					int num = this.FTree.RecordsCount - 1;
					for (int i = 0; i <= num; i++)
					{
						TGEDCOMRecord rec = this.FTree[i];
						if (this.FSplitList.IndexOf(rec) >= 0)
						{
							rec.SaveToStream(fs);
						}
					}
					this.FTree.SaveFooterToStream(fs);
					this.FTree.Header.CharacterSet = TGEDCOMCharacterSet.csASCII;
				}
				finally
				{
					SysUtils.Free(fs);
				}
			}
		}

		void btnSearch_Click(object sender, EventArgs e)
		{
			this.FRMIndex = 0;
			this.FRMSkip.Clear();
			this.SearchDups();
		}

		void btnRec1Select_Click(object sender, EventArgs e)
		{
			TGEDCOMRecord irec = this.Base.SelectRecord(this.FRMMode, null);
			if (irec != null) this.SetRec1(irec);
		}

		void btnRec2Select_Click(object sender, EventArgs e)
		{
			TGEDCOMRecord irec = this.Base.SelectRecord(this.FRMMode, null);
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

		void btnSkip_Click(object sender, EventArgs e)
		{
			if (this.FRec1 != null && this.FRec2 != null)
			{
				this.FRMSkip.Add(this.FRec1.XRef + "-" + this.FRec2.XRef);
			}
			this.SearchDups();
		}

		void btnImportFileChoose_Click(object sender, EventArgs e)
		{
			if (this.OpenDialog2.ShowDialog() == DialogResult.OK)
			{
				this.edImportFile.Text = this.OpenDialog2.FileName;
				Importer imp = new Importer(this.Base.Engine, this.ListBox1.Items);
				try
				{
					imp.TreeImportEx(this.edImportFile.Text);
				}
				finally
				{
					imp.Free();
				}
				this.ListBox1.SelectedIndex = this.ListBox1.Items.Count - 1;
				this.Base.ListsRefresh(false);
			}
		}

		void TreeView1_DoubleClick(object sender, EventArgs e)
		{
			GKTreeNode node = this.TreeView1.SelectedNode as GKTreeNode;
			if (node != null)
			{
				TGEDCOMIndividualRecord i_rec = node.Data as TGEDCOMIndividualRecord;
				if (i_rec != null)
				{
					this.Base.SelectRecordByXRef(i_rec.XRef);
					base.Close();
				}
			}
		}

		void btnPatSearch_Click(object sender, EventArgs e)
		{
			this.ListPatriarchs.BeginUpdate();
			TList lst = new TList(true);
			try
			{
				this.ListPatriarchs.Items.Clear();
				TreeTools.GetPatriarchsList(this.Base.Engine.Tree, true, false, ref lst, decimal.ToInt32(this.edMinGens.Value), !chkWithoutDates.Checked);

				int num = lst.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TPatriarchObj p_obj = lst[i] as TPatriarchObj;
					string p_sign = ((p_obj.IRec.Patriarch) ? "[*] " : "");

					GKListItem item = this.ListPatriarchs.AddItem(p_sign + p_obj.IRec.aux_GetNameStr(true, false), p_obj.IRec);
					item.SubItems.Add(p_obj.IBirthYear.ToString());
					item.SubItems.Add(p_obj.IDescendantsCount.ToString());
					item.SubItems.Add(p_obj.IDescGenerations.ToString());
				}
			}
			finally
			{
				lst.Dispose();
				this.ListPatriarchs.EndUpdate();
			}
		}

		void btnSetPatriarch_Click(object sender, EventArgs e)
		{
			try
			{
				GKListItem item = this.ListPatriarchs.SelectedItem();
				if (item != null)
				{
					TGEDCOMIndividualRecord i_rec = item.Data as TGEDCOMIndividualRecord;
					if (i_rec != null)
					{
						i_rec.Patriarch = true;
					}
				}
			}
			finally
			{
				this.btnPatSearch_Click(null, null);
				this.Base.ListsRefresh(false);
			}
		}

		void btnIntoList_Click(object sender, EventArgs e)
		{
			this.ListPlacesDblClick(null, null);
		}

		void btnHelp_Click(object sender, EventArgs e)
		{
			GKUI.TfmGEDKeeper.Instance.ShowHelpTopic(TfmTreeTools.HelpTopics[this.PageControl.TabIndex]);
		}

		void PageControl_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (this.PageControl.SelectedTab == this.SheetFamilyGroups)
			{
				this.CheckGroups();
			}
			else
			{
				if (this.PageControl.SelectedTab == this.SheetTreeCheck)
				{
					this.CheckBase();
				}
				else
				{
					if (this.PageControl.SelectedTab == this.SheetPlaceManage)
					{
						this.CheckPlaces();
					}
				}
			}
		}

		void btnUpdateSelect_Click(object sender, EventArgs e)
		{
			if (this.OpenDialog1.ShowDialog() == DialogResult.OK)
			{
				this.edUpdateBase.Text = this.OpenDialog1.FileName;
				int tmt = 0;
				if (this.RadioButton3.Checked)
				{
					tmt = 0;
				}
				else
				{
					if (this.RadioButton3.Checked)
					{
						tmt = 1;
					}
				}
				if (tmt != 0)
				{
					if (tmt == 1)
					{
						TreeTools.TreeSync(this.Base.Tree, this.edUpdateBase.Text, this.mSyncRes);
					}
				}
				else
				{
					TreeTools.TreeMerge(this.Base.Tree, this.edUpdateBase.Text, this.mSyncRes);
				}

				this.Base.ListsRefresh(false);
			}
		}

		void btnSelectAll_Click(object sender, EventArgs e)
		{
			this.Select(this.Base.GetSelectedPerson(), TreeTools.TTreeWalkMode.twmAll);
		}

		void RadioButton3_Click(object sender, EventArgs e)
		{
			this.gbSyncType.Enabled = this.RadioButton4.Checked;
		}

		void RadioButton8_Click(object sender, EventArgs e)
		{
			if (this.RadioButton5.Checked) this.FRMMode = TGEDCOMRecordType.rtIndividual;
			if (this.RadioButton6.Checked) this.FRMMode = TGEDCOMRecordType.rtNote;
			if (this.RadioButton7.Checked) this.FRMMode = TGEDCOMRecordType.rtFamily;
			if (this.RadioButton8.Checked) this.FRMMode = TGEDCOMRecordType.rtSource;

			this.btnRec1Select.Enabled = (this.FRMMode != TGEDCOMRecordType.rtFamily);
			this.btnRec2Select.Enabled = (this.FRMMode != TGEDCOMRecordType.rtFamily);
		}

		protected override void Dispose(bool Disposing)
		{
			if (Disposing)
			{
				this.FChecksList.Dispose();
				this.PlacesClear();
				this.FPlaces.Free();
				this.FRMSkip.Free();
				this.FSplitList.Dispose();
			}
			base.Dispose(Disposing);
		}

		public TfmTreeTools(TfmBase aBase)
		{
			this.InitializeComponent();

			this.FBase = aBase;
			this.FTree = this.Base.Tree;

			this.PageControl.SelectedIndex = 0;
			this.FSplitList = new TList();
			this.Memo1 = new GKHyperView();
			this.Memo1.Location = new Point(8, 56);
			this.Memo1.Size = new Size(329, 248);
			this.SheetMerge.Controls.Add(this.Memo1);
			this.Memo2 = new GKHyperView();
			this.Memo2.Location = new Point(344, 56);
			this.Memo2.Size = new Size(329, 248);
			this.SheetMerge.Controls.Add(this.Memo2);

			this.FRMSkip = new StringList();
			this.SetRec1(null);
			this.SetRec2(null);
			this.FRMMode = TGEDCOMRecordType.rtIndividual;

			this.FPlaces = new StringList();
			this.FPlaces.Sorted = true;
			this.FChecksList = new TList(true);

			this.PrepareChecksList();
			this.PreparePatriarchsList();
			this.PreparePlacesList();

			this.SetLang();
		}

		public void SetLang()
		{
			this.Text = LangMan.LS(LSID.LSID_MITreeTools);

			this.SheetTreeCompare.Text = LangMan.LS(LSID.LSID_ToolOp_1);
			this.SheetTreeMerge.Text = LangMan.LS(LSID.LSID_ToolOp_2);
			this.SheetTreeSplit.Text = LangMan.LS(LSID.LSID_ToolOp_3);
			this.SheetRecMerge.Text = LangMan.LS(LSID.LSID_ToolOp_4);
			this.SheetTreeImport.Text = LangMan.LS(LSID.LSID_ToolOp_5);
			this.SheetFamilyGroups.Text = LangMan.LS(LSID.LSID_ToolOp_6);
			this.SheetTreeCheck.Text = LangMan.LS(LSID.LSID_ToolOp_7);
			this.SheetPatSearch.Text = LangMan.LS(LSID.LSID_ToolOp_8);
			this.SheetPlaceManage.Text = LangMan.LS(LSID.LSID_ToolOp_9);
			
			//this.SheetMerge.Text
			//this.SheetOptions.Text

			this.btnClose.Text = LangMan.LSList[99];
			this.btnHelp.Text = LangMan.LSList[5];
			this.Label1.Text = LangMan.LSList[0];
			this.btnFileChoose.Text = LangMan.LSList[100] + "...";
			this.btnUpdateSelect.Text = LangMan.LSList[100] + "...";
			this.btnSelectAll.Text = LangMan.LSList[557];
			this.btnSelectFamily.Text = LangMan.LSList[558];
			this.btnSelectAncestors.Text = LangMan.LSList[559];
			this.btnSelectDescendants.Text = LangMan.LSList[560];
			this.btnDelete.Text = LangMan.LSList[231];
			this.btnSave.Text = LangMan.LSList[9];
			this.SheetMerge.Text = LangMan.LSList[561];
			this.SheetOptions.Text = LangMan.LSList[39];
			this.btnRec1Select.Text = LangMan.LSList[100] + "...";
			this.btnRec2Select.Text = LangMan.LSList[100] + "...";
			this.btnSearch.Text = LangMan.LSList[562];
			this.btnSkip.Text = LangMan.LSList[563];
			this.rgMode.Text = LangMan.LSList[564];
			this.RadioButton5.Text = LangMan.LSList[52];
			this.RadioButton6.Text = LangMan.LSList[54];
			this.RadioButton7.Text = LangMan.LSList[53];
			this.RadioButton8.Text = LangMan.LSList[56];
			this.GroupBox1.Text = LangMan.LSList[565];
			this.chkIndistinctMatching.Text = LangMan.LSList[567];
			this.chkBirthYear.Text = LangMan.LSList[569];
			this.Label5.Text = LangMan.LSList[570];
			this.Label6.Text = LangMan.LSList[571];
			this.Label3.Text = LangMan.LSList[0];
			this.btnImportFileChoose.Text = LangMan.LSList[100] + "...";
			this.btnBaseRepair.Text = LangMan.LSList[572];
			this.Label8.Text = LangMan.LSList[573];
			this.btnSetPatriarch.Text = LangMan.LSList[574];
			this.btnPatSearch.Text = LangMan.LSList[175];
			this.btnIntoList.Text = LangMan.LSList[575];
		}

		static TfmTreeTools()
		{
			TfmTreeTools.HelpTopics = new string[]
			{
				"::/gkhTools_TreeCompare.htm", 
				"::/gkhTools_TreeMerge.htm", 
				"::/gkhTools_TreeSplit.htm", 
				"::/gkhTools_DubsMerge.htm", 
				"::/gkhTools_TreeImport.htm", 
				"::/gkhTools_FamiliesConnectivity.htm", 
				"::/gkhTools_TreeCheck.htm", 
				"::/gkhTools_PatSearch.htm", 
				"::/gkhTools_PlacesManage.htm"
			};
		}

		private void _CheckRelations_AddRel(TGEDCOMRecord aRec)
		{
			if (FSplitList.IndexOf(aRec) < 0)
			{
				FSplitList.Add(aRec);
			}
		}

		private void _CheckRelations_CheckRecord(TGEDCOMRecord rec)
		{
			int num = rec.MultimediaLinks.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				_CheckRelations_AddRel(rec.MultimediaLinks[i].Value);
			}

			int num2 = rec.Notes.Count - 1;
			for (int i = 0; i <= num2; i++)
			{
				_CheckRelations_AddRel(rec.Notes[i].Value);
			}

			int num3 = rec.SourceCitations.Count - 1;
			for (int i = 0; i <= num3; i++)
			{
				_CheckRelations_AddRel(rec.SourceCitations[i].Value);
			}
		}

		private void _CheckRelations_CheckTag(TGEDCOMTagWithLists tag)
		{
			int num = tag.MultimediaLinks.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				_CheckRelations_AddRel(tag.MultimediaLinks[i].Value);
			}

			int num2 = tag.Notes.Count - 1;
			for (int i = 0; i <= num2; i++)
			{
				_CheckRelations_AddRel(tag.Notes[i].Value);
			}

			int num3 = tag.SourceCitations.Count - 1;
			for (int i = 0; i <= num3; i++)
			{
				_CheckRelations_AddRel(tag.SourceCitations[i].Value);
			}
		}

		private void _CheckRelations_CheckIndividual(TGEDCOMIndividualRecord iRec)
		{
			_CheckRelations_CheckRecord(iRec);

			int num = iRec.ChildToFamilyLinks.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				_CheckRelations_AddRel(iRec.ChildToFamilyLinks[i].Family);
			}

			int num2 = iRec.SpouseToFamilyLinks.Count - 1;
			for (int i = 0; i <= num2; i++)
			{
				_CheckRelations_AddRel(iRec.SpouseToFamilyLinks[i].Family);
			}

			int num3 = iRec.IndividualEvents.Count - 1;
			for (int i = 0; i <= num3; i++)
			{
				_CheckRelations_CheckTag(iRec.IndividualEvents[i].Detail);
			}

			int num4 = iRec.IndividualOrdinances.Count - 1;
			for (int i = 0; i <= num4; i++)
			{
				_CheckRelations_CheckTag(iRec.IndividualOrdinances[i]);
			}

			int num5 = iRec.Submittors.Count - 1;
			for (int i = 0; i <= num5; i++)
			{
				_CheckRelations_AddRel(iRec.Submittors[i].Value);
			}

			int num6 = iRec.Associations.Count - 1;
			for (int i = 0; i <= num6; i++)
			{
				_CheckRelations_AddRel(iRec.Associations[i].Value);
			}

			int num7 = iRec.Aliasses.Count - 1;
			for (int i = 0; i <= num7; i++)
			{
				_CheckRelations_AddRel(iRec.Aliasses[i].Value);
			}

			int num8 = iRec.AncestorsInterest.Count - 1;
			for (int i = 0; i <= num8; i++)
			{
				_CheckRelations_AddRel(iRec.AncestorsInterest[i].Value);
			}

			int num9 = iRec.DescendantsInterest.Count - 1;
			for (int i = 0; i <= num9; i++)
			{
				_CheckRelations_AddRel(iRec.DescendantsInterest[i].Value);
			}

			int num10 = iRec.Groups.Count - 1;
			for (int i = 0; i <= num10; i++)
			{
				_CheckRelations_AddRel(iRec.Groups[i].Value);
			}
		}

		private void _CheckRelations_CheckFamily(TGEDCOMFamilyRecord fRec)
		{
			_CheckRelations_CheckRecord(fRec);

			int num = fRec.FamilyEvents.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				_CheckRelations_CheckTag(fRec.FamilyEvents[i].Detail);
			}
			_CheckRelations_AddRel(fRec.Submitter.Value);

			int num2 = fRec.SpouseSealings.Count - 1;
			for (int i = 0; i <= num2; i++)
			{
				_CheckRelations_CheckTag(fRec.SpouseSealings[i]);
			}
		}

		private void _CheckRelations_CheckSource(TGEDCOMSourceRecord sRec)
		{
			_CheckRelations_CheckRecord(sRec);

			int num = sRec.RepositoryCitations.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				_CheckRelations_AddRel(sRec.RepositoryCitations[i].Value);
			}
		}

		private string _btnPatSearch_Click_GetLinks([In] ref TList lst, TPatriarchObj pObj)
		{
			string Result = "";
			int num = pObj.ILinks.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				byte ix = pObj.ILinks[i];
				if (Result != "") Result += ", ";
				Result += (lst[ix] as TPatriarchObj).IRec.aux_GetNameStr(true, false);
			}
			return Result;
		}

		private void _CheckPlaces_PrepareEvent(TGEDCOMCustomEvent aEvent)
		{
			string place_str = aEvent.Detail.Place.StringValue;
			if (place_str != "")
			{
				TGEDCOMLocationRecord loc = aEvent.Detail.Place.Location.Value as TGEDCOMLocationRecord;
				if (loc != null)
				{
					place_str = "[*] " + place_str;
				}

				int idx = FPlaces.IndexOf(place_str);

				TPlaceObj place_obj;
				if (idx >= 0)
				{
					place_obj = (FPlaces.GetObject(idx) as TPlaceObj);
				}
				else
				{
					place_obj = new TPlaceObj();
					place_obj.Name = place_str;
					FPlaces.AddObject(place_str, place_obj);
				}
				place_obj.Facts.Add(aEvent);
			}
		}

		private void TreeCompare(TGEDCOMTree aMainTree, string aFileName)
		{
			TGEDCOMTree tempTree = new TGEDCOMTree();
			tempTree.LoadFromFile(aFileName);

			StringList fams = new StringList();
			StringList names = new StringList();

			try
			{
				this.ListCompare.AppendText(LangMan.LSList[520] + "\r\n");

				int num = aMainTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					if (aMainTree[i] is TGEDCOMIndividualRecord)
					{
						TGEDCOMIndividualRecord iRec = (TGEDCOMIndividualRecord)aMainTree[i];

						int idx = names.AddObject(iRec.aux_GetNameStr(true, false), new TList());
						(names.GetObject(idx) as TList).Add(iRec);

						string fam, nam, pat;
						iRec.aux_GetNameParts(out fam, out nam, out pat);

						fams.AddObject(TGenEngine.PrepareRusFamily(fam, iRec.Sex == TGEDCOMSex.svFemale), null);
					}
				}

				int num2 = tempTree.RecordsCount - 1;
				for (int i = 0; i <= num2; i++)
				{
					if (tempTree[i] is TGEDCOMIndividualRecord)
					{
						TGEDCOMIndividualRecord iRec = (TGEDCOMIndividualRecord)tempTree[i];

						string tm = iRec.aux_GetNameStr(true, false);
						int idx = names.IndexOf(tm);
						if (idx >= 0)
						{
							(names.GetObject(idx) as TList).Add(iRec);
						}

						string fam, nam, pat;
						iRec.aux_GetNameParts(out fam, out nam, out pat);

						tm = TGenEngine.PrepareRusFamily(fam, iRec.Sex == TGEDCOMSex.svFemale);
						idx = fams.IndexOf(tm);
						if (idx >= 0)
						{
							fams.PutObject(idx, 1);
						}
					}
				}

				for (int i = fams.Count - 1; i >= 0; i--)
				{
					if (fams.GetObject(i) == null || fams[i] == "?")
						fams.Delete(i);
				}

				for (int i = names.Count - 1; i >= 0; i--)
				{
					if ((names.GetObject(i) as TList).Count == 1)
					{
						(names.GetObject(i) as TList).Dispose();
						names.Delete(i);
					}
				}

				if (fams.Count != 0)
				{
					this.ListCompare.AppendText(LangMan.LSList[576] + "\r\n");

					int num3 = fams.Count - 1;
					for (int i = 0; i <= num3; i++)
					{
						this.ListCompare.AppendText("    " + fams[i] + "\r\n");
					}
				}

				if (names.Count != 0)
				{
					this.ListCompare.AppendText(LangMan.LSList[577] + "\r\n");

					int num4 = names.Count - 1;
					for (int i = 0; i <= num4; i++)
					{
						this.ListCompare.AppendText("    " + names[i] + "\r\n");
						TList lst = names.GetObject(i) as TList;

						int num5 = lst.Count - 1;
						for (int j = 0; j <= num5; j++)
						{
							TGEDCOMIndividualRecord iRec = lst[j] as TGEDCOMIndividualRecord;
							this.ListCompare.AppendText("      * " + iRec.aux_GetNameStr(true, false) + " " + TGenEngine.GetLifeStr(iRec) + "\r\n");
						}
					}
				}
			}
			finally
			{
				int num6 = names.Count - 1;
				for (int i = 0; i <= num6; i++)
				{
					SysUtils.Free(names.GetObject(i));
				}

				names.Free();
				fams.Free();

				tempTree.Dispose();
			}
		}

		private string external_match_db;
		private enum TreeMatchType { tmtInternal, tmtExternal, tmtAnalysis }

		void btnFileChoose_Click(object sender, EventArgs e)
		{
			if (this.OpenDialog1.ShowDialog() == DialogResult.OK)
			{
				external_match_db = this.OpenDialog1.FileName;
				this.edCompareFile.Text = Path.GetFileName(external_match_db);
			}
		}

		private void DuplicateFoundFunc(TGEDCOMIndividualRecord indivA, TGEDCOMIndividualRecord indivB)
		{
			this.ListCompare.AppendText("    * [" + indivA.aux_GetNameStr(true, false) + "]\r\n");
			this.ListCompare.AppendText("      [" + indivB.aux_GetNameStr(true, false) + "]\r\n");
		}

		private TreeMatchType GetTreeMatchType()
		{
			TreeMatchType type = 
				((rbtnMatchInternal.Checked) ? 
				 	TreeMatchType.tmtInternal : 
				 ((rbtnMathExternal.Checked) ? TreeMatchType.tmtExternal : TreeMatchType.tmtAnalysis));

			return type;
		}

		void btnMatch_Click(object sender, EventArgs e)
		{
			TreeMatchType type = GetTreeMatchType();

			this.ListCompare.Clear();

			switch (type) {
				case TreeMatchType.tmtInternal:
					{
						TGenEngine.FindDuplicates(this.FTree, this.FTree, 85 /*min: 80-85*/, DuplicateFoundFunc);
						break;
					}

				case TreeMatchType.tmtExternal:
					{
						this.TreeCompare(this.FTree, external_match_db);
						break;
					}

				case TreeMatchType.tmtAnalysis:
					{
						List<TGenEngine.ULIndividual> uln = FBase.Engine.GetUnlinkedNamesakes();

						this.ListCompare.AppendText("  Поиск несвязанных однофамильцев:\r\n");
						if (uln != null && uln.Count > 0)
						{
							for (int i = 0; i < uln.Count; i++)
							{
								TGenEngine.ULIndividual indiv = uln[i];
								this.ListCompare.AppendText("    - [" + indiv.Family + "] " + indiv.iRec.aux_GetNameStr(true, false) + "\r\n");
							}
						}
						else
						{
							this.ListCompare.AppendText("    - not found.");
						}
						break;
					}
			}
		}

		void rbtnMatch_CheckedChanged(object sender, EventArgs e)
		{
			TreeMatchType type = GetTreeMatchType();

			this.Label1.Enabled = (type == TreeMatchType.tmtExternal);
			this.edCompareFile.Enabled = (type == TreeMatchType.tmtExternal);
			this.btnFileChoose.Enabled = (type == TreeMatchType.tmtExternal);
		}

		void BtnPatriarchsDiagramClick(object sender, EventArgs e)
		{
			PatriarchsViewer wnd = new PatriarchsViewer(FBase, decimal.ToInt32(this.edMinGens.Value));
			wnd.Show();
		}
	}
}

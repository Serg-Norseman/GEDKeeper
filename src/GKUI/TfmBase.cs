using System;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Runtime.InteropServices;
using System.Windows.Forms;

using GedCom551;
using GKCore;
using GKCore.Commands;
using GKCore.IO;
using GKSys;
using GKUI.Charts;
using GKUI.Controls;
using GKUI.Lists;

/// <summary>
/// Localization: unknown
/// </summary>

namespace GKUI
{
	public partial class TfmBase : Form, ILocalization
	{
		private NavManager FNavman;
		private TList[] FChangedRecords = new TList[14];
		private TGenEngine FEngine;
		private TList FLockedRecords;
		private bool FModified;
		private TGenEngine.TShieldState FShieldState;
		private TGEDCOMTree FTree;
		private UndoManager FUndoman;
		private TPersonsFilter FXFilter;
		public TabControl PageRecords;
		public TRecordsView ListPersons;
		public TGKHyperView mPersonSummary;
		public TRecordsView ListFamilies;
		public TGKHyperView mFamilySummary;
		public TRecordsView ListNotes;
		public TGKHyperView mNoteSummary;
		public TRecordsView ListMultimedia;
		public TGKHyperView mMediaSummary;
		public TRecordsView ListSources;
		public TGKHyperView mSourceSummary;
		public TRecordsView ListRepositories;
		public TGKHyperView mRepositorySummary;
		public TRecordsView ListGroups;
		public TGKHyperView mGroupSummary;
		public TRecordsView ListResearches;
		public TGKHyperView mResearchSummary;
		public TRecordsView ListTasks;
		public TGKHyperView mTaskSummary;
		public TRecordsView ListCommunications;
		public TGKHyperView mCommunicationSummary;
		public TRecordsView ListLocations;
		public TGKHyperView mLocationSummary;

		public NavManager Navman
		{
			get { return this.FNavman; }
		}

		public TGenEngine Engine
		{
			get { return this.FEngine; }
		}

		public string FileName
		{
			get { return this.GetFileName(); }
			set { this.SetFileName(value); }
		}

		public TPersonsFilter Filter
		{
			get { return this.FXFilter; }
		}

		public bool Modified
		{
			get { return this.FModified; }
			set { this.SetModified(value); }
		}

		public TGenEngine.TShieldState ShieldState
		{
			get { return this.FShieldState; }
			set { this.SetShieldState(value); }
		}

		public TGEDCOMTree Tree
		{
			get { return this.FTree; }
		}

		public UndoManager Undoman
		{
			get { return this.FUndoman; }
		}

		private void OutLink(TGEDCOMRecord aSubject, StringList aToList, TGEDCOMRecord aRec, TGEDCOMTag aTag, TGEDCOMPointer aExt)
		{
			string prefix;
			if (aSubject is TGEDCOMSourceRecord && aExt != null)
			{
				if ((aExt as TGEDCOMSourceCitation).Page != "")
				{
					prefix = (aExt as TGEDCOMSourceCitation).Page + ": ";
				}
				else
				{
					prefix = "";
				}
			}
			else
			{
				prefix = "";
			}

			string suffix;
			if (aTag != null && aTag is TGEDCOMCustomEvent)
			{
				suffix = ", " + TGenEngine.GetEventName(aTag as TGEDCOMCustomEvent).ToLower();
			}
			else
			{
				suffix = "";
			}
			aToList.Add("    " + prefix + TGenEngine.GenRecordLink(this.FTree, aRec, true) + suffix);
		}

		private void PrepareEvent(TGEDCOMRecord aSubject, StringList aToList, TGEDCOMRecord aRec, TGEDCOMCustomEvent @event)
		{
			if (aSubject is TGEDCOMNoteRecord)
			{
				int num = @event.Detail.Notes.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					if (@event.Detail.Notes[i].Value == aSubject)
					{
						this.OutLink(aSubject, aToList, aRec, @event, null);
					}
				}
			}
			else
			{
				if (aSubject is TGEDCOMMultimediaRecord)
				{
					int num2 = @event.Detail.MultimediaLinks.Count - 1;
					for (int i = 0; i <= num2; i++)
					{
						if (@event.Detail.MultimediaLinks[i].Value == aSubject)
						{
							this.OutLink(aSubject, aToList, aRec, @event, null);
						}
					}
				}
				else
				{
					if (aSubject is TGEDCOMSourceRecord)
					{
						int num3 = @event.Detail.SourceCitations.Count - 1;
						for (int i = 0; i <= num3; i++)
						{
							if (@event.Detail.SourceCitations[i].Value == aSubject)
							{
								this.OutLink(aSubject, aToList, aRec, @event, @event.Detail.SourceCitations[i]);
							}
						}
					}
				}
			}
		}

		private TGEDCOMFamilyRecord GetFamilyBySpouse(TGEDCOMIndividualRecord aNewParent)
		{
			TGEDCOMFamilyRecord result = null;

			int num = this.FTree.RecordsCount - 1;
			for (int i = 0; i <= num; i++)
			{
				if (this.FTree.GetRecord(i) is TGEDCOMFamilyRecord)
				{
					TGEDCOMFamilyRecord fam = (TGEDCOMFamilyRecord)this.FTree.GetRecord(i);
					TGEDCOMIndividualRecord husb = fam.Husband.Value as TGEDCOMIndividualRecord;
					TGEDCOMIndividualRecord wife = fam.Wife.Value as TGEDCOMIndividualRecord;
					if (husb == aNewParent || wife == aNewParent)
					{
						string msg = string.Format(LangMan.LSList[70], TGenEngine.GetFamilyStr(fam));
						if (TGenEngine.ShowQuestion(msg) == DialogResult.Yes)
						{
							result = fam;
							break;
						}
					}
				}
			}

			return result;
		}

		private void List_SelectedIndexChanged(object sender, EventArgs e)
		{
			TGEDCOMRecord data = (sender as TRecordsView).GetSelectedRecord();
			if (data != null)
			{
				this.NavAdd(data);
			}
			this.ShowRecordInfo(data);
		}

		private void NavAdd(TGEDCOMRecord aRec)
		{
			if (aRec != null && !this.FNavman.Busy)
			{
				this.FNavman.Current = aRec;
				GKUI.TfmGEDKeeper.Instance.UpdateControls(false);
			}
		}

		private void SetFileName([In] string Value)
		{
			this.FEngine.FileName = Value;
			this.SetMainTitle();
			GKUI.TfmGEDKeeper.Instance.Options.LastDir = Path.GetDirectoryName(this.FEngine.FileName);
		}

		private void SetMainTitle()
		{
			this.Text = Path.GetFileName(this.FileName);
			if (this.FModified)
			{
				this.Text = "* " + this.Text;
			}
		}

		private void SetModified([In] bool Value)
		{
			this.FModified = Value;
			this.SetMainTitle();
		}

		private void ShowAddress(TGEDCOMAddress anAddress, StringList aSummary)
		{
			if (!anAddress.IsEmpty() && aSummary != null)
			{
				aSummary.Add("    " + LangMan.LSList[82] + ":");

				string ts = "";
				if (anAddress.AddressCountry != "")
				{
					ts = ts + anAddress.AddressCountry + ", ";
				}
				if (anAddress.AddressState != "")
				{
					ts = ts + anAddress.AddressState + ", ";
				}
				if (anAddress.AddressCity != "")
				{
					ts += anAddress.AddressCity;
				}
				if (ts != "")
				{
					aSummary.Add("    " + ts);
				}

				ts = "";
				if (anAddress.AddressPostalCode != "")
				{
					ts = ts + anAddress.AddressPostalCode + ", ";
				}
				if (anAddress.Address.Text.Trim() != "")
				{
					ts += anAddress.Address.Text.Trim();
				}
				if (ts != "")
				{
					aSummary.Add("    " + ts);
				}

				int num = anAddress.GetPhoneNumbersCount() - 1;
				for (int i = 0; i <= num; i++)
				{
					aSummary.Add("    " + anAddress.GetPhoneNumber(i));
				}

				int num2 = anAddress.GetEmailAddressesCount() - 1;
				for (int i = 0; i <= num2; i++)
				{
					aSummary.Add("    " + anAddress.GetEmailAddress(i));
				}

				int num3 = anAddress.GetWebPagesCount() - 1;
				for (int i = 0; i <= num3; i++)
				{
					aSummary.Add("    " + anAddress.GetWebPage(i));
				}
			}
		}

		private void SetShieldState([In] TGenEngine.TShieldState Value)
		{
			bool up = (this.FShieldState != TGenEngine.TShieldState.ssNone && Value == TGenEngine.TShieldState.ssNone) || (this.FShieldState == TGenEngine.TShieldState.ssNone && Value != TGenEngine.TShieldState.ssNone);
			this.FShieldState = Value;
			if (up)
			{
				this.ListsRefresh(false);
			}
		}

		private string GetFileName()
		{
			return this.FEngine.FileName;
		}

		private void mPersonSummaryLink(object sender, string LinkName)
		{
			if (SysUtils.Pos("view_", LinkName) > 0)
			{
				string xref = LinkName.Remove(0, 5);
				TGEDCOMRecord rec = this.FTree.XRefIndex_Find(xref);
				if (rec != null)
				{
					this.ShowMedia(rec as TGEDCOMMultimediaRecord);
				}
			}
			else
			{
				this.SelectRecordByXRef(LinkName);
			}
		}

		private void FormActivate(object sender, EventArgs e)
		{
			GKUI.TfmGEDKeeper.Instance.UpdateControls(false);
			if (GKUI.TfmGEDKeeper.Instance.fmTimeLine != null)
			{
				GKUI.TfmGEDKeeper.Instance.fmTimeLine.CheckTimeLine(this);
			}
		}

		private void FormDeactivate(object sender, EventArgs e)
		{
			TfmGEDKeeper.Instance.UpdateControls(true);
			if (TfmGEDKeeper.Instance.fmTimeLine != null)
			{
				TfmGEDKeeper.Instance.fmTimeLine.CheckTimeLine(null);
			}
		}

		private void PageRecords_SelectedIndexChanged(object sender, EventArgs e)
		{
			TfmGEDKeeper.Instance.UpdateControls(false);
		}

		private void TfmBase_Closing(object sender, CancelEventArgs e)
		{
			e.Cancel = !this.CheckModified();

			if (!e.Cancel)
			{
				GKUI.TfmGEDKeeper.Instance.CheckMRUWin(this.FileName, this);
			}
		}

		protected override void Dispose(bool Disposing)
		{
			if (Disposing)
			{
				this.FNavman.Dispose();
				this.FUndoman.Dispose();
				this.FLockedRecords.Dispose();
				this.FXFilter.Free();
				this.FTree = null;
				this.FEngine.Dispose();
				for (TGEDCOMRecordType rt = TGEDCOMRecordType.rtNone; rt != TGEDCOMRecordType.rtLast; rt++)
				{
					this.FChangedRecords[(int)rt].Dispose();
				}
			}
			base.Dispose(Disposing);
		}

		public TfmBase()
		{
			this.InitializeComponent();
			for (TGEDCOMRecordType rt = TGEDCOMRecordType.rtNone; rt != TGEDCOMRecordType.rtLast; rt++)
			{
				this.FChangedRecords[(int)rt] = new TList();
			}
			this.FEngine = new TGenEngine();
			this.FTree = this.FEngine.Tree;
			this.FXFilter = new TPersonsFilter();
			this.FLockedRecords = new TList();
			this.FNavman = new NavManager();
			this.FUndoman = new UndoManager(this.FTree, UndoManager.TUndoManType.manualCommit);
			this.CreatePage(LangMan.LSList[52], TGEDCOMRecordType.rtIndividual, ref this.ListPersons, ref this.mPersonSummary);
			this.CreatePage(LangMan.LSList[53], TGEDCOMRecordType.rtFamily, ref this.ListFamilies, ref this.mFamilySummary);
			this.CreatePage(LangMan.LSList[54], TGEDCOMRecordType.rtNote, ref this.ListNotes, ref this.mNoteSummary);
			this.CreatePage(LangMan.LSList[55], TGEDCOMRecordType.rtMultimedia, ref this.ListMultimedia, ref this.mMediaSummary);
			this.CreatePage(LangMan.LSList[56], TGEDCOMRecordType.rtSource, ref this.ListSources, ref this.mSourceSummary);
			this.CreatePage(LangMan.LSList[57], TGEDCOMRecordType.rtRepository, ref this.ListRepositories, ref this.mRepositorySummary);
			this.CreatePage(LangMan.LSList[58], TGEDCOMRecordType.rtGroup, ref this.ListGroups, ref this.mGroupSummary);
			this.CreatePage(LangMan.LSList[59], TGEDCOMRecordType.rtResearch, ref this.ListResearches, ref this.mResearchSummary);
			this.CreatePage(LangMan.LSList[60], TGEDCOMRecordType.rtTask, ref this.ListTasks, ref this.mTaskSummary);
			this.CreatePage(LangMan.LSList[61], TGEDCOMRecordType.rtCommunication, ref this.ListCommunications, ref this.mCommunicationSummary);
			this.CreatePage(LangMan.LSList[62], TGEDCOMRecordType.rtLocation, ref this.ListLocations, ref this.mLocationSummary);
			this.PageRecords.SelectedIndex = 0;
		}

		public void ApplyFilter()
		{
			if (this.FTree.RecordsCount > 0)
			{
				this.ListsRefresh(false);
			}
		}

		public void ChangeRecord(TGEDCOMRecord record)
		{
			if (record != null)
			{
				int rt = (int)record.RecordType;
				this.FChangedRecords[rt].Add(record);
				record.ChangeDate.ChangeDateTime = DateTime.Now;
				this.Modified = true;

				this.FTree.Header.TransmissionDateTime = DateTime.Now;

				TfmGEDKeeper.Instance.SearchMan.UpdateRecord(this, record);
			}
		}

		public void ChangesClear()
		{
			for (TGEDCOMRecordType rt = TGEDCOMRecordType.rtNone; rt != TGEDCOMRecordType.rtLast; rt++)
			{
				this.FChangedRecords[(int)rt].Clear();
			}
		}

		public bool CheckModified()
		{
			bool result = true;

			if (this.Modified)
			{
				DialogResult dialogResult = MessageBox.Show(LangMan.LSList[69], "GEDKeeper2", MessageBoxButtons.YesNoCancel, MessageBoxIcon.Exclamation);

				switch (dialogResult) {
					case DialogResult.Yes:
						GKUI.TfmGEDKeeper.Instance.miFileSaveClick(null, null);
						break;
					case DialogResult.No:
						break;
					case DialogResult.Cancel:
						result = false;
						break;
				}
			}

			return result;
		}

		public void Clear()
		{
			this.FTree.Clear();
			this.FNavman.Clear();
			this.FUndoman.Clear();
		}

		public void CreateListView(Control aOwner, ref TGKListView aList)
		{
			aList = new TGKListView(null);
			aList.HideSelection = false;
			aList.LabelEdit = false;
			aList.FullRowSelect = true;
			aList.View = View.Details;
			aList.Dock = DockStyle.Fill;
			aOwner.Controls.Add(aList);
		}

		private void CreatePage(string aPageText, TGEDCOMRecordType aRecType, ref TRecordsView aList, ref TGKHyperView aSummary)
		{
			this.PageRecords.SuspendLayout();

			TabPage sheet = new TabPage();
			sheet.Text = aPageText;
			this.PageRecords.Controls.Add(sheet);
			this.PageRecords.ResumeLayout(false);

			aSummary = new TGKHyperView();
			aSummary.BorderWidth = 4;
			aSummary.Dock = DockStyle.Right;
			aSummary.Size = new Size(300, 290);
			aSummary.OnLink += new TGKHyperView.TLinkEvent(this.mPersonSummaryLink);

			Splitter spl = new Splitter();
			spl.Dock = DockStyle.Right;
			spl.Size = new Size(4, 290);
			spl.MinExtra = 100;
			spl.MinSize = 100;

			sheet.Controls.Add(aSummary);
			sheet.Controls.Add(spl);

			this.CreateRecordsView(sheet, aRecType, ref aList);
			aList.IsMainList = true;
			aList.DoubleClick += new EventHandler(this.RecordEdit);
			aList.SelectedIndexChanged += new EventHandler(this.List_SelectedIndexChanged);
			aList.UpdateTitles();

			sheet.Controls.SetChildIndex(spl, 1);
			sheet.Controls.SetChildIndex(aSummary, 2);
		}

		void TfmBaseKeyDown(object sender, KeyEventArgs e)
		{
			if (e.Control)
			{
				switch (e.KeyCode) {
					/*case Keys.I:
						this.ItemAdd();
						break;
					case Keys.D:
						this.ItemDelete();
						break;*/
					case Keys.Return:
						this.RecordEdit(null, null);
						break;
				}
			}
		}

		public TGEDCOMIndividualRecord CreatePersonDialog(TGEDCOMIndividualRecord aTarget, TGenEngine.TTargetMode aTargetMode, TGEDCOMSex aNeedSex)
		{
			TGEDCOMIndividualRecord result = null;
			TfmPersonNew dlg = new TfmPersonNew();
			try
			{
				dlg.EditSex.SelectedIndex = (int)aNeedSex;
				dlg.TargetMode = aTargetMode;
				dlg.Target = aTarget;
				if (GKUI.TfmGEDKeeper.Instance.ShowModalEx(dlg, this, false) == DialogResult.OK)
				{
					result = TGenEngine.CreatePersonEx(this.FTree, dlg.edName.Text, dlg.edPatronymic.Text, dlg.edFamily.Text, (TGEDCOMSex)dlg.EditSex.SelectedIndex, true);
					this.ChangeRecord(result);
				}
			}
			finally
			{
				SysUtils.Free(dlg);
			}
			return result;
		}

		public void CreateRecordsView(Control aParent, TGEDCOMRecordType aRecordType, ref TRecordsView aList)
		{
			aList = new TRecordsView(this);
			aList.HideSelection = false;
			aList.LabelEdit = false;
			aList.FullRowSelect = true;
			aList.View = View.Details;
			aList.Tree = this.FTree;
			aList.RecordType = aRecordType;
			aList.Dock = DockStyle.Fill;
			aParent.Controls.Add(aList);
			aParent.Controls.SetChildIndex(aList, 0);
		}

		public void ExportToExcel(bool appmode)
		{
			ExcelExporter ex_exp = new ExcelExporter(this.FEngine, this.GetCurFileTempPath());
			try
			{
				ex_exp.Options = GKUI.TfmGEDKeeper.Instance.Options;
				ex_exp.SelectedRecords = this.ListPersons.ContentList;
				ex_exp.AppMode = appmode;
				ex_exp.Generate();
			}
			finally
			{
				ex_exp.Dispose();
			}
		}

		public void ExportToWeb()
		{
			WebExporter web = new WebExporter(this.FEngine, this.GetCurFileTempPath());
			try
			{
				web.Generate();
			}
			finally
			{
				web.Dispose();
			}
		}

		public string GetRestoreFilename(string aFileName)
		{
			string rfn = Path.ChangeExtension(aFileName, ".restore");
			return rfn;
		}

		private void LoadProgress(object sender, EventArgs e)
		{
			TfmProgress.Progress = this.FTree.Progress;
		}

		public void FileLoad(string aFileName)
		{
			this.ChangesClear();
			this.Clear();

			//try
			//{
			TfmProgress.ProgressInit(100, LangMan.LS(LSID.LSID_Loading));
			try
			{
				this.FTree.ProgressEvent += LoadProgress;
				this.FTree.LoadFromFile(aFileName);
				this.FTree.ProgressEvent -= LoadProgress;
			}
			finally
			{
				TfmProgress.ProgressDone();
			}
			//}
			//catch (Exception E)
			//{
			//	SysUtils.LogWrite("GKBase.FileLoad().TreeLoad(): " + E.Message);
			//	SysUtils.ShowError(LangMan.LSList[245]);
			//}

			try
			{
				TGenEngine.CheckGEDCOMFormat(this.FTree);
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.FileLoad().CheckFormat(): " + E.Message);
				TGenEngine.ShowError(LangMan.LSList[246]);
			}

			this.FileName = aFileName;
			this.Modified = false;

			TfmGEDKeeper.Instance.AddMRU(aFileName);

			this.ListsRefresh(false);
			this.ShowTips();
		}

		public void FileNew()
		{
			this.ChangesClear();
			this.Clear();
			this.ListsRefresh(false);
			this.ShowPersonInfo(null, this.mPersonSummary.Lines);
			this.FileName = LangMan.LSList[165];
			this.Modified = false;
		}

		public DialogResult FileProperties()
		{
			TfmFileProperties fp_dlg = new TfmFileProperties(this);
			DialogResult Result;
			try
			{
				Result = GKUI.TfmGEDKeeper.Instance.ShowModalEx(fp_dlg, this, false);
			}
			finally
			{
				fp_dlg.Dispose();
			}
			return Result;
		}

		public void CheckMediaContainers(string oldFileName, string newFileName)
		{
			bool has_arc = File.Exists(this.Engine.GetArcFileName());
			bool has_stg = Directory.Exists(this.Engine.GetStgFolder(false));
			string old_path = Path.GetDirectoryName(oldFileName);
			string new_path = Path.GetDirectoryName(newFileName);

			if (!string.Equals(old_path, new_path)) {
				// переместить архив и хранилище
				if (has_arc) {
					string new_arc = new_path + "\\" + this.Engine.GetContainerName(true);
					File.Move(this.Engine.GetArcFileName(), new_arc);
				}

				if (has_stg) {
					string new_stg = new_path + "\\" + this.Engine.GetContainerName(false);
					Directory.Move(this.Engine.GetStgFolder(false), new_stg);
				}
			}
		}

		public void FileSave([In] string aFileName)
		{
			try
			{
				if (GKUI.TfmGEDKeeper.Instance.Options.RevisionsBackup)
				{
					int rev = this.Tree.Header.FileRevision;
					if (File.Exists(aFileName)) 
					{
						string bak_path = Path.GetDirectoryName(aFileName) + "\\__history\\";
						string bak_file = Path.GetFileName(aFileName) + "." + SysUtils.NumUpdate(rev, 3);

						if (!Directory.Exists(bak_path)) Directory.CreateDirectory(bak_path);
						File.Move(aFileName, bak_path + bak_file);
					}

					// fixme: обязательная чистка файлов истории, установить границу в 2-5 кб
				}

				// проверка наличия архива и хранилища, перемещение их, если файл изменил местоположение
				CheckMediaContainers(this.FileName, aFileName);

				this.FTree.SaveToFile(aFileName, GKUI.TfmGEDKeeper.Instance.Options.DefCharacterSet);

				this.FileName = aFileName;
				GKUI.TfmGEDKeeper.Instance.AddMRU(aFileName);
				this.Modified = false;
			}
			catch (UnauthorizedAccessException)
			{
				TGenEngine.ShowError(string.Format(LangMan.LS(LSID.LSID_FileSaveError), new object[] { aFileName, ": доступ закрыт" }));
			}
			catch (Exception E)
			{
				TGenEngine.ShowError(string.Format(LangMan.LS(LSID.LSID_FileSaveError), new object[] { aFileName, "" }));
				SysUtils.LogWrite("GKBase.FileSave(): " + E.Message);
			}
		}

		public void GenPedigree_dAboville()
		{
			TPedigree p = new TPedigree(this.FEngine, this.GetCurFileTempPath());
			try
			{
				p.Ancestor = this.GetSelectedPerson();
				p.Options = GKUI.TfmGEDKeeper.Instance.Options;
				p.ShieldState = this.FShieldState;
				p.Kind = TPedigree.TPedigreeKind.pk_dAboville;
				p.Generate();
			}
			finally
			{
				p.Dispose();
			}
		}

		public void GenPedigree_Konovalov()
		{
			TPedigree p = new TPedigree(this.FEngine, this.GetCurFileTempPath());
			try
			{
				p.Ancestor = this.GetSelectedPerson();
				p.Options = GKUI.TfmGEDKeeper.Instance.Options;
				p.ShieldState = this.FShieldState;
				p.Kind = TPedigree.TPedigreeKind.pk_Konovalov;
				p.Generate();
			}
			finally
			{
				p.Dispose();
			}
		}

		public TGEDCOMFamilyRecord GetChildFamily(TGEDCOMIndividualRecord iChild, bool aCanCreate, TGEDCOMIndividualRecord aNewParent)
		{
			TGEDCOMFamilyRecord Result = null;
			if (iChild != null)
			{
				if (iChild.ChildToFamilyLinks.Count != 0)
				{
					Result = iChild.ChildToFamilyLinks[0].Family;
				}
				else
				{
					if (aCanCreate)
					{
						TGEDCOMFamilyRecord fam = this.GetFamilyBySpouse(aNewParent);
						if (fam == null)
						{
							fam = TGenEngine.CreateFamilyEx(this.FTree);
						}
						this.FEngine.AddFamilyChild(fam, iChild);
						Result = fam;
					}
				}
			}
			return Result;
		}

		public string GetCurFileTempPath()
		{
			return Path.GetDirectoryName(this.FileName) + "\\~temp\\";
		}

		public TGEDCOMIndividualRecord GetSelectedPerson()
		{
			return this.ListPersons.GetSelectedRecord() as TGEDCOMIndividualRecord;
		}

		private void IntUpdate(TRecordsView aRecView, int ASCol, bool aTitles)
		{
			TGEDCOMRecord rec = aRecView.GetSelectedRecord();
			aRecView.UpdateContents(this.FShieldState, aTitles, this.FXFilter, ASCol);
			if (rec != null) aRecView.SelectItemByRec(rec);
		}

		public void ListsRefresh(bool aTitles)
		{
			this.IntUpdate(this.ListPersons, 2, aTitles);
			this.IntUpdate(this.ListFamilies, 1, aTitles);
			this.IntUpdate(this.ListNotes, -1, aTitles);
			this.IntUpdate(this.ListMultimedia, 1, aTitles);
			this.IntUpdate(this.ListSources, 1, aTitles);
			this.IntUpdate(this.ListRepositories, 1, aTitles);
			this.IntUpdate(this.ListGroups, 1, aTitles);
			this.IntUpdate(this.ListResearches, 1, aTitles);
			this.IntUpdate(this.ListTasks, 1, aTitles);
			this.IntUpdate(this.ListCommunications, 1, aTitles);
			this.IntUpdate(this.ListLocations, 1, aTitles);

			this.PageRecords_SelectedIndexChanged(null, null);

			if (GKUI.TfmGEDKeeper.Instance.fmTimeLine != null)
			{
				GKUI.TfmGEDKeeper.Instance.fmTimeLine.UpdateControls();
			}
		}

		public void NavNext()
		{
			this.FNavman.BeginNav();
			try
			{
				TGEDCOMRecord rec = this.FNavman.Next() as TGEDCOMRecord;
				this.SelectRecordByXRef(rec.XRef);
				GKUI.TfmGEDKeeper.Instance.UpdateControls(false);
			}
			finally
			{
				this.FNavman.EndNav();
			}
		}

		public void NavPrev()
		{
			this.FNavman.BeginNav();
			try
			{
				TGEDCOMRecord rec = this.FNavman.Back() as TGEDCOMRecord;
				this.SelectRecordByXRef(rec.XRef);
				GKUI.TfmGEDKeeper.Instance.UpdateControls(false);
			}
			finally
			{
				this.FNavman.EndNav();
			}
		}

		public void PersonScan()
		{
			TfmPersonScan fmPersonScan = new TfmPersonScan(this);
			try
			{
				GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmPersonScan, this, false);
			}
			finally
			{
				SysUtils.Free(fmPersonScan);
			}
		}

		public void RecordAdd()
		{
			TGEDCOMRecord rec = null;
			bool res = false;
			switch (this.PageRecords.SelectedIndex)
			{
				case 0:
				{
					rec = this.CreatePersonDialog(null, TGenEngine.TTargetMode.tmParent, TGEDCOMSex.svNone);
					res = (rec != null);
					break;
				}
				case 1:
				{
					TGEDCOMFamilyRecord fam = rec as TGEDCOMFamilyRecord;
					res = this.ModifyFamily(ref fam, TGenEngine.TFamilyTarget.ftNone, null);
					rec = fam;
					break;
				}
				case 2:
				{
					TGEDCOMNoteRecord note = rec as TGEDCOMNoteRecord;
					res = this.ModifyNote(ref note);
					rec = note;
					break;
				}
				case 3:
				{
					TGEDCOMMultimediaRecord mm_rec = rec as TGEDCOMMultimediaRecord;
					res = this.ModifyMedia(ref mm_rec);
					rec = mm_rec;
					break;
				}
				case 4:
				{
					TGEDCOMSourceRecord src = rec as TGEDCOMSourceRecord;
					res = this.ModifySource(ref src);
					rec = src;
					break;
				}
				case 5:
				{
					TGEDCOMRepositoryRecord rep = rec as TGEDCOMRepositoryRecord;
					res = this.ModifyRepository(ref rep);
					rec = rep;
					break;
				}
				case 6:
				{
					TGEDCOMGroupRecord grp = rec as TGEDCOMGroupRecord;
					res = this.ModifyGroup(ref grp);
					rec = grp;
					break;
				}
				case 7:
				{
					TGEDCOMResearchRecord rsr = rec as TGEDCOMResearchRecord;
					res = this.ModifyResearch(ref rsr);
					rec = rsr;
					break;
				}
				case 8:
				{
					TGEDCOMTaskRecord tsk = rec as TGEDCOMTaskRecord;
					res = this.ModifyTask(ref tsk);
					rec = tsk;
					break;
				}
				case 9:
				{
					TGEDCOMCommunicationRecord comm = rec as TGEDCOMCommunicationRecord;
					res = this.ModifyCommunication(ref comm);
					rec = comm;
					break;
				}
				case 10:
				{
					TGEDCOMLocationRecord loc = rec as TGEDCOMLocationRecord;
					res = this.ModifyLocation(ref loc);
					rec = loc;
					break;
				}
			}
			if (res)
			{
				this.ListsRefresh(false);
				this.SelectRecordByXRef(rec.XRef);
			}
		}

		public void RecordDelete()
		{
			bool res = false;
			switch (this.PageRecords.SelectedIndex)
			{
				case 0:
				{
					res = this.DeleteRecord(this.ListPersons.GetSelectedRecord(), true);
					break;
				}
				case 1:
				{
					res = this.DeleteRecord(this.ListFamilies.GetSelectedRecord(), true);
					break;
				}
				case 2:
				{
					res = this.DeleteRecord(this.ListNotes.GetSelectedRecord(), true);
					break;
				}
				case 3:
				{
					res = this.DeleteRecord(this.ListMultimedia.GetSelectedRecord(), true);
					break;
				}
				case 4:
				{
					res = this.DeleteRecord(this.ListSources.GetSelectedRecord(), true);
					break;
				}
				case 5:
				{
					res = this.DeleteRecord(this.ListRepositories.GetSelectedRecord(), true);
					break;
				}
				case 6:
				{
					res = this.DeleteRecord(this.ListGroups.GetSelectedRecord(), true);
					break;
				}
				case 7:
				{
					res = this.DeleteRecord(this.ListResearches.GetSelectedRecord(), true);
					break;
				}
				case 8:
				{
					res = this.DeleteRecord(this.ListTasks.GetSelectedRecord(), true);
					break;
				}
				case 9:
				{
					res = this.DeleteRecord(this.ListCommunications.GetSelectedRecord(), true);
					break;
				}
				case 10:
				{
					res = this.DeleteRecord(this.ListLocations.GetSelectedRecord(), true);
					break;
				}
			}
			if (res)
			{
				this.ListsRefresh(false);
			}
		}

		public void RecordEdit(object sender, EventArgs e)
		{
			bool res = false;
			TGEDCOMRecord rec = null;
			switch (this.PageRecords.SelectedIndex)
			{
				case 0:
				{
					rec = this.ListPersons.GetSelectedRecord();
					TGEDCOMIndividualRecord ind = rec as TGEDCOMIndividualRecord;
					res = this.ModifyPerson(ref ind);
					break;
				}
				case 1:
				{
					rec = this.ListFamilies.GetSelectedRecord();
					TGEDCOMFamilyRecord fam = rec as TGEDCOMFamilyRecord;
					res = this.ModifyFamily(ref fam, TGenEngine.TFamilyTarget.ftNone, null);
					break;
				}
				case 2:
				{
					rec = this.ListNotes.GetSelectedRecord();
					TGEDCOMNoteRecord note = rec as TGEDCOMNoteRecord;
					res = this.ModifyNote(ref note);
					break;
				}
				case 3:
				{
					rec = this.ListMultimedia.GetSelectedRecord();
					TGEDCOMMultimediaRecord mm_rec = rec as TGEDCOMMultimediaRecord;
					res = this.ModifyMedia(ref mm_rec);
					break;
				}
				case 4:
				{
					rec = this.ListSources.GetSelectedRecord();
					TGEDCOMSourceRecord src = rec as TGEDCOMSourceRecord;
					res = this.ModifySource(ref src);
					break;
				}
				case 5:
				{
					rec = this.ListRepositories.GetSelectedRecord();
					TGEDCOMRepositoryRecord rep = rec as TGEDCOMRepositoryRecord;
					res = this.ModifyRepository(ref rep);
					break;
				}
				case 6:
				{
					rec = this.ListGroups.GetSelectedRecord();
					TGEDCOMGroupRecord grp = rec as TGEDCOMGroupRecord;
					res = this.ModifyGroup(ref grp);
					break;
				}
				case 7:
				{
					rec = this.ListResearches.GetSelectedRecord();
					TGEDCOMResearchRecord rsr = rec as TGEDCOMResearchRecord;
					res = this.ModifyResearch(ref rsr);
					break;
				}
				case 8:
				{
					rec = this.ListTasks.GetSelectedRecord();
					TGEDCOMTaskRecord tsk = rec as TGEDCOMTaskRecord;
					res = this.ModifyTask(ref tsk);
					break;
				}
				case 9:
				{
					rec = this.ListCommunications.GetSelectedRecord();
					TGEDCOMCommunicationRecord comm = rec as TGEDCOMCommunicationRecord;
					res = this.ModifyCommunication(ref comm);
					break;
				}
				case 10:
				{
					rec = this.ListLocations.GetSelectedRecord();
					TGEDCOMLocationRecord loc = rec as TGEDCOMLocationRecord;
					res = this.ModifyLocation(ref loc);
					break;
				}
			}
			if (res)
			{
				this.ListsRefresh(false);
				this.ShowRecordInfo(rec);
			}
		}

		public TRecordsView GetRecordsViewByType(TGEDCOMRecordType aType)
		{
			TRecordsView list = null;
			switch (aType) {
				case TGEDCOMRecordType.rtIndividual:
					list = this.ListPersons;
					break;
				case TGEDCOMRecordType.rtFamily:
					list = this.ListFamilies;
					break;
				case TGEDCOMRecordType.rtNote:
					list = this.ListNotes;
					break;
				case TGEDCOMRecordType.rtMultimedia:
					list = this.ListMultimedia;
					break;
				case TGEDCOMRecordType.rtSource:
					list = this.ListSources;
					break;
				case TGEDCOMRecordType.rtRepository:
					list = this.ListRepositories;
					break;
				case TGEDCOMRecordType.rtGroup:
					list = this.ListGroups;
					break;
				case TGEDCOMRecordType.rtResearch:
					list = this.ListResearches;
					break;
				case TGEDCOMRecordType.rtTask:
					list = this.ListTasks;
					break;
				case TGEDCOMRecordType.rtCommunication:
					list = this.ListCommunications;
					break;
				case TGEDCOMRecordType.rtLocation:
					list = this.ListLocations;
					break;
			}
			return list;
		}

		public void RecordNotify(TGEDCOMRecord record, TRecNotify notify)
		{
			if (record != null)
			{
				TRecordsView list = GetRecordsViewByType(record.RecordType);

				if (list != null && notify == TRecNotify.rnDelete)
				{
					list.DeleteRecord(record);
				}
			}
		}

		public void SearchSubjectLinks(TGEDCOMRecord aInRecord, TGEDCOMRecord aSubject, StringList aToList)
		{
			try
			{
				int num;

				if (aSubject is TGEDCOMNoteRecord) {
					num = aInRecord.Notes.Count - 1;
					for (int i = 0; i <= num; i++) {
						if (object.Equals(aInRecord.Notes[i].Value, aSubject)) {
							this.OutLink(aSubject, aToList, aInRecord, null, null);
						}
					}
				} else if (aSubject is TGEDCOMMultimediaRecord) {
					num = aInRecord.MultimediaLinks.Count - 1;
					for (int i = 0; i <= num; i++) {
						if (object.Equals(aInRecord.MultimediaLinks[i].Value, aSubject)) {
							this.OutLink(aSubject, aToList, aInRecord, null, null);
						}
					}
				} else if (aSubject is TGEDCOMSourceRecord) {
					num = aInRecord.SourceCitations.Count - 1;
					for (int i = 0; i <= num; i++) {
						if (object.Equals(aInRecord.SourceCitations[i].Value, aSubject)) {
							this.OutLink(aSubject, aToList, aInRecord, null, aInRecord.SourceCitations[i]);
						}
					}
				}

				if (aInRecord is TGEDCOMIndividualRecord) {
					TGEDCOMIndividualRecord i_rec = (TGEDCOMIndividualRecord)aInRecord;
					num = i_rec.IndividualEvents.Count - 1;
					for (int i = 0; i <= num; i++) {
						this.PrepareEvent(aSubject, aToList, i_rec, i_rec.IndividualEvents[i]);
					}
				} else if (aInRecord is TGEDCOMFamilyRecord) {
					TGEDCOMFamilyRecord f_rec = (TGEDCOMFamilyRecord)aInRecord;
					num = f_rec.FamilyEvents.Count - 1;
					for (int i = 0; i <= num; i++) {
						this.PrepareEvent(aSubject, aToList, f_rec, f_rec.FamilyEvents[i]);
					}
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.SearchSubjectLinks(): " + E.Message);
			}
		}

		public TGEDCOMFamilyRecord SelectFamily(TGEDCOMIndividualRecord aTarget)
		{
			TGEDCOMFamilyRecord Result;
			try
			{
				TfmRecordSelect dlg = new TfmRecordSelect(this);
				try
				{
					dlg.FTarget = aTarget;
					dlg.FNeedSex = TGEDCOMSex.svNone;
					dlg.TargetMode = TGenEngine.TTargetMode.tmChildToFamily;
					dlg.Mode = TGEDCOMRecordType.rtFamily;
					if (GKUI.TfmGEDKeeper.Instance.ShowModalEx(dlg, this, false) == DialogResult.OK)
					{
						Result = (dlg.ResultRecord as TGEDCOMFamilyRecord);
					} else {
						Result = null;
					}
				}
				finally
				{
					SysUtils.Free(dlg);
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.SelectFamily(): " + E.Message);
				Result = null;
			}
			return Result;
		}

		public TGEDCOMIndividualRecord SelectPerson(TGEDCOMIndividualRecord aTarget, TGenEngine.TTargetMode aTargetMode, TGEDCOMSex aNeedSex)
		{
			TGEDCOMIndividualRecord Result;
			try
			{
				TfmRecordSelect dlg = new TfmRecordSelect(this);
				try
				{
					dlg.FTarget = aTarget;
					dlg.FNeedSex = aNeedSex;
					dlg.TargetMode = aTargetMode;
					dlg.Mode = TGEDCOMRecordType.rtIndividual;
					if (GKUI.TfmGEDKeeper.Instance.ShowModalEx(dlg, this, false) == DialogResult.OK)
					{
						Result = (dlg.ResultRecord as TGEDCOMIndividualRecord);
					} else {
						Result = null;
					}
				}
				finally
				{
					SysUtils.Free(dlg);
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.SelectPerson(): " + E.Message);
				Result = null;
			}
			return Result;
		}

		public TGEDCOMRecord SelectRecord(TGEDCOMRecordType aMode, params object[] anArgs)
		{
			TGEDCOMRecord Result;
			try
			{
				anArgs = (object[])anArgs.Clone();
				TfmRecordSelect dlg = new TfmRecordSelect(this);
				try
				{
					dlg.Mode = aMode;
					int args_cnt = ((anArgs != null) ? anArgs.Length : 0);
					if (args_cnt > 0) {
						dlg.edFastFilter.Text = (anArgs[0] as string);
					}

					if (GKUI.TfmGEDKeeper.Instance.ShowModalEx(dlg, this, false) == DialogResult.OK)
					{
						Result = dlg.ResultRecord;
					} else {
						Result = null;
					}
				}
				finally
				{
					SysUtils.Free(dlg);
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.SelectRecord(): " + E.Message);
				Result = null;
			}
			return Result;
		}

		public void SelectRecordByXRef(string XRef)
		{
			TGEDCOMRecord rec = this.FTree.XRefIndex_Find(XRef);
			if (rec != null)
			{
				switch (rec.RecordType)
				{
					case TGEDCOMRecordType.rtIndividual:
					{
						TfmBase._SelectRecordByXRef_SelectItemByRec(this, this.ListPersons, rec, 0);
						break;
					}
					case TGEDCOMRecordType.rtFamily:
					{
						TfmBase._SelectRecordByXRef_SelectItemByRec(this, this.ListFamilies, rec, 1);
						break;
					}
					case TGEDCOMRecordType.rtNote:
					{
						TfmBase._SelectRecordByXRef_SelectItemByRec(this, this.ListNotes, rec, 2);
						break;
					}
					case TGEDCOMRecordType.rtMultimedia:
					{
						TfmBase._SelectRecordByXRef_SelectItemByRec(this, this.ListMultimedia, rec, 3);
						break;
					}
					case TGEDCOMRecordType.rtSource:
					{
						TfmBase._SelectRecordByXRef_SelectItemByRec(this, this.ListSources, rec, 4);
						break;
					}
					case TGEDCOMRecordType.rtRepository:
					{
						TfmBase._SelectRecordByXRef_SelectItemByRec(this, this.ListRepositories, rec, 5);
						break;
					}
					case TGEDCOMRecordType.rtGroup:
					{
						TfmBase._SelectRecordByXRef_SelectItemByRec(this, this.ListGroups, rec, 6);
						break;
					}
					case TGEDCOMRecordType.rtResearch:
					{
						TfmBase._SelectRecordByXRef_SelectItemByRec(this, this.ListResearches, rec, 7);
						break;
					}
					case TGEDCOMRecordType.rtTask:
					{
						TfmBase._SelectRecordByXRef_SelectItemByRec(this, this.ListTasks, rec, 8);
						break;
					}
					case TGEDCOMRecordType.rtCommunication:
					{
						TfmBase._SelectRecordByXRef_SelectItemByRec(this, this.ListCommunications, rec, 9);
						break;
					}
					case TGEDCOMRecordType.rtLocation:
					{
						TfmBase._SelectRecordByXRef_SelectItemByRec(this, this.ListLocations, rec, 10);
						break;
					}
				}
			}
		}

		public void SetFilter()
		{
			TfmFilter fmFilter = new TfmFilter(this);
			try
			{
				GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmFilter, this, false);
			}
			finally
			{
				SysUtils.Free(fmFilter);
			}
		}

		public void ShowMap()
		{
			TfmMaps frm_maps = new TfmMaps(this.FTree, this.ListPersons.ContentList);
			frm_maps.MdiParent = GKUI.TfmGEDKeeper.Instance;
			frm_maps.Show();
		}

		public void ShowMedia(TGEDCOMMultimediaRecord aMediaRec)
		{
			TfmMediaView fmMediaView = new TfmMediaView(this);
			try
			{
				fmMediaView.FileRef = aMediaRec.FileReferences[0];
				if (!fmMediaView.Extern)
				{
					GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmMediaView, GKUI.TfmGEDKeeper.Instance, false);
				}
			}
			finally
			{
				SysUtils.Free(fmMediaView);
			}
		}

		public void ShowOrganizer()
		{
			TfmOrganizer dlg = new TfmOrganizer(this);
			try
			{
				GKUI.TfmGEDKeeper.Instance.ShowModalEx(dlg, this, false);
			}
			finally
			{
				SysUtils.Free(dlg);
			}
		}

		public void ShowScriptDaemon()
		{
			TfmScriptDaemon dmn = new TfmScriptDaemon(this);
			try
			{
				GKUI.TfmGEDKeeper.Instance.ShowModalEx(dmn, this, false);
			}
			finally
			{
				SysUtils.Free(dmn);
			}
		}

		public void ShowStats()
		{
			TfmStats fmStats = new TfmStats(this);
			fmStats.Show();
		}

		public void ShowTips()
		{
			try
			{
				if (GKUI.TfmGEDKeeper.Instance.Options.ShowTips)
				{
					StringList birth_days = new StringList();
					try
					{
						int num = this.FTree.RecordsCount - 1;
						for (int i = 0; i <= num; i++)
						{
							TGEDCOMRecord rec = this.FTree.GetRecord(i);
							if (rec is TGEDCOMIndividualRecord)
							{
								TGEDCOMIndividualRecord i_rec = (TGEDCOMIndividualRecord)rec;
								string nm = TGenEngine.GetNameStr(i_rec, true, false);
								string days = TGenEngine.GetDaysForBirth(i_rec);

								if (days != "" && int.Parse(days) < 3)
								{
									birth_days.Add(string.Format(LangMan.LSList[248], new object[]
									{ nm, days }));
								}
							}
						}
						if (birth_days.Count > 0)
						{
							GKUI.TfmGEDKeeper.Instance.Options.ShowTips = TfmTipsDialog.ShowTipsEx(LangMan.LSList[247], GKUI.TfmGEDKeeper.Instance.Options.ShowTips, birth_days);
						}
					}
					finally
					{
						birth_days.Free();
					}
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.ShowTips(): " + E.Message);
			}
		}

		public void ShowTreeAncestors()
		{
			if (TfmChart.CheckData(this.FTree, this.GetSelectedPerson(), TAncestryChartBox.TChartKind.ckAncestors))
			{
				TfmChart fmChart = new TfmChart(this, this.GetSelectedPerson());
				fmChart.ChartKind = TAncestryChartBox.TChartKind.ckAncestors;
				fmChart.GenChart(true);
			}
		}

		public void ShowTreeDescendants()
		{
			if (TfmChart.CheckData(this.FTree, this.GetSelectedPerson(), TAncestryChartBox.TChartKind.ckDescendants))
			{
				TfmChart fmChart = new TfmChart(this, this.GetSelectedPerson());
				fmChart.ChartKind = TAncestryChartBox.TChartKind.ckDescendants;
				fmChart.GenChart(true);
			}
		}

		public void ShowTreeBoth()
		{
			if (TfmChart.CheckData(this.FTree, this.GetSelectedPerson(), TAncestryChartBox.TChartKind.ckBoth))
			{
				TfmChart fmChart = new TfmChart(this, this.GetSelectedPerson());
				fmChart.ChartKind = TAncestryChartBox.TChartKind.ckBoth;
				fmChart.GenChart(true);
			}
		}

		public void TreeTools()
		{
			TfmTreeTools fmTreeTools = new TfmTreeTools(this);
			try
			{
				GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmTreeTools, this, false);
			}
			finally
			{
				SysUtils.Free(fmTreeTools);
			}
		}

		public bool ModifyName(ref TNamesTable.TName aName)
		{
			TfmNameEdit dlg = new TfmNameEdit();
			bool Result;
			try
			{
				dlg.IName = aName;
				Result = (GKUI.TfmGEDKeeper.Instance.ShowModalEx(dlg, this, false) == DialogResult.OK);
			}
			finally
			{
				SysUtils.Free(dlg);
			}
			return Result;
		}

		public string DefinePatronymic(string aName, TGEDCOMSex aSex, bool aConfirm)
		{
			string Result = "";

			TNamesTable.TName n = GKUI.TfmGEDKeeper.Instance.NamesTable.FindName(aName);
			if (n == null) {
				if (!aConfirm) {
					return Result;
				} else {
					n = GKUI.TfmGEDKeeper.Instance.NamesTable.AddName(aName);
				}
			}

			switch (aSex) {
				case TGEDCOMSex.svMale: {
					Result = n.M_Patronymic;
					break;
				}
				case TGEDCOMSex.svFemale: {
					Result = n.F_Patronymic;
					break;
				}
			}
			
			if (Result == "") {
				if (!aConfirm) {
					return Result;
				} else {
					this.ModifyName(ref n);
				}
			}

			switch (aSex) {
				case TGEDCOMSex.svMale: {
					Result = n.M_Patronymic;
					break;
				}
				case TGEDCOMSex.svFemale: {
					Result = n.F_Patronymic;
					break;
				}
			}
			
			return Result;
		}

		public bool DeleteFamilyRecord(TGEDCOMFamilyRecord aFamily, bool aConfirm)
		{
			bool Result = false;
			if (aFamily != null && (!aConfirm || TGenEngine.ShowQuestion(string.Format(LangMan.LSList[72], new object[]
			{
				TGenEngine.GetFamilyStr(aFamily)
			})) != DialogResult.No))
			{
				this.FEngine.CleanFamily(aFamily);
				this.RecordNotify(aFamily, TRecNotify.rnDelete);
				this.FTree.Delete(this.FTree.IndexOfRecord(aFamily));
				this.Modified = true;
				Result = true;
			}
			return Result;
		}

		public bool DeleteGroupRecord(TGEDCOMGroupRecord groupRec, bool aConfirm)
		{
			bool Result = false;
			if (groupRec != null && (!aConfirm || TGenEngine.ShowQuestion(string.Format(LangMan.LSList[77], new object[]
			{
				groupRec.GroupName
			})) != DialogResult.No))
			{
				for (int i = groupRec.Members.Count - 1; i >= 0; i--)
				{
					TGEDCOMIndividualRecord member = groupRec.Members[i].Value as TGEDCOMIndividualRecord;
					this.FEngine.RemoveGroupMember(groupRec, member);
				}

				this.RecordNotify(groupRec, TRecNotify.rnDelete);
				this.FTree.Delete(this.FTree.IndexOfRecord(groupRec));
				this.Modified = true;
				Result = true;
			}
			return Result;
		}

		public bool DeleteIndividualRecord(TGEDCOMIndividualRecord iRec, bool aConfirm)
		{
			bool Result = false;
			if (iRec != null && (!aConfirm || TGenEngine.ShowQuestion(string.Format(LangMan.LSList[71], new object[]
			{
				TGenEngine.GetNameStr(iRec, true, false)
			})) != DialogResult.No))
			{
				for (int i = iRec.ChildToFamilyLinks.Count - 1; i >= 0; i--)
				{
					TGEDCOMFamilyRecord family = iRec.ChildToFamilyLinks[i].Family;
					family.DeleteChild(iRec);
				}

				for (int i = iRec.SpouseToFamilyLinks.Count - 1; i >= 0; i--)
				{
					TGEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[i].Family;
					this.FEngine.RemoveFamilySpouse(family, iRec);
				}

				for (int i = iRec.Groups.Count - 1; i >= 0; i--)
				{
					TGEDCOMPointer ptr = iRec.Groups[i];
					TGEDCOMGroupRecord group = ptr.Value as TGEDCOMGroupRecord;
					this.FEngine.RemoveGroupMember(group, iRec);
				}

				this.RecordNotify(iRec, TRecNotify.rnDelete);
				this.FTree.Delete(this.FTree.IndexOfRecord(iRec));
				this.Modified = true;
				Result = true;
			}
			return Result;
		}

		public bool DeleteMediaRecord(TGEDCOMMultimediaRecord mRec, bool aConfirm)
		{
			bool Result = false;
			if (mRec != null && (!aConfirm || TGenEngine.ShowQuestion(string.Format(LangMan.LSList[75], new object[]
			{
				mRec.StringValue
			})) != DialogResult.No))
			{
				int num = this.FTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = this.FTree.GetRecord(i);
					for (int j = rec.MultimediaLinks.Count - 1; j >= 0; j--)
					{
						if (rec.MultimediaLinks[j].Value == mRec)
						{
							rec.MultimediaLinks.Delete(j);
						}
					}
				}
				this.RecordNotify(mRec, TRecNotify.rnDelete);
				this.FTree.Delete(this.FTree.IndexOfRecord(mRec));
				this.Modified = true;
				Result = true;
			}
			return Result;
		}

		public bool DeleteNoteRecord(TGEDCOMNoteRecord nRec, bool aConfirm)
		{
			bool Result = false;
			if (nRec != null && (!aConfirm || TGenEngine.ShowQuestion(LangMan.LSList[73]) != DialogResult.No))
			{
				int num = this.FTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = this.FTree.GetRecord(i);
					for (int j = rec.Notes.Count - 1; j >= 0; j--)
					{
						if (rec.Notes[j].Value == nRec)
							rec.Notes.Delete(j);
					}
				}

				this.RecordNotify(nRec, TRecNotify.rnDelete);
				this.FTree.Delete(this.FTree.IndexOfRecord(nRec));
				this.Modified = true;
				Result = true;
			}
			return Result;
		}

		public bool DeleteRepositoryRecord(TGEDCOMRepositoryRecord repRec, bool aConfirm)
		{
			bool Result = false;
			if (repRec != null && (!aConfirm || TGenEngine.ShowQuestion(string.Format(LangMan.LSList[76], new object[]
			{
				repRec.RepositoryName
			})) != DialogResult.No))
			{
				int num = this.FTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = this.FTree.GetRecord(i);
					if (rec is TGEDCOMSourceRecord)
					{
						TGEDCOMSourceRecord srcRec = (TGEDCOMSourceRecord)rec;
						for (int j = srcRec.RepositoryCitations.Count - 1; j >= 0; j--)
						{
							if (srcRec.RepositoryCitations[j].Value == repRec)
							{
								srcRec.RepositoryCitations.DeleteObject(srcRec.RepositoryCitations[j]);
							}
						}
					}
				}

				this.RecordNotify(repRec, TRecNotify.rnDelete);
				this.FTree.Delete(this.FTree.IndexOfRecord(repRec));
				this.Modified = true;
				Result = true;
			}
			return Result;
		}

		public bool DeleteResearchRecord(TGEDCOMResearchRecord resRec, bool aConfirm)
		{
			bool Result = false;
			if (resRec != null && (!aConfirm || TGenEngine.ShowQuestion(string.Format(LangMan.LSList[78], new object[]
			{
				resRec.ResearchName
			})) != DialogResult.No))
			{
				this.RecordNotify(resRec, TRecNotify.rnDelete);
				this.FTree.Delete(this.FTree.IndexOfRecord(resRec));
				this.Modified = true;
				Result = true;
			}
			return Result;
		}

		public bool DeleteSourceRecord(TGEDCOMSourceRecord srcRec, bool aConfirm)
		{
			bool Result = false;
			if (srcRec != null && (!aConfirm || TGenEngine.ShowQuestion(string.Format(LangMan.LSList[74], new object[]
			{
				srcRec.FiledByEntry
			})) != DialogResult.No))
			{
				int num = this.FTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = this.FTree.GetRecord(i);
					for (int j = rec.SourceCitations.Count - 1; j >= 0; j--)
					{
						if (rec.SourceCitations[j].Value == srcRec)
						{
							rec.SourceCitations.Delete(j);
						}
					}
				}
				this.RecordNotify(srcRec, TRecNotify.rnDelete);
				this.FTree.Delete(this.FTree.IndexOfRecord(srcRec));
				this.Modified = true;
				Result = true;
			}
			return Result;
		}

		public bool DeleteTaskRecord(TGEDCOMTaskRecord TaskRec, bool aConfirm)
		{
			bool Result = false;
			if (TaskRec != null && (!aConfirm || TGenEngine.ShowQuestion(string.Format(LangMan.LSList[79], new object[]
			{
				TGenEngine.GetTaskGoalStr(this.FTree, TaskRec)
			})) != DialogResult.No))
			{
				int num = this.FTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = this.FTree.GetRecord(i);
					if (rec is TGEDCOMResearchRecord)
					{
						TGEDCOMResearchRecord resRec = (TGEDCOMResearchRecord)rec;
						for (int j = resRec.Tasks.Count - 1; j >= 0; j--)
						{
							if (resRec.Tasks[j].Value == TaskRec)
							{
								resRec.Tasks.Delete(j);
							}
						}
					}
				}
				this.RecordNotify(TaskRec, TRecNotify.rnDelete);
				this.FTree.Delete(this.FTree.IndexOfRecord(TaskRec));
				this.Modified = true;
				Result = true;
			}
			return Result;
		}

		public bool DeleteCommunicationRecord(TGEDCOMCommunicationRecord ComRec, bool aConfirm)
		{
			bool Result = false;
			if (ComRec != null && (!aConfirm || TGenEngine.ShowQuestion(string.Format(LangMan.LSList[80], new object[]
			{
				ComRec.CommName
			})) != DialogResult.No))
			{
				int num = this.FTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = this.FTree.GetRecord(i);
					if (rec is TGEDCOMResearchRecord)
					{
						TGEDCOMResearchRecord resRec = (TGEDCOMResearchRecord)rec;
						for (int j = resRec.Communications.Count - 1; j >= 0; j--)
						{
							if (resRec.Communications[j].Value == ComRec)
							{
								resRec.Communications.Delete(j);
							}
						}
					}
				}
				this.RecordNotify(ComRec, TRecNotify.rnDelete);
				this.FTree.Delete(this.FTree.IndexOfRecord(ComRec));
				this.Modified = true;
				Result = true;
			}
			return Result;
		}

		public bool DeleteLocationRecord(TGEDCOMLocationRecord LocRec, bool aConfirm)
		{
			bool Result = false;
			if (LocRec != null && (!aConfirm || TGenEngine.ShowQuestion(string.Format(LangMan.LSList[81], new object[]
			{
				LocRec.LocationName
			})) != DialogResult.No))
			{
				int num = this.FTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
						TGEDCOMRecord rec = this.FTree.GetRecord(i);
						if (rec is TGEDCOMIndividualRecord)
						{
							TGEDCOMIndividualRecord iRec = (TGEDCOMIndividualRecord)rec;
							for (int j = iRec.IndividualEvents.Count - 1; j >= 0; j--)
							{
								TGEDCOMCustomEvent ev = iRec.IndividualEvents[j];
								if (object.Equals(ev.Detail.Place.Location.Value, LocRec))
								{
									ev.Detail.Place.DeleteTag("_LOC");
								}
							}
						}
						else
						{
							if (rec is TGEDCOMFamilyRecord)
							{
								TGEDCOMFamilyRecord fRec = (TGEDCOMFamilyRecord)rec;
								for (int j = fRec.FamilyEvents.Count - 1; j >= 0; j--)
								{
									TGEDCOMCustomEvent ev = fRec.FamilyEvents[j];
									if (object.Equals(ev.Detail.Place.Location.Value, LocRec))
									{
										ev.Detail.Place.DeleteTag("_LOC");
									}
								}
							}
						}
				}
				this.RecordNotify(LocRec, TRecNotify.rnDelete);
				this.FTree.Delete(this.FTree.IndexOfRecord(LocRec));
				this.Modified = true;
				Result = true;
			}
			return Result;
		}

		public bool DeleteRecord(TGEDCOMRecord aRecord, bool aConfirm)
		{
			bool Result = false;

			if (aRecord != null)
			{
				string xref = aRecord.XRef;

				switch (aRecord.RecordType)
				{
					case TGEDCOMRecordType.rtIndividual:
					{
						Result = this.DeleteIndividualRecord((TGEDCOMIndividualRecord)aRecord, aConfirm);
						break;
					}
					case TGEDCOMRecordType.rtFamily:
					{
						Result = this.DeleteFamilyRecord((TGEDCOMFamilyRecord)aRecord, aConfirm);
						break;
					}
					case TGEDCOMRecordType.rtNote:
					{
						Result = this.DeleteNoteRecord((TGEDCOMNoteRecord)aRecord, aConfirm);
						break;
					}
					case TGEDCOMRecordType.rtMultimedia:
					{
						Result = this.DeleteMediaRecord((TGEDCOMMultimediaRecord)aRecord, aConfirm);
						break;
					}
					case TGEDCOMRecordType.rtSource:
					{
						Result = this.DeleteSourceRecord((TGEDCOMSourceRecord)aRecord, aConfirm);
						break;
					}
					case TGEDCOMRecordType.rtRepository:
					{
						Result = this.DeleteRepositoryRecord((TGEDCOMRepositoryRecord)aRecord, aConfirm);
						break;
					}
					case TGEDCOMRecordType.rtGroup:
					{
						Result = this.DeleteGroupRecord((TGEDCOMGroupRecord)aRecord, aConfirm);
						break;
					}
					case TGEDCOMRecordType.rtResearch:
					{
						Result = this.DeleteResearchRecord((TGEDCOMResearchRecord)aRecord, aConfirm);
						break;
					}
					case TGEDCOMRecordType.rtTask:
					{
						Result = this.DeleteTaskRecord((TGEDCOMTaskRecord)aRecord, aConfirm);
						break;
					}
					case TGEDCOMRecordType.rtCommunication:
					{
						Result = this.DeleteCommunicationRecord((TGEDCOMCommunicationRecord)aRecord, aConfirm);
						break;
					}
					case TGEDCOMRecordType.rtLocation:
					{
						Result = this.DeleteLocationRecord((TGEDCOMLocationRecord)aRecord, aConfirm);
						break;
					}
				}

				if (Result)
				{
					this.FTree.Header.TransmissionDateTime = DateTime.Now;

					TfmGEDKeeper.Instance.SearchMan.DeleteRecord(this, xref);
				}
			}

			return Result;
		}

		public bool RecordIsFiltered(TGEDCOMRecord aRecord)
		{
			bool Result = false;
			if (aRecord != null)
			{
				switch (aRecord.RecordType)
				{
					case TGEDCOMRecordType.rtIndividual:
					{
						Result = (this.ListPersons.ContentList.IndexOf(aRecord) >= 0);
						break;
					}
					case TGEDCOMRecordType.rtFamily:
					{
						Result = (this.ListFamilies.ContentList.IndexOf(aRecord) >= 0);
						break;
					}
					case TGEDCOMRecordType.rtNote:
					{
						Result = (this.ListNotes.ContentList.IndexOf(aRecord) >= 0);
						break;
					}
					case TGEDCOMRecordType.rtMultimedia:
					{
						Result = (this.ListMultimedia.ContentList.IndexOf(aRecord) >= 0);
						break;
					}
					case TGEDCOMRecordType.rtSource:
					{
						Result = (this.ListSources.ContentList.IndexOf(aRecord) >= 0);
						break;
					}
					case TGEDCOMRecordType.rtRepository:
					{
						Result = (this.ListRepositories.ContentList.IndexOf(aRecord) >= 0);
						break;
					}
					case TGEDCOMRecordType.rtGroup:
					{
						Result = (this.ListGroups.ContentList.IndexOf(aRecord) >= 0);
						break;
					}
					case TGEDCOMRecordType.rtResearch:
					{
						Result = (this.ListResearches.ContentList.IndexOf(aRecord) >= 0);
						break;
					}
					case TGEDCOMRecordType.rtTask:
					{
						Result = (this.ListTasks.ContentList.IndexOf(aRecord) >= 0);
						break;
					}
					case TGEDCOMRecordType.rtCommunication:
					{
						Result = (this.ListCommunications.ContentList.IndexOf(aRecord) >= 0);
						break;
					}
					case TGEDCOMRecordType.rtLocation:
					{
						Result = (this.ListLocations.ContentList.IndexOf(aRecord) >= 0);
						break;
					}
				}
			}
			return Result;
		}

		public void DoUndo()
		{
			this.FUndoman.CmdUndo();
			this.ListsRefresh(false);
			GKUI.TfmGEDKeeper.Instance.UpdateControls(false);
		}

		public void DoRedo()
		{
			this.FUndoman.CmdRedo();
			this.ListsRefresh(false);
			GKUI.TfmGEDKeeper.Instance.UpdateControls(false);
		}

		public void DoPersonChangeSex(TGEDCOMIndividualRecord aPerson, TGEDCOMSex NewSex)
		{
			if (aPerson.Sex != NewSex)
			{
				this.FUndoman.CmdDo(new TCmdPersonChangeSex(this.FUndoman, aPerson, NewSex));
				this.FUndoman.Commit();
			}
		}

		public void DoPersonChangePatriarch(TGEDCOMIndividualRecord aPerson, bool NewValue)
		{
			if (aPerson.Patriarch != NewValue)
			{
				this.FUndoman.CmdDo(new TCmdPersonChangePatriarch(this.FUndoman, aPerson, NewValue));
				this.FUndoman.Commit();
			}
		}

		public bool ModifyPerson(ref TGEDCOMIndividualRecord aIndivRec)
		{
			bool Result = false;
			if (aIndivRec != null)
			{
				TfmPersonEdit dlg = new TfmPersonEdit(this);
				try
				{
					dlg.Person = aIndivRec;
					Result = (GKUI.TfmGEDKeeper.Instance.ShowModalEx(dlg, this, false) == DialogResult.OK);
				}
				finally
				{
					SysUtils.Free(dlg);
				}
			}
			return Result;
		}

		public bool ModifyFamily(ref TGEDCOMFamilyRecord aFamilyRec, TGenEngine.TFamilyTarget aTarget, TGEDCOMIndividualRecord aPerson)
		{
			bool Result = false;
			if (aTarget == TGenEngine.TFamilyTarget.ftSpouse && aPerson != null)
			{
				TGEDCOMSex sex = aPerson.Sex;
				if (sex < TGEDCOMSex.svMale || sex >= TGEDCOMSex.svUndetermined)
				{
					TGenEngine.ShowError(LangMan.LSList[210]);
					return Result;
				}
			}
			TfmFamilyEdit fmFamEdit = new TfmFamilyEdit(this);
			try
			{
				bool exists = aFamilyRec != null;
				if (!exists)
				{
					aFamilyRec = new TGEDCOMFamilyRecord(this.FTree, this.FTree, "", "");
					aFamilyRec.InitNew();
				}
				if (aTarget != TGenEngine.TFamilyTarget.ftSpouse)
				{
					if (aTarget == TGenEngine.TFamilyTarget.ftChild)
					{
						if (aPerson != null)
						{
							this.FEngine.AddFamilyChild(aFamilyRec, aPerson);
						}
					}
				}
				else
				{
					if (aPerson != null)
					{
						this.FEngine.AddFamilySpouse(aFamilyRec, aPerson);
					}
				}
				fmFamEdit.Family = aFamilyRec;
				Result = (GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmFamEdit, this, false) == DialogResult.OK);
				if (Result)
				{
					if (!exists)
					{
						this.FTree.AddRecord(aFamilyRec);
					}
				}
				else
				{
					if (!exists)
					{
						this.FEngine.CleanFamily(aFamilyRec);
						aFamilyRec.Dispose();
						aFamilyRec = null;
					}
				}
			}
			finally
			{
				SysUtils.Free(fmFamEdit);
			}
			return Result;
		}

		public bool ModifyNote(ref TGEDCOMNoteRecord aNoteRec)
		{
			bool Result = false;
			TfmNoteEdit fmNoteEdit = new TfmNoteEdit(this);
			try
			{
				bool exists = aNoteRec != null;
				if (!exists)
				{
					aNoteRec = new TGEDCOMNoteRecord(this.FTree, this.FTree, "", "");
					aNoteRec.InitNew();
				}
				fmNoteEdit.NoteRecord = aNoteRec;
				if (GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmNoteEdit, this, false) == DialogResult.OK)
				{
					if (!exists)
					{
						this.FTree.AddRecord(aNoteRec);
					}
					Result = true;
				}
				else
				{
					if (!exists)
					{
						aNoteRec.Dispose();
						aNoteRec = null;
					}
				}
			}
			finally
			{
				SysUtils.Free(fmNoteEdit);
			}
			return Result;
		}

		public bool ModifyMedia(ref TGEDCOMMultimediaRecord aMediaRec)
		{
			bool Result = false;
			TfmMediaEdit fmMediaEdit = new TfmMediaEdit(this);
			try
			{
				bool exists = aMediaRec != null;
				if (!exists)
				{
					aMediaRec = new TGEDCOMMultimediaRecord(this.FTree, this.FTree, "", "");
					aMediaRec.FileReferences.Add(new TGEDCOMFileReferenceWithTitle(this.FTree, aMediaRec, "", ""));
					aMediaRec.InitNew();
				}
				fmMediaEdit.MediaRec = aMediaRec;
				if (GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmMediaEdit, this, false) == DialogResult.OK)
				{
					if (!exists)
					{
						this.FTree.AddRecord(aMediaRec);
					}
					Result = true;
				}
				else
				{
					if (!exists)
					{
						aMediaRec.Dispose();
						aMediaRec = null;
					}
				}
			}
			finally
			{
				SysUtils.Free(fmMediaEdit);
			}
			return Result;
		}

		public bool ModifySource(ref TGEDCOMSourceRecord aSourceRec)
		{
			bool Result = false;
			TfmSourceEdit fmSrcEdit = new TfmSourceEdit(this);
			try
			{
				bool exists = aSourceRec != null;
				if (!exists)
				{
					aSourceRec = new TGEDCOMSourceRecord(this.FTree, this.FTree, "", "");
					aSourceRec.InitNew();
				}
				fmSrcEdit.SourceRecord = aSourceRec;
				if (GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmSrcEdit, this, false) == DialogResult.OK)
				{
					if (!exists)
					{
						this.FTree.AddRecord(aSourceRec);
					}
					Result = true;
				}
				else
				{
					if (!exists)
					{
						aSourceRec.Dispose();
						aSourceRec = null;
					}
				}
			}
			finally
			{
				SysUtils.Free(fmSrcEdit);
			}
			return Result;
		}

		public bool ModifyRepository(ref TGEDCOMRepositoryRecord aRepRec)
		{
			bool Result = false;
			TfmRepositoryEdit fmRepEdit = new TfmRepositoryEdit(this);
			try
			{
				bool exists = aRepRec != null;
				if (!exists)
				{
					aRepRec = new TGEDCOMRepositoryRecord(this.FTree, this.FTree, "", "");
					aRepRec.InitNew();
				}
				fmRepEdit.Repository = aRepRec;
				if (GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmRepEdit, this, false) == DialogResult.OK)
				{
					if (!exists)
					{
						this.FTree.AddRecord(aRepRec);
					}
					Result = true;
				}
				else
				{
					if (!exists)
					{
						aRepRec.Dispose();
						aRepRec = null;
					}
				}
			}
			finally
			{
				SysUtils.Free(fmRepEdit);
			}
			return Result;
		}

		public bool ModifyGroup(ref TGEDCOMGroupRecord aGroupRec)
		{
			bool Result = false;
			TfmGroupEdit fmGrpEdit = new TfmGroupEdit(this);
			try
			{
				bool exists = aGroupRec != null;
				if (!exists)
				{
					aGroupRec = new TGEDCOMGroupRecord(this.FTree, this.FTree, "", "");
					aGroupRec.InitNew();
				}
				fmGrpEdit.Group = aGroupRec;
				if (GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmGrpEdit, this, false) == DialogResult.OK)
				{
					if (!exists)
					{
						this.FTree.AddRecord(aGroupRec);
					}
					Result = true;
				}
				else
				{
					if (!exists)
					{
						aGroupRec.Dispose();
						aGroupRec = null;
					}
				}
			}
			finally
			{
				fmGrpEdit.Dispose();
			}
			return Result;
		}

		public bool ModifyResearch(ref TGEDCOMResearchRecord aResearchRec)
		{
			bool Result = false;
			TfmResearchEdit fmResEdit = new TfmResearchEdit(this);
			try
			{
				bool exists = aResearchRec != null;
				if (!exists)
				{
					aResearchRec = new TGEDCOMResearchRecord(this.FTree, this.FTree, "", "");
					aResearchRec.InitNew();
				}
				fmResEdit.Research = aResearchRec;
				if (GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmResEdit, this, false) == DialogResult.OK)
				{
					if (!exists)
					{
						this.FTree.AddRecord(aResearchRec);
					}
					Result = true;
				}
				else
				{
					if (!exists)
					{
						aResearchRec.Dispose();
						aResearchRec = null;
					}
				}
			}
			finally
			{
				fmResEdit.Dispose();
			}
			return Result;
		}

		public bool ModifyTask(ref TGEDCOMTaskRecord aTaskRec)
		{
			bool Result = false;
			TfmTaskEdit fmTaskEdit = new TfmTaskEdit(this);
			try
			{
				bool exists = aTaskRec != null;
				if (!exists)
				{
					aTaskRec = new TGEDCOMTaskRecord(this.FTree, this.FTree, "", "");
					aTaskRec.InitNew();
				}
				fmTaskEdit.Task = aTaskRec;
				if (GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmTaskEdit, this, false) == DialogResult.OK)
				{
					if (!exists)
					{
						this.FTree.AddRecord(aTaskRec);
					}
					Result = true;
				}
				else
				{
					if (!exists)
					{
						aTaskRec.Dispose();
						aTaskRec = null;
					}
				}
			}
			finally
			{
				fmTaskEdit.Dispose();
			}
			return Result;
		}

		public bool ModifyCommunication(ref TGEDCOMCommunicationRecord aCommunicationRec)
		{
			bool Result = false;
			TfmCommunicationEdit fmCorrEdit = new TfmCommunicationEdit(this);
			try
			{
				bool exists = aCommunicationRec != null;
				if (!exists)
				{
					aCommunicationRec = new TGEDCOMCommunicationRecord(this.FTree, this.FTree, "", "");
					aCommunicationRec.InitNew();
				}
				fmCorrEdit.Communication = aCommunicationRec;
				if (GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmCorrEdit, this, false) == DialogResult.OK)
				{
					if (!exists)
					{
						this.FTree.AddRecord(aCommunicationRec);
					}
					Result = true;
				}
				else
				{
					if (!exists)
					{
						aCommunicationRec.Dispose();
						aCommunicationRec = null;
					}
				}
			}
			finally
			{
				fmCorrEdit.Dispose();
			}
			return Result;
		}

		public bool ModifyLocation(ref TGEDCOMLocationRecord aLocationRec)
		{
			bool Result = false;
			TfmLocationEdit fmLocEdit = new TfmLocationEdit(this);
			try
			{
				bool exists = aLocationRec != null;
				if (!exists)
				{
					aLocationRec = new TGEDCOMLocationRecord(this.FTree, this.FTree, "", "");
					aLocationRec.InitNew();
				}
				fmLocEdit.LocationRecord = aLocationRec;
				if (GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmLocEdit, this, false) == DialogResult.OK)
				{
					if (!exists)
					{
						this.FTree.AddRecord(aLocationRec);
					}
					Result = true;
				}
				else
				{
					if (!exists)
					{
						aLocationRec.Dispose();
						aLocationRec = null;
					}
				}
			}
			finally
			{
				fmLocEdit.Dispose();
			}
			return Result;
		}

		public bool ModifyAddress(Form aSender, TGEDCOMAddress anAddress)
		{
			TfmAddressEdit dlg = new TfmAddressEdit();
			bool Result;
			try
			{
				dlg.Address = anAddress;
				Result = (GKUI.TfmGEDKeeper.Instance.ShowModalEx(dlg, aSender, false) == DialogResult.OK);
			}
			finally
			{
				dlg.Dispose();
			}
			return Result;
		}

		public bool ModifyRecAssociation(Form aSender, TGEDCOMIndividualRecord aRecord, TGEDCOMAssociation aAssociation, TGenEngine.TRecAction anAction)
		{
			bool Result = false;
			if (anAction == TGenEngine.TRecAction.raDelete)
			{
				if (TGenEngine.ShowQuestion(LangMan.LSList[243]) != DialogResult.No)
				{
					aRecord.Associations.DeleteObject(aAssociation);
					Result = true;
					this.Modified = true;
				}
			}
			else
			{
				TfmAssociationEdit fmAstEdit = new TfmAssociationEdit(this);
				try
				{
					TGEDCOMAssociation ast;
					if (anAction == TGenEngine.TRecAction.raEdit && aAssociation != null)
					{
						ast = aAssociation;
					}
					else
					{
						ast = new TGEDCOMAssociation(this.FTree, aRecord, "", "");
					}
					fmAstEdit.Association = ast;
					DialogResult res = GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmAstEdit, aSender, false);
					if (anAction == TGenEngine.TRecAction.raAdd)
					{
						if (res == DialogResult.OK)
						{
							aRecord.Associations.Add(ast);
						}
						else
						{
							ast.Dispose();
						}
					}
					Result = (res == DialogResult.OK);
				}
				finally
				{
					fmAstEdit.Dispose();
				}
			}
			return Result;
		}

		public bool ModifyRecEvent(Form aSender, TGEDCOMRecord aRecord, TGEDCOMCustomEvent aEvent, TGenEngine.TRecAction anAction)
		{
			try
			{
				bool Result = false;
				if (anAction == TGenEngine.TRecAction.raDelete)
				{
					if (TGenEngine.ShowQuestion(LangMan.LSList[239]) != DialogResult.No)
					{
						if (aRecord is TGEDCOMIndividualRecord)
						{
							(aRecord as TGEDCOMIndividualRecord).IndividualEvents.DeleteObject(aEvent);
						}
						else
						{
							(aRecord as TGEDCOMFamilyRecord).FamilyEvents.DeleteObject(aEvent as TGEDCOMFamilyEvent);
						}
						Result = true;
						this.Modified = true;
					}
				}
				else
				{
					TfmEventEdit fmEventEdit = new TfmEventEdit(this);
					try
					{
						TGEDCOMCustomEvent @event;
						if (aEvent != null)
						{
							@event = aEvent;
						}
						else
						{
							if (aRecord is TGEDCOMIndividualRecord)
							{
								@event = new TGEDCOMIndividualEvent(this.FTree, aRecord, "", "");
							}
							else
							{
								@event = new TGEDCOMFamilyEvent(this.FTree, aRecord, "", "");
							}
						}
						fmEventEdit.Event = @event;
						DialogResult dialogResult = GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmEventEdit, aSender, true);
						if (dialogResult != DialogResult.OK)
						{
							if (dialogResult == DialogResult.Cancel)
							{
								if (aEvent == null)
								{
									@event.Dispose();
								}
							}
						}
						else
						{
							if (aEvent == null)
							{
								@event = fmEventEdit.Event;
								if (aRecord is TGEDCOMIndividualRecord)
								{
									(aRecord as TGEDCOMIndividualRecord).AddIndividualEvent(@event);
								}
								else
								{
									(aRecord as TGEDCOMFamilyRecord).FamilyEvents.Add(@event as TGEDCOMFamilyEvent);
								}
							}
							else
							{
								if (aRecord is TGEDCOMIndividualRecord && !object.Equals(fmEventEdit.Event, aEvent))
								{
									(aRecord as TGEDCOMIndividualRecord).IndividualEvents.DeleteObject(aEvent);
									@event = fmEventEdit.Event;
									(aRecord as TGEDCOMIndividualRecord).AddIndividualEvent(@event);
								}
							}
							Result = true;
							this.Modified = true;
						}
					}
					finally
					{
						fmEventEdit.Dispose();
					}
				}
				return Result;
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.ModifyRecEvent(): " + E.Message);
				return false;
			}
		}

		public bool ModifyRecMultimedia(Form aSender, TGEDCOMRecord aRecord, TGEDCOMMultimediaLink aLink, TGenEngine.TRecAction anAction)
		{
			bool Result = false;
			if (anAction == TGenEngine.TRecAction.raDelete)
			{
				if (TGenEngine.ShowQuestion(LangMan.LSList[241]) != DialogResult.No)
				{
					aRecord.MultimediaLinks.DeleteObject(aLink);
					Result = true;
					this.Modified = true;
				}
			}
			else
			{
				if (anAction == TGenEngine.TRecAction.raEdit && aLink != null)
				{
					TGEDCOMMultimediaRecord mmRec = aLink.Value as TGEDCOMMultimediaRecord;
					Result = this.ModifyMedia(ref mmRec);
				}
				else
				{
					TGEDCOMMultimediaRecord mmRec = this.SelectRecord(TGEDCOMRecordType.rtMultimedia, new object[0]) as TGEDCOMMultimediaRecord;
					if (mmRec != null)
					{
						TGEDCOMMultimediaLink mmLink = new TGEDCOMMultimediaLink(this.FTree, aRecord, "", "");
						mmLink.Value = mmRec;
						aRecord.MultimediaLinks.Add(mmLink);
						Result = true;
					}
				}
			}
			return Result;
		}

		public bool ModifyRecNote(Form aSender, TGEDCOMRecord aRecord, TGEDCOMNotes aNote, TGenEngine.TRecAction anAction)
		{
			bool Result = false;
			if (anAction == TGenEngine.TRecAction.raDelete)
			{
				if (TGenEngine.ShowQuestion(LangMan.LSList[240]) != DialogResult.No)
				{
					aRecord.Notes.DeleteObject(aNote);
					Result = true;
					this.Modified = true;
				}
			}
			else
			{
				if (anAction == TGenEngine.TRecAction.raEdit && aNote != null)
				{
					TGEDCOMNoteRecord noteRec = aNote.Value as TGEDCOMNoteRecord;
					Result = this.ModifyNote(ref noteRec);
				}
				else
				{
					TGEDCOMNoteRecord noteRec = this.SelectRecord(TGEDCOMRecordType.rtNote, new object[0]) as TGEDCOMNoteRecord;
					if (noteRec != null)
					{
						TGEDCOMNotes note = new TGEDCOMNotes(this.FTree, aRecord, "", "");
						note.Value = noteRec;
						aRecord.Notes.Add(note);
						Result = true;
					}
				}
			}
			return Result;
		}

		public bool ModifyRecSource(Form aSender, TGEDCOMRecord aRecord, TGEDCOMSourceCitation aCit, TGenEngine.TRecAction anAction)
		{
			bool Result = false;
			if (anAction == TGenEngine.TRecAction.raDelete)
			{
				if (TGenEngine.ShowQuestion(LangMan.LSList[242]) != DialogResult.No)
				{
					aRecord.SourceCitations.DeleteObject(aCit);
					Result = true;
					this.Modified = true;
				}
			}
			else
			{
				TfmSourceCitEdit fmSrcCitEdit = new TfmSourceCitEdit(this);
				try
				{
					TGEDCOMSourceCitation cit;
					if (anAction == TGenEngine.TRecAction.raEdit && aCit != null)
					{
						cit = aCit;
					}
					else
					{
						cit = new TGEDCOMSourceCitation(this.FTree, aRecord, "", "");
					}
					fmSrcCitEdit.SourceCitation = cit;
					DialogResult res = GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmSrcCitEdit, aSender, false);
					if (anAction == TGenEngine.TRecAction.raAdd)
					{
						if (res == DialogResult.OK)
						{
							aRecord.SourceCitations.Add(cit);
						}
						else
						{
							cit.Dispose();
						}
					}
					Result = (res == DialogResult.OK);
				}
				finally
				{
					fmSrcCitEdit.Dispose();
				}
			}
			return Result;
		}

		public bool ModifyRecUserRef(Form aSender, TGEDCOMRecord aRecord, TGEDCOMUserReference aUserRef, TGenEngine.TRecAction anAction)
		{
			bool Result = false;
			if (anAction == TGenEngine.TRecAction.raDelete)
			{
				if (TGenEngine.ShowQuestion(LangMan.LSList[244]) != DialogResult.No)
				{
					aRecord.UserReferences.DeleteObject(aUserRef);
					Result = true;
					this.Modified = true;
				}
			}
			else
			{
				TfmUserRefEdit dlg = new TfmUserRefEdit(this);
				try
				{
					TGEDCOMUserReference @ref;
					if (anAction == TGenEngine.TRecAction.raEdit && aUserRef != null)
					{
						@ref = aUserRef;
					}
					else
					{
						@ref = new TGEDCOMUserReference(this.FTree, aRecord, "", "");
					}
					dlg.UserRef = @ref;
					DialogResult res = GKUI.TfmGEDKeeper.Instance.ShowModalEx(dlg, aSender, false);
					if (anAction == TGenEngine.TRecAction.raAdd)
					{
						if (res == DialogResult.OK)
						{
							aRecord.UserReferences.Add(@ref);
						}
						else
						{
							@ref.Dispose();
						}
					}
					Result = (res == DialogResult.OK);
				}
				finally
				{
					dlg.Dispose();
				}
			}
			return Result;
		}

		public bool ModifyTagMultimedia(TGEDCOMTagWithLists aTag, TGEDCOMMultimediaLink aLink, TGenEngine.TRecAction anAction)
		{
			bool Result = false;
			if (anAction == TGenEngine.TRecAction.raDelete)
			{
				if (TGenEngine.ShowQuestion(LangMan.LSList[241]) != DialogResult.No)
				{
					aTag.MultimediaLinks.DeleteObject(aLink);
					this.Modified = true;
					Result = true;
				}
			}
			else
			{
				if (anAction == TGenEngine.TRecAction.raEdit)
				{
					if (aLink != null)
					{
						TGEDCOMMultimediaRecord mmRec = aLink.Value as TGEDCOMMultimediaRecord;
						Result = this.ModifyMedia(ref mmRec);
						this.Modified |= Result;
					}
				}
				else
				{
					TGEDCOMMultimediaRecord mmRec = this.SelectRecord(TGEDCOMRecordType.rtMultimedia, new object[0]) as TGEDCOMMultimediaRecord;
					if (mmRec != null)
					{
						TGEDCOMMultimediaLink mmLink = new TGEDCOMMultimediaLink(this.FTree, aTag, "", "");
						mmLink.Value = mmRec;
						aTag.MultimediaLinks.Add(mmLink);
						this.Modified = true;
						Result = true;
					}
				}
			}
			return Result;
		}

		public bool ModifyTagNote(TGEDCOMTagWithLists aTag, TGEDCOMNotes aNote, TGenEngine.TRecAction anAction)
		{
			bool Result = false;
			if (anAction == TGenEngine.TRecAction.raDelete)
			{
				if (TGenEngine.ShowQuestion(LangMan.LSList[240]) != DialogResult.No)
				{
					aTag.Notes.DeleteObject(aNote);
					this.Modified = true;
					Result = true;
				}
			}
			else
			{
				if (anAction == TGenEngine.TRecAction.raEdit)
				{
					if (aNote != null)
					{
						TGEDCOMNoteRecord noteRec = aNote.Value as TGEDCOMNoteRecord;
						Result = this.ModifyNote(ref noteRec);
					}
				}
				else
				{
					TGEDCOMNoteRecord noteRec = this.SelectRecord(TGEDCOMRecordType.rtNote, new object[0]) as TGEDCOMNoteRecord;
					if (noteRec != null)
					{
						TGEDCOMNotes note = new TGEDCOMNotes(this.FTree, aTag, "", "");
						note.Value = noteRec;
						aTag.Notes.Add(note);
						Result = true;
					}
				}
			}
			return Result;
		}

		public bool ModifyTagSource(TGEDCOMTagWithLists aTag, TGEDCOMSourceCitation aCit, TGenEngine.TRecAction anAction)
		{
			bool Result = false;
			if (anAction == TGenEngine.TRecAction.raDelete)
			{
				if (TGenEngine.ShowQuestion(LangMan.LSList[242]) != DialogResult.No)
				{
					aTag.SourceCitations.DeleteObject(aCit);
					this.Modified = true;
					Result = true;
				}
			}
			else
			{
				TfmSourceCitEdit fmSrcCitEdit = new TfmSourceCitEdit(this);
				try
				{
					TGEDCOMSourceCitation cit;
					if (anAction == TGenEngine.TRecAction.raEdit && aCit != null)
					{
						cit = aCit;
					}
					else
					{
						cit = new TGEDCOMSourceCitation(this.FTree, aTag, "", "");
					}
					fmSrcCitEdit.SourceCitation = cit;
					DialogResult res = GKUI.TfmGEDKeeper.Instance.ShowModalEx(fmSrcCitEdit, GKUI.TfmGEDKeeper.Instance, false);
					if (anAction == TGenEngine.TRecAction.raAdd)
					{
						if (res == DialogResult.OK)
						{
							aTag.SourceCitations.Add(cit);
						}
						else
						{
							cit.Dispose();
						}
					}
					Result = (res == DialogResult.OK);
				}
				finally
				{
					fmSrcCitEdit.Dispose();
				}
			}
			return Result;
		}

		public void RecListAssociationsRefresh(TGEDCOMIndividualRecord aRecord, TGKListView aList, StringList aSummary)
		{
			try
			{
				if (aList != null)
				{
					aList.Items.Clear();
				}
				if (aRecord.Associations.Count != 0)
				{
					if (aList == null && aSummary != null)
					{
						aSummary.Add("");
						aSummary.Add(LangMan.LSList[154] + ":");
					}

					int num = aRecord.Associations.Count - 1;
					for (int idx = 0; idx <= num; idx++)
					{
						TGEDCOMAssociation ast = aRecord.Associations[idx];
						string nm = TGenEngine.GetNameStr(ast.Individual, true, false);
						if (aList == null && aSummary != null)
						{
							aSummary.Add("    " + ast.Relation + " " + TGenEngine.HyperLink(ast.Individual.XRef, nm, 0));
						}
						if (aList != null)
						{
							TExtListItem item = aList.AddItem(ast.Relation, ast);
							item.SubItems.Add(nm);
						}
					}
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.RecListAssociationsRefresh(): " + E.Message);
			}
		}

		public void RecListFamilyEventsRefresh(TGEDCOMFamilyRecord aRecord, TGKListView aList, StringList aSummary)
		{
			try
			{
				if (aList != null)
				{
					aList.Items.Clear();
				}

				if (aRecord.FamilyEvents.Count != 0)
				{
					if (aSummary != null)
					{
						aSummary.Add("");
						aSummary.Add(LangMan.LSList[83] + ":");
					}

					int num = aRecord.FamilyEvents.Count - 1;
					for (int idx = 0; idx <= num; idx++)
					{
						TGEDCOMFamilyEvent @event = aRecord.FamilyEvents[idx];
						int ev = TGenEngine.GetFamilyEventIndex(@event.Name);
						string st;
						if (ev == 0)
						{
							st = @event.Detail.Classification;
						}
						else
						{
							if (ev > 0)
							{
								st = LangMan.LSList[(int)TGenEngine.FamilyEvents[ev].Name - 1];
							}
							else
							{
								st = @event.Name;
							}
						}
						if (aSummary != null)
						{
							aSummary.Add(st + ": " + TGenEngine.GetEventDesc(@event.Detail));
							this.ShowDetailCause(@event.Detail, aSummary);
						}
						this.ShowDetailInfo(@event.Detail, aSummary);
						if (aList != null)
						{
							TExtListItem item = aList.AddItem(Convert.ToString(idx + 1), @event);
							item.SubItems.Add(st);
							item.SubItems.Add(TGenEngine.GEDCOMCustomDateToStr(@event.Detail.Date, GKUI.TfmGEDKeeper.Instance.Options.DefDateFormat, false));
							item.SubItems.Add(@event.Detail.Place.StringValue);
							item.SubItems.Add(TGenEngine.GetEventCause(@event.Detail));
						}
					}
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.RecListFamilyEventsRefresh(): " + E.Message);
			}
		}

		public void RecListGroupsRefresh(TGEDCOMIndividualRecord aRecord, TGKListView aList, StringList aSummary)
		{
			try
			{
				if (aList != null)
				{
					aList.Items.Clear();
				}
				if (aRecord.Groups.Count != 0)
				{
					if (aList == null && aSummary != null)
					{
						aSummary.Add("");
						aSummary.Add(LangMan.LSList[58] + ":");
					}

					int num = aRecord.Groups.Count - 1;
					for (int idx = 0; idx <= num; idx++)
					{
						TGEDCOMPointer ptr = aRecord.Groups[idx];
						TGEDCOMGroupRecord grp = ptr.Value as TGEDCOMGroupRecord;
						if (grp != null)
						{
							if (aList == null && aSummary != null)
							{
								aSummary.Add("    " + TGenEngine.HyperLink(grp.XRef, grp.GroupName, 0));
							}
							if (aList != null)
							{
								TExtListItem item = aList.AddItem(grp.GroupName, grp);
							}
						}
					}
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.RecListGroupsRefresh(): " + E.Message);
			}
		}

		public void RecListIndividualEventsRefresh(TGEDCOMIndividualRecord aRecord, TGKListView aList, StringList aSummary)
		{
			try
			{
				if (aList != null)
				{
					aList.Items.Clear();
				}

				if (aRecord.IndividualEvents.Count != 0)
				{
					if (aSummary != null)
					{
						aSummary.Add("");
						aSummary.Add(LangMan.LSList[83] + ":");
					}

					int num = aRecord.IndividualEvents.Count - 1;
					for (int idx = 0; idx <= num; idx++)
					{
						TGEDCOMCustomEvent @event = aRecord.IndividualEvents[idx];
						int ev = TGenEngine.GetPersonEventIndex(@event.Name);
						string st;
						if (ev == 0)
						{
							st = @event.Detail.Classification;
						}
						else
						{
							if (ev > 0)
							{
								st = LangMan.LSList[(int)TGenEngine.PersonEvents[ev].Name - 1];
							}
							else
							{
								st = @event.Name;
							}
						}

						if (aSummary != null)
						{
							string sv = "";
							if (@event.StringValue != "") {
								sv = @event.StringValue + ", ";
							}
							aSummary.Add(st + ": " + sv + TGenEngine.GetEventDesc(@event.Detail));
							this.ShowDetailCause(@event.Detail, aSummary);
							this.ShowAddress(@event.Detail.Address, aSummary);
							this.ShowDetailInfo(@event.Detail, aSummary);
						}

						if (aList != null)
						{
							TExtListItem item = aList.AddItem(Convert.ToString(idx + 1), @event);
							item.SubItems.Add(st);
							item.SubItems.Add(TGenEngine.GEDCOMCustomDateToStr(@event.Detail.Date, GKUI.TfmGEDKeeper.Instance.Options.DefDateFormat, false));
							st = @event.Detail.Place.StringValue;
							if (@event.StringValue != "")
							{
								st = st + " [" + @event.StringValue + "]";
							}
							item.SubItems.Add(st);
							item.SubItems.Add(TGenEngine.GetEventCause(@event.Detail));
						}
					}

					if (aList != null) aList.ResizeColumn(2);
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.RecListIndividualEventsRefresh(): " + E.Message);
			}
		}

		public void RecListMediaRefresh(TGEDCOMRecord aRecord, TGKListView aList, StringList aSummary)
		{
			try
			{
				if (aList != null)
				{
					aList.Items.Clear();
				}
				if (aRecord.MultimediaLinks.Count != 0)
				{
					if (aList == null && aSummary != null)
					{
						aSummary.Add("");
						aSummary.Add(LangMan.LSList[55] + " (" + aRecord.MultimediaLinks.Count.ToString() + "):");
					}

					int num = aRecord.MultimediaLinks.Count - 1;
					for (int idx = 0; idx <= num; idx++)
					{
						TGEDCOMMultimediaLink mmLink = aRecord.MultimediaLinks[idx];
						TGEDCOMMultimediaRecord mmRec = mmLink.Value as TGEDCOMMultimediaRecord;
						if (mmRec != null && mmRec.FileReferences.Count != 0)
						{
							string st = mmRec.FileReferences[0].Title;
							if (aList == null && aSummary != null)
							{
								aSummary.Add(string.Concat(new string[]
								{
									"  ", 
									TGenEngine.HyperLink(mmRec.XRef, st, 0), 
									" (", 
									TGenEngine.HyperLink("view_" + mmRec.XRef, "просмотр", 0), 
									")"
								}));
							}
							if (aList != null)
							{
								aList.AddItem(st, mmLink);
							}
						}
					}
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.RecListMediaRefresh(): " + E.Message);
			}
		}

		public void RecListNotesRefresh(TGEDCOMRecord aRecord, TGKListView aList, StringList aSummary)
		{
			try
			{
				if (aList != null)
				{
					aList.Items.Clear();
				}

				if (aRecord.Notes.Count != 0)
				{
					if (aSummary != null)
					{
						aSummary.Add("");
						aSummary.Add(LangMan.LSList[54] + " (" + aRecord.Notes.Count.ToString() + "):");
					}

					int num = aRecord.Notes.Count - 1;
					for (int idx = 0; idx <= num; idx++)
					{
						TGEDCOMNotes note = aRecord.Notes[idx];

						if (aSummary != null)
						{
							int num2 = note.Notes.Count - 1;
							for (int i = 0; i <= num2; i++)
							{
								string st = note.Notes[i];
								aSummary.Add(st);
							}

							if (idx < aRecord.Notes.Count - 1)
							{
								aSummary.Add("");
							}
						}

						if (aList != null)
						{
							aList.AddItem(note.Notes.Text.Trim(), note);
						}
					}
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.RecListNotesRefresh(): " + E.Message);
			}
		}

		public void RecListSourcesRefresh(TGEDCOMRecord aRecord, TGKListView aList, StringList aSummary)
		{
			try
			{
				if (aList != null)
				{
					aList.Items.Clear();
				}
				if (aRecord.SourceCitations.Count != 0)
				{
					if (aList == null && aSummary != null)
					{
						aSummary.Add("");
						aSummary.Add(LangMan.LSList[56] + " (" + aRecord.SourceCitations.Count.ToString() + "):");
					}

					int num = aRecord.SourceCitations.Count - 1;
					for (int idx = 0; idx <= num; idx++)
					{
						TGEDCOMSourceCitation cit = aRecord.SourceCitations[idx];
						TGEDCOMSourceRecord sourceRec = cit.Value as TGEDCOMSourceRecord;
						if (sourceRec != null)
						{
							string nm = "\"" + sourceRec.FiledByEntry + "\"";
							if (cit.Page != "")
							{
								nm = nm + ", " + cit.Page;
							}
							if (aList == null && aSummary != null)
							{
								aSummary.Add("  " + TGenEngine.HyperLink(sourceRec.XRef, nm, 0));
							}
							if (aList != null)
							{
								TExtListItem item = aList.AddItem(sourceRec.Originator.Text.Trim(), cit);
								item.SubItems.Add(nm);
							}
						}
					}
					if (aList != null)
					{
						aList.ResizeColumn(1);
					}
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.RecListSourcesRefresh(): " + E.Message);
			}
		}

		public void SetupRecEventsList(TSheetList aList, bool PersonsMode)
		{
			aList.Columns_BeginUpdate();
			aList.Columns_Clear();
			aList.AddColumn("№", 25, false);
			aList.AddColumn(LangMan.LSList[203], 90, false);
			aList.AddColumn(LangMan.LSList[139], 80, false);
			if (!PersonsMode)
			{
				aList.AddColumn(LangMan.LSList[204], 200, false);
			}
			else
			{
				aList.AddColumn(LangMan.LSList[235], 200, false);
			}
			aList.AddColumn(LangMan.LSList[205], 130, false);
			aList.Columns_EndUpdate();
		}

		public void SetupRecMediaList(TSheetList aList)
		{
			aList.Columns_BeginUpdate();
			aList.Columns_Clear();
			aList.AddColumn(LangMan.LSList[55], 300, false);
			aList.Columns_EndUpdate();
		}

		public void SetupRecNotesList(TSheetList aList)
		{
			aList.Columns_BeginUpdate();
			aList.Columns_Clear();
			aList.AddColumn(LangMan.LSList[108], 300, false);
			aList.Columns_EndUpdate();
		}

		public void SetupRecSourcesList(TSheetList aList)
		{
			aList.Columns_BeginUpdate();
			aList.Columns_Clear();
			aList.AddColumn(LangMan.LSList[142], 120, false);
			aList.AddColumn(LangMan.LSList[125], 180, false);
			aList.Columns_EndUpdate();
		}

		public void ShowDetailCause(TGEDCOMEventDetail aDetail, StringList aSummary)
		{
			string cause = TGenEngine.GetEventCause(aDetail);
			if (aSummary != null && cause != "")
			{
				aSummary.Add("    " + cause);
			}
		}

		public void ShowDetailInfo(TGEDCOMEventDetail aDetail, StringList aSummary)
		{
			if (aSummary != null && aDetail.SourceCitations.Count != 0)
			{
				aSummary.Add("    " + LangMan.LSList[56] + " (" + aDetail.SourceCitations.Count.ToString() + "):");

				int num = aDetail.SourceCitations.Count - 1;
				for (int idx = 0; idx <= num; idx++)
				{
					TGEDCOMSourceCitation cit = aDetail.SourceCitations[idx];
					TGEDCOMSourceRecord sourceRec = cit.Value as TGEDCOMSourceRecord;
					if (sourceRec != null)
					{
						string nm = "\"" + sourceRec.FiledByEntry + "\"";
						if (cit.Page != "")
						{
							nm = nm + ", " + cit.Page;
						}
						aSummary.Add("      " + TGenEngine.HyperLink(sourceRec.XRef, nm, 0));
					}
				}
			}
		}
		public void ShowFamilyInfo(TGEDCOMFamilyRecord aFamily, StringList aSummary)
		{
			try
			{
				aSummary.BeginUpdate();
				try
				{
					aSummary.Clear();
					if (aFamily != null)
					{
						aSummary.Add("");
						TGEDCOMIndividualRecord irec = aFamily.Husband.Value as TGEDCOMIndividualRecord;
						string st;
						if (irec != null)
						{
							st = TGenEngine.HyperLink(irec.XRef, TGenEngine.GetNameStr(irec, true, false), 0);
						}
						else
						{
							st = LangMan.LSList[64];
						}
						aSummary.Add(LangMan.LSList[115] + ": " + st + TGenEngine.GetLifeStr(irec));
						irec = (aFamily.Wife.Value as TGEDCOMIndividualRecord);
						if (irec != null)
						{
							st = TGenEngine.HyperLink(irec.XRef, TGenEngine.GetNameStr(irec, true, false), 0);
						}
						else
						{
							st = LangMan.LSList[63];
						}

						aSummary.Add(LangMan.LSList[116] + ": " + st + TGenEngine.GetLifeStr(irec));
						aSummary.Add("");
						if (aFamily.Childrens.Count != 0)
						{
							aSummary.Add(LangMan.LSList[118] + ":");
						}

						int num = aFamily.Childrens.Count - 1;
						for (int i = 0; i <= num; i++)
						{
							irec = (aFamily.Childrens[i].Value as TGEDCOMIndividualRecord);
							aSummary.Add("    " + TGenEngine.HyperLink(irec.XRef, TGenEngine.GetNameStr(irec, true, false), 0) + TGenEngine.GetLifeStr(irec));
						}
						aSummary.Add("");
						this.RecListFamilyEventsRefresh(aFamily, null, aSummary);
						this.RecListNotesRefresh(aFamily, null, aSummary);
						this.RecListMediaRefresh(aFamily, null, aSummary);
						this.RecListSourcesRefresh(aFamily, null, aSummary);
					}
				}
				finally
				{
					aSummary.EndUpdate();
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.ShowFamilyInfo(): " + E.Message);
			}
		}

		public void ShowGroupInfo(TGEDCOMGroupRecord aGroup, StringList aSummary)
		{
			try
			{
				StringList mbrList = new StringList();
				aSummary.BeginUpdate();
				try
				{
					aSummary.Clear();
					if (aGroup != null)
					{
						aSummary.Add("");
						aSummary.Add("~ub+1~" + aGroup.GroupName + "~bu-1~");
						aSummary.Add("");
						aSummary.Add(LangMan.LSList[126] + " (" + aGroup.Members.Count.ToString() + "):");

						int num = aGroup.Members.Count - 1;
						for (int i = 0; i <= num; i++)
						{
							TGEDCOMPointer ptr = aGroup.Members[i];
							TGEDCOMIndividualRecord member = ptr.Value as TGEDCOMIndividualRecord;
							mbrList.AddObject(TGenEngine.GetNameStr(member, true, false), member);
						}
						mbrList.Sort();

						int num2 = mbrList.Count - 1;
						for (int i = 0; i <= num2; i++)
						{
							TGEDCOMIndividualRecord member = mbrList.GetObject(i) as TGEDCOMIndividualRecord;
							aSummary.Add("    " + TGenEngine.HyperLink(member.XRef, mbrList[i], i + 1));
						}
						this.RecListNotesRefresh(aGroup, null, aSummary);
						this.RecListMediaRefresh(aGroup, null, aSummary);
					}
				}
				finally
				{
					aSummary.EndUpdate();
					mbrList.Free();
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.ShowGroupInfo(): " + E.Message);
			}
		}

		public void ShowMultimediaInfo(TGEDCOMMultimediaRecord aMultimediaRec, StringList aSummary)
		{
			try
			{
				aSummary.BeginUpdate();
				try
				{
					aSummary.Clear();
					if (aMultimediaRec != null)
					{
						aSummary.Add("");
						aSummary.Add("~ub+1~" + aMultimediaRec.FileReferences[0].Title + "~bu-1~");
						aSummary.Add("");
						aSummary.Add("[ " + TGenEngine.HyperLink("view_" + aMultimediaRec.XRef, LangMan.LSList[148], 0) + " ]");
						aSummary.Add("");
						aSummary.Add(LangMan.LSList[234] + ":");

						int num = this.FTree.RecordsCount - 1;
						for (int i = 0; i <= num; i++)
						{
							this.SearchSubjectLinks(this.FTree.GetRecord(i), aMultimediaRec, aSummary);
						}
						this.RecListNotesRefresh(aMultimediaRec, null, aSummary);
						this.RecListSourcesRefresh(aMultimediaRec, null, aSummary);
					}
				}
				finally
				{
					aSummary.EndUpdate();
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.ShowMultimediaInfo(): " + E.Message);
			}
		}

		public void ShowNoteInfo(TGEDCOMNoteRecord aNoteRec, StringList aSummary)
		{
			try
			{
				aSummary.BeginUpdate();
				try
				{
					aSummary.Clear();
					if (aNoteRec != null)
					{
						aSummary.Add("");
						aSummary.AddStrings(aNoteRec.Note);
						aSummary.Add("");
						aSummary.Add(LangMan.LSList[234] + ":");

						int num = this.FTree.RecordsCount - 1;
						for (int i = 0; i <= num; i++)
						{
							this.SearchSubjectLinks(this.FTree.GetRecord(i), aNoteRec, aSummary);
						}
					}
				}
				finally
				{
					aSummary.EndUpdate();
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.ShowNoteInfo(): " + E.Message);
			}
		}

		public void ShowPersonInfo(TGEDCOMIndividualRecord iRec, StringList aSummary)
		{
			try
			{
				aSummary.BeginUpdate();
				aSummary.Clear();
				try
				{
					if (iRec != null)
					{
						aSummary.Add("");
						aSummary.Add("~ub+1~" + TGenEngine.GetNameStr(iRec, true, true) + "~bu-1~");
						aSummary.Add(LangMan.LSList[87] + ": " + TGenEngine.SexStr(iRec.Sex));
						try
						{
							if (iRec.ChildToFamilyLinks.Count != 0)
							{
								aSummary.Add("");
								aSummary.Add(LangMan.LSList[152] + ":");
								TGEDCOMFamilyRecord family = iRec.ChildToFamilyLinks[0].Family;
								TGEDCOMIndividualRecord rel_person = family.Husband.Value as TGEDCOMIndividualRecord;
								string st;
								if (rel_person != null)
								{
									st = TGenEngine.HyperLink(rel_person.XRef, TGenEngine.GetNameStr(rel_person, true, false), 0);
								}
								else
								{
									st = LangMan.LSList[64];
								}
								aSummary.Add("  " + LangMan.LSList[150] + ": " + st + TGenEngine.GetLifeStr(rel_person));
								rel_person = (family.Wife.Value as TGEDCOMIndividualRecord);
								if (rel_person != null)
								{
									st = TGenEngine.HyperLink(rel_person.XRef, TGenEngine.GetNameStr(rel_person, true, false), 0);
								}
								else
								{
									st = LangMan.LSList[63];
								}
								aSummary.Add("  " + LangMan.LSList[151] + ": " + st + TGenEngine.GetLifeStr(rel_person));
							}
						}
						catch (Exception E)
						{
							SysUtils.LogWrite("GKBase.ShowPersonInfo().Parents(): " + E.Message);
						}
						try
						{
							int num = iRec.SpouseToFamilyLinks.Count - 1;
							for (int idx = 0; idx <= num; idx++)
							{
								TGEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[idx].Family;
								if (family == null)
								{
									SysUtils.LogWrite("File (" + this.FileName + "), iRec (" + iRec.XRef + "): empty family entry");
								}
								else
								{
									if (TGenEngine.IsRecordAccess(family.Restriction, this.FShieldState))
									{
										string st;
										TGEDCOMPointer sp;
										string unk;
										if (iRec.Sex == TGEDCOMSex.svMale)
										{
											sp = family.Wife;
											st = LangMan.LSList[116] + ": ";
											unk = LangMan.LSList[63];
										}
										else
										{
											sp = family.Husband;
											st = LangMan.LSList[115] + ": ";
											unk = LangMan.LSList[64];
										}
										string marr = TGenEngine.GetMarriageDate(family, TGenEngine.TDateFormat.dfDD_MM_YYYY);
										if (marr != "")
										{
											marr = LangMan.LSList[236] + " " + marr;
										}
										else
										{
											marr = LangMan.LSList[237];
										}
										TGEDCOMIndividualRecord rel_person = sp.Value as TGEDCOMIndividualRecord;
										aSummary.Add("");
										if (rel_person != null)
										{
											st = string.Concat(new string[]
											{
												st, 
												TGenEngine.HyperLink(rel_person.XRef, TGenEngine.GetNameStr(rel_person, true, false), 0), 
												" (", 
												TGenEngine.HyperLink(family.XRef, marr, 0), 
												")"
											});
										}
										else
										{
											st = st + unk + " (" + TGenEngine.HyperLink(family.XRef, marr, 0) + ")";
										}
										aSummary.Add(st);
										if (family.Childrens.Count != 0)
										{
											aSummary.Add("");
											aSummary.Add(LangMan.LSList[118] + ":");
										}

										int num2 = family.Childrens.Count - 1;
										for (int i = 0; i <= num2; i++)
										{
											rel_person = (family.Childrens[i].Value as TGEDCOMIndividualRecord);
											aSummary.Add("    " + TGenEngine.HyperLink(rel_person.XRef, TGenEngine.GetNameStr(rel_person, true, false), 0) + TGenEngine.GetLifeStr(rel_person));
										}
									}
								}
							}
						}
						catch (Exception E)
						{
							SysUtils.LogWrite("GKBase.ShowPersonInfo().Families(): " + E.Message);
						}

						this.RecListIndividualEventsRefresh(iRec, null, aSummary);
						this.RecListNotesRefresh(iRec, null, aSummary);
						this.RecListMediaRefresh(iRec, null, aSummary);
						this.RecListSourcesRefresh(iRec, null, aSummary);
						this.RecListAssociationsRefresh(iRec, null, aSummary);
						this.RecListGroupsRefresh(iRec, null, aSummary);
						StringList namesakes = new StringList();
						try
						{
							string st = TGenEngine.GetNameStr(iRec, true, false);

							int num3 = this.FTree.RecordsCount - 1;
							for (int i = 0; i <= num3; i++)
							{
								TGEDCOMRecord rec = this.FTree.GetRecord(i);
								if (rec is TGEDCOMIndividualRecord && !object.Equals(rec, iRec))
								{
									TGEDCOMIndividualRecord rel_person = (TGEDCOMIndividualRecord)rec;
									string unk = TGenEngine.GetNameStr(rel_person, true, false);
									if (st == unk)
									{
										namesakes.AddObject(unk + TGenEngine.GetLifeStr(rel_person), rel_person);
									}
								}
							}

							if (namesakes.Count > 0)
							{
								aSummary.Add("");
								aSummary.Add(LangMan.LSList[238] + ":");

								int num4 = namesakes.Count - 1;
								for (int i = 0; i <= num4; i++)
								{
									TGEDCOMIndividualRecord rel_person = (TGEDCOMIndividualRecord)namesakes.GetObject(i);
									aSummary.Add("    " + TGenEngine.HyperLink(rel_person.XRef, namesakes[i], 0));
								}
							}
						}
						finally
						{
							namesakes.Free();
						}
					}
				}
				finally
				{
					aSummary.EndUpdate();
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.ShowPersonInfo(): " + E.Message);
			}
		}

		public void ShowSourceInfo(TGEDCOMSourceRecord aSourceRec, StringList aSummary)
		{
			try
			{
				aSummary.BeginUpdate();
				StringList link_list = new StringList();
				try
				{
					aSummary.Clear();
					if (aSourceRec != null)
					{
						aSummary.Add("");
						aSummary.Add("~ub+1~" + aSourceRec.FiledByEntry + "~bu-1~");
						aSummary.Add("");
						aSummary.Add(LangMan.LSList[142] + ": " + aSourceRec.Originator.Text.Trim());
						aSummary.Add(LangMan.LSList[125] + ": \"" + aSourceRec.Title.Text.Trim() + "\"");
						aSummary.Add(LangMan.LSList[143] + ": \"" + aSourceRec.Publication.Text.Trim() + "\"");

						if (aSourceRec.RepositoryCitations.Count > 0)
						{
							aSummary.Add("");
							aSummary.Add(LangMan.LSList[57] + ":");

							int num = aSourceRec.RepositoryCitations.Count - 1;
							for (int i = 0; i <= num; i++)
							{
								TGEDCOMRepositoryRecord rep = aSourceRec.RepositoryCitations[i].Value as TGEDCOMRepositoryRecord;
								aSummary.Add("    " + TGenEngine.HyperLink(rep.XRef, rep.RepositoryName, 0));
							}
						}

						aSummary.Add("");
						aSummary.Add(LangMan.LSList[234] + ":");

						int num2 = this.FTree.RecordsCount - 1;
						for (int j = 0; j <= num2; j++)
						{
							this.SearchSubjectLinks(this.FTree.GetRecord(j), aSourceRec, link_list);
						}

						link_list.Sort();

						int num3 = link_list.Count - 1;
						for (int j = 0; j <= num3; j++)
						{
							aSummary.Add(link_list[j]);
						}
						this.RecListNotesRefresh(aSourceRec, null, aSummary);
						this.RecListMediaRefresh(aSourceRec, null, aSummary);
					}
				}
				finally
				{
					link_list.Free();
					aSummary.EndUpdate();
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.ShowSourceInfo(): " + E.Message);
			}
		}

		public void ShowRepositoryInfo(TGEDCOMRepositoryRecord aRepositoryRec, StringList aSummary)
		{
			try
			{
				aSummary.BeginUpdate();
				try
				{
					aSummary.Clear();
					if (aRepositoryRec != null)
					{
						aSummary.Add("");
						aSummary.Add("~ub+1~" + aRepositoryRec.RepositoryName.Trim() + "~bu-1~");
						aSummary.Add("");
						this.ShowAddress(aRepositoryRec.Address, aSummary);
						aSummary.Add("");
						aSummary.Add(LangMan.LSList[56] + ":");

						int num = this.FTree.RecordsCount - 1;
						for (int i = 0; i <= num; i++)
						{
							TGEDCOMRecord rec = this.FTree.GetRecord(i);

							if (rec is TGEDCOMSourceRecord)
							{
								TGEDCOMSourceRecord srcRec = (TGEDCOMSourceRecord)rec;

								int num2 = srcRec.RepositoryCitations.Count - 1;
								for (int j = 0; j <= num2; j++)
								{
									if (object.Equals(srcRec.RepositoryCitations[j].Value, aRepositoryRec))
									{
										aSummary.Add("    " + TGenEngine.GenRecordLink(this.FTree, srcRec, false));
									}
								}
							}
						}
						this.RecListNotesRefresh(aRepositoryRec, null, aSummary);
					}
				}
				finally
				{
					aSummary.EndUpdate();
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.ShowRepositoryInfo(): " + E.Message);
			}
		}

		public void ShowResearchInfo(TGEDCOMResearchRecord aResearchRec, StringList aSummary)
		{
			try
			{
				aSummary.BeginUpdate();
				try
				{
					aSummary.Clear();
					if (aResearchRec != null)
					{
						aSummary.Add("");
						aSummary.Add(LangMan.LSList[125] + ": \"~ub+1~" + aResearchRec.ResearchName.Trim() + "~bu-1~\"");
						aSummary.Add("");
						aSummary.Add(LangMan.LSList[178] + ": " + LangMan.LSList[(int)TGenEngine.PriorityNames[(int)aResearchRec.Priority] - 1]);
						aSummary.Add(string.Concat(new string[]
						{
							LangMan.LSList[117], 
							": ", 
							LangMan.LSList[(int)TGenEngine.StatusNames[(int)aResearchRec.Status] - 1], 
							" (", 
							aResearchRec.Percent.ToString(), 
							"%)"
						}));

						aSummary.Add(LangMan.LSList[180] + ": " + TGenEngine.GEDCOMDateToStr(aResearchRec.StartDate, TGenEngine.TDateFormat.dfDD_MM_YYYY));
						aSummary.Add(LangMan.LSList[181] + ": " + TGenEngine.GEDCOMDateToStr(aResearchRec.StopDate, TGenEngine.TDateFormat.dfDD_MM_YYYY));

						if (aResearchRec.Tasks.Count > 0)
						{
							aSummary.Add("");
							aSummary.Add(LangMan.LSList[60] + ":");

							int num = aResearchRec.Tasks.Count - 1;
							for (int i = 0; i <= num; i++)
							{
								TGEDCOMTaskRecord taskRec = aResearchRec.Tasks[i].Value as TGEDCOMTaskRecord;
								aSummary.Add("    " + TGenEngine.GenRecordLink(this.FTree, taskRec, false));
							}
						}

						if (aResearchRec.Communications.Count > 0)
						{
							aSummary.Add("");
							aSummary.Add(LangMan.LSList[61] + ":");

							int num2 = aResearchRec.Communications.Count - 1;
							for (int i = 0; i <= num2; i++)
							{
								TGEDCOMCommunicationRecord corrRec = aResearchRec.Communications[i].Value as TGEDCOMCommunicationRecord;
								aSummary.Add("    " + TGenEngine.GenRecordLink(this.FTree, corrRec, false));
							}
						}

						if (aResearchRec.Groups.Count != 0)
						{
							aSummary.Add("");
							aSummary.Add(LangMan.LSList[58] + ":");

							int num3 = aResearchRec.Groups.Count - 1;
							for (int i = 0; i <= num3; i++)
							{
								TGEDCOMGroupRecord grp = aResearchRec.Groups[i].Value as TGEDCOMGroupRecord;
								aSummary.Add("    " + TGenEngine.HyperLink(grp.XRef, grp.GroupName, 0));
							}
						}

						this.RecListNotesRefresh(aResearchRec, null, aSummary);
					}
				}
				finally
				{
					aSummary.EndUpdate();
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.ShowResearchInfo(): " + E.Message);
			}
		}

		public void ShowTaskInfo(TGEDCOMTaskRecord aTaskRec, StringList aSummary)
		{
			try
			{
				aSummary.BeginUpdate();
				try
				{
					aSummary.Clear();
					if (aTaskRec != null)
					{
						aSummary.Add("");
						aSummary.Add(LangMan.LSList[182] + ": ~ub+1~" + TGenEngine.GetTaskGoalStr(this.FTree, aTaskRec) + "~bu-1~");
						aSummary.Add("");
						aSummary.Add(LangMan.LSList[178] + ": " + LangMan.LSList[(int)TGenEngine.PriorityNames[(int)aTaskRec.Priority] - 1]);
						aSummary.Add(LangMan.LSList[180] + ": " + TGenEngine.GEDCOMDateToStr(aTaskRec.StartDate, TGenEngine.TDateFormat.dfDD_MM_YYYY));
						aSummary.Add(LangMan.LSList[181] + ": " + TGenEngine.GEDCOMDateToStr(aTaskRec.StopDate, TGenEngine.TDateFormat.dfDD_MM_YYYY));
						this.RecListNotesRefresh(aTaskRec, null, aSummary);
					}
				}
				finally
				{
					aSummary.EndUpdate();
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.ShowTaskInfo(): " + E.Message);
			}
		}

		public void ShowCommunicationInfo(TGEDCOMCommunicationRecord aCommunicationRec, StringList aSummary)
		{
			try
			{
				aSummary.BeginUpdate();
				try
				{
					aSummary.Clear();
					if (aCommunicationRec != null)
					{
						aSummary.Add("");
						aSummary.Add(LangMan.LSList[183] + ": \"~ub+1~" + aCommunicationRec.CommName.Trim() + "~bu-1~\"");
						aSummary.Add("");
						aSummary.Add(LangMan.LSList[184] + ": " + TGenEngine.GetCorresponderStr(this.FTree, aCommunicationRec, true));
						aSummary.Add(LangMan.LSList[113] + ": " + LangMan.LSList[(int)TGenEngine.CommunicationNames[(int)aCommunicationRec.CommunicationType] - 1]);
						aSummary.Add(LangMan.LSList[139] + ": " + TGenEngine.GEDCOMDateToStr(aCommunicationRec.Date, TGenEngine.TDateFormat.dfDD_MM_YYYY));
						this.RecListNotesRefresh(aCommunicationRec, null, aSummary);
						this.RecListMediaRefresh(aCommunicationRec, null, aSummary);
					}
				}
				finally
				{
					aSummary.EndUpdate();
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.ShowCommunicationInfo(): " + E.Message);
			}
		}

		public void ShowLocationInfo(TGEDCOMLocationRecord aLocationRec, StringList aSummary)
		{
			try
			{
				aSummary.BeginUpdate();
				StringList link_list = new StringList();
				try
				{
					aSummary.Clear();
					if (aLocationRec != null)
					{
						aSummary.Add("");
						aSummary.Add("~ub+1~" + aLocationRec.LocationName.Trim() + "~bu-1~");
						aSummary.Add("");
						aSummary.Add(LangMan.LSList[171] + ": " + aLocationRec.Map.Lati);
						aSummary.Add(LangMan.LSList[172] + ": " + aLocationRec.Map.Long);

						TGenEngine.GetLocationLinks(this.FTree, aLocationRec, ref link_list);

						if (link_list.Count > 0)
						{
							link_list.Sort();

							aSummary.Add("");
							aSummary.Add(LangMan.LSList[234] + ":");

							int num = link_list.Count - 1;
							for (int i = 0; i <= num; i++)
							{
								aSummary.Add("    " + link_list[i]);
							}
						}

						this.RecListNotesRefresh(aLocationRec, null, aSummary);
						this.RecListMediaRefresh(aLocationRec, null, aSummary);
					}
				}
				finally
				{
					link_list.Free();
					aSummary.EndUpdate();
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKBase.ShowLocationInfo(): " + E.Message);
			}
		}

		void ILocalization.SetLang()
		{
			this.PageRecords.TabPages[0].Text = LangMan.LSList[52];
			this.PageRecords.TabPages[1].Text = LangMan.LSList[53];
			this.PageRecords.TabPages[2].Text = LangMan.LSList[54];
			this.PageRecords.TabPages[3].Text = LangMan.LSList[55];
			this.PageRecords.TabPages[4].Text = LangMan.LSList[56];
			this.PageRecords.TabPages[5].Text = LangMan.LSList[57];
			this.PageRecords.TabPages[6].Text = LangMan.LSList[58];
			this.PageRecords.TabPages[7].Text = LangMan.LSList[59];
			this.PageRecords.TabPages[8].Text = LangMan.LSList[60];
			this.PageRecords.TabPages[9].Text = LangMan.LSList[61];
			this.PageRecords.TabPages[10].Text = LangMan.LSList[62];
		}

		public void TimeLine_Init()
		{
			this.FXFilter.LifeMode = TGenEngine.TLifeMode.lmTimeLine;
		}

		public void TimeLine_Done()
		{
			this.FXFilter.LifeMode = TGenEngine.TLifeMode.lmAll;
			this.FXFilter.TimeLineYear = -1;
			this.ApplyFilter();
		}

		public int TimeLineYear
		{
			get
			{
				return this.FXFilter.TimeLineYear;
			}
			set
			{
				this.FXFilter.TimeLineYear = value;
				this.ApplyFilter();
			}
		}

		private static void _SelectRecordByXRef_SelectItemByRec([In] TfmBase Self, TRecordsView aList, TGEDCOMRecord aRec, int aTab)
		{
			Self.PageRecords.SelectedIndex = aTab;
			Self.PageRecords_SelectedIndexChanged(null, null);
			Self.ActiveControl = aList;
			aList.SelectItemByRec(aRec);
		}

		public void ShowRecordInfo(TGEDCOMRecord aRecord)
		{
			if (aRecord != null)
			{
				try
				{
					switch (aRecord.RecordType)
					{
						case TGEDCOMRecordType.rtIndividual:
						{
							this.ShowPersonInfo(aRecord as TGEDCOMIndividualRecord, this.mPersonSummary.Lines);
							break;
						}
						case TGEDCOMRecordType.rtFamily:
						{
							this.ShowFamilyInfo(aRecord as TGEDCOMFamilyRecord, this.mFamilySummary.Lines);
							break;
						}
						case TGEDCOMRecordType.rtNote:
						{
							this.ShowNoteInfo(aRecord as TGEDCOMNoteRecord, this.mNoteSummary.Lines);
							break;
						}
						case TGEDCOMRecordType.rtMultimedia:
						{
							this.ShowMultimediaInfo(aRecord as TGEDCOMMultimediaRecord, this.mMediaSummary.Lines);
							break;
						}
						case TGEDCOMRecordType.rtSource:
						{
							this.ShowSourceInfo(aRecord as TGEDCOMSourceRecord, this.mSourceSummary.Lines);
							break;
						}
						case TGEDCOMRecordType.rtRepository:
						{
							this.ShowRepositoryInfo(aRecord as TGEDCOMRepositoryRecord, this.mRepositorySummary.Lines);
							break;
						}
						case TGEDCOMRecordType.rtGroup:
						{
							this.ShowGroupInfo(aRecord as TGEDCOMGroupRecord, this.mGroupSummary.Lines);
							break;
						}
						case TGEDCOMRecordType.rtResearch:
						{
							this.ShowResearchInfo(aRecord as TGEDCOMResearchRecord, this.mResearchSummary.Lines);
							break;
						}
						case TGEDCOMRecordType.rtTask:
						{
							this.ShowTaskInfo(aRecord as TGEDCOMTaskRecord, this.mTaskSummary.Lines);
							break;
						}
						case TGEDCOMRecordType.rtCommunication:
						{
							this.ShowCommunicationInfo(aRecord as TGEDCOMCommunicationRecord, this.mCommunicationSummary.Lines);
							break;
						}
						case TGEDCOMRecordType.rtLocation:
						{
							this.ShowLocationInfo(aRecord as TGEDCOMLocationRecord, this.mLocationSummary.Lines);
							break;
						}
					}
				}
				catch (Exception E)
				{
					SysUtils.LogWrite("GKBase.ShowRecordInfo(): " + E.Message);
				}
			}
		}

		public StringList GetRecordContext(TGEDCOMRecord aRecord)
		{
			//string res = "";
			StringList ctx = new StringList();
			if (aRecord != null)
			{
				try
				{
					//StringList ctx = new StringList();
					try
					{
						switch (aRecord.RecordType)
						{
							case TGEDCOMRecordType.rtIndividual:
								{
									this.ShowPersonInfo(aRecord as TGEDCOMIndividualRecord, ctx);
									break;
								}
							case TGEDCOMRecordType.rtFamily:
								{
									this.ShowFamilyInfo(aRecord as TGEDCOMFamilyRecord, ctx);
									break;
								}
							case TGEDCOMRecordType.rtNote:
								{
									this.ShowNoteInfo(aRecord as TGEDCOMNoteRecord, ctx);
									break;
								}
							case TGEDCOMRecordType.rtMultimedia:
								{
									this.ShowMultimediaInfo(aRecord as TGEDCOMMultimediaRecord, ctx);
									break;
								}
							case TGEDCOMRecordType.rtSource:
								{
									this.ShowSourceInfo(aRecord as TGEDCOMSourceRecord, ctx);
									break;
								}
							case TGEDCOMRecordType.rtRepository:
								{
									this.ShowRepositoryInfo(aRecord as TGEDCOMRepositoryRecord, ctx);
									break;
								}
							case TGEDCOMRecordType.rtGroup:
								{
									this.ShowGroupInfo(aRecord as TGEDCOMGroupRecord, ctx);
									break;
								}
							case TGEDCOMRecordType.rtResearch:
								{
									this.ShowResearchInfo(aRecord as TGEDCOMResearchRecord, ctx);
									break;
								}
							case TGEDCOMRecordType.rtTask:
								{
									this.ShowTaskInfo(aRecord as TGEDCOMTaskRecord, ctx);
									break;
								}
							case TGEDCOMRecordType.rtCommunication:
								{
									this.ShowCommunicationInfo(aRecord as TGEDCOMCommunicationRecord, ctx);
									break;
								}
							case TGEDCOMRecordType.rtLocation:
								{
									this.ShowLocationInfo(aRecord as TGEDCOMLocationRecord, ctx);
									break;
								}
						}
						//res = ctx.Text;
					}
					finally
					{
						//ctx.Free();
					}
				}
				catch (Exception E)
				{
					SysUtils.LogWrite("GKBase.GetRecordContext(): " + E.Message);
				}
			}
			return ctx;
		}
	}
}

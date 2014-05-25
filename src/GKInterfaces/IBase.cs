using System;
using System.Drawing;
using System.IO;

using ExtUtils;
using GedCom551;

namespace GKCore.Interfaces
{
	public interface IBase : IWorkWindow, IProgressController
	{
		IHost Host { get; }
		IBaseContext Context { get; }
		
		bool Modified { get; set; }
		ShieldState ShieldState { get; set; }
		TGEDCOMTree Tree { get; }
		ValuesCollection ValuesCollection { get; }

		void ApplyFilter();
		void ChangeRecord(TGEDCOMRecord record);
		void Close();

		string DefinePatronymic(string name, TGEDCOMSex sex, bool confirm); // bad
		TGEDCOMSex DefineSex(string iName, string iPatr);
		void CheckPersonSex(TGEDCOMIndividualRecord iRec);

		void FileNew();
		void FileLoad(string fileName);
		void FileSave(string fileName);

		TGEDCOMFamilyRecord GetChildFamily(TGEDCOMIndividualRecord iChild, bool aCanCreate, TGEDCOMIndividualRecord aNewParent); // bad
		ExtList GetContentList(TGEDCOMRecordType recType); // bad
		StringList GetRecordContent(TGEDCOMRecord record);
		IListManager GetRecordsListManByType(TGEDCOMRecordType recType); // bad
		TGEDCOMIndividualRecord GetSelectedPerson();
		TGEDCOMRecordType GetSelectedRecordType();
		void RefreshLists(bool titles);
		//void RefreshRecordsView(TGEDCOMRecordType recType);
		
		//
		bool CheckBasePath();
		MediaStoreType GetStoreType(TGEDCOMFileReference fileReference, ref string fileName);
		void MediaLoad(TGEDCOMFileReference fileReference, out Stream aStream, bool throwException);
		void MediaLoad(TGEDCOMFileReference fileReference, ref string fileName);
		bool MediaSave(TGEDCOMFileReference fileReference, string fileName, MediaStoreType storeType);
		Bitmap BitmapLoad(TGEDCOMFileReference fileReference, int thumbWidth, int thumbHeight, bool throwException);
		Bitmap GetPrimaryBitmap(TGEDCOMIndividualRecord iRec, int thumbWidth, int thumbHeight, bool throwException);
		//
		
		TGEDCOMIndividualRecord CreatePersonDialog(TGEDCOMIndividualRecord target, TargetMode targetMode, TGEDCOMSex needSex); // bad
		
		bool ModifyMedia(ref TGEDCOMMultimediaRecord mediaRec);
		bool ModifyNote(ref TGEDCOMNoteRecord noteRec);
		bool ModifySource(ref TGEDCOMSourceRecord sourceRec);
		bool ModifyRepository(ref TGEDCOMRepositoryRecord repRec);
		bool ModifyGroup(ref TGEDCOMGroupRecord groupRec);
		bool ModifyResearch(ref TGEDCOMResearchRecord researchRec);
		bool ModifyTask(ref TGEDCOMTaskRecord taskRec);
		bool ModifyCommunication(ref TGEDCOMCommunicationRecord commRec);
		bool ModifyLocation(ref TGEDCOMLocationRecord locRec);
		bool ModifyPerson(ref TGEDCOMIndividualRecord indivRec);
		bool ModifyFamily(ref TGEDCOMFamilyRecord familyRec, FamilyTarget target, TGEDCOMIndividualRecord person);
		bool ModifyAddress(TGEDCOMAddress address);

		void RecordAdd();
		void RecordDelete();
		bool RecordDelete(TGEDCOMRecord record, bool confirm);
		void RecordEdit(object sender, EventArgs e);
		bool RecordIsFiltered(TGEDCOMRecord record);

		TGEDCOMFamilyRecord SelectFamily(TGEDCOMIndividualRecord target);
		TGEDCOMIndividualRecord SelectPerson(TGEDCOMIndividualRecord target, TargetMode targetMode, TGEDCOMSex needSex);
		TGEDCOMRecord SelectRecord(TGEDCOMRecordType mode, params object[] args);
		void SelectRecordByXRef(string XRef);
		void SetFilter();
		void Show();
		void ShowMedia(TGEDCOMMultimediaRecord mediaRec, bool modal);

	}
}

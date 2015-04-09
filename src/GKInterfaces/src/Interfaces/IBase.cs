using System;
using System.Collections.Generic;
using System.Drawing;
using System.IO;

using GKCommon;
using GKCommon.GEDCOM;
using GKCommon.GEDCOM.Enums;
using GKCore.Types;

namespace GKCore.Interfaces
{
	public interface IBase : IWorkWindow, IProgressController
	{
		IHost Host { get; }
		IBaseContext Context { get; }
		
		bool Modified { get; set; }
		ShieldState ShieldState { get; set; }
		GEDCOMTree Tree { get; }
		ValuesCollection ValuesCollection { get; }

		void ApplyFilter();
		void ChangeRecord(GEDCOMRecord record);
		void Close();

		string DefinePatronymic(string name, GEDCOMSex sex, bool confirm); // bad
		GEDCOMSex DefineSex(string iName, string iPatr);
		void CheckPersonSex(GEDCOMIndividualRecord iRec);

		void FileNew();
		void FileLoad(string fileName);
		void FileSave(string fileName);

		GEDCOMFamilyRecord GetChildFamily(GEDCOMIndividualRecord iChild, bool aCanCreate, GEDCOMIndividualRecord aNewParent); // bad
		List<GEDCOMRecord> GetContentList(GEDCOMRecordType recType);
		StringList GetRecordContent(GEDCOMRecord record);
		IListManager GetRecordsListManByType(GEDCOMRecordType recType); // bad
		GEDCOMIndividualRecord GetSelectedPerson();
		GEDCOMRecordType GetSelectedRecordType();
		void RefreshLists(bool titles);
		//void RefreshRecordsView(GEDCOMRecordType recType);
		
		//
		bool CheckBasePath();
		MediaStoreType GetStoreType(GEDCOMFileReference fileReference, ref string fileName);
		void MediaLoad(GEDCOMFileReference fileReference, out Stream aStream, bool throwException);
		void MediaLoad(GEDCOMFileReference fileReference, ref string fileName);
		bool MediaSave(GEDCOMFileReference fileReference, string fileName, MediaStoreType storeType);
		Bitmap BitmapLoad(GEDCOMFileReference fileReference, int thumbWidth, int thumbHeight, bool throwException);
		Bitmap GetPrimaryBitmap(GEDCOMIndividualRecord iRec, int thumbWidth, int thumbHeight, bool throwException);
		//
		
		GEDCOMIndividualRecord CreatePersonDialog(GEDCOMIndividualRecord target, TargetMode targetMode, GEDCOMSex needSex); // bad
		
		bool ModifyMedia(ref GEDCOMMultimediaRecord mediaRec);
		bool ModifyNote(ref GEDCOMNoteRecord noteRec);
		bool ModifySource(ref GEDCOMSourceRecord sourceRec);
		bool ModifyRepository(ref GEDCOMRepositoryRecord repRec);
		bool ModifyGroup(ref GEDCOMGroupRecord groupRec);
		bool ModifyResearch(ref GEDCOMResearchRecord researchRec);
		bool ModifyTask(ref GEDCOMTaskRecord taskRec);
		bool ModifyCommunication(ref GEDCOMCommunicationRecord commRec);
		bool ModifyLocation(ref GEDCOMLocationRecord locRec);
		bool ModifyPerson(ref GEDCOMIndividualRecord indivRec);
		bool ModifyFamily(ref GEDCOMFamilyRecord familyRec, FamilyTarget target, GEDCOMIndividualRecord person);
		bool ModifyAddress(GEDCOMAddress address);

		void RecordAdd();
		void RecordDelete();
		bool RecordDelete(GEDCOMRecord record, bool confirm);
		void RecordEdit(object sender, EventArgs e);
		bool RecordIsFiltered(GEDCOMRecord record);

		GEDCOMFamilyRecord SelectFamily(GEDCOMIndividualRecord target);
		GEDCOMIndividualRecord SelectPerson(GEDCOMIndividualRecord target, TargetMode targetMode, GEDCOMSex needSex);
		GEDCOMRecord SelectRecord(GEDCOMRecordType mode, params object[] args);
		void SelectRecordByXRef(string xref);
		void SetFilter();
		void Show();
		void ShowMedia(GEDCOMMultimediaRecord mediaRec, bool modal);

	}
}

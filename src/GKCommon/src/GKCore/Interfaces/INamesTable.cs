using GKCommon.GEDCOM;
using GKCore.Types;

namespace GKCore.Interfaces
{
	public interface INamesTable
	{
		NameEntry AddName(string name);
		NameEntry FindName(string name);
		string GetPatronymicByName(string name, GEDCOMSex sex);
		string GetNameByPatronymic(string patronymic);
		GEDCOMSex GetSexByName(string name);
		void SetName(string name, string patronymic, GEDCOMSex sex);
		void SetNameSex(string name, GEDCOMSex sex);
		void ImportNames(GEDCOMIndividualRecord iRec);
	}
}

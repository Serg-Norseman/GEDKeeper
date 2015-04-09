using GKCommon;
using GKCommon.GEDCOM;
using GKCommon.GEDCOM.Enums;
using GKCommon.Graph;
using GKCore.Types;

namespace GKCore.Interfaces
{
	public interface IBaseContext
	{
		GEDCOMTree Tree { get; }

		// Data search
		GEDCOMSourceRecord aux_FindSource(string sourceName);
		void aux_GetSourcesList(StringList aSources);

		// Data Manipulation
		GEDCOMCustomEvent CreateEventEx(GEDCOMRecord aRec, string evSign, string evDate, string evPlace);
		GEDCOMIndividualRecord CreatePersonEx(string iName, string iPatronymic, string iSurname, GEDCOMSex iSex, bool birthEvent);

		// Individual utils
		bool IsChildless(GEDCOMIndividualRecord iRec);
		int FindBirthYear(GEDCOMIndividualRecord iRec);
		int FindDeathYear(GEDCOMIndividualRecord iRec);
		
		// Patriarchs Search
		void GetPatriarchsList(ExtList<PatriarchObj> patList, int gensMin, bool datesCheck);
		void GetPatriarchsLinks(ExtList<PatriarchObj> patList, int gensMin, bool datesCheck, bool loneSuppress);
		TGraph GetPatriarchsGraph(int gensMin, bool datesCheck, bool loneSuppress = true);
	}
}

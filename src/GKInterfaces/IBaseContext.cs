using System;

using ExtUtils;
using ExtUtils.Graph;
using GedCom551;

namespace GKCore.Interfaces
{
	public interface IBaseContext
	{
		TGEDCOMTree Tree { get; }

		// Data search
		TGEDCOMSourceRecord aux_FindSource(string sourceName);
		void aux_GetSourcesList(StringList aSources);

		// Data Manipulation
		TGEDCOMCustomEvent CreateEventEx(TGEDCOMRecord aRec, string evSign, string evDate, string evPlace);
		TGEDCOMIndividualRecord CreatePersonEx(string iName, string iPatronymic, string iSurname, TGEDCOMSex iSex, bool birthEvent);

		// Individual utils
		bool IsChildless(TGEDCOMIndividualRecord iRec);
		int FindBirthYear(TGEDCOMIndividualRecord iRec);
		int FindDeathYear(TGEDCOMIndividualRecord iRec);
		
		// Patriarchs Search
		void GetPatriarchsList(ExtList<TPatriarchObj> patList, int gensMin, bool datesCheck);
		void GetPatriarchsLinks(ExtList<TPatriarchObj> patList, int gensMin, bool datesCheck, bool loneSuppress);
		TGraph GetPatriarchsGraph(int gensMin, bool datesCheck, bool loneSuppress = true);
	}
}

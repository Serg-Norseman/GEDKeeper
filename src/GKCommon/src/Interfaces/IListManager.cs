using System;
using GKCommon.GEDCOM;

namespace GKCore.Interfaces
{
    public delegate bool ExternalFilterHandler(GEDCOMRecord record);

	public interface IListManager
	{
		ExternalFilterHandler ExternalFilter { get; set; }
		IListFilter Filter { get; }
		IListColumns ListColumns { get; }

		void AddCondition(Enum column, ConditionKind condition, string value);
		DataType GetColumnDataType(int index);
		string GetColumnName(Enum colType);
		void InitFilter();
		void WidthChanged(int colIndex, int colWidth);
	}
}

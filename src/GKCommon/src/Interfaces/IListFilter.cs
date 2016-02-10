using System;
using System.Collections.Generic;
using GKCore.Types;

namespace GKCore.Interfaces
{
	public enum DataType
	{
		dtString,
		dtInteger,
		dtFloat,
		dtDateTime,
		dtGEDCOMDate
	}

	public enum ConditionKind
	{
		ck_NotEq, ck_LT, ck_LET, ck_Eq, ck_GET, ck_GT, ck_Contains, ck_NotContains
	}

	public sealed class FilterCondition
	{
		public int ColumnIndex;
		public ConditionKind Condition;
		public object Value;
		
		public FilterCondition(int columnIndex, ConditionKind condition, object value)
		{
			this.ColumnIndex = columnIndex;
			this.Condition = condition;
			this.Value = value;
		}
	}

	public interface IListFilter
	{
		List<FilterCondition> Conditions { get; }
		void Clear();
	}

	public interface IIndividualListFilter : IListFilter
	{
		FilterLifeMode FilterLifeMode { get; set; }
	}

	public interface IListColumns
	{
		void Clear();
		void CopyTo(IListColumns columns);
		bool MoveColumn(int idx, bool up);
		void ResetDefaults();
	}
}

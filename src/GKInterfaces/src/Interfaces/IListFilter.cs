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
		public int col_index;
		public Enum column;
		public ConditionKind condition;
		public object value;
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

using System;
using System.Collections.Generic;
using GKCore.Types;

namespace GKCore.Interfaces
{
	public enum TDataType
	{
		dtString,
		dtInteger,
		dtFloat,
		dtDate,
		dtDateTime
	}

	public enum ConditionKind
	{
		ck_NotEq, ck_LT, ck_LET, ck_Eq, ck_GET, ck_GT, ck_Contains, ck_NotContains
	}

	// TODO: refactoring!
	public sealed class FilterCondition
	{
		public int col_index;
		public Enum column;
		public ConditionKind condition;
		public object value;
	}

	public interface IListFilter
	{
		List<FilterCondition> ColumnsFilter { get; }
		void Clear();
	}
	
	public interface IIndividualListFilter : IListFilter
	{
		FilterLifeMode FilterLifeMode { get; set; }
	}
	
	public interface IListColumns
	{
		void CopyTo(IListColumns columns);
	}
}

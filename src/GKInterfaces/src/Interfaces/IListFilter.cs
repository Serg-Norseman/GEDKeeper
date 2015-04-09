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

	public enum TConditionKind
	{
		ck_NotEq, ck_LT, ck_LET, ck_Eq, ck_GET, ck_GT, ck_Contains, ck_NotContains
	}

	public struct TFilterCondition
	{
		public int col_index;

		public Enum column;
		public TConditionKind condition;
		public object value;
	}

	public interface IListFilter
	{
		List<TFilterCondition> ColumnsFilter { get; }
		void Clear();
	}
	
	public interface IIndividualListFilter : IListFilter
	{
		FilterLifeMode FilterLifeMode { get; set; }
	}
}
